{ A tiny BASIC interpreter
License: The UNLICENSE
Support WHILE/WEND }

{$B-,S-,G-,I-,R-,N-,O-,P-,L-,F-}
CONST

LABEL_COUNT = 512;
FOR_COUNT   = 32;
WHILE_COUNT = 32;
GOSUB_COUNT = 32;
SUB_COUNT   = 128;

TVARIABLE   = 2;
TNUMBER     = 3;
TQUOTE      = 5;

TAADD     = ORD('+');
TASUB     = ORD('-');
TAMUL     = ORD('*');
TADIV     = ORD('/');
TAMOD     = ORD('%');

TLRB      = ORD('(');
TRRB      = ORD(')');
TCOMA     = ORD(',');
TSCOL     = ORD(';');

TEQ       = ORD('=');
TL        = ORD('<');
TG        = ORD('>');
TLE       = 250;
TGE       = 251;
TNEQ      = 252;

TPRINT    = ORD('?');
TINPUT    = 120;

TIF       = 130;
TTHEN     = 131;
TELSE     = 132;

TGOTO     = 140;
TGOSUB    = 141;
TRETURN   = 142;

TLET      = 150;

TWHILE    = 160;
TWEND     = 161;
TBREAK    = 162;
TCONTINUE = 163;

TFOR      = 165;
TTO       = 166;
TSTEP     = 167;
TNEXT     = 168;

TSUB      = 170;
TENDSUB   = 171;

TEOL      = 170;
TEND      = 171;

TYPE
TBASVNAME = STRING[1];
TBASCMD   = STRING[16];

TCOMMAND = RECORD
    cmd        : TBASCMD;
    token_type : BYTE;
END;

TVAL = INTEGER;

TLABEL = RECORD
        name : STRING[16];
        loc  : PCHAR;
END;

PFORSTK = ^TFORSTK;
TFORSTK = RECORD
        vname  : TBASVNAME;
        step   : INTEGER;
        target : TVAL;
        loc    : PCHAR;
END;

TWHILESTK = RECORD
        token      : STRING;
        token_type : BYTE;
        loc        : PCHAR;
END;

TSUBR = RECORD
        name : STRING[32];
        loc  : PCHAR;
END;

VAR
table       : ARRAY [0..64] OF TCOMMAND;
table_ptr   : INTEGER;
token       : STRING;
token_type  : BYTE;
prog        : PCHAR;
stop        : BOOLEAN;
variables   : ARRAY [0..25] OF TVAL;
label_table : ARRAY [0..LABEL_COUNT] OF TLABEL;
sub_table   : ARRAY [0..SUB_COUNT] OF TSUBR;
sub_ptr     : INTEGER;
fstk        : ARRAY [0..FOR_COUNT] OF TFORSTK;
fstk_ptr    : INTEGER;
wstk        : ARRAY [0..WHILE_COUNT] OF TWHILESTK;
wstk_ptr    : INTEGER;
gstk        : ARRAY [0..GOSUB_COUNT] OF PCHAR;
gstk_ptr    : INTEGER;
int_value   : INTEGER;

PROCEDURE get_exp(VAR result : TVAL);FORWARD;

PROCEDURE serror(error : INTEGER);
BEGIN
        IF NOT stop THEN BEGIN
                System.write('ERROR: ');
                CASE error OF
                0: System.write('Syntax error');
                1: System.write('Unbalanced parentheses');
                2: System.write('No expression present');
                3: System.write('Equals sign expected');
                4: System.write('Not a variable');
                5: System.write('Label table full');
                6: System.write('Duplicate label');
                7: System.write('Undefined label');
                8: System.write('THEN expected');
                9: System.write('TO expected');
                10: System.write('Too many nested FOR loops');
                11: System.write('NEXT without FOR');
                12: System.write('Too many nested GOSUBs');
                13: System.write('RETURN without GOSUB');
                14: System.write('EOL expected');
                15: System.write('Too many nested WHILE loops');
                16: System.write('WEND without WHILE');
                END;
                System.writeln;
                stop := TRUE;
        END;
END;

FUNCTION load_program(VAR size : WORD; fname : STRING) : PCHAR;
VAR     f : FILE;
        i : INTEGER;
        s : LONGINT;
        p : PCHAR;
BEGIN
        p := NIL;
        size := 0;
        Assign(f, fname);
        {$I-}
        Reset(f, 1);
        i := ioresult;
        {$I+}
        IF i = 0 THEN BEGIN
                s := FileSize(f);
                IF s > 64000 THEN s := 64000;
                size := s;
                GetMem(p, size + 1);
                IF p <> NIL THEN BEGIN
                        BlockRead(f, p[0], size);
                        p[size] := #0;
                END ELSE BEGIN
                        size := 0;
                        System.writeln('Memory allocation failure');
                END;
                Close(f);
        END ELSE BEGIN
                System.writeln('Could not read ', fname);
        END;
        load_program := p;
END;

FUNCTION look_up(VAR s : STRING) : BYTE;
VAR     i : INTEGER;
BEGIN
        i := 0;
        WHILE (table[i].cmd <> s) AND (table[i].cmd[0] <> #0) DO INC(i);
        look_up := table[i].token_type;
END;

PROCEDURE get_token;
VAR     temp  : PCHAR;
        i     : INTEGER;
        c, c2 : CHAR;
BEGIN

        token[0] := #0;
        int_value := 0;
        token_type := TEND;

        WHILE prog[0] IN [#1..#$09,#$0B..#$20] DO INC(prog);
        c := UpCase(prog[0]);
        c2 := prog[1];
        IF c IN ['A'..'Z'] THEN BEGIN
                i := 0;
                WHILE c IN ['A'..'Z','0'..'9'] DO BEGIN
                        INC(i);
                        token[i] := c;
                        INC(prog);
                        c := UpCase(prog[0]);
                END;
                token[0] := CHR(i);
                token_type := look_up(token);
                IF token_type = 0 THEN token_type := TVARIABLE;
        END ELSE IF c IN ['0'..'9'] THEN BEGIN
                i := 0;
                WHILE c IN ['0'..'9'] DO BEGIN
                        INC(i);
                        token[i] := c;
                        INC(prog);
                        c := prog[0];
                END;
                token[0] := CHR(i);
                val(token, int_value, i);
                token_type := TNUMBER;
        END ELSE IF c = #$0a THEN BEGIN
                INC(prog);
                token := #$0a;
                token_type := TEOL;
        END ELSE IF ((c = '<') AND (c2 = '=')) THEN BEGIN
                        token := c + c2;
                        INC(prog, 2);
                        token_type := TLE;
        END ELSE IF ((c = '>') AND (c2 = '=')) THEN BEGIN
                        token := c + c2;
                        INC(prog, 2);
                        token_type := TGE;
        END ELSE IF ((c = '<') AND (c2 = '>')) THEN BEGIN
                        token := c + c2;
                        INC(prog, 2);
                        token_type := TNEQ;
        END ELSE IF c = '"' THEN BEGIN
                INC(prog);
                i := 0;
                c := prog[0];
                WHILE NOT (c IN['"',#$0a,#0]) DO BEGIN
                        INC(i);
                        token[i] := prog[0];
                        INC(prog);
                        c := prog[0];
                END;
                token[0] := CHR(i);
                IF prog[0] = '"' THEN BEGIN
                        INC(prog);
                        token_type := TQUOTE;
                END ELSE serror(1);
        END ELSE IF c <> #0 THEN BEGIN
                token := c;
                INC(prog);
                token_type := ORD(c);
        END;
END;

PROCEDURE find_eol;
BEGIN
        WHILE NOT (prog[0] IN [#$0a, #0]) DO INC(prog);
        IF prog[0] = #$0a THEN INC(prog);
        get_token;
END;

PROCEDURE get_var_value(VAR s : TBASVNAME; VAR result : TVAL);
BEGIN
        result := variables[ORD(s[1]) - ORD('A')];
END;

PROCEDURE set_var_value(VAR s : TBASVNAME; VAR result : TVAL);
BEGIN
        variables[ORD(s[1]) - ORD('A')] := result;
END;

PROCEDURE arith(o : BYTE; VAR r : TVAL; h : TVAL);
BEGIN
        CASE o OF
        TASUB: DEC(r, h);
        TAADD: INC(r, h);
        TAMUL: r := r * h;
        TADIV: r := r div h;
        TAMOD: r := r mod h;
        END;
END;

FUNCTION isToken(t : BYTE) : BOOLEAN;ASSEMBLER;
ASM
        MOV  AL, t
        MOV  AH, token_type
        XOR  CX, CX
        CMP  AL, AH
        JNE  @end
        INC  CX
        PUSH CX
        CALL get_token
        POP  CX
@end:
        MOV  AX, CX
END;
{
FUNCTION isToken(t : BYTE) : BOOLEAN;
BEGIN
        IF token_type = t THEN BEGIN
                get_token;
                isToken := TRUE;
        END ELSE isToken := FALSE;
END;
}
PROCEDURE level2(VAR result : TVAL);
VAR     op    : BOOLEAN;
        i     : INTEGER;
        vname : TBASVNAME;
BEGIN
        op := isToken(TASUB);
        IF isToken(TLRB) THEN BEGIN
                get_exp(result);
                IF NOT isToken(TRRB) THEN serror(1);
        END ELSE BEGIN
                CASE token_type OF
                TVARIABLE: BEGIN
                        vname := token;
                        get_var_value(vname, result);
                        get_token;
                        END;
                TNUMBER: BEGIN
                        result := int_value;
                        get_token;
                        END;
                ELSE
                    serror(0);
                END;
        END;
        IF op THEN result := -result;
END;

PROCEDURE level1(VAR result : TVAL);
VAR     hold : TVAL;
        op   : BYTE;
BEGIN
        level2(result);
        op := token_type;
        WHILE isToken(TAMUL) OR isToken(TADIV) OR isToken(TAMOD) DO BEGIN
                level2(hold);
                arith(op, result, hold);
                op := token_type;
        END;
END;

PROCEDURE get_exp(VAR result : TVAL);
VAR     hold : TVAL;
        op   : BYTE;
BEGIN
        result := 0;
        level1(result);
        op := token_type;
        WHILE isToken(TAADD) OR isToken(TASUB) DO BEGIN
                level1(hold);
                arith(op, result, hold);
                op := token_type;
        END;
END;

PROCEDURE bas_assign;
VAR     var_name : TBASVNAME;
        myvar    : TVAL;
BEGIN
        IF token[1] IN ['A'..'Z'] THEN BEGIN
                var_name := token;
                get_token;
                IF isToken(TEQ) THEN BEGIN
                        get_exp(myvar);
                        set_var_value(var_name, myvar);
                END ELSE serror(3);
        END ELSE serror(4);
END;

PROCEDURE bas_print;
VAR     v          : TVAL;
        last_delim : CHAR;
BEGIN
        last_delim := #0;
        WHILE NOT stop DO BEGIN
                IF (token_type = TEOL) OR (token_type = TEND) THEN BREAK;
                IF token_type = TQUOTE THEN BEGIN
                        System.write(token);
                        get_token;
                END ELSE BEGIN
                        get_exp(v);
                        System.write(v);
                END;
                last_delim := token[1];
                
                IF isToken(TSCOL) THEN BEGIN
                        last_delim := ';';
                        System.write(' ');
                END ELSE IF isToken(TCOMA) THEN BEGIN
                        last_delim := ',';
                        System.write(#9);
                END ELSE BREAK;
        END;
        IF (token_type = TEOL) OR (token_type = TEND) THEN BEGIN
                IF NOT (last_delim IN [';',',']) THEN System.writeln;
        END ELSE serror(0);
END;

PROCEDURE scan_labels;
VAR     temp : PCHAR;
        t    : INTEGER;
BEGIN
        t := 0;
        WHILE t < LABEL_COUNT DO BEGIN
                label_table[t].name := '';
                INC(t);
        END;
        temp := prog;
        sub_ptr := -1;
        stop := FALSE;
        t := 0;
        get_token;
        WHILE (prog[0] <> #0) AND (t < LABEL_COUNT) DO BEGIN
                IF (token_type = TNUMBER) AND (t < LABEL_COUNT) THEN BEGIN
                        label_table[t].name := token;
                        label_table[t].loc := prog;
                        INC(t);
                END ELSE IF (token_type = TSUB) AND (sub_ptr < SUB_COUNT) THEN BEGIN
                        INC(sub_ptr);
                        get_token;
                        sub_table[sub_ptr].name := token;
                        sub_table[sub_ptr].loc := prog;
                END;
                find_eol;
        END;
        stop := FALSE;
        prog := temp;
END;

FUNCTION find_label(s : STRING) : PCHAR;
VAR     t : INTEGER;
BEGIN
        t := 0;
        WHILE t < LABEL_COUNT DO BEGIN
                IF label_table[t].name = s THEN BEGIN
                        find_label := label_table[t].loc;
                        EXIT;
                END;
                INC(t);
        END;
        serror(7);
        find_label := NIL;
END;

PROCEDURE bas_if;
VAR     x, y : TVAL;
        cond : BOOLEAN;
        op   : BYTE;
BEGIN
        get_exp(x);
        op := token_type;
        get_token;
        get_exp(y);

        cond := FALSE;
        IF op = TL THEN cond := x < y
        ELSE IF op = TLE THEN cond := x <= y
        ELSE IF op = TG THEN cond := x > y
        ELSE IF op = TGE THEN cond := x >= y
        ELSE IF op = TNEQ THEN cond := x <> y
        ELSE IF op = TEQ THEN cond := x = y
        ELSE BEGIN
                serror(0);
                EXIT;
        END;
        IF cond THEN BEGIN
                IF NOT IsToken(TTHEN) THEN serror(8);
        END ELSE find_eol;
END;

PROCEDURE bas_break;
VAR     i : INTEGER;
BEGIN
        DEC(wstk_ptr);
        i := 1;
        WHILE TRUE DO BEGIN
                IF IsToken(TWHILE) THEN INC(i)
                ELSE IF IsToken(TWEND) THEN BEGIN
                        DEC(i);
                        IF i = 0 THEN BREAK;
                END ELSE get_token;
        END;
END;

PROCEDURE bas_while;
VAR     x, y : TVAL;
        cond : BOOLEAN;
        op   : BYTE;
BEGIN
        IF wstk_ptr = WHILE_COUNT THEN BEGIN
                serror(15);
                EXIT;
        END;
        INC(wstk_ptr);
        wstk[wstk_ptr].loc := prog;
        wstk[wstk_ptr].token := token;
        wstk[wstk_ptr].token_type := token_type;

        get_exp(x);
        op := token_type;
        get_token;
        get_exp(y);

        cond := FALSE;
        IF op = TL THEN cond := x < y
        ELSE IF op = TLE THEN cond := x <= y
        ELSE IF op = TG THEN cond := x > y
        ELSE IF op = TGE THEN cond := x >= y
        ELSE IF op = TNEQ THEN cond := x <> y
        ELSE IF op = TEQ THEN cond := x = y
        ELSE BEGIN
                serror(0);
                EXIT;
        END;

        IF NOT IsToken(TEOL) THEN BEGIN
                serror(14);
                EXIT;
        END;

{                15: System.write('Too many nested WHILE loops');}
        IF NOT cond THEN bas_break;
END;

PROCEDURE bas_wend;
BEGIN
        IF wstk_ptr >= 0 THEN BEGIN
                prog := wstk[wstk_ptr].loc;
                token := wstk[wstk_ptr].token;
                token_type := wstk[wstk_ptr].token_type;
                DEC(wstk_ptr);
                bas_while;
        END ELSE serror(16);
END;

PROCEDURE bas_for;
VAR     i        : INTEGER;
        var_name : TBASVNAME;
        start    : TVAL;
        target   : TVAL;
        step     : TVAL;
BEGIN
        IF fstk_ptr = FOR_COUNT THEN BEGIN
                serror(10);
                EXIT;
        END;
        IF NOT (token[1] IN ['A'..'Z']) THEN BEGIN
                serror(4);
                EXIT;
        END;
        var_name := token;
        get_token;
        IF NOT IsToken(TEQ) THEN BEGIN
                serror(3);
                EXIT;
        END;
        get_exp(start);
        set_var_value(var_name, start);

        IF NOT IsToken(TTO) THEN BEGIN
                serror(9);
                EXIT;
        END;
        get_exp(target);
        IF start < target THEN step := 1 ELSE step := -1;
        IF IsToken(TSTEP) THEN get_exp(step);

        IF start <> target THEN BEGIN
                INC(fstk_ptr);
                fstk[fstk_ptr].vname := var_name;
                fstk[fstk_ptr].target := target;
                fstk[fstk_ptr].loc := prog;
                fstk[fstk_ptr].step := step;
        END ELSE BEGIN
                i := 1;
                WHILE TRUE DO BEGIN
                        IF IsToken(TFOR) THEN INC(i)
                        ELSE IF IsToken(TNEXT) THEN BEGIN
                                DEC(i);
                                IF i = 0 THEN BREAK;
                        END ELSE get_token;
                END;
        END;
END;

PROCEDURE bas_next;
VAR     v : TVAL;
        p : PFORSTK;
BEGIN
        IF fstk_ptr >= 0 THEN BEGIN
                p := @fstk[fstk_ptr];
                get_var_value(p^.vname, v);
                IF v <> p^.target THEN BEGIN
                        INC(v, p^.step);
                        set_var_value(p^.vname, v);
                        prog := p^.loc;
                        get_token;
                END ELSE DEC(fstk_ptr);
        END ELSE serror(11);
END;

PROCEDURE bas_input;
VAR     v : BYTE;
BEGIN
        IF token_type = TQUOTE THEN BEGIN
                System.write(token, ' ? ');
                get_token;
                IF token = ',' THEN get_token ELSE serror(1);
        END ELSE System.write('? ');
        v := ORD(token[1]) - ORD('A');
        System.readln(variables[v]);
        get_token;
END;

PROCEDURE addCmd(cmd : TBASCMD; ttype : BYTE);
VAR     i : INTEGER;
BEGIN
        table[table_ptr].cmd := cmd;
        table[table_ptr].token_type := ttype;
        FOR i := 1 TO Length(table[table_ptr].cmd) DO table[table_ptr].cmd[i] := UpCase(table[table_ptr].cmd[i]);
        INC(table_ptr);
END;

VAR     p_buf, loc : PCHAR;
        size       : WORD;
BEGIN
        table_ptr := 0;
        addCmd('WEND', TWEND);
        addCmd('NEXT', TNEXT);
        addCmd('RETURN', TRETURN);
        addCmd('IF', TIF);
        addCmd('THEN', TTHEN);
        addCmd('ELSE', TELSE);
        addCmd('GOTO', TGOTO);
        addCmd('GOSUB', TGOSUB);
        addCmd('WHILE', TWHILE);
        addCmd('BREAK', TBREAK);
        addCmd('CONTINUE', TCONTINUE);
        addCmd('FOR', TFOR);
        addCmd('TO', TTO);
        addCmd('STEP', TSTEP);
        addCmd('PRINT',TPRINT);
        addCmd('INPUT', TINPUT);
        addCmd('LET', TLET);
        addCmd('END', TEND);
        addCmd('SUB', TSUB);
        addCmd('ENDSUB', TENDSUB);

        addCmd('', 0);
        IF ParamCount <> 1 THEN BEGIN
                System.writeln('Usage :', ParamStr(0), ' filename.bas');
                Halt(1);
        END;
        p_buf := load_program(size, ParamStr(1));
        IF p_buf = NIL THEN Halt(1);
        prog := p_buf;
        scan_labels;
        sub_ptr  := -1;
        fstk_ptr := -1;
        wstk_ptr := -1;
        gstk_ptr := -1;
        get_token;
        WHILE NOT stop DO BEGIN
                IF token_type = TVARIABLE THEN bas_assign
                ELSE IF isToken(TNEXT) THEN bas_next
                ELSE IF isToken(TWEND) THEN bas_wend
                ELSE IF isToken(TFOR) THEN bas_for
                ELSE IF isToken(TWHILE) THEN bas_while
                ELSE IF isToken(TGOTO) THEN BEGIN
                        loc := find_label(token);
                        IF loc <> NIL THEN BEGIN
                                prog := loc;
                                get_token;
                        END;
                END ELSE IF isToken(TBREAK) THEN bas_break
                ELSE IF isToken(TCONTINUE) THEN bas_wend
                ELSE IF isToken(TIF) THEN bas_if
                ELSE IF isToken(TGOSUB) THEN BEGIN
                        loc := find_label(token);
                        IF loc <> NIL THEN BEGIN
                                INC(gstk_ptr);
                                gstk[gstk_ptr] := prog;
                                prog := loc;
                                get_token;
                        END;
                END ELSE IF isToken(TRETURN) THEN BEGIN
                        prog := gstk[gstk_ptr];
                        DEC(gstk_ptr);
                        get_token;
                END ELSE IF isToken(TLET) THEN bas_assign
                ELSE IF isToken(TPRINT) THEN bas_print
                ELSE IF isToken(TINPUT) THEN bas_input
                ELSE IF isToken(TEND) THEN stop := TRUE
                ELSE get_token;
        END;
        FreeMem(p_buf, size);
END.
