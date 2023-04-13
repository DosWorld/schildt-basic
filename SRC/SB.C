/* A tiny BASIC interpreter */
/* License: The UNLICENSE */
/* Does not support WHILE/WEND */

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define NUM_LAB 512
#define LAB_LEN 16
#define FOR_NEST 32
#define SUB_NEST 32
#define PROG_SIZE 64000

#define DELIMITER  1
#define VARIABLE   2
#define NUMBER     3
#define QUOTE      5

#define PRINT    10
#define INPUT    11
#define IF       12
#define THEN     13
#define ELSE     14
#define FOR      15
#define NEXT     16
#define TO       17
#define GOTO     18
#define EOL      19
#define GOSUB    21
#define RETURN   22
#define LET      23
#define END      24

#define NAME_SIZE 32

char *prog;
int stop;

typedef struct value {
    int int_val;
} value;

value variables[26];

struct commands {
    char *command;
    char token_type;
} table[] = {
    "print", PRINT,
    "input", INPUT,
    "if", IF,
    "then", THEN,
    "else", ELSE,
    "goto", GOTO,
    "for", FOR,
    "next", NEXT,
    "to", TO,
    "gosub", GOSUB,
    "return", RETURN,
    "end", END,
    "let", LET,
    NULL, END
};

char token[256];
char token_type;

struct label {
    char name[LAB_LEN];
    char *p;
};

struct label label_table[NUM_LAB];

struct for_stack {
    char var[NAME_SIZE + 1];
    value target;
    char *loc;
} fstk[FOR_NEST];
int fstk_ptr;

char *gstk[SUB_NEST];
int gstk_ptr;

void get_exp(value *result);

static char *err_msg[] = {
    "Syntax error",
    "Unbalanced parentheses",
    "No expression present",
    "Equals sign expected",
    "Not a variable",
    "Label table full",
    "Duplicate label",
    "Undefined label",
    "THEN expected",
    "TO expected",
    "Too many nested FOR loops",
    "NEXT without FOR",
    "Too many nested GOSUBs",
    "RETURN without GOSUB"
};

void serror(int error) {
    if(!stop) {
        printf("ERROR: %s\n", err_msg[error]);
        stop = 1;
    }
}

char load_program(char *p, char *fname) {
    FILE *fp;
    unsigned int i;

    if((fp = fopen(fname, "rb")) != NULL) {
        i = fread(p, 1, PROG_SIZE, fp);
        p[i] = 0;
        fclose(fp);
    }
    return i != 0;
}

char iswhite(char c) {
    return  (c <= ' ') && (c != '\n') && (c != 0);
}

char isalp(char c) {
    return ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z'));
}

char isdig(char c) {
    return ((c >= '0') && (c <= '9'));
}

char isalpdig(char c) {
    return ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')) || ((c >= '0') && (c <= '9'));
}

int look_up(char *s) {
    size_t i;
    char *p = s;

    while(*p != 0) {
        *p = tolower(*p);
        p++;
    }

    i = 0;
    while(table[i].command != NULL) {
        if(!strcmp(table[i].command, s)) {
            return table[i].token_type;
        }
        i++;
    }
    return 0;
}

char isTwoChar(char c1, char c2) {
    return ((*prog == c1) && (*(prog + 1) == c2));
}

void get_token() {
    char *temp;
    int i;

    *token = 0;
    token_type = END;
    if(stop || (prog == NULL)) {
        return;
    }
    temp = token;

    while(iswhite(*prog)) {
        prog++;
    }

    if(*prog == '\n') {
        prog++;
        *temp = '\n';
        temp++;
        *temp = 0;
        token_type = EOL;
    } else if(strchr("+-*^/%=;(),<>", *prog)) {
        if(isTwoChar('<', '=') || isTwoChar('>', '=') || isTwoChar('<', '>')) {
            i = 2;
        } else {
            i = 1;
        }
        while(i > 0) {
            *temp = *prog;
            prog++;
            temp++;
            i--;
        }
        *temp = 0;
        token_type = DELIMITER;
    } else if(*prog == '"') {
        prog++;
        while((*prog != '"') && (*prog != '\n') && (*prog != 0)) {
            *temp = *prog;
            temp++;
            prog++;
        }
        *temp = 0;
        if(*prog == '"') {
            prog++;
            token_type = QUOTE;
        } else {
            serror(1);
        }
    } else if(isdig(*prog)) {
        while(isdig(*prog)) {
            *temp = *prog;
            temp++;
            prog++;
        }
        *temp = 0;
        token_type = NUMBER;
    } else if(isalp(*prog)) {
        while(isalpdig(*prog)) {
            *temp = *prog;
            temp++;
            prog++;
        }
        *temp = 0;
        token_type = look_up(token);
        token_type = token_type ? token_type : VARIABLE;
    }
}

void find_eol() {
    if(stop) {
        return;
    }
    while((*prog != '\n') && (*prog != 0)) {
        prog++;
    }
    if(*prog == '\n') {
        prog++;
    }
    get_token();
}

void get_var_value(char *s, value *result) {
    result->int_val = variables[toupper(*s) - 'A'].int_val;
}

void set_var_value(char *s, value * result) {
    variables[toupper(*s) - 'A'].int_val = result->int_val;
}

void arith(char o, value * r, value * h) {
    int ex;

    switch(o) {
    case '-':
        r->int_val = r->int_val - h->int_val;
        break;
    case '+':
        r->int_val = r->int_val + h->int_val;
        break;
    case '*':
        r->int_val = r->int_val * h->int_val;
        break;
    case '/':
        r->int_val = r->int_val / h->int_val;
        break;
    case '%':
        r->int_val = r->int_val % h->int_val;
        break;
    case '^':
        ex = r->int_val;
        if(h->int_val != 0) {
            while(h->int_val > 1) {
                r->int_val = r->int_val * ex;
                h->int_val--;
            }
        } else {
            r->int_val = 1;
        }
        break;
    }
}

char isToken(int t) {
    if (token_type == t) {
        get_token();
        return 1;
    } else {
        return 0;
    }
}

char isDelim(char *s) {
    if ((token_type == DELIMITER) && (!strcmp(s, token))) {
        get_token();
        return 1;
    } else {
        return 0;
    }
}

void level3(value * result) {
    char  op;
    op = isDelim("-");
    if(isDelim("(")) {
        get_exp(result);
        if(!isDelim(")")) {
            serror(1);
        }
    } else {
        switch(token_type) {
        case VARIABLE:
            get_var_value(token, result);
            get_token();
            break;
        case NUMBER:
            result->int_val = atoi(token);
            get_token();
            break;
        default:
            serror(0);
        }
    }
    if(op) {
        result->int_val = -result->int_val;
    }
}

void level2(value * result) {
    value hold;

    level3(result);
    if(isDelim("^")) {
        level3(&hold);
        arith('^', result, &hold);
    }
}

void level1(value * result) {
    char  op;
    value hold;

    level2(result);
    op = *token;
    while(isDelim("*") || isDelim("/") || isDelim("%")) {
        level2(&hold);
        arith(op, result, &hold);
        op = *token;
    }
}

void get_exp(value * result) {
    char  op;
    value hold;

    result->int_val = 0;
    level1(result);
    op = *token;
    while(isDelim("+") || isDelim("-")) {
        level3(&hold);
        arith(op, result, &hold);
        op = *token;
    }
}

void bas_assign() {
    int var;
    char var_name[NAME_SIZE + 1];
    value val;

    val.int_val = 0;
    if(isalpha(*token)) {
        strncpy(var_name, token, NAME_SIZE);
        var_name[NAME_SIZE] = 0;
        get_token();
        if(isDelim("=")) {
            get_exp(&val);
            set_var_value(var_name, &val);
        } else {
            serror(3);
        }
    } else {
        serror(4);
    }
}

void bas_print() {
    value v;
    int len = 0, spaces;
    char last_delim;
    last_delim = 0;
    while (!stop) {
        if((token_type == EOL) || (token_type == END)) {
            break;
        }
        if(token_type == QUOTE) {
            printf(token);
            get_token();
        } else {
            get_exp(&v);
            printf("%d", v.int_val);
        }
        last_delim = *token;

        if(isDelim(";")) {
            last_delim = ';';
            printf("\t");
        } else if(isDelim(",")) {
            last_delim = ',';
            printf(" ");
        } else {
            break;
        }
    }

    if((token_type == EOL) || (token_type == END)) {
        if((last_delim != ';') && (last_delim != ',')) {
            printf("\n");
        }
    } else {
        serror(0);
    }
}

void scan_labels() {
    int addr;
    char *temp;
    size_t t;

    t = 0;
    while(t < NUM_LAB) {
        label_table[t].name[0] = 0;
        t++;
    }
    temp = prog;
    t = 0;
    get_token();
    while((*prog != 0) && (t < NUM_LAB)) {
        if(token_type == NUMBER) {
            strcpy(label_table[t].name, token);
            label_table[t].p = prog;
            t++;
        }
        find_eol();
    }
    stop = 0;
    prog = temp;
}

char *find_label(char *s) {
    size_t t;
    t = 0;
    while(t < NUM_LAB) {
        if(!strcmp(label_table[t].name, s)) {
            return label_table[t].p;
        }
        t++;
    }
    serror(7);
    return NULL;
}

void bas_if() {
    value x, y;
    int cond;
    char op[18];

    get_exp(&x);

    strncpy(op, token, 3);
    get_token();
    get_exp(&y);

    cond = 0;
    if(!strcmp(op, "<")) {
        cond =  (x.int_val < y.int_val);
    } else if(!strcmp(op, "<=")) {
        cond =  (x.int_val <= y.int_val);
    } else if(!strcmp(op, ">")) {
        cond = (x.int_val > y.int_val);
    } else if(!strcmp(op, ">=")) {
        cond = (x.int_val >= y.int_val);
    } else if(!strcmp(op, "<>")) {
        cond = (x.int_val != y.int_val);
    } else if(!strcmp(op, "=")) {
        cond = (x.int_val == y.int_val);
    } else {
        serror(0);
        return;
    }
    if(cond) {
        if(!isToken(THEN)) {
            serror(8);
        }
    } else {
        find_eol();
    }
}

void bas_for() {
    value start, target;
    char var_name[NAME_SIZE + 1];
    int  i;
    if(!isalpha(*token)) {
        serror(4);
        return;
    }
    strncpy(var_name, token, NAME_SIZE);
    var_name[NAME_SIZE] = 0;
    get_token();

    if(*token != '=') {
        serror(3);
        return;
    }
    get_token();
    get_exp(&start);
    set_var_value(var_name, &start);

    if(!isToken(TO)) {
        serror(9);
        return;
    }
    get_exp(&target);

    if(start.int_val <= target.int_val) {
        fstk_ptr++;
        strcpy(fstk[fstk_ptr].var, var_name);
        fstk[fstk_ptr].target = target;
        fstk[fstk_ptr].loc = prog;
    } else {
        i = 1;
        while(1) {
            if(isToken(FOR)) {
                i++;
            } else if(isToken(NEXT)) {
                i--;
                if(i == 0) {
                    break;
                }
            } else {
                get_token();
            }
        }
    }
}

void bas_next() {
    value v;
    if (fstk_ptr >= 0) {
        get_var_value(fstk[fstk_ptr].var, &v);
        v.int_val++;
        set_var_value(fstk[fstk_ptr].var, &v);
        if(v.int_val > fstk[fstk_ptr].target.int_val) {
            fstk_ptr--;
        } else {
            prog = fstk[fstk_ptr].loc;
            get_token();
        }
    } else {
        serror(11);
    }
}

void bas_input() {
    char var;

    if(token_type == QUOTE) {
        printf("%s ? ", token);
        get_token();
        if(*token == ',') {
            get_token();
        } else {
            serror(1);
        }
    } else {
        printf("? ");
    }
    var = toupper(*token) - 'A';
    scanf("%d", &variables[var]);
    get_token();
}


int main(int argc, char *argv[]) {
    char *p_buf;
    char *loc;

    if(argc != 2) {
        printf("Usage: %s filename.bas", argv[0]);
        exit(1);
    }

    if(!(p_buf = (char *) malloc(PROG_SIZE))) {
        printf("Memory allocation failure");
        exit(1);
    }
    if(!load_program(p_buf, argv[1])) {
        free(p_buf);
        exit(1);
    }
    stop = 0;
    prog = p_buf;
    scan_labels();
    fstk_ptr = -1;
    gstk_ptr = -1;
    get_token();
    while (!stop) {
        if(token_type == VARIABLE) {
            bas_assign();
        } else if(isToken(PRINT)) {
            bas_print();
        } else if(isToken(GOTO)) {
            if((loc = find_label(token)) != NULL) {
                prog = loc;
                get_token();
            }
        } else if(isToken(IF)) {
            bas_if();
        } else if(isToken(FOR)) {
            bas_for();
        } else if(isToken(NEXT)) {
            bas_next();
        } else if(isToken(INPUT)) {
            bas_input();
        } else if(isToken(GOSUB)) {
            if((loc = find_label(token)) != NULL) {
                gstk_ptr++;
                gstk[gstk_ptr] = prog;
                prog = loc;
                get_token();
            }
        } else if(isToken(RETURN)) {
            prog = gstk[gstk_ptr];
            gstk_ptr--;
            get_token();
        } else if(isToken(LET)) {
            bas_assign();
        } else if(isToken(END)) {
            stop = 1;
        } else {
            get_token();
        }
    }
    free(p_buf);
    return 0;
}
