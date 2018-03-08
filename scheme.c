#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
 * Scheme object data type
 *
 */

enum ObjectType {
	T_TRUE,
	T_FALSE,
	T_EOF,
	T_SYMBOL,
	T_NUMBER,
	T_STRING,
	T_NIL,
	T_CONS,
};

struct Object {
	enum ObjectType tag;
	union {
		struct {
			int id;
		} symbol;
		struct {
			int val;
		} number;
		struct {
			int len;
			char *text;
		} string;
		struct {
			struct Object *car;
			struct Object *cdr;
		} cons;
	};
};

/*
 * The rails
 * https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-33.html#%_sec_5.3
 *
 */

#define MAX_CONSES 4096
struct Object the_cars_array[MAX_CONSES];
struct Object the_cdrs_array[MAX_CONSES];
struct Object new_cars_array[MAX_CONSES];
struct Object new_cdrs_array[MAX_CONSES];
struct Object *the_cars = the_cars_array;
struct Object *the_cdrs = the_cdrs_array;
struct Object *new_cars = new_cars_array;
struct Object *new_cdrs = new_cdrs_array;
int conses_size = 0;

struct Object scheme_cons(struct Object car, struct Object cdr) {
	struct Object ret;
	
	if(conses_size == MAX_CONSES) {
		fprintf(stderr, "scheme_cons: ran out of cons cells.\n");
	}
	
	the_cars[conses_size] = car;
	the_cdrs[conses_size] = cdr;

	ret = (struct Object){ .tag = T_CONS, .cons.car = &the_cars[conses_size], .cons.cdr = &the_cdrs[conses_size] };
	conses_size++;
	
	return ret;
}

void scheme_gc() {
	// TODO
}


/*
 * Symbol table
 */

#define MAX_SYMBOLS 4096
char symbol_table[MAX_SYMBOLS][64];
int symbol_table_size = 0;

struct Object scheme_intern(char *name) {
	int i;
	
	for(i = 0; i < symbol_table_size; i++) {
		if(!strcmp(symbol_table[i], name)) {
			return (struct Object){ .tag = T_SYMBOL, .symbol.id = i };
		}
	}
	
	if(i == MAX_SYMBOLS) {
		fprintf(stderr, "scheme_intern: ran out of space for symbols.\n");
		exit(1);
	}
	
	strncpy(symbol_table[i], name, 64);
	symbol_table_size++;
	
	return (struct Object){ .tag = T_SYMBOL, .symbol.id = i };
}

char *scheme_symbol_name(int id) {
	if(!(0 <= id && id < symbol_table_size)) {
		fprintf(stderr, "scheme_symbol_name: out of bounds access.\n");
		exit(1);
	}

	return symbol_table[id];
}

/*
 * Global Variables
 *
 */

#define MAX_GLOBALS 4096
int global_id_table[MAX_GLOBALS];
struct Object global_val_table[MAX_GLOBALS];
int global_table_size = 0;

int scheme_global_intern(char *name) {
	int id, i;
	
	id = scheme_intern(name).symbol.id;
	
	for(i = 0; i < global_table_size; i++) {
		if(global_id_table[i] == id) {
			return i;
		}
	}
	
	if(i == MAX_GLOBALS) {
		fprintf(stderr, "scheme_global_allocate: ran out of space for globals.\n");
		exit(1);
	}
	
	global_id_table[i] = id;
	global_val_table[i] = (struct Object){ .tag = T_FALSE };

	global_table_size++;
	
	return i;
}

struct Object scheme_get_global(int gid) {
	if(!(0 <= gid && gid < global_table_size)) {
		fprintf(stderr, "scheme_get_global: out of bounds access.\n");
		exit(1);
	}

	return global_val_table[gid];
}

void scheme_set_global(int gid, struct Object val) {
	if(!(0 <= gid && gid < global_table_size)) {
		fprintf(stderr, "scheme_set_global: out of bounds access.\n");
		exit(1);
	}

	global_val_table[gid] = val;
}

/*
 * Display
 *
 */

void scheme_display(struct Object x) {
	int i;

	switch(x.tag) {
	case T_TRUE:
		fprintf(stdout, "#t");
		break;
	case T_FALSE:
		fprintf(stdout, "#f");
		break;
	case T_EOF:
		fprintf(stdout, "#<EOF>");
		break;
	case T_SYMBOL:
		fprintf(stdout, "%s", scheme_symbol_name(x.symbol.id));
		break;
	case T_NUMBER:
		fprintf(stdout, "%d", x.number.val);
		break;
	case T_STRING:
		fprintf(stdout, "\"");
		for(i = 0; i < x.string.len; i++) {
			if(x.string.text[i] == '\\' || x.string.text[i] == '"') {
				fprintf(stdout, "\\%c", x.string.text[i]);
			}
			else {
				fprintf(stdout, "%c", x.string.text[i]);
			}
		}
		fprintf(stdout, "\"");
		break;
	case T_NIL:
		fprintf(stdout, "()");
		break;
	case T_CONS:
		fprintf(stdout, "(");
loop:
		scheme_display(*x.cons.car);
		switch(x.cons.cdr->tag) {
		case T_NIL:
			fprintf(stdout, ")");
			break;
		case T_CONS:
			fprintf(stdout, " ");
			x = *x.cons.cdr;
			goto loop;
		default:
			fprintf(stdout, " . ");
			scheme_display(*x.cons.cdr);
			fprintf(stdout, ")");
			break;
		}
		break;
	default:
		fprintf(stderr, "scheme_display: unknown object.\n");
		exit(1);
	}
}

/*
 * Reader
 *
 */

//#define DEBUG
#ifdef DEBUG

#define DOUBLEESCAPE(a) #a
#define ESCAPEQUOTE(a) DOUBLEESCAPE(a)
#define READ_LBL_X(lbl, lblnm) lbl: fprintf(stderr, "[DEBUG] scheme_read: %s\n", lblnm);
#define READ_LBL(lbl) READ_LBL_X(lbl, ESCAPEQUOTE(lbl))

#else

#define READ_LBL(lbl) lbl:

#endif

struct Object scheme_read_many();

// TODO: check EOF on each fgetc
// TODO: bounds check on input buffers
struct Object scheme_read() {
	char c;
	
	goto read_start;

READ_LBL(read_start)
	c = fgetc(stdin);
	switch(c) {
	case EOF:
		return (struct Object){ .tag = T_EOF };
	case ' ':
	case '\n':
	case '\t':
		goto read_start;
	case '(':
		return scheme_read_many();
	case ')':
		fprintf(stderr, "scheme_read: too many close parenthesis.");
		exit(1);
	case '#':
		goto read_hash;
	case '"':
		goto read_string;
	case '\'':
		goto read_quote;
	case '`':
		goto read_quasiquote;
	case ',':
		goto read_comma;
	default:
		if('-' == c || isdigit(c))
			goto read_number;
		else
			goto read_symbol;
	}

READ_LBL(read_hash)
	c = fgetc(stdin);
	switch(c) {
	case 't':
		return (struct Object){ .tag = T_TRUE };
	case 'f':
		return (struct Object){ .tag = T_FALSE };
	default:
		fprintf(stderr, "scheme_read: error inside read_hash.");
		exit(1);
	}

	char read_string_buf[64];
	int read_string_buflen;
READ_LBL(read_string)
	read_string_buf[0] = '\0';
	read_string_buflen = 0;
READ_LBL(read_string_char)
	c = fgetc(stdin);
	switch(c) {
	case '\\':
		goto read_string_esc_char;
	case '"':
		return (struct Object){ .tag = T_STRING, .string.len = strlen(read_string_buf), .string.text = strdup(read_string_buf) };
	default:
		read_string_buf[read_string_buflen++] = c;
		read_string_buf[read_string_buflen] = '\0';
		goto read_string_char;
	}
READ_LBL(read_string_esc_char)
	c = fgetc(stdin);
	read_string_buf[read_string_buflen++] = c;
	read_string_buf[read_string_buflen] = '\0';
	goto read_string_char;

	int read_number_negative;
	char read_number_buf[64];
	int read_number_buflen;
	int val;
READ_LBL(read_number)
	if(c == '-') {
		read_number_negative = 1;
		read_number_buf[0] = '\0';
		read_number_buflen = 0;
	}
	else {
		read_number_negative = 0;
		read_number_buf[0] = c;
		read_number_buf[1] = '\0';
		read_number_buflen = 1;
	}

READ_LBL(read_number_digit)
	c = fgetc(stdin);
	if(isdigit(c)) {
		read_number_buf[read_number_buflen++] = c;
		read_number_buf[read_number_buflen] = '\0';
		goto read_number_digit;
	}
	else {
		ungetc(c, stdin);
		val = atoi(read_number_buf);
		return (struct Object){ .tag = T_NUMBER, .number.val = read_number_negative ? -val : val };
	}

	char read_symbol_buf[64];
	int read_symbol_buflen;
READ_LBL(read_symbol)
	read_symbol_buf[0] = c;
	read_symbol_buf[1] = '\0';
	read_symbol_buflen = 1;
READ_LBL(read_symbol_char)
	c = fgetc(stdin);
	if(c == ')' || c == ' ' || c == '\n' || c == '\t') {
		ungetc(c, stdin);
		return scheme_intern(read_symbol_buf);
	}
	read_symbol_buf[read_symbol_buflen++] = c;
	read_symbol_buf[read_symbol_buflen] = '\0';
	goto read_symbol_char;

	struct Object x;
READ_LBL(read_quote)
	x = scheme_read();
	x = scheme_cons(x, (struct Object){ .tag = T_NIL });
	x = scheme_cons(scheme_intern("quote"), x);
	return x;

READ_LBL(read_quasiquote)
	x = scheme_read();
	x = scheme_cons(x, (struct Object){ .tag = T_NIL });
	x = scheme_cons(scheme_intern("quasiquote"), x);
	return x;

READ_LBL(read_comma)
	x = scheme_read();
	x = scheme_cons(x, (struct Object){ .tag = T_NIL });
	x = scheme_cons(scheme_intern("comma"), x);
	return x;	
}

struct Object scheme_read_many() {
	char c;
	struct Object x, xs;

READ_LBL(read_many)
	c = fgetc(stdin);
	switch(c) {
	case ' ':
	case '\n':
	case '\t':
		goto read_many;
	case ')':
		return (struct Object){ .tag = T_NIL };
	case '.':
		x = scheme_read();
		goto read_finish;
	default:
		ungetc(c, stdin);
		x = scheme_read();
		xs = scheme_read_many();
		return scheme_cons(x, xs);
	}

READ_LBL(read_finish)
	c = fgetc(stdin);
	switch(c) {
	case ' ':
	case '\n':
	case '\t':
		goto read_finish;
	case ')':
		return x;
	default:
		fprintf(stderr, "scheme_read_many: error multiple items after dot.");
		exit(1);
	}
}

/*
 * Closure Conversion and Flattening
 *
 */

/*
 * Evaluator
 *
 */

/*
 * Main
 *
 */

int main(int argc, char **argv) {
	// read an expression
	// compile it, adding the flattened code to the code store
	// execute the resulting definition or expression
	// until EOF

	struct Object x;
	
	do {
		x = scheme_read();
		if(x.tag == T_EOF) break;
		scheme_display(x);
		puts("");
	} while(1);
	
	return 0;
}

