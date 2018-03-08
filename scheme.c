#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Scheme object data type
 *
 */

enum ObjectType {
	T_TRUE,
	T_FALSE,
	T_SYMBOL,
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

	ret = (struct Object){ .tag = T_CONS, .cons.car = &the_cars[conses_size], .cons.cdr = &the_cars[conses_size] };
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
	case T_SYMBOL:
		fprintf(stdout, "%s", scheme_symbol_name(x.symbol.id));
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
	
	return 0;
}

