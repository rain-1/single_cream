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
	T_CHARACTER,
	T_STRING,
	T_NIL,
	T_CONS,
	
	T_GC_FWD,
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
			char val;
		} character;
		struct {
			int len;
			char *text;
		} string;
		struct {
			struct Object *car;
			struct Object *cdr;
		} cons;
		struct {
			struct Object *ptr;
		} gc_fwd;
	};
};

/*
 * Prototypes
 */

struct Object scheme_cons(struct Object car, struct Object cdr);
struct Root *scheme_gc_add_root(struct Object obj);
void scheme_gc_delete_root(struct Root *rt);
void scheme_gc();

struct Object scheme_intern(char *name);
char *scheme_symbol_name(int id);

void scheme_display(struct Object x);

struct Object scheme_read(int *line_no);

int scheme_global_intern(char *name);
struct Object scheme_get_global(int gid);
void scheme_set_global(int gid, struct Object val);

void scheme_eval(struct Object exp, struct Object env);

/*
 * The heap
 *
 */

#define MAX_CELLS 4096
struct Object gc_heap_a[MAX_CELLS];
struct Object gc_heap_b[MAX_CELLS];

struct Object *gc_from_heap = gc_heap_a;
struct Object *gc_to_heap = gc_heap_b;
int gc_free = 0;

struct Object scheme_cons(struct Object car, struct Object cdr) {
	struct Object ret;
	
	if(gc_free + 2 > MAX_CELLS) {
		fprintf(stderr, "scheme_cons: ran out of cons cells.\n");
		exit(1);
	}

	ret = (struct Object){ .tag = T_CONS };
	
	gc_from_heap[gc_free] = car;
	ret.cons.car = &gc_from_heap[gc_free];
	gc_free++;
	
	gc_from_heap[gc_free] = cdr;
	ret.cons.cdr = &gc_from_heap[gc_free];
	gc_free++;

	return ret;
}

struct Root {
	struct Root *prev;
	struct Root *next;
	struct Object obj;
};

struct Root *gc_roots = NULL;
int gc_scan;

struct Root *scheme_gc_add_root(struct Object obj) {
	struct Root *rt;
	
	rt = malloc(sizeof(struct Root));
	rt->prev = NULL;
	rt->next = gc_roots;
	rt->obj = obj;
	if(gc_roots) gc_roots->prev = rt;
	
	gc_roots = rt;
	
	return rt;
}

void scheme_gc_delete_root(struct Root *rt) {
	if(rt->prev) {
		rt->prev->next = rt->next;
	}
	else {
		// this is the case where gc_roots = rt
		// to not lose all of our other roots
		// we will need to update gc_reets
		gc_roots = rt->next;
	}

	if(rt->next) {
		rt->next->prev = rt->prev;
	}

	rt->prev = NULL;
	rt->next = NULL;
	rt->obj = (struct Object){ .tag = T_NIL };
	free(rt);
}

void scheme_gc_forward(struct Object *obj);

//#define DEBUG_GC

void scheme_gc() {
	struct Root *rt;
	
	gc_free = 0;
	gc_scan = 0;

#define SWAP(a,b) do { struct Object *tmp; tmp = a; a = b; b = tmp; } while(0)
	SWAP(gc_from_heap, gc_to_heap);
	
	for(rt = gc_roots; rt; rt = rt->next) {
		scheme_gc_forward(&rt->obj);
	}
	
	// TODO: also process the globals as roots
	
	for(; gc_scan < gc_free; gc_scan++) {
		scheme_gc_forward(&gc_from_heap[gc_scan]);
	}
	
#ifdef DEBUG_GC
	printf("[GC] gc_free=%d\n", gc_free);
#endif
}

void scheme_gc_forward(struct Object *obj) {
	struct Object *old_car, *old_cdr;

#ifdef DEBUG_GC
	scheme_display(*obj);
	puts("");
#endif

	switch(obj->tag) {
	case T_CONS:
		old_car = obj->cons.car;
		old_cdr = obj->cons.cdr;
		*obj = scheme_cons(*old_car, *old_cdr);
		*old_car = (struct Object){ .tag = T_GC_FWD, .gc_fwd.ptr = obj->cons.car };
		*old_cdr = (struct Object){ .tag = T_GC_FWD, .gc_fwd.ptr = obj->cons.cdr };
		break;
	case T_GC_FWD:
		*obj = *(obj->gc_fwd.ptr);
		break;
	default:
		break;
	}
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

int scheme_eq(struct Object x, struct Object y) {
	if(x.tag != y.tag) return 0;
	
	switch(x.tag) {
	case T_TRUE:
	case T_FALSE:
	case T_EOF:
	case T_NIL:
		return 1;

	case T_SYMBOL:
		return x.symbol.id == y.symbol.id;
	case T_NUMBER:
		return x.number.val == y.number.val;
	case T_CHARACTER:
		return x.character.val == y.character.val;

	case T_CONS:
		return x.cons.car == y.cons.car && x.cons.cdr == y.cons.cdr;
	case T_STRING:
		return x.string.len == y.string.len && x.string.text == y.string.text;
	default:
		return 0;
	}
}

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
	case T_CHARACTER:
		fprintf(stdout, "#\\%c", x.character.val); // TODO: escaping special characters
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

struct Object scheme_read_many(int *line_no);

#define GETCHAR(c, port) do { c = fgetc(port); if(c == '\n') ++*line_no; } while(0)

// TODO: bounds check on input buffers
struct Object scheme_read(int *line_no) {
	char c;
	
	goto read_start;

READ_LBL(read_start)
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		return (struct Object){ .tag = T_EOF };
	case ' ':
	case '\n':
	case '\t':
		goto read_start;
	case ';':
		goto read_comment;
	case '(':
		return scheme_read_many(line_no);
	case ')':
		fprintf(stderr, "scheme_read: too many close parenthesis on line %d.\n", *line_no);
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

READ_LBL(read_comment)
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		return (struct Object){ .tag = T_EOF };
	case '\n':
		goto read_start;
	default:
		goto read_comment;
	}

READ_LBL(read_hash)
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		fprintf(stderr, "scheme_read: early EOF in read_hash on line %d.\n", *line_no);
		exit(1);
	case 't':
		return (struct Object){ .tag = T_TRUE };
	case 'f':
		return (struct Object){ .tag = T_FALSE };
	case '\\':
		goto read_hash_char;
	default:
		fprintf(stderr, "scheme_read: error inside read_hash on line %d.\n", *line_no);
		exit(1);
	}

READ_LBL(read_hash_char)
	// TODO: handle #\s pace #\t ab #\n ewline correctly
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		fprintf(stderr, "scheme_read: early EOF in read_hash_char on line %d.\n", *line_no);
		exit(1);
	default:
		return (struct Object){ .tag = T_CHARACTER, .character.val = c };
	}

	char read_string_buf[64];
	int read_string_buflen;
READ_LBL(read_string)
	read_string_buf[0] = '\0';
	read_string_buflen = 0;
READ_LBL(read_string_char)
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		fprintf(stderr, "scheme_read: early EOF in read_string on line %d.\n", *line_no);
		exit(1);
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
	GETCHAR(c, stdin);
	read_string_buf[read_string_buflen++] = c;
	read_string_buf[read_string_buflen] = '\0';
	goto read_string_char;

	int read_number_negative;
	char read_number_buf[64];
	int read_number_buflen;
	int val;
READ_LBL(read_number)
	if(c == EOF) {
		fprintf(stderr, "scheme_read: early EOF in read_number on line %d.\n", *line_no);
		exit(1);
	}
	else if(c == '-') {
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
	GETCHAR(c, stdin);
	if(c == EOF) {
		fprintf(stderr, "scheme_read: early EOF in read_number_digit on line %d.\n", *line_no);
		exit(1);
	}
	else if(isdigit(c)) {
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
	GETCHAR(c, stdin);
	if(c == EOF) {
		fprintf(stderr, "scheme_read: early EOF in read_symbol_char on line %d.\n", *line_no);
		exit(1);
	}
	else if(c == ')' || c == ' ' || c == '\n' || c == '\t') {
		ungetc(c, stdin);
		return scheme_intern(read_symbol_buf);
	}
	read_symbol_buf[read_symbol_buflen++] = c;
	read_symbol_buf[read_symbol_buflen] = '\0';
	goto read_symbol_char;

	struct Object x;
READ_LBL(read_quote)
	x = scheme_read(line_no);
	x = scheme_cons(x, (struct Object){ .tag = T_NIL });
	x = scheme_cons(scheme_intern("quote"), x);
	return x;

READ_LBL(read_quasiquote)
	x = scheme_read(line_no);
	x = scheme_cons(x, (struct Object){ .tag = T_NIL });
	x = scheme_cons(scheme_intern("quasiquote"), x);
	return x;

READ_LBL(read_comma)
	x = scheme_read(line_no);
	x = scheme_cons(x, (struct Object){ .tag = T_NIL });
	x = scheme_cons(scheme_intern("comma"), x);
	return x;	
}

struct Object scheme_read_many(int *line_no) {
	char c;
	struct Object x, xs;

READ_LBL(read_many)
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		fprintf(stderr, "scheme_read_many: early EOF on line %d.\n", *line_no);
		exit(1);
	case ' ':
	case '\n':
	case '\t':
		goto read_many;
	case ';':
		goto read_comment;
	case ')':
		return (struct Object){ .tag = T_NIL };
	case '.':
		x = scheme_read(line_no);
		goto read_finish;
	default:
		ungetc(c, stdin);
		x = scheme_read(line_no);
		xs = scheme_read_many(line_no);
		return scheme_cons(x, xs);
	}

READ_LBL(read_comment)
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		fprintf(stderr, "scheme_read_many: early EOF on line %d.\n", *line_no);
		exit(1);
	case '\n':
		goto read_many;
	default:
		goto read_comment;
	}

READ_LBL(read_finish)
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		fprintf(stderr, "scheme_read_many: early EOF in read_finish on line %d.\n", *line_no);
		exit(1);
	case ' ':
	case '\n':
	case '\t':
		goto read_finish;
	case ')':
		return x;
	default:
		fprintf(stderr, "scheme_read_many: error multiple items after dot on line %d.\n", *line_no);
		exit(1);
	}
}

/*
 * Evaluator
 *
 */

int scheme_shape_define(struct Object exp) {
// (define (def? exp)
//   (and (pair? exp)
//        (eq? 'define (car exp))
//        (pair? (cdr exp))
//        (pair? (cddr exp)))
	return
		exp.tag == T_CONS &&
		exp.cons.car->tag == T_SYMBOL &&
		scheme_eq(scheme_intern("define"), *exp.cons.car) &&
		exp.cons.cdr->tag == T_CONS &&
		exp.cons.cdr->cons.cdr->tag == T_CONS;
}

void scheme_eval(struct Object exp, struct Object env) {
	if(scheme_shape_define(exp)) {
		puts("def");
	}
	else {
		puts("exp");
	}
}

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
	struct Root *rt;
	
	int line_no = 0;
	
	do {
		x = scheme_read(&line_no);
		if(x.tag == T_EOF) break;

		rt = scheme_gc_add_root(x);

		scheme_display(rt->obj);
		puts("");

		scheme_eval(rt->obj, (struct Object){ .tag = T_NIL });

		scheme_gc();
		
//		scheme_display(rt->obj);
//		puts("");

		scheme_gc_delete_root(rt);
	} while(1);
	
	return 0;
}

