#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <assert.h>

#define DOUBLEESCAPE(a) #a
#define ESCAPEQUOTE(a) DOUBLEESCAPE(a)

/*
 * Scheme object data type
 *
 */

enum ObjectType {
	T_FALSE,
	T_TRUE,
	T_EOF,
	T_SYMBOL,
	T_NUMBER,
	T_CHARACTER,
	T_STRING,
	T_NIL,
	T_CONS,
	
	T_CLOSURE,
	
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
			struct Object *args;
			struct Object *body;
			struct Object *env;
		} closure;
		struct {
			struct Object *ptr;
		} gc_fwd;
	};
};

/*
 * Prototypes
 */

// gc
#define MAX_CELLS 4096
struct Object *gc_from_heap;
struct Object *gc_to_heap;
void scheme_init_gc();
struct Object scheme_cons(struct Object car, struct Object cdr);
struct Object scheme_make_closure(struct Object args, struct Object body, struct Object env);
struct Root *scheme_gc_add_root(struct Object obj);
void scheme_gc_delete_root(struct Root *rt);
void scheme_gc();

// symbols
#define MAX_SYMBOLS 4096
char **symbol_table;
struct Object scheme_intern(char *name);
char *scheme_symbol_name(int id);
void scheme_init_symbols();

// globals
#define MAX_GLOBALS 4096
int *global_id_table;
struct Object *global_val_table;
int global_table_size = 0;
void scheme_init_globals();
int scheme_intern_global(int name);
struct Object scheme_get_global(int gid);
void scheme_set_global(int gid, struct Object val);

// display
int scheme_eq(struct Object x, struct Object y);
void scheme_display(struct Object x);

// reader
struct Object scheme_read(int *line_no);

// eval
struct Object scheme_eval(struct Object exp, struct Object env);

void scheme_init() {
	scheme_init_gc();
	scheme_init_symbols();
	scheme_init_globals();
}

/*
 * The heap
 *
 */

int gc_free = 0;

void scheme_init_gc() {
	gc_from_heap = calloc(MAX_CELLS, sizeof(struct Object));
	assert(gc_from_heap);
	gc_to_heap = calloc(MAX_CELLS, sizeof(struct Object));
	assert(gc_to_heap);
}

struct Object scheme_cons(struct Object car, struct Object cdr) {
	struct Object ret;
	
	if(gc_free + 2 > MAX_CELLS) {
		fprintf(stderr, "scheme_cons: ran out of cells.\n");
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

struct Object scheme_make_closure(struct Object args, struct Object body, struct Object env) {
	struct Object ret;
	
	if(gc_free + 3 > MAX_CELLS) {
		fprintf(stderr, "scheme_make_closure: ran out of cells.\n");
		exit(1);
	}

	ret = (struct Object){ .tag = T_CLOSURE };
	
	gc_from_heap[gc_free] = args;
	ret.closure.args = &gc_from_heap[gc_free];
	gc_free++;
	
	gc_from_heap[gc_free] = body;
	ret.closure.body = &gc_from_heap[gc_free];
	gc_free++;

	gc_from_heap[gc_free] = env;
	ret.closure.env = &gc_from_heap[gc_free];
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

#define DEBUG_GC

void scheme_gc() {
	struct Root *rt;
	int gid;
	
	gc_free = 0;
	gc_scan = 0;

#define SWAP(a,b) do { struct Object *tmp; tmp = a; a = b; b = tmp; } while(0)
	SWAP(gc_from_heap, gc_to_heap);
	
	for(rt = gc_roots; rt; rt = rt->next) {
		scheme_gc_forward(&rt->obj);
	}
	
	for(gid = 0; gid < global_table_size; gid++) {
		scheme_gc_forward(&global_val_table[gid]);
	}
	
	for(; gc_scan < gc_free; gc_scan++) {
		scheme_gc_forward(&gc_from_heap[gc_scan]);
	}
	
#ifdef DEBUG_GC
	printf("[GC] gc_free=%d\n", gc_free);
#endif
}

void scheme_gc_forward(struct Object *obj) {
	struct Object *old_car, *old_cdr;
	struct Object *old_args, *old_body, *old_env;

#ifdef DEBUG_GC
	scheme_display(*obj);
	puts("");
#endif

	switch(obj->tag) {
	case T_CONS:
		old_car = obj->cons.car;
		scheme_gc_forward(obj->cons.car);
		*old_car = (struct Object){ .tag = T_GC_FWD, .gc_fwd.ptr = obj->cons.car };

		old_cdr = obj->cons.cdr;
		scheme_gc_forward(obj->cons.cdr);
		*old_cdr = (struct Object){ .tag = T_GC_FWD, .gc_fwd.ptr = obj->cons.cdr };

		*obj = scheme_cons(*obj->cons.car, *obj->cons.cdr);

		break;
	case T_CLOSURE:
		old_args = obj->closure.args;
		scheme_gc_forward(obj->closure.args);
		*old_args = (struct Object){ .tag = T_GC_FWD, .gc_fwd.ptr = obj->closure.args };

		old_body = obj->closure.body;
		scheme_gc_forward(obj->closure.body);
		*old_body = (struct Object){ .tag = T_GC_FWD, .gc_fwd.ptr = obj->closure.body };

		old_env = obj->closure.env;
		scheme_gc_forward(obj->closure.env);
		*old_env = (struct Object){ .tag = T_GC_FWD, .gc_fwd.ptr = obj->closure.env };

		*obj = scheme_make_closure(*obj->closure.args, *obj->closure.body, *obj->closure.env);

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

int symbol_table_size = 0;

// provide sym_foo for various symbols
#define DO_SYMBOLS \
	DO_SYMBOL(quote) \
	DO_SYMBOL(quasiquote) \
	DO_SYMBOL(comma) \
	DO_SYMBOL(define) \
	DO_SYMBOL(lambda) \
	DO_SYMBOL(if) \
	DO_SYMBOL(begin)

#define DO_SYMBOL(name) struct Object sym_ ## name;
DO_SYMBOLS
#undef DO_SYMBOL

void scheme_init_symbols() {
	int i;
	char *slab;

	slab = calloc(MAX_SYMBOLS, 64);
	assert(slab);
	symbol_table = calloc(MAX_SYMBOLS, sizeof(char*));
	assert(symbol_table);
	for(i = 0; i < MAX_SYMBOLS; i++) {
		symbol_table[i] = slab + 64*i;
	}

#define INTERN_SYMBOL_X(name, name_str) sym_ ## name = scheme_intern(name_str);
#define DO_SYMBOL(name) INTERN_SYMBOL_X(name, ESCAPEQUOTE(name))
DO_SYMBOLS
#undef DO_SYMBOL
}

#undef DO_SYMBOLS

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

// TODO: How to check if a global is undefined?

void scheme_init_globals() {
	global_id_table = calloc(MAX_GLOBALS, sizeof(int));
	assert(global_id_table);
	global_val_table = calloc(MAX_GLOBALS, sizeof(struct Object));
	assert(global_val_table);
}

int scheme_intern_global(int id) {
	int i;
	
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

	case T_CLOSURE:
		return
			x.closure.args == y.closure.args &&
			x.closure.body == y.closure.body &&
			x.closure.env == y.closure.env;

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
//		fprintf(stdout, "%s:%d", scheme_symbol_name(x.symbol.id), x.symbol.id);
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
	case T_CLOSURE:
		fprintf(stdout, "#<closure>");
		break;
	case T_GC_FWD:
		fprintf(stdout, "#<GCFWD[");
//		scheme_display(*x.gc_fwd.ptr);
		fprintf(stdout, "]>");
		break;
	default:
		fprintf(stderr, "scheme_display: unknown object [%d].\n", x.tag);
		exit(1);
	}
}

/*
 * Reader
 *
 */

//#define DEBUG
#ifdef DEBUG

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
	else if(c == '(' || c == ')' || c == ' ' || c == '\n' || c == '\t') {
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
	x = scheme_cons(sym_quote, x);
	return x;

READ_LBL(read_quasiquote)
	x = scheme_read(line_no);
	x = scheme_cons(x, (struct Object){ .tag = T_NIL });
	x = scheme_cons(sym_quasiquote, x);
	return x;

READ_LBL(read_comma)
	x = scheme_read(line_no);
	x = scheme_cons(x, (struct Object){ .tag = T_NIL });
	x = scheme_cons(sym_comma, x);
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
//        (eq? (car exp) 'define)
//        (pair? (cdr exp))
//        (pair? (cddr exp)))
	return
		exp.tag == T_CONS &&
		scheme_eq(*exp.cons.car, sym_define) &&
		exp.cons.cdr->tag == T_CONS &&
		exp.cons.cdr->cons.cdr->tag == T_CONS;
}

int scheme_shape_if(struct Object exp) {
// (define (if? exp)
//   (and (pair? exp)
//        (eq? (car exp) 'if)
//        (pair? (cdr exp))
//        (pair? (cddr exp))
//        (pair? (cdddr exp))
//        (null? (cddddr exp))))
	return
		exp.tag == T_CONS &&
		scheme_eq(*exp.cons.car, sym_if) &&
		exp.cons.cdr->tag == T_CONS &&
		exp.cons.cdr->cons.cdr->tag == T_CONS &&
		exp.cons.cdr->cons.cdr->cons.cdr->tag == T_CONS &&
		exp.cons.cdr->cons.cdr->cons.cdr->cons.cdr->tag == T_NIL;
}

int scheme_shape_quote(struct Object exp) {
// (define (quote? exp)
//   (and (pair? exp)
//        (eq? (car exp) 'quote)
//        (pair? (cdr exp))
//        (null? (cddr exp))))
	return
		exp.tag == T_CONS &&
		scheme_eq(*exp.cons.car, sym_quote) &&
		exp.cons.cdr->tag == T_CONS &&
		exp.cons.cdr->cons.cdr->tag == T_NIL;
}

int scheme_shape_lambda(struct Object exp) {
// (define (lambda? exp)
//   (and (pair? exp)
//        (eq? (car exp) 'lambda)
//        (pair? (cdr exp))
//        (pair? (cddr exp))))
	return
		exp.tag == T_CONS &&
		scheme_eq(*exp.cons.car, sym_lambda) &&
		exp.cons.cdr->tag == T_CONS &&
		exp.cons.cdr->cons.cdr->tag == T_CONS;
}

int scheme_self_evaluating(enum ObjectType tag) {
	return
		tag == T_TRUE ||
		tag == T_FALSE ||
		tag == T_EOF ||
		tag == T_NUMBER ||
		tag == T_CHARACTER ||
		tag == T_STRING ||
		tag == T_NIL;
}

struct Object scheme_execute(struct Object exp, struct Object env) {
	if(scheme_shape_define(exp)) {
		struct Object def_name;
		struct Object def_body;
		int gid;
		
		def_name = *exp.cons.cdr->cons.car;
		def_body = *exp.cons.cdr->cons.cdr->cons.car;

		// TODO: desugar (define (f args ...) ...)

		assert(def_name.tag == T_SYMBOL);
		gid = scheme_intern_global(def_name.symbol.id);
		exp = scheme_eval(def_body, env);
		scheme_set_global(gid, exp);
		// GC note: exp is now rooted

		return exp;
	}
	else {
		exp = scheme_eval(exp, env);

		scheme_display(exp);
		puts("");
		
		return exp;
	}
}

// TODO: this is not ok. we cannot mutate like this
void scheme_evlist_bang(struct Object exp, struct Object env) {
evlist_loop:
	if(exp.tag == T_NIL)
		return;

	if(exp.tag != T_CONS) {
		fprintf(stderr, "scheme_evlist: not a list [%d].\n", exp.tag);
		exit(1);
	}
	
	*exp.cons.car = scheme_eval(*exp.cons.car, env);
	
	exp = *exp.cons.cdr;
	goto evlist_loop;
}

struct Object scheme_eval(struct Object exp, struct Object env) {
	int gid;
	struct Object res;

eval_continue:
	if(scheme_self_evaluating(exp.tag)) {
		return exp;
	}
	else if(exp.tag == T_SYMBOL) {
		gid = scheme_intern_global(exp.symbol.id);

		return scheme_get_global(gid);
	}
	else if(exp.tag == T_CONS && scheme_eq(*exp.cons.car, sym_quote)) {
		assert(scheme_shape_quote(exp));
		
		return *exp.cons.cdr->cons.car;
	}
	else if(exp.tag == T_CONS && scheme_eq(*exp.cons.car, sym_begin)) {
		do {
			exp = *exp.cons.cdr;
			assert(exp.tag == T_CONS);
			
			res = scheme_eval(*exp.cons.car, env);
			// GC note: allow res to be GC'd

			if(exp.cons.cdr->tag == T_NIL)
				return res;
		} while(1);
	}
	else if(exp.tag == T_CONS && scheme_eq(*exp.cons.car, sym_if)) {
		struct Object if_test, if_consequent, if_antecedent;

		assert(scheme_shape_if(exp));

		if_test = *exp.cons.cdr->cons.car;
		if_consequent = *exp.cons.cdr->cons.cdr->cons.car;
		if_antecedent = *exp.cons.cdr->cons.cdr->cons.cdr->cons.car;
		// GC note: allow if_test to be GC'd

		res = scheme_eval(if_test, env);
		if(res.tag == T_FALSE) {
			exp = if_antecedent;
		}
		else {
			exp = if_consequent;
		}
		
		goto eval_continue;
	}
	else if(exp.tag == T_CONS && scheme_eq(*exp.cons.car, sym_lambda)) {
		struct Object lambda_args, lambda_body;
				
		assert(scheme_shape_lambda(exp));
		
		lambda_args = *exp.cons.cdr->cons.car;
		lambda_body = *exp.cons.cdr->cons.cdr;
		lambda_body = *exp.cons.cdr->cons.cdr->cons.car;
		// TODO: implicit begin
		// GC note: implicit begin needs to be rooted
		
		return scheme_make_closure(lambda_args, lambda_body, env);
	}
	else if(exp.tag == T_CONS) {
		// function application
		
		// TODO: this one will have to set up a root, then nondestructively evlist
		
		// GC note: we overwrite the elements of the list "exp"
		// so everything is rooted and preserved
		//exp = scheme_evlist_bang(exp, env);
		
		// TODO: perform the application
		
		return sym_if;
	}

	fprintf(stderr, "scheme_eval: error unhandled object type %d.\n", exp.tag);
	exit(1);
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

	scheme_init();
	
	do {
		x = scheme_read(&line_no);
		if(x.tag == T_EOF) break;
		rt = scheme_gc_add_root(x);

		scheme_display(rt->obj);
		puts("");

		scheme_execute(rt->obj, (struct Object){ .tag = T_NIL });
		scheme_gc_delete_root(rt);
		scheme_gc();
	} while(1);
	
	return 0;
}

