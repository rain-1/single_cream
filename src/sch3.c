#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/*
 * SECTION Declarations
 */

enum Tag {
	TAG_FALSE	= 0,
	TAG_TRUE	= 1,
	TAG_EOF		= 2,
	TAG_SYMBOL	= 3,
	TAG_NUMBER	= 4,
	TAG_CHARACTER	= 5,
	TAG_NIL		= 7,
	TAG_CONS	= 8,
	TAG_CLOSURE	= 9,
	TAG_BUILTIN	= 10,
	TAG_STRING	= 11,
	TAG_VECTOR	= 12,
	TAG_PORT	= 13,
	
	TAG_GC		= 0xFF,
};

struct Obj {
	enum Tag tag;
	union {
		struct { int id; } symbol;
		struct { int val; } number;
		struct { char val; } character;
		struct { struct Obj *car, *cdr; } cons;
		struct { struct Obj *args, *env, *body; } closure;
		struct { int n_args; struct Obj (*impl)(struct Obj *args); } builtin;
		struct { struct Obj *it; } string;
		struct { struct Obj *it; } vector;
		struct { FILE *fptr; } port;
		struct { struct Obj *fwd; } gc;
	};
};

#define SEMISPACE_SIZE (1<<17)
struct Obj *gc_live_space, *gc_dead_space;
struct Obj *gc_free_ptr, *gc_scan_ptr;
#define ROOTSTACK_SIZE (1<<16)
struct Obj *gc_root_stack[ROOTSTACK_SIZE];
int gc_root_stack_height = 0;

#define MAX_SYMBOLS (1<<10)
char **symbol_table;

#define MAX_BUILTIN_ARGS 5

// GC_OFF = 1 -> off
// GC_OFF = 0 -> on
#define GC_OFF 0

struct Obj globals;

void scheme_init(void);

void scheme_gc_init(void);
struct Obj *scheme_gc_alloc(int cells);
void scheme_gc(void);

void scheme_root_push_value(struct Obj *obj);
void scheme_root_push(struct Obj *obj);
void scheme_root_pop();

void scheme_symbol_init(void);
struct Obj scheme_symbol_intern(char *name);
char *scheme_symbol_name(int id);

void scheme_read(struct Obj *rt, int *line_no);

struct Obj scheme_cons(struct Obj *x, struct Obj *y);
struct Obj scheme_closure(struct Obj *args, struct Obj *env, struct Obj *body);
void scheme_display(FILE *fptr, struct Obj *x);
struct Obj scheme_eq(struct Obj *x, struct Obj *y);
struct Obj scheme_append(struct Obj *xs, struct Obj *ys);
struct Obj scheme_assoc(struct Obj *key, struct Obj *table);
struct Obj scheme_evlist(struct Obj *exps, struct Obj *env);
struct Obj scheme_eval(struct Obj *exp, struct Obj *env);

void scheme_builtins_init(void);
void scheme_constants_init(void);

struct Obj const_false, const_true, const_eof, const_nil, const_stdout, const_stderr;
void scheme_init(void) {
	const_false = (struct Obj){ .tag = TAG_FALSE };
	const_true = (struct Obj){ .tag = TAG_TRUE };
	const_eof = (struct Obj){ .tag = TAG_EOF };
	const_nil = (struct Obj){ .tag = TAG_NIL };
	scheme_gc_init();
	scheme_root_push(&globals);
	scheme_symbol_init();
	scheme_builtins_init();
	scheme_constants_init();
}


/*
 * SECTION Managing GC Roots
 */

void scheme_root_push_value(struct Obj *obj) {
	assert(gc_root_stack_height < ROOTSTACK_SIZE);
	gc_root_stack[gc_root_stack_height] = obj;
	gc_root_stack_height++;
}

void scheme_root_push(struct Obj *obj) {
	*obj = const_nil;
	scheme_root_push_value(obj);
}

void scheme_root_pop() {
	gc_root_stack[gc_root_stack_height] = NULL;
	gc_root_stack_height--;
	assert(gc_root_stack_height >= 0);
}


/*
 * SECTION Garbage Collector
 */

void scheme_gc_forward(struct Obj *obj);
struct Obj *scheme_gc_copy(struct Obj *obj);

void scheme_gc_init(void) {
	gc_live_space = calloc(SEMISPACE_SIZE, sizeof(struct Obj));
	gc_dead_space = calloc(SEMISPACE_SIZE, sizeof(struct Obj));
	assert(gc_live_space);
	assert(gc_dead_space);
	gc_free_ptr = gc_live_space;
	gc_scan_ptr = NULL;
}

struct Obj *scheme_gc_alloc_internal(int cells, int gc_loop_check) {
	struct Obj *res = gc_free_ptr;
	if(gc_free_ptr + cells < gc_live_space + SEMISPACE_SIZE) {
		gc_free_ptr += cells;
		return res;
	}
	else if(gc_loop_check) {
		fprintf(stderr, "gc: totally out of memory\n");
		exit(1);
	}
	else {
		scheme_gc();
		return scheme_gc_alloc_internal(cells, 1);
	}
}

struct Obj *scheme_gc_alloc(int cells) {
	return scheme_gc_alloc_internal(cells, GC_OFF);
}

#define SWAP(x,y) do { struct Obj *tmp = x; x = y; y = tmp; } while(0)
void scheme_gc(void) {
	int i;

	SWAP(gc_live_space, gc_dead_space);
	gc_free_ptr = gc_live_space;
	gc_scan_ptr = gc_live_space;
	
	for(i = 0; i < gc_root_stack_height; i++)
		scheme_gc_forward(gc_root_stack[i]);

	for(; gc_scan_ptr < gc_free_ptr; gc_scan_ptr++)
		scheme_gc_forward(gc_scan_ptr);
}

void scheme_gc_forward(struct Obj *obj) {
	switch(obj->tag) {
	case TAG_CONS:
		obj->cons.car = scheme_gc_copy(obj->cons.car);
		obj->cons.cdr = scheme_gc_copy(obj->cons.cdr);
		break;
	case TAG_CLOSURE:
		obj->closure.args = scheme_gc_copy(obj->closure.args);
		obj->closure.env = scheme_gc_copy(obj->closure.env);
		obj->closure.body = scheme_gc_copy(obj->closure.body);
		break;
	case TAG_STRING:
		obj->string.it = scheme_gc_copy(obj->string.it);
		break;
	case TAG_VECTOR:
		obj->vector.it = scheme_gc_copy(obj->vector.it);
		break;
	case TAG_GC:
		fprintf(stderr, "gc: error gc_forward\n");
		exit(1);
		break;
	default:
		break;
	}
}

int scheme_gc_dead(struct Obj *obj) {
	return gc_dead_space <= obj && obj < gc_dead_space + SEMISPACE_SIZE;
}

struct Obj *scheme_gc_copy(struct Obj *obj) {
	struct Obj *res;
	
	if(obj->tag == TAG_GC) {
		return obj->gc.fwd;
	}
	if(scheme_gc_dead(obj)) {
		res = scheme_gc_alloc(1);
		*res = *obj;
		*obj = (struct Obj){ .tag = TAG_GC, .gc.fwd = res };
		return res;
	}
	else {
		//fprintf(stderr, "gc: error in gc_copy\n");
		//scheme_display(obj);
		//exit(1);
		return obj;
	}
}


/*
 * SECTION Symbol table
 */

int symbol_table_size = 0;

// provide sym_foo for commonly used symbols
#define DO_SYMBOLS \
	DO_SYMBOL(quote) \
	DO_SYMBOL(quasiquote) \
	DO_SYMBOL(unquote) \
	DO_SYMBOL(define) \
	DO_SYMBOL(lambda) \
	DO_SYMBOL(if) \
	DO_SYMBOL(begin) \
	DO_SYMBOL(preprocess) \
	DO_SYMBOL(stdout) \
	DO_SYMBOL(stderr)

#define DO_SYMBOL(name) struct Obj sym_ ## name;
DO_SYMBOLS
#undef DO_SYMBOL
struct Obj sym_set_bang;

void scheme_symbol_init(void) {
	int i;
	char *slab;

	slab = calloc(MAX_SYMBOLS, 64);
	assert(slab);
	symbol_table = calloc(MAX_SYMBOLS, sizeof(char*));
	assert(symbol_table);
	for(i = 0; i < MAX_SYMBOLS; i++) {
		symbol_table[i] = slab + 64*i;
	}

#define DOUBLEESCAPE(a) #a
#define ESCAPEQUOTE(a) DOUBLEESCAPE(a)
#define INTERN_SYMBOL_X(name, name_str) sym_ ## name = scheme_symbol_intern(name_str);
#define DO_SYMBOL(name) INTERN_SYMBOL_X(name, ESCAPEQUOTE(name))
DO_SYMBOLS
#undef DO_SYMBOL
sym_set_bang = scheme_symbol_intern("set!");
}

#undef DO_SYMBOLS

struct Obj scheme_symbol_intern(char *name) {
	int i;
	
	for(i = 0; i < symbol_table_size; i++) {
		if(!strcmp(symbol_table[i], name)) {
			return (struct Obj){ .tag = TAG_SYMBOL, .symbol.id = i };
		}
	}
	
	if(i == MAX_SYMBOLS) {
		fprintf(stderr, "scheme_symbol_intern: ran out of space for symbols.\n");
		exit(1);
	}
	
	strncpy(symbol_table[i], name, 64);
	symbol_table_size++;
	
	return (struct Obj){ .tag = TAG_SYMBOL, .symbol.id = i };
}

char *scheme_symbol_name(int id) {
	if(!(0 <= id && id < symbol_table_size)) {
		fprintf(stderr, "scheme_symbol_name: out of bounds access.\n");
		exit(1);
	}

	return symbol_table[id];
}


/*
 * SECTION reader
 */

void scheme_read_many(struct Obj *rt, int *line_no);
void scheme_build_string(struct Obj *rt, char *str);
// inverse of build_string, but not actually part of reader
void scheme_string_to_buf(struct Obj *obj, char *buf, int len);

//#define DEBUG
#ifdef DEBUG

#define LBL_X(lbl, lblnm) lbl: fprintf(stderr, "[DEBUG] scheme_read: %s\n", lblnm);
#define LBL(lbl) LBL_X(lbl, ESCAPEQUOTE(lbl))

#else

#define LBL(lbl) lbl:

#endif

#define GETCHAR(c, port) do { c = fgetc(port); if(c == '\n') ++*line_no; } while(0)
#define UNGETCHAR(c, port) do { ungetc(c, port); if(c == '\n') --*line_no; } while(0)

void scheme_read_skip_ws(int *line_no, int eof_err) {
	int c;

skip_loop:
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		if(eof_err) {
			fprintf(stderr, "scheme_skip_ws: early EOF on line %d.\n", *line_no);
			exit(1);
		}
		fclose(stdin);
		return;
	case ' ':
	case '\n':
	case '\t':
		goto skip_loop;
	case ';':
		goto skip_comment;
	default:
		UNGETCHAR(c, stdin);
		return;
	}

skip_comment:
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		if(eof_err) {
			fprintf(stderr, "scheme_skip_ws: early EOF inside comment on line %d.\n", *line_no);
			exit(1);
		}
		return;
	case '\n':
		goto skip_loop;
	default:
		goto skip_comment;
	}

}

void scheme_read_atom(struct Obj *rt, int *line_no) {
	int c;
//	int negative = 0;

#define ATOM_BUFLIMIT 256
	char buf[ATOM_BUFLIMIT];
	int buflen = 0;
	buf[buflen] = '\0';

	GETCHAR(c, stdin);
	switch(c) {
	case '#':
		goto read_atom_hash;
	case '"':
		goto read_atom_string;
	default:
		goto read_buf;
	}

LBL(read_atom_hash)
	GETCHAR(c, stdin);
	switch(c) {
	case 't':
		*rt = const_true;
		return;
	case 'f':
		*rt = const_false;
		return;
	case '\\':
		goto read_atom_char;
	default:
		fprintf(stderr, "error in scheme_read_atom: invalid hash on line %d.\n", *line_no);
		exit(1);
	}

LBL(read_atom_char)
	GETCHAR(c, stdin);
	switch(c) {
	case 't':
		GETCHAR(c, stdin);
		if(c != 'a') {
			UNGETCHAR(c, stdin);
			c = 't';
			goto finish_atom_char;
		}
		GETCHAR(c, stdin);
		assert(c == 'b');
		c = '\t';
		goto finish_atom_char;
	case 'n':
		GETCHAR(c, stdin);
		if(c != 'e') {
			UNGETCHAR(c, stdin);
			c = 'n';
			goto finish_atom_char;
		}
		GETCHAR(c, stdin);
		assert(c == 'w');
		GETCHAR(c, stdin);
		assert(c == 'l');
		GETCHAR(c, stdin);
		assert(c == 'i');
		GETCHAR(c, stdin);
		assert(c == 'n');
		GETCHAR(c, stdin);
		assert(c == 'e');
		c = '\n';
		goto finish_atom_char;
	case 's':
		GETCHAR(c, stdin);
		if(c != 'p') {
			UNGETCHAR(c, stdin);
			c = 's';
			goto finish_atom_char;
		}
		GETCHAR(c, stdin);
		assert(c == 'a');
		GETCHAR(c, stdin);
		assert(c == 'c');
		GETCHAR(c, stdin);
		assert(c == 'e');
		c = ' ';
		goto finish_atom_char;
	default:
LBL(finish_atom_char)
		*rt = (struct Obj){ .tag = TAG_CHARACTER, .character.val = (char) c };
		return;
	}

LBL(read_atom_string)
	GETCHAR(c, stdin);
	if(c == EOF) {
		fprintf(stderr, "error in scheme_read_atom: eof inside string on line %d.\n", *line_no);
		exit(1);
	}
	if(c == '"') {
		scheme_build_string(rt, buf);
		return;
	}
	if(c == '\\') {
		GETCHAR(c, stdin);
	}
	if(buflen >= ATOM_BUFLIMIT) {
		fprintf(stderr, "scheme_read_atom: token longer than buffer\n");
		exit(1);
	}
	buf[buflen++] = (char) c;
	buf[buflen] = '\0';
	goto read_atom_string;

LBL(read_buf)
	if(c == EOF ||
	   c == ' ' || c == '\n' || c == '\t' ||
	   c == '(' || c == ')') {
		if(c == '(' || c == ')')
			UNGETCHAR(c, stdin);

		// TODO: we should really check that all chars are digits
		if((buf[0] == '-' && buf[1] != '\0') ||
                   ('0' <= buf[0] && buf[0] <= '9')) {
			*rt = (struct Obj){ .tag = TAG_NUMBER, .number.val = atoi(buf) };
			return;
		}
		else {
			*rt = scheme_symbol_intern(buf);
			return;
		}
	}
	if(buflen >= ATOM_BUFLIMIT) {
		fprintf(stderr, "scheme_read_atom: token longer than buffer\n");
		exit(1);
	}
	buf[buflen++] = (char) c;
	buf[buflen] = '\0';
	GETCHAR(c, stdin);
	goto read_buf;
}

void scheme_read(struct Obj *rt, int *line_no) {
	int c;
	
	scheme_read_skip_ws(line_no, 0);
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		*rt = const_eof;
		return;
	case '(':
		scheme_read_many(rt, line_no);
		return;
	case ')':
		fprintf(stderr, "scheme_read: too many close parenthesis on line %d.\n", *line_no);
		exit(1);
	case '\'':
	case '`':
	case ',':
		goto read_shorthand;
	default:
		UNGETCHAR(c, stdin);
		scheme_read_atom(rt, line_no);
		return;
	}

LBL(read_shorthand)
	scheme_read(rt, line_no);
	*rt = scheme_cons(rt, NULL);
	*rt = scheme_cons(NULL, rt);
	switch(c) {
	case '\'':
		*rt->cons.car = sym_quote;
		break;
	case '`':
		*rt->cons.car = sym_quasiquote;
		break;
	case ',':
		*rt->cons.car = sym_unquote;
		break;
	default:
		fprintf(stderr, "scheme_read: error with shorthand on line %d.\n", *line_no);
		exit(1);
	}
	return;
}

void scheme_read_many(struct Obj *rt, int *line_no) {
	int c;
	
	struct Obj rt_1, rt_2;
	
	scheme_read_skip_ws(line_no, 1);
	GETCHAR(c, stdin);
	switch(c) {
	case ')':
		*rt = const_nil;
		return;
	case '.':
		scheme_read(rt, line_no);
		goto read_many_finish;
	default:
		UNGETCHAR(c, stdin);
		
		scheme_root_push(&rt_1);
		scheme_root_push(&rt_2);
		
		scheme_read(&rt_1, line_no);
		scheme_read_many(&rt_2, line_no);

		*rt = scheme_cons(&rt_1, &rt_2);
		
		scheme_root_pop();
		scheme_root_pop();
		
		return;
	}

LBL(read_many_finish)
	scheme_read_skip_ws(line_no, 1);
	GETCHAR(c, stdin);
	switch(c) {
	case ')':
		return;
	default:
		fprintf(stderr, "scheme_read_many: error multiple items after dot on line %d.\n", *line_no);
		exit(1);
	}
}

void scheme_build_string(struct Obj *rt, char *str) {
	int i;
	
	struct Obj rt_1;
	
	*rt = const_nil;
	for(i = (int)strlen(str)-1; i >= 0; i--) {
		rt_1 = (struct Obj){ .tag = TAG_CHARACTER, .character.val = str[i] };
		*rt = scheme_cons(&rt_1, rt);
	}
	
	// *rt is rooted so we can allocate here
	rt_1 = (struct Obj){ .tag = TAG_STRING, .string.it = scheme_gc_alloc(1) };
	*rt_1.string.it = *rt;
	*rt = rt_1;
}

void scheme_string_to_buf(struct Obj *obj, char *buf, int len) {
	int i = 0; // len must be >= 1
	
	assert(obj->tag == TAG_STRING);
	
	obj = obj->string.it;
	while(obj->tag == TAG_CONS) {
		assert(obj->cons.car->tag == TAG_CHARACTER);
		if(i + 1 > len) break;
		buf[i] = obj->cons.car->character.val;
		obj = obj->cons.cdr;
		i++;
	}
	buf[i] = 0;
}

/*
 * SECTION scheme core
 */

struct Obj scheme_cons(struct Obj *x, struct Obj *y) {
	struct Obj res = (struct Obj){ .tag = TAG_CONS };
	res.cons.car = scheme_gc_alloc(2);
	*res.cons.car = x ? *x : const_nil;
	res.cons.cdr = res.cons.car + 1;
	*res.cons.cdr = y ? *y : const_nil;
	return res;
}

struct Obj scheme_closure(struct Obj *args, struct Obj *env, struct Obj *body) {
	struct Obj res = (struct Obj){ .tag = TAG_CLOSURE };
	res.closure.args = scheme_gc_alloc(3);
	*res.closure.args = *args;
	res.closure.env = res.closure.args + 1;
	*res.closure.env = *env;
	res.closure.body = res.closure.args + 2;
	*res.closure.body = *body;
	return res;
}

int scheme_eq_internal(struct Obj *x, struct Obj *y) {
	if(x->tag != y->tag) return 0;
	
	switch(x->tag) {
	case TAG_TRUE:
	case TAG_FALSE:
	case TAG_EOF:
	case TAG_NIL:
		return 1;

	case TAG_SYMBOL:
		return x->symbol.id == y->symbol.id;
	case TAG_NUMBER:
		return x->number.val == y->number.val;
	case TAG_CHARACTER:
		return x->character.val == y->character.val;

	case TAG_CONS:
		return x->cons.car == y->cons.car && x->cons.cdr == y->cons.cdr;

	case TAG_CLOSURE:
		return
			x->closure.args == y->closure.args &&
			x->closure.body == y->closure.body &&
			x->closure.env == y->closure.env;
	
	case TAG_STRING:
		return x->string.it == y->string.it;
	case TAG_VECTOR:
		return x->vector.it == y->vector.it;
	case TAG_PORT:
		return x->port.fptr == y->port.fptr;
	
	case TAG_BUILTIN:
		return x->builtin.n_args == y->builtin.n_args && x->builtin.impl == y->builtin.impl;

	default:
		return 0;
	}
}

struct Obj scheme_eq(struct Obj *x, struct Obj *y) {
	return scheme_eq_internal(x, y) ? const_true : const_false;
}

void scheme_display(FILE *fptr, struct Obj *x) {
	char c;

	switch(x->tag) {
	case TAG_TRUE:
		fprintf(fptr, "#t");
		break;
	case TAG_FALSE:
		fprintf(fptr, "#f");
		break;
	case TAG_EOF:
		fprintf(fptr, "#<EOF>");
		break;
	case TAG_SYMBOL:
		fprintf(fptr, "%s", scheme_symbol_name(x->symbol.id));
		break;
	case TAG_NUMBER:
		fprintf(fptr, "%d", x->number.val);
		break;
	case TAG_CHARACTER:
		if(x->character.val == '\n') fprintf(fptr, "#\\newline");
		else if(x->character.val == '\t') fprintf(fptr, "#\\tab");
		else if(x->character.val == ' ') fprintf(fptr, "#\\space");
		else fprintf(fptr, "#\\%c", x->character.val);
		break;
	case TAG_NIL:
		fprintf(fptr, "()");
		break;
	case TAG_CONS:
		fprintf(fptr, "(");
loop:
		scheme_display(fptr, x->cons.car);
		switch(x->cons.cdr->tag) {
		case TAG_NIL:
			fprintf(fptr, ")");
			break;
		case TAG_CONS:
			fprintf(fptr, " ");
			x = x->cons.cdr;
			goto loop;
		default:
			fprintf(fptr, " . ");
			scheme_display(fptr, x->cons.cdr);
			fprintf(fptr, ")");
			break;
		}
		break;
	case TAG_CLOSURE:
		fprintf(fptr, "#<closure>");
		break;
	case TAG_PORT:
		fprintf(fptr, "#<port>");
		break;
	case TAG_STRING:
		fprintf(fptr, "\"");
		x = x->string.it;
		while(x->tag == TAG_CONS) {
			assert(x->cons.car->tag == TAG_CHARACTER);
			c = x->cons.car->character.val;
			x = x->cons.cdr;
			if(c == '\\' || c == '"') {
				fprintf(fptr, "\\%c", c);
			}
			else {
				fprintf(fptr, "%c", c);
			}
		}
		fprintf(fptr, "\"");
		break;
	case TAG_VECTOR:
		fprintf(fptr, "#");
		scheme_display(fptr, x->vector.it);
		break;
	case TAG_BUILTIN:
		fprintf(fptr, "#<builtin>");
		break;
	case TAG_GC:
		fprintf(fptr, "#<gcfwd[%s]>", scheme_gc_dead(x->gc.fwd) ? "dead" : "live");
		break;
	default:
		fprintf(fptr, "scheme_display: unknown Obj [%d].\n", x->tag);
		exit(1);
	}
}

struct Obj scheme_append(struct Obj *xs, struct Obj *ys) {
/*
(define (append xs ys)
  (if (null? xs)
      ys
      (let* ((t1 (cdr xs))
             (t2 (append t1 ys))
             (t3 (car xs)))
        (cons t3 t2)))
*/
	struct Obj t1, t2, t3;
	struct Obj res;

	if(xs->tag == TAG_NIL) {
		return *ys;
	}
	
	assert(xs->tag == TAG_CONS);

	scheme_root_push(&t1);
	scheme_root_push(&t2);
	scheme_root_push(&t3);

	t1 = *xs->cons.cdr;
	t2 = scheme_append(&t1, ys);
	t3 = *xs->cons.car;
	res = scheme_cons(&t3, &t2);

	scheme_root_pop();
	scheme_root_pop();
	scheme_root_pop();

	return res;
}

struct Obj scheme_zip_append(struct Obj *xs, struct Obj *ys, struct Obj *zs) {
/*
(define (zip-append xs ys zs)
  (if (null? xs)
      zs
      (cons (cons (car xs) (car ys))
            (zip-append (cdr xs) (cdr ys) zs))))

(define (zip-append xs ys zs)
  (if (null? xs)
      zs
      (let ((t1 (car xs))
            (t2 (car ys))
            (t3 (cons t1 t2))
            (t4 (cdr xs))
            (t5 (cdr ys))
            (t6 (zip-append t4 t5 zs)))
        (cons t3 t6))))
*/
	struct Obj t1, t2, t3, t4, t5, t6;
	struct Obj res;

	if(xs->tag == TAG_NIL) {
		if(ys->tag != TAG_NIL) {
			fprintf(stderr, "error in zip_append: function called with too many arguments.\n");
			exit(1);
		}
		return *zs;
	}
	
	if(ys->tag != TAG_CONS) {
		fprintf(stderr, "error in zip_append: function called with too few arguments.\n");
		exit(1);
	}
	assert(xs->tag == TAG_CONS);

	scheme_root_push(&t1);
	scheme_root_push(&t2);
	scheme_root_push(&t3);
	scheme_root_push(&t4);
	scheme_root_push(&t5);
	scheme_root_push(&t6);

	t1 = *xs->cons.car;
	t2 = *ys->cons.car;
	t3 = scheme_cons(&t1, &t2);
	t4 = *xs->cons.cdr;
	t5 = *ys->cons.cdr;
	t6 = scheme_zip_append(&t4, &t5, zs);
	res = scheme_cons(&t3, &t6);

	scheme_root_pop();
	scheme_root_pop();
	scheme_root_pop();
	scheme_root_pop();
	scheme_root_pop();
	scheme_root_pop();

	return res;
}

struct Obj scheme_assoc(struct Obj *key, struct Obj *table) {
/*
(define (assoc key table)
  (if (null? table)
      #f
      (if (eq? key (caar table))
          (car table)
          (assoc key (cdr table)))))
*/
	if(table->tag == TAG_NIL) {
		return const_false;
	}
	
	assert(table->tag == TAG_CONS);
	assert(table->cons.car->tag == TAG_CONS);
	
	if(scheme_eq(key, table->cons.car->cons.car).tag != TAG_FALSE) {
		return *table->cons.car;
	}
	
	return scheme_assoc(key, table->cons.cdr);
}

struct Obj scheme_evlist(struct Obj *exps, struct Obj *env) {
/*
(define (evlist exps env)
  (if (null? exps)
      '()
      (let ((t1 (car exps))
            (t2 (cdr exps))
            (t1 (eval t1 env))
            (t2 (evlist t2 env)))
        (cons t1 t2))))
*/
	struct Obj t1, t2;
	struct Obj res;

	if(exps->tag == TAG_NIL) {
		return const_nil;
	}
	
	assert(exps->tag == TAG_CONS);

	scheme_root_push(&t1);
	scheme_root_push(&t2);

	t1 = *exps->cons.car;
	t2 = *exps->cons.cdr;
	t1 = scheme_eval(&t1, env);
	t2 = scheme_evlist(&t2, env);
	res = scheme_cons(&t1, &t2);

	scheme_root_pop();
	scheme_root_pop();

	return res;
}

int scheme_is_self_evaluating(enum Tag tag) {
	return
		tag == TAG_FALSE ||
		tag == TAG_TRUE ||
		tag == TAG_EOF ||
		tag == TAG_NUMBER ||
		tag == TAG_CHARACTER ||
		tag == TAG_STRING ||
		tag == TAG_VECTOR;
}

struct Obj scheme_make_begin(struct Obj *lst) {
/*
(define (make-begin lst)
  (if (null? (cdr lst))
      (car lst)
      (cons 'begin lst)))
*/
	struct Obj tmp;

	if(lst->cons.cdr->tag == TAG_NIL)
		return *lst->cons.car;
	
	scheme_root_push(&tmp);
	tmp = *lst;
	tmp = scheme_cons(&sym_begin, &tmp);
	scheme_root_pop();
	return tmp;
}

struct Obj scheme_eval_internal(struct Obj *exp, struct Obj *env);
struct Obj scheme_eval(struct Obj *exp_in, struct Obj *env_in) {
	struct Obj exp, env;
	struct Obj res;
	
	scheme_root_push(&exp);
	scheme_root_push(&env);

	exp = *exp_in;
	env = *env_in;

	res = scheme_eval_internal(&exp, &env);
	
	scheme_root_pop();
	scheme_root_pop();
	
	return res;
}

struct Obj scheme_eval_internal(struct Obj *exp, struct Obj *env) {
	// The crucial thing in this evaluator is to never recursively call 'eval' from a tail position
	// TODO: properly shadow builtins like begin, if etc.

	struct Obj f, vals;
	struct Obj t1, t2, t3;
	struct Obj res;
	struct Obj args[MAX_BUILTIN_ARGS];
	int i;

eval:
	if(scheme_is_self_evaluating(exp->tag))
		return *exp;
	
	if(exp->tag == TAG_SYMBOL) {
		res = scheme_assoc(exp, env);
		if(res.tag == TAG_FALSE) {
			res = scheme_assoc(exp, &globals);
		}
		if(res.tag == TAG_FALSE) {
			fprintf(stderr, "error in scheme_eval: reference to an undefined variable [%s].\n", scheme_symbol_name(exp->symbol.id));
			exit(1);
		}
		assert(res.tag == TAG_CONS);
		return *res.cons.cdr;
	}
	
	if(exp->tag != TAG_CONS) {
		fprintf(stderr, "error in scheme_eval: unknown object type [%d].\n", exp->tag);
		exit(1);
	}

	if(scheme_eq_internal(exp->cons.car, &sym_quote)) {
		// (quote <exp>)
		
		assert(exp->cons.cdr->cons.cdr->tag == TAG_NIL);
		
		return *exp->cons.cdr->cons.car;
	}
		
	if(scheme_eq_internal(exp->cons.car, &sym_set_bang)) {
		// (set! <name> <val>)
		
		assert(exp->cons.cdr->cons.cdr->cons.cdr->tag == TAG_NIL);
		*exp = *exp->cons.cdr;
		assert(exp->cons.car->tag == TAG_SYMBOL);
		
		t1 = *exp->cons.cdr->cons.car;
		t1 = scheme_eval_internal(&t1, env);
		
		res = scheme_assoc(exp->cons.car, env);
		if(res.tag != TAG_CONS) {
			fprintf(stderr, "scheme_builtin_set: failure looking up [%s]\n", scheme_symbol_name(exp->cons.car->symbol.id));
			scheme_display(stdout, &res);
			exit(1);
		}
		assert(res.tag == TAG_CONS);
		*res.cons.cdr = t1;
		
		return *exp->cons.cdr->cons.car;
	}

	if(scheme_eq_internal(exp->cons.car, &sym_begin)) {
		// (begin <exp> ...)
		
		scheme_root_push(&t1);
//		scheme_root_push(env);
loop_begin:
		*exp = *exp->cons.cdr;
		assert(exp->tag == TAG_CONS);
		
		t1 = *exp->cons.car;
		if(exp->cons.cdr->tag == TAG_NIL) {
			// this is the final one, tail position
			// so do not recursively call eval
			*exp = t1;
			scheme_root_pop();
//			scheme_root_pop();
			goto eval;
		}
		else {
			scheme_eval(&t1, env);
			goto loop_begin;
		}
	}

	if(scheme_eq_internal(exp->cons.car, &sym_if)) {
		// (if t1 t2 t3)
		
		assert(exp->cons.cdr->tag == TAG_CONS);
		assert(exp->cons.cdr->cons.cdr->tag == TAG_CONS);
		assert(exp->cons.cdr->cons.cdr->cons.cdr->tag == TAG_CONS);
		assert(exp->cons.cdr->cons.cdr->cons.cdr->cons.cdr->tag == TAG_NIL);

		scheme_root_push(&t1);
		scheme_root_push(&t2);
		scheme_root_push(&t3);
		
		t1 = *exp->cons.cdr->cons.car;
		t2 = *exp->cons.cdr->cons.cdr->cons.car;
		t3 = *exp->cons.cdr->cons.cdr->cons.cdr->cons.car;
		
		t1 = scheme_eval(&t1, env);
		
		if(t1.tag != TAG_FALSE) {
			*exp = t2;
		}
		else {
			*exp = t3;
		}

		scheme_root_pop();
		scheme_root_pop();
		scheme_root_pop();
		
		goto eval;
	}

	if(scheme_eq_internal(exp->cons.car, &sym_lambda)) {
		// (lambda <args> <body> ...)
		
		assert(exp->cons.cdr->tag == TAG_CONS);
		assert(exp->cons.cdr->cons.cdr->tag == TAG_CONS);
		
		scheme_root_push(&t1);
		scheme_root_push(&t2);
		
		t1 = *exp->cons.cdr->cons.car;
		t2 = *exp->cons.cdr->cons.cdr;
		t2 = scheme_make_begin(&t2);

		res = scheme_closure(&t1, env, &t2);
		
		scheme_root_pop();
		scheme_root_pop();
		
		return res;
	}

	// (f arg ...)

	scheme_root_push(&f);
	scheme_root_push(&vals);
	scheme_root_push(&t1);
	scheme_root_push(&t2);
	
	f = *exp->cons.car;
	vals = *exp->cons.cdr;

	f = scheme_eval(&f, env);
	vals = scheme_evlist(&vals, env);
	
	if(f.tag == TAG_CLOSURE) {
		t1 = *f.closure.args;
		*exp = *f.closure.body;
		*env = *f.closure.env;
		*env = scheme_zip_append(&t1, &vals, env);
	}
	else if(f.tag == TAG_BUILTIN) {
		for(i = 0; i < f.builtin.n_args; i++) {
			scheme_root_push_value(&args[i]);
			if(vals.tag != TAG_CONS) {
				fprintf(stderr, "scheme_eval: too few args when calling builtin.\n");
				exit(1);
			}
			args[i] = *vals.cons.car;
			vals = *vals.cons.cdr;
		}
		if(vals.tag != TAG_NIL) {
			fprintf(stderr, "scheme_eval: too many args when calling builtin.\n");
			exit(1);
		}
		res = f.builtin.impl(args);
		for(i = 0; i < f.builtin.n_args + 4; i++) { // + 4 for f, vals, env, t1, t2
			scheme_root_pop();
		}
		return res;
	}
	else {
		fprintf(stderr, "scheme_eval: applying a non function [%d].\n", exp->tag);
		exit(1);
	}
	
	scheme_root_pop();
	scheme_root_pop();
	scheme_root_pop();
	scheme_root_pop();
	
	goto eval;
}

int scheme_shape_define(struct Obj *exp) {
/*
(define (def? exp)
  (and (pair? exp)
       (eq? (car exp) 'define)
       (pair? (cdr exp))
       (pair? (cddr exp)))
*/
	return
		exp->tag == TAG_CONS &&
		scheme_eq_internal(exp->cons.car, &sym_define) &&
		exp->cons.cdr->tag == TAG_CONS &&
		exp->cons.cdr->cons.cdr->tag == TAG_CONS;
}

struct Obj scheme_curry_definition(struct Obj *exp) {
/*
(define (curry-def def)
  `(define ,(car (cadr def)) (lambda ,(cdr (cadr def)) . ,(cddr def))))
                 ^ t1                       ^ t2            ^ t3
*/
	struct Obj t1, t2, t3;
	
	scheme_root_push(&t1);
	scheme_root_push(&t2);
	scheme_root_push(&t3);

	t1 = *exp->cons.cdr->cons.car->cons.car;
	t2 = *exp->cons.cdr->cons.car->cons.cdr;
	t3 = *exp->cons.cdr->cons.cdr;
	
	t3 = scheme_cons(&t2, &t3);
	t3 = scheme_cons(&sym_lambda, &t3);
	t3 = scheme_cons(&t3, &const_nil);
	t3 = scheme_cons(&t1, &t3);
	t3 = scheme_cons(&sym_define, &t3);
	
	scheme_root_pop();
	scheme_root_pop();
	scheme_root_pop();
	
	return t3;
}

struct Obj scheme_exec(struct Obj *exp, struct Obj *env, int display_result) {
	struct Obj t1, t2;
	struct Obj res;
	
	if(scheme_shape_define(exp)) {
define_loop:
		// (define t1 t2 ...)

		// (define (t1 args ...) t2 ...) =>
		// (define t1 (lambda (args ...) t2 ...)
		if(exp->cons.cdr->cons.car->tag == TAG_CONS) {
			*exp = scheme_curry_definition(exp);
			goto define_loop;
		}
		
		scheme_root_push(&t1);
		scheme_root_push(&t2);
		
		t1 = *exp->cons.cdr->cons.car;
		t2 = *exp->cons.cdr->cons.cdr;
		t2 = scheme_make_begin(&t2);
		t2 = scheme_eval(&t2, env);
		t1 = scheme_cons(&t1, &t2);
		// TODO: overwrite existing
		globals = scheme_cons(&t1, &globals);
		
		scheme_root_pop();
		scheme_root_pop();
		
		return const_nil;
	}
	
	res = scheme_eval(exp, env);
	
	if(display_result) {
		scheme_display(stdout, &res);
		puts("");
	}

	return res;
}


/*
 * SECTION builtins
 */

struct Obj scheme_builtin_preprocess(struct Obj *args) {
	return args[0];
}

struct Obj scheme_builtin_display(struct Obj *args) {
	scheme_display(stdout, &args[0]);
	return const_nil;
}

struct Obj scheme_builtin_display_port(struct Obj *args) {
	assert(args[0].tag == TAG_PORT);
	scheme_display(args[0].port.fptr, &args[1]);
	return const_nil;
}

struct Obj scheme_builtin_newline(struct Obj *args) {
	(void) args;
	puts("");
	return const_nil;
}

struct Obj scheme_builtin_newline_port(struct Obj *args) {
	assert(args[0].tag == TAG_PORT);
	fprintf(args[0].port.fptr, "\n");
	return const_nil;
}

struct Obj scheme_builtin_read_char(struct Obj *args) {
	int c;
	assert(args[0].tag == TAG_PORT);
	c = fgetc(args[0].port.fptr);
	if(c == -1) return const_eof;
	return (struct Obj){ .tag = TAG_CHARACTER, .character.val = (char) c };
}

int fpeek(FILE *stream) {
	int c;
	c = fgetc(stream);
 	ungetc(c, stream);
	return c;
}

struct Obj scheme_builtin_peek_char(struct Obj *args) {
	int c;
	assert(args[0].tag == TAG_PORT);
	c = fpeek(args[0].port.fptr);
	if(c == -1) return const_eof;
	return (struct Obj){ .tag = TAG_CHARACTER, .character.val = (char) c };
}

struct Obj scheme_builtin_write_char(struct Obj *args) {
	assert(args[0].tag == TAG_PORT);
	assert(args[1].tag == TAG_CHARACTER);
	fputc(args[1].character.val, args[0].port.fptr);
	return const_nil;
}

struct Obj scheme_builtin_close(struct Obj *args) {
	assert(args[0].tag == TAG_PORT);
	fclose(args[0].port.fptr);
	args[0].port.fptr = NULL;
	return const_nil;
}

struct Obj scheme_builtin_error(struct Obj *args) {
	fprintf(stderr, "Scheme Error: ");
	scheme_display(stderr, &args[0]);
	fprintf(stderr, "\n");
	exit(1);
	return const_nil;
}

struct Obj scheme_builtin_gensym(struct Obj *args) {
	char name[64] = { 0 };
	assert(args[0].tag == TAG_SYMBOL);
	if(sprintf(name, "gensym-%s-%ld", scheme_symbol_name(args[0].symbol.id), random()%99999) <= 0) {
		fprintf(stderr, "scheme_builtin_gensym: sprintf failed");
		exit(1);
	}
	return scheme_symbol_intern(name);
}

struct Obj scheme_builtin_eq(struct Obj *args) {
	return scheme_eq(&args[0], &args[1]);
}

struct Obj scheme_builtin_cons(struct Obj *args) {
	return scheme_cons(&args[0], &args[1]);
}

struct Obj scheme_builtin_car(struct Obj *args) {
	assert(args[0].tag == TAG_CONS);
	return *args[0].cons.car;
}

struct Obj scheme_builtin_cdr(struct Obj *args) {
	assert(args[0].tag == TAG_CONS);
	return *args[0].cons.cdr;
}

struct Obj scheme_builtin_set_car(struct Obj *args) {
	assert(args[0].tag == TAG_CONS);
	*args[0].cons.car = args[1];
	return const_nil;
}

struct Obj scheme_builtin_set_cdr(struct Obj *args) {
	assert(args[0].tag == TAG_CONS);
	*args[0].cons.cdr = args[1];
	return const_nil;
}

#define DEFINE_ARITH_BUILTIN(NM, OP) \
struct Obj scheme_builtin_ ## NM(struct Obj *args) { \
	assert(args[0].tag == TAG_NUMBER); \
	assert(args[1].tag == TAG_NUMBER); \
	return (struct Obj){ .tag = TAG_NUMBER, .number.val = args[0].number.val OP args[1].number.val }; \
}
DEFINE_ARITH_BUILTIN(plus, +)
DEFINE_ARITH_BUILTIN(minus, -)
DEFINE_ARITH_BUILTIN(times, *)
DEFINE_ARITH_BUILTIN(quotient, /)
DEFINE_ARITH_BUILTIN(remainder, %)

#define DEFINE_TYPE_PREDICATE_BUILTIN(NM, TAG) \
struct Obj scheme_builtin_ ## NM(struct Obj *args) { \
	return (args[0].tag == TAG) ? const_true : const_false; \
}
DEFINE_TYPE_PREDICATE_BUILTIN(eof_objectp, TAG_EOF)
DEFINE_TYPE_PREDICATE_BUILTIN(symbolp, TAG_SYMBOL)
DEFINE_TYPE_PREDICATE_BUILTIN(numberp, TAG_NUMBER)
DEFINE_TYPE_PREDICATE_BUILTIN(charp, TAG_CHARACTER)
DEFINE_TYPE_PREDICATE_BUILTIN(nullp, TAG_NIL)
DEFINE_TYPE_PREDICATE_BUILTIN(pairp, TAG_CONS)
DEFINE_TYPE_PREDICATE_BUILTIN(stringp, TAG_STRING)
DEFINE_TYPE_PREDICATE_BUILTIN(vectorp, TAG_VECTOR)
struct Obj scheme_builtin_booleanp(struct Obj *args) { \
	return (args[0].tag == TAG_TRUE || args[0].tag == TAG_FALSE) ? const_true : const_false; \
}
struct Obj scheme_builtin_procedurep(struct Obj *args) { \
	return (args[0].tag == TAG_CLOSURE || args[0].tag == TAG_BUILTIN) ? const_true : const_false; \
}

#define DEFINE_ARITH_COMPARE_BUILTIN(NM, OP) \
struct Obj scheme_builtin_ ## NM(struct Obj *args) { \
	assert(args[0].tag == TAG_NUMBER); \
	assert(args[1].tag == TAG_NUMBER); \
	return (args[0].number.val OP args[1].number.val) ? const_true : const_false; \
}
DEFINE_ARITH_COMPARE_BUILTIN(lt, <)
DEFINE_ARITH_COMPARE_BUILTIN(gt, >)
DEFINE_ARITH_COMPARE_BUILTIN(le, <=)
DEFINE_ARITH_COMPARE_BUILTIN(ge, >=)

struct Obj scheme_builtin_display_char(struct Obj *args) {
	assert(args[0].tag == TAG_CHARACTER);
	printf("%c", args[0].character.val);
	return const_nil;
}

struct Obj scheme_builtin_string_to_list(struct Obj *args) {
	assert(args[0].tag == TAG_STRING);
	return *args[0].string.it;
}

struct Obj scheme_builtin_vector_to_list(struct Obj *args) {
	assert(args[0].tag == TAG_VECTOR);
	return *args[0].vector.it;
}

struct Obj scheme_builtin_list_to_string(struct Obj *args) {
	struct Obj s  = (struct Obj){ .tag = TAG_STRING, .string.it = scheme_gc_alloc(1) };
	*s.string.it = args[0];
	return s;
}

struct Obj scheme_builtin_list_to_vector(struct Obj *args) {
	struct Obj s  = (struct Obj){ .tag = TAG_VECTOR, .string.it = scheme_gc_alloc(1) };
	*s.string.it = args[0];
	return s;
}

struct Obj scheme_builtin_char_to_integer(struct Obj *args) {
	assert(args[0].tag == TAG_CHARACTER);
	return (struct Obj){ .tag = TAG_NUMBER, .number.val = args[0].character.val };
}

struct Obj scheme_builtin_string_to_symbol(struct Obj *args) {
	char buf[128] = { 0 };
	assert(args[0].tag == TAG_STRING);
	scheme_string_to_buf(&args[0], buf, sizeof buf);
	return scheme_symbol_intern(buf);
}

#define PATH_MAX 1024
struct Obj scheme_builtin_open_input_output_file(char *mode, struct Obj *args) {
	FILE* fptr;
	char filename[PATH_MAX] = { 0 };
	assert(args[0].tag == TAG_STRING);
	scheme_string_to_buf(&args[0], filename, PATH_MAX);
	fptr = fopen(filename, mode);
	if(!fptr) return const_false;
	return (struct Obj){ .tag = TAG_PORT, .port.fptr = fptr };
}

struct Obj scheme_builtin_open_input_file(struct Obj *args) {
	return scheme_builtin_open_input_output_file("r", args);
}

struct Obj scheme_builtin_open_output_file(struct Obj *args) {
	return scheme_builtin_open_input_output_file("w", args);
}

void scheme_builtins_init(void) {
	struct Obj tmp, nm;
	scheme_root_push(&tmp);
	scheme_root_push(&nm);
#define BUILTIN(IMPL, NARGS) BUILTIN_(IMPL, # IMPL, NARGS)
#define BUILTIN_(IMPL, NAME, NARGS) \
	do { \
		assert(NARGS <= MAX_BUILTIN_ARGS); \
		tmp = (struct Obj){ .tag = TAG_BUILTIN, .builtin.n_args = NARGS, .builtin.impl = scheme_builtin_ ## IMPL }; \
		nm = scheme_symbol_intern(NAME); \
		tmp = scheme_cons(&nm, &tmp); \
		globals = scheme_cons(&tmp, &globals); \
	} while(0)

	BUILTIN(preprocess, 1);

	BUILTIN(display, 1);
	BUILTIN_(display_port, "display/port", 2);
	BUILTIN(newline, 0);
	BUILTIN_(newline, "newline/port", 1);
	BUILTIN_(read_char, "read-char", 1);
	BUILTIN_(peek_char, "peek-char", 1);
	BUILTIN_(write_char, "write-char", 2);
	BUILTIN(close, 1);
	BUILTIN(error, 1);
	BUILTIN_(gensym, "builtin-gensym", 1);
	BUILTIN_(eq, "eq?", 2);

	BUILTIN(cons, 2);
	BUILTIN(car, 1);
	BUILTIN(cdr, 1);
	BUILTIN_(set_car, "set-car!", 2);
	BUILTIN_(set_cdr, "set-cdr!", 2);

	BUILTIN_(plus, "+", 2);
	BUILTIN_(minus, "-", 2);
	BUILTIN_(times, "*", 2);
	BUILTIN(quotient, 2);
	BUILTIN(remainder, 2);

	BUILTIN_(eof_objectp, "eof-object?", 1);
	BUILTIN_(symbolp, "symbol?", 1);
	BUILTIN_(numberp, "number?", 1);
	BUILTIN_(charp, "char?", 1);
	BUILTIN_(nullp, "null?", 1);
	BUILTIN_(pairp, "pair?", 1);
	BUILTIN_(booleanp, "boolean?", 1);
	BUILTIN_(procedurep, "procedure?", 1);
	BUILTIN_(stringp, "string?", 1);
	BUILTIN_(vectorp, "vector?", 1);

	BUILTIN_(lt, "<", 2);
	BUILTIN_(gt, ">", 2);
	BUILTIN_(le, "<=", 2);
	BUILTIN_(ge, ">=", 2);
	
	BUILTIN_(display_char, "display-char", 1);
	
	BUILTIN_(string_to_list, "string->list", 1);
	BUILTIN_(list_to_string, "list->string", 1);
	BUILTIN_(vector_to_list, "vector->list", 1);
	BUILTIN_(list_to_vector, "list->vector", 1);
	BUILTIN_(char_to_integer, "char->integer", 1);
	BUILTIN_(string_to_symbol, "string->symbol", 1);
	
	BUILTIN_(open_input_file, "open-input-file", 1);
	BUILTIN_(open_output_file, "open-output-file", 1);
	
	scheme_root_pop();
	scheme_root_pop();
}

void scheme_constants_init(void) {
	struct Obj tmp, obj;
	scheme_root_push(&tmp);
	
	obj = (struct Obj){ .tag = TAG_PORT, .port.fptr = stdout };
	tmp = scheme_cons(&sym_stdout, &obj);
	globals = scheme_cons(&tmp, &globals);
	
	obj = (struct Obj){ .tag = TAG_PORT, .port.fptr = stderr };
	tmp = scheme_cons(&sym_stderr, &obj);
	globals = scheme_cons(&tmp, &globals);

	scheme_root_pop();
}


/*
 * SECTION main
 */

void wrap_preprocess(struct Obj *rt) {
	// TODO: tidy
	
	*rt = scheme_cons(rt, NULL);
	*rt = scheme_cons(NULL, rt);
	*rt->cons.car = sym_quote;
	
	*rt = scheme_cons(rt, NULL);
	*rt = scheme_cons(NULL, rt);
	*rt->cons.car = sym_preprocess;
}

int main(int argc, char **argv) {
	struct Obj rt, rt2, res;
	int line_no = 0;

	(void) argc;
	(void) argv;
	
	scheme_init();
	scheme_root_push(&rt);
	scheme_root_push(&rt2);
	scheme_root_push(&res);
	do {
		rt = const_nil;
		scheme_read(&rt, &line_no);
		
		if(rt.tag == TAG_EOF)
			break;
		
		wrap_preprocess(&rt);
		
		rt2 = const_nil;
		rt = scheme_exec(&rt, &rt2, 0);
		
		rt2 = const_nil;
		res = scheme_exec(&rt, &rt2, 1);
		
		res = const_nil;
		scheme_gc();
	} while(1);
	return 0;
}
