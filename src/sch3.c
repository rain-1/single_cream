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
		struct { struct Obj *fwd; } gc;
	};
};

struct Root {
	int free;
	struct Root *prev;
	struct Root *next;
	struct Obj obj;
};

#define SEMISPACE_SIZE (1<<12)
struct Obj *gc_live_space, *gc_dead_space;
struct Obj *gc_free_ptr, *gc_scan_ptr;
struct Root *gc_roots;
#define ROOTSTACK_SIZE (1<<10)
struct Obj *gc_root_stack[ROOTSTACK_SIZE];
int gc_root_stack_height = 0;

#define MAX_SYMBOLS (1<<8)
char **symbol_table;

#define MAX_BUILTIN_ARGS 5

// GC_OFF = 1 -> off
// GC_OFF = 0 -> on
#define GC_OFF 0

struct Root *globals;

void scheme_init(void);

void scheme_root_setup(struct Root *);
struct Root *scheme_root_alloc(void);
void scheme_root_delete(struct Root *rt);

void scheme_gc_init(void);
struct Obj *scheme_gc_alloc(int cells);
void scheme_gc(void);

void scheme_symbol_init(void);
struct Obj scheme_symbol_intern(char *name);
char *scheme_symbol_name(int id);

void scheme_read(struct Obj *rt, int *line_no);

struct Obj scheme_cons(struct Obj *x, struct Obj *y);
struct Obj scheme_closure(struct Obj *args, struct Obj *env, struct Obj *body);
void scheme_display(struct Obj *x);
struct Obj scheme_eq(struct Obj *x, struct Obj *y);
struct Obj scheme_append(struct Obj *xs, struct Obj *ys);
struct Obj scheme_assoc(struct Obj *key, struct Obj *table);
struct Obj scheme_evlist(struct Obj *exps, struct Obj *env);
struct Obj scheme_eval(struct Obj *exp, struct Obj *env);

void scheme_builtins_init(void);

struct Obj const_false, const_true, const_eof, const_nil;
void scheme_init(void) {
	const_false = (struct Obj){ .tag = TAG_FALSE };
	const_true = (struct Obj){ .tag = TAG_TRUE };
	const_eof = (struct Obj){ .tag = TAG_EOF };
	const_nil = (struct Obj){ .tag = TAG_NIL };
	scheme_gc_init();
	globals = scheme_root_alloc();
	scheme_symbol_init();
	scheme_builtins_init();
}


/*
 * SECTION Managing GC Roots
 */

void scheme_root_setup(struct Root *rt) {
	rt->free = 0;
	rt->prev = NULL;
	rt->next = gc_roots;
	rt->obj = const_nil;
	
	if(gc_roots) gc_roots->prev = rt;
	gc_roots = rt;
}

struct Root *scheme_root_alloc(void) {
	struct Root *rt = malloc(sizeof(struct Root));
	assert(rt);
	scheme_root_setup(rt);
	rt->free = 1;
	return rt;
}

void scheme_root_delete(struct Root *rt) {
	if(rt->prev) rt->prev->next = rt->next;
	else gc_roots = rt->next;
	if(rt->next) rt->next->prev = rt->prev;
	rt->prev = NULL;
	rt->next = NULL;
	rt->obj = const_nil;
	if(rt->free) free(rt);
}

void scheme_root_push(struct Obj *obj) {
	assert(gc_root_stack_height < ROOTSTACK_SIZE);
	gc_root_stack[gc_root_stack_height] = obj;
	gc_root_stack_height++;
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
	gc_roots = NULL;
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
	struct Root *rt;

	SWAP(gc_live_space, gc_dead_space);
	gc_free_ptr = gc_live_space;
	gc_scan_ptr = gc_live_space;
	
	for(rt = gc_roots; rt; rt = rt->next)
		scheme_gc_forward(&rt->obj);

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
	DO_SYMBOL(begin)

#define DO_SYMBOL(name) struct Obj sym_ ## name;
DO_SYMBOLS
#undef DO_SYMBOL

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

void scheme_build_string(struct Obj *rt, char *str);
void scheme_read_many(struct Obj *rt, int *line_no);

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
	int negative = 0;

	char buf[64];
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
	case 'n':
	case 's':
	case 't':
		// TODO: check for ewline pace ab
	default:
		*rt = (struct Obj){ .tag = TAG_CHARACTER, .character.val = c };
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
	buf[buflen++] = c;
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
	buf[buflen++] = c;
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

void scheme_build_string(struct Obj *rt, char *str) {
	int i;
	
	struct Obj rt_1;
	
	for(i = strlen(str); i >= 0; i--) {
		rt_1 = (struct Obj){ .tag = TAG_CHARACTER, .character.val = str[i] };
		*rt = scheme_cons(&rt_1, rt);
	}
	
	*rt = scheme_cons(rt, NULL);
	*rt = scheme_cons(NULL, rt);
	*rt->cons.car = sym_quote;
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
	
	case TAG_BUILTIN:
		return x->builtin.n_args == y->builtin.n_args && x->builtin.impl == y->builtin.impl;

	default:
		return 0;
	}
}

struct Obj scheme_eq(struct Obj *x, struct Obj *y) {
	return scheme_eq_internal(x, y) ? const_true : const_false;
}

void scheme_display(struct Obj *x) {
	int i;

	switch(x->tag) {
	case TAG_TRUE:
		fprintf(stdout, "#t");
		break;
	case TAG_FALSE:
		fprintf(stdout, "#f");
		break;
	case TAG_EOF:
		fprintf(stdout, "#<EOF>");
		break;
	case TAG_SYMBOL:
		fprintf(stdout, "%s", scheme_symbol_name(x->symbol.id));
		break;
	case TAG_NUMBER:
		fprintf(stdout, "%d", x->number.val);
		break;
	case TAG_CHARACTER:
		// TODO: escaping special characters
		fprintf(stdout, "#\\%c", x->character.val);
		break;
/*
	case TAG_STRING:
		fprintf(stdout, "\"");
		for(i = 0; i < x->string.len; i++) {
			if(x->string.text[i] == '\\' || x->string.text[i] == '"') {
				fprintf(stdout, "\\%c", x->string.text[i]);
			}
			else {
				fprintf(stdout, "%c", x->string.text[i]);
			}
		}
		fprintf(stdout, "\"");
		break;
*/
	case TAG_NIL:
		fprintf(stdout, "()");
		break;
	case TAG_CONS:
		fprintf(stdout, "(");
loop:
		scheme_display(x->cons.car);
		switch(x->cons.cdr->tag) {
		case TAG_NIL:
			fprintf(stdout, ")");
			break;
		case TAG_CONS:
			fprintf(stdout, " ");
			x = x->cons.cdr;
			goto loop;
		default:
			fprintf(stdout, " . ");
			scheme_display(x->cons.cdr);
			fprintf(stdout, ")");
			break;
		}
		break;
	case TAG_CLOSURE:
		fprintf(stdout, "#<closure>");
		break;
	case TAG_BUILTIN:
		fprintf(stdout, "#<builtin>");
		break;
	case TAG_GC:
		fprintf(stdout, "#<gcfwd[%s]>", scheme_gc_dead(x->gc.fwd) ? "dead" : "live");
		break;
	default:
		fprintf(stderr, "scheme_display: unknown Obj [%d].\n", x->tag);
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
		tag == TAG_CHARACTER;
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
			res = scheme_assoc(exp, &globals->obj);
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
		
	if(scheme_eq_internal(exp->cons.car, &sym_begin)) {
		// (begin <exp> ...)
		
		scheme_root_push(&t1);
		scheme_root_push(env);
loop_begin:
		*exp = *exp->cons.cdr;
		assert(exp->tag == TAG_CONS);
		
		t1 = *exp->cons.car;
		if(exp->cons.cdr->tag == TAG_NIL) {
			// this is the final one, tail position
			// so do not recursively call eval
			*exp = t1;
			scheme_root_pop();
			scheme_root_pop();
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
			scheme_root_push(&args[i]);
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

struct Obj scheme_exec(struct Obj *exp, struct Obj *env) {
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
		globals->obj = scheme_cons(&t1, &globals->obj);
		
		scheme_root_pop();
		scheme_root_pop();
		
		return const_nil;
	}
	
	res = scheme_eval(exp, env);
	scheme_display(&res);
	puts("");

	return res;
}


/*
 * SECTION builtins
 */

struct Obj scheme_builtin_display(struct Obj *args) {
	scheme_display(&args[0]);
	return const_nil;
}

struct Obj scheme_builtin_newline(struct Obj *args) {
	puts("");
	return const_nil;
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
DEFINE_ARITH_COMPARE_BUILTIN(lt, <);
DEFINE_ARITH_COMPARE_BUILTIN(gt, >);
DEFINE_ARITH_COMPARE_BUILTIN(le, <=);
DEFINE_ARITH_COMPARE_BUILTIN(ge, >=);

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
		globals->obj = scheme_cons(&tmp, &globals->obj); \
	} while(0)

	BUILTIN(display, 1);
	BUILTIN(newline, 0);
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

	BUILTIN_(lt, "<", 2);
	BUILTIN_(gt, ">", 2);
	BUILTIN_(le, "<=", 2);
	BUILTIN_(ge, ">=", 2);

	scheme_root_pop();
	scheme_root_pop();
}

/*
 * SECTION main
 */

int main(int argc, char **argv) {
	struct Root *rt, *rt2, *res;
	int line_no = 0;
	scheme_init();
	rt = scheme_root_alloc();
	rt2 = scheme_root_alloc();
	res = scheme_root_alloc();
	do {
		rt->obj = const_nil;
		scheme_read(&rt->obj, &line_no);
		
		if(rt->obj.tag == TAG_EOF)
			break;

		rt2->obj = const_nil;
		res->obj = scheme_exec(&rt->obj, &rt2->obj);
		res->obj = const_nil;
		scheme_gc();
	} while(1);
	return 0;
}


