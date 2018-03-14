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
	TAG_STRING	= 6,
	TAG_NIL		= 7,
	TAG_CONS	= 8,
	TAG_CLOSURE	= 9,
	
	TAG_GC		= 0xFF,
};

struct Obj {
	enum Tag tag;
	union {
		struct { int id; } symbol;
		struct { int val; } number;
		struct { char val; } character;
		struct { int len; char *text; } string;
		struct { struct Obj *car, *cdr; } cons;
		struct { struct Obj *args, *env, *body; } closure;
		struct { struct Obj *fwd; } gc;
	};
};

struct Root {
	int free;
	struct Root *prev;
	struct Root *next;
	struct Obj obj;
};

#define SEMISPACE_SIZE 480
struct Obj *gc_live_space, *gc_dead_space;
struct Obj *gc_free_ptr, *gc_scan_ptr;
struct Root *gc_roots;

#define MAX_SYMBOLS 4096
char **symbol_table;

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

struct Obj scheme_cons(struct Root *x, struct Root *y);
void scheme_display(struct Obj x);

void scheme_read(struct Root *rt, int *line_no);

struct Obj const_false, const_true, const_eof, const_nil;
void scheme_init(void) {
	const_false = (struct Obj){ .tag = TAG_FALSE };
	const_true = (struct Obj){ .tag = TAG_TRUE };
	const_eof = (struct Obj){ .tag = TAG_EOF };
	const_nil = (struct Obj){ .tag = TAG_NIL };
	scheme_gc_init();
	scheme_symbol_init();
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
	return scheme_gc_alloc_internal(cells, 0);
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
		fprintf(stderr, "gc: error in gc_copy\n");
		exit(1);
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
 * SECTION Display
 */

struct Obj scheme_cons(struct Root *x, struct Root *y) {
	struct Obj res = (struct Obj){ .tag = TAG_CONS };
	res.cons.car = scheme_gc_alloc(2);
	*res.cons.car = x ? x->obj : const_nil;
	res.cons.cdr = res.cons.car + 1;
	*res.cons.cdr = y ? y->obj : const_nil;
	return res;
}

void scheme_display(struct Obj x) {
	int i;

	switch(x.tag) {
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
		fprintf(stdout, "%s", scheme_symbol_name(x.symbol.id));
		break;
	case TAG_NUMBER:
		fprintf(stdout, "%d", x.number.val);
		break;
	case TAG_CHARACTER:
		// TODO: escaping special characters
		fprintf(stdout, "#\\%c", x.character.val);
		break;
	case TAG_STRING:
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
	case TAG_NIL:
		fprintf(stdout, "()");
		break;
	case TAG_CONS:
		fprintf(stdout, "(");
loop:
		scheme_display(*x.cons.car);
		switch(x.cons.cdr->tag) {
		case TAG_NIL:
			fprintf(stdout, ")");
			break;
		case TAG_CONS:
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
	case TAG_CLOSURE:
		fprintf(stdout, "#<closure>");
		break;
	case TAG_GC:
		fprintf(stdout, "#<gcfwd[%s]>", scheme_gc_dead(x.gc.fwd) ? "dead" : "live");
		break;
	default:
		fprintf(stderr, "scheme_display: unknown Obj [%d].\n", x.tag);
		exit(1);
	}
}


/*
 * SECTION reader
 */

void scheme_read_many(struct Root *rt, int *line_no);

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
	char c;

skip_loop:
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		if(eof_err) {
			fprintf(stderr, "scheme_skip_ws: early EOF on line %d.\n", *line_no);
			exit(1);
		}
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

void scheme_read_atom(struct Root *rt, int *line_no) {
	char c;
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
		rt->obj = const_true;
		return;
	case 'f':
		rt->obj = const_false;
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
		rt->obj = (struct Obj){ .tag = TAG_CHARACTER, .character.val = c };
		return;
	}

LBL(read_atom_string)
	GETCHAR(c, stdin);
	if(c == EOF) {
		fprintf(stderr, "error in scheme_read_atom: eof inside string on line %d.\n", *line_no);
		exit(1);
	}
	if(c == '"') {
		// TODO: make a string
		rt->obj = scheme_symbol_intern(buf);
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
	   c == ')') {
		if(c == ')')
			UNGETCHAR(c, stdin);

		if(buf[0] == '-' || ('0' <= buf[0] && buf[0] <= '9')) {
			rt->obj = (struct Obj){ .tag = TAG_NUMBER, .number.val = atoi(buf) };
			return;
		}
		else {
			rt->obj = scheme_symbol_intern(buf);
			return;
		}
	}
	buf[buflen++] = c;
	buf[buflen] = '\0';
	GETCHAR(c, stdin);
	goto read_buf;
}

void scheme_read(struct Root *rt, int *line_no) {
	char c;
	
	scheme_read_skip_ws(line_no, 0);
	GETCHAR(c, stdin);
	switch(c) {
	case EOF:
		rt->obj = const_eof;
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
	rt->obj = scheme_cons(rt, NULL);
	rt->obj = scheme_cons(NULL, rt);
	switch(c) {
	case '\'':
		*rt->obj.cons.car = sym_quote;
		break;
	case '`':
		*rt->obj
.cons.car = sym_quasiquote;
		break;
	case ',':
		*rt->obj.cons.car = sym_unquote;
		break;
	default:
		fprintf(stderr, "scheme_read: error with shorthand on line %d.\n", *line_no);
		exit(1);
	}
}

void scheme_read_many(struct Root *rt, int *line_no) {
	char c;
	
	struct Root rt_1, rt_2;
	
	scheme_read_skip_ws(line_no, 1);
	GETCHAR(c, stdin);
	switch(c) {
	case ')':
		rt->obj = const_nil;
		return;
	case '.':
		scheme_read(rt, line_no);
		goto read_many_finish;
	default:
		UNGETCHAR(c, stdin);
		
		scheme_root_setup(&rt_1);
		scheme_root_setup(&rt_2);
		
		scheme_read(&rt_1, line_no);
		scheme_read_many(&rt_2, line_no);

		rt->obj = scheme_cons(&rt_1, &rt_2);
		
		scheme_root_delete(&rt_1);
		scheme_root_delete(&rt_2);
		
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
 * SECTION main
 */

int main(int argc, char **argv) {
	struct Root *rt;
	int line_no = 0;
	scheme_init();
	rt = scheme_root_alloc();
	do {
		rt->obj = const_nil;
		scheme_read(rt, &line_no);
		scheme_display(rt->obj);
		puts("");
	} while(rt->obj.tag != TAG_EOF);
	scheme_gc();
	return 0;
}

