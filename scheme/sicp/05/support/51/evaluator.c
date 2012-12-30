/*
 * This is the rudimentary scheme interpreter, I had to write for exercise
 * 5.51. This is very exciting for me for several reasons.
 *
 * For starters, it is the first actual C code I have written since school. I
 * had a very different memory of the language and it certainly is contrasted
 * by using Vim and gcc (or clang). It is very far from the best C code that
 * I'm able to produce, but I wanted to keep it simple and rudimentary.
 *
 * Second, it includes know-how from every part of Structure and
 * Interpretation of Computer Programs, excluding 5.5 Compilation, which is
 * the next exercise.
 *
 * Finally, it was just tons of fun writing it. I did not use my usual method
 * and I did not use a debugger to make up for it, which made it way harder.
 * More reflections can be found in the notes.
 *
 * As for what this is, it a Scheme evaluator based on the explicit-control
 * evaluator (translated line-by-line) with a garbage collector (based on the
 * one in section 5.3). It supports the functionality of the explicit-control
 * evaluator (no conds) and has tail recursion. It has a hand-written parser
 * that I care not to classify and a memory implementation like the one in
 * section 5.3. It memory leaks symbols and strings, but apart from that,
 * everything is (supposed to be) nicely collected.
 *
 * More details and commentary are intermingled with the code in comments.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <errno.h>

/*
 * Some constants that set up the boundaries of the interpreter.
 */

#define MAX_FILE_SIZE 10000
#define MAX_LINE_LENGTH 1024
#define MAX_TOKEN_LENGTH 128
#define MEMORY_SIZE 512
#define MEMORY_WIGGLE_ROOM 20
#define STACK_SIZE 100

/*
 * Various typedefs for all the types we're using. There are special
 * structures for the symbol table and the memory. Everything else is stored
 * in pairs.
 */

typedef enum p_type p_type;
typedef struct symbol_node symbol_node;
typedef struct pairs memory;
typedef struct value value;
typedef const char* symbol;
typedef value (*primitive_function)(value);

/*
 * Definitions for values and memory.
 *
 * They need to be up here in order to get the file to compile. The different
 * types of values are represented by p_type. A value is a pair of a type and
 * a long. Depending on the value, the long can contain a pointer, a number or
 * some other value.
 */

enum p_type {
  p_null,
  p_boolean,
  p_number,
  p_symbol,
  p_string,
  p_pair,
  p_procedure,
  p_primitive,
  p_label,
  p_broken_heart
};

struct value {
  p_type type;
  unsigned long value;
};

/*
 * Memory declarations
 *
 * Since we use stop and copy garbage collection, modelled after section 5.3,
 * we allocate two segments of memory for the working memory and the free
 * memory. Each segment contains space for the cars and cdrs of the pairs
 * stored.
 */

struct pairs {
  value cars[MEMORY_SIZE];
  value cdrs[MEMORY_SIZE];
};

unsigned int free_ptr = 0;

memory *working_mem;
memory *free_mem;

memory block1;
memory block2;

/*
 * The main interface
 *
 * Those functions are the C interface to the evaluator. They are used to
 * build the read-eval-print loop
 */

value read(const char *);
value eval(value);
void print(value);

/*
 * The number of times the garbage collector has run
 *
 * This is declared up here, since it is tricky to organize the code the way I
 * want it and not have it declared first.
 */

int gc_runs = 0;

/*
 * Error reporting
 *
 * A couple of functions used to report errors. They are not that much to look
 * at.
 */

void error(const char *message) {
  printf("%s\n", message);
  exit(0);
}

void errorc(const char *message, char c) {
  printf("%s: %c\n", message, c);
  exit(0);
}

void errorv(const char *message, value val) {
  printf("%s: ", message);
  print(val);
  printf("\n");
  exit(0);
}

void errort(const char *message, char *token) {
  token[MAX_TOKEN_LENGTH] = '\0';
  printf("%s: %s\n", message, token);
  exit(0);
}

void errors(const char *message, symbol sym) {
  printf("%s: %s\n", message, sym);
  exit(0);
}

/*
 * Symbols
 *
 * The interpreter maintains a symbole table (more like a symbol linked list)
 * in order to intern symbols so they can be compared with == (and also take
 * less memory). intern() checks if the passed symbol is already allocated and
 * returns it if so. If not, it allocated a new one, copies the symbol in it
 * and returns the copy.
 */

symbol_node *symbol_table_head;

struct symbol_node {
  symbol symbol;
  symbol_node* next;
};

symbol_node* allocate_symbol_node(const char *name) {
  char *copy = calloc(sizeof(char), 1 + strlen(name));
  strcpy(copy, name);

  symbol_node *node = malloc(sizeof(node));
  node->symbol = copy;
  node->next = NULL;

  return node;
}

symbol intern(const char *name) {
  if (!symbol_table_head) {
    symbol_table_head = allocate_symbol_node(name);
    return symbol_table_head->symbol;
  }

  symbol_node* current = symbol_table_head;

  while (true) {
    if (strcmp(name, current->symbol) == 0)
      return current->symbol;
    if (!current->next) {
      symbol_node *new = allocate_symbol_node(name);
      current->next = new;
      return new->symbol;
    }
    current = current->next;
  }
}

/*
 * Preallocated symbols
 *
 * Some symbols that we will preallocate to avoid looking up the symbol table
 * every time
 */

symbol s_quote;
symbol s_begin;
symbol s_if;
symbol s_define;
symbol s_set_bang;
symbol s_lambda;
symbol s_ok;

void initialize_symbols() {
  s_quote    = intern("quote");
  s_begin    = intern("begin");
  s_if       = intern("if");
  s_define   = intern("define");
  s_set_bang = intern("set!");
  s_lambda   = intern("lambda");
  s_ok       = intern("ok");
}

/*
 * Values
 *
 * This section defines the values that the interpreter works with. They can
 * be pointers to memory (in the case of pairs), self contained (in the case
 * of booleans and numbers) or have a C pointer to some address in the heap
 * (in the case of primitive procedures and strings).
 *
 * Procedures are implemented as a special case of pairs. They are essentially
 * the same, but with a different type. I've chosen to go that way in order to
 * have the procedures (and environments they generate) garbage collected.
 *
 * Strings, symbols and primitives are implemented as pointers to the C
 * memory. That causes a memory leak for unneeded strings. Symbols don't
 * really need to be garbage collected and primitive procedures don't allocate
 * extra memory.
 *
 * Broken hearts are something that is used when garbage collecting.
 *
 * This section contains all the basic constructors and selectors for the
 * primitives. There are other selectors, implemented in terms of them, that
 * are defined later.
 */

// Type predicates

bool null_p(value val)         { return val.type == p_null;         }
bool boolean_p(value val)      { return val.type == p_boolean;      }
bool number_p(value val)       { return val.type == p_number;       }
bool symbol_p(value val)       { return val.type == p_symbol;       }
bool string_p(value val)       { return val.type == p_string;       }
bool pair_p(value val)         { return val.type == p_pair;         }
bool procedure_p(value val)    { return val.type == p_procedure;    }
bool primitive_p(value val)    { return val.type == p_primitive;    }
bool label_p(value val)        { return val.type == p_label;        }
bool broken_heart_p(value val) { return val.type == p_broken_heart; }

// Null

value null() {
  return (value) { p_null, 0 };
}

// Booleans

value truev() {
  return (value) { p_boolean, 1 };
}

value falsev() {
  return (value) { p_boolean, 0 };
}

value booleanv(bool val) {
  return val ? truev() : falsev();
}

// Numbers

value num(long n) {
  return (value) { p_number, n };
}

long value_to_long(value val) {
  if (val.type != p_number)
    errorv("Expected a number", val);

  return (long) val.value;
}

// Symbols

value sym(const char *s) {
  return (value) { p_symbol, (unsigned long) intern(s) };
}

symbol value_to_sym(value val) {
  if (val.type != p_symbol)
    errorv("Expected a symbol", val);

  return (symbol) val.value;
}

// Strings

value str(const char *s) {
  char *copy = calloc(sizeof(char), strlen(s) + 1);
  strcpy(copy, s);
  return (value) { p_string, (unsigned long) copy };
}

// Pairs

value cons(value car, value cdr) {
  if (free_ptr == MEMORY_SIZE) {
    error("Ran out of memory");
  }

  working_mem->cars[free_ptr] = car;
  working_mem->cdrs[free_ptr] = cdr;

  value pair = (value) { p_pair, free_ptr };

  free_ptr++;

  return pair;
}

value car(value pair) {
  if (pair.type != p_pair)
    errorv("Expected carable value", pair);

  return working_mem->cars[pair.value];
}

value cdr(value pair) {
  if (pair.type != p_pair)
    errorv("Expected cdrable value", pair);

  return working_mem->cdrs[pair.value];
}

void set_car(value pair, value val) {
  if (pair.type != p_pair)
    errorv("set_car expects a pair", pair);

  unsigned int offset = (unsigned int) pair.value;
  working_mem->cars[offset] = val;
}

void set_cdr(value pair, value val) {
  if (pair.type != p_pair)
    errorv("set_cdr expects a pair", pair);

  unsigned int offset = (unsigned int) pair.value;
  working_mem->cdrs[offset] = val;
}

// Procedures

value pcons(value car, value cdr) {
  value pair = cons(car, cdr);

  // A little hack to reuse the cons() code.
  pair.type = p_procedure;

  return pair;
}

value pcdr(value proc) {
  if (proc.type != p_procedure)
    errorv("Expected a pcdrable value", proc);

  return working_mem->cdrs[proc.value];
}

value pcar(value proc) {
  if (proc.type != p_procedure)
    errorv("Expected a pcarable value", proc);

  return working_mem->cars[proc.value];
}

// Primitives

value primitive(primitive_function function) {
  return (value) { p_primitive, (unsigned long) function };
}

// Labels

value label(void *label) {
  return (value) { p_label, (unsigned long) label };
}

void *value_to_label(value label) {
  if (!label_p(label))
    errorv("Expected a label", label);

  return (void *) label.value;
}

// Broken heart

value broken_heart() {
  return (value) { p_broken_heart };
}

/*
 * Memory initialization
 *
 * Memory is organized as in chapter 5.3. A struct represents two arrays of
 * cars and cdrs. There are two such structs that get rotated with a
 * stop-and-copy garbage collector.
 */

void wipe_memory(memory *memory) {
  for (int i = 0; i < MEMORY_SIZE; i++) {
    memory->cars[i] = (value) { p_null, 0 };
  }
}

void initialize_memory() {
  wipe_memory(&block1);
  wipe_memory(&block2);

  working_mem = &block1;
  free_mem    = &block2;
}

/*
 * The stack
 *
 * This is a straightforward implementation of a stack.
 */

int stack_ptr = 0;
value stack[STACK_SIZE];

void push(value v) {
  if (stack_ptr == STACK_SIZE)
    error("Stack overflow");

  stack[stack_ptr++] = v;
}

value pop() {
  if (!stack_ptr)
    error("Stack is empty");

  return stack[--stack_ptr];
}

/*
 * The registers
 *
 * Those are the registers that the explicit-control evaluator uses.
 */

value cont;
value val;
value expr;
value unev;
value proc;
value argl;
value env;

void initialize_registers() {
  cont = null();
  val  = null();
  expr = null();
  unev = null();
  proc = null();
  argl = null();
  env  = null();
}

/*
 * Utility functions for working with lists
 *
 * Simle convenience functions, used in C.
 */

value first(value pair)  { return car(pair);                }
value second(value pair) { return car(cdr(pair));           }
value third(value pair)  { return car(cdr(cdr(pair)));      }
value fourth(value pair) { return car(cdr(cdr(cdr(pair)))); }

/*
 * Standard functions
 *
 * Some of the standard Scheme functions, implemented in C. They are wrapped
 * as primitives to be available to the evaluator.
 */

int length(value list) {
  int result = 0;

  while (!null_p(list)) {
    list = cdr(list);
    result++;
  }

  return result;
}

bool eq(value v1, value v2) {
  return v1.type == v2.type && v1.value == v2.value;
}

bool equal(value v1, value v2) {
  if (v1.type != v2.type)
    return false;

  if (eq(v1, v2))
    return true;

  switch (v1.type) {
    case p_string:
      return strcmp((char *) v1.value, (char *) v2.value) == 0;

    case p_pair:
      return equal(car(v1), car(v2)) && equal(cdr(v1), cdr(v2));

    default:
      return false;
  }
}

/*
 * Environments
 *
 * Environments are implemented simply as lists, using the same structure as
 * the metacircular evaluator. This way they get to be garbage collected, even
 * if they are not optimal in variable lookup.
 *
 * Environments cannot be obtained with Scheme code, since they get
 * constructed only in the explicit-evaluator and are stored in registers or
 * stack. The only case when environments are stored in memory is when there
 * is a procedure in memory, but the environment cannot be obtained from
 * within the interpreter.
 */

value empty_environment() {
  return cons(cons(null(), cons(null(), null())), null());
}

value extend_environment(value vars, value vals, value enclosing) {
  if (length(vars) != length(vals))
    errorv("Argument count mismatch", cons(vars, cons(vals, null())));

  value frame = cons(vars, cons(vals, null()));
  return cons(frame, enclosing);
}

value lookup_variable(symbol name, value env) {
  while (!null_p(env)) {
    value frame = car(env);
    value vars  = car(frame);
    value vals  = car(cdr(frame));

    while (!null_p(vars)) {
      if (value_to_sym(car(vars)) == name)
        return car(vals);

      vars = cdr(vars);
      vals = cdr(vals);
    }

    env = cdr(env);
  }

  errors("Variable not found", name);
  exit(0);
}

void define_variable(symbol name, value val, value env) {
  value frame = car(env);
  value vars  = car(frame);
  value vals  = car(cdr(frame));

  set_car(frame, cons(sym(name), vars));
  set_car(cdr(frame), cons(val, vals));
}

void set_variable_value(symbol name, value val, value env) {
  while (!null_p(env)) {
    value frame = car(env);
    value vars  = car(frame);
    value vals  = car(cdr(frame));

    while (!null_p(vars)) {
      if (value_to_sym(car(vars)) == name) {
        set_car(vals, val);
        return;
      }

      vars = cdr(vars);
      vals = cdr(vals);
    }

    env = cdr(env);
  }

  errors("Variable not set", name);
  exit(0);
}

/*
 * The functions used by the explicit control evaluator.
 *
 * Those functions are used in the explicit control evaluator.
 */

bool true_p(value val) {
  return !boolean_p(val) || val.value != 0;
}

value make_procedure(value params, value body, value env) {
  return pcons(params, cons(body, cons(env, null())));
}

value procedure_parameters(value proc)  { return pcar(proc);         }
value procedure_body(value proc)        { return first(pcdr(proc));  }
value procedure_environment(value proc) { return second(pcdr(proc)); }

value empty_arglist() {
  return null();
}

value adjoin_arg(value arg, value arglist) {
  if (null_p(arglist))
    return cons(arg, null());
  else
    return cons(car(arglist), adjoin_arg(arg, cdr(arglist)));
}

value apply_primitive_procedure(value proc, value args) {
  if (!primitive_p(proc))
    errorv("Trying to apply a non-primitive procedure", proc);

  primitive_function function = (primitive_function) proc.value;

  return function(args);
}

/*
 * The primitive procedures
 *
 * Below are the C functions that represent all the primitive procedures in
 * the language
 */

value prim_add(value args) {
  long result = 0;

  while (!null_p(args)) {
    result += value_to_long(car(args));
    args = cdr(args);
  }

  return num(result);
}

value prim_sub(value args) {
  long result = value_to_long(car(args));
  args = cdr(args);

  if (null_p(args))
    return num(-result);

  while (!null_p(args)) {
    result -= value_to_long(car(args));
    args = cdr(args);
  }

  return num(result);
}

value prim_mul(value args) {
  long result = 1;

  while (!null_p(args)) {
    result *= value_to_long(car(args));
    args = cdr(args);
  }

  return num(result);
}

value prim_equal_sign(value args) {
  long num1 = value_to_long(first(args));
  long num2 = value_to_long(second(args));

  return booleanv(num1 == num2);
}

value prim_less_than_sign(value args) {
  long num1 = value_to_long(first(args));
  long num2 = value_to_long(second(args));

  return booleanv(num1 < num2);
}

value prim_eq_p(value args) {
  return booleanv(eq(first(args), second(args)));
}

value prim_equal_p(value args) {
  return booleanv(equal(first(args), second(args)));
}

value prim_number_p(value args) {
  return booleanv(number_p(first(args)));
}

value prim_string_p(value args) {
  return booleanv(string_p(first(args)));
}

value prim_symbol_p(value args) {
  return booleanv(symbol_p(first(args)));
}

value prim_pair_p(value args) {
  return booleanv(pair_p(first(args)));
}

value prim_null_p(value args) {
  return booleanv(null_p(first(args)));
}

value prim_car(value args) {
  return car(first(args));
}

value prim_cdr(value args) {
  return cdr(first(args));
}

value prim_cons(value args) {
  return cons(first(args), second(args));
}

value prim_list(value args) {
  return args;
}

value prim_set_car_bang(value args) {
  set_car(first(args), second(args));
  return sym(s_ok);
}

value prim_set_cdr_bang(value args) {
  set_cdr(first(args), second(args));
  return sym(s_ok);
}

value prim_display(value args) {
  print(first(args));

  return sym(s_ok);
}

value prim_newline(value args) {
  printf("\n");

  return sym(s_ok);
}

value prim_gc_runs(value args) {
  return num(gc_runs);
}

value prim_stack_max_depth(value args) {
  return num(STACK_SIZE);
}

/*
 * The global environment
 *
 * The code below initializes the global environment
 */

value global_env;

void add_primitive_to_env(value env, const char *name, primitive_function function) {
  define_variable(intern(name), primitive(function), env);
}

void initialize_global_environment() {
  global_env = empty_environment();

  define_variable(intern("true"),  truev(),  global_env);
  define_variable(intern("false"), falsev(), global_env);

  add_primitive_to_env(global_env, "+", prim_add);
  add_primitive_to_env(global_env, "-", prim_sub);
  add_primitive_to_env(global_env, "*", prim_mul);
  add_primitive_to_env(global_env, "=", prim_equal_sign);
  add_primitive_to_env(global_env, "<", prim_less_than_sign);

  add_primitive_to_env(global_env, "eq?", prim_eq_p);
  add_primitive_to_env(global_env, "equal?", prim_equal_p);

  add_primitive_to_env(global_env, "number?", prim_number_p);
  add_primitive_to_env(global_env, "string?", prim_string_p);
  add_primitive_to_env(global_env, "symbol?", prim_symbol_p);
  add_primitive_to_env(global_env, "pair?", prim_pair_p);
  add_primitive_to_env(global_env, "null?", prim_null_p);

  add_primitive_to_env(global_env, "car", prim_car);
  add_primitive_to_env(global_env, "cdr", prim_cdr);
  add_primitive_to_env(global_env, "cons", prim_cons);
  add_primitive_to_env(global_env, "list", prim_list);

  add_primitive_to_env(global_env, "set-car!", prim_set_car_bang);
  add_primitive_to_env(global_env, "set-cdr!", prim_set_cdr_bang);

  add_primitive_to_env(global_env, "display", prim_display);
  add_primitive_to_env(global_env, "newline", prim_newline);

  add_primitive_to_env(global_env, "gc-runs", prim_gc_runs);
  add_primitive_to_env(global_env, "stack-max-depth", prim_stack_max_depth);
}

/*
 * The garbage collector
 *
 * This is based on the stop-and-copy garbage collector of section 5.3. It
 * uses a few modifications, namely how root is handled.
 *
 * Instead of having a root, we start by relocating all the registers and
 * stack. Afterwards, we run the garbage collection as usual. When it scan
 * overtakes free, we update the registers and stack pointers before flipping
 * the memory. If there are pairs in the stack or registers, they point to an
 * address in the old memory. Since it is already relocated, it will contain
 * broken hearts and forwarding addresses. All those pairs get updated with
 * the forwarding address.
 *
 * Note that procedures are treated as pairs for the purpose of garbage
 * collection.
 *
 * There is a fine point when the garbage collector is run. Before evaluating
 * an expressing, the explicit-control evaluator checks to see if there is
 * enough free memory (where "enough" is defined by MEMORY_WIGGLE_ROOM). If
 * not, garbage collection is triggered.
 *
 * The alternative approach will be to do that in the cons() call when we're
 * running out of memory. While that would be better, it poses two problems.
 *
 * First, the car and cdr passed to cons will contain addresses to the old
 * memory. This can be solved simply by pushing them on the stack, garbage
 * collecting and poping. That would solve the problem.
 *
 * The second is more subtle. Since the C code calls cons(), that might
 * trigger garbage collection. Let's say there is a call like this:
 *
 * cons(cons(num(1), num(2)), cons(num(3), num(4)))
 *
 * The third call to cons takes two pairs as arguments - (1 . 2) and (3 . 4).
 * If the second call triggers garbage collection, the result will be an
 * address in the new memory (provided we solved the first problem), but the
 * first pair will still have the old address. That will result to an invalid
 * pair.
 *
 * A solution I can think of is if the C cons function does not take its
 * arguments directly, but from the Scheme stack instead. That way if a cons
 * triggers garbage collection, all the intermediate pairs will be in the
 * stack and relocated from the garbage collecter.
 *
 * This is an interesting change to the code that I might revisit some day.
 * Until then, there is a preemptive check if garbage collection needs to run
 * on each evaluation of an expression.
 *
 * Garbage collection is triggered by the gc() function. I've always wanted to
 * write a function called "gc";
 */

int gc_scan;
int gc_free;

void relocate(value);
void update(value *);
void relocate_registers_and_stack();
void update_registers_and_stack();
void update_value_from_old_memory(value *);

void gc() {
  gc_scan = 0;
  gc_free = 0;

  relocate_registers_and_stack();

  while (gc_scan < gc_free) {
    update(&free_mem->cars[gc_scan]);
    update(&free_mem->cdrs[gc_scan]);

    gc_scan++;
  }

  update_registers_and_stack();

  memory *tmp = working_mem;
  working_mem = free_mem;
  free_mem    = tmp;

  free_ptr = gc_free;
  gc_runs  += 1;
}

void relocate(value val) {
  if (!pair_p(val) && !procedure_p(val))
    return;

  int old_address = val.value;

  if (broken_heart_p(working_mem->cars[old_address]))
    return;

  free_mem->cars[gc_free] = working_mem->cars[old_address];
  free_mem->cdrs[gc_free] = working_mem->cdrs[old_address];

  working_mem->cars[old_address] = broken_heart();
  working_mem->cdrs[old_address] = num(gc_free);

  gc_free++;
}

void update(value *val) {
  if (!pair_p(*val) && !procedure_p(*val))
    return;

  relocate(*val);

  int old_address = val->value;
  int new_address = working_mem->cdrs[old_address].value;

  val->value = new_address;
}

void relocate_registers_and_stack() {
  relocate(val);
  relocate(expr);
  relocate(unev);
  relocate(proc);
  relocate(argl);
  relocate(env);
  relocate(cont);
  relocate(global_env);

  for(int i = 0; i < stack_ptr; i++) {
    relocate(stack[i]);
  }
}

void update_registers_and_stack() {
  update_value_from_old_memory(&val);
  update_value_from_old_memory(&expr);
  update_value_from_old_memory(&unev);
  update_value_from_old_memory(&proc);
  update_value_from_old_memory(&argl);
  update_value_from_old_memory(&env);
  update_value_from_old_memory(&cont);
  update_value_from_old_memory(&global_env);

  for(int i = 0; i < stack_ptr; i++) {
    update_value_from_old_memory(&stack[i]);
  }
}

void update_value_from_old_memory(value *val) {
  if (!pair_p(*val) && !procedure_p(*val))
    return;

  val->value = working_mem->cdrs[val->value].value;
}

void gc_if_necessary() {
  if (MEMORY_SIZE - free_ptr < MEMORY_WIGGLE_ROOM) {
    gc();
  }
}

/*
 * The printer
 *
 * This is based on the code in exercise 4.34. It was nice to be able to reuse
 * an algorithm I though about on a piece of paper.
 */

void print_cdr(value v) {
  switch (v.type) {
    case p_null:
      printf(")");
      break;
    case p_pair:
      printf(" ");
      print(car(v));
      print_cdr(cdr(v));
      break;
    default:
      printf(" . ");
      print(v);
      printf(")");
  }
}

void print(value v) {
  switch (v.type) {
    case p_null:
      printf("()");
      break;
    case p_boolean:
      if (v.value)
        printf("#t");
      else
        printf("#f");
      break;
    case p_number:
      printf("%ld", (long) v.value);
      break;
    case p_symbol:
      printf("%s", (symbol) v.value);
      break;
    case p_string:
      printf("%s", (const char *) v.value);
      break;
    case p_pair:
      printf("(");
      print(car(v));
      print_cdr(cdr(v));
      break;
    case p_procedure:
      printf("<procedure:%d>", (int) v.value);
      break;
    case p_primitive:
      printf("<primitive:%ld>", (long) v.value);
      break;
    case p_label:
      printf("<label:%ld>", (unsigned long) v.value);
      break;
    case p_broken_heart:
      printf("<broken-heart>");
      break;
  }
}

/*
 * The parser
 *
 * It is a simple, hand-written parser. I managed to whip it up why almost
 * asleep and without consulting the Dragon Book. I'm not extremely proud, so
 * I will gloss over details.
 *
 * It calls sym() and str(), which makes it allocate strings and symbols
 * freely. If pointers to those strings are garbage collected, this will
 * result to memory leaks.
 */

const char *input;
char lookahead;
int pos = 0;

value parse_sexp();

value read(const char *string) {
  input     = string;
  pos       = 0;
  lookahead = input[pos];

  return parse_sexp();
}

char advance() {
  if (lookahead == '\0')
    error("Unexpected end of input");

  char result = input[pos];
  lookahead = input[++pos];

  return result;
}

void match(char c) {
  if (lookahead != c)
    errorc("Expected character", c);
  advance();
}

void whitespace() {
  while (lookahead == ' ' || lookahead == '\n' || lookahead == ';') {
    if (lookahead == ';') {
      match(';');
      while (lookahead != '\n') advance();
      match('\n');
    } else {
      advance();
    }
  }
}

value parse_symbol() {
  char buffer[MAX_TOKEN_LENGTH + 1];
  int i = 0;

  while (lookahead != ' ' && lookahead != '\n' && lookahead != ')' && lookahead != '\0') {
    buffer[i++] = advance();

    if (i > MAX_TOKEN_LENGTH)
      errort("Parser encountered a token that is too long", buffer);
  }

  buffer[i] = '\0';

  return sym(buffer);
}

value parse_number() {
  char buffer[MAX_TOKEN_LENGTH + 1];
  int i = 0;

  while ('0' <= lookahead && lookahead <= '9') {
    buffer[i++] = advance();

    if (i > MAX_TOKEN_LENGTH)
      errort("Parser encountered a token that is too long", buffer);
  }

  buffer[i] = '\0';

  return num(atoi(buffer));
}

value parse_string() {
  char buffer[MAX_TOKEN_LENGTH + 1];
  int i = 0;

  match('"');

  while (lookahead != '"') {
    buffer[i++] = advance();

    if (i > MAX_TOKEN_LENGTH)
      errort("Parser encountered a token that is too long...", buffer);
  }

  match('"');

  buffer[i] = '\0';

  return str(buffer);
}

value parse_quote() {
  match('\'');
  value quoted = parse_sexp();

  return cons(sym("quote"),
              cons(quoted, null()));
}

value parse_list() {
  whitespace();

  if (lookahead == ')') {
    match(')');
    return null();
  } else if (lookahead == '.') {
    match('.');
    value sexp = parse_sexp();
    whitespace();
    match(')');
    return sexp;
  } else {
    value car = parse_sexp();
    whitespace();
    value cdr = parse_list();
    return cons(car, cdr);
  }
}

value parse_sexp() {
  whitespace();
  if (lookahead == '(') {
    match('(');
    return parse_list();
  } else if (lookahead == '\'') {
    return parse_quote();
  } else if ('0' <= lookahead && lookahead <= '9') {
    return parse_number();
  } else if (lookahead == '"') {
    return parse_string();
  } else {
    return parse_symbol();
  }
}

/*
 * Syntax functions
 *
 * The familiar syntax functions that are used to figure out what to evaluate.
 */

bool tagged_list_p(value expr, symbol tag) {
  if (expr.type == p_pair) {
    value head = car(expr);
    return head.type == p_symbol && ((const char *) head.value) == tag;
  } else {
    return false;
  }
}

bool self_evaluating_p(value expr) { return number_p(expr) || string_p(expr); }
bool variable_p(value expr)        { return symbol_p(expr);                   }
bool quoted_p(value expr)          { return tagged_list_p(expr, s_quote);     }
bool begin_p(value expr)           { return tagged_list_p(expr, s_begin);     }
bool if_p(value expr)              { return tagged_list_p(expr, s_if);        }
bool definition_p(value expr)      { return tagged_list_p(expr, s_define);    }
bool assignment_p(value expr)      { return tagged_list_p(expr, s_set_bang);  }
bool lambda_p(value expr)          { return tagged_list_p(expr, s_lambda);    }
bool application_p(value expr)     { return pair_p(expr);                     }

value text_of_quotation(value expr) { return car(cdr(expr)); }

value if_predicate(value expr)   { return second(expr); }
value if_consequent(value expr)  { return third(expr);  }
value if_alternative(value expr) { return fourth(expr); }

value operator(value expr)     { return car(expr);    }
value operands(value expr)     { return cdr(expr);    }
bool no_operands_p(value expr) { return null_p(expr); }

value lambda_parameters(value expr) { return car(cdr(expr)); }
value lambda_body(value expr)       { return cdr(cdr(expr)); }

value assignment_variable(value expr) { return second(expr); }
value assignment_value(value expr)    { return third(expr);  }

value make_lambda(value parameters, value body) {
  return cons(sym(s_lambda), cons(parameters, body));
}

value definition_variable(value expr) {
  if (symbol_p(car(cdr(expr))))
    return car(cdr(expr));
  else
    return car(car(cdr(expr)));
}

value definition_value(value expr) {
  if (symbol_p(car(cdr(expr))))
    return car(cdr(cdr(expr)));
  else
    return make_lambda(cdr(car(cdr(expr))),
                       cdr(cdr(expr)));
}

value begin_actions(value expr) { return cdr(expr); }
value first_exp(value expr)     { return car(expr); }
value rest_exps(value expr)     { return cdr(expr); }
int last_exp_p(value expr)      { return pair_p(expr) && null_p(cdr(expr)); }

value first_operand(value expr) { return car(expr); }
value rest_operands(value expr) { return cdr(expr); }
int last_operand_p(value expr)  { return pair_p(expr) && null_p(cdr(expr)); }

/*
 * The explicit-control evaluator
 *
 * I've chosen to translate this line-by-line for two reasons. First, I get
 * one more chance to write the explicit evaluator, using C this time, which
 * was an interesting exercise. Second, this gives me tail-call optimizations
 * for free, which is way better than figuring out a way to implement them
 * myself.
 *
 * As a fun note, we're storing pointers to labels, which I did not know was
 * possible.
 */

value start() {
  cont = label(&&done);

eval_dispatch:
  gc_if_necessary();
  if (self_evaluating_p(expr))
    goto ev_self_eval;
  if (variable_p(expr))
    goto ev_variable;
  if (quoted_p(expr))
    goto ev_quoted;
  if (assignment_p(expr))
    goto ev_assignment;
  if (if_p(expr))
    goto ev_if;
  if (definition_p(expr))
    goto ev_definition;
  if (begin_p(expr))
    goto ev_begin;
  if (lambda_p(expr))
    goto ev_lambda;
  if (application_p(expr))
    goto ev_application;
  errorv("Unrecognized expression", expr);

ev_self_eval:
  val = expr;
  goto *value_to_label(cont);
ev_variable:
  val = lookup_variable(value_to_sym(expr), env);
  goto *value_to_label(cont);
ev_quoted:
  val = text_of_quotation(expr);
  goto *value_to_label(cont);
ev_lambda:
  unev = lambda_parameters(expr);
  expr = lambda_body(expr);
  val  = make_procedure(unev, expr, env);
  goto *value_to_label(cont);

ev_application:
  push(cont);
  push(env);
  unev = operands(expr);
  push(unev);
  expr = operator(expr);
  cont = label(&&ev_appl_did_operator);
  goto eval_dispatch;
ev_appl_did_operator:
  unev = pop();
  env  = pop();
  argl = empty_arglist();
  proc = val;
  if (no_operands_p(unev))
    goto apply_dispatch;
  push(proc);
ev_appl_operand_loop:
  push(argl);
  expr = first_operand(unev);
  if (last_operand_p(unev))
    goto ev_appl_last_arg;
  push(env);
  push(unev);
  cont = label(&&ev_appl_accumulate_arg);
  goto eval_dispatch;
ev_appl_accumulate_arg:
  unev = pop();
  env  = pop();
  argl = pop();
  argl = adjoin_arg(val, argl);
  unev = rest_operands(unev);
  goto ev_appl_operand_loop;
ev_appl_last_arg:
  cont = label(&&ev_appl_accum_last_arg);
  goto eval_dispatch;
ev_appl_accum_last_arg:
  argl = pop();
  argl = adjoin_arg(val, argl);
  proc = pop();
  goto apply_dispatch;

apply_dispatch:
  if (primitive_p(proc))
    goto primitive_apply;
  if (procedure_p(proc))
    goto compound_apply;
  errorv("Unknown procedure type", proc);
primitive_apply:
  val  = apply_primitive_procedure(proc, argl);
  cont = pop();
  goto *value_to_label(cont);
compound_apply:
  unev = procedure_parameters(proc);
  env  = procedure_environment(proc);
  env  = extend_environment(unev, argl, env);
  unev = procedure_body(proc);
  goto ev_sequence;

ev_begin:
  unev = begin_actions(expr);
  push(cont);
ev_sequence:
  expr = first_exp(unev);
  if (last_exp_p(unev))
    goto ev_sequence_last_exp;
  push(unev);
  push(env);
  cont = label(&&ev_sequence_continue);
  goto eval_dispatch;
ev_sequence_continue:
  env  = pop();
  unev = pop();
  unev = rest_exps(unev);
  goto ev_sequence;
ev_sequence_last_exp:
  cont = pop();
  goto eval_dispatch;

ev_if:
  push(expr);
  push(env);
  push(cont);
  cont = label(&&ev_if_decide);
  expr = if_predicate(expr);
  goto eval_dispatch;
ev_if_decide:
  cont = pop();
  env  = pop();
  expr = pop();
  if (true_p(val))
    goto ev_if_consequent;
ev_if_alternative:
  expr = if_alternative(expr);
  goto eval_dispatch;
ev_if_consequent:
  expr = if_consequent(expr);
  goto eval_dispatch;

ev_assignment:
  unev = assignment_variable(expr);
  push(unev);
  expr = assignment_value(expr);
  push(env);
  push(cont);
  cont = label(&&ev_assignment_1);
  goto eval_dispatch;
ev_assignment_1:
  cont = pop();
  env  = pop();
  unev = pop();
  set_variable_value(value_to_sym(unev), val, env);
  val  = sym(s_ok);
  goto *value_to_label(cont);

ev_definition:
  unev = definition_variable(expr);
  push(unev);
  expr = definition_value(expr);
  push(env);
  push(cont);
  cont = label(&&ev_definition_1);
  goto eval_dispatch;
ev_definition_1:
  cont = pop();
  env  = pop();
  unev = pop();
  define_variable(value_to_sym(unev), val, env);
  val  = sym(s_ok);
  goto *value_to_label(cont);
done:
  return val;
}

/*
 * The eval function
 */

value eval(value expression) {
  env  = global_env;
  expr = expression;

  start();

  return val;
}

/*
 * Running a file
 *
 * The file is read in a buffer and then the buffer is read and evaluated. The
 * buffer has a maximum size, over which the program will error out. Of
 * course, the buffer can be allocated dynamically, but this is just simpler.
 */

void run_file(const char *filename) {
  int chars_read;
  char buffer[MAX_FILE_SIZE];

  FILE *file = fopen(filename, "r");

  if (!file) {
    printf("Failed to open %s: %s\n", filename, strerror(errno));
    exit(0);
  }

  chars_read = fread(buffer, sizeof(char), MAX_FILE_SIZE, file);
  fclose(file);

  if (chars_read == MAX_FILE_SIZE) {
    printf("File %s is too big (more than %d bytes)\n", filename, MAX_FILE_SIZE);
    exit(0);
  }

  buffer[chars_read] = '\0';
  eval(read(buffer));
}

/*
 * The read-eval-print loop
 *
 * There is not much needed to explain here. repl() is another name I've
 * always wanted to give to a function.
 */

char line[MAX_LINE_LENGTH + 1];

value read_line() {
  if (!fgets(line, MAX_LINE_LENGTH, stdin))
    error("Line too long");

  return read(line);
}

void repl() {
  value input, result;

  printf("Structure and Interpretation of Computer Programs\n");
  printf("Exercise 5.51 - Scheme evaluator in C\n");

  while (true) {
    printf("> ");
    input = read_line();
    result = eval(input);
    print(result);
    printf("\n");
  }
}

/*
 * The main function
 *
 * It initializes everything first. Afterwards, it runs any files if passed as
 * command line arguments. Otherwise, it runs the REPL.
 */

int main(int argc, char *argv[]) {
  initialize_memory();
  initialize_symbols();
  initialize_registers();
  initialize_global_environment();

  if (argc > 1) {
    for (int i = 1; i < argc; i++) {
      run_file(argv[i]);
    }
  } else {
    repl();
  }
}
