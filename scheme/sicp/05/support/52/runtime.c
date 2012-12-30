/*
 * The code for exercise 5.52.
 *
 * I wrote it by copying the code for 5.51, removing the unneeded parts and
 * adding the required primitive operations. The new code is at the end of the
 * file, after the main function.
 *
 * All this code here is necessary as a runtime for the compiled Scheme
 * interpreter.
 *
 * It's noteworthy that the call to gc_if_necessary() is triggered from the
 * interpreter. There is a primitive procedure gc-if-necessary that wraps the
 * call and it is called before evaluating an expression.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <errno.h>

#define MAX_FILE_SIZE 10000
#define MAX_LINE_LENGTH 50
#define MAX_TOKEN_LENGTH 128
#define MEMORY_SIZE 10000
#define MEMORY_WIGGLE_ROOM 1000
#define STACK_SIZE 400

typedef enum p_type p_type;
typedef struct symbol_node symbol_node;
typedef struct pairs memory;
typedef struct value value;
typedef const char* symbol;
typedef value (*primitive_function)(value);

/*
 * Definitions for values and memory.
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
 */

value read(const char *);
value eval(value);
void print(value);
void gc_if_necessary();

/*
 * The number of times the garbage collector has run
 */

int gc_runs = 0;

/*
 * Error reporting
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
 */

bool test;
value cont;
value val;
value proc;
value argl;
value env;

void initialize_registers() {
  cont = null();
  val  = null();
  proc = null();
  argl = null();
  env  = null();
}

/*
 * Utility functions for working with lists
 */

value first(value pair)  { return car(pair);                }
value second(value pair) { return car(cdr(pair));           }
value third(value pair)  { return car(cdr(cdr(pair)));      }
value fourth(value pair) { return car(cdr(cdr(cdr(pair)))); }

/*
 * Standard functions
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
 * Applying a primitive procedure
 */

value apply_primitive_procedure(value proc, value args) {
  if (!primitive_p(proc))
    errorv("Trying to apply a non-primitive procedure", proc);

  primitive_function function = (primitive_function) proc.value;

  return function(args);
}

/*
 * The primitive procedures
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

value prim_gc_if_necessary(value args) {
  gc_if_necessary();
  return sym(s_ok);
}

value prim_gc_runs(value args) {
  return num(gc_runs);
}

value prim_stack_max_depth(value args) {
  return num(STACK_SIZE);
}

value prim_apply_primitive(value args) {
  value proc      = first(args);
  value arguments = second(args);

  if (!primitive_p(proc))
    errorv("Expected a primitive procedure", proc);

  return apply_primitive_procedure(proc, arguments);
}

value prim_error(value args) {
  value message = car(args);
  value info    = cdr(args);

  errorv((const char *) message.value, info);
  exit(0);
}

/*
 * The global environment
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

  add_primitive_to_env(global_env, "apply-primitive", prim_apply_primitive);
  add_primitive_to_env(global_env, "error", prim_error);

  add_primitive_to_env(global_env, "gc-runs", prim_gc_runs);
  add_primitive_to_env(global_env, "gc-if-necessary", prim_gc_if_necessary);
  add_primitive_to_env(global_env, "stack-max-depth", prim_stack_max_depth);
}

/*
 * The garbage collector
 */

int gc_scan;
int gc_free;

void relocate(value);
void update(value *);
void relocate_registers_and_stack();
void update_registers_and_stack();
void update_value_from_old_memory(value *);

bool repeat = true;

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
    repeat = true;
    gc();
  }
}

/*
 * The printer
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
    case p_pair:
      printf("(");
      print(car(v));
      print_cdr(cdr(v));
      break;
    case p_null:
      printf("()");
      break;
    case p_symbol:
      printf("%s", (symbol) v.value);
      break;
    case p_number:
      printf("%ld", (long) v.value);
      break;
    case p_string:
      printf("%s", (const char *) v.value);
      break;
    case p_procedure:
      printf("<procedure:%d>", (int) v.value);
      break;
    case p_primitive:
      printf("<primitive:%ld>", (long) v.value);
      break;
    case p_label:
      printf("<label:%ld>", (unsigned long) v.value);
      return;
    case p_boolean:
      if (v.value)
        printf("#t");
      else
        printf("#f");
      break;
    case p_broken_heart:
      printf("<broken-heart>");
      break;
  }
}

/*
 * The parser
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
 * Running a file
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
  printf("Exercise 5.52 - (Compiled) Scheme in C\n");

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

/*
 * Primitive operations
 *
 * Those correspond to the primitive operations that the compiler expects.
 * They are, of course, implemented in C.
 */

bool primitive_procedure_p(value proc) {
  return primitive_p(proc);
}

value lookup_variable_value(value name, value env) {
  return lookup_variable(value_to_sym(name), env);
}

value list(value singleton) {
  return cons(singleton, null());
}

value make_compiled_procedure(value entry, value env) {
  return pcons(entry, env);
}

value compiled_procedure_entry(value proc) {
  return pcar(proc);
}

value compiled_procedure_env(value proc) {
  return pcdr(proc);
}

void set_variable_value_bang(value name, value val, value env) {
  set_variable_value(value_to_sym(name), val, env);
}

void define_variable_bang(value name, value val, value env) {
  define_variable(value_to_sym(name), val, env);
}

bool true_p(value val) {
  return !boolean_p(val) || val.value != 0;
}

bool false_p(value val) {
  return !true_p(val);
}

/*
 * The eval function
 *
 * The function is similar to what we do in section 5.5, except that the
 * read-eval-print loop is written in C. The first time the function is called
 * it runs the compiled interpreter in order to populate the global
 * environment. Afterwards the interpreter code is skipped and the function
 * jumps directly to evaluating the passed expression.
 *
 * The expression is evaluated in a tricky way. The C code knows that the
 * compiled interpreter defines a procedure eval* that evalutes and expression
 * in the global environment. It finds that procedure in the environment and
 * calls it. That happens by assigning it to proc, assigning its argument
 * (wrapped in a list) to argl and jumping to its entry point. Upon completion
 * the result is in val.
 */

bool interpreter_loaded = false;

value eval(value expr) {
  push(expr);
  env  = global_env;

  if (interpreter_loaded)
    goto evaluate_expression;

#include "../bin/compiled_interpreter"
  interpreter_loaded = true;

evaluate_expression:
  cont = label(&&done);
  proc = lookup_variable(intern("eval*"), env);
  val  = compiled_procedure_entry(proc);
  argl = list(expr);
  goto *value_to_label(val);

done:
  pop();
  return val;
}
