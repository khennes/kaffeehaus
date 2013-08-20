import sys
import ply.lex as lex
import re
import header  # Header code for top of output JS file


""" TODO """
# Implement 'get' (scanf); handle constants; postfix ++ and --
# BNF grammar
# Improve error handling
# Implement lexical scope for kicks


### GLOBALS ###
token = None        # contains current token object
symbol_table = {}   # store instantiated symbol classes
token_stack = []    # contains remaining tokens
env = {}            # store declared variables for evaluation 
const_table = {}    # store variables for codegen
strings = []        # keep list of strings to convert to ints
heap_usage = 0      # keep track to allocate space for strings in heap
heap_access = []    # global list of string locations in memory


######################################
############ DEFINE TOKENS ###########
######################################

tokens_list = (
    'NUMBER',
    'INCLUDE',
    'COMMENT',
    'ID',
    'STRING',

    # Operators
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'POWER',
    'MODULO',
    'DOT',
    'INCREMENT',
    'DECREMENT',
    'EQUALS',
    'PLUSEQ',
    'MINUSEQ',
    'ISEQ',
    'ISNOTEQ',
    'GREATER',
    'LESSER',
    'LESSEQ',
    'GREATEQ',
    'BOOLAND',
    'BOOLOR',

    # Delimiters
    'NEWLINE',
    'COMMA',
    'LBRACK',
    'RBRACK',
    'LBRACE',
    'RBRACE',
    'LPAREN',
    'RPAREN'
    'HASH',
    'STCOMM',
    'ENDCOMM'
)

# Store reserved keywords in Python dictionary
reserved = { 'if' : 'IF',
    'then' : 'THEN',
    'else' : 'ELSE',
    'elsif' : 'ELSIF',
    'while' : 'WHILE',
    'for' : 'FOR',
    'def' : 'DEF',
    'none' : 'NONE',
    'var' : 'VAR',
    'bool' : 'BOOL',
    'true' : 'TRUE',
    'false' : 'FALSE',
    'int' : 'INT',
    'float' : 'FLOAT',
    'struct' : 'STRUCT',
    'char' : 'CHAR', 
    'array' : 'ARRAY',
    'break' : 'BREAK',
    'continue' : 'CONTINUE',
    'return' : 'RETURN',
    'get' : 'GET',  
    'print' : 'PRINT'
}

# Build list of tokens + reserved keywords
tokens = [ 'NUMBER', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER', 'MODULO',
            'DOT', 'INCREMENT', 'DECREMENT', 'EQUALS', 'PLUSEQ', 'MINUSEQ',
            'ISEQ', 'ISNOTEQ', 'GREATER', 'LESSER', 'LESSEQ', 'GREATEQ',
            'BOOLAND', 'BOOLOR', 'NEWLINE', 'COMMA', 'LBRACK', 'RBRACK',
            'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN', 'DEFCONST',
            'INCLUDE', 'COMMENT', 'ID', 'STRING', 'HASH', 'STCOMM', 'ENDCOMM'
            ] + list(reserved.values())


# Regular expression rules for simple tokens and reserved keywords
t_PLUS	    = r'\+'
t_MINUS	    = r'-'
t_TIMES	    = r'\*'
t_DIVIDE    = r'/'
t_POWER	    = r'\*\*'
t_MODULO    = r'%'
t_DOT       = r'.'
t_INCREMENT = r'\+\+'
t_DECREMENT = r'--'
t_EQUALS    = r'='
t_PLUSEQ    = r'\+='
t_MINUSEQ   = r'\-='
t_ISEQ	    = r'=='
t_ISNOTEQ   = r'!='
t_GREATER   = r'>'
t_LESSER    = r'<'
t_LESSEQ    = r'<='
t_GREATEQ   = r'>='
t_BOOLAND   = r'&&'
t_BOOLOR    = r'\|\|'
t_COMMA     = r','
t_LBRACK    = r'\['
t_RBRACK    = r'\]'
t_LBRACE    = r'{'
t_RBRACE    = r'}'
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_HASH      = r'\#'  # can mean include, define (constant), or inline comment
t_STCOMM    = r'\/\*'  
t_ENDCOMM   = r'\*\/'

"""
Function documentation strings: take single argument (instance of LexToken)
LexToken attributes:
* t.type: token type (as str); defaults to name following t_ prefix 
* t.value: lexeme (actual text matched)
* t.lineno: current line number
* t.lexpos: position of token relative to beginning of input text
"""

# Ignore tabs, spaces 
t_ignore = ' \t\v'

# t_NUMBER matches numbers and converts string to Python integer
def t_NUMBER(t):
    r'\d+' 
    r'[-+]?[0-9]*\.?[0-9]+'  # floating point numbers (does not handle exponents
    t.value = int(t.value)   # or scientific notation)
    return t

# Track line numbers
def t_NEWLINE(t):
    r'\n'
    t.lexer.lineno += len(t.value)
    return t

# Recognize, discard comments (no return value)
def t_COMMENT(t):
    r'\#.*|/\*(.|\n)*?\*/'
    pass

# Strings -- ok to have two function documentation strings?
def t_STRING(t):
    r'"(.*)"'
    r"'(.*)'"
    return t

# Check identifiers/names against reserved keywords
def t_ID(t):
    r'\$?[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')  # default to 'ID' if not a keyword
    return t

# Error handling rule
def t_error(t):
    print #Invalid character: '%s'# % t.value[0]
    t.lexer.skip(1)


#######################################
### CALL LEXING & PARSING FUNCTIONS ###
#######################################

def generate_tokens(program):    
    print "----------- Begin input program -----------"
    print program
    print "------------ End input program ------------"
    token_stream = []
    lexer = lex.lex()
    lexer.input(program)
    while True:
        tokens = lexer.token()
        if not tokens:
            break
        token_stream.append(tokens)
    return token_stream             


def tokenize(token_stream):
    global symbol_table, token_stack
    
    # ugly hack to remove leading, trailing newlines
    while token_stream[0].type == "NEWLINE":  
        del token_stream[0]
    while token_stream[-1].type == "NEWLINE":
        del token_stream[-1]

    for token in token_stream:
        if token.type in [ 'NUMBER', 'STRING', 'ID' ]:
            symbol = symbol_table[token.type]
        else:
            symbol = symbol_table[token.value]
        s = symbol(token.type, token.value, token.lineno, token.lexpos)
        token_stack.append(s)
        if not symbol:
            raise SyntaxError("Unknown operator (%r)" % token.type)
    return token_stack


# Tokenization entry point/command center
def start_lex(filename=None):
    global token, token_stack
    if not filename:
        filename = raw_input("> ")
    program = open(filename).read()
    token_stream = generate_tokens(program)
    token_stack = tokenize(token_stream)
    token = token_stack.pop(0)
    return token_stack


class Program(object):
    """ make this inherit from BaseSymbol """
    def __init__(self):
        self.lines = []

    def stmtd(self):
        while len(token_stack):
            expr = parse_expression()
            self.lines.append(expr)
        return self

    def eval(self, env=None):
        for line in self.lines:
            line.eval(env)
    
    def emit(self, c=None):
        jscode = []
        for line in self.lines:
            code = line.emit()
            print code
            jscode.append(code)
        return jscode

           
"""
def build_const_table(self):
    const_table = {}
    for line in self.lines:
        if token == "var":
            const_table[self.first] = # MEMORY LOCATION dependent on data type

    def constant_fold(self):
        pass
"""


######################################
############## ADVANCE ###############
######################################

# Check for errors before fetching next token
def advance(value=None):
    global token, token_stack
    if value:
        if token.ttype in [ 'ID', 'STRING', 'NUMBER' ]:
            if token.ttype != value:
                raise SyntaxError("Expected %r, not %r on %s" % (value, token.value, token.lineno))
        elif token.value != value:
            raise SyntaxError("Expected %r, not %r on %s" % (value, token.value, token.lineno))
    
    if len(token_stack) > 0:
        next_token = token_stack.pop(0)
        token = next_token
        if token.value == "\n":
            advance()
    else:
        return


######################################
### EXPRESSION & STATEMENT PARSERS ###
######################################

def parse_expression(rbp=0):
    global token
    t = token
    advance()
    if hasattr(t, "stmtd"):
        left = t.stmtd()
    elif hasattr(t, "nulld"):
        left = t.nulld()
        while rbp < token.leftbp:  # keep going till rbp >= current token's bp
            t = token
            advance()
            left = t.leftd(left)
        if token.value == "\n":
            advance()
    return left


# Parse a single statement
def parse_statement():
    global token
    return token.stmtd()


######################################
###### CREATE BASE TOKEN CLASS #######
######################################

class BaseSymbol:
    def __init__(self, ttype, value, lineno, lexpos):
        self.ttype = ttype
        self.value = value
        self.lineno = lineno
        self.lexpos = lexpos

    # for AST nodes
    first = None
    second = None
    third = None

    def nulld(self):
        raise SyntaxError("Syntax error: %r, line %r." % (self.value, self.lineno))

    def leftd(self, left):
        raise SyntaxError("Unknown operator: %r, line %r." % (self.value, self.lineno))

    # output Py string representation of abstract syntax tree (AST)
    def __repr__(self):
        if self.ttype in [ 'ID', 'STRING', 'NUMBER' ]:
            return "%s" % self.value  # changed from (%s %s)
        out = [self.value, self.first, self.second, self.third]
        out = map(str, filter(None, out))
        return "(" + " ".join(out) + ")"


######################################
########## SYMBOL FACTORY ############
######################################

def symbol(ttype, bp=0):
    try:
        NewSymbol = symbol_table[ttype]
    except KeyError:                    # if key missing, create new key/class
        class NewSymbol(BaseSymbol):    # inherit from BaseSymbol class
            pass                
        NewSymbol.__name__ = "symbol-" + ttype  # for debugging
        NewSymbol.ttype = ttype
        NewSymbol.leftbp = bp
        symbol_table[ttype] = NewSymbol
    else:
        NewSymbol.leftbp = max(bp, NewSymbol.leftbp)
    return NewSymbol


# Register simple tokens to symbol_table

symbol("ID").nulld = lambda self: self
ID_class = symbol("ID")
def eval_id(self, env):
    return env[self.value]
ID_class.eval = eval_id
def emit_id(self):
    if "$" in self.value:
        self.value = self.value.replace("$", "")
    return self.value
ID_class.emit = emit_id

symbol("NUMBER").nulld = lambda self: self
num_class = symbol("NUMBER")
def eval_num(self, env):
    return self.value 
num_class.eval = eval_num
def emit_num(self):
    return self.value
num_class.emit = emit_num

symbol("STRING").nulld = lambda self: self
string_class = symbol("STRING")
def eval_string(self, env):
    return self.value[1:-1]  # print out sans quotes
string_class.eval = eval_string
def emit_string(self):
    global heap_usage
    strings.append(self.value)  # add to global list of strings
    heap_access.append("U8[strings[%d]]>>%d" % (strings.index(self.value), heap_usage))  # assumes 8 bit ints
    heap_usage += len(self.value[1:-1]) + 1  # count num bytes to allocate in the heap
    return [ ord(letter) for letter in self.value[1:-1].strip('"') ]  # return list of ints
string_class.emit = emit_string

symbol("true").nulld = lambda self: self
true_class = symbol("true")
def eval_true(self, env):
    return True 
true_class.eval = eval_true
def emit_true(self):
    return "true" 
true_class.emit = emit_true

symbol("false").nulld = lambda self: self
false_class = symbol("false")
def eval_false(self, env):
    return False 
false_class.eval = eval_false
def emit_false(self):
    return "false"
false_class.emit = emit_false

symbol("none").nulld = lambda self: self
none_class = symbol("none")
def eval_none(self, env):
    return None 
none_class.eval = eval_none
def emit_none(self):
    return "void"
none_class.emit = emit_none

symbol("int").nulld = lambda self: self
int_class = symbol("int")
def eval_int(self, env):
    pass
int_class.eval = eval_int
def emit_int(self):
    return "int"
int_class.emit = emit_int 

symbol("bool").nulld = lambda self: self
bool_class = symbol("bool")
def eval_bool(self, env):
    pass
bool_class.eval = eval_bool
def emit_bool(self):
    return "bool"
bool_class.emit = emit_bool

symbol("float").nulld = lambda self: self
float_class = symbol("float")
def eval_float(self, env):
    pass
float_class.eval = eval_float
def emit_float(self):
    print self.value
float_class.emit = emit_float

symbol("struct").nulld = lambda self: self
struct_class = symbol("struct")
def eval_struct(self, env):
    pass
struct_class.eval = eval_struct 
def emit_struct(self):
    print self.value
struct_class.emit = emit_struct

symbol(")")
symbol(",")
symbol("]")
symbol("}")
symbol(";")
symbol("\n")
symbol("[", 150)
symbol("(", 150)
symbol(".", 150)


######################################
######### PREFIX OPERATORS ###########
######################################

def prefix(ttype, bp):
    def nulld(self):                        # attach nodes to nulld method
        self.first = parse_expression(bp)   # bp = rbp
        self.second = None
        return self
    def eval(self, env=None):
        pass
    def emit(self):
        pass
    sym = symbol(ttype)
    sym.nulld = nulld           # attach nulld, eval methods to symbol,
    sym.eval = eval             # add to symbol_table
    sym.emit = emit
    return sym

# Register operator symbols to symbol_table
# prefix("!", 20)
negative_class = prefix("-", 130)
def eval_negative(self, env):
    return -(self.first.eval(env))
negative_class.eval = eval_negative


######################################
######### INFIX OPERATORS ############
######################################

def infix(ttype, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp)
        return self
    def eval(self, env=None):
        pass
    def emit(self):
        pass
    sym = symbol(ttype, bp)
    sym.leftd = leftd
    sym.eval = eval 
    sym.emit = emit
    return sym


# Register symbols & respective eval methods to symbol_table

# Less than
lesser_class = infix("<", 60)
def eval_lesser(self, env):
    if self.first.eval(env) < self.second.eval(env):
        return True
    else:
        return False
lesser_class.eval = eval_lesser
def emit_lesser(self):
    return "%s < %s" % (self.first.emit(), self.second.emit())
lesser_class.emit = emit_lesser

# Less than or equal
lesseq_class = infix("<=", 60)
def eval_lesseq(self, env):
    if self.first.eval(env) <= self.second.eval(env):
        return True
    else:
        return False
lesseq_class.eval = eval_lesseq
def emit_lesseq(self):
    return "%s <= %s" % (self.first.emit(), self.second.emit())
lesseq_class.emit = emit_lesseq

# Greater than
greater_class = infix(">", 60)
def eval_greater(self, env):
    if self.first.eval(env) > self.second.eval(env):
        return True
    else:
        return False
greater_class.eval = eval_greater
def emit_greater(self):
    return "%s > %s" % (self.first.emit(), self.second.emit())
greater_class.emit = emit_greater

# Greater than or equal
greateq_class = infix(">=", 60)
def eval_greateq(self, env):
    if self.first.eval(env) >= self.second.eval(env):
        return True
    else:
        return False
greateq_class.eval = eval_greateq
def emit_greateq(self):
    return "%s >= %s" % (self.first.emit(), self.second.emit())
greateq_class.emit = emit_greateq

# Equality
iseq_class = infix("==", 60)
def eval_iseq(self, env):
    if self.first.eval(env) == self.second.eval(env):
        return True
    else:
        return False
iseq_class.eval = eval_iseq
def emit_iseq(self):
    return "%s === %s|0" % (self.first.emit(), self.second.emit())
iseq_class.emit = emit_iseq

# Non-equality
isnoteq_class = infix("!=", 60)
def eval_isnoteq(self, env):
    if self.first.eval(env) != self.second.eval(env):
        return True
    else:
        return False
isnoteq_class.eval = eval_isnoteq
def emit_isnoteq(self):
    return "%s !== %s|0" % (self.first.emit(), self.second.emit())
isnoteq_class.emit = emit_isnoteq

# Plus
plus_class = infix("+", 110)
def eval_plus(self, env):
    return self.first.eval(env) + self.second.eval(env)
plus_class.eval = eval_plus
def emit_plus(self):
    return "%s + %s" % (self.first.emit(), self.second.emit())
plus_class.emit = emit_plus

# Minus
minus_class = infix("-", 110)
def eval_minus(self, env):
    return self.first.eval(env) - self.second.eval(env)
minus_class.eval = eval_minus
def emit_minus(self):
    return "%s - %s" % (self.first.emit(), self.second.emit())
minus_class.emit = emit_minus

# Multiply
times_class = infix("*", 120)
def eval_times(self, env):
    return self.first.eval(env) * self.second.eval(env)
times_class.eval = eval_times
def emit_times(self):
    return "%s * %s" % (self.first.emit(), self.second.emit())
times_class.emit = emit_times

# Divide
divide_class = infix("/", 120)
def eval_divide(self, env):
    return self.first.eval(env) / self.second.eval(env)
divide_class.eval = eval_divide
def emit_divide(self):
    return "%s / %s" % (self.first.emit(), self.second.emit())
divide_class.emit = emit_divide

# Modulo
modulo_class = infix("%", 120)
def eval_modulo(self, env):
    return self.first.eval(env) % self.second.eval(env)
modulo_class.eval = eval_modulo
def emit_modulo(self):
    return "%s %% %s" % (self.first.emit(), self.second.emit())
modulo_class.emit = emit_modulo


######################################
### INFIX_R & ASSIGNMENT OPERATORS ###
######################################

def infix_r(ttype, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp-1)
        return self
    def eval(self, env=None):
        pass
    def emit(self):
        pass
    sym = symbol(ttype, bp)
    sym.leftd = leftd
    sym.eval = eval
    sym.emit = emit
    return sym


# Register symbols & respective eval methods to symbol_table 

# Assignment
equals_class = infix_r("=", 30)
def eval_equals(self, env):
    env[self.first] = self.second.eval(env)
equals_class.eval = eval_equals
def emit_equals(self):
    return "%s = %s|0" % (self.first.emit(), self.second.emit())
equals_class.emit = emit_equals

# Increment
increment_class = infix_r("+=", 10)
def eval_increment(self, env):
    env[self.first.value] = env[self.first.value] + self.second.eval(env)    
    return
increment_class.eval = eval_increment
def emit_increment(self):
    if self.second.emit() == 1:
        return "%s++" % self.first.emit()
    else:
        return "%s += %s" % (self.first.emit(), self.second.emit())
increment_class.emit = emit_increment

# Decrement
decrement_class = infix_r("-=", 10)
def eval_decrement(self, env):
    env[self.first] = self.first - self.second.eval(env)    
    return
decrement_class.eval = eval_decrement
def emit_decrement(self):
    return "%s -= %s" % (self.first.emit(), self.second.emit())
decrement_class.emit = emit_decrement

# Boolean 'or'
boolor_class = infix_r("||", 30)
def eval_boolor(self, env):
    if self.first.eval(env) or self.second.eval(env):
        return True
    else:
        return False
boolor_class.eval = eval_boolor
def emit_boolor(self):
    return "%s || %s" % (self.first.emit(), self.second.emit())
boolor_class.emit = emit_boolor

# Boolean 'and'
booland_class = infix_r("&&", 40)
def eval_booland(self, env):
    if self.first.eval(env) and self.second.eval(env):
        return True
    else:
        return False
booland_class.eval = eval_booland
def emit_booland(self):
    return "%s && %s" % (self.first.emit(), self.second.emit())
booland_class.emit = emit_booland

# Exponent 
power_class = infix_r("**", 140)
def eval_power(self, env):
    return self.first.eval(env) ** self.second.eval(env)
power_class.eval = eval_power
def emit_power(self):
    return "%s ** %s" % (self.first.emit(), self.second.emit())
power_class.emit = emit_power


######################################
####### SPECIAL CASE HANDLERS ########
######################################

# Function decorator to avoid repeating code
def method(NewSymbol):
    assert issubclass(NewSymbol, BaseSymbol)
    def bind(fn):
        setattr(NewSymbol, fn.__name__, fn)
    return bind


# Function calls
@method(symbol("("))
def leftd(self, left):
    self.first = left
    self.second = []
    while token.value != ")":
        self.second.append(parse_expression())
        if token.value != ",":
            break
        advance(",")
    advance(")")
    return self
@method(symbol("("))
def eval(self, env):
    fn = env[self.first.value]
    arglist = [ each.value for each in fn.second ]
    pass_values = [ item.eval(env) for item in self.second ]
    zipped = zip(arglist, pass_values)
    env.update(dict(zipped))             # create new var env for life of function
    return fn.third.eval(env)
@method(symbol("("))
def emit(self):
    fn = const_table[self.first.value]
    arglist = [ each for each in fn.second ]
    pass_values = [ item.emit() for item in self.second ]
    zipped = zip(arglist, pass_values)
    const_table.update(dict(zipped))
    return "%s(%s)" % (self.first, ", ".join([ each.emit() for each in self.second ])) 


# Dot notation (access struct members) 
@method(symbol("."))
def leftd(self, left):
    self.first = left
    self.second = token.value
    advance("ID")
    if token.value == "=":
        advance()
        self.third = parse_expression()
    advance()
    return self
@method(symbol("."))
def eval(self, env):
    if self.third: 
        env[self.first.value][self.second] = self.third
    return env[self.first.value]
@method(symbol("."))
def emit(self):
    if self.third:
        env[self.first.value][self.second] = self.third


# Lists 
@method(symbol("["))
def nulld(self):
    self.first = []
    while token.value != "]":  # TODO: Add extra 'if' to allow optional trailing commas
        self.first.append(parse_expression())
        if token.value != ",":
            break
        advance(",")
    advance("]")
    return self
@method(symbol("["))
def leftd(self, left):
    self.first = left
    self.second = parse_expression()
    advance("]")
    return self
@method(symbol("["))
def eval(self, env):
    if isinstance(self.first, list):
        return self.first
    else:
        array_name = env[self.first.value]
        return array_name[self.second.value] 
@method(symbol("["))
def emit(self):
    if isinstance(self.first, list):
        return self.first
    else:
        return "%s[%s]" % (self.first.value, self.second.value) 


# Statement blocks
@method(symbol("{"))
def nulld(self):
    statements = []
    while token.value != "}":
        statements.append(parse_expression())
        if token.value == ",":
            advance()
    advance("}")
    self.first = statements 
    return self
@method(symbol("{"))
def eval(self, env):
    for each in self.first:
        last_val = each.eval(env)
        if each.value == "return":  # or break?
            break
    return last_val  # Ruby-style: if no return statement, implicitly return last value
@method(symbol("{"))
def emit(self):
    return "{%s\n}" % "".join([ "\n\t" + each.emit() for each in self.first ])


# Helper method for statements
def statement(ttype, bp):
    def stmtd(self):
        self.first = parse_expression() 
        self.second = None 
        return self
    sym = symbol(ttype, bp)
    sym.stmtd = stmtd
    sym.eval = eval
    return sym


# Break
break_class = statement("break", 0)
def eval_break(self, env):
    while self.value == "break":
        break
break_class.eval = eval_break
def emit_break(self):
    return "break"
break_class.emit = emit_break


# Continue
continue_class = statement("continue", 0)
def eval_continue(self, env):
    while self.value == "continue":
        continue
continue_class.eval = eval_continue


# Get
get_class = statement("get", 20)
def eval_get(self, env):
    env[self.first.value] = raw_input("> ")  # get input from user
    return env[self.first.value]
get_class.eval = eval_get


# Return
return_class = statement("return", 0)
def eval_return(self, env):
    return self.first.eval(env)
return_class.eval = eval_return
def emit_return(self):
    # no return necessary when return type is void
    #if self.first:
    #    return "return %s" % self.first.emit()
    #else:
    return
return_class.emit = emit_return


# Print
print_class = statement("print", 0)
def eval_print(self, env):
    print self.first.eval(env)
print_class.eval = eval_print
def emit_print(self):
    # will have to pass console.log in to the asm.js module as an FFI (alias: log)
    return "log(%s);" % self.first.emit()
print_class.emit = emit_print


# Variable declarations
@method(symbol("var"))
def stmtd(self):
    self.first = token.value
    env[self.first] = None
    advance("ID")
    if not token.ttype in [ 'ID', 'INT', 'FLOAT', 'STRING', 'BOOL', 'STRUCT', 'LBRACK' ]:
        raise SyntaxError("Expected variable type.")
    if token.value == "[":      # check if array
        advance()
        type = []
        type.append(token)      # array size
        advance("NUMBER")
        advance("]")
        type.append(token.value)  # array type
        self.second = type      # for arrays, type = '[size]type'
    else:
        self.second = token.value  # variable type
    advance()
    if token.value == "=":
        advance()
        self.third = parse_expression()
    return self    
@method(symbol("var"))
def eval(self, env):
    if self.third:
        env[self.first] = self.third.eval(env)
@method(symbol("var"))
def emit(self):
    global const_table 
    # get rid of global prefix
    if "$" in self.first:
        self.first = self.first.replace('$', '')

    # upon assignment, store value in const_table
    if self.third:
        const_table[self.first] = self.third.emit()

    # if var is an array
    if isinstance(self.second, list):
        return "var %s = %s" % (self.first, const_table[self.first] if self.third else "[]")

    # int -> 32Uint 
    elif self.second == "int":
        return "var %s = %s|0;" % (self.first, const_table[self.first] if self.third else self.first)

    # float -> double
    elif self.second == "float":
        return "var %s = +(%s)" % (self.first, const_table[self.first] if self.third else self.first)

    # need to convert booleans to 0 and 1 
    elif self.second == "bool":
        return "var %s" % self.first  

    elif self.second == "struct":
        print "struct"
        return "var %s = %s" % (self.first, self.third.emit() if self.third else "{}")
    
    # if self.third == "none":
    #     self.second = "void"


# While-loop statements
@method(symbol("while"))
def stmtd(self):
    advance("(")
    self.first = parse_expression()
    advance(")")
    if token.value != "{":
        raise SyntaxError("Expected a block.")
    expressions = parse_expression()
    self.second = expressions
    return self
@method(symbol("while"))
def eval(self, env):
    while self.first.eval(env) == True:
        self.second.eval(env)
        if self.first.eval(env) == False:
            break
@method(symbol("while"))
def emit(self):
    return "while (%s) %s" % (self.first.emit(), self.second.emit())


# For-loop statements
@method(symbol("for"))
def stmtd(self):
    conditions = []
    advance("(")
    while token.value != ")":
        conditions.append(parse_expression())
        if token.value == ";":
            advance()
    advance(")")
    self.first = conditions
    if token.value != "{":
        raise SyntaxError("Expected a block.")
    expressions = parse_expression()
    self.second = expressions
    return self
@method(symbol("for"))
def eval(self, env):
    self.first[0].eval(env)
    while self.first[1].eval(env) == True:
        self.second.eval(env)
        self.first[2].eval(env)
@method(symbol("for"))
def emit(self):
    return "for (%s) %s" % (self.first[0].emit() + self.first[1].emit() + "; " + self.first[2].emit(), self.second.emit())

            
# If/elsif/else conditional statements 
@method(symbol("if"))
def stmtd(self):
    advance("(")
    self.first = parse_expression()  # predicate 
    advance(")")
    if token.value != "{":
        raise SyntaxError("Expected a block.")
    
    # list of expressions to execute if condition is True
    expressions = parse_expression()
    self.second = expressions

    # check for optional ternary operator
    if token.value == "elsif":
        self.third = parse_expression()
    elif token.value == "else":
        self.third = parse_expression()
    return self
@method(symbol("if"))
def eval(self, env):
    if self.first.eval(env) == True:
        return self.second.eval(env)
    elif self.third:
        return self.third.eval(env)
@method(symbol("if"))
def emit(self):
    if self.third:
        return "if (%s) %s %s" % (self.first.emit(), self.second.emit(), self.third.emit())
    else:
        return "if (%s) %s" % (self.first.emit(), self.second.emit())


# Elsif
@method(symbol("elsif"))
def stmtd(self):
    advance("(")
    self.first = parse_expression()  # predicate 
    advance(")")
    if token.value != "{":
        raise SyntaxError("Expected a block.")
    
    # list of expressions to execute if condition is True
    expressions = parse_expression()
    self.second = expressions

    # check for optional ternary operator
    if token.value == "elsif":
        self.third = parse_expression()
    elif token.value == "else":
        self.third = parse_expression()
    return self
@method(symbol("elsif"))
def eval(self, env):
    if self.first.eval(env) == True:
        return self.second.eval(env)
    elif self.third:
        return self.third.eval(env)
@method(symbol("elsif"))
def emit(self):
    if self.third:
        return "else if (%s) %s %s" % (self.first.emit(), self.second.emit(), self.third.emit())
    else:
        return "else if (%s) %s" % (self.first.emit(), self.second.emit())


# Else
@method(symbol("else"))
def stmtd(self):
    expressions = parse_expression()
    self.first = expressions
    return self
@method(symbol("else"))
def eval(self, env):
    return self.first.eval(env) 
@method(symbol("else"))
def emit(self):
    return "else %s" % self.first.emit()  # emit block


# Function declarations with "def"
@method(symbol("def"))
def stmtd(self):
    self.first = token.value
    advance("ID")
    arguments = []
    advance("(")
    while token.value != ")":
        arguments.append(parse_expression())
        if token.value != ",":
            break
        advance(",")
    self.second = arguments
    advance(")")
#   brace = advance("{")
    if token.value != "{":
        raise SyntaxError("Expected block.")
    self.third = parse_expression()
    return self
@method(symbol("def"))
def eval(self, env):
    env[self.first] = self
@method(symbol("def"))
def emit(self):
    global const_table  # do I need to declare this?
    const_table[self.first] = self  # save fn defs to const_table also, or to new fn mapping dict?
    return '''function %s(%s) %s''' \
            % (self.first, ", ".join([ each.emit() for each in self.second ]), self.third.emit())


######################################
######################################

def write_file(filename, jscode):
    global header, footer, heap_access
    f = open(filename + ".js", "w+")
    returns = "return { fizzbuzz: fizzbuzz }"
    output = header.TEMPLATE % (strings, jscode, ";\n\t".join([ each for each in heap_access ]), returns)
    f.write(output)
    f.close()

def main():
    filename = sys.argv[1] if len(sys.argv) > 1 else None
    start_lex(filename)
    prefix, suffix = filename.split(".")
    p = Program() 
    ast = p.stmtd().lines
    print "\nABSTRACT SYNTAX TREE: ", ast
    print "\nPython interpreter says: "
    p.eval(env)                     # pass in empty global env dictionary
    # p.build_const_table()         # produce table of fn names & vars & locations in memory
    print "\nOh look it's Javascript: "
    jscode = p.emit()
    write_file(prefix, "\n".join(jscode))  # must write type string to file
    return jscode

if __name__ == "__main__":
    main()
