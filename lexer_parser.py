import sys
import ply.lex as lex
import re

""" QUESTIONS, TODO """
# Why no tuples?
# Implement 'get' (scanf)
# Handle constants
# BNF grammar!
# Add postfix symbols ++ and --
# Add better error handling
# Make token ttype/value consistent across program... when you have time


### GLOBALS ###
token = None        # contains current token object
symbol_table = {}   # store instantiated symbol classes
token_stack = []    # contains remaining tokens
function_defs = {}  # map fn names to their code objects (for compiling)
env = {}      # store declared variables in separate table


#############################
####### DEFINE TOKENS #######
#############################

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
    #r'$[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')  # default to 'ID' if not a keyword
    return t

# Error handling rule
def t_error(t):
    print #Invalid character: '%s'# % t.value[0]
    t.lexer.skip(1)


#########################################
#### CALL LEXING & PARSING FUNCTIONS ####
#########################################

def generate_tokens(program):    
    print program
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
            result = line.eval(env)  # pass in env here?

##########################
######## ADVANCE #########
##########################

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


##########################################
##### EXPRESSION & STATEMENT PARSERS #####
##########################################

def parse_expression(rbp=0):
    global token
    t = token
    advance()
    if hasattr(t, "stmtd"):
        left = t.stmtd()
    elif hasattr(t, "nulld"):
        left = t.nulld()
        while rbp < token.leftbp:  # keep going till rbp > current token's bp
            t = token
            advance()
            left = t.leftd(left)
        if token.value == "\n":
            advance()
    return left


# Parse a single statement
def parse_statement():
    global token
    t = token
    return t.stmtd()


##################################
#### CREATE BASE TOKEN CLASS #####
##################################

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
            return "%s" % self.value                # changed from (%s %s)
        out = [self.value, self.first, self.second, self.third]
        out = map(str, filter(None, out))
        return "(" + " ".join(out) + ")"


#############################################
############## SYMBOL FACTORY ###############
#############################################

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
    env[self.value] = env.get(self.value, None)
    return env[self.value]
def emit_id(self):
    print self.value
ID_class.eval = eval_id
ID_class.emit = emit_id

symbol("NUMBER").nulld = lambda self: self
num_class = symbol("NUMBER")
def eval_num(self, env):
    return self.value 
def emit_num(self):
    print self.value
num_class.eval = eval_num
num_class.emit = emit_num

symbol("STRING").nulld = lambda self: self
string_class = symbol("STRING")
def eval_string(self, env):
    return self.value[1:-1]  # print out sans quotes
def emit_string(self):
    print self.value
string_class.eval = eval_string
string_class.emit = emit_string

symbol("true").nulld = lambda self: self
true_class = symbol("true")
def eval_true(self, env):
    return True 
def emit_true(self):
    print self.value
true_class.eval = eval_true
true_class.emit = emit_true

symbol("false").nulld = lambda self: self
false_class = symbol("false")
def eval_false(self, env):
    return False 
def emit_false(self):
    print self.value
false_class.eval = eval_false
false_class.emit = emit_false

symbol("none").nulld = lambda self: self
none_class = symbol("none")
def eval_none(self, env):
    return None 
def emit_none(self):
    print self.value
none_class.eval = eval_none
none_class.emit = emit_none

symbol("int").nulld = lambda self: self
int_class = symbol("int")
def eval_int(self, env):
    pass
def emit_int(self):
    print self.value
int_class.eval = eval_int
int_class.emit = emit_int 

symbol("bool").nulld = lambda self: self
bool_class = symbol("bool")
def eval_bool(self, env):
    pass
def emit_bool(self):
    print self.value
bool_class.eval = eval_bool
bool_class.emit = emit_bool

symbol("float").nulld = lambda self: self
float_class = symbol("float")
def eval_float(self, env):
    pass
def emit_float(self):
    print self.value
float_class.eval = eval_float
float_class.emit = emit_float

symbol("struct").nulld = lambda self: self
struct_class = symbol("struct")
def eval_struct(self, env):
    pass
def emit_struct(self):
    print self.value
struct_class.eval = eval_struct 
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


#######################################
########## PREFIX OPERATORS ###########
#######################################

def prefix(ttype, bp):
    def nulld(self):                        # attach nodes to nulld method
        self.first = parse_expression(bp)   # bp = rbp
        self.second = None
        return self
    def eval(self, env=None):
        pass
    sym = symbol(ttype)
    sym.nulld = nulld           # attach nulld, eval methods to symbol,
    sym.eval = eval             # add to symbol_table
    return sym

# Register operator symbols to symbol_table
# prefix("!", 20)
negative_class = prefix("-", 130)
def eval_negative(self, env):
    return -(self.first.eval(env))
negative_class.eval = eval_negative


#######################################
########## INFIX OPERATORS ############
#######################################

def infix(ttype, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp)
        return self
    def eval(self):
        pass
    sym = symbol(ttype, bp)
    sym.leftd = leftd
    sym.eval = eval 
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

# Less than or equal
lesseq_class = infix("<=", 60)
def eval_lesseq(self, env):
    if self.first.eval(env) <= self.second.eval(env):
        return True
    else:
        return False
lesseq_class.eval = eval_lesseq

# Greater than
greater_class = infix(">", 60)
def eval_greater(self, env):
    if self.first.eval(env) > self.second.eval(env):
        return True
    else:
        return False
greater_class.eval = eval_greater

# Greater than or equal
greateq_class = infix(">=", 60)
def eval_greateq(self, env):
    if self.first.eval(env) >= self.second.eval(env):
        return True
    else:
        return False
greateq_class.eval = eval_greateq

# Equality
iseq_class = infix("==", 60)
def eval_iseq(self, env):
    if self.first.eval(env) == self.second.eval(env):
        return True
    else:
        return False
iseq_class.eval = eval_iseq

# Non-equality
isnoteq_class = infix("!=", 60)
def eval_isnoteq(self, env):
    if self.first.eval(env) != self.second.eval(env):
        return True
    else:
        return False
isnoteq_class.eval = eval_isnoteq

# Plus
plus_class = infix("+", 110)
def eval_plus(self, env):
    return self.first.eval(env) + self.second.eval(env)
plus_class.eval = eval_plus

# Minus
minus_class = infix("-", 110)
def eval_minus(self, env):
    return self.first.eval(env) - self.second.eval(env)
minus_class.eval = eval_minus

# Multiply
times_class = infix("*", 120)
def eval_times(self, env):
    return self.first.eval(env) * self.second.eval(env)
times_class.eval = eval_times

# Divide
divide_class = infix("/", 120)
def eval_divide(self, env):
    return self.first.eval(env) / self.second.eval(env)
divide_class.eval = eval_divide

# Modulo
modulo_class = infix("%", 120)
def eval_modulo(self, env):
    return self.first.eval(env) % self.second.eval(env)
modulo_class.eval = eval_modulo


########################################
#### INFIX_R & ASSIGNMENT OPERATORS ####
########################################

def infix_r(ttype, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp-1)
        return self
    def eval(self):
        pass
    sym = symbol(ttype, bp)
    sym.leftd = leftd
    sym.eval = eval
    return sym


# Register symbols & respective eval methods to symbol_table 

# Assignment
equals_class = infix_r("=", 30)
def eval_equals(self, env):
    env[self.first] = self.second.eval(env)
    return
equals_class.eval = eval_equals

# Increment
increment_class = infix_r("+=", 10)
def eval_increment(self, env):
    env[self.first.value] = env[self.first.value] + self.second.eval(env)    
    return
increment_class.eval = eval_increment

# Decrement
decrement_class = infix_r("-=", 10)
def eval_decrement(self, env):
    env[self.first] = self.first - self.second.eval(env)    
    return
decrement_class.eval = eval_decrement

# Boolean 'or'
boolor_class = infix_r("||", 30)
def eval_boolor(self, env):
    if self.first.eval(env) or self.second.eval(env):
        return True
    else:
        return False
boolor_class.eval = eval_boolor

# Boolean 'and'
booland_class = infix_r("&&", 40)
def eval_booland(self, env):
    if self.first.eval(env) and self.second.eval(env):
        return True
    else:
        return False
booland_class.eval = eval_booland

# Exponent 
power_class = infix_r("**", 140)
def eval_power(self, env):
    return self.first.eval(env) ** self.second.eval(env)
power_class.eval = eval_power


########################################
######## SPECIAL CASE HANDLERS #########
########################################

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
    if token.value != ")":
        while True:
            self.second.append(parse_expression())
            if token.value != ",":
                break
            advance(",")
    advance(")")
    return self
@method(symbol("("))
def eval(self, env):
    arglist = env[self.first.value][0]
    zipped = zip(arglist, self.second)
    env.update(dict(zipped))             # create new var env for life of function
    env[self.first.value][1].eval(env)
    print env
    return env


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
    return env[self.first.value][self.second]


# Lists 
@method(symbol("["))
def nulld(self):
    print "NOW PARSE A LIST"
    self.first = []
    if token.value != "]":
        while True:  # TODO: Add extra 'if' to allow optional trailing commas
            if token.value == "]":
                break
            self.first.append(parse_expression())
            if token.value != ",":
                break
            advance(",")
    advance("]")
    return self
@method(symbol("["))
def eval(self, env):
    print "THIS IS THE LIST EVAL"
    print "SELF VALUE", self.value
    return self.value
    

@method(symbol("["))
def leftd(self, left):
    print "NOW PARSE A LOOKUP"
    self.first = left
    self.second = parse_expression()
    advance("]")
    return self  # return a lookup node - need a new class
@method(symbol("["))
def eval(self, env):
    print "THIS IS THE LOOKUP EVAL"
    print "self", self.first
    array_name = self.first
    index_pos = self.second
    return array_name[index_pos]


# Statement blocks
@method(symbol("{"))
def nulld(self):
    statements = []
    if token.value != "}":
        while True:
            if token.value == "}":
                break
            statements.append(parse_expression())
            if token.value == ",":
                advance()
    advance("}")
    self.first = statements 
    return self
@method(symbol("{"))
def eval(self, env):
    for each in self.first:
        each.eval(env)
    return env


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
    # break
    pass
break_class.eval = eval_break


# Continue
continue_class = statement("continue", 0)
def eval_continue(self, env):
    # continue
    pass
continue_class.eval = eval_continue


# Get
get_class = statement("get", 20)
def eval_get(self, env):
    # get input from user
    pass
get_class.eval = eval_get


# Return
return_class = statement("return", 0)
def eval_return(self, env):
    return self.first.eval(env)
return_class.eval = eval_return


# Print
print_class = statement("print", 0)
def eval_print(self, env):
    print self.first.eval(env)
print_class.eval = eval_print


# Variable declarations, with checks for arrays and structures
@method(symbol("var"))
def stmtd(self):
    self.first = token.value
    env[self.first] = None
    advance("ID")
    if not token.ttype in [ 'ID', 'INT', 'FLOAT', 'CHAR', 'BOOL', 'STRUCT', 'LBRACK' ]:
        raise SyntaxError("Expected variable type.")
    if token.value == "[":      # check if array type annotation
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
    #if self.second == 'struct':
    #    self.first = {}
    #if self.second not in [ 'int', 'float', 'char', 'bool' ]:
    #    env[self.first] = []
    if hasattr(self, "third"):
        env[self.first] = self.third.eval(env)


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


# For-loop statements
@method(symbol("for"))
def stmtd(self):
    conditions = []
    advance("(")
    if token.value != ")":
        while True:
            conditions.append(parse_expression())
            if token.value == ";":
                advance()
            if token.value == ")":
                break
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
        if self.first[1].eval(env) == False:
            break

            
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
    else:
        return self.third.eval(env)


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
    else:
        return self.third.eval(env)


@method(symbol("else"))
def stmtd(self):
    expressions = parse_expression()
    self.first = expressions
    return self
@method(symbol("else"))
def eval(self, env):
    return self.first.eval(env) 


# Function declarations with "def"
@method(symbol("def"))
def stmtd(self):
    self.first = token.value
    advance("ID")
    self.second = []  # store args and definition in right node
    arguments = []
    advance("(")
    if token.value != ")":
        while True:
            if token.value == ")":
                break
            arguments.append(parse_expression())
            if token.value != ",":
                break
            advance(",")
    self.second.append(arguments)
    advance(")")
#   brace = advance("{")
    if token.value != "{":
        raise SyntaxError("Expected block.")
    self.second.append(parse_expression())
    return self
@method(symbol("def"))
def eval(self, env):
    env[self.first] = self.second


def main():
    filename = sys.argv[1] if len(sys.argv) > 1 else None
    start_lex(filename)
    p = Program() 
    p.stmtd()
    p.eval(env)  # pass in empty global env dictionary

if __name__ == "__main__":
    main()
