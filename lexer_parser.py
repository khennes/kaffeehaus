import sys
import ply.lex as lex
import re

""" QUESTIONS, TODO """
# Must I parse statements differently from expressions?
# How to implement strings?
# Scope
# Also need !, ., 
# Read up on Python decorators
# Get parser to handle names ('ID')
# Padrino vs. Sinatra
# Why no tuples? (explain: tuples vs. lists?)
# Constants
# Should reserved keywords each be handled by separate function?
# Or should they be treated as identifiers?
# TODO: STATEMENTS, SCOPE


### GLOBALS ###
token = None        # contains current token object
next                # holds next token obj in token_stream
symbol_table = {}   # store symbol classes
token_stream = None # does this need to be global?
scope = None        # contains current scope object


### LEXER ###

# LexToken(self.type, self.value, self.lineno, self.lexpos)

tokens_list = (
    'NUMBER',
    'GLOBAL',
    'INCLUDE',
    'COMMENT',
    'ID',

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
    'ISEQ',
    'ISNOTEQ',
    'GREATER',
    'LESS',
    'LESSEQ',
    'GREATEQ',
    'BOOLAND',
    'BOOLOR',

    # Delimiters
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
    'bool' : 'BOOL',
    'true' : 'TRUE',
    'false' : 'FALSE',
    'int' : 'INT',
    'float' : 'FLOAT',
    'struct' : 'STRUCT',
    'char' : 'CHAR', 
    'string' : 'STRING',  # Undecided how to implement (most likely as struct)
    'array' : 'ARRAY',
    'break' : 'BREAK',
    'continue' : 'CONTINUE',
    'return' : 'RETURN',
    'get' : 'GET',  
    'print' : 'PRINT'
}

# Build list of tokens + reserved keywords
tokens = [ 'NUMBER', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER', 'MODULO',
            'DOT', 'INCREMENT', 'DECREMENT', 'EQUALS', 'ISEQ', 'ISNOTEQ', 'GREATER',
            'LESS', 'LESSEQ', 'GREATEQ', 'BOOLAND', 'BOOLOR', 'COMMA', 'LBRACK',
            'RBRACK', 'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN', 'GLOBAL',
            'DEFCONST', 'INCLUDE', 'COMMENT', 'ID', 'HASH', 'STCOMM', 'ENDCOMM'
            ] + list(reserved.values())


# Regular expression rules for simple tokens
# Python raw strings are used for convenience
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
t_ISEQ	    = r'=='
t_ISNOTEQ   = r'!='
t_GREATER   = r'>'
t_LESS	    = r'<'
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
t_GLOBAL    = r'\$'
t_HASH      = r'\#'  # can mean include, define (constant), or inline comment
t_STCOMM    = r'\/\*'  
t_ENDCOMM   = r'\*\/'

"""
If some action needs to be taken, define rules by functions.
t_NUMBER matches numbers and converts string -> Python integer
Function documentation string: takes single argument (instance of LexToken)
LexToken attributes:
* t.type: token type (as str); defaults to name following t_ prefix 
* t.value: lexeme (actual text matched)
* t.lineno: current line number
* t.lexpos: position of token relative to beginning of input text
"""

# Ignore tabs, spaces 
t_ignore = ' \t'

def t_NUMBER(t):
    r'\d+' 
    t.value = int(t.value)  # How to handle floats?
    return t

# Track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Recognize, discard comments (no return value)
def t_COMMENT(t):
    r'\#.*|/\*(.|\n)*?\*/'  # Test for inline AND multiline comments
    pass

# Check identifiers/names against reserved keywords
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')  # default to 'ID' if not a keyword
    return t

# Error handling rule
def t_error(t):
    print #Invalid character: '%s'# % t.value[0]
    t.lexer.skip(1)


### CALL LEXING & PARSING FUNCTIONS ###

def generate_tokens(program):    
    token_stream = []
    print program
    lexer = lex.lex()       # build lexer
    lexer.input(program)

    while True:
        tokens = lexer.token()
        if not tokens:
            break           # no more input
        token_stream.append(tokens)  # creating a list rather than generator
    return token_stream             


######## TODO: variable names raise KeyError #########

def tokenize(token_stream):
    global symbol_table
    print token_stream
    for token in token_stream:      # token = LexToken object
        if token.type == "NUMBER":  # renamed 'ttype' to 'type' (Ply docs)
            print "YES A NUMBER", token.value
            symbol = symbol_table["NUMBER"]  
            s = symbol(token.type, token.value, token.lineno, token.lexpos)
            # s = Literal(token.value)
            # s.value = value  
        else:                       # if name or operator
            print "NOT A NUMBER", token.type, token.value
            symbol = symbol_table.get(token.value)
            if symbol:
                s = symbol(token.type, token.value, token.lineno, token.lexpos)
                # s = NewSymbol(token.type, token.value, token.lineno, token.lexpos)
            elif token.type == "ID":
                print "SYMBOL IS A NAME/ID", token.value
                symbol = symbol_table[token.type]
                s = symbol(token.type, token.value, token.lineno, token.lexpos)
                # s = NewSymbol(token.type, token.value, token.lineno, token.lexpos)
                # s.value = value
            else:
                raise SyntaxError("Unknown operator (%r)" % id)
        print "TOKENIZE -> S: ", s
        yield s                     # yields a generator


def parse():
    global token, next 
    filename = raw_input("> ")      # get program from user
    program = open(filename).read()
    token_stream = generate_tokens(program)
    next = tokenize(token_stream).next  # what exactly does this do?
    token = next()                      # TODO: explore Crockford's advance()
    print "TOKEN: ", token
    return parse_expression()  

"""
### SCOPE ###
# Default scope is local rather than $global

class Rule(object):
    def nulld(self, ....):
        raise NotImplementedError

class Scope(object):

    def __init__(self,token_name):
        self.token_name = token_name  # new variable
        self.parent = None

    def define(self, token_name):
        if 
        self.reserved = False
        self.nulld = itself  # what
        self.leftd = null
        self.leftbp = 0
        self.scope = scope
    
    def find(self, token_name):
        while True:
            
    
    t.type = reserved.get(t.value, 'ID')
    
    def pop(self):  # close scope, return focus to parent
        global scope
        s = scope
        scope = s.parent
        # return scope? self?

    # how to handle? what is the difference betw using an already-defined
    # and a reserved name?
    def reserve(token_name):
        pass

def new_scope():
    # if token.id == "ID" and token is not reserved keyword
    global scope, token
    s = scope
    scope = Scope(token.value) 
"""

"""
BINDING POWERS (from Crockford)
THESE WILL CHANGE
* nulld used by values (literals, variables) & prefix operators
* leftd used by infix & suffix operators
* binding power = how tightly expression binds to tokens on right side
In example "1 + 2 * 4", you have +, 2, and *. * has higher bp, so it 'wins' 2.

0   nonbinding ;
10  assignment =
20  ?
30  ||, &&
40  relational ===
50  +, -
60  *, /
70  unary !
80  ., [, (
"""

### EXPRESSION PARSER ###
def parse_expression(rbp=0):    # default binding power = 0 
    global token
    t = token
    try:
        token = next()              # to handle StopIteration exception 
    except StopIteration:           # a more elegant way to do this?
        pass
    left = t.nulld()
    while rbp < token.leftbp:       # keep going till rbp > current token's bp
        t = token
        token = next()
        left = t.leftd(left)
    return left


### STATEMENT PARSER ###

# Parse a single statement at a time
def parse_statement():
    global token
    t = token
    if t.stmtd:         # where to add this method?
        try:
            token = next()
        except StopIteration:
            pass
        # scope.reserve(t)
        return t.stmtd()
    expression = parse_expression()
    # TODO: if not an assignment and not "(", throw error
    advance(";")
    return expression


# Continue parsing all statements in a row, return list
def parse_stmts():
    stmtlist = []
    while True:
        if token.value == "}" or token.value == "(end)":
            break
        s = parse_statement()
        if s:
            stmtlist.append(s)
    if len(stmtlist) == 0:
        stmtlist = null
    elif len(stmtlist) == 1:
        stmtlist = stmtlist[0]
    return stmtlist


### TOKEN CLASSES ##"

# WHERE DO THESE COME IN? So far, everything passes through BaseSymbol instead?
# for numbers and constants
class Literal(object):
    def __init__(self, value):
        self.value = value

    def nulld(self):
        return self  # token literals return themselves

    """ for error-checking: outputs Py string representation of AST """
    def __repr__(self):
        return "(number %s)" % self.value

# base class for operators
class BaseSymbol(object):
    def __init__(self, ttype, value, lineno, lexpos):
        self.ttype = ttype  # token type
        self.value = value
        self.lineno = lineno
        self.lexpos = lexpos

    # for AST nodes
    first = None
    second = None
    third = None

    def stmtd(self):
        pass            # raise SyntaxError?

    def nulld(self):
        raise SyntaxError("Syntax error (%r)." % self.value)

    def leftd(self, left):
        raise SyntaxError("Unknown operator (%r)." % self.value)

    """ outputs Py string representation of parse tree """
    def __repr__(self):
        if self.ttype == "ID" or self.ttype == "NUMBER":
            return "(%s %s)" % (self.ttype, self.value)
        out = [self.value, self.first, self.second, self.third]
        out = map(str, filter(None, out))
        return "(" + " ".join(out) + ")"


### SYMBOL FACTORY ###

def symbol(ttype, bp=0):
    try:
        NewSymbol = symbol_table[ttype]
    except KeyError:            # if key missing, create new key/class
        class NewSymbol(BaseSymbol):
            pass                # inherit from BaseSymbol class
        NewSymbol.__name__ = "symbol-" + ttype  # for debugging
        NewSymbol.ttype = ttype
        NewSymbol.leftbp = bp
        symbol_table[ttype] = NewSymbol
    else:
        NewSymbol.leftbp = max(bp, NewSymbol.leftbp)
    return NewSymbol

# Register simple tokens to symbol_table
symbol("(literal)").nulld = lambda self: self
symbol("ID").nulld = lambda self: self  # variables and function names
symbol("NUMBER").nulld = lambda self: self
symbol(")")
symbol("]")
"""
symbol("(lambda)")
symbol("if", 20)
symbol("else")
symbol("END")
"""


### BASIC PREFIX OPERATORS ###

def prefix(ttype, bp):
    def nulld(self):  # attach nodes to nulld method
        self.first = parse_expression(bp)  # bp = rbp
        self.second = None
        return self
    symbol(ttype).nulld = nulld  # attach nulld method to symbol, add to symbol_table

# Register operator symbols to symbol_table
prefix("!", 20)
prefix("-", 20)  # not sure if I'm using this?


### INFIX OPERATORS ###

# Helper method for led method: THERE IS A LEFT
def infix(ttype, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp)  # bp = rbp
        return self
    symbol(ttype, bp).leftd = leftd

# Register symbols to symbol_table
infix("<", 60);
infix("<=", 60);
infix(">", 60);
infix(">=", 60);
infix("==", 60);
infix("!=", 60);
infix("+", 110)
infix("-", 110)
infix("*", 120)
infix("/", 120)
infix("%", 120)


### INFIX_R & ASSIGNMENT OPERATORS ###

def infix_r(ttype, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp-1)  # still not certain why...
        return self
    symbol(ttype, bp).leftd = leftd

# Register symbols to symbol_table
infix_r("=", 10)        # why is this infix_r? did i put it here?
infix_r("+=", 10)
infix_r("-=", 10)
infix_r("||", 30)
infix_r("&&", 40)       # why more than || ?
infix_r("**", 140)      # why such a high bp?
infix_r("++", 120)      # postfix?
infix_r("--", 120)      # postfix?


# Helper method to handle LPAREN (first token in expression, NO LEFT)
def nulld(self):
    expression = parse_expression()
    advance(")")  # check current token for given value before fetching next
    return expression
symbol("(").nulld = nulld


# Helper method to handle reserved keywords & variables?
def statement(ttype, bp):
    def stmtd(self):
        self.first = parse_statement()
        self.second = None
        return self
    symbol(ttype).stmtd = stmtd

statement("def", 20)
statement("if", 20)
statement("elsif", 20)
statement("else", 20)
statement("break", 20)
statement("continue", 20)
statement("return", 20)
statement("for", 20)
statement("while", 20)
statement("print", 20)
# symbol("ID").nulld = lambda self: self  # variables and function names


# Helper method for next (check for errors before fetching next expression)
def advance(value=None):
    global token
    if value and token.value != value:
        raise SyntaxError("Expected %r" % value)
    try:
        token = next()
    except StopIteration:
        pass


# Function decorator to avoid repeating code
def method(NewSymbol):
    assert issubclass(NewSymbol, BaseSymbol)
    def bind(fn):
        setattr(NewSymbol, fn.__name__, fn)
    return bind


# Helper method to handle LBRACK (item lookup)
@method(symbol("["))
def leftd(self, left):
    self.first = left
    self.second = parse_expression()
    advance("]")
    return self


# Lists
@method(symbol("("))
def nulld(self):
    self.first = []
    if token.value != "]":
        while True:  # Add extra 'if' to allow optional trailing commas
            if token.value == "]":
                break
            self.first.append(parse_expression())
            if token.value != ",":
                break
            advance(",")
    advance("]")
    return self


# Dictionaries 
@method(symbol("{"))
def nulld(self):
    self.first = []
    if token.value != "}":
        while True:
            if token.value == "}":
                break
            self.first.append(parse_expression())
            advance(":")
            self.first.append(parse_expression())
            if token.value != ",":
                break
            advance(",")
    advance("}")
    return self
# TODO: function definitions, conditional blocks, loops


# Function calls: treat LPAREN as binary operator
@method(symbol("("))
def leftd(self, left):
    self.first = left  # this must be a non-reserved identifier/name
    self.second = []  # Right node will be list of function params
    if token.value != ")":
        while True:
            self.second.append(parse_expression())
            if token.value != ",":
                break
            advance(",")
    advance(")")
    return self


### STATEMENT SYMBOLS ###




# STATEMENTS
# def, if, then?, elsif, else, while, for, break, continue, return, get  
