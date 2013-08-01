import sys
import ply.lex as lex
import re

""" QUESTIONS, TODO """
# Also need !, . 
# Why no tuples? (explain: tuples vs. lists?)
# Constants
# TODO: SCOPE


### GLOBALS ###
token = None        # contains current token object
next                # holds next token obj in token_stream
symbol_table = {}   # store instantiated symbol classes
scope = None        # contains current scope object


### LEXER ###
# outputs LexToken(self.type, self.value, self.lineno, self.lexpos)

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
    'bool' : 'BOOL',
    'true' : 'TRUE',
    'false' : 'FALSE',
    'int' : 'INT',
    'float' : 'FLOAT',
    'struct' : 'STRUCT',
    'char' : 'CHAR', 
    'string' : 'STRING',
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
            'LESS', 'LESSEQ', 'GREATEQ', 'BOOLAND', 'BOOLOR', 'NEWLINE', 'COMMA', 'LBRACK',
            'RBRACK', 'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN', 'GLOBAL',
            'DEFCONST', 'INCLUDE', 'COMMENT', 'ID', 'HASH', 'STCOMM', 'ENDCOMM'
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
t_GLOBAL    = r'\$'  # prefix for global variables
t_HASH      = r'\#'  # can mean include, define (constant), or inline comment
t_STCOMM    = r'\/\*'  
t_ENDCOMM   = r'\*\/'

"""
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
    t.value = int(t.value)
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


# TODO: need to generate (end) token?
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
    global symbol_table
    print token_stream
    for token in token_stream:
        if token.type == "NUMBER" or token.type == "ID":
            symbol = symbol_table[token.type]
        else:
            symbol = symbol_table[token.value]
        s = symbol(token.type, token.value, token.lineno, token.lexpos)
        if not symbol:
            raise SyntaxError("Unknown operator (%r)" % token.type)
        print "TOKENIZED S: ", s.ttype, s.value
        yield s     # returns a generator


def parse(filename=None):
    global token, next 
    if not filename:
        filename = raw_input("> ")
    program = open(filename).read()
    token_stream = generate_tokens(program)
    next = tokenize(token_stream).next  # what exactly is .next ?
    token = next()
    return parse_expression()  


"""
### SCOPE ###
class Rule(object):
    def nulld(self, ....):
        raise NotImplementedError

class Scope:
    def __init__(ttype):
        self.ttype = ttype  # new variable
        self.parent = None

    def define(self):
        if 
        ttype.reserved = False
        ttype.nulld = itself  # what
        ttype.leftd = null
        ttype.leftbp = 0
        ttype.scope = scope
    
    def find(self, token_name):
        while True:
    
    def pop(self):  # close scope, return focus to parent
        global scope
        s = scope
        scope = s.parent
        # return?

    def reserve():
        global token
        t = token
        if t.reserved or t.type in reserved.values():
            return
        

def new_scope():
    # if token.id == "ID" and token is not reserved keyword
    global scope, token
    s = scope
    scope = Scope(token.value) 
"""

"""
BINDING POWERS (from Crockford)
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

### ADVANCE ###
# Check for errors before fetching next expression

### TODO: Make token ttype/value consistent across program... when you have time
def advance(value=None):
    global token
    if value:
        if token.ttype == "ID": 
            if token.ttype != value:
                raise SyntaxError("Expected %r" % value)
        elif token.value != value:
            raise SyntaxError("Expected %r" % value)
    try:
        token = next()
    except StopIteration:
        pass


### EXPRESSION PARSER ###

def parse_expression(rbp=0):            # default binding power = 0 
    global token
    t = token
    advance()
    if t.stmtd():
        left = t.stmtd()
        parse_statement()
        print "Parsed a statement!"
    else:
        left = t.nulld()
        if token.leftbp != None:            # this 'if' from Gulnara's code
            while rbp < token.leftbp:       # keep going till rbp > current token's bp
                t = token
                token = next()
                left = t.leftd(left)
                print "Parsed an expression!"
    return left


### STATEMENT PARSER ###

# Parse a single statement at a time
def parse_statement():
    global token
    t = token
    if t.stmtd:
        advance()
        # scope.reserve(t) - for when scope is implemented
        return t.stmtd()
    else:
        expression = parse_expression()
        advance("\n")
        # TODO: if not an assignment and not "(", throw error
        return expression


# Continue parsing all statements in a row, return list
def parse_stmts():
    stmtlist = []
    if token.value != "}":
        while True:
            if token.value == "}":  # or token.value == "(end)":
                break
            s = parse_statement()
            if s:
                stmtlist.append(s)
    if len(stmtlist) == 0:
        stmtlist = None
    elif len(stmtlist) == 1:
        stmtlist = stmtlist[0]
    return stmtlist


### TOKEN CLASSES ###

# base class for operators
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

    def stmtd(self):
        pass

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
# creates new symbol classes as needed #

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
symbol("int").nulld = lambda self: self
symbol("bool").nulld = lambda self: self
symbol("float").nulld = lambda self: self
symbol(")")
symbol(",")
symbol("]")
symbol("}")
symbol("\n")
symbol("[", 150)
symbol("(", 150)
symbol(".", 150)
# symbol("else")


### BASIC PREFIX OPERATORS ###

def prefix(ttype, bp):
    def nulld(self):  # attach nodes to nulld method
        self.first = parse_expression(bp)  # bp = rbp
        self.second = None
        return self
    symbol(ttype).nulld = nulld  # attach nulld method to symbol, add to symbol_table

# Register operator symbols to symbol_table
# prefix("!", 20)
prefix("-", 130)  # not sure if I'm using this?


### INFIX OPERATORS ###

# Helper method for leftd method: THERE IS A LEFT
def infix(ttype, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp)  # bp = rbp
        return self
    symbol(ttype, bp).leftd = leftd

# Register symbols to symbol_table
infix("<", 60)
infix("<=", 60)
infix(">", 60)
infix(">=", 60)
infix("==", 60)
infix("!=", 60)
infix("+", 110)
infix("-", 110)
infix("*", 120)
infix("/", 120)
infix("%", 120)
infix("=", 10) 

### INFIX_R & ASSIGNMENT OPERATORS ###

def infix_r(ttype, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp-1)  # still not certain why...
        return self
    symbol(ttype, bp).leftd = leftd

# Register symbols to symbol_table
# Assignment operators should maybe have their own helper method
# infix_r("=", 10)        # why is this infix_r? did i put it here?
infix_r("+=", 10)
infix_r("-=", 10)
infix_r("||", 30)
infix_r("&&", 40)       # why more than || ?
infix_r("**", 140)      # why such a high bp?
# infix_r("++", 120)      # postfix?
# infix_r("--", 120)      # postfix?


# Helper method to handle reserved keywords & variables?
def statement(ttype, bp):
    def stmtd(self):
        self.first = parse_stmts() 
        self.second = None  # ?? 
        return self
    symbol(ttype).stmtd = stmtd

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
# For lists - how to distinguish?
@method(symbol("["))
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


########## How to treat function declarations? Is prefix "def" the operative
########## symbol, and all other tokens, (, [arguments], ), {, and } all
########## parsed as part of the same expression/statement?

# Function declarations with "def"
@method(symbol("def"))
def nulld(self):
    advance("ID")
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
    self.first = arguments
    advance(")")
    advance("{")
    if token.value == "\n":
        advance() 
    expressions = []
    if token.value != "}":
        while True:
            if token.value == "}":
                break
            expressions.append(parse_expression())
            if token.value == "\n":
                advance() 
    self.second = expressions
    advance("\n")
    advance("}")
    return self


"""
def main():
    parse(small.kh)

if __name__ == "__main__":
    main()
"""
