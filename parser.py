import sys
import ply.lex as lex
import re


""" GLOBALS """
token = None        # contains current token object
next                # holds next token obj in token_stream?
symbol_table = {}   # store symbol classes
symbols_found = {}  # for debugging - which tokens have been parsed
token_stream = None # does this need to be global?
# scope             # contains current scope object


""" LEXER """

# Also need !, ., 
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
    t.type = reserved.get(t.value, 'ID')  
    return t

# Error handling rule
def t_error(t):
    print "Invalid character: '%s'" % t.value[0]
    t.lexer.skip(1)


""" CALL LEXING & PARSING FUNCTIONS """

def generate_tokens(program):    
    print program
    token_stream = []
    lexer = lex.lex()           # build lexer
    lexer.input(program)

    while True:
        tokens = lexer.token()
        print tokens
        if not tokens:
            break                   # no more input
        token_stream.append(tokens)  # creating a list rather than generator
    return token_stream             

def tokenize(token_stream):
    global symbol_table
    print token_stream
    for token in token_stream:      # token = LexToken object
        print token
        if token.type == "NUMBER":  # renamed 'ttype' to 'type' (Ply docs)
            print "YES A NUMBER"
            symbol = symbol_table[token.type]  
            s = symbol(token.type, token.value, token.lineno, token.lexpos)
            # s.value = value  
        else:                       # if name or operator
            print "NO NOT A NUMBER", token.type, token.value
            symbol = symbol_table.get(token.value)
            if symbol:
                s = symbol(token.type, token.value, token.lineno, token.lexpos)
            elif token.type == "ID":
                print "SYMBOL IS A NAME/ID", token.type
                symbol = symbol_table[token.type]
                s = symbol(token.type, token.value, token.lineno, token.lexpos)
                # s.value = value
            else:
                raise SyntaxError("Unknown operator (%r)" % id)
        yield s     # yields a generator


def parse():
    global token, next 
    filename = raw_input("> ")
    program = open(filename).read()
    token_stream = generate_tokens(program)
    next = tokenize(token_stream).next
    token = next()  # What do these two lines do, exactly?
    return parse_expression()  


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

""" EXPRESSION PARSER """
def parse_expression(rbp=0):    # default binding power = 0 
    global token
    t = token
    try:
        token = next()              # to handle StopIteration exception 
    except StopIteration:           # a more elegant way to do this?
        pass
    left = t.nulld()
    while rbp < token.lbp:          # keep going till rbp > current token's bp
        t = token
        token = next()
        left = t.leftd(left)
    print "LEFT: ", left
    return left


""" STATEMENT PARSER """
# Do I need this?
def parse_statement(rbp=0):
    pass


""" TOKEN CLASSES """

""" Why is Literal a separate class from BaseSymbol? """
# for numbers and constants
class Literal(object):
    def __init__(self, ttype, value, lineno, lexpos):
        self.ttype = ttype
        self.value = value
        self.lineno = lineno
        self.lexpos = lexpos
    def nulld(self):
        return self  # token literals return themselves

    def __repr__(self):
        return "(literal %s) % self.value"  # generate parse tree

# base class for operators
class BaseSymbol(object):
    def __init__(self, ttype, value, lineno, lexpos):
        self.ttype = ttype  # token type
        self.value = value  # used by literals and names
        self.lineno = lineno
        self.lexpos = lexpos

    # for AST nodes
    first = None
    second = None
    third = None

    def nulld(self):
        raise SyntaxError("Syntax error (%r)." % self.ttype)

    def leftd(self, left):
        raise SyntaxError("Unknown operator (%r)." % self.ttype)

    """ for error-checking: outputs Py string representation of an object """
    def __repr__(self):
        if self.ttype == "ID" or self.ttype == "NUMBER":
            return "(%s %s)" % (self.ttype, self.value)  # why slice?
        out = [self.ttype, self.first, self.second, self.third]
        out = map(str, filter(None, out))  # what is happeninggg
        return "(" + " ".join(out) + ")"

""" TOKEN FACTORY!
Take token id & optional bp, generate new class if needed. If symbol is
already in symbol_table, update the bp. """

def symbol(ttype, bp=0):
    try:
        symbol = symbol_table[ttype]
    except KeyError:            # if key missing, create new key/class
        class symbol_class(BaseSymbol):
            pass                # inherit from BaseSymbol class
        symbol_class.__name__ = "symbol-"  # for debugging (?)
        symbol_class.ttype = ttype
        symbol_class.lbp = bp
        symbol_table[ttype] = symbol_class
    else:
        # if not the above two cases, where is symbol_class defined?
        symbol_class.lbp = max(bp, symbol_class.lbp)  # wtf does this do
    return symbol_class

# Register simple tokens to symbol_table
symbol("(literal)").nulld = lambda self: self  # wtf is happening
symbol("(name)").nulld = lambda self: self  # is this for reserved keywords?
symbol("NUMBER").nulld = lambda self: self
symbol("END")
""" COME BACK TO THESE
symbol("(lambda)")  # do I need this?
symbol("if", 20)
symbol(".", 150)
symbol("[", 150)
symbol("(", 150)
symbol("]")  # no bp?
symbol("}")
symbol(")")
symbol(",")
# symbol(":")
symbol("else")
"""

""" PREFIX OPERATORS """
"""
def prefix(ttype, bp):
    def nulld(self):  # attach nodes to nulld method
        self.first = parse_expression(bp)  # bp = rbp
        self.second = None
        return self
    symbol(ttype).nulld = nulld  # attach nulld method to symbol, add to symbol_table

# Register operator symbols to symbol_table
prefix("!", 20)
prefix("-", 20)  # not sure if I'm using this?
"""

""" INFIX OPERATORS """

def infix(ttype, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp)  # bp = rbp
        return self
    symbol(ttype, bp).leftd = leftd

# Register operator symbols to symbol_table
"""
infix("<", 60);
infix("<=", 60);
infix(">", 60);
infix(">=", 60);
infix("==", 60);
infix("!=", 60);
"""
infix("+", 110);
infix("-", 110);
infix("*", 120);
infix("/", 120);
# infix("%", 120);


""" INFIX_R OPERATORS """
"""
def infix_r(ttype, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp-1)  # still not certain why...
        return self
    symbol(ttype, bp).leftd = leftd

# Register operator symbols to symbol_table
infix_r("||", 30)
infix_r("&&", 40)  # why more than || ?
infix_r("**", 140)  # why such a high bp?
"""


"""
TODO: SCOPE
# using Crockford's JS implementation
# global scope variable declared at top
# in Python, default scope is local rather than $global

class Rule(object):
    def nulld(self, ....):
        raise NotImplementedError
    
class Scope(Rule):
    reserved = False
    nulld    = itself  # what
    leftd    = null
    leftbp   = 0
    scope    = scope
    def __init__(self, token_name):  # ??
        #jdunck - assign to self here, not mutating token_name.
        # Class defaults?
        return token_name
    
    def define(token_name):
        pass
    
    def find(token_name):
        pass
    
    def pop():  # close scope, return focus to parent
        # scope = s.parent
        pass

    def reserve(token_name):
        pass

def new_scope():
    s = scope
    scope = 
"""
