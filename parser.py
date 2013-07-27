import sys
import ply.lex as lex
import re


""" GLOBALS """
token = None  # contains current token object
next = None
symbol_table = {}  # store symbol classes
token_stream = None  # returned from tokenize(); does this need to be global?
# scope  # contains current scope object


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

def tokenize(program):
    lexer = lex.lex()  # build lexer
    lexer.input(program)

    while True:
        tokens = lexer.token()
        if not tokens:
            break  # no more input
        token_stream = list(tokens)
        print token_stream
    return token_stream

def parse():
    global token, next
    program = raw_input("> ")
    token_stream = tokenize(program)
    next = token_stream.next
    token = next()
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
def parse_expression(rbp=0):  # default binding power = 0 
    global token 
    t = token  # contains copy of current token
    token = next()  # fetch next token in list, store as token
    left = t.nulld()  # this will be first token in expr - no left
    while rbp < token.lbp:  # keep going till rbp > current token's bp?
        t = token
        token = next()
        left = t.leftd(left)
    return left


""" STATEMENT PARSER """
# Do I need this?
def parse_statement(rbp=0):
    pass


""" TOKEN CLASSES """

""" Why is Literal a separate class from Token? """
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
class Symbol(object):
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
        raise SyntaxError("Syntax error (%r)." % self.id)

    def leftd(self, left):
        raise SyntaxError("Unknown operator (%r)." % self.id)

    """ for error-checking: outputs Py string representation of an object """
    def __repr__(self):
        if self.id == "(name)" or self.id == "(literal)":
            return "(%s %s)" % (self.id[1:-1], self.value)  # why slice?
        out = [self.id, self.first, self.second, self.third]
        out = map(str, filter(None, out))  # what is happeninggg
        return "(" + " ".join(out) + ")"

""" TOKEN FACTORY!
Take token id & optional bp, generate new class if needed. If symbol is
already in symbol_table, update the bp. """

def symbol(id, bp=0):
    try:
        symbol = symbol_table[id]
    except KeyError:  # if key missing, create new key/class
        class symbol_class(Symbol):
            pass  # inherit from BaseSymbol class
        symbol_class.__name__ = "symbol-"  # for debugging (?)
        symbol_class.id = id
        symbol_class.lbp = bp
        symbol_table[id] = symbol_class
    else:
        # if not the above two cases, where is symbol_class defined?
        symbol_class.lbp = max(bp, symbol_class.lbp)  # wtf does this do
    return symbol_class

# Register simple tokens to symbol_table
symbol("(literal)").nulld = lambda self: self  # wtf is happening
symbol("(name)").nulld = lambda self: self  # is this for reserved keywords?
symbol("(end)")
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
def prefix(id, bp):
    def nulld(self):  # attach nodes to nulld method
        self.first = parse_expression(bp)  # bp = rbp
        self.second = None
        return self
    symbol(id).nulld = nulld  # attach nulld method to symbol, add to symbol_table

# Register operator symbols to symbol_table
prefix("!", 20)
prefix("-", 20)  # not sure if I'm using this?
"""

""" INFIX OPERATORS """

def infix(id, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp)  # bp = rbp
        return self
    symbol(id, bp).leftd = leftd

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
def infix_r(id, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp-1)  # still not certain why...
        return self
    symbol(id, bp).leftd = leftd

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
