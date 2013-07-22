import sys
import ply.lex as lex

# List of token names. This is always required.
tokens = (
    'NUMBER',
    'GLOBAL',
    'DEFCONST',
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
    'LBRACK',
    'RBRACK',
    'LBRACE',
    'RBRACE',
    'LPAREN',
    'RPAREN'
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
tokens = [ 'NUMBER', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'POWER', 'MODULO', 'INCREMENT', 'DECREMENT', 'EQUALS', 'ISEQ', 'ISNOTEQ', 'GREATER', 'LESS', 'LESSEQ', 'GREATEQ', 'BOOLAND', 'BOOLOR', 'LBRACK', 'RBRACK', 'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN', 'GLOBAL', 'DEFCONST', 'INCLUDE', 'COMMENT', 'ID' ] + list(reserved.values())


# Regular expression rules for simple tokens
# Python raw strings are used for convenience
t_PLUS	    = r'\+'
t_MINUS	    = r'-'
t_TIMES	    = r'\*'
t_DIVIDE    = r'/'
t_POWER	    = r'\*\*'
t_MODULO    = r'%'
t_INCREMENT = r'\+\+'
t_DECREMENT = r'--'
t_EQUALS    = r'='
t_ISEQ	    = r'\=\='
t_ISNOTEQ   = r'!='
t_GREATER   = r'>'
t_LESS	    = r'<'
t_LESSEQ    = r'<='
t_GREATEQ   = r'>='
t_BOOLAND   = r'&&'
t_BOOLOR    = r'\|\|'
t_LBRACK    = r'\['
t_RBRACK    = r'\]'
t_LBRACE    = r'{'
t_RBRACE    = r'}'
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_GLOBAL    = r'\$'
# DEFCONST, INCLUDE, ID  # can mean include, define (constant), or inline comment
# t_STCOMM  = r'\/\*\*\*'  # How to handle multiline comments?
# t_ENDCOMM = r'\*\*\*\/'

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

# A string containing ignored characters (spaces, tabs)
t_ignore = ' \t'

def t_NUMBER(t):
    r'\d+' 
    t.value = int(t.value)
    return t

# Track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Recognize, discard comments
def t_COMMENT(t):
    r'\#.*'  # Should this be r'\#*'? How to handle multi-line comments?
    pass
    # no return value, token discarded

# Store IDs in a dictionary?
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')  # Check for reserved words
    return t

# Error handling rule
def t_error(t):
    print "Invalid character: '%s'" % t.value[0]
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Feed input program to lexer
args = sys.argv
script, filename = args
f = open(filename).read()
lexer.input(f)

# Tokenize
while True:
    tokens = lexer.token()
    if not tokens: 
	break  # No more input
    print tokens

