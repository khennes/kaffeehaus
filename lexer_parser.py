import sys
import ply.lex as lex
import re

""" QUESTIONS, TODO """
# Also need !, . 
# Why no tuples? (explain: tuples vs. lists?)
# TODO: Implement lexical scoping
# TODO: Constants, global vars, array declaration, error-checking for type declarations
# TODO: Structs? (Worth it?)
# TODO: Make token ttype/value consistent across program... when you have time


### GLOBALS ###
token = None        # contains current token object
symbol_table = {}   # store instantiated symbol classes
token_stack = []    # contains remaining tokens


### LEXER ###

tokens_list = (
    'NUMBER',
    'GLOBAL',
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
            'ISEQ', 'ISNOTEQ', 'GREATER', 'LESS', 'LESSEQ', 'GREATEQ',
            'BOOLAND', 'BOOLOR', 'NEWLINE', 'COMMA', 'LBRACK', 'RBRACK',
            'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN', 'GLOBAL', 'DEFCONST',
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
Function documentation string: takes single argument (instance of LexToken)
LexToken attributes:
* t.type: token type (as str); defaults to name following t_ prefix 
* t.value: lexeme (actual text matched)
* t.lineno: current line number
* t.lexpos: position of token relative to beginning of input text
"""

# Ignore tabs, spaces 
t_ignore = ' \t\v'

# t_NUMBER matches numbers and converts string -> Python integer
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

# Strings -- ok to have two function documentation strings?
def t_STRING(t):
    r'"(.*)"'
    r"'(.*)'"
    return t

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
        if token.type == "NUMBER" or token.type == "ID" or token.type == "STRING":
            symbol = symbol_table[token.type]
        else:
            symbol = symbol_table[token.value]
        s = symbol(token.type, token.value, token.lineno, token.lexpos)
        token_stack.append(s)
        if not symbol:
            raise SyntaxError("Unknown operator (%r)" % token.type)
    return token_stack


def parse(filename=None):
    global token, token_stack 
    expression_list = []
    if not filename:
        filename = raw_input("> ")
    program = open(filename).read()
    token_stream = generate_tokens(program)
    token_stack = tokenize(token_stream)
    token = token_stack.pop(0)

    while len(token_stack) > 0:
        expression = parse_expression()  
        expression_list.append(expression)

    if len(token_stack) == 0:
        print "ABSTRACT SYNTAX TREE:\n ", expression_list
        return expression_list  # this returns the AST


# def parse_program(
    

### ADVANCE ###

# Check for errors before fetching next token
def advance(value=None):
    global token, token_stack
    if value:
        if token.ttype == "ID" or token.ttype == "STRING":
            if token.ttype != value:
                raise SyntaxError("Expected %r" % value)
        elif token.value != value:
            raise SyntaxError("Expected %r" % value)
    
    if len(token_stack) > 0:
        next_token = token_stack.pop(0)
        token = next_token
    else:
        return


### EXPRESSION PARSER ###

def parse_expression(rbp=0):
    global token
    t = token
    print "T ", t
    advance()
    if hasattr(t, "stmtd"):
        left = t.stmtd()
        print "STATEMENT: ", left
    elif hasattr(t, "nulld"):
        left = t.nulld()
        while rbp < token.leftbp:  # keep going till rbp > current token's bp
            t = token
            advance()
            left = t.leftd(left)
            print "EXPRESSION: ", left
    else:
        advance()
    return left


### STATEMENT PARSER ###

# Parse a single statement
def parse_statement():
    global token
    t = token
    return t.stmtd()


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

    def nulld(self):
        raise SyntaxError("Syntax error: %r, line %r." % (self.value, self.lineno))

    def leftd(self, left):
        raise SyntaxError("Unknown operator: %r, line %r." % (self.value, self.lineno))

    """ outputs Py string representation of parse tree """
    def __repr__(self):
        if self.ttype == "ID" or self.ttype == "ID" or self.ttype == "NUMBER":
            return "(%s %s)" % (self.ttype, self.value)
        out = [self.value, self.first, self.second, self.third]
        out = map(str, filter(None, out))
        return "(" + " ".join(out) + ")"


### SYMBOL FACTORY ###
# Creates new symbol classes as needed
# Binding power: how tightly expression binds to tokens on right side
# Ex. "1 + 2 * 4", * has higher bp, so it 'wins' 2.

def symbol(ttype, bp=0):
    try:
        NewSymbol = symbol_table[ttype]
    except KeyError:            # if key missing, create new key/class
        class NewSymbol(BaseSymbol):  # inherit from BaseSymbol class
            pass                
        NewSymbol.__name__ = "symbol-" + ttype  # for debugging
        NewSymbol.ttype = ttype
        NewSymbol.leftbp = bp
        symbol_table[ttype] = NewSymbol
    else:
        NewSymbol.leftbp = max(bp, NewSymbol.leftbp)
    return NewSymbol

# Register simple tokens to symbol_table
symbol("ID").nulld = lambda self: self  # variables and function names
symbol("NUMBER").nulld = lambda self: self
symbol("STRING").nulld = lambda self: self
symbol("true").nulld = lambda self: self
symbol("false").nulld = lambda self: self
symbol(")")
symbol(",")
symbol("]")
symbol("}")
symbol("\n").nulld = lambda self: self
symbol("[", 150)
symbol("(", 150)
# symbol("$", 10)  # treat similar to variable? should bind tightly to right
symbol("int", 0).nulld = lambda self: self
symbol("bool", 0).nulld = lambda self: self
symbol("float", 0).nulld = lambda self: self
symbol("char*", 0).nulld = lambda self: self


### BASIC PREFIX OPERATORS ###

# Helper method for nulld method: THERE IS NO LEFT
def prefix(ttype, bp):
    def nulld(self):                        # attach nodes to nulld method
        self.first = parse_expression(bp)   # bp = rbp
        self.second = None
        return self
    symbol(ttype).nulld = nulld             # attach nulld method to symbol,
                                            # add to symbol_table

# Register operator symbols to symbol_table
# prefix("!", 20)
prefix("-", 130)
prefix("$", 130)  # prefix for global variables


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


### INFIX_R & ASSIGNMENT OPERATORS ###

def infix_r(ttype, bp):
    def leftd(self, left):
        self.first = left
        self.second = parse_expression(bp-1)  # still not certain why...
        return self
    symbol(ttype, bp).leftd = leftd

# Register symbols to symbol_table
# Assignment operators should maybe have their own helper method
infix_r("=", 10)
infix_r("+=", 10)
infix_r("-=", 10)
infix_r("||", 30)
infix_r("&&", 40)
infix_r("**", 140)
# infix_r("++", 120)      # postfix?
# infix_r("--", 120)      # postfix?


# Function decorator to avoid repeating code
# FIND OUT WHAT THIS DOES
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
        while True:  # TODO: Add extra 'if' to allow optional trailing commas
            if token.value == "]":
                break
            self.first.append(parse_expression())
            if token.value != ",":
                break
            advance(",")
    advance("]")
    if token.ttype == "ID":
        self.first.append(token)
    if token.value == "\n":
        advance()
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


# Helper method to handle statements
def statement(ttype, bp):
    def stmtd(self):
        self.first = parse_expression() 
        self.second = None
        return self
    symbol(ttype).stmtd = stmtd

statement("break", 0)
statement("get", 0)
statement("continue", 0)
statement("return", 20)
statement("for", 20)
statement("print", 20)
statement("elsif", 20)
statement("else", 20)

# Variable declaration
@method(symbol("var"))
def stmtd(self):
    if token.ttype != "ID":
        raise SyntaxError("Expected a new variable name.")
    self.first = parse_expression()
    advance()
    if token.value == "=":
        advance()
        self.second = parse_expression()
    if token.value == "\n":
        advance()
    return self        


# While-loop statements
@method(symbol("while"))
def stmtd(self):
    advance("(")
    self.first = parse_expression()
    advance(")")
    advance("{")
    if token.value == "\n":
        advance()
    expressions = []
    if token.value != "}":
        while True:
            expressions.append(parse_expression())
            if token.value == "\n":
                advance()
            if token.value == "}":
                break
    self.second = expressions
    advance("}")
    return self
    

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
    advance("{")
    if token.value != "}":
        while True:
            self.second.append(parse_expression())
            if token.value == "\n":
                advance()
            if token.value == "}":
                break
    if token.value == "\n":
        advance()
    advance("}")
    return self

            
# If/elsif/else conditional statements 
@method(symbol("if"))
def stmtd(self):
    advance("(")
    self.first = parse_expression()  # condition
    advance(")")
    advance("{")
    if token == "\n":
        advance()
    expressions = []
    if token.value != "}":
        while True:
            expressions.append(parse_expression())
            if token == "\n":
                advance()
            if token.value == "}":
                break
    self.second = expressions
    advance("}")
    if token == "elsif":
        advance("{")
        expressions = []
        if token.value != "}":
            while True:
                expressions.append(parse_expression())
                if token == "\n":
                    advance()
                if token.value == "}":
                    break
        advance("}")
    if token == "else":
        print "ELSE: ", token
        advance("{")
        if token == "\n":
            advance()
        if token.value != "}":
            while True:
                expressions.append(parse_expression())
                if token == "\n":
                    advance()
                if token.value == "}":
                    break
        advance("}")
    return self
 

# Function declarations with "def"
@method(symbol("def"))
def stmtd(self):
    print "FUNCTION DEF TOKEN: ", token
    if token.ttype != "ID":
        raise SyntaxError("Expected function name.")
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
    advance("\n")
    expressions = []
    if token.value != "}":
        while True:
            expression = parse_expression()
            if token.value == "\n":
                advance()
            expressions.append(expression)
            if token.value == "}":
                break 
    self.second = expressions
    advance("}")
    return self


def main():
    parse()

if __name__ == "__main__":
    main()
