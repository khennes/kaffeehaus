# Following http://effbot.org/zone/simple-top-down-parsing.htm to get started.

# import lexer
import re

def parse_expression(rbp=0):
    global token  # contains current token
    t = token
    token = next()  # built-in Python function, fetches next token in list
    left = t.nud()
    while rbp < token.lbp:
        t = token
        token = next()
        left = t.led(left)  # used to pass some value that represents the 'left' \
                            # part of the expression thru to the led method 
    return left

# Each token has two associated functions:
# * nud: used when token appears at beginning of a lang construct
# * led: when token appears inside the construct (to left of rest of construct)
# * lbp value: binding power; controls operator precedence (higher == tighter)
# Top-down parsing creates new object for each token: easy to build parse trees

class literal_token:
    def __init__(self, value):
        self.value = value
    def nud(self):
        return self
    def __repr__(self):
        return "(literal %s)" % self.value

# Create base class for token types, according to Crockford's JS implementation
# of Pratt. Class will contain a factory function to generate new classes on
# the fly.

class symbol_base(object):
    id = None  # node/token type name
    value = None  # used by literals & names
    first = second = third = None  # used by tree nodes

    def nud(self):
        raise SyntaxError("Syntax error (%r)." % self.id)

    def led(self, left):
        raise SyntaxError("Unknown operator (%r)." % self.id)

    def __repr__(self):
        if self.id == "(name)" or self.id == "(literal)":
            return "(%s %s)" % (self.id[1:-1], self.value)
        out = [self.id, self.first, self.second, self.third]
        out = map(str, filter(None, out))
        return "(" + " ".join(out) + ")"

# Token type factory

symbol_table = {}

def symbol(id, bp=0):
    try:
        s = symbol_table[id]
    except KeyError:
        class s(symbol_base):
            pass
        s.__name__ = "symbol-" + id  # for debugging
        s.id = id
        s.lbp = bp
        symbol_table[id] = s
    else:
        s.lbp = max(bp, s.lbp)
    return s

# symbol() takes a token identifier, plus optional bp, & creates a new class
# if needed. If symbol() is called for symbol that's already registered, it
# updates the bp.

# Populate token class registry with symbols we'll use:
symbol("(literal)").nud = lambda self: self  # whaaat is going on
# symbol("+", 10); symbol("-", 10)
# symbol("*", 20); symbol("/", 20)
# symbol("**", 30)
symbol("(end)")

def infix(id, bp):
    def led(self, left):
        self.first = left
        self.second = parse_expression(bp)
        return self
    symbol(id, bp).led = led

infix("+", 10); infix("-", 10)
infix("*", 20); infix("/", 20)

def prefix(id, bp):
    def nud(self):
        self.first = parse_expression(bp)
        self.second = None
        return self
    symbol(id).nud = nud

prefix("+", 100); prefix("-", 100)

def infix_r(id, bp):
    def led(self, left):
        self.first = left
        self.second = parse_expression(bp-1)
        return self
    symbol(id, bp).led = led

infix_r("**", 30)

"""
# Next, parser checks if binding power of next token is at least as large as the
# given binding power (the "rbp" arg aka 'right binding power). If yes, it calls
# the "led" method for that token. Here, the rpb = 0, and next token is an
# operator:

class operator_add_token:
    lbp = 10
    def nud(self):  # Skips extra node for unary operators
        return parse_expression(100)
    def led(self, left):
        self.first = left
        self.second = parse_expression(10)
        return self
    def __repr__(self):  # Print tree
        return "(add %s %s)" % (self.first, self.second)

class operator_sub_token:
    lbp = 10
    def nud(self):  # Skips extra node for unary operators
        return parse_expression(10)
    def led(self, left):
        self.first = left
        self.second = parse_expression(10)
        return self
    def __repr__(self):  # Print tree
        return "(subtract %s %s)" % (self.first, self.second)

class operator_mul_token:
    lbp = 20
    def led(self, left):
        self.first = left
        self.second = parse_expression(20)
        return self
    def __repr__(self):
        return "(mul %s %s)" % (self.first, self.second)

class operator_pow_token:
    lbp = 30
    def led(self, left):
        self.first = left
        # treats subsequent exponentian ops as subexpressions to current one
        # ie, 2**3**4 == 2**(3**4)
        self.second = parse_expression(30-1)
        return self
    def __repr__(self):
        return "(pow %s %s)" % (self.first, self.second)

class operator_div_token:
    lbp = 20
    def led(self, left):
        self.first = left
        self.second = parse_expression(20)
    def __repr__(self):
        return "(div %s %s)" % (self.first, self.second)

# The operator has binding power of 10. The led method calls parse_expression()
# again, passing in the rbp == operator's own binding power. This causes
# parse_expression() to treat everything with a higher power as a subexpression
# & return its result. The method then adds the left value to parse_expression()'s
# return value & returns the result. The end of the program is indicated by a
# special marker token with bp = 0.

class end_token:
    lbp = 0
"""

token_pat = re.compile("\s*(?:(\d+)|(\*\*|.))")

def tokenize(program):
    for number, operator in token_pat.findall(program):
        if number:
            symbol = symbol_table["(literal)"]
            s = symbol()  # invoke factory function
            s.value = number
            yield s
        else:
            symbol = symbol_table.get(operator)
            if not symbol:
                raise SyntaxError("Unknown operator")
            yield symbol()
    symbol = symbol_table["(end)"]
    yield symbol()

def parse(program):
    global token, next
    next = tokenize(program).next
    token = next()
    return parse_expression()

# Not counting calls to tokenizer, this parser algorithm will make 4 calls
# total to parse this expression: one for each token, plus one for the 
# recursive call to parse_expression() in the "led" method.


