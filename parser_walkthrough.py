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
# * nud: used when token appears at beg of lang construct (aka there is NO LEFT)
# * led: when token appears inside the construct (aka there is a left)
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

# Take a token identifier, plus optional bp, & create a new class if needed. If
# symbol is already registered, update the bp.

def symbol(id, bp=0):
    try:
        s = symbol_table[id]
    except KeyError:  # if not in symbol_table dict, create new key/class
        class s(symbol_base):
            pass  # Inherit from symbol_base class
        s.__name__ = "symbol-" + id  # for debugging
        s.id = id
        s.lbp = bp
        symbol_table[id] = s
    else:  # whaat does this do?
        s.lbp = max(bp, s.lbp)
    return s

# Populate token class registry with symbols we'll use:
symbol("(literal)").nud = lambda self: self  # whaaat is going on
symbol("(name)").nud = lambda self: self
symbol("(end)")
symbol("lambda", 20)
symbol("if", 20)  # ternary form (if, elsif, else?)
symbol(".", 150); symbol("[", 150); symbol("(", 150)  # What about ] and )?
symbol("]")
symbol(")")
symbol("else")

# Helper function for led method: THERE IS A LEFT
def infix(id, bp):
    def led(self, left):
        self.first = left
        self.second = parse_expression(bp)
        return self
    symbol(id, bp).led = led

infix("in", 60); infix("not", 60)  # in, not in
infix("is", 60)  # is, is not
infix("<", 60); infix("<=", 60)
infix(">", 60); infix(">=", 60)
infix("<>", 60); infix("!=", 60); infix("==", 60)

infix("|", 70); infix("^", 80); infix("&", 90)

infix("<<", 100); infix(">>", 100)

infix("+", 110); infix("-", 110)
infix("*", 120); infix("/", 120); infix("//", 120)
infix("%", 120)


# Helper function for nud method: Null denotation (there is no left)
# These will be the first tokens in an expression
def prefix(id, bp):
    def nud(self):
        self.first = parse_expression(bp)
        self.second = None
        return self
    symbol(id).nud = nud

prefix("-", 130); prefix("+", 130); prefix("~", 130)


# Helper function for operators with right associativity:
def infix_r(id, bp):
    def led(self, left):
        self.first = left
        self.second = parse_expression(bp-1)
        return self
    symbol(id, bp).led = led

infix_r("or", 30); infix_r("and", 40); prefix("not", 50)  # ||, &&, !=
infix_r("**", 140)

# Parenthesized expressions
def nud(self):
    expr = parse_expression()
    advance(")")  # checks current token for a given value before fetching next
    return expr
symbol("(").nud = nud

def advance(id=None):
    global token
    if id and token.id != id:
        raise SyntaxError("Expected %r" % id)
    token = next()

def method(s):  # FIGURE OUT WHAT TO DO WITH THIS, BELOW
    assert issubclass(s, symbol_base)
    def bind(fn):
        setattr(s, fn.__name__, fn)
    return bind

# Ternary operators
def led(self, left):
    self.first = left
    self.second = parse_expression()
    advance("else")
    self.third = parse_expression()
    return self
symbol("if").led = led

# Attribute lookup
def led(self, left):
    if token.id != "(name)":
        SyntaxError("Expected an attribute name.")
    self.first = left
    self.second = token
    advance()
    return self
symbol(".").led = led

# Item lookup
def led(self, left):
    self.first = left
    self.second = parse_expression()
    advance("]")
    return self
symbol("[").led = led


# token_pat = re.compile("\s*(?:(\d+)|(\*\*|.))")  # now using Tokenize mod

# Here we split the tokenizer into two parts: one returns token stream, the
# other turns those into token instances. The second part also checks operators
# and names against the symbol_table (to handle keyword operators) and uses a
# pseudo-symbol ("(name)") for all other names.

def tokenize_python(program):
    import tokenize
    from cStringIO import StringIO
    type_map = {
        tokenize.NUMBER: "(literal)",
        tokenize.STRING: "(literal)",
        tokenize.OP: "(operator)",
        tokenize.NAME: "(name)",
    }
    for t in tokenize.generate_tokens(StringIO(program).next):
        try:
            yield type_map[t[0]], t[1]
        except KeyError:
            if t[0] == tokenize.ENDMARKER:
                break
            else:
                raise SyntaxError("Syntax error")
    yield "(end)", "(end)"

def tokenize(program):
    for id, value in tokenize_python(program):
        if id == "(literal)":
            symbol = symbol_table[id]
            s = symbol()
            s.value = value
        else:
            # name or operator
            symbol = symbol_table.get(value)
            if symbol:
                s = symbol()
            elif id == "(name)":
                symbol = symbol_table[id]
                s = symbol()
                s.value = value
            else:
                raise SyntaxError("Unknown operator (%r)" % id) 
        yield s

def parse(program):
    global token, next
    next = tokenize(program).next
    token = next()
    return parse_expression()

