// Crockford's JS implementation of Pratt parsing
// http://javascript.crockford.com/tdop/tdop.html

var symbol_table = {};

// Prototype for all other symbols
// These default functions will usually be overridden
var original_symbol = {
    nud: function() {
        this.error("Undefined.");
    },
    led: function(left) {
        this.error("Missing operator.");
    }
};

// Factory function for making symbol objects
// If symbol already exists in symbol_table, return that symbol object
// Otherwise, make new symbol object that inherits from original_symbol &
// contains id, value, lbp.
var symbol = function(id, bp) {  // can't default to 0 here, ie bp=0?
    var s = symbol_table[id];
    bp = bp || 0;  // default 0 bp
    if (s) {
        if (bp >= s.lbp) {  // why does current token's bp have to == lbp?
            s.lbp = bp;
        }
    } else {  // create instance of symbol obj if not already in table
        s = Object.create(original_symbol); 
        s.id = s.value = id;  // why id == value? will this change later?
        s.lbp = bp;  // again, why does current token's bp have to == lbp?
        symbol_table[id] = s;  // insert new symbol obj into symbol_table
    }
    return s;  // return symbol object with id, value, lbp
};


/**** REGISTER SYMBOL OBJECTS ****/

symbol(":");  // add bp arg later?
symbol(";");
symbol(",");
symbol(")");
symbol("]");
symbol("}");
symbol("else"); 
symbol("(end)");  // indicates end of token stream
symbol("(name)");  // prototype for new names, eg variable names


/**** TOKENS ****/

var token;  // will always contain current token
var token_nr;  // added this, wasn't defined before reference

// make new token obj from next token in array, assign it to var token
// new token obj prototype is a (name) token in current scope, or a symbol from
// symbol_table. New token's arity is name, literal, or operator.
var advance = function(id) {
    var a, o, t, v;
    if (id && token.id !== id) {  // throw error if id differs from token.id
        token.error("Expected '" + id + "'.");
    }
    if (token_nr >= tokens.length) {  // where is token_nr defined?
        token = symbol_table["(end)"];  // end token stream
        return;
    }
    t = tokens[token_nr];  // tokens = token stream from lexer
    token_nr += 1;  //  go to next token in token stream
    v = t.value;
    a = t.type;
    if (a === "name") {
        o = scope.find(v);  // if current token type is name, find definition
    } else if (a === "operator") {
        o = symbol_table[v];  // if token type is operator, find token value
        if (!o) {
            t.error("Unknown operator.");  // throw error if value not found
        }
    } else if (a === "string" || a === "number") {
        a = "literal";  // classify as literal if token type is string or num
        o = symbol_table["(literal)"];  // still need a better handle on o
    } else {
        t.error("Unexpected token.");
    }
    token = Object.create(o);
    token.value = v;  // append value of current token to new token object
    token.arity = a;  // append type to new token object (as arg?)
                      // is arity still valid property in JS?
    return token;
};

/**** SCOPE ****/

var scope;  // contains current scope object

var itself = function() {
    return this;
}

var original_scope = {  // prototype for (name token) scope objects
    define: function(n) {  // transforms name -> variable token
        var t = this.def[n.value];  // anon fn - what does it dooo
        if (typeof t === "object") {  // why do we need t?
            n.error(t.reserved ?  //  error if t is already an object???
                "Already reserved." :
                "Already defined.");
        }
        // prototype defaults, to be mostly overridden
        this.def[n.value] = n;
        n.reserved  = false;
        n.nud       = itself;
        n.led       = null;
        n.std       = null;
        n.lbp       = 0;
        n.scope     = scope;
        return n;
    },
    find: function(n) {  // traces back thru scopes to find definition of name
        var e = this, o;  
        while (true) {
            o = e.def[n];
            if (o && typeof o !== 'function') {  // if o exists & is not a fn
                return e.def[n];  // return definition
            }
            e = e.parent;  // look inside parent scope
            if (!e) {  // if there is no parent scope, look inside symbol_table
                o = symbol_table[n];
                return o && typeof o !== 'function' ?  // if o exists & it's not a fn,
                                                       // return o; otherwise, return
                                                       // symbol_table["(name)"]
                                                       // (ternary if)
                        o : symbol_table["(name)"];
            }
        }
    },
    pop: function() {  // closes a scope, returns focus to parent
        scope = this.parent;
    },
    reserve: function(n) {  // indicates name has been used in current scope
        if (n.arity !== "name" || n.reserved) {  // if token type is not name \
                                                 // or if n.reserved == true
            return;
        }
        var t = this.def[n.value];  // still don't know what def does
        if (t) { 
            if (t.reserved) {  // if name is already reserved, return?
                return;
            }
            if (t.arity === "name") {
                n.error("Already defined.");
            }
        }
        this.def[n.value] = n;
        n.reserved = true;
    }
};

// create new scope instance
var new_scope = function() {
    var s = scope;
    scope = Object.create(original_scope);
    scope.def = {};  // why an object?
    scope.parent = s;
    return scope;
};


/**** PRECEDENCE ****/

/* Binding powers *
nud used by values (variables, literals), & prefix operators
led used by infix & suffix operators
0   non-binding ;
10  assignment =
20  ?
30  ||, &&
40  relational ===
50  +, -
60  *, /
70  unary !
80  ., [, (
*/

var parse_expression = function(rbp) {  // rbp = how tightly expr binds to tokens on right
    var left;
    var t = token;
    while (rbp < token.lbp) {  // here, token = next token
        t = token;
        advance();
        left = t.led(left);  // while true, invoke led method on next token; can recurse
    }
    return left;
}

/**** INFIX OPERATORS ****/

var infix = function(id, bp, led) {  // if no led given, supplies default method
    var s = symbol(id, bp);  // create symbol object
    s.led = led || function(left) {  // left operand passed in
        this.first = left;
        this.second = parse_expression(bp);  // right operand obtained by calling expr
        this.arity = "binary";
        return this;
    };
    return s;
}

infix("+", 50);
infix("-", 50);
infix("*", 60);
infix("/", 60);
infix("===", 40);
infix("!==", 40);
infix("<", 40);
infix("<=", 40);
infix(">", 40);
infix("=>", 40);

// LEFT-ASSOCIATIVE IRREGULAR PREFIX OPERATORS

// ternary operator
infix("?", 20, function(left) {
    this.first = left;
    this.second = parse_expression(0);
    advance(":");
    this.third = expression(0);
    this.arity = "ternary";
    return this;
});

// . operator
infix(".", 80, function(left) {
    this.first = left;
    if (token.arity !== "name") {
        token.error("Expected a property name.");
    }
    token.arity = "literal";
    this.second = token;
    this.arity = "binary";
    advance();
    return this;
});

// [ operator
infix("[", 80, function(left) {
    this.first = left;
    this.second = expression(0);  // why 0?
    this.arity = "binary";
    advance("]");
    return this;
});

// RIGHT-ASSOCIATIVE INFIX OPERATORS
var infix_r = function(id, bp, led) {
    var s = symbol(id, bp);
    s.led = led || function(left) {
        this.first = left;
        this.second = parse_expression(bp - 1);  // reduce rbp
        this.arity = "binary";
        return this;
    };
    return s;
}

// &&, || operators
infix_r("&&", 30);
infix_r("||", 30);

/**** PREFIX OPERATORS ****/

var prefix = function(id, nud) {
    var s = symbol(id);
    s.nud = nud || function() {  // no left, use nud
        scope.reserve(this);
        this.first = parse_expression(70);
        this.arity = "unary";
        return this;
    };
    return s;
}

prefix("-");
prefix("!");
prefix("typeof");

prefix("(", function() {
    var e = parse_expression(0);
    advance(")";
    return e;  // ( token does not become part of parse tree; nud returns inner expr
});


/**** ASSIGNMENT OPERATORS ****/

// We could use infix_r, but have added extra capability
var assignment = function(id) {
    return infix_r(id, 10, function(left) {
        if left.id !== "." && left.id !== "[" &&  // throw error if token type 
                                                  // is not . or [ & is not a name
                left.arity !== "name") {
            left.error("Bad lvalue.");
        }
        this.first = left;
        this.second = parse_expression(9);  // why lower bp?
        this.assignment = true;
        this.arity = "binary";
        return this;
    });
};

// Inheritance pattern: assignment returns infix_r result, which returns symbol result
assignment("=");
assignment("+=");
assignment("-=");


/**** CONSTANTS ****/

var constant = function(s, v) {  // change name token -> literal token
    var x = symbol(s);
    x.nud = function() {
        scope.reserve(this);  // reserved name?
        this.value = symbol_table[this.id].value;
        this.arity = "literal";
        return this;
    };
    x.value = v
    return x;
};

constant("true", true);
constant("false", false);
constant("null", null);
constant("pi", 3.141592653589793);

symbol("(literal)").nud = itself;  // prototype for str, num literals (see l95)


/**** STATEMENTS ****/

// Crockford adds functionality to handle statements as well as expressions
var parse_statement = function() {
    var n = token, v;
    if (n.std) {  // if token has std method, reserve token & invoke method
        advance();
        scope.reserve(n);
        return n.std();
    }
    v = parse_expression(0);  // otherwise, parse as expression
    if (!v.assignment && v.id !== "(" {  // throw error if not an assignment type or ( id
        v.error("Bad expression statement.");
    }
    advance(";");  // check for semicolon
    return v;
};

// Parse statements until (end) or } (end of block)
var statements = function() {
    var a = [], s;
    while (true) {
        if (token.id === "}" || token.id === "(end)" {
            break;
        }
        s = parse_statement();
        if (s) {
            a.push(s);
        }
    }
    return a.length === 0 ? null : a.length === 1 ? a[0] : a;
};

// add statement symbols to symbol_table
var stmt = function(s, f) {  // pass in statement id, std
    var x = symbol(s);
    x.std = f;
    return x;
};

// block statements: add block scope (not in actual JS)
stmt("{", function() {
    new_scope();
    var a = statements();
    advance("}");
    scope.pop();
    return a;
});

// parse block
var block = function() {
    var t = token;
    advance("{");
    return t.std();
};

// define variables in current block
// add 'var' symbol to symbol_table
stmt("var", function() {
    var a = [], n, t;
    while (true) {
        n = token;
        if (n.arity !== "name") {
            n.error("Expected a new variable name.");
        }
        scope.define(n);
        advance();
        if (token.id === "=") {
            t = token;
            advance("=");
            t.first = n;
            t.second = parse_expression(0);
            t.arity = "binary";
            a.push(t);
        }
        if (token.id !== ",") {
            break;
        }
        advance(",");
    }
    advance(";");
    return a.length === 0 ? null : a.length === 1 ? a[0] : a;
});

// add 'while' symbol to symbol_table
stmt("while", function() {
    advance("(");
    this.first = parse_expression(0);  // left child node = conditional expr
    advance(")");  // check for end of expr before moving on
    this.second = block();  // right child node = block
    this.arity = "statement";  // symbol type
    return this;
});

// add 'if' symbol to symbol_table
stmt("if", function() {
    advance("(");
    this.first = parse_expression(0);  // parse conditional expr (left child node)
    advance(")");  // check for end of expr, next
    this.second = block();  // right child node is a block
    if (token.id === "else") {
        scope.reserve(token);  // what does this line do?
        advance("else");
        this.third = token.id === "if" ? statement() : block();  // check for ternary case
    } else {
        this.third = null;
    }
    this.arity = "statement";
    return this;
});

// add 'break' symbol to symbol_table
stmt("break", function() {
    advance(";");
    if (token.id !== "}") {
        token.error("Unreachable statement.");
    }
    this.arity = "statement";
    return this;
});

// add 'return' symbol to symbol_table
stmt("return", function() {
    if (token.id !== ";") {
        this.first = parse_expression(0);
    }
    advance(";");
    if (token.id !== "}") {
        token.error("Unreachable statement.");
    }
    this.arity = "statement";
    return this;
});


/**** FUNCTIONS ****/

prefix("function", function() {
    var a = [];  // list of parameter names
    new_scope();  // create new scope instance with parent s
    if (token.arity === "name") {
        scope.define(token);  // define (reserve?) function name
        this.name = token.value;
        advance();
    }
    advance("(");
    if (token.id !== ")") {  // if function takes arguments
        while (true) {
            if (token.arity !== "name") {  // throw error if token type is not "name"
                token.error("Expected a parameter name.");
            }
            scope.define(token);  // reserve? token
            a.push(token); // add token to list of parameter names
            advance();
            if (token.id !== ",") {  // if only one parameter, break
                break; 
            }
            advance(",");  // return to top if >1 parameter
        }
    }
    this.first = a;  // left child: array of parameter names
    advance(")");  // check for closing RPAREN
    advance("{");  // check for opening LBRACE
    this.second = statements();  // right child: parse statements until end of block
    advance("}");  // check for closing RBRACE
    this.arity = "function";  // conclude that token type is function
    scope.pop();  // end scope
    return this;
});

infix("(", 80, function(left) {
    var a = [];
    if (left.id === "." || left.id === "[") {
        this.arity = "ternary";
        this.first = left.first;
        this.second = left.second;
        this.third = a;
    } else {
        this.arity = "binary";
        this.first = left;
        this.second = a;
        if ((left.arity !== "unary" || left.id !== "function") &&
                left.arity !== "name" && left.id !== "(" &&
                left.id !== "&&" && left.id !== "||" && left.id !== "?") {
            left.error("Expected a variable name.");
        }
    }
    if (token.id !== ")" {
        while (true) {
            a.push(parse_expression(0));
            if (token.id !== ",") {
                break;
            }
            advance(",");
        }
    }
    advance(")";
    return this;
});


symbol("this").nud = function() {
    scope.reserve(this);
    this.arity = "this";
    return this;
};


/**** OBJECT LITERALS ****/

// array literal: zero or more comma-separated expressions
// each expr is evaluated & results are collected into a new array

prefix("[", function() {
    var a = [];
    if (token.id !== "]") {
        while (true) {
            a.push(parse_expression(0));
            if (token.id !== ",") {
                break;
            }
            advance(",");
        }
    }
    advance("]");
    this.first = a;
    this.arity = "unary";
    return this;
});

// object literal: zero or more comma-separated key/expr pairs (key = literal)

prefix("{", function() {
    var a = [];
    if (token.id !== "}") {
        while (true) {
            var n = token;
            if (n.arity !== "name" && n.arity !== "literal") {
                token.error("Bad key.");
            }
            advance();
            advance(":");
            var v = parse_expression(0);
            v.key = n.value;
            a.push(v);
            if (token.id !== ",") {
                break;
            }
            advance(",");
        }
    }
    advance("}");
    this.first = a;
    this.arity = "unary";
    return this;
});

