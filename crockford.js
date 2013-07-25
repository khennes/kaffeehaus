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

















    


