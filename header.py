""" Header/footer for compiled .js files """

""" Typed arrays split implementation into a buffer [ArrayBuffer class] and a
view [ArrayBufferView class]."""

TEMPLATE = '''\
main = function() {

    var heap = new ArrayBuffer(512);
    strings = %s;

    function Module(stdlib, foreign, heap) {
        "use asm";

        <!-- Globals -->
        var I8 = new stdlib.Int8Array(heap);
        var U8 = new stdlib.Uint8Array(heap);
        var I16 = new stdlib.Int16Array(heap);
        var U16 = new stdlib.Uint16Array(heap);
        var I32 = new stdlib.Int32Array(heap);
        var U32 = new stdlib.Uint32Array(heap);

        var log = foreign.print;

        <!-- Functions -->
        
            %s

        <!-- End Functions -->
        
        <!-- Pointers to heap access -->

        %s;

        <!-- Exporting compiled functions -->

        %s;
    }

    <!-- Revert strings to characters -->
    var print = function(input) {
        if (typeof(input) == "object") {
            for (var i = 0; i <= input.length; i++) {
                var char_ = String.fromCharCode(input[i]);
                document.getElementById("compilerOutput").innerHTML += char_;
            }
        }
        else {
            document.getElementById("compilerOutput").innerHTML += input;
        }
        document.getElementById("compilerOutput").innerHTML += "<br />";
    }

    <!-- Declare foreign function interface -->
    var foreign = {"print": print};

    <!-- Call asm.js module inside of regular script -->
    var asmjs = Module(window, foreign, heap);
    return asmjs
}

main().fizzbuzz();
'''

