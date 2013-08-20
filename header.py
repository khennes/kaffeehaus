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

    var print = function(input) {
        for (i = 0; i <= input; i++) {
            while (heap[i] != "\0") {
                heap[i] = String.fromCharCode(heap[i]);
            }
        }
        document.getElementById("compilerOutput").innerHTML += input;
    }

    <!-- Revert strings to characters -->


    var foreign = {"print": print};

    var asmjs = Module(window, foreign, heap);
    return asmjs

    }
main().asmjs();
'''

