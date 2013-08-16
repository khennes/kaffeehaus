""" Header file for compiled asm.js files """

""" Typed arrays split implementation into a buffer [ArrayBuffer class] and a
view [ArrayBufferView class]."""

def header():
    x = '''
        var buffer = new ArrayBuffer(24);  # why 24?

        var Int8Array = new ArrayBufferView(buffer, 0, 0);  // data type, starting offset, # elements
        var Uint8Array = new ArrayBufferView(buffer, 
        var Int16Array = new ArrayBufferView(buffer, 
        var Uint16Array = new ArrayBufferView(buffer,
        var Int32Array = new ArrayBufferView(buffer,
        var Uint32Array = new ArrayBufferView(buffer,
        var Float32Array = new ArrayBufferView(buffer,
        var Float64Array = new ArrayBufferView(buffer,
    '''
    return x




