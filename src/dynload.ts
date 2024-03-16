import { FFIType, dlopen } from "bun:ffi";

export default dlopen("../deps/dyncall/macos_arm/libdynload_s.dylib", {
    dcFree: {
        args: [FFIType.ptr],
        returns: FFIType.void,
    },

    dlLoadLibrary: {
        args: [FFIType.cstring],
        returns: FFIType.ptr,
    },

    dlFreeLibrary: {
        args: [FFIType.ptr],
        returns: FFIType.void,
    },

    dlFindSymbol: {
        args: [FFIType.ptr, FFIType.cstring],
        returns: FFIType.ptr,
    },

    dlGetLibraryPath: {
        args: [FFIType.ptr, FFIType.ptr, FFIType.i32],
        returns: FFIType.i32,
    },

    dlSymsInit: {
        args: [FFIType.cstring],
        returns: FFIType.ptr,
    },

    dlSymsCleanup: {
        args: [FFIType.ptr],
        returns: FFIType.void,
    },

    dlSymsCount: {
        args: [FFIType.ptr],
        returns: FFIType.i32,
    },

    dlSymsName: {
        args: [FFIType.ptr, FFIType.i32],
        returns: FFIType.cstring,
    },

    dlSymsNameFromValue: {
        args: [FFIType.ptr, FFIType.ptr],
        returns: FFIType.cstring,
    },
}).symbols;
