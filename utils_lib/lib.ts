import { FFIType, JSCallback, Pointer, dlopen, suffix } from "bun:ffi";

const platform = process.platform;
let path: string = "";

if (platform == "darwin") {
    path = import.meta.dir + `/build/libutils.${suffix}`;
} else {
    throw new Error(`not supported wgpu bindings platform "${platform}"`);
}

const utilslib = dlopen(path, {
    wgpuRequestInstance: {
        returns: FFIType.ptr,
        args: [
            // descriptor may be null
            FFIType.ptr,
        ],
    },
    wgpuRequestAdapter: {
        returns: FFIType.ptr,
        args: [
            // instance
            FFIType.ptr,
            // WGPURequestAdapterOptions
            FFIType.ptr,
        ],
    },
    glfwGetWGPUSurface: {
        returns: FFIType.ptr,
        args: [FFIType.ptr, FFIType.ptr],
    },
    wgpuRequestAdapter_window: {
        returns: FFIType.ptr,
        args: [FFIType.ptr, FFIType.ptr, FFIType.ptr],
    },
    wgpuRequestDevice: {
        returns: FFIType.ptr,
        args: [FFIType.ptr, FFIType.ptr],
    },
    catchException: {
        returns: FFIType.bool,
        args: [FFIType.function],
    },
}).symbols;

export function catchException(cb: () => void) {
    const jscb = new JSCallback(cb, {
        returns: FFIType.void,
        args: [],
    });

    const r = utilslib.catchException(jscb);
    jscb.close();

    if (!r) {
        throw new Error("ffi exception");
    }
}

export function wgpuRequestInstance() {
    return utilslib.wgpuRequestInstance(null);
}

export function wgpuRequestAdapter(instance: Pointer) {
    return utilslib.wgpuRequestAdapter(instance, null);
}

export function glfwGetWGPUSurface(instance: Pointer, window: Pointer) {
    return utilslib.glfwGetWGPUSurface(instance, window);
}

export function wgpuRequestAdapterGLFW(
    instance: Pointer,
    window: Pointer,
    surface: Pointer
) {
    return utilslib.wgpuRequestAdapter_window(instance, window, surface);
}

export function wgpuRequestDevice(adapter: Pointer) {
    return utilslib.wgpuRequestDevice(adapter, null);
}
