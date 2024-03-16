import {
    FFIType,
    JSCallback,
    dlopen,
    ptr,
    Pointer as BunPointer,
    Pointer,
    read,
    CString,
} from "bun:ffi";
import {
    writeWGPUSurfaceTexture,
    type WGPUSurfaceTexture,
    wgpuDeviceCreateShaderModule,
    writeWGPUShaderModuleDescriptor,
    writeWGPUShaderModuleWGSLDescriptor,
    writeWGPUChainedStruct,
    WGPUSType,
    WGPU_NULL,
    makeCString,
    WGPUShaderModuleWGSLDescriptor,
    readWGPUSurfaceTexture,
} from "./wgpu";

const platform = process.platform;
let path: string = "";

if (platform == "darwin") {
    path = import.meta.dir + "/../deps/wgpu/libwgpu_native.dylib";
} else {
    throw new Error("not supported wgpu bindings platform");
}

const wgpulib = dlopen(path, {
    wgpuSurfaceGetCurrentTexture: {
        returns: FFIType.void,
        args: [
            // surface
            FFIType.ptr,
            FFIType.ptr,
        ],
    },
}).symbols;

export function wgpuSurfaceGetCurrentTextureUtil(
    surface: Pointer
): WGPUSurfaceTexture {
    const buf = writeWGPUSurfaceTexture({});
    wgpulib.wgpuSurfaceGetCurrentTexture(surface, buf.typed);

    const result: WGPUSurfaceTexture = readWGPUSurfaceTexture(ptr(buf), 0);
    return result;
}

export function createShaderModuleWGSL(
    device: Pointer,
    name: string,
    sourceCode: string
) {
    return wgpuDeviceCreateShaderModule(device, {
        label: makeCString(sourceCode),
        nextInChain: writeWGPUShaderModuleWGSLDescriptor({
            chain: {
                next: WGPU_NULL,
                sType: WGPUSType.WGPUSType_ShaderModuleWGSLDescriptor,
            },
            code: makeCString(sourceCode),
        }).typed as any,
    });
}
