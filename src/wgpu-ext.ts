import { ptr, Pointer } from "bun:ffi";
import {
    type WGPUSurfaceTexture,
    wgpuDeviceCreateShaderModule,
    WGPUSType,
    NULL,
    alloc_CString,
    read_WGPUSurfaceTexture,
    alloc_WGPUShaderModuleWGSLDescriptor,
    alloc_WGPUSurfaceTexture,
    alloc_WGPUShaderModuleDescriptor,
    wgpuSurfaceGetCurrentTexture,
} from "./wgpu";

export function wgpuSurfaceGetCurrentTextureUtil(surface: Pointer): WGPUSurfaceTexture {
    const buf = alloc_WGPUSurfaceTexture({});
    wgpuSurfaceGetCurrentTexture(surface, buf);

    const result: WGPUSurfaceTexture = read_WGPUSurfaceTexture(ptr(buf), 0);
    return result;
}

export function createShaderModuleWGSL(device: Pointer, name: string, sourceCode: string) {
    const nextInChain = alloc_WGPUShaderModuleWGSLDescriptor({
        chain: {
            next: NULL,
            sType: WGPUSType.WGPUSType_ShaderModuleWGSLDescriptor,
        },
        code: alloc_CString(sourceCode),
    });

    const sd = alloc_WGPUShaderModuleDescriptor({
        label: alloc_CString(sourceCode),
        nextInChain: nextInChain as any,
    });

    return wgpuDeviceCreateShaderModule(device, sd);
}
