import { ptr, Pointer } from "bun:ffi";
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
    makeWGPUShaderModuleWGSLDescriptor,
    makeWGPUSurfaceTexture,
    makeWGPUShaderModuleDescriptor,
    wgpuSurfaceGetCurrentTexture,
} from "./wgpu";

export function wgpuSurfaceGetCurrentTextureUtil(surface: Pointer): WGPUSurfaceTexture {
    const buf = makeWGPUSurfaceTexture({});
    wgpuSurfaceGetCurrentTexture(surface, buf);

    const result: WGPUSurfaceTexture = readWGPUSurfaceTexture(ptr(buf), 0);
    return result;
}

export function createShaderModuleWGSL(device: Pointer, name: string, sourceCode: string) {
    const sd = makeWGPUShaderModuleDescriptor({
        label: makeCString(sourceCode),
        nextInChain: makeWGPUShaderModuleWGSLDescriptor({
            chain: {
                next: WGPU_NULL,
                sType: WGPUSType.WGPUSType_ShaderModuleWGSLDescriptor,
            },
            code: makeCString(sourceCode),
        }) as any,
    });

    return wgpuDeviceCreateShaderModule(device, sd);
}
