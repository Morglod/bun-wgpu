import { ptr } from "bun:ffi";
import { catchException, glfwGetWGPUSurface, wgpuRequestAdapter, wgpuRequestAdapterGLFW, wgpuRequestDevice, wgpuRequestInstance } from "../utils_lib/lib";
import {
    GLFW_CLIENT_API,
    GLFW_DOUBLEBUFFER,
    GLFW_KEY_ESCAPE,
    GLFW_PRESS,
    GLFW_REFRESH_RATE,
    GLFW_TRUE,
    GLFW_VISIBLE,
    GLFWkeyfun,
    glfwCreateWindow,
    glfwDefaultWindowHints,
    glfwInit,
    glfwMakeContextCurrent,
    glfwPollEvents,
    glfwSetErrorCallback,
    glfwSetKeyCallback,
    glfwSetWindowShouldClose,
    glfwShowWindow,
    glfwSwapBuffers,
    glfwSwapInterval,
    glfwWindow,
    glfwWindowHint,
    glfwWindowShouldClose,
    glfw_error_codes,
} from "./glfw";
import {
    WGPUColorWriteMask,
    WGPUCompositeAlphaMode,
    WGPULoadOp,
    WGPUPresentMode,
    WGPUPrimitiveTopology,
    WGPURenderPipelineDescriptor,
    WGPUStoreOp,
    WGPUSurfaceConfiguration,
    WGPUSurfaceGetCurrentTextureStatus,
    WGPUTextureFormat,
    WGPUTextureUsage,
    NULL,
    alloc_CString,
    alloc_opaque_pointer,
    alloc_WGPUColorTargetState,
    alloc_WGPUFragmentState,
    alloc_WGPURenderPassColorAttachment,
    alloc_WGPUSurfaceCapabilities,
    alloc_WGPUSurfaceConfiguration,
    bunReadArray,
    read_WGPUCompositeAlphaMode,
    read_WGPUSurfaceCapabilities,
    read_WGPUTextureFormat,
    wgpuAdapterEnumerateFeatures,
    wgpuCommandBufferRelease,
    wgpuCommandEncoderBeginRenderPass,
    wgpuCommandEncoderFinish,
    wgpuCommandEncoderRelease,
    wgpuCreateInstance,
    wgpuDeviceCreateCommandEncoder,
    wgpuDeviceCreatePipelineLayout,
    wgpuDeviceCreateRenderPipeline,
    wgpuDeviceCreateShaderModule,
    wgpuDeviceGetQueue,
    wgpuInstanceRequestAdapter,
    wgpuQueueSubmit,
    wgpuRenderPassEncoderDraw,
    wgpuRenderPassEncoderEnd,
    wgpuRenderPassEncoderRelease,
    wgpuRenderPassEncoderSetPipeline,
    wgpuSurfaceConfigure,
    wgpuSurfaceGetCapabilities,
    wgpuSurfaceGetCurrentTexture,
    wgpuSurfaceGetPreferredFormat,
    wgpuSurfacePresent,
    wgpuSurfaceUnconfigure,
    wgpuTextureCreateView,
    wgpuTextureRelease,
    wgpuTextureViewRelease,
    write_opaque_pointer,
    write_WGPUColorTargetState,
    write_WGPUFragmentState,
    write_WGPUInstanceDescriptor,
    write_WGPURenderPassColorAttachment,
    write_WGPUSurfaceCapabilities,
    write_WGPUSurfaceConfiguration,
    write_WGPUSurfaceTexture,
} from "./wgpu";
import { createShaderModuleWGSL, wgpuSurfaceGetCurrentTextureUtil } from "./wgpu-ext";
// without this import crashing
import { gcAndSweep, heapSize, memoryUsage } from "bun:jsc";

const error_call = (error_code: number, description: string) => {
    [...glfw_error_codes.keys()].forEach((key) => {
        if (glfw_error_codes.get(key) == error_code) {
            console.error(key);
        }
    });
};

let controls: GLFWkeyfun = (window: glfwWindow, key: number, scancode: number, action: number, mods: number) => {
    console.log("input", action, key);
    if (action == GLFW_PRESS) {
        if (key == GLFW_KEY_ESCAPE) {
            glfwSetWindowShouldClose(window, GLFW_TRUE);
        }
    }
};

console.log("arch", process.arch);

async function main() {
    // https://github.com/gfx-rs/wgpu-native/blob/trunk/examples/triangle/main.c

    glfwInit();
    glfwDefaultWindowHints();
    glfwWindowHint(GLFW_CLIENT_API, 0);
    glfwWindowHint(GLFW_VISIBLE, 0);
    glfwWindowHint(GLFW_REFRESH_RATE, 0);
    glfwWindowHint(GLFW_DOUBLEBUFFER, 0);

    const instance = wgpuRequestInstance()!;
    const window = glfwCreateWindow(800, 600, "Hello world", null, null)!;
    glfwMakeContextCurrent(window);

    glfwSwapInterval(0);

    glfwSetKeyCallback(window, controls);
    glfwSetErrorCallback(error_call);

    const surface = glfwGetWGPUSurface(instance, window)!;
    const adapter = wgpuRequestAdapterGLFW(instance, window, surface)!;
    const device = wgpuRequestDevice(adapter)!;

    const surfaceCapabilitiesBuf = alloc_WGPUSurfaceCapabilities({});
    wgpuSurfaceGetCapabilities(surface, adapter, surfaceCapabilitiesBuf);
    const surfaceCapabilities = read_WGPUSurfaceCapabilities(ptr(surfaceCapabilitiesBuf), 0);

    const surfaceCapabilitiesFormats = bunReadArray<WGPUTextureFormat>(surfaceCapabilities.formats, 0, 4, read_WGPUTextureFormat, surfaceCapabilities.formatCount);

    const surfaceCapabilitiesAlphaModes = bunReadArray<WGPUCompositeAlphaMode>(surfaceCapabilities.alphaModes, 0, 4, read_WGPUCompositeAlphaMode, surfaceCapabilities.alphaModeCount);

    const surfaceFormat = surfaceCapabilitiesFormats[0];
    const alphaMode = surfaceCapabilitiesAlphaModes[0];

    const queue = wgpuDeviceGetQueue(device)!;

    glfwShowWindow(window);

    debugger;

    const shaderModule = createShaderModuleWGSL(
        device,
        "shader.wgsl",
        `
        @vertex
        fn vs_main(@builtin(vertex_index) in_vertex_index: u32) -> @builtin(position) vec4<f32> {
            let x = f32(i32(in_vertex_index) - 1);
            let y = f32(i32(in_vertex_index & 1u) * 2 - 1);
            return vec4<f32>(x, y, 0.0, 1.0);
        }

        @fragment
        fn fs_main() -> @location(0) vec4<f32> {
            return vec4<f32>(1.0, 0.0, 0.0, 1.0);
        }`
    );

    debugger;

    const pipelineLayout = wgpuDeviceCreatePipelineLayout(device, {
        // label: alloc_CString("pipeline_layout"),
    })!;

    debugger;

    const renderPipeline = wgpuDeviceCreateRenderPipeline(device, {
        // label: alloc_CString("render_pipeline"),
        layout: pipelineLayout,
        vertex: {
            module: shaderModule,
            entryPoint: alloc_CString("vs_main"),
        },
        fragment: alloc_WGPUFragmentState({
            module: shaderModule,
            entryPoint: alloc_CString("fs_main"),
            targetCount: 1,
            targets: alloc_WGPUColorTargetState({
                format: surfaceFormat,
                writeMask: WGPUColorWriteMask.WGPUColorWriteMask_All,
            }),
        }),
        primitive: {
            topology: WGPUPrimitiveTopology.WGPUPrimitiveTopology_TriangleList,
        },
        multisample: {
            count: 1,
            mask: 0xffffffff,
        },
    })!;

    const textureFormat = wgpuSurfaceGetPreferredFormat(surface, adapter);
    const surfaceConfig: Partial<WGPUSurfaceConfiguration> = {
        device,
        format: textureFormat,
        usage: WGPUTextureUsage.WGPUTextureUsage_RenderAttachment,
        presentMode: WGPUPresentMode.WGPUPresentMode_Fifo,
        alphaMode: WGPUCompositeAlphaMode.WGPUCompositeAlphaMode_Auto,
        width: 800,
        height: 600,
    };
    wgpuSurfaceConfigure(surface, surfaceConfig);

    debugger;

    let frames = 0;
    let frameTimeTotal = 0;

    while (!glfwWindowShouldClose(window)) {
        glfwPollEvents();

        const frameStarTime = Date.now();

        const surfaceTexture = wgpuSurfaceGetCurrentTextureUtil(surface);

        switch (surfaceTexture.status) {
            case WGPUSurfaceGetCurrentTextureStatus.WGPUSurfaceGetCurrentTextureStatus_Success:
                // All good, could check for `surface_texture.suboptimal` here.
                break;
            case WGPUSurfaceGetCurrentTextureStatus.WGPUSurfaceGetCurrentTextureStatus_Timeout:
            case WGPUSurfaceGetCurrentTextureStatus.WGPUSurfaceGetCurrentTextureStatus_Outdated:
            case WGPUSurfaceGetCurrentTextureStatus.WGPUSurfaceGetCurrentTextureStatus_Lost: {
                // Skip this frame, and re-configure surface.
                if (surfaceTexture.texture !== NULL) {
                    wgpuTextureRelease(surfaceTexture.texture);
                }
                wgpuSurfaceConfigure(surface, alloc_WGPUSurfaceConfiguration(surfaceConfig));
                continue;
            }
            case WGPUSurfaceGetCurrentTextureStatus.WGPUSurfaceGetCurrentTextureStatus_OutOfMemory:
            case WGPUSurfaceGetCurrentTextureStatus.WGPUSurfaceGetCurrentTextureStatus_DeviceLost:
            case WGPUSurfaceGetCurrentTextureStatus.WGPUSurfaceGetCurrentTextureStatus_Force32:
                // Fatal error
                throw new Error("get_current_texture status " + surfaceTexture.status);
        }

        debugger;

        const frame = wgpuTextureCreateView(surfaceTexture.texture, NULL);

        const commandEncoder = wgpuDeviceCreateCommandEncoder(device, {
            // label: alloc_CString("command_encoder"),
        });

        debugger;

        const renderPassEncoder = wgpuCommandEncoderBeginRenderPass(commandEncoder, {
            // label: alloc_CString("render_pass_encoder"),
            colorAttachmentCount: 1,
            colorAttachments: alloc_WGPURenderPassColorAttachment({
                view: frame,
                loadOp: WGPULoadOp.WGPULoadOp_Clear,
                storeOp: WGPUStoreOp.WGPUStoreOp_Store,
                clearValue: {
                    r: 0.0,
                    g: 1.0,
                    b: 0.0,
                    a: 1.0,
                },
            }),
        });

        debugger;

        wgpuRenderPassEncoderSetPipeline(renderPassEncoder, renderPipeline);
        wgpuRenderPassEncoderDraw(renderPassEncoder, 3, 1, 0, 0);
        wgpuRenderPassEncoderEnd(renderPassEncoder);

        const commandBuffer = wgpuCommandEncoderFinish(commandEncoder, {})!;

        const commandBufferArr = alloc_opaque_pointer(commandBuffer);
        wgpuQueueSubmit(queue, 1, ptr(commandBufferArr) as any);

        wgpuSurfacePresent(surface);

        wgpuCommandBufferRelease(commandBuffer);
        wgpuRenderPassEncoderRelease(renderPassEncoder);
        wgpuCommandEncoderRelease(commandEncoder);
        wgpuTextureViewRelease(frame);
        wgpuTextureRelease(surfaceTexture.texture);

        frameTimeTotal += Date.now() - frameStarTime;
        ++frames;

        // debugger;

        // glfwSwapBuffers(window);
    }

    const fps = frames / (frameTimeTotal / 1000);
    console.log({ fps, frames });
}

main();
