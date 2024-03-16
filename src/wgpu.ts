import {
    FFIType,
    JSCallback as BunJSCallback,
    CFunction as BunCFunction,
    dlopen,
    CString as BunCString,
    ptr,
    Pointer as BunPointer,
    read as bunRead,
} from "bun:ffi";

// prettier-ignore
type DeepPartial<T> = {
    [P in keyof T]?:
    T[P] extends (...args: any) => any ? T[P] :
    T[P] extends PtrT<any> ? T[P] :
    T[P] extends ConstPtrT<any> ? T[P] :
    T[P] extends CString ? T[P] :
    T[P] extends object ? DeepPartial<T[P]> :
    T[P];
};

const int32_t = FFIType.int32_t;
type int32_t = number;

const uint16_t = FFIType.uint16_t;
type uint16_t = number;

const uint32_t = FFIType.uint32_t;
type uint32_t = number;

const uint64_t = FFIType.uint64_t;
type uint64_t = bigint | number;

const double = FFIType.double;
type double = number;

const size_t = FFIType.uint64_t;
type size_t = bigint | number;

const float = FFIType.f32;
type float = number;

const Pointer = FFIType.ptr;
type Pointer = BunPointer | null;

const CString = FFIType.cstring;
type CString = BunCString;

type ConstPtrT<T> = (Pointer | TypedArray) & { __type: T; __const_ptr: true };
type PtrT<T> = (Pointer | TypedArray) & { __type: T; __const_ptr: false };

function isPtrOrConstPtr(x: any): x is Pointer | TypedArray {
    if (!x) return true;
    return (
        typeof x === "number" ||
        (typeof x === "object" && "BYTES_PER_ELEMENT" in x)
    );
}

type TypedArrayPtr<T> = TypedArray & { __type: T; __const_ptr: any };

const void_FFI = FFIType.void;
const size_t_FFI = FFIType.uint64_t;

const int32_t_FFI = FFIType.int32_t;
const uint16_t_FFI = FFIType.uint16_t;
const uint32_t_FFI = FFIType.uint32_t;
const uint64_t_FFI = FFIType.uint64_t;
const double_FFI = FFIType.double;
const float_FFI = FFIType.f32;

export const WGPU_NULL = null as any as Pointer & {
    __type: any;
    __const_ptr: any;
};

export class DataViewExt<T = 1> extends DataView {
    constructor(
        buffer: ArrayBufferLike & { BYTES_PER_ELEMENT?: undefined },
        byteOffset?: number | undefined,
        byteLength?: number | undefined
    ) {
        super(buffer, byteOffset, byteLength);
    }

    get typed(): T extends 1 ? TypedArray : TypedArrayPtr<T> {
        return new Uint8Array(this.buffer) as any;
    }

    subview(offset: number): DataViewExt {
        return new DataViewExt(this.buffer, this.byteOffset + offset);
    }

    static alloc(size: number) {
        return new DataViewExt(Buffer.alloc(size).buffer);
    }
}

export function writePtrT<T>(x: PtrT<T>, dataView?: DataViewExt) {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView;
    if (typeof x === "object") {
        if ("BYTES_PER_ELEMENT" in x) x = ptr(x) as any;
        else throw new Error("got unknown object in writePtrT");
    }
    dataView.setBigUint64(0, BigInt(x as any), true);
    return dataView;
}

export function writePointer(x: Pointer, dataView?: DataViewExt) {
    return writePtrT(x as any, dataView);
}

export const writeConstPtrT = writePtrT;

export function readPtrT<T>(from: Pointer, offset: number): PtrT<T> {
    return bunRead.ptr(from!, offset) as any as PtrT<T>;
}

export const readConstPtrT = readPtrT;

export function writeCString(x: CString, dataView?: DataViewExt): DataViewExt {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView;
    dataView.setBigUint64(0, BigInt(x.ptr), true);
    return dataView;
}

export function writevoid(x: any, dataView?: DataViewExt): DataViewExt {
    throw new Error("called stub writevoid");
}

export function readCString(from: Pointer, offset: number): CString {
    return new BunCString(bunRead.ptr(from!, offset));
}

export function makeCString(str: string) {
    return new BunCString(ptr(Buffer.from(str + "\0")));
}

export function readArray<T>(
    from: Pointer | PtrT<T>,
    offset: number,
    cTypeSize: number,
    itemReader: (from: Pointer, offset: number) => T,
    length: number | bigint
): T[] {
    if (from === null) throw new Error("readArray null pointer");
    if (typeof from !== "number") from = ptr(from);
    const out = [] as T[];
    for (let i = 0; i < length; ++i) {
        out.push(itemReader(from!, offset + cTypeSize * i));
    }
    return out;
}

export type WGPUFlags = uint32_t;
export const WGPUFlags_FFI = uint32_t_FFI;
export type WGPUBool = uint32_t;
export const WGPUBool_FFI = uint32_t_FFI;
export type WGPUAdapter = Pointer;
export const WGPUAdapter_FFI = Pointer;
export type WGPUBindGroup = Pointer;
export const WGPUBindGroup_FFI = Pointer;
export type WGPUBindGroupLayout = Pointer;
export const WGPUBindGroupLayout_FFI = Pointer;
export type WGPUBuffer = Pointer;
export const WGPUBuffer_FFI = Pointer;
export type WGPUCommandBuffer = Pointer;
export const WGPUCommandBuffer_FFI = Pointer;
export type WGPUCommandEncoder = Pointer;
export const WGPUCommandEncoder_FFI = Pointer;
export type WGPUComputePassEncoder = Pointer;
export const WGPUComputePassEncoder_FFI = Pointer;
export type WGPUComputePipeline = Pointer;
export const WGPUComputePipeline_FFI = Pointer;
export type WGPUDevice = Pointer;
export const WGPUDevice_FFI = Pointer;
export type WGPUInstance = Pointer;
export const WGPUInstance_FFI = Pointer;
export type WGPUPipelineLayout = Pointer;
export const WGPUPipelineLayout_FFI = Pointer;
export type WGPUQuerySet = Pointer;
export const WGPUQuerySet_FFI = Pointer;
export type WGPUQueue = Pointer;
export const WGPUQueue_FFI = Pointer;
export type WGPURenderBundle = Pointer;
export const WGPURenderBundle_FFI = Pointer;
export type WGPURenderBundleEncoder = Pointer;
export const WGPURenderBundleEncoder_FFI = Pointer;
export type WGPURenderPassEncoder = Pointer;
export const WGPURenderPassEncoder_FFI = Pointer;
export type WGPURenderPipeline = Pointer;
export const WGPURenderPipeline_FFI = Pointer;
export type WGPUSampler = Pointer;
export const WGPUSampler_FFI = Pointer;
export type WGPUShaderModule = Pointer;
export const WGPUShaderModule_FFI = Pointer;
export type WGPUSurface = Pointer;
export const WGPUSurface_FFI = Pointer;
export type WGPUTexture = Pointer;
export const WGPUTexture_FFI = Pointer;
export type WGPUTextureView = Pointer;
export const WGPUTextureView_FFI = Pointer;
export enum WGPUAdapterType {
    WGPUAdapterType_DiscreteGPU = 0,
    WGPUAdapterType_IntegratedGPU = 1,
    WGPUAdapterType_CPU = 2,
    WGPUAdapterType_Unknown = 3,
    WGPUAdapterType_Force32 = 2147483647,
}
export const WGPUAdapterType_FFI = FFIType.int32_t;

export enum WGPUAddressMode {
    WGPUAddressMode_Repeat = 0,
    WGPUAddressMode_MirrorRepeat = 1,
    WGPUAddressMode_ClampToEdge = 2,
    WGPUAddressMode_Force32 = 2147483647,
}
export const WGPUAddressMode_FFI = FFIType.int32_t;

export enum WGPUBackendType {
    WGPUBackendType_Undefined = 0,
    WGPUBackendType_Null = 1,
    WGPUBackendType_WebGPU = 2,
    WGPUBackendType_D3D11 = 3,
    WGPUBackendType_D3D12 = 4,
    WGPUBackendType_Metal = 5,
    WGPUBackendType_Vulkan = 6,
    WGPUBackendType_OpenGL = 7,
    WGPUBackendType_OpenGLES = 8,
    WGPUBackendType_Force32 = 2147483647,
}
export const WGPUBackendType_FFI = FFIType.int32_t;

export enum WGPUBlendFactor {
    WGPUBlendFactor_Zero = 0,
    WGPUBlendFactor_One = 1,
    WGPUBlendFactor_Src = 2,
    WGPUBlendFactor_OneMinusSrc = 3,
    WGPUBlendFactor_SrcAlpha = 4,
    WGPUBlendFactor_OneMinusSrcAlpha = 5,
    WGPUBlendFactor_Dst = 6,
    WGPUBlendFactor_OneMinusDst = 7,
    WGPUBlendFactor_DstAlpha = 8,
    WGPUBlendFactor_OneMinusDstAlpha = 9,
    WGPUBlendFactor_SrcAlphaSaturated = 10,
    WGPUBlendFactor_Constant = 11,
    WGPUBlendFactor_OneMinusConstant = 12,
    WGPUBlendFactor_Force32 = 2147483647,
}
export const WGPUBlendFactor_FFI = FFIType.int32_t;

export enum WGPUBlendOperation {
    WGPUBlendOperation_Add = 0,
    WGPUBlendOperation_Subtract = 1,
    WGPUBlendOperation_ReverseSubtract = 2,
    WGPUBlendOperation_Min = 3,
    WGPUBlendOperation_Max = 4,
    WGPUBlendOperation_Force32 = 2147483647,
}
export const WGPUBlendOperation_FFI = FFIType.int32_t;

export enum WGPUBufferBindingType {
    WGPUBufferBindingType_Undefined = 0,
    WGPUBufferBindingType_Uniform = 1,
    WGPUBufferBindingType_Storage = 2,
    WGPUBufferBindingType_ReadOnlyStorage = 3,
    WGPUBufferBindingType_Force32 = 2147483647,
}
export const WGPUBufferBindingType_FFI = FFIType.int32_t;

export enum WGPUBufferMapAsyncStatus {
    WGPUBufferMapAsyncStatus_Success = 0,
    WGPUBufferMapAsyncStatus_ValidationError = 1,
    WGPUBufferMapAsyncStatus_Unknown = 2,
    WGPUBufferMapAsyncStatus_DeviceLost = 3,
    WGPUBufferMapAsyncStatus_DestroyedBeforeCallback = 4,
    WGPUBufferMapAsyncStatus_UnmappedBeforeCallback = 5,
    WGPUBufferMapAsyncStatus_MappingAlreadyPending = 6,
    WGPUBufferMapAsyncStatus_OffsetOutOfRange = 7,
    WGPUBufferMapAsyncStatus_SizeOutOfRange = 8,
    WGPUBufferMapAsyncStatus_Force32 = 2147483647,
}
export const WGPUBufferMapAsyncStatus_FFI = FFIType.int32_t;

export enum WGPUBufferMapState {
    WGPUBufferMapState_Unmapped = 0,
    WGPUBufferMapState_Pending = 1,
    WGPUBufferMapState_Mapped = 2,
    WGPUBufferMapState_Force32 = 2147483647,
}
export const WGPUBufferMapState_FFI = FFIType.int32_t;

export enum WGPUCompareFunction {
    WGPUCompareFunction_Undefined = 0,
    WGPUCompareFunction_Never = 1,
    WGPUCompareFunction_Less = 2,
    WGPUCompareFunction_LessEqual = 3,
    WGPUCompareFunction_Greater = 4,
    WGPUCompareFunction_GreaterEqual = 5,
    WGPUCompareFunction_Equal = 6,
    WGPUCompareFunction_NotEqual = 7,
    WGPUCompareFunction_Always = 8,
    WGPUCompareFunction_Force32 = 2147483647,
}
export const WGPUCompareFunction_FFI = FFIType.int32_t;

export enum WGPUCompilationInfoRequestStatus {
    WGPUCompilationInfoRequestStatus_Success = 0,
    WGPUCompilationInfoRequestStatus_Error = 1,
    WGPUCompilationInfoRequestStatus_DeviceLost = 2,
    WGPUCompilationInfoRequestStatus_Unknown = 3,
    WGPUCompilationInfoRequestStatus_Force32 = 2147483647,
}
export const WGPUCompilationInfoRequestStatus_FFI = FFIType.int32_t;

export enum WGPUCompilationMessageType {
    WGPUCompilationMessageType_Error = 0,
    WGPUCompilationMessageType_Warning = 1,
    WGPUCompilationMessageType_Info = 2,
    WGPUCompilationMessageType_Force32 = 2147483647,
}
export const WGPUCompilationMessageType_FFI = FFIType.int32_t;

export enum WGPUCompositeAlphaMode {
    WGPUCompositeAlphaMode_Auto = 0,
    WGPUCompositeAlphaMode_Opaque = 1,
    WGPUCompositeAlphaMode_Premultiplied = 2,
    WGPUCompositeAlphaMode_Unpremultiplied = 3,
    WGPUCompositeAlphaMode_Inherit = 4,
    WGPUCompositeAlphaMode_Force32 = 2147483647,
}
export const WGPUCompositeAlphaMode_FFI = FFIType.int32_t;

export enum WGPUCreatePipelineAsyncStatus {
    WGPUCreatePipelineAsyncStatus_Success = 0,
    WGPUCreatePipelineAsyncStatus_ValidationError = 1,
    WGPUCreatePipelineAsyncStatus_InternalError = 2,
    WGPUCreatePipelineAsyncStatus_DeviceLost = 3,
    WGPUCreatePipelineAsyncStatus_DeviceDestroyed = 4,
    WGPUCreatePipelineAsyncStatus_Unknown = 5,
    WGPUCreatePipelineAsyncStatus_Force32 = 2147483647,
}
export const WGPUCreatePipelineAsyncStatus_FFI = FFIType.int32_t;

export enum WGPUCullMode {
    WGPUCullMode_None = 0,
    WGPUCullMode_Front = 1,
    WGPUCullMode_Back = 2,
    WGPUCullMode_Force32 = 2147483647,
}
export const WGPUCullMode_FFI = FFIType.int32_t;

export enum WGPUDeviceLostReason {
    WGPUDeviceLostReason_Undefined = 0,
    WGPUDeviceLostReason_Destroyed = 1,
    WGPUDeviceLostReason_Force32 = 2147483647,
}
export const WGPUDeviceLostReason_FFI = FFIType.int32_t;

export enum WGPUErrorFilter {
    WGPUErrorFilter_Validation = 0,
    WGPUErrorFilter_OutOfMemory = 1,
    WGPUErrorFilter_Internal = 2,
    WGPUErrorFilter_Force32 = 2147483647,
}
export const WGPUErrorFilter_FFI = FFIType.int32_t;

export enum WGPUErrorType {
    WGPUErrorType_NoError = 0,
    WGPUErrorType_Validation = 1,
    WGPUErrorType_OutOfMemory = 2,
    WGPUErrorType_Internal = 3,
    WGPUErrorType_Unknown = 4,
    WGPUErrorType_DeviceLost = 5,
    WGPUErrorType_Force32 = 2147483647,
}
export const WGPUErrorType_FFI = FFIType.int32_t;

export enum WGPUFeatureName {
    WGPUFeatureName_Undefined = 0,
    WGPUFeatureName_DepthClipControl = 1,
    WGPUFeatureName_Depth32FloatStencil8 = 2,
    WGPUFeatureName_TimestampQuery = 3,
    WGPUFeatureName_TextureCompressionBC = 4,
    WGPUFeatureName_TextureCompressionETC2 = 5,
    WGPUFeatureName_TextureCompressionASTC = 6,
    WGPUFeatureName_IndirectFirstInstance = 7,
    WGPUFeatureName_ShaderF16 = 8,
    WGPUFeatureName_RG11B10UfloatRenderable = 9,
    WGPUFeatureName_BGRA8UnormStorage = 10,
    WGPUFeatureName_Float32Filterable = 11,
    WGPUFeatureName_Force32 = 2147483647,
}
export const WGPUFeatureName_FFI = FFIType.int32_t;

export enum WGPUFilterMode {
    WGPUFilterMode_Nearest = 0,
    WGPUFilterMode_Linear = 1,
    WGPUFilterMode_Force32 = 2147483647,
}
export const WGPUFilterMode_FFI = FFIType.int32_t;

export enum WGPUFrontFace {
    WGPUFrontFace_CCW = 0,
    WGPUFrontFace_CW = 1,
    WGPUFrontFace_Force32 = 2147483647,
}
export const WGPUFrontFace_FFI = FFIType.int32_t;

export enum WGPUIndexFormat {
    WGPUIndexFormat_Undefined = 0,
    WGPUIndexFormat_Uint16 = 1,
    WGPUIndexFormat_Uint32 = 2,
    WGPUIndexFormat_Force32 = 2147483647,
}
export const WGPUIndexFormat_FFI = FFIType.int32_t;

export enum WGPULoadOp {
    WGPULoadOp_Undefined = 0,
    WGPULoadOp_Clear = 1,
    WGPULoadOp_Load = 2,
    WGPULoadOp_Force32 = 2147483647,
}
export const WGPULoadOp_FFI = FFIType.int32_t;

export enum WGPUMipmapFilterMode {
    WGPUMipmapFilterMode_Nearest = 0,
    WGPUMipmapFilterMode_Linear = 1,
    WGPUMipmapFilterMode_Force32 = 2147483647,
}
export const WGPUMipmapFilterMode_FFI = FFIType.int32_t;

export enum WGPUPowerPreference {
    WGPUPowerPreference_Undefined = 0,
    WGPUPowerPreference_LowPower = 1,
    WGPUPowerPreference_HighPerformance = 2,
    WGPUPowerPreference_Force32 = 2147483647,
}
export const WGPUPowerPreference_FFI = FFIType.int32_t;

export enum WGPUPresentMode {
    WGPUPresentMode_Fifo = 0,
    WGPUPresentMode_FifoRelaxed = 1,
    WGPUPresentMode_Immediate = 2,
    WGPUPresentMode_Mailbox = 3,
    WGPUPresentMode_Force32 = 2147483647,
}
export const WGPUPresentMode_FFI = FFIType.int32_t;

export enum WGPUPrimitiveTopology {
    WGPUPrimitiveTopology_PointList = 0,
    WGPUPrimitiveTopology_LineList = 1,
    WGPUPrimitiveTopology_LineStrip = 2,
    WGPUPrimitiveTopology_TriangleList = 3,
    WGPUPrimitiveTopology_TriangleStrip = 4,
    WGPUPrimitiveTopology_Force32 = 2147483647,
}
export const WGPUPrimitiveTopology_FFI = FFIType.int32_t;

export enum WGPUQueryType {
    WGPUQueryType_Occlusion = 0,
    WGPUQueryType_Timestamp = 1,
    WGPUQueryType_Force32 = 2147483647,
}
export const WGPUQueryType_FFI = FFIType.int32_t;

export enum WGPUQueueWorkDoneStatus {
    WGPUQueueWorkDoneStatus_Success = 0,
    WGPUQueueWorkDoneStatus_Error = 1,
    WGPUQueueWorkDoneStatus_Unknown = 2,
    WGPUQueueWorkDoneStatus_DeviceLost = 3,
    WGPUQueueWorkDoneStatus_Force32 = 2147483647,
}
export const WGPUQueueWorkDoneStatus_FFI = FFIType.int32_t;

export enum WGPURequestAdapterStatus {
    WGPURequestAdapterStatus_Success = 0,
    WGPURequestAdapterStatus_Unavailable = 1,
    WGPURequestAdapterStatus_Error = 2,
    WGPURequestAdapterStatus_Unknown = 3,
    WGPURequestAdapterStatus_Force32 = 2147483647,
}
export const WGPURequestAdapterStatus_FFI = FFIType.int32_t;

export enum WGPURequestDeviceStatus {
    WGPURequestDeviceStatus_Success = 0,
    WGPURequestDeviceStatus_Error = 1,
    WGPURequestDeviceStatus_Unknown = 2,
    WGPURequestDeviceStatus_Force32 = 2147483647,
}
export const WGPURequestDeviceStatus_FFI = FFIType.int32_t;

export enum WGPUSType {
    WGPUSType_Invalid = 0,
    WGPUSType_SurfaceDescriptorFromMetalLayer = 1,
    WGPUSType_SurfaceDescriptorFromWindowsHWND = 2,
    WGPUSType_SurfaceDescriptorFromXlibWindow = 3,
    WGPUSType_SurfaceDescriptorFromCanvasHTMLSelector = 4,
    WGPUSType_ShaderModuleSPIRVDescriptor = 5,
    WGPUSType_ShaderModuleWGSLDescriptor = 6,
    WGPUSType_PrimitiveDepthClipControl = 7,
    WGPUSType_SurfaceDescriptorFromWaylandSurface = 8,
    WGPUSType_SurfaceDescriptorFromAndroidNativeWindow = 9,
    WGPUSType_SurfaceDescriptorFromXcbWindow = 10,
    WGPUSType_RenderPassDescriptorMaxDrawCount = 15,
    WGPUSType_Force32 = 2147483647,
}
export const WGPUSType_FFI = FFIType.int32_t;

export enum WGPUSamplerBindingType {
    WGPUSamplerBindingType_Undefined = 0,
    WGPUSamplerBindingType_Filtering = 1,
    WGPUSamplerBindingType_NonFiltering = 2,
    WGPUSamplerBindingType_Comparison = 3,
    WGPUSamplerBindingType_Force32 = 2147483647,
}
export const WGPUSamplerBindingType_FFI = FFIType.int32_t;

export enum WGPUStencilOperation {
    WGPUStencilOperation_Keep = 0,
    WGPUStencilOperation_Zero = 1,
    WGPUStencilOperation_Replace = 2,
    WGPUStencilOperation_Invert = 3,
    WGPUStencilOperation_IncrementClamp = 4,
    WGPUStencilOperation_DecrementClamp = 5,
    WGPUStencilOperation_IncrementWrap = 6,
    WGPUStencilOperation_DecrementWrap = 7,
    WGPUStencilOperation_Force32 = 2147483647,
}
export const WGPUStencilOperation_FFI = FFIType.int32_t;

export enum WGPUStorageTextureAccess {
    WGPUStorageTextureAccess_Undefined = 0,
    WGPUStorageTextureAccess_WriteOnly = 1,
    WGPUStorageTextureAccess_ReadOnly = 2,
    WGPUStorageTextureAccess_ReadWrite = 3,
    WGPUStorageTextureAccess_Force32 = 2147483647,
}
export const WGPUStorageTextureAccess_FFI = FFIType.int32_t;

export enum WGPUStoreOp {
    WGPUStoreOp_Undefined = 0,
    WGPUStoreOp_Store = 1,
    WGPUStoreOp_Discard = 2,
    WGPUStoreOp_Force32 = 2147483647,
}
export const WGPUStoreOp_FFI = FFIType.int32_t;

export enum WGPUSurfaceGetCurrentTextureStatus {
    WGPUSurfaceGetCurrentTextureStatus_Success = 0,
    WGPUSurfaceGetCurrentTextureStatus_Timeout = 1,
    WGPUSurfaceGetCurrentTextureStatus_Outdated = 2,
    WGPUSurfaceGetCurrentTextureStatus_Lost = 3,
    WGPUSurfaceGetCurrentTextureStatus_OutOfMemory = 4,
    WGPUSurfaceGetCurrentTextureStatus_DeviceLost = 5,
    WGPUSurfaceGetCurrentTextureStatus_Force32 = 2147483647,
}
export const WGPUSurfaceGetCurrentTextureStatus_FFI = FFIType.int32_t;

export enum WGPUTextureAspect {
    WGPUTextureAspect_All = 0,
    WGPUTextureAspect_StencilOnly = 1,
    WGPUTextureAspect_DepthOnly = 2,
    WGPUTextureAspect_Force32 = 2147483647,
}
export const WGPUTextureAspect_FFI = FFIType.int32_t;

export enum WGPUTextureDimension {
    WGPUTextureDimension_1D = 0,
    WGPUTextureDimension_2D = 1,
    WGPUTextureDimension_3D = 2,
    WGPUTextureDimension_Force32 = 2147483647,
}
export const WGPUTextureDimension_FFI = FFIType.int32_t;

export enum WGPUTextureFormat {
    WGPUTextureFormat_Undefined = 0,
    WGPUTextureFormat_R8Unorm = 1,
    WGPUTextureFormat_R8Snorm = 2,
    WGPUTextureFormat_R8Uint = 3,
    WGPUTextureFormat_R8Sint = 4,
    WGPUTextureFormat_R16Uint = 5,
    WGPUTextureFormat_R16Sint = 6,
    WGPUTextureFormat_R16Float = 7,
    WGPUTextureFormat_RG8Unorm = 8,
    WGPUTextureFormat_RG8Snorm = 9,
    WGPUTextureFormat_RG8Uint = 10,
    WGPUTextureFormat_RG8Sint = 11,
    WGPUTextureFormat_R32Float = 12,
    WGPUTextureFormat_R32Uint = 13,
    WGPUTextureFormat_R32Sint = 14,
    WGPUTextureFormat_RG16Uint = 15,
    WGPUTextureFormat_RG16Sint = 16,
    WGPUTextureFormat_RG16Float = 17,
    WGPUTextureFormat_RGBA8Unorm = 18,
    WGPUTextureFormat_RGBA8UnormSrgb = 19,
    WGPUTextureFormat_RGBA8Snorm = 20,
    WGPUTextureFormat_RGBA8Uint = 21,
    WGPUTextureFormat_RGBA8Sint = 22,
    WGPUTextureFormat_BGRA8Unorm = 23,
    WGPUTextureFormat_BGRA8UnormSrgb = 24,
    WGPUTextureFormat_RGB10A2Uint = 25,
    WGPUTextureFormat_RGB10A2Unorm = 26,
    WGPUTextureFormat_RG11B10Ufloat = 27,
    WGPUTextureFormat_RGB9E5Ufloat = 28,
    WGPUTextureFormat_RG32Float = 29,
    WGPUTextureFormat_RG32Uint = 30,
    WGPUTextureFormat_RG32Sint = 31,
    WGPUTextureFormat_RGBA16Uint = 32,
    WGPUTextureFormat_RGBA16Sint = 33,
    WGPUTextureFormat_RGBA16Float = 34,
    WGPUTextureFormat_RGBA32Float = 35,
    WGPUTextureFormat_RGBA32Uint = 36,
    WGPUTextureFormat_RGBA32Sint = 37,
    WGPUTextureFormat_Stencil8 = 38,
    WGPUTextureFormat_Depth16Unorm = 39,
    WGPUTextureFormat_Depth24Plus = 40,
    WGPUTextureFormat_Depth24PlusStencil8 = 41,
    WGPUTextureFormat_Depth32Float = 42,
    WGPUTextureFormat_Depth32FloatStencil8 = 43,
    WGPUTextureFormat_BC1RGBAUnorm = 44,
    WGPUTextureFormat_BC1RGBAUnormSrgb = 45,
    WGPUTextureFormat_BC2RGBAUnorm = 46,
    WGPUTextureFormat_BC2RGBAUnormSrgb = 47,
    WGPUTextureFormat_BC3RGBAUnorm = 48,
    WGPUTextureFormat_BC3RGBAUnormSrgb = 49,
    WGPUTextureFormat_BC4RUnorm = 50,
    WGPUTextureFormat_BC4RSnorm = 51,
    WGPUTextureFormat_BC5RGUnorm = 52,
    WGPUTextureFormat_BC5RGSnorm = 53,
    WGPUTextureFormat_BC6HRGBUfloat = 54,
    WGPUTextureFormat_BC6HRGBFloat = 55,
    WGPUTextureFormat_BC7RGBAUnorm = 56,
    WGPUTextureFormat_BC7RGBAUnormSrgb = 57,
    WGPUTextureFormat_ETC2RGB8Unorm = 58,
    WGPUTextureFormat_ETC2RGB8UnormSrgb = 59,
    WGPUTextureFormat_ETC2RGB8A1Unorm = 60,
    WGPUTextureFormat_ETC2RGB8A1UnormSrgb = 61,
    WGPUTextureFormat_ETC2RGBA8Unorm = 62,
    WGPUTextureFormat_ETC2RGBA8UnormSrgb = 63,
    WGPUTextureFormat_EACR11Unorm = 64,
    WGPUTextureFormat_EACR11Snorm = 65,
    WGPUTextureFormat_EACRG11Unorm = 66,
    WGPUTextureFormat_EACRG11Snorm = 67,
    WGPUTextureFormat_ASTC4x4Unorm = 68,
    WGPUTextureFormat_ASTC4x4UnormSrgb = 69,
    WGPUTextureFormat_ASTC5x4Unorm = 70,
    WGPUTextureFormat_ASTC5x4UnormSrgb = 71,
    WGPUTextureFormat_ASTC5x5Unorm = 72,
    WGPUTextureFormat_ASTC5x5UnormSrgb = 73,
    WGPUTextureFormat_ASTC6x5Unorm = 74,
    WGPUTextureFormat_ASTC6x5UnormSrgb = 75,
    WGPUTextureFormat_ASTC6x6Unorm = 76,
    WGPUTextureFormat_ASTC6x6UnormSrgb = 77,
    WGPUTextureFormat_ASTC8x5Unorm = 78,
    WGPUTextureFormat_ASTC8x5UnormSrgb = 79,
    WGPUTextureFormat_ASTC8x6Unorm = 80,
    WGPUTextureFormat_ASTC8x6UnormSrgb = 81,
    WGPUTextureFormat_ASTC8x8Unorm = 82,
    WGPUTextureFormat_ASTC8x8UnormSrgb = 83,
    WGPUTextureFormat_ASTC10x5Unorm = 84,
    WGPUTextureFormat_ASTC10x5UnormSrgb = 85,
    WGPUTextureFormat_ASTC10x6Unorm = 86,
    WGPUTextureFormat_ASTC10x6UnormSrgb = 87,
    WGPUTextureFormat_ASTC10x8Unorm = 88,
    WGPUTextureFormat_ASTC10x8UnormSrgb = 89,
    WGPUTextureFormat_ASTC10x10Unorm = 90,
    WGPUTextureFormat_ASTC10x10UnormSrgb = 91,
    WGPUTextureFormat_ASTC12x10Unorm = 92,
    WGPUTextureFormat_ASTC12x10UnormSrgb = 93,
    WGPUTextureFormat_ASTC12x12Unorm = 94,
    WGPUTextureFormat_ASTC12x12UnormSrgb = 95,
    WGPUTextureFormat_Force32 = 2147483647,
}
export const WGPUTextureFormat_FFI = FFIType.int32_t;

export enum WGPUTextureSampleType {
    WGPUTextureSampleType_Undefined = 0,
    WGPUTextureSampleType_Float = 1,
    WGPUTextureSampleType_UnfilterableFloat = 2,
    WGPUTextureSampleType_Depth = 3,
    WGPUTextureSampleType_Sint = 4,
    WGPUTextureSampleType_Uint = 5,
    WGPUTextureSampleType_Force32 = 2147483647,
}
export const WGPUTextureSampleType_FFI = FFIType.int32_t;

export enum WGPUTextureViewDimension {
    WGPUTextureViewDimension_Undefined = 0,
    WGPUTextureViewDimension_1D = 1,
    WGPUTextureViewDimension_2D = 2,
    WGPUTextureViewDimension_2DArray = 3,
    WGPUTextureViewDimension_Cube = 4,
    WGPUTextureViewDimension_CubeArray = 5,
    WGPUTextureViewDimension_3D = 6,
    WGPUTextureViewDimension_Force32 = 2147483647,
}
export const WGPUTextureViewDimension_FFI = FFIType.int32_t;

export enum WGPUVertexFormat {
    WGPUVertexFormat_Undefined = 0,
    WGPUVertexFormat_Uint8x2 = 1,
    WGPUVertexFormat_Uint8x4 = 2,
    WGPUVertexFormat_Sint8x2 = 3,
    WGPUVertexFormat_Sint8x4 = 4,
    WGPUVertexFormat_Unorm8x2 = 5,
    WGPUVertexFormat_Unorm8x4 = 6,
    WGPUVertexFormat_Snorm8x2 = 7,
    WGPUVertexFormat_Snorm8x4 = 8,
    WGPUVertexFormat_Uint16x2 = 9,
    WGPUVertexFormat_Uint16x4 = 10,
    WGPUVertexFormat_Sint16x2 = 11,
    WGPUVertexFormat_Sint16x4 = 12,
    WGPUVertexFormat_Unorm16x2 = 13,
    WGPUVertexFormat_Unorm16x4 = 14,
    WGPUVertexFormat_Snorm16x2 = 15,
    WGPUVertexFormat_Snorm16x4 = 16,
    WGPUVertexFormat_Float16x2 = 17,
    WGPUVertexFormat_Float16x4 = 18,
    WGPUVertexFormat_Float32 = 19,
    WGPUVertexFormat_Float32x2 = 20,
    WGPUVertexFormat_Float32x3 = 21,
    WGPUVertexFormat_Float32x4 = 22,
    WGPUVertexFormat_Uint32 = 23,
    WGPUVertexFormat_Uint32x2 = 24,
    WGPUVertexFormat_Uint32x3 = 25,
    WGPUVertexFormat_Uint32x4 = 26,
    WGPUVertexFormat_Sint32 = 27,
    WGPUVertexFormat_Sint32x2 = 28,
    WGPUVertexFormat_Sint32x3 = 29,
    WGPUVertexFormat_Sint32x4 = 30,
    WGPUVertexFormat_Force32 = 2147483647,
}
export const WGPUVertexFormat_FFI = FFIType.int32_t;

export enum WGPUVertexStepMode {
    WGPUVertexStepMode_Vertex = 0,
    WGPUVertexStepMode_Instance = 1,
    WGPUVertexStepMode_VertexBufferNotUsed = 2,
    WGPUVertexStepMode_Force32 = 2147483647,
}
export const WGPUVertexStepMode_FFI = FFIType.int32_t;

export enum WGPUBufferUsage {
    WGPUBufferUsage_None = 0,
    WGPUBufferUsage_MapRead = 1,
    WGPUBufferUsage_MapWrite = 2,
    WGPUBufferUsage_CopySrc = 4,
    WGPUBufferUsage_CopyDst = 8,
    WGPUBufferUsage_Index = 16,
    WGPUBufferUsage_Vertex = 32,
    WGPUBufferUsage_Uniform = 64,
    WGPUBufferUsage_Storage = 128,
    WGPUBufferUsage_Indirect = 256,
    WGPUBufferUsage_QueryResolve = 512,
    WGPUBufferUsage_Force32 = 2147483647,
}
export const WGPUBufferUsage_FFI = FFIType.int32_t;

export type WGPUBufferUsageFlags = WGPUFlags;
export const WGPUBufferUsageFlags_FFI = WGPUFlags_FFI;
export enum WGPUColorWriteMask {
    WGPUColorWriteMask_None = 0,
    WGPUColorWriteMask_Red = 1,
    WGPUColorWriteMask_Green = 2,
    WGPUColorWriteMask_Blue = 4,
    WGPUColorWriteMask_Alpha = 8,
    WGPUColorWriteMask_All = 15,
    WGPUColorWriteMask_Force32 = 2147483647,
}
export const WGPUColorWriteMask_FFI = FFIType.int32_t;

export type WGPUColorWriteMaskFlags = WGPUFlags;
export const WGPUColorWriteMaskFlags_FFI = WGPUFlags_FFI;
export enum WGPUMapMode {
    WGPUMapMode_None = 0,
    WGPUMapMode_Read = 1,
    WGPUMapMode_Write = 2,
    WGPUMapMode_Force32 = 2147483647,
}
export const WGPUMapMode_FFI = FFIType.int32_t;

export type WGPUMapModeFlags = WGPUFlags;
export const WGPUMapModeFlags_FFI = WGPUFlags_FFI;
export enum WGPUShaderStage {
    WGPUShaderStage_None = 0,
    WGPUShaderStage_Vertex = 1,
    WGPUShaderStage_Fragment = 2,
    WGPUShaderStage_Compute = 4,
    WGPUShaderStage_Force32 = 2147483647,
}
export const WGPUShaderStage_FFI = FFIType.int32_t;

export type WGPUShaderStageFlags = WGPUFlags;
export const WGPUShaderStageFlags_FFI = WGPUFlags_FFI;
export enum WGPUTextureUsage {
    WGPUTextureUsage_None = 0,
    WGPUTextureUsage_CopySrc = 1,
    WGPUTextureUsage_CopyDst = 2,
    WGPUTextureUsage_TextureBinding = 4,
    WGPUTextureUsage_StorageBinding = 8,
    WGPUTextureUsage_RenderAttachment = 16,
    WGPUTextureUsage_Force32 = 2147483647,
}
export const WGPUTextureUsage_FFI = FFIType.int32_t;

export type WGPUTextureUsageFlags = WGPUFlags;
export const WGPUTextureUsageFlags_FFI = WGPUFlags_FFI;
export type WGPUBufferMapCallback = (
    arg0: WGPUBufferMapAsyncStatus,
    arg1: PtrT<void>
) => void;
export const WGPUBufferMapCallback_FFI = Pointer as PtrT<WGPUBufferMapCallback>;

export type WGPUCompilationInfoCallback = (
    arg0: WGPUCompilationInfoRequestStatus,
    arg1: PtrT<WGPUCompilationInfo>,
    arg2: PtrT<void>
) => void;
export const WGPUCompilationInfoCallback_FFI =
    Pointer as PtrT<WGPUCompilationInfoCallback>;

export type WGPUCreateComputePipelineAsyncCallback = (
    arg0: WGPUCreatePipelineAsyncStatus,
    arg1: WGPUComputePipeline,
    arg2: CString,
    arg3: PtrT<void>
) => void;
export const WGPUCreateComputePipelineAsyncCallback_FFI =
    Pointer as PtrT<WGPUCreateComputePipelineAsyncCallback>;

export type WGPUCreateRenderPipelineAsyncCallback = (
    arg0: WGPUCreatePipelineAsyncStatus,
    arg1: WGPURenderPipeline,
    arg2: CString,
    arg3: PtrT<void>
) => void;
export const WGPUCreateRenderPipelineAsyncCallback_FFI =
    Pointer as PtrT<WGPUCreateRenderPipelineAsyncCallback>;

export type WGPUDeviceLostCallback = (
    arg0: WGPUDeviceLostReason,
    arg1: CString,
    arg2: PtrT<void>
) => void;
export const WGPUDeviceLostCallback_FFI =
    Pointer as PtrT<WGPUDeviceLostCallback>;

export type WGPUErrorCallback = (
    arg0: WGPUErrorType,
    arg1: CString,
    arg2: PtrT<void>
) => void;
export const WGPUErrorCallback_FFI = Pointer as PtrT<WGPUErrorCallback>;

export type WGPUProc = () => void;
export const WGPUProc_FFI = Pointer as PtrT<WGPUProc>;

export type WGPUQueueWorkDoneCallback = (
    arg0: WGPUQueueWorkDoneStatus,
    arg1: PtrT<void>
) => void;
export const WGPUQueueWorkDoneCallback_FFI =
    Pointer as PtrT<WGPUQueueWorkDoneCallback>;

export type WGPURequestAdapterCallback = (
    arg0: WGPURequestAdapterStatus,
    arg1: WGPUAdapter,
    arg2: CString,
    arg3: PtrT<void>
) => void;
export const WGPURequestAdapterCallback_FFI =
    Pointer as PtrT<WGPURequestAdapterCallback>;

export type WGPURequestDeviceCallback = (
    arg0: WGPURequestDeviceStatus,
    arg1: WGPUDevice,
    arg2: CString,
    arg3: PtrT<void>
) => void;
export const WGPURequestDeviceCallback_FFI =
    Pointer as PtrT<WGPURequestDeviceCallback>;

export type WGPUChainedStruct = {
    next: ConstPtrT<WGPUChainedStruct>;
    sType: WGPUSType;
};
export const WGPUChainedStruct_FFI = Pointer as PtrT<WGPUChainedStruct>;

export type WGPUChainedStructOut = {
    next: PtrT<WGPUChainedStructOut>;
    sType: WGPUSType;
};
export const WGPUChainedStructOut_FFI = Pointer as PtrT<WGPUChainedStructOut>;

export type WGPUAdapterProperties = {
    nextInChain: PtrT<WGPUChainedStructOut>;
    vendorID: uint32_t;
    vendorName: CString;
    architecture: CString;
    deviceID: uint32_t;
    name: CString;
    driverDescription: CString;
    adapterType: WGPUAdapterType;
    backendType: WGPUBackendType;
};
export const WGPUAdapterProperties_FFI = Pointer as PtrT<WGPUAdapterProperties>;

export type WGPUBindGroupEntry = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    binding: uint32_t;
    buffer: WGPUBuffer;
    offset: uint64_t;
    size: uint64_t;
    sampler: WGPUSampler;
    textureView: WGPUTextureView;
};
export const WGPUBindGroupEntry_FFI = Pointer as PtrT<WGPUBindGroupEntry>;

export type WGPUBlendComponent = {
    operation: WGPUBlendOperation;
    srcFactor: WGPUBlendFactor;
    dstFactor: WGPUBlendFactor;
};
export const WGPUBlendComponent_FFI = Pointer as PtrT<WGPUBlendComponent>;

export type WGPUBufferBindingLayout = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    type: WGPUBufferBindingType;
    hasDynamicOffset: WGPUBool;
    minBindingSize: uint64_t;
};
export const WGPUBufferBindingLayout_FFI =
    Pointer as PtrT<WGPUBufferBindingLayout>;

export type WGPUBufferDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    usage: WGPUBufferUsageFlags;
    size: uint64_t;
    mappedAtCreation: WGPUBool;
};
export const WGPUBufferDescriptor_FFI = Pointer as PtrT<WGPUBufferDescriptor>;

export type WGPUColor = {
    r: double;
    g: double;
    b: double;
    a: double;
};
export const WGPUColor_FFI = Pointer as PtrT<WGPUColor>;

export type WGPUCommandBufferDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
};
export const WGPUCommandBufferDescriptor_FFI =
    Pointer as PtrT<WGPUCommandBufferDescriptor>;

export type WGPUCommandEncoderDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
};
export const WGPUCommandEncoderDescriptor_FFI =
    Pointer as PtrT<WGPUCommandEncoderDescriptor>;

export type WGPUCompilationMessage = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    message: CString;
    type: WGPUCompilationMessageType;
    lineNum: uint64_t;
    linePos: uint64_t;
    offset: uint64_t;
    length: uint64_t;
    utf16LinePos: uint64_t;
    utf16Offset: uint64_t;
    utf16Length: uint64_t;
};
export const WGPUCompilationMessage_FFI =
    Pointer as PtrT<WGPUCompilationMessage>;

export type WGPUComputePassTimestampWrites = {
    querySet: WGPUQuerySet;
    beginningOfPassWriteIndex: uint32_t;
    endOfPassWriteIndex: uint32_t;
};
export const WGPUComputePassTimestampWrites_FFI =
    Pointer as PtrT<WGPUComputePassTimestampWrites>;

export type WGPUConstantEntry = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    key: CString;
    value: double;
};
export const WGPUConstantEntry_FFI = Pointer as PtrT<WGPUConstantEntry>;

export type WGPUExtent3D = {
    width: uint32_t;
    height: uint32_t;
    depthOrArrayLayers: uint32_t;
};
export const WGPUExtent3D_FFI = Pointer as PtrT<WGPUExtent3D>;

export type WGPUInstanceDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
};
export const WGPUInstanceDescriptor_FFI =
    Pointer as PtrT<WGPUInstanceDescriptor>;

export type WGPULimits = {
    maxTextureDimension1D: uint32_t;
    maxTextureDimension2D: uint32_t;
    maxTextureDimension3D: uint32_t;
    maxTextureArrayLayers: uint32_t;
    maxBindGroups: uint32_t;
    maxBindGroupsPlusVertexBuffers: uint32_t;
    maxBindingsPerBindGroup: uint32_t;
    maxDynamicUniformBuffersPerPipelineLayout: uint32_t;
    maxDynamicStorageBuffersPerPipelineLayout: uint32_t;
    maxSampledTexturesPerShaderStage: uint32_t;
    maxSamplersPerShaderStage: uint32_t;
    maxStorageBuffersPerShaderStage: uint32_t;
    maxStorageTexturesPerShaderStage: uint32_t;
    maxUniformBuffersPerShaderStage: uint32_t;
    maxUniformBufferBindingSize: uint64_t;
    maxStorageBufferBindingSize: uint64_t;
    minUniformBufferOffsetAlignment: uint32_t;
    minStorageBufferOffsetAlignment: uint32_t;
    maxVertexBuffers: uint32_t;
    maxBufferSize: uint64_t;
    maxVertexAttributes: uint32_t;
    maxVertexBufferArrayStride: uint32_t;
    maxInterStageShaderComponents: uint32_t;
    maxInterStageShaderVariables: uint32_t;
    maxColorAttachments: uint32_t;
    maxColorAttachmentBytesPerSample: uint32_t;
    maxComputeWorkgroupStorageSize: uint32_t;
    maxComputeInvocationsPerWorkgroup: uint32_t;
    maxComputeWorkgroupSizeX: uint32_t;
    maxComputeWorkgroupSizeY: uint32_t;
    maxComputeWorkgroupSizeZ: uint32_t;
    maxComputeWorkgroupsPerDimension: uint32_t;
};
export const WGPULimits_FFI = Pointer as PtrT<WGPULimits>;

export type WGPUMultisampleState = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    count: uint32_t;
    mask: uint32_t;
    alphaToCoverageEnabled: WGPUBool;
};
export const WGPUMultisampleState_FFI = Pointer as PtrT<WGPUMultisampleState>;

export type WGPUOrigin3D = {
    x: uint32_t;
    y: uint32_t;
    z: uint32_t;
};
export const WGPUOrigin3D_FFI = Pointer as PtrT<WGPUOrigin3D>;

export type WGPUPipelineLayoutDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    bindGroupLayoutCount: size_t;
    bindGroupLayouts: ConstPtrT<WGPUBindGroupLayout>;
};
export const WGPUPipelineLayoutDescriptor_FFI =
    Pointer as PtrT<WGPUPipelineLayoutDescriptor>;

export type WGPUPrimitiveDepthClipControl = {
    chain: WGPUChainedStruct;
    unclippedDepth: WGPUBool;
};
export const WGPUPrimitiveDepthClipControl_FFI =
    Pointer as PtrT<WGPUPrimitiveDepthClipControl>;

export type WGPUPrimitiveState = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    topology: WGPUPrimitiveTopology;
    stripIndexFormat: WGPUIndexFormat;
    frontFace: WGPUFrontFace;
    cullMode: WGPUCullMode;
};
export const WGPUPrimitiveState_FFI = Pointer as PtrT<WGPUPrimitiveState>;

export type WGPUQuerySetDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    type: WGPUQueryType;
    count: uint32_t;
};
export const WGPUQuerySetDescriptor_FFI =
    Pointer as PtrT<WGPUQuerySetDescriptor>;

export type WGPUQueueDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
};
export const WGPUQueueDescriptor_FFI = Pointer as PtrT<WGPUQueueDescriptor>;

export type WGPURenderBundleDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
};
export const WGPURenderBundleDescriptor_FFI =
    Pointer as PtrT<WGPURenderBundleDescriptor>;

export type WGPURenderBundleEncoderDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    colorFormatCount: size_t;
    colorFormats: ConstPtrT<WGPUTextureFormat>;
    depthStencilFormat: WGPUTextureFormat;
    sampleCount: uint32_t;
    depthReadOnly: WGPUBool;
    stencilReadOnly: WGPUBool;
};
export const WGPURenderBundleEncoderDescriptor_FFI =
    Pointer as PtrT<WGPURenderBundleEncoderDescriptor>;

export type WGPURenderPassDepthStencilAttachment = {
    view: WGPUTextureView;
    depthLoadOp: WGPULoadOp;
    depthStoreOp: WGPUStoreOp;
    depthClearValue: float;
    depthReadOnly: WGPUBool;
    stencilLoadOp: WGPULoadOp;
    stencilStoreOp: WGPUStoreOp;
    stencilClearValue: uint32_t;
    stencilReadOnly: WGPUBool;
};
export const WGPURenderPassDepthStencilAttachment_FFI =
    Pointer as PtrT<WGPURenderPassDepthStencilAttachment>;

export type WGPURenderPassDescriptorMaxDrawCount = {
    chain: WGPUChainedStruct;
    maxDrawCount: uint64_t;
};
export const WGPURenderPassDescriptorMaxDrawCount_FFI =
    Pointer as PtrT<WGPURenderPassDescriptorMaxDrawCount>;

export type WGPURenderPassTimestampWrites = {
    querySet: WGPUQuerySet;
    beginningOfPassWriteIndex: uint32_t;
    endOfPassWriteIndex: uint32_t;
};
export const WGPURenderPassTimestampWrites_FFI =
    Pointer as PtrT<WGPURenderPassTimestampWrites>;

export type WGPURequestAdapterOptions = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    compatibleSurface: WGPUSurface;
    powerPreference: WGPUPowerPreference;
    backendType: WGPUBackendType;
    forceFallbackAdapter: WGPUBool;
};
export const WGPURequestAdapterOptions_FFI =
    Pointer as PtrT<WGPURequestAdapterOptions>;

export type WGPUSamplerBindingLayout = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    type: WGPUSamplerBindingType;
};
export const WGPUSamplerBindingLayout_FFI =
    Pointer as PtrT<WGPUSamplerBindingLayout>;

export type WGPUSamplerDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    addressModeU: WGPUAddressMode;
    addressModeV: WGPUAddressMode;
    addressModeW: WGPUAddressMode;
    magFilter: WGPUFilterMode;
    minFilter: WGPUFilterMode;
    mipmapFilter: WGPUMipmapFilterMode;
    lodMinClamp: float;
    lodMaxClamp: float;
    compare: WGPUCompareFunction;
    maxAnisotropy: uint16_t;
};
export const WGPUSamplerDescriptor_FFI = Pointer as PtrT<WGPUSamplerDescriptor>;

export type WGPUShaderModuleCompilationHint = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    entryPoint: CString;
    layout: WGPUPipelineLayout;
};
export const WGPUShaderModuleCompilationHint_FFI =
    Pointer as PtrT<WGPUShaderModuleCompilationHint>;

export type WGPUShaderModuleSPIRVDescriptor = {
    chain: WGPUChainedStruct;
    codeSize: uint32_t;
    code: ConstPtrT<uint32_t>;
};
export const WGPUShaderModuleSPIRVDescriptor_FFI =
    Pointer as PtrT<WGPUShaderModuleSPIRVDescriptor>;

export type WGPUShaderModuleWGSLDescriptor = {
    chain: WGPUChainedStruct;
    code: CString;
};
export const WGPUShaderModuleWGSLDescriptor_FFI =
    Pointer as PtrT<WGPUShaderModuleWGSLDescriptor>;

export type WGPUStencilFaceState = {
    compare: WGPUCompareFunction;
    failOp: WGPUStencilOperation;
    depthFailOp: WGPUStencilOperation;
    passOp: WGPUStencilOperation;
};
export const WGPUStencilFaceState_FFI = Pointer as PtrT<WGPUStencilFaceState>;

export type WGPUStorageTextureBindingLayout = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    access: WGPUStorageTextureAccess;
    format: WGPUTextureFormat;
    viewDimension: WGPUTextureViewDimension;
};
export const WGPUStorageTextureBindingLayout_FFI =
    Pointer as PtrT<WGPUStorageTextureBindingLayout>;

export type WGPUSurfaceCapabilities = {
    nextInChain: PtrT<WGPUChainedStructOut>;
    formatCount: size_t;
    formats: PtrT<WGPUTextureFormat>;
    presentModeCount: size_t;
    presentModes: PtrT<WGPUPresentMode>;
    alphaModeCount: size_t;
    alphaModes: PtrT<WGPUCompositeAlphaMode>;
};
export const WGPUSurfaceCapabilities_FFI =
    Pointer as PtrT<WGPUSurfaceCapabilities>;

export type WGPUSurfaceConfiguration = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    device: WGPUDevice;
    format: WGPUTextureFormat;
    usage: WGPUTextureUsageFlags;
    viewFormatCount: size_t;
    viewFormats: ConstPtrT<WGPUTextureFormat>;
    alphaMode: WGPUCompositeAlphaMode;
    width: uint32_t;
    height: uint32_t;
    presentMode: WGPUPresentMode;
};
export const WGPUSurfaceConfiguration_FFI =
    Pointer as PtrT<WGPUSurfaceConfiguration>;

export type WGPUSurfaceDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
};
export const WGPUSurfaceDescriptor_FFI = Pointer as PtrT<WGPUSurfaceDescriptor>;

export type WGPUSurfaceDescriptorFromAndroidNativeWindow = {
    chain: WGPUChainedStruct;
    window: PtrT<void>;
};
export const WGPUSurfaceDescriptorFromAndroidNativeWindow_FFI =
    Pointer as PtrT<WGPUSurfaceDescriptorFromAndroidNativeWindow>;

export type WGPUSurfaceDescriptorFromCanvasHTMLSelector = {
    chain: WGPUChainedStruct;
    selector: CString;
};
export const WGPUSurfaceDescriptorFromCanvasHTMLSelector_FFI =
    Pointer as PtrT<WGPUSurfaceDescriptorFromCanvasHTMLSelector>;

export type WGPUSurfaceDescriptorFromMetalLayer = {
    chain: WGPUChainedStruct;
    layer: PtrT<void>;
};
export const WGPUSurfaceDescriptorFromMetalLayer_FFI =
    Pointer as PtrT<WGPUSurfaceDescriptorFromMetalLayer>;

export type WGPUSurfaceDescriptorFromWaylandSurface = {
    chain: WGPUChainedStruct;
    display: PtrT<void>;
    surface: PtrT<void>;
};
export const WGPUSurfaceDescriptorFromWaylandSurface_FFI =
    Pointer as PtrT<WGPUSurfaceDescriptorFromWaylandSurface>;

export type WGPUSurfaceDescriptorFromWindowsHWND = {
    chain: WGPUChainedStruct;
    hinstance: PtrT<void>;
    hwnd: PtrT<void>;
};
export const WGPUSurfaceDescriptorFromWindowsHWND_FFI =
    Pointer as PtrT<WGPUSurfaceDescriptorFromWindowsHWND>;

export type WGPUSurfaceDescriptorFromXcbWindow = {
    chain: WGPUChainedStruct;
    connection: PtrT<void>;
    window: uint32_t;
};
export const WGPUSurfaceDescriptorFromXcbWindow_FFI =
    Pointer as PtrT<WGPUSurfaceDescriptorFromXcbWindow>;

export type WGPUSurfaceDescriptorFromXlibWindow = {
    chain: WGPUChainedStruct;
    display: PtrT<void>;
    window: uint64_t;
};
export const WGPUSurfaceDescriptorFromXlibWindow_FFI =
    Pointer as PtrT<WGPUSurfaceDescriptorFromXlibWindow>;

export type WGPUSurfaceTexture = {
    texture: WGPUTexture;
    suboptimal: WGPUBool;
    status: WGPUSurfaceGetCurrentTextureStatus;
};
export const WGPUSurfaceTexture_FFI = Pointer as PtrT<WGPUSurfaceTexture>;

export type WGPUTextureBindingLayout = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    sampleType: WGPUTextureSampleType;
    viewDimension: WGPUTextureViewDimension;
    multisampled: WGPUBool;
};
export const WGPUTextureBindingLayout_FFI =
    Pointer as PtrT<WGPUTextureBindingLayout>;

export type WGPUTextureDataLayout = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    offset: uint64_t;
    bytesPerRow: uint32_t;
    rowsPerImage: uint32_t;
};
export const WGPUTextureDataLayout_FFI = Pointer as PtrT<WGPUTextureDataLayout>;

export type WGPUTextureViewDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    format: WGPUTextureFormat;
    dimension: WGPUTextureViewDimension;
    baseMipLevel: uint32_t;
    mipLevelCount: uint32_t;
    baseArrayLayer: uint32_t;
    arrayLayerCount: uint32_t;
    aspect: WGPUTextureAspect;
};
export const WGPUTextureViewDescriptor_FFI =
    Pointer as PtrT<WGPUTextureViewDescriptor>;

export type WGPUVertexAttribute = {
    format: WGPUVertexFormat;
    offset: uint64_t;
    shaderLocation: uint32_t;
};
export const WGPUVertexAttribute_FFI = Pointer as PtrT<WGPUVertexAttribute>;

export type WGPUBindGroupDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    layout: WGPUBindGroupLayout;
    entryCount: size_t;
    entries: ConstPtrT<WGPUBindGroupEntry>;
};
export const WGPUBindGroupDescriptor_FFI =
    Pointer as PtrT<WGPUBindGroupDescriptor>;

export type WGPUBindGroupLayoutEntry = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    binding: uint32_t;
    visibility: WGPUShaderStageFlags;
    buffer: WGPUBufferBindingLayout;
    sampler: WGPUSamplerBindingLayout;
    texture: WGPUTextureBindingLayout;
    storageTexture: WGPUStorageTextureBindingLayout;
};
export const WGPUBindGroupLayoutEntry_FFI =
    Pointer as PtrT<WGPUBindGroupLayoutEntry>;

export type WGPUBlendState = {
    color: WGPUBlendComponent;
    alpha: WGPUBlendComponent;
};
export const WGPUBlendState_FFI = Pointer as PtrT<WGPUBlendState>;

export type WGPUCompilationInfo = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    messageCount: size_t;
    messages: ConstPtrT<WGPUCompilationMessage>;
};
export const WGPUCompilationInfo_FFI = Pointer as PtrT<WGPUCompilationInfo>;

export type WGPUComputePassDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    timestampWrites: ConstPtrT<WGPUComputePassTimestampWrites>;
};
export const WGPUComputePassDescriptor_FFI =
    Pointer as PtrT<WGPUComputePassDescriptor>;

export type WGPUDepthStencilState = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    format: WGPUTextureFormat;
    depthWriteEnabled: WGPUBool;
    depthCompare: WGPUCompareFunction;
    stencilFront: WGPUStencilFaceState;
    stencilBack: WGPUStencilFaceState;
    stencilReadMask: uint32_t;
    stencilWriteMask: uint32_t;
    depthBias: int32_t;
    depthBiasSlopeScale: float;
    depthBiasClamp: float;
};
export const WGPUDepthStencilState_FFI = Pointer as PtrT<WGPUDepthStencilState>;

export type WGPUImageCopyBuffer = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    layout: WGPUTextureDataLayout;
    buffer: WGPUBuffer;
};
export const WGPUImageCopyBuffer_FFI = Pointer as PtrT<WGPUImageCopyBuffer>;

export type WGPUImageCopyTexture = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    texture: WGPUTexture;
    mipLevel: uint32_t;
    origin: WGPUOrigin3D;
    aspect: WGPUTextureAspect;
};
export const WGPUImageCopyTexture_FFI = Pointer as PtrT<WGPUImageCopyTexture>;

export type WGPUProgrammableStageDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    module: WGPUShaderModule;
    entryPoint: CString;
    constantCount: size_t;
    constants: ConstPtrT<WGPUConstantEntry>;
};
export const WGPUProgrammableStageDescriptor_FFI =
    Pointer as PtrT<WGPUProgrammableStageDescriptor>;

export type WGPURenderPassColorAttachment = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    view: WGPUTextureView;
    resolveTarget: WGPUTextureView;
    loadOp: WGPULoadOp;
    storeOp: WGPUStoreOp;
    clearValue: WGPUColor;
};
export const WGPURenderPassColorAttachment_FFI =
    Pointer as PtrT<WGPURenderPassColorAttachment>;

export type WGPURequiredLimits = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    limits: WGPULimits;
};
export const WGPURequiredLimits_FFI = Pointer as PtrT<WGPURequiredLimits>;

export type WGPUShaderModuleDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    hintCount: size_t;
    hints: ConstPtrT<WGPUShaderModuleCompilationHint>;
};
export const WGPUShaderModuleDescriptor_FFI =
    Pointer as PtrT<WGPUShaderModuleDescriptor>;

export type WGPUSupportedLimits = {
    nextInChain: PtrT<WGPUChainedStructOut>;
    limits: WGPULimits;
};
export const WGPUSupportedLimits_FFI = Pointer as PtrT<WGPUSupportedLimits>;

export type WGPUTextureDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    usage: WGPUTextureUsageFlags;
    dimension: WGPUTextureDimension;
    size: WGPUExtent3D;
    format: WGPUTextureFormat;
    mipLevelCount: uint32_t;
    sampleCount: uint32_t;
    viewFormatCount: size_t;
    viewFormats: ConstPtrT<WGPUTextureFormat>;
};
export const WGPUTextureDescriptor_FFI = Pointer as PtrT<WGPUTextureDescriptor>;

export type WGPUVertexBufferLayout = {
    arrayStride: uint64_t;
    stepMode: WGPUVertexStepMode;
    attributeCount: size_t;
    attributes: ConstPtrT<WGPUVertexAttribute>;
};
export const WGPUVertexBufferLayout_FFI =
    Pointer as PtrT<WGPUVertexBufferLayout>;

export type WGPUBindGroupLayoutDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    entryCount: size_t;
    entries: ConstPtrT<WGPUBindGroupLayoutEntry>;
};
export const WGPUBindGroupLayoutDescriptor_FFI =
    Pointer as PtrT<WGPUBindGroupLayoutDescriptor>;

export type WGPUColorTargetState = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    format: WGPUTextureFormat;
    blend: ConstPtrT<WGPUBlendState>;
    writeMask: WGPUColorWriteMaskFlags;
};
export const WGPUColorTargetState_FFI = Pointer as PtrT<WGPUColorTargetState>;

export type WGPUComputePipelineDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    layout: WGPUPipelineLayout;
    compute: WGPUProgrammableStageDescriptor;
};
export const WGPUComputePipelineDescriptor_FFI =
    Pointer as PtrT<WGPUComputePipelineDescriptor>;

export type WGPUDeviceDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    requiredFeatureCount: size_t;
    requiredFeatures: ConstPtrT<WGPUFeatureName>;
    requiredLimits: ConstPtrT<WGPURequiredLimits>;
    defaultQueue: WGPUQueueDescriptor;
    deviceLostCallback: WGPUDeviceLostCallback;
    deviceLostUserdata: PtrT<void>;
};
export const WGPUDeviceDescriptor_FFI = Pointer as PtrT<WGPUDeviceDescriptor>;

export type WGPURenderPassDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    colorAttachmentCount: size_t;
    colorAttachments: ConstPtrT<WGPURenderPassColorAttachment>;
    depthStencilAttachment: ConstPtrT<WGPURenderPassDepthStencilAttachment>;
    occlusionQuerySet: WGPUQuerySet;
    timestampWrites: ConstPtrT<WGPURenderPassTimestampWrites>;
};
export const WGPURenderPassDescriptor_FFI =
    Pointer as PtrT<WGPURenderPassDescriptor>;

export type WGPUVertexState = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    module: WGPUShaderModule;
    entryPoint: CString;
    constantCount: size_t;
    constants: ConstPtrT<WGPUConstantEntry>;
    bufferCount: size_t;
    buffers: ConstPtrT<WGPUVertexBufferLayout>;
};
export const WGPUVertexState_FFI = Pointer as PtrT<WGPUVertexState>;

export type WGPUFragmentState = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    module: WGPUShaderModule;
    entryPoint: CString;
    constantCount: size_t;
    constants: ConstPtrT<WGPUConstantEntry>;
    targetCount: size_t;
    targets: ConstPtrT<WGPUColorTargetState>;
};
export const WGPUFragmentState_FFI = Pointer as PtrT<WGPUFragmentState>;

export type WGPURenderPipelineDescriptor = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    label: CString;
    layout: WGPUPipelineLayout;
    vertex: WGPUVertexState;
    primitive: WGPUPrimitiveState;
    depthStencil: ConstPtrT<WGPUDepthStencilState>;
    multisample: WGPUMultisampleState;
    fragment: ConstPtrT<WGPUFragmentState>;
};
export const WGPURenderPipelineDescriptor_FFI =
    Pointer as PtrT<WGPURenderPipelineDescriptor>;

export type WGPUProcCreateInstance = (
    arg0: PtrT<WGPUInstanceDescriptor>
) => WGPUInstance;
export const WGPUProcCreateInstance_FFI =
    Pointer as PtrT<WGPUProcCreateInstance>;

export type WGPUProcGetProcAddress = (
    arg0: WGPUDevice,
    arg1: CString
) => WGPUProc;
export const WGPUProcGetProcAddress_FFI =
    Pointer as PtrT<WGPUProcGetProcAddress>;

export type WGPUProcAdapterEnumerateFeatures = (
    arg0: WGPUAdapter,
    arg1: PtrT<WGPUFeatureName>
) => size_t;
export const WGPUProcAdapterEnumerateFeatures_FFI =
    Pointer as PtrT<WGPUProcAdapterEnumerateFeatures>;

export type WGPUProcAdapterGetLimits = (
    arg0: WGPUAdapter,
    arg1: PtrT<WGPUSupportedLimits>
) => WGPUBool;
export const WGPUProcAdapterGetLimits_FFI =
    Pointer as PtrT<WGPUProcAdapterGetLimits>;

export type WGPUProcAdapterGetProperties = (
    arg0: WGPUAdapter,
    arg1: PtrT<WGPUAdapterProperties>
) => void;
export const WGPUProcAdapterGetProperties_FFI =
    Pointer as PtrT<WGPUProcAdapterGetProperties>;

export type WGPUProcAdapterHasFeature = (
    arg0: WGPUAdapter,
    arg1: WGPUFeatureName
) => WGPUBool;
export const WGPUProcAdapterHasFeature_FFI =
    Pointer as PtrT<WGPUProcAdapterHasFeature>;

export type WGPUProcAdapterRequestDevice = (
    arg0: WGPUAdapter,
    arg1: PtrT<WGPUDeviceDescriptor>,
    arg2: WGPURequestDeviceCallback,
    arg3: PtrT<void>
) => void;
export const WGPUProcAdapterRequestDevice_FFI =
    Pointer as PtrT<WGPUProcAdapterRequestDevice>;

export type WGPUProcAdapterReference = (arg0: WGPUAdapter) => void;
export const WGPUProcAdapterReference_FFI =
    Pointer as PtrT<WGPUProcAdapterReference>;

export type WGPUProcAdapterRelease = (arg0: WGPUAdapter) => void;
export const WGPUProcAdapterRelease_FFI =
    Pointer as PtrT<WGPUProcAdapterRelease>;

export type WGPUProcBindGroupSetLabel = (
    arg0: WGPUBindGroup,
    arg1: CString
) => void;
export const WGPUProcBindGroupSetLabel_FFI =
    Pointer as PtrT<WGPUProcBindGroupSetLabel>;

export type WGPUProcBindGroupReference = (arg0: WGPUBindGroup) => void;
export const WGPUProcBindGroupReference_FFI =
    Pointer as PtrT<WGPUProcBindGroupReference>;

export type WGPUProcBindGroupRelease = (arg0: WGPUBindGroup) => void;
export const WGPUProcBindGroupRelease_FFI =
    Pointer as PtrT<WGPUProcBindGroupRelease>;

export type WGPUProcBindGroupLayoutSetLabel = (
    arg0: WGPUBindGroupLayout,
    arg1: CString
) => void;
export const WGPUProcBindGroupLayoutSetLabel_FFI =
    Pointer as PtrT<WGPUProcBindGroupLayoutSetLabel>;

export type WGPUProcBindGroupLayoutReference = (
    arg0: WGPUBindGroupLayout
) => void;
export const WGPUProcBindGroupLayoutReference_FFI =
    Pointer as PtrT<WGPUProcBindGroupLayoutReference>;

export type WGPUProcBindGroupLayoutRelease = (
    arg0: WGPUBindGroupLayout
) => void;
export const WGPUProcBindGroupLayoutRelease_FFI =
    Pointer as PtrT<WGPUProcBindGroupLayoutRelease>;

export type WGPUProcBufferDestroy = (arg0: WGPUBuffer) => void;
export const WGPUProcBufferDestroy_FFI = Pointer as PtrT<WGPUProcBufferDestroy>;

export type WGPUProcBufferGetConstMappedRange = (
    arg0: WGPUBuffer,
    arg1: size_t,
    arg2: size_t
) => PtrT<void>;
export const WGPUProcBufferGetConstMappedRange_FFI =
    Pointer as PtrT<WGPUProcBufferGetConstMappedRange>;

export type WGPUProcBufferGetMapState = (
    arg0: WGPUBuffer
) => WGPUBufferMapState;
export const WGPUProcBufferGetMapState_FFI =
    Pointer as PtrT<WGPUProcBufferGetMapState>;

export type WGPUProcBufferGetMappedRange = (
    arg0: WGPUBuffer,
    arg1: size_t,
    arg2: size_t
) => PtrT<void>;
export const WGPUProcBufferGetMappedRange_FFI =
    Pointer as PtrT<WGPUProcBufferGetMappedRange>;

export type WGPUProcBufferGetSize = (arg0: WGPUBuffer) => uint64_t;
export const WGPUProcBufferGetSize_FFI = Pointer as PtrT<WGPUProcBufferGetSize>;

export type WGPUProcBufferGetUsage = (arg0: WGPUBuffer) => WGPUBufferUsageFlags;
export const WGPUProcBufferGetUsage_FFI =
    Pointer as PtrT<WGPUProcBufferGetUsage>;

export type WGPUProcBufferMapAsync = (
    arg0: WGPUBuffer,
    arg1: WGPUMapModeFlags,
    arg2: size_t,
    arg3: size_t,
    arg4: WGPUBufferMapCallback,
    arg5: PtrT<void>
) => void;
export const WGPUProcBufferMapAsync_FFI =
    Pointer as PtrT<WGPUProcBufferMapAsync>;

export type WGPUProcBufferSetLabel = (arg0: WGPUBuffer, arg1: CString) => void;
export const WGPUProcBufferSetLabel_FFI =
    Pointer as PtrT<WGPUProcBufferSetLabel>;

export type WGPUProcBufferUnmap = (arg0: WGPUBuffer) => void;
export const WGPUProcBufferUnmap_FFI = Pointer as PtrT<WGPUProcBufferUnmap>;

export type WGPUProcBufferReference = (arg0: WGPUBuffer) => void;
export const WGPUProcBufferReference_FFI =
    Pointer as PtrT<WGPUProcBufferReference>;

export type WGPUProcBufferRelease = (arg0: WGPUBuffer) => void;
export const WGPUProcBufferRelease_FFI = Pointer as PtrT<WGPUProcBufferRelease>;

export type WGPUProcCommandBufferSetLabel = (
    arg0: WGPUCommandBuffer,
    arg1: CString
) => void;
export const WGPUProcCommandBufferSetLabel_FFI =
    Pointer as PtrT<WGPUProcCommandBufferSetLabel>;

export type WGPUProcCommandBufferReference = (arg0: WGPUCommandBuffer) => void;
export const WGPUProcCommandBufferReference_FFI =
    Pointer as PtrT<WGPUProcCommandBufferReference>;

export type WGPUProcCommandBufferRelease = (arg0: WGPUCommandBuffer) => void;
export const WGPUProcCommandBufferRelease_FFI =
    Pointer as PtrT<WGPUProcCommandBufferRelease>;

export type WGPUProcCommandEncoderBeginComputePass = (
    arg0: WGPUCommandEncoder,
    arg1: PtrT<WGPUComputePassDescriptor>
) => WGPUComputePassEncoder;
export const WGPUProcCommandEncoderBeginComputePass_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderBeginComputePass>;

export type WGPUProcCommandEncoderBeginRenderPass = (
    arg0: WGPUCommandEncoder,
    arg1: PtrT<WGPURenderPassDescriptor>
) => WGPURenderPassEncoder;
export const WGPUProcCommandEncoderBeginRenderPass_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderBeginRenderPass>;

export type WGPUProcCommandEncoderClearBuffer = (
    arg0: WGPUCommandEncoder,
    arg1: WGPUBuffer,
    arg2: uint64_t,
    arg3: uint64_t
) => void;
export const WGPUProcCommandEncoderClearBuffer_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderClearBuffer>;

export type WGPUProcCommandEncoderCopyBufferToBuffer = (
    arg0: WGPUCommandEncoder,
    arg1: WGPUBuffer,
    arg2: uint64_t,
    arg3: WGPUBuffer,
    arg4: uint64_t,
    arg5: uint64_t
) => void;
export const WGPUProcCommandEncoderCopyBufferToBuffer_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderCopyBufferToBuffer>;

export type WGPUProcCommandEncoderCopyBufferToTexture = (
    arg0: WGPUCommandEncoder,
    arg1: PtrT<WGPUImageCopyBuffer>,
    arg2: PtrT<WGPUImageCopyTexture>,
    arg3: PtrT<WGPUExtent3D>
) => void;
export const WGPUProcCommandEncoderCopyBufferToTexture_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderCopyBufferToTexture>;

export type WGPUProcCommandEncoderCopyTextureToBuffer = (
    arg0: WGPUCommandEncoder,
    arg1: PtrT<WGPUImageCopyTexture>,
    arg2: PtrT<WGPUImageCopyBuffer>,
    arg3: PtrT<WGPUExtent3D>
) => void;
export const WGPUProcCommandEncoderCopyTextureToBuffer_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderCopyTextureToBuffer>;

export type WGPUProcCommandEncoderCopyTextureToTexture = (
    arg0: WGPUCommandEncoder,
    arg1: PtrT<WGPUImageCopyTexture>,
    arg2: PtrT<WGPUImageCopyTexture>,
    arg3: PtrT<WGPUExtent3D>
) => void;
export const WGPUProcCommandEncoderCopyTextureToTexture_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderCopyTextureToTexture>;

export type WGPUProcCommandEncoderFinish = (
    arg0: WGPUCommandEncoder,
    arg1: PtrT<WGPUCommandBufferDescriptor>
) => WGPUCommandBuffer;
export const WGPUProcCommandEncoderFinish_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderFinish>;

export type WGPUProcCommandEncoderInsertDebugMarker = (
    arg0: WGPUCommandEncoder,
    arg1: CString
) => void;
export const WGPUProcCommandEncoderInsertDebugMarker_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderInsertDebugMarker>;

export type WGPUProcCommandEncoderPopDebugGroup = (
    arg0: WGPUCommandEncoder
) => void;
export const WGPUProcCommandEncoderPopDebugGroup_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderPopDebugGroup>;

export type WGPUProcCommandEncoderPushDebugGroup = (
    arg0: WGPUCommandEncoder,
    arg1: CString
) => void;
export const WGPUProcCommandEncoderPushDebugGroup_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderPushDebugGroup>;

export type WGPUProcCommandEncoderResolveQuerySet = (
    arg0: WGPUCommandEncoder,
    arg1: WGPUQuerySet,
    arg2: uint32_t,
    arg3: uint32_t,
    arg4: WGPUBuffer,
    arg5: uint64_t
) => void;
export const WGPUProcCommandEncoderResolveQuerySet_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderResolveQuerySet>;

export type WGPUProcCommandEncoderSetLabel = (
    arg0: WGPUCommandEncoder,
    arg1: CString
) => void;
export const WGPUProcCommandEncoderSetLabel_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderSetLabel>;

export type WGPUProcCommandEncoderWriteTimestamp = (
    arg0: WGPUCommandEncoder,
    arg1: WGPUQuerySet,
    arg2: uint32_t
) => void;
export const WGPUProcCommandEncoderWriteTimestamp_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderWriteTimestamp>;

export type WGPUProcCommandEncoderReference = (
    arg0: WGPUCommandEncoder
) => void;
export const WGPUProcCommandEncoderReference_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderReference>;

export type WGPUProcCommandEncoderRelease = (arg0: WGPUCommandEncoder) => void;
export const WGPUProcCommandEncoderRelease_FFI =
    Pointer as PtrT<WGPUProcCommandEncoderRelease>;

export type WGPUProcComputePassEncoderDispatchWorkgroups = (
    arg0: WGPUComputePassEncoder,
    arg1: uint32_t,
    arg2: uint32_t,
    arg3: uint32_t
) => void;
export const WGPUProcComputePassEncoderDispatchWorkgroups_FFI =
    Pointer as PtrT<WGPUProcComputePassEncoderDispatchWorkgroups>;

export type WGPUProcComputePassEncoderDispatchWorkgroupsIndirect = (
    arg0: WGPUComputePassEncoder,
    arg1: WGPUBuffer,
    arg2: uint64_t
) => void;
export const WGPUProcComputePassEncoderDispatchWorkgroupsIndirect_FFI =
    Pointer as PtrT<WGPUProcComputePassEncoderDispatchWorkgroupsIndirect>;

export type WGPUProcComputePassEncoderEnd = (
    arg0: WGPUComputePassEncoder
) => void;
export const WGPUProcComputePassEncoderEnd_FFI =
    Pointer as PtrT<WGPUProcComputePassEncoderEnd>;

export type WGPUProcComputePassEncoderInsertDebugMarker = (
    arg0: WGPUComputePassEncoder,
    arg1: CString
) => void;
export const WGPUProcComputePassEncoderInsertDebugMarker_FFI =
    Pointer as PtrT<WGPUProcComputePassEncoderInsertDebugMarker>;

export type WGPUProcComputePassEncoderPopDebugGroup = (
    arg0: WGPUComputePassEncoder
) => void;
export const WGPUProcComputePassEncoderPopDebugGroup_FFI =
    Pointer as PtrT<WGPUProcComputePassEncoderPopDebugGroup>;

export type WGPUProcComputePassEncoderPushDebugGroup = (
    arg0: WGPUComputePassEncoder,
    arg1: CString
) => void;
export const WGPUProcComputePassEncoderPushDebugGroup_FFI =
    Pointer as PtrT<WGPUProcComputePassEncoderPushDebugGroup>;

export type WGPUProcComputePassEncoderSetBindGroup = (
    arg0: WGPUComputePassEncoder,
    arg1: uint32_t,
    arg2: WGPUBindGroup,
    arg3: size_t,
    arg4: PtrT<uint32_t>
) => void;
export const WGPUProcComputePassEncoderSetBindGroup_FFI =
    Pointer as PtrT<WGPUProcComputePassEncoderSetBindGroup>;

export type WGPUProcComputePassEncoderSetLabel = (
    arg0: WGPUComputePassEncoder,
    arg1: CString
) => void;
export const WGPUProcComputePassEncoderSetLabel_FFI =
    Pointer as PtrT<WGPUProcComputePassEncoderSetLabel>;

export type WGPUProcComputePassEncoderSetPipeline = (
    arg0: WGPUComputePassEncoder,
    arg1: WGPUComputePipeline
) => void;
export const WGPUProcComputePassEncoderSetPipeline_FFI =
    Pointer as PtrT<WGPUProcComputePassEncoderSetPipeline>;

export type WGPUProcComputePassEncoderReference = (
    arg0: WGPUComputePassEncoder
) => void;
export const WGPUProcComputePassEncoderReference_FFI =
    Pointer as PtrT<WGPUProcComputePassEncoderReference>;

export type WGPUProcComputePassEncoderRelease = (
    arg0: WGPUComputePassEncoder
) => void;
export const WGPUProcComputePassEncoderRelease_FFI =
    Pointer as PtrT<WGPUProcComputePassEncoderRelease>;

export type WGPUProcComputePipelineGetBindGroupLayout = (
    arg0: WGPUComputePipeline,
    arg1: uint32_t
) => WGPUBindGroupLayout;
export const WGPUProcComputePipelineGetBindGroupLayout_FFI =
    Pointer as PtrT<WGPUProcComputePipelineGetBindGroupLayout>;

export type WGPUProcComputePipelineSetLabel = (
    arg0: WGPUComputePipeline,
    arg1: CString
) => void;
export const WGPUProcComputePipelineSetLabel_FFI =
    Pointer as PtrT<WGPUProcComputePipelineSetLabel>;

export type WGPUProcComputePipelineReference = (
    arg0: WGPUComputePipeline
) => void;
export const WGPUProcComputePipelineReference_FFI =
    Pointer as PtrT<WGPUProcComputePipelineReference>;

export type WGPUProcComputePipelineRelease = (
    arg0: WGPUComputePipeline
) => void;
export const WGPUProcComputePipelineRelease_FFI =
    Pointer as PtrT<WGPUProcComputePipelineRelease>;

export type WGPUProcDeviceCreateBindGroup = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUBindGroupDescriptor>
) => WGPUBindGroup;
export const WGPUProcDeviceCreateBindGroup_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateBindGroup>;

export type WGPUProcDeviceCreateBindGroupLayout = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUBindGroupLayoutDescriptor>
) => WGPUBindGroupLayout;
export const WGPUProcDeviceCreateBindGroupLayout_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateBindGroupLayout>;

export type WGPUProcDeviceCreateBuffer = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUBufferDescriptor>
) => WGPUBuffer;
export const WGPUProcDeviceCreateBuffer_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateBuffer>;

export type WGPUProcDeviceCreateCommandEncoder = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUCommandEncoderDescriptor>
) => WGPUCommandEncoder;
export const WGPUProcDeviceCreateCommandEncoder_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateCommandEncoder>;

export type WGPUProcDeviceCreateComputePipeline = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUComputePipelineDescriptor>
) => WGPUComputePipeline;
export const WGPUProcDeviceCreateComputePipeline_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateComputePipeline>;

export type WGPUProcDeviceCreateComputePipelineAsync = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUComputePipelineDescriptor>,
    arg2: WGPUCreateComputePipelineAsyncCallback,
    arg3: PtrT<void>
) => void;
export const WGPUProcDeviceCreateComputePipelineAsync_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateComputePipelineAsync>;

export type WGPUProcDeviceCreatePipelineLayout = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUPipelineLayoutDescriptor>
) => WGPUPipelineLayout;
export const WGPUProcDeviceCreatePipelineLayout_FFI =
    Pointer as PtrT<WGPUProcDeviceCreatePipelineLayout>;

export type WGPUProcDeviceCreateQuerySet = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUQuerySetDescriptor>
) => WGPUQuerySet;
export const WGPUProcDeviceCreateQuerySet_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateQuerySet>;

export type WGPUProcDeviceCreateRenderBundleEncoder = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPURenderBundleEncoderDescriptor>
) => WGPURenderBundleEncoder;
export const WGPUProcDeviceCreateRenderBundleEncoder_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateRenderBundleEncoder>;

export type WGPUProcDeviceCreateRenderPipeline = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPURenderPipelineDescriptor>
) => WGPURenderPipeline;
export const WGPUProcDeviceCreateRenderPipeline_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateRenderPipeline>;

export type WGPUProcDeviceCreateRenderPipelineAsync = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPURenderPipelineDescriptor>,
    arg2: WGPUCreateRenderPipelineAsyncCallback,
    arg3: PtrT<void>
) => void;
export const WGPUProcDeviceCreateRenderPipelineAsync_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateRenderPipelineAsync>;

export type WGPUProcDeviceCreateSampler = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUSamplerDescriptor>
) => WGPUSampler;
export const WGPUProcDeviceCreateSampler_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateSampler>;

export type WGPUProcDeviceCreateShaderModule = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUShaderModuleDescriptor>
) => WGPUShaderModule;
export const WGPUProcDeviceCreateShaderModule_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateShaderModule>;

export type WGPUProcDeviceCreateTexture = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUTextureDescriptor>
) => WGPUTexture;
export const WGPUProcDeviceCreateTexture_FFI =
    Pointer as PtrT<WGPUProcDeviceCreateTexture>;

export type WGPUProcDeviceDestroy = (arg0: WGPUDevice) => void;
export const WGPUProcDeviceDestroy_FFI = Pointer as PtrT<WGPUProcDeviceDestroy>;

export type WGPUProcDeviceEnumerateFeatures = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUFeatureName>
) => size_t;
export const WGPUProcDeviceEnumerateFeatures_FFI =
    Pointer as PtrT<WGPUProcDeviceEnumerateFeatures>;

export type WGPUProcDeviceGetLimits = (
    arg0: WGPUDevice,
    arg1: PtrT<WGPUSupportedLimits>
) => WGPUBool;
export const WGPUProcDeviceGetLimits_FFI =
    Pointer as PtrT<WGPUProcDeviceGetLimits>;

export type WGPUProcDeviceGetQueue = (arg0: WGPUDevice) => WGPUQueue;
export const WGPUProcDeviceGetQueue_FFI =
    Pointer as PtrT<WGPUProcDeviceGetQueue>;

export type WGPUProcDeviceHasFeature = (
    arg0: WGPUDevice,
    arg1: WGPUFeatureName
) => WGPUBool;
export const WGPUProcDeviceHasFeature_FFI =
    Pointer as PtrT<WGPUProcDeviceHasFeature>;

export type WGPUProcDevicePopErrorScope = (
    arg0: WGPUDevice,
    arg1: WGPUErrorCallback,
    arg2: PtrT<void>
) => void;
export const WGPUProcDevicePopErrorScope_FFI =
    Pointer as PtrT<WGPUProcDevicePopErrorScope>;

export type WGPUProcDevicePushErrorScope = (
    arg0: WGPUDevice,
    arg1: WGPUErrorFilter
) => void;
export const WGPUProcDevicePushErrorScope_FFI =
    Pointer as PtrT<WGPUProcDevicePushErrorScope>;

export type WGPUProcDeviceSetLabel = (arg0: WGPUDevice, arg1: CString) => void;
export const WGPUProcDeviceSetLabel_FFI =
    Pointer as PtrT<WGPUProcDeviceSetLabel>;

export type WGPUProcDeviceSetUncapturedErrorCallback = (
    arg0: WGPUDevice,
    arg1: WGPUErrorCallback,
    arg2: PtrT<void>
) => void;
export const WGPUProcDeviceSetUncapturedErrorCallback_FFI =
    Pointer as PtrT<WGPUProcDeviceSetUncapturedErrorCallback>;

export type WGPUProcDeviceReference = (arg0: WGPUDevice) => void;
export const WGPUProcDeviceReference_FFI =
    Pointer as PtrT<WGPUProcDeviceReference>;

export type WGPUProcDeviceRelease = (arg0: WGPUDevice) => void;
export const WGPUProcDeviceRelease_FFI = Pointer as PtrT<WGPUProcDeviceRelease>;

export type WGPUProcInstanceCreateSurface = (
    arg0: WGPUInstance,
    arg1: PtrT<WGPUSurfaceDescriptor>
) => WGPUSurface;
export const WGPUProcInstanceCreateSurface_FFI =
    Pointer as PtrT<WGPUProcInstanceCreateSurface>;

export type WGPUProcInstanceProcessEvents = (arg0: WGPUInstance) => void;
export const WGPUProcInstanceProcessEvents_FFI =
    Pointer as PtrT<WGPUProcInstanceProcessEvents>;

export type WGPUProcInstanceRequestAdapter = (
    arg0: WGPUInstance,
    arg1: PtrT<WGPURequestAdapterOptions>,
    arg2: WGPURequestAdapterCallback,
    arg3: PtrT<void>
) => void;
export const WGPUProcInstanceRequestAdapter_FFI =
    Pointer as PtrT<WGPUProcInstanceRequestAdapter>;

export type WGPUProcInstanceReference = (arg0: WGPUInstance) => void;
export const WGPUProcInstanceReference_FFI =
    Pointer as PtrT<WGPUProcInstanceReference>;

export type WGPUProcInstanceRelease = (arg0: WGPUInstance) => void;
export const WGPUProcInstanceRelease_FFI =
    Pointer as PtrT<WGPUProcInstanceRelease>;

export type WGPUProcPipelineLayoutSetLabel = (
    arg0: WGPUPipelineLayout,
    arg1: CString
) => void;
export const WGPUProcPipelineLayoutSetLabel_FFI =
    Pointer as PtrT<WGPUProcPipelineLayoutSetLabel>;

export type WGPUProcPipelineLayoutReference = (
    arg0: WGPUPipelineLayout
) => void;
export const WGPUProcPipelineLayoutReference_FFI =
    Pointer as PtrT<WGPUProcPipelineLayoutReference>;

export type WGPUProcPipelineLayoutRelease = (arg0: WGPUPipelineLayout) => void;
export const WGPUProcPipelineLayoutRelease_FFI =
    Pointer as PtrT<WGPUProcPipelineLayoutRelease>;

export type WGPUProcQuerySetDestroy = (arg0: WGPUQuerySet) => void;
export const WGPUProcQuerySetDestroy_FFI =
    Pointer as PtrT<WGPUProcQuerySetDestroy>;

export type WGPUProcQuerySetGetCount = (arg0: WGPUQuerySet) => uint32_t;
export const WGPUProcQuerySetGetCount_FFI =
    Pointer as PtrT<WGPUProcQuerySetGetCount>;

export type WGPUProcQuerySetGetType = (arg0: WGPUQuerySet) => WGPUQueryType;
export const WGPUProcQuerySetGetType_FFI =
    Pointer as PtrT<WGPUProcQuerySetGetType>;

export type WGPUProcQuerySetSetLabel = (
    arg0: WGPUQuerySet,
    arg1: CString
) => void;
export const WGPUProcQuerySetSetLabel_FFI =
    Pointer as PtrT<WGPUProcQuerySetSetLabel>;

export type WGPUProcQuerySetReference = (arg0: WGPUQuerySet) => void;
export const WGPUProcQuerySetReference_FFI =
    Pointer as PtrT<WGPUProcQuerySetReference>;

export type WGPUProcQuerySetRelease = (arg0: WGPUQuerySet) => void;
export const WGPUProcQuerySetRelease_FFI =
    Pointer as PtrT<WGPUProcQuerySetRelease>;

export type WGPUProcQueueOnSubmittedWorkDone = (
    arg0: WGPUQueue,
    arg1: WGPUQueueWorkDoneCallback,
    arg2: PtrT<void>
) => void;
export const WGPUProcQueueOnSubmittedWorkDone_FFI =
    Pointer as PtrT<WGPUProcQueueOnSubmittedWorkDone>;

export type WGPUProcQueueSetLabel = (arg0: WGPUQueue, arg1: CString) => void;
export const WGPUProcQueueSetLabel_FFI = Pointer as PtrT<WGPUProcQueueSetLabel>;

export type WGPUProcQueueSubmit = (
    arg0: WGPUQueue,
    arg1: size_t,
    arg2: PtrT<WGPUCommandBuffer>
) => void;
export const WGPUProcQueueSubmit_FFI = Pointer as PtrT<WGPUProcQueueSubmit>;

export type WGPUProcQueueWriteBuffer = (
    arg0: WGPUQueue,
    arg1: WGPUBuffer,
    arg2: uint64_t,
    arg3: PtrT<void>,
    arg4: size_t
) => void;
export const WGPUProcQueueWriteBuffer_FFI =
    Pointer as PtrT<WGPUProcQueueWriteBuffer>;

export type WGPUProcQueueWriteTexture = (
    arg0: WGPUQueue,
    arg1: PtrT<WGPUImageCopyTexture>,
    arg2: PtrT<void>,
    arg3: size_t,
    arg4: PtrT<WGPUTextureDataLayout>,
    arg5: PtrT<WGPUExtent3D>
) => void;
export const WGPUProcQueueWriteTexture_FFI =
    Pointer as PtrT<WGPUProcQueueWriteTexture>;

export type WGPUProcQueueReference = (arg0: WGPUQueue) => void;
export const WGPUProcQueueReference_FFI =
    Pointer as PtrT<WGPUProcQueueReference>;

export type WGPUProcQueueRelease = (arg0: WGPUQueue) => void;
export const WGPUProcQueueRelease_FFI = Pointer as PtrT<WGPUProcQueueRelease>;

export type WGPUProcRenderBundleSetLabel = (
    arg0: WGPURenderBundle,
    arg1: CString
) => void;
export const WGPUProcRenderBundleSetLabel_FFI =
    Pointer as PtrT<WGPUProcRenderBundleSetLabel>;

export type WGPUProcRenderBundleReference = (arg0: WGPURenderBundle) => void;
export const WGPUProcRenderBundleReference_FFI =
    Pointer as PtrT<WGPUProcRenderBundleReference>;

export type WGPUProcRenderBundleRelease = (arg0: WGPURenderBundle) => void;
export const WGPUProcRenderBundleRelease_FFI =
    Pointer as PtrT<WGPUProcRenderBundleRelease>;

export type WGPUProcRenderBundleEncoderDraw = (
    arg0: WGPURenderBundleEncoder,
    arg1: uint32_t,
    arg2: uint32_t,
    arg3: uint32_t,
    arg4: uint32_t
) => void;
export const WGPUProcRenderBundleEncoderDraw_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderDraw>;

export type WGPUProcRenderBundleEncoderDrawIndexed = (
    arg0: WGPURenderBundleEncoder,
    arg1: uint32_t,
    arg2: uint32_t,
    arg3: uint32_t,
    arg4: int32_t,
    arg5: uint32_t
) => void;
export const WGPUProcRenderBundleEncoderDrawIndexed_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderDrawIndexed>;

export type WGPUProcRenderBundleEncoderDrawIndexedIndirect = (
    arg0: WGPURenderBundleEncoder,
    arg1: WGPUBuffer,
    arg2: uint64_t
) => void;
export const WGPUProcRenderBundleEncoderDrawIndexedIndirect_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderDrawIndexedIndirect>;

export type WGPUProcRenderBundleEncoderDrawIndirect = (
    arg0: WGPURenderBundleEncoder,
    arg1: WGPUBuffer,
    arg2: uint64_t
) => void;
export const WGPUProcRenderBundleEncoderDrawIndirect_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderDrawIndirect>;

export type WGPUProcRenderBundleEncoderFinish = (
    arg0: WGPURenderBundleEncoder,
    arg1: PtrT<WGPURenderBundleDescriptor>
) => WGPURenderBundle;
export const WGPUProcRenderBundleEncoderFinish_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderFinish>;

export type WGPUProcRenderBundleEncoderInsertDebugMarker = (
    arg0: WGPURenderBundleEncoder,
    arg1: CString
) => void;
export const WGPUProcRenderBundleEncoderInsertDebugMarker_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderInsertDebugMarker>;

export type WGPUProcRenderBundleEncoderPopDebugGroup = (
    arg0: WGPURenderBundleEncoder
) => void;
export const WGPUProcRenderBundleEncoderPopDebugGroup_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderPopDebugGroup>;

export type WGPUProcRenderBundleEncoderPushDebugGroup = (
    arg0: WGPURenderBundleEncoder,
    arg1: CString
) => void;
export const WGPUProcRenderBundleEncoderPushDebugGroup_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderPushDebugGroup>;

export type WGPUProcRenderBundleEncoderSetBindGroup = (
    arg0: WGPURenderBundleEncoder,
    arg1: uint32_t,
    arg2: WGPUBindGroup,
    arg3: size_t,
    arg4: PtrT<uint32_t>
) => void;
export const WGPUProcRenderBundleEncoderSetBindGroup_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderSetBindGroup>;

export type WGPUProcRenderBundleEncoderSetIndexBuffer = (
    arg0: WGPURenderBundleEncoder,
    arg1: WGPUBuffer,
    arg2: WGPUIndexFormat,
    arg3: uint64_t,
    arg4: uint64_t
) => void;
export const WGPUProcRenderBundleEncoderSetIndexBuffer_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderSetIndexBuffer>;

export type WGPUProcRenderBundleEncoderSetLabel = (
    arg0: WGPURenderBundleEncoder,
    arg1: CString
) => void;
export const WGPUProcRenderBundleEncoderSetLabel_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderSetLabel>;

export type WGPUProcRenderBundleEncoderSetPipeline = (
    arg0: WGPURenderBundleEncoder,
    arg1: WGPURenderPipeline
) => void;
export const WGPUProcRenderBundleEncoderSetPipeline_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderSetPipeline>;

export type WGPUProcRenderBundleEncoderSetVertexBuffer = (
    arg0: WGPURenderBundleEncoder,
    arg1: uint32_t,
    arg2: WGPUBuffer,
    arg3: uint64_t,
    arg4: uint64_t
) => void;
export const WGPUProcRenderBundleEncoderSetVertexBuffer_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderSetVertexBuffer>;

export type WGPUProcRenderBundleEncoderReference = (
    arg0: WGPURenderBundleEncoder
) => void;
export const WGPUProcRenderBundleEncoderReference_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderReference>;

export type WGPUProcRenderBundleEncoderRelease = (
    arg0: WGPURenderBundleEncoder
) => void;
export const WGPUProcRenderBundleEncoderRelease_FFI =
    Pointer as PtrT<WGPUProcRenderBundleEncoderRelease>;

export type WGPUProcRenderPassEncoderBeginOcclusionQuery = (
    arg0: WGPURenderPassEncoder,
    arg1: uint32_t
) => void;
export const WGPUProcRenderPassEncoderBeginOcclusionQuery_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderBeginOcclusionQuery>;

export type WGPUProcRenderPassEncoderDraw = (
    arg0: WGPURenderPassEncoder,
    arg1: uint32_t,
    arg2: uint32_t,
    arg3: uint32_t,
    arg4: uint32_t
) => void;
export const WGPUProcRenderPassEncoderDraw_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderDraw>;

export type WGPUProcRenderPassEncoderDrawIndexed = (
    arg0: WGPURenderPassEncoder,
    arg1: uint32_t,
    arg2: uint32_t,
    arg3: uint32_t,
    arg4: int32_t,
    arg5: uint32_t
) => void;
export const WGPUProcRenderPassEncoderDrawIndexed_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderDrawIndexed>;

export type WGPUProcRenderPassEncoderDrawIndexedIndirect = (
    arg0: WGPURenderPassEncoder,
    arg1: WGPUBuffer,
    arg2: uint64_t
) => void;
export const WGPUProcRenderPassEncoderDrawIndexedIndirect_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderDrawIndexedIndirect>;

export type WGPUProcRenderPassEncoderDrawIndirect = (
    arg0: WGPURenderPassEncoder,
    arg1: WGPUBuffer,
    arg2: uint64_t
) => void;
export const WGPUProcRenderPassEncoderDrawIndirect_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderDrawIndirect>;

export type WGPUProcRenderPassEncoderEnd = (
    arg0: WGPURenderPassEncoder
) => void;
export const WGPUProcRenderPassEncoderEnd_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderEnd>;

export type WGPUProcRenderPassEncoderEndOcclusionQuery = (
    arg0: WGPURenderPassEncoder
) => void;
export const WGPUProcRenderPassEncoderEndOcclusionQuery_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderEndOcclusionQuery>;

export type WGPUProcRenderPassEncoderExecuteBundles = (
    arg0: WGPURenderPassEncoder,
    arg1: size_t,
    arg2: PtrT<WGPURenderBundle>
) => void;
export const WGPUProcRenderPassEncoderExecuteBundles_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderExecuteBundles>;

export type WGPUProcRenderPassEncoderInsertDebugMarker = (
    arg0: WGPURenderPassEncoder,
    arg1: CString
) => void;
export const WGPUProcRenderPassEncoderInsertDebugMarker_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderInsertDebugMarker>;

export type WGPUProcRenderPassEncoderPopDebugGroup = (
    arg0: WGPURenderPassEncoder
) => void;
export const WGPUProcRenderPassEncoderPopDebugGroup_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderPopDebugGroup>;

export type WGPUProcRenderPassEncoderPushDebugGroup = (
    arg0: WGPURenderPassEncoder,
    arg1: CString
) => void;
export const WGPUProcRenderPassEncoderPushDebugGroup_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderPushDebugGroup>;

export type WGPUProcRenderPassEncoderSetBindGroup = (
    arg0: WGPURenderPassEncoder,
    arg1: uint32_t,
    arg2: WGPUBindGroup,
    arg3: size_t,
    arg4: PtrT<uint32_t>
) => void;
export const WGPUProcRenderPassEncoderSetBindGroup_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderSetBindGroup>;

export type WGPUProcRenderPassEncoderSetBlendConstant = (
    arg0: WGPURenderPassEncoder,
    arg1: PtrT<WGPUColor>
) => void;
export const WGPUProcRenderPassEncoderSetBlendConstant_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderSetBlendConstant>;

export type WGPUProcRenderPassEncoderSetIndexBuffer = (
    arg0: WGPURenderPassEncoder,
    arg1: WGPUBuffer,
    arg2: WGPUIndexFormat,
    arg3: uint64_t,
    arg4: uint64_t
) => void;
export const WGPUProcRenderPassEncoderSetIndexBuffer_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderSetIndexBuffer>;

export type WGPUProcRenderPassEncoderSetLabel = (
    arg0: WGPURenderPassEncoder,
    arg1: CString
) => void;
export const WGPUProcRenderPassEncoderSetLabel_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderSetLabel>;

export type WGPUProcRenderPassEncoderSetPipeline = (
    arg0: WGPURenderPassEncoder,
    arg1: WGPURenderPipeline
) => void;
export const WGPUProcRenderPassEncoderSetPipeline_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderSetPipeline>;

export type WGPUProcRenderPassEncoderSetScissorRect = (
    arg0: WGPURenderPassEncoder,
    arg1: uint32_t,
    arg2: uint32_t,
    arg3: uint32_t,
    arg4: uint32_t
) => void;
export const WGPUProcRenderPassEncoderSetScissorRect_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderSetScissorRect>;

export type WGPUProcRenderPassEncoderSetStencilReference = (
    arg0: WGPURenderPassEncoder,
    arg1: uint32_t
) => void;
export const WGPUProcRenderPassEncoderSetStencilReference_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderSetStencilReference>;

export type WGPUProcRenderPassEncoderSetVertexBuffer = (
    arg0: WGPURenderPassEncoder,
    arg1: uint32_t,
    arg2: WGPUBuffer,
    arg3: uint64_t,
    arg4: uint64_t
) => void;
export const WGPUProcRenderPassEncoderSetVertexBuffer_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderSetVertexBuffer>;

export type WGPUProcRenderPassEncoderSetViewport = (
    arg0: WGPURenderPassEncoder,
    arg1: float,
    arg2: float,
    arg3: float,
    arg4: float,
    arg5: float,
    arg6: float
) => void;
export const WGPUProcRenderPassEncoderSetViewport_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderSetViewport>;

export type WGPUProcRenderPassEncoderReference = (
    arg0: WGPURenderPassEncoder
) => void;
export const WGPUProcRenderPassEncoderReference_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderReference>;

export type WGPUProcRenderPassEncoderRelease = (
    arg0: WGPURenderPassEncoder
) => void;
export const WGPUProcRenderPassEncoderRelease_FFI =
    Pointer as PtrT<WGPUProcRenderPassEncoderRelease>;

export type WGPUProcRenderPipelineGetBindGroupLayout = (
    arg0: WGPURenderPipeline,
    arg1: uint32_t
) => WGPUBindGroupLayout;
export const WGPUProcRenderPipelineGetBindGroupLayout_FFI =
    Pointer as PtrT<WGPUProcRenderPipelineGetBindGroupLayout>;

export type WGPUProcRenderPipelineSetLabel = (
    arg0: WGPURenderPipeline,
    arg1: CString
) => void;
export const WGPUProcRenderPipelineSetLabel_FFI =
    Pointer as PtrT<WGPUProcRenderPipelineSetLabel>;

export type WGPUProcRenderPipelineReference = (
    arg0: WGPURenderPipeline
) => void;
export const WGPUProcRenderPipelineReference_FFI =
    Pointer as PtrT<WGPUProcRenderPipelineReference>;

export type WGPUProcRenderPipelineRelease = (arg0: WGPURenderPipeline) => void;
export const WGPUProcRenderPipelineRelease_FFI =
    Pointer as PtrT<WGPUProcRenderPipelineRelease>;

export type WGPUProcSamplerSetLabel = (
    arg0: WGPUSampler,
    arg1: CString
) => void;
export const WGPUProcSamplerSetLabel_FFI =
    Pointer as PtrT<WGPUProcSamplerSetLabel>;

export type WGPUProcSamplerReference = (arg0: WGPUSampler) => void;
export const WGPUProcSamplerReference_FFI =
    Pointer as PtrT<WGPUProcSamplerReference>;

export type WGPUProcSamplerRelease = (arg0: WGPUSampler) => void;
export const WGPUProcSamplerRelease_FFI =
    Pointer as PtrT<WGPUProcSamplerRelease>;

export type WGPUProcShaderModuleGetCompilationInfo = (
    arg0: WGPUShaderModule,
    arg1: WGPUCompilationInfoCallback,
    arg2: PtrT<void>
) => void;
export const WGPUProcShaderModuleGetCompilationInfo_FFI =
    Pointer as PtrT<WGPUProcShaderModuleGetCompilationInfo>;

export type WGPUProcShaderModuleSetLabel = (
    arg0: WGPUShaderModule,
    arg1: CString
) => void;
export const WGPUProcShaderModuleSetLabel_FFI =
    Pointer as PtrT<WGPUProcShaderModuleSetLabel>;

export type WGPUProcShaderModuleReference = (arg0: WGPUShaderModule) => void;
export const WGPUProcShaderModuleReference_FFI =
    Pointer as PtrT<WGPUProcShaderModuleReference>;

export type WGPUProcShaderModuleRelease = (arg0: WGPUShaderModule) => void;
export const WGPUProcShaderModuleRelease_FFI =
    Pointer as PtrT<WGPUProcShaderModuleRelease>;

export type WGPUProcSurfaceConfigure = (
    arg0: WGPUSurface,
    arg1: PtrT<WGPUSurfaceConfiguration>
) => void;
export const WGPUProcSurfaceConfigure_FFI =
    Pointer as PtrT<WGPUProcSurfaceConfigure>;

export type WGPUProcSurfaceGetCapabilities = (
    arg0: WGPUSurface,
    arg1: WGPUAdapter,
    arg2: PtrT<WGPUSurfaceCapabilities>
) => void;
export const WGPUProcSurfaceGetCapabilities_FFI =
    Pointer as PtrT<WGPUProcSurfaceGetCapabilities>;

export type WGPUProcSurfaceGetCurrentTexture = (
    arg0: WGPUSurface,
    arg1: PtrT<WGPUSurfaceTexture>
) => void;
export const WGPUProcSurfaceGetCurrentTexture_FFI =
    Pointer as PtrT<WGPUProcSurfaceGetCurrentTexture>;

export type WGPUProcSurfaceGetPreferredFormat = (
    arg0: WGPUSurface,
    arg1: WGPUAdapter
) => WGPUTextureFormat;
export const WGPUProcSurfaceGetPreferredFormat_FFI =
    Pointer as PtrT<WGPUProcSurfaceGetPreferredFormat>;

export type WGPUProcSurfacePresent = (arg0: WGPUSurface) => void;
export const WGPUProcSurfacePresent_FFI =
    Pointer as PtrT<WGPUProcSurfacePresent>;

export type WGPUProcSurfaceUnconfigure = (arg0: WGPUSurface) => void;
export const WGPUProcSurfaceUnconfigure_FFI =
    Pointer as PtrT<WGPUProcSurfaceUnconfigure>;

export type WGPUProcSurfaceReference = (arg0: WGPUSurface) => void;
export const WGPUProcSurfaceReference_FFI =
    Pointer as PtrT<WGPUProcSurfaceReference>;

export type WGPUProcSurfaceRelease = (arg0: WGPUSurface) => void;
export const WGPUProcSurfaceRelease_FFI =
    Pointer as PtrT<WGPUProcSurfaceRelease>;

export type WGPUProcSurfaceCapabilitiesFreeMembers = (
    arg0: WGPUSurfaceCapabilities
) => void;
export const WGPUProcSurfaceCapabilitiesFreeMembers_FFI =
    Pointer as PtrT<WGPUProcSurfaceCapabilitiesFreeMembers>;

export type WGPUProcTextureCreateView = (
    arg0: WGPUTexture,
    arg1: PtrT<WGPUTextureViewDescriptor>
) => WGPUTextureView;
export const WGPUProcTextureCreateView_FFI =
    Pointer as PtrT<WGPUProcTextureCreateView>;

export type WGPUProcTextureDestroy = (arg0: WGPUTexture) => void;
export const WGPUProcTextureDestroy_FFI =
    Pointer as PtrT<WGPUProcTextureDestroy>;

export type WGPUProcTextureGetDepthOrArrayLayers = (
    arg0: WGPUTexture
) => uint32_t;
export const WGPUProcTextureGetDepthOrArrayLayers_FFI =
    Pointer as PtrT<WGPUProcTextureGetDepthOrArrayLayers>;

export type WGPUProcTextureGetDimension = (
    arg0: WGPUTexture
) => WGPUTextureDimension;
export const WGPUProcTextureGetDimension_FFI =
    Pointer as PtrT<WGPUProcTextureGetDimension>;

export type WGPUProcTextureGetFormat = (arg0: WGPUTexture) => WGPUTextureFormat;
export const WGPUProcTextureGetFormat_FFI =
    Pointer as PtrT<WGPUProcTextureGetFormat>;

export type WGPUProcTextureGetHeight = (arg0: WGPUTexture) => uint32_t;
export const WGPUProcTextureGetHeight_FFI =
    Pointer as PtrT<WGPUProcTextureGetHeight>;

export type WGPUProcTextureGetMipLevelCount = (arg0: WGPUTexture) => uint32_t;
export const WGPUProcTextureGetMipLevelCount_FFI =
    Pointer as PtrT<WGPUProcTextureGetMipLevelCount>;

export type WGPUProcTextureGetSampleCount = (arg0: WGPUTexture) => uint32_t;
export const WGPUProcTextureGetSampleCount_FFI =
    Pointer as PtrT<WGPUProcTextureGetSampleCount>;

export type WGPUProcTextureGetUsage = (
    arg0: WGPUTexture
) => WGPUTextureUsageFlags;
export const WGPUProcTextureGetUsage_FFI =
    Pointer as PtrT<WGPUProcTextureGetUsage>;

export type WGPUProcTextureGetWidth = (arg0: WGPUTexture) => uint32_t;
export const WGPUProcTextureGetWidth_FFI =
    Pointer as PtrT<WGPUProcTextureGetWidth>;

export type WGPUProcTextureSetLabel = (
    arg0: WGPUTexture,
    arg1: CString
) => void;
export const WGPUProcTextureSetLabel_FFI =
    Pointer as PtrT<WGPUProcTextureSetLabel>;

export type WGPUProcTextureReference = (arg0: WGPUTexture) => void;
export const WGPUProcTextureReference_FFI =
    Pointer as PtrT<WGPUProcTextureReference>;

export type WGPUProcTextureRelease = (arg0: WGPUTexture) => void;
export const WGPUProcTextureRelease_FFI =
    Pointer as PtrT<WGPUProcTextureRelease>;

export type WGPUProcTextureViewSetLabel = (
    arg0: WGPUTextureView,
    arg1: CString
) => void;
export const WGPUProcTextureViewSetLabel_FFI =
    Pointer as PtrT<WGPUProcTextureViewSetLabel>;

export type WGPUProcTextureViewReference = (arg0: WGPUTextureView) => void;
export const WGPUProcTextureViewReference_FFI =
    Pointer as PtrT<WGPUProcTextureViewReference>;

export type WGPUProcTextureViewRelease = (arg0: WGPUTextureView) => void;
export const WGPUProcTextureViewRelease_FFI =
    Pointer as PtrT<WGPUProcTextureViewRelease>;

export function wgpuCreateInstance(
    descriptor:
        | ConstPtrT<WGPUInstanceDescriptor>
        | DeepPartial<WGPUInstanceDescriptor>
): WGPUInstance {
    let arg0;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg0 = writeWGPUInstanceDescriptor(descriptor as any).typed as any;
    } else {
        arg0 = descriptor as any;
    }

    const result = wgpulib.wgpuCreateInstance(arg0!);
    return result as any;
}

export function wgpuGetProcAddress(
    device: WGPUDevice,
    procName: CString
): WGPUProc {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1 = procName;
    const result = wgpulib.wgpuGetProcAddress(arg0!, arg1!);
    return result as any;
}

export function wgpuAdapterEnumerateFeatures(
    adapter: WGPUAdapter,
    features: PtrT<WGPUFeatureName>
): size_t {
    let arg0;

    if (
        adapter &&
        typeof adapter === "object" &&
        !("BYTES_PER_ELEMENT" in adapter) &&
        // @ts-ignore
        WGPUAdapter_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = adapter as any;
    }

    let arg1;

    if (
        features &&
        typeof features === "object" &&
        !("BYTES_PER_ELEMENT" in features) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = features;
    } else {
        arg1 = features as any;
    }

    const result = wgpulib.wgpuAdapterEnumerateFeatures(arg0!, arg1!);
    return result as any;
}

export function wgpuAdapterGetLimits(
    adapter: WGPUAdapter,
    limits: PtrT<WGPUSupportedLimits>
): WGPUBool {
    let arg0;

    if (
        adapter &&
        typeof adapter === "object" &&
        !("BYTES_PER_ELEMENT" in adapter) &&
        // @ts-ignore
        WGPUAdapter_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = adapter as any;
    }

    let arg1;

    if (
        limits &&
        typeof limits === "object" &&
        !("BYTES_PER_ELEMENT" in limits) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = limits;
    } else {
        arg1 = limits as any;
    }

    const result = wgpulib.wgpuAdapterGetLimits(arg0!, arg1!);
    return result as any;
}

export function wgpuAdapterGetProperties(
    adapter: WGPUAdapter,
    properties: PtrT<WGPUAdapterProperties>
): void {
    let arg0;

    if (
        adapter &&
        typeof adapter === "object" &&
        !("BYTES_PER_ELEMENT" in adapter) &&
        // @ts-ignore
        WGPUAdapter_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = adapter as any;
    }

    let arg1;

    if (
        properties &&
        typeof properties === "object" &&
        !("BYTES_PER_ELEMENT" in properties) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = properties;
    } else {
        arg1 = properties as any;
    }

    const result = wgpulib.wgpuAdapterGetProperties(arg0!, arg1!);
    return result as any;
}

export function wgpuAdapterHasFeature(
    adapter: WGPUAdapter,
    feature: WGPUFeatureName
): WGPUBool {
    let arg0;

    if (
        adapter &&
        typeof adapter === "object" &&
        !("BYTES_PER_ELEMENT" in adapter) &&
        // @ts-ignore
        WGPUAdapter_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = adapter as any;
    }

    let arg1;

    if (
        feature &&
        typeof feature === "object" &&
        !("BYTES_PER_ELEMENT" in feature) &&
        // @ts-ignore
        WGPUFeatureName_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = feature as any;
    }

    const result = wgpulib.wgpuAdapterHasFeature(arg0!, arg1!);
    return result as any;
}

export function wgpuAdapterRequestDevice(
    adapter: WGPUAdapter,
    descriptor:
        | ConstPtrT<WGPUDeviceDescriptor>
        | DeepPartial<WGPUDeviceDescriptor>,
    callback: WGPURequestDeviceCallback,
    userdata: PtrT<void>
): void {
    let arg0;

    if (
        adapter &&
        typeof adapter === "object" &&
        !("BYTES_PER_ELEMENT" in adapter) &&
        // @ts-ignore
        WGPUAdapter_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = adapter as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUDeviceDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    let arg2 = new BunJSCallback(
        callback,
        eval("WGPURequestDeviceCallback_FFI_funcdef")
    ).ptr;
    if (
        callback &&
        typeof callback === "object" &&
        !("BYTES_PER_ELEMENT" in callback) &&
        // @ts-ignore
        WGPURequestDeviceCallback_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = callback as any;
    }

    let arg3;

    if (
        userdata &&
        typeof userdata === "object" &&
        !("BYTES_PER_ELEMENT" in userdata) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg3 = userdata;
    } else {
        arg3 = userdata as any;
    }

    const result = wgpulib.wgpuAdapterRequestDevice(arg0!, arg1!, arg2!, arg3!);
    return result as any;
}

export function wgpuAdapterReference(adapter: WGPUAdapter): void {
    let arg0;

    if (
        adapter &&
        typeof adapter === "object" &&
        !("BYTES_PER_ELEMENT" in adapter) &&
        // @ts-ignore
        WGPUAdapter_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = adapter as any;
    }

    const result = wgpulib.wgpuAdapterReference(arg0!);
    return result as any;
}

export function wgpuAdapterRelease(adapter: WGPUAdapter): void {
    let arg0;

    if (
        adapter &&
        typeof adapter === "object" &&
        !("BYTES_PER_ELEMENT" in adapter) &&
        // @ts-ignore
        WGPUAdapter_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = adapter as any;
    }

    const result = wgpulib.wgpuAdapterRelease(arg0!);
    return result as any;
}

export function wgpuBindGroupSetLabel(
    bindGroup: WGPUBindGroup,
    label: CString
): void {
    let arg0;

    if (
        bindGroup &&
        typeof bindGroup === "object" &&
        !("BYTES_PER_ELEMENT" in bindGroup) &&
        // @ts-ignore
        WGPUBindGroup_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = bindGroup as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuBindGroupSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuBindGroupReference(bindGroup: WGPUBindGroup): void {
    let arg0;

    if (
        bindGroup &&
        typeof bindGroup === "object" &&
        !("BYTES_PER_ELEMENT" in bindGroup) &&
        // @ts-ignore
        WGPUBindGroup_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = bindGroup as any;
    }

    const result = wgpulib.wgpuBindGroupReference(arg0!);
    return result as any;
}

export function wgpuBindGroupRelease(bindGroup: WGPUBindGroup): void {
    let arg0;

    if (
        bindGroup &&
        typeof bindGroup === "object" &&
        !("BYTES_PER_ELEMENT" in bindGroup) &&
        // @ts-ignore
        WGPUBindGroup_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = bindGroup as any;
    }

    const result = wgpulib.wgpuBindGroupRelease(arg0!);
    return result as any;
}

export function wgpuBindGroupLayoutSetLabel(
    bindGroupLayout: WGPUBindGroupLayout,
    label: CString
): void {
    let arg0;

    if (
        bindGroupLayout &&
        typeof bindGroupLayout === "object" &&
        !("BYTES_PER_ELEMENT" in bindGroupLayout) &&
        // @ts-ignore
        WGPUBindGroupLayout_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = bindGroupLayout as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuBindGroupLayoutSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuBindGroupLayoutReference(
    bindGroupLayout: WGPUBindGroupLayout
): void {
    let arg0;

    if (
        bindGroupLayout &&
        typeof bindGroupLayout === "object" &&
        !("BYTES_PER_ELEMENT" in bindGroupLayout) &&
        // @ts-ignore
        WGPUBindGroupLayout_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = bindGroupLayout as any;
    }

    const result = wgpulib.wgpuBindGroupLayoutReference(arg0!);
    return result as any;
}

export function wgpuBindGroupLayoutRelease(
    bindGroupLayout: WGPUBindGroupLayout
): void {
    let arg0;

    if (
        bindGroupLayout &&
        typeof bindGroupLayout === "object" &&
        !("BYTES_PER_ELEMENT" in bindGroupLayout) &&
        // @ts-ignore
        WGPUBindGroupLayout_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = bindGroupLayout as any;
    }

    const result = wgpulib.wgpuBindGroupLayoutRelease(arg0!);
    return result as any;
}

export function wgpuBufferDestroy(buffer: WGPUBuffer): void {
    let arg0;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = buffer as any;
    }

    const result = wgpulib.wgpuBufferDestroy(arg0!);
    return result as any;
}

export function wgpuBufferGetConstMappedRange(
    buffer: WGPUBuffer,
    offset: size_t,
    size: size_t
): ConstPtrT<void> {
    let arg0;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = buffer as any;
    }

    let arg1;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = offset as any;
    }

    let arg2;

    if (
        size &&
        typeof size === "object" &&
        !("BYTES_PER_ELEMENT" in size) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = size as any;
    }

    const result = wgpulib.wgpuBufferGetConstMappedRange(arg0!, arg1!, arg2!);
    return result as any;
}

export function wgpuBufferGetMapState(buffer: WGPUBuffer): WGPUBufferMapState {
    let arg0;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = buffer as any;
    }

    const result = wgpulib.wgpuBufferGetMapState(arg0!);
    return result as any;
}

export function wgpuBufferGetMappedRange(
    buffer: WGPUBuffer,
    offset: size_t,
    size: size_t
): PtrT<void> {
    let arg0;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = buffer as any;
    }

    let arg1;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = offset as any;
    }

    let arg2;

    if (
        size &&
        typeof size === "object" &&
        !("BYTES_PER_ELEMENT" in size) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = size as any;
    }

    const result = wgpulib.wgpuBufferGetMappedRange(arg0!, arg1!, arg2!);
    return result as any;
}

export function wgpuBufferGetSize(buffer: WGPUBuffer): uint64_t {
    let arg0;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = buffer as any;
    }

    const result = wgpulib.wgpuBufferGetSize(arg0!);
    return result as any;
}

export function wgpuBufferGetUsage(buffer: WGPUBuffer): WGPUBufferUsageFlags {
    let arg0;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = buffer as any;
    }

    const result = wgpulib.wgpuBufferGetUsage(arg0!);
    return result as any;
}

export function wgpuBufferMapAsync(
    buffer: WGPUBuffer,
    mode: WGPUMapModeFlags,
    offset: size_t,
    size: size_t,
    callback: WGPUBufferMapCallback,
    userdata: PtrT<void>
): void {
    let arg0;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = buffer as any;
    }

    let arg1;

    if (
        mode &&
        typeof mode === "object" &&
        !("BYTES_PER_ELEMENT" in mode) &&
        // @ts-ignore
        WGPUMapModeFlags_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = mode as any;
    }

    let arg2;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = offset as any;
    }

    let arg3;

    if (
        size &&
        typeof size === "object" &&
        !("BYTES_PER_ELEMENT" in size) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = size as any;
    }

    let arg4 = new BunJSCallback(
        callback,
        eval("WGPUBufferMapCallback_FFI_funcdef")
    ).ptr;
    if (
        callback &&
        typeof callback === "object" &&
        !("BYTES_PER_ELEMENT" in callback) &&
        // @ts-ignore
        WGPUBufferMapCallback_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = callback as any;
    }

    let arg5;

    if (
        userdata &&
        typeof userdata === "object" &&
        !("BYTES_PER_ELEMENT" in userdata) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg5 = userdata;
    } else {
        arg5 = userdata as any;
    }

    const result = wgpulib.wgpuBufferMapAsync(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!,
        arg5!
    );
    return result as any;
}

export function wgpuBufferSetLabel(buffer: WGPUBuffer, label: CString): void {
    let arg0;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = buffer as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuBufferSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuBufferUnmap(buffer: WGPUBuffer): void {
    let arg0;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = buffer as any;
    }

    const result = wgpulib.wgpuBufferUnmap(arg0!);
    return result as any;
}

export function wgpuBufferReference(buffer: WGPUBuffer): void {
    let arg0;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = buffer as any;
    }

    const result = wgpulib.wgpuBufferReference(arg0!);
    return result as any;
}

export function wgpuBufferRelease(buffer: WGPUBuffer): void {
    let arg0;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = buffer as any;
    }

    const result = wgpulib.wgpuBufferRelease(arg0!);
    return result as any;
}

export function wgpuCommandBufferSetLabel(
    commandBuffer: WGPUCommandBuffer,
    label: CString
): void {
    let arg0;

    if (
        commandBuffer &&
        typeof commandBuffer === "object" &&
        !("BYTES_PER_ELEMENT" in commandBuffer) &&
        // @ts-ignore
        WGPUCommandBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandBuffer as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuCommandBufferSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuCommandBufferReference(
    commandBuffer: WGPUCommandBuffer
): void {
    let arg0;

    if (
        commandBuffer &&
        typeof commandBuffer === "object" &&
        !("BYTES_PER_ELEMENT" in commandBuffer) &&
        // @ts-ignore
        WGPUCommandBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandBuffer as any;
    }

    const result = wgpulib.wgpuCommandBufferReference(arg0!);
    return result as any;
}

export function wgpuCommandBufferRelease(
    commandBuffer: WGPUCommandBuffer
): void {
    let arg0;

    if (
        commandBuffer &&
        typeof commandBuffer === "object" &&
        !("BYTES_PER_ELEMENT" in commandBuffer) &&
        // @ts-ignore
        WGPUCommandBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandBuffer as any;
    }

    const result = wgpulib.wgpuCommandBufferRelease(arg0!);
    return result as any;
}

export function wgpuCommandEncoderBeginComputePass(
    commandEncoder: WGPUCommandEncoder,
    descriptor:
        | ConstPtrT<WGPUComputePassDescriptor>
        | DeepPartial<WGPUComputePassDescriptor>
): WGPUComputePassEncoder {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUComputePassDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuCommandEncoderBeginComputePass(arg0!, arg1!);
    return result as any;
}

export function wgpuCommandEncoderBeginRenderPass(
    commandEncoder: WGPUCommandEncoder,
    descriptor:
        | ConstPtrT<WGPURenderPassDescriptor>
        | DeepPartial<WGPURenderPassDescriptor>
): WGPURenderPassEncoder {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPURenderPassDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuCommandEncoderBeginRenderPass(arg0!, arg1!);
    return result as any;
}

export function wgpuCommandEncoderClearBuffer(
    commandEncoder: WGPUCommandEncoder,
    buffer: WGPUBuffer,
    offset: uint64_t,
    size: uint64_t
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = buffer as any;
    }

    let arg2;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = offset as any;
    }

    let arg3;

    if (
        size &&
        typeof size === "object" &&
        !("BYTES_PER_ELEMENT" in size) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = size as any;
    }

    const result = wgpulib.wgpuCommandEncoderClearBuffer(
        arg0!,
        arg1!,
        arg2!,
        arg3!
    );
    return result as any;
}

export function wgpuCommandEncoderCopyBufferToBuffer(
    commandEncoder: WGPUCommandEncoder,
    source: WGPUBuffer,
    sourceOffset: uint64_t,
    destination: WGPUBuffer,
    destinationOffset: uint64_t,
    size: uint64_t
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1;

    if (
        source &&
        typeof source === "object" &&
        !("BYTES_PER_ELEMENT" in source) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = source as any;
    }

    let arg2;

    if (
        sourceOffset &&
        typeof sourceOffset === "object" &&
        !("BYTES_PER_ELEMENT" in sourceOffset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = sourceOffset as any;
    }

    let arg3;

    if (
        destination &&
        typeof destination === "object" &&
        !("BYTES_PER_ELEMENT" in destination) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = destination as any;
    }

    let arg4;

    if (
        destinationOffset &&
        typeof destinationOffset === "object" &&
        !("BYTES_PER_ELEMENT" in destinationOffset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = destinationOffset as any;
    }

    let arg5;

    if (
        size &&
        typeof size === "object" &&
        !("BYTES_PER_ELEMENT" in size) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg5 = size as any;
    }

    const result = wgpulib.wgpuCommandEncoderCopyBufferToBuffer(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!,
        arg5!
    );
    return result as any;
}

export function wgpuCommandEncoderCopyBufferToTexture(
    commandEncoder: WGPUCommandEncoder,
    source: ConstPtrT<WGPUImageCopyBuffer> | DeepPartial<WGPUImageCopyBuffer>,
    destination:
        | ConstPtrT<WGPUImageCopyTexture>
        | DeepPartial<WGPUImageCopyTexture>,
    copySize: ConstPtrT<WGPUExtent3D> | DeepPartial<WGPUExtent3D>
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1;

    if (
        source &&
        typeof source === "object" &&
        !("BYTES_PER_ELEMENT" in source) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUImageCopyBuffer(source as any).typed as any;
    } else {
        arg1 = source as any;
    }

    let arg2;

    if (
        destination &&
        typeof destination === "object" &&
        !("BYTES_PER_ELEMENT" in destination) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = writeWGPUImageCopyTexture(destination as any).typed as any;
    } else {
        arg2 = destination as any;
    }

    let arg3;

    if (
        copySize &&
        typeof copySize === "object" &&
        !("BYTES_PER_ELEMENT" in copySize) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg3 = writeWGPUExtent3D(copySize as any).typed as any;
    } else {
        arg3 = copySize as any;
    }

    const result = wgpulib.wgpuCommandEncoderCopyBufferToTexture(
        arg0!,
        arg1!,
        arg2!,
        arg3!
    );
    return result as any;
}

export function wgpuCommandEncoderCopyTextureToBuffer(
    commandEncoder: WGPUCommandEncoder,
    source: ConstPtrT<WGPUImageCopyTexture> | DeepPartial<WGPUImageCopyTexture>,
    destination:
        | ConstPtrT<WGPUImageCopyBuffer>
        | DeepPartial<WGPUImageCopyBuffer>,
    copySize: ConstPtrT<WGPUExtent3D> | DeepPartial<WGPUExtent3D>
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1;

    if (
        source &&
        typeof source === "object" &&
        !("BYTES_PER_ELEMENT" in source) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUImageCopyTexture(source as any).typed as any;
    } else {
        arg1 = source as any;
    }

    let arg2;

    if (
        destination &&
        typeof destination === "object" &&
        !("BYTES_PER_ELEMENT" in destination) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = writeWGPUImageCopyBuffer(destination as any).typed as any;
    } else {
        arg2 = destination as any;
    }

    let arg3;

    if (
        copySize &&
        typeof copySize === "object" &&
        !("BYTES_PER_ELEMENT" in copySize) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg3 = writeWGPUExtent3D(copySize as any).typed as any;
    } else {
        arg3 = copySize as any;
    }

    const result = wgpulib.wgpuCommandEncoderCopyTextureToBuffer(
        arg0!,
        arg1!,
        arg2!,
        arg3!
    );
    return result as any;
}

export function wgpuCommandEncoderCopyTextureToTexture(
    commandEncoder: WGPUCommandEncoder,
    source: ConstPtrT<WGPUImageCopyTexture> | DeepPartial<WGPUImageCopyTexture>,
    destination:
        | ConstPtrT<WGPUImageCopyTexture>
        | DeepPartial<WGPUImageCopyTexture>,
    copySize: ConstPtrT<WGPUExtent3D> | DeepPartial<WGPUExtent3D>
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1;

    if (
        source &&
        typeof source === "object" &&
        !("BYTES_PER_ELEMENT" in source) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUImageCopyTexture(source as any).typed as any;
    } else {
        arg1 = source as any;
    }

    let arg2;

    if (
        destination &&
        typeof destination === "object" &&
        !("BYTES_PER_ELEMENT" in destination) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = writeWGPUImageCopyTexture(destination as any).typed as any;
    } else {
        arg2 = destination as any;
    }

    let arg3;

    if (
        copySize &&
        typeof copySize === "object" &&
        !("BYTES_PER_ELEMENT" in copySize) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg3 = writeWGPUExtent3D(copySize as any).typed as any;
    } else {
        arg3 = copySize as any;
    }

    const result = wgpulib.wgpuCommandEncoderCopyTextureToTexture(
        arg0!,
        arg1!,
        arg2!,
        arg3!
    );
    return result as any;
}

export function wgpuCommandEncoderFinish(
    commandEncoder: WGPUCommandEncoder,
    descriptor:
        | ConstPtrT<WGPUCommandBufferDescriptor>
        | DeepPartial<WGPUCommandBufferDescriptor>
): WGPUCommandBuffer {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUCommandBufferDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuCommandEncoderFinish(arg0!, arg1!);
    return result as any;
}

export function wgpuCommandEncoderInsertDebugMarker(
    commandEncoder: WGPUCommandEncoder,
    markerLabel: CString
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1 = markerLabel;
    const result = wgpulib.wgpuCommandEncoderInsertDebugMarker(arg0!, arg1!);
    return result as any;
}

export function wgpuCommandEncoderPopDebugGroup(
    commandEncoder: WGPUCommandEncoder
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    const result = wgpulib.wgpuCommandEncoderPopDebugGroup(arg0!);
    return result as any;
}

export function wgpuCommandEncoderPushDebugGroup(
    commandEncoder: WGPUCommandEncoder,
    groupLabel: CString
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1 = groupLabel;
    const result = wgpulib.wgpuCommandEncoderPushDebugGroup(arg0!, arg1!);
    return result as any;
}

export function wgpuCommandEncoderResolveQuerySet(
    commandEncoder: WGPUCommandEncoder,
    querySet: WGPUQuerySet,
    firstQuery: uint32_t,
    queryCount: uint32_t,
    destination: WGPUBuffer,
    destinationOffset: uint64_t
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1;

    if (
        querySet &&
        typeof querySet === "object" &&
        !("BYTES_PER_ELEMENT" in querySet) &&
        // @ts-ignore
        WGPUQuerySet_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = querySet as any;
    }

    let arg2;

    if (
        firstQuery &&
        typeof firstQuery === "object" &&
        !("BYTES_PER_ELEMENT" in firstQuery) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = firstQuery as any;
    }

    let arg3;

    if (
        queryCount &&
        typeof queryCount === "object" &&
        !("BYTES_PER_ELEMENT" in queryCount) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = queryCount as any;
    }

    let arg4;

    if (
        destination &&
        typeof destination === "object" &&
        !("BYTES_PER_ELEMENT" in destination) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = destination as any;
    }

    let arg5;

    if (
        destinationOffset &&
        typeof destinationOffset === "object" &&
        !("BYTES_PER_ELEMENT" in destinationOffset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg5 = destinationOffset as any;
    }

    const result = wgpulib.wgpuCommandEncoderResolveQuerySet(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!,
        arg5!
    );
    return result as any;
}

export function wgpuCommandEncoderSetLabel(
    commandEncoder: WGPUCommandEncoder,
    label: CString
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuCommandEncoderSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuCommandEncoderWriteTimestamp(
    commandEncoder: WGPUCommandEncoder,
    querySet: WGPUQuerySet,
    queryIndex: uint32_t
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    let arg1;

    if (
        querySet &&
        typeof querySet === "object" &&
        !("BYTES_PER_ELEMENT" in querySet) &&
        // @ts-ignore
        WGPUQuerySet_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = querySet as any;
    }

    let arg2;

    if (
        queryIndex &&
        typeof queryIndex === "object" &&
        !("BYTES_PER_ELEMENT" in queryIndex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = queryIndex as any;
    }

    const result = wgpulib.wgpuCommandEncoderWriteTimestamp(
        arg0!,
        arg1!,
        arg2!
    );
    return result as any;
}

export function wgpuCommandEncoderReference(
    commandEncoder: WGPUCommandEncoder
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    const result = wgpulib.wgpuCommandEncoderReference(arg0!);
    return result as any;
}

export function wgpuCommandEncoderRelease(
    commandEncoder: WGPUCommandEncoder
): void {
    let arg0;

    if (
        commandEncoder &&
        typeof commandEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in commandEncoder) &&
        // @ts-ignore
        WGPUCommandEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = commandEncoder as any;
    }

    const result = wgpulib.wgpuCommandEncoderRelease(arg0!);
    return result as any;
}

export function wgpuComputePassEncoderDispatchWorkgroups(
    computePassEncoder: WGPUComputePassEncoder,
    workgroupCountX: uint32_t,
    workgroupCountY: uint32_t,
    workgroupCountZ: uint32_t
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    let arg1;

    if (
        workgroupCountX &&
        typeof workgroupCountX === "object" &&
        !("BYTES_PER_ELEMENT" in workgroupCountX) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = workgroupCountX as any;
    }

    let arg2;

    if (
        workgroupCountY &&
        typeof workgroupCountY === "object" &&
        !("BYTES_PER_ELEMENT" in workgroupCountY) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = workgroupCountY as any;
    }

    let arg3;

    if (
        workgroupCountZ &&
        typeof workgroupCountZ === "object" &&
        !("BYTES_PER_ELEMENT" in workgroupCountZ) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = workgroupCountZ as any;
    }

    const result = wgpulib.wgpuComputePassEncoderDispatchWorkgroups(
        arg0!,
        arg1!,
        arg2!,
        arg3!
    );
    return result as any;
}

export function wgpuComputePassEncoderDispatchWorkgroupsIndirect(
    computePassEncoder: WGPUComputePassEncoder,
    indirectBuffer: WGPUBuffer,
    indirectOffset: uint64_t
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    let arg1;

    if (
        indirectBuffer &&
        typeof indirectBuffer === "object" &&
        !("BYTES_PER_ELEMENT" in indirectBuffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = indirectBuffer as any;
    }

    let arg2;

    if (
        indirectOffset &&
        typeof indirectOffset === "object" &&
        !("BYTES_PER_ELEMENT" in indirectOffset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = indirectOffset as any;
    }

    const result = wgpulib.wgpuComputePassEncoderDispatchWorkgroupsIndirect(
        arg0!,
        arg1!,
        arg2!
    );
    return result as any;
}

export function wgpuComputePassEncoderEnd(
    computePassEncoder: WGPUComputePassEncoder
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    const result = wgpulib.wgpuComputePassEncoderEnd(arg0!);
    return result as any;
}

export function wgpuComputePassEncoderInsertDebugMarker(
    computePassEncoder: WGPUComputePassEncoder,
    markerLabel: CString
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    let arg1 = markerLabel;
    const result = wgpulib.wgpuComputePassEncoderInsertDebugMarker(
        arg0!,
        arg1!
    );
    return result as any;
}

export function wgpuComputePassEncoderPopDebugGroup(
    computePassEncoder: WGPUComputePassEncoder
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    const result = wgpulib.wgpuComputePassEncoderPopDebugGroup(arg0!);
    return result as any;
}

export function wgpuComputePassEncoderPushDebugGroup(
    computePassEncoder: WGPUComputePassEncoder,
    groupLabel: CString
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    let arg1 = groupLabel;
    const result = wgpulib.wgpuComputePassEncoderPushDebugGroup(arg0!, arg1!);
    return result as any;
}

export function wgpuComputePassEncoderSetBindGroup(
    computePassEncoder: WGPUComputePassEncoder,
    groupIndex: uint32_t,
    group: WGPUBindGroup,
    dynamicOffsetCount: size_t,
    dynamicOffsets: ConstPtrT<uint32_t> | DeepPartial<uint32_t>
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    let arg1;

    if (
        groupIndex &&
        typeof groupIndex === "object" &&
        !("BYTES_PER_ELEMENT" in groupIndex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = groupIndex as any;
    }

    let arg2;

    if (
        group &&
        typeof group === "object" &&
        !("BYTES_PER_ELEMENT" in group) &&
        // @ts-ignore
        WGPUBindGroup_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = group as any;
    }

    let arg3;

    if (
        dynamicOffsetCount &&
        typeof dynamicOffsetCount === "object" &&
        !("BYTES_PER_ELEMENT" in dynamicOffsetCount) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = dynamicOffsetCount as any;
    }

    let arg4;

    if (
        dynamicOffsets &&
        typeof dynamicOffsets === "object" &&
        !("BYTES_PER_ELEMENT" in dynamicOffsets) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg4 = writeuint32_t(dynamicOffsets as any).typed as any;
    } else {
        arg4 = dynamicOffsets as any;
    }

    const result = wgpulib.wgpuComputePassEncoderSetBindGroup(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!
    );
    return result as any;
}

export function wgpuComputePassEncoderSetLabel(
    computePassEncoder: WGPUComputePassEncoder,
    label: CString
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuComputePassEncoderSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuComputePassEncoderSetPipeline(
    computePassEncoder: WGPUComputePassEncoder,
    pipeline: WGPUComputePipeline
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    let arg1;

    if (
        pipeline &&
        typeof pipeline === "object" &&
        !("BYTES_PER_ELEMENT" in pipeline) &&
        // @ts-ignore
        WGPUComputePipeline_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = pipeline as any;
    }

    const result = wgpulib.wgpuComputePassEncoderSetPipeline(arg0!, arg1!);
    return result as any;
}

export function wgpuComputePassEncoderReference(
    computePassEncoder: WGPUComputePassEncoder
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    const result = wgpulib.wgpuComputePassEncoderReference(arg0!);
    return result as any;
}

export function wgpuComputePassEncoderRelease(
    computePassEncoder: WGPUComputePassEncoder
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    const result = wgpulib.wgpuComputePassEncoderRelease(arg0!);
    return result as any;
}

export function wgpuComputePipelineGetBindGroupLayout(
    computePipeline: WGPUComputePipeline,
    groupIndex: uint32_t
): WGPUBindGroupLayout {
    let arg0;

    if (
        computePipeline &&
        typeof computePipeline === "object" &&
        !("BYTES_PER_ELEMENT" in computePipeline) &&
        // @ts-ignore
        WGPUComputePipeline_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePipeline as any;
    }

    let arg1;

    if (
        groupIndex &&
        typeof groupIndex === "object" &&
        !("BYTES_PER_ELEMENT" in groupIndex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = groupIndex as any;
    }

    const result = wgpulib.wgpuComputePipelineGetBindGroupLayout(arg0!, arg1!);
    return result as any;
}

export function wgpuComputePipelineSetLabel(
    computePipeline: WGPUComputePipeline,
    label: CString
): void {
    let arg0;

    if (
        computePipeline &&
        typeof computePipeline === "object" &&
        !("BYTES_PER_ELEMENT" in computePipeline) &&
        // @ts-ignore
        WGPUComputePipeline_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePipeline as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuComputePipelineSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuComputePipelineReference(
    computePipeline: WGPUComputePipeline
): void {
    let arg0;

    if (
        computePipeline &&
        typeof computePipeline === "object" &&
        !("BYTES_PER_ELEMENT" in computePipeline) &&
        // @ts-ignore
        WGPUComputePipeline_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePipeline as any;
    }

    const result = wgpulib.wgpuComputePipelineReference(arg0!);
    return result as any;
}

export function wgpuComputePipelineRelease(
    computePipeline: WGPUComputePipeline
): void {
    let arg0;

    if (
        computePipeline &&
        typeof computePipeline === "object" &&
        !("BYTES_PER_ELEMENT" in computePipeline) &&
        // @ts-ignore
        WGPUComputePipeline_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePipeline as any;
    }

    const result = wgpulib.wgpuComputePipelineRelease(arg0!);
    return result as any;
}

export function wgpuDeviceCreateBindGroup(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPUBindGroupDescriptor>
        | DeepPartial<WGPUBindGroupDescriptor>
): WGPUBindGroup {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUBindGroupDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuDeviceCreateBindGroup(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceCreateBindGroupLayout(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPUBindGroupLayoutDescriptor>
        | DeepPartial<WGPUBindGroupLayoutDescriptor>
): WGPUBindGroupLayout {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUBindGroupLayoutDescriptor(descriptor as any)
            .typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuDeviceCreateBindGroupLayout(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceCreateBuffer(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPUBufferDescriptor>
        | DeepPartial<WGPUBufferDescriptor>
): WGPUBuffer {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUBufferDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuDeviceCreateBuffer(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceCreateCommandEncoder(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPUCommandEncoderDescriptor>
        | DeepPartial<WGPUCommandEncoderDescriptor>
): WGPUCommandEncoder {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUCommandEncoderDescriptor(descriptor as any)
            .typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuDeviceCreateCommandEncoder(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceCreateComputePipeline(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPUComputePipelineDescriptor>
        | DeepPartial<WGPUComputePipelineDescriptor>
): WGPUComputePipeline {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUComputePipelineDescriptor(descriptor as any)
            .typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuDeviceCreateComputePipeline(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceCreateComputePipelineAsync(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPUComputePipelineDescriptor>
        | DeepPartial<WGPUComputePipelineDescriptor>,
    callback: WGPUCreateComputePipelineAsyncCallback,
    userdata: PtrT<void>
): void {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUComputePipelineDescriptor(descriptor as any)
            .typed as any;
    } else {
        arg1 = descriptor as any;
    }

    let arg2 = new BunJSCallback(
        callback,
        eval("WGPUCreateComputePipelineAsyncCallback_FFI_funcdef")
    ).ptr;
    if (
        callback &&
        typeof callback === "object" &&
        !("BYTES_PER_ELEMENT" in callback) &&
        // @ts-ignore
        WGPUCreateComputePipelineAsyncCallback_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = callback as any;
    }

    let arg3;

    if (
        userdata &&
        typeof userdata === "object" &&
        !("BYTES_PER_ELEMENT" in userdata) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg3 = userdata;
    } else {
        arg3 = userdata as any;
    }

    const result = wgpulib.wgpuDeviceCreateComputePipelineAsync(
        arg0!,
        arg1!,
        arg2!,
        arg3!
    );
    return result as any;
}

export function wgpuDeviceCreatePipelineLayout(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPUPipelineLayoutDescriptor>
        | DeepPartial<WGPUPipelineLayoutDescriptor>
): WGPUPipelineLayout {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUPipelineLayoutDescriptor(descriptor as any)
            .typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuDeviceCreatePipelineLayout(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceCreateQuerySet(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPUQuerySetDescriptor>
        | DeepPartial<WGPUQuerySetDescriptor>
): WGPUQuerySet {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUQuerySetDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuDeviceCreateQuerySet(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceCreateRenderBundleEncoder(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPURenderBundleEncoderDescriptor>
        | DeepPartial<WGPURenderBundleEncoderDescriptor>
): WGPURenderBundleEncoder {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPURenderBundleEncoderDescriptor(descriptor as any)
            .typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuDeviceCreateRenderBundleEncoder(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceCreateRenderPipeline(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPURenderPipelineDescriptor>
        | DeepPartial<WGPURenderPipelineDescriptor>
): WGPURenderPipeline {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPURenderPipelineDescriptor(descriptor as any)
            .typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuDeviceCreateRenderPipeline(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceCreateRenderPipelineAsync(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPURenderPipelineDescriptor>
        | DeepPartial<WGPURenderPipelineDescriptor>,
    callback: WGPUCreateRenderPipelineAsyncCallback,
    userdata: PtrT<void>
): void {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPURenderPipelineDescriptor(descriptor as any)
            .typed as any;
    } else {
        arg1 = descriptor as any;
    }

    let arg2 = new BunJSCallback(
        callback,
        eval("WGPUCreateRenderPipelineAsyncCallback_FFI_funcdef")
    ).ptr;
    if (
        callback &&
        typeof callback === "object" &&
        !("BYTES_PER_ELEMENT" in callback) &&
        // @ts-ignore
        WGPUCreateRenderPipelineAsyncCallback_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = callback as any;
    }

    let arg3;

    if (
        userdata &&
        typeof userdata === "object" &&
        !("BYTES_PER_ELEMENT" in userdata) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg3 = userdata;
    } else {
        arg3 = userdata as any;
    }

    const result = wgpulib.wgpuDeviceCreateRenderPipelineAsync(
        arg0!,
        arg1!,
        arg2!,
        arg3!
    );
    return result as any;
}

export function wgpuDeviceCreateSampler(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPUSamplerDescriptor>
        | DeepPartial<WGPUSamplerDescriptor>
): WGPUSampler {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUSamplerDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuDeviceCreateSampler(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceCreateShaderModule(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPUShaderModuleDescriptor>
        | DeepPartial<WGPUShaderModuleDescriptor>
): WGPUShaderModule {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUShaderModuleDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuDeviceCreateShaderModule(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceCreateTexture(
    device: WGPUDevice,
    descriptor:
        | ConstPtrT<WGPUTextureDescriptor>
        | DeepPartial<WGPUTextureDescriptor>
): WGPUTexture {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUTextureDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuDeviceCreateTexture(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceDestroy(device: WGPUDevice): void {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    const result = wgpulib.wgpuDeviceDestroy(arg0!);
    return result as any;
}

export function wgpuDeviceEnumerateFeatures(
    device: WGPUDevice,
    features: PtrT<WGPUFeatureName>
): size_t {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        features &&
        typeof features === "object" &&
        !("BYTES_PER_ELEMENT" in features) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = features;
    } else {
        arg1 = features as any;
    }

    const result = wgpulib.wgpuDeviceEnumerateFeatures(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceGetLimits(
    device: WGPUDevice,
    limits: PtrT<WGPUSupportedLimits>
): WGPUBool {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        limits &&
        typeof limits === "object" &&
        !("BYTES_PER_ELEMENT" in limits) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = limits;
    } else {
        arg1 = limits as any;
    }

    const result = wgpulib.wgpuDeviceGetLimits(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceGetQueue(device: WGPUDevice): WGPUQueue {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    const result = wgpulib.wgpuDeviceGetQueue(arg0!);
    return result as any;
}

export function wgpuDeviceHasFeature(
    device: WGPUDevice,
    feature: WGPUFeatureName
): WGPUBool {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        feature &&
        typeof feature === "object" &&
        !("BYTES_PER_ELEMENT" in feature) &&
        // @ts-ignore
        WGPUFeatureName_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = feature as any;
    }

    const result = wgpulib.wgpuDeviceHasFeature(arg0!, arg1!);
    return result as any;
}

export function wgpuDevicePopErrorScope(
    device: WGPUDevice,
    callback: WGPUErrorCallback,
    userdata: PtrT<void>
): void {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1 = new BunJSCallback(
        callback,
        eval("WGPUErrorCallback_FFI_funcdef")
    ).ptr;
    if (
        callback &&
        typeof callback === "object" &&
        !("BYTES_PER_ELEMENT" in callback) &&
        // @ts-ignore
        WGPUErrorCallback_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = callback as any;
    }

    let arg2;

    if (
        userdata &&
        typeof userdata === "object" &&
        !("BYTES_PER_ELEMENT" in userdata) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = userdata;
    } else {
        arg2 = userdata as any;
    }

    const result = wgpulib.wgpuDevicePopErrorScope(arg0!, arg1!, arg2!);
    return result as any;
}

export function wgpuDevicePushErrorScope(
    device: WGPUDevice,
    filter: WGPUErrorFilter
): void {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        filter &&
        typeof filter === "object" &&
        !("BYTES_PER_ELEMENT" in filter) &&
        // @ts-ignore
        WGPUErrorFilter_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = filter as any;
    }

    const result = wgpulib.wgpuDevicePushErrorScope(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceSetLabel(device: WGPUDevice, label: CString): void {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuDeviceSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuDeviceSetUncapturedErrorCallback(
    device: WGPUDevice,
    callback: WGPUErrorCallback,
    userdata: PtrT<void>
): void {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1 = new BunJSCallback(
        callback,
        eval("WGPUErrorCallback_FFI_funcdef")
    ).ptr;
    if (
        callback &&
        typeof callback === "object" &&
        !("BYTES_PER_ELEMENT" in callback) &&
        // @ts-ignore
        WGPUErrorCallback_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = callback as any;
    }

    let arg2;

    if (
        userdata &&
        typeof userdata === "object" &&
        !("BYTES_PER_ELEMENT" in userdata) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = userdata;
    } else {
        arg2 = userdata as any;
    }

    const result = wgpulib.wgpuDeviceSetUncapturedErrorCallback(
        arg0!,
        arg1!,
        arg2!
    );
    return result as any;
}

export function wgpuDeviceReference(device: WGPUDevice): void {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    const result = wgpulib.wgpuDeviceReference(arg0!);
    return result as any;
}

export function wgpuDeviceRelease(device: WGPUDevice): void {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    const result = wgpulib.wgpuDeviceRelease(arg0!);
    return result as any;
}

export function wgpuInstanceCreateSurface(
    instance: WGPUInstance,
    descriptor:
        | ConstPtrT<WGPUSurfaceDescriptor>
        | DeepPartial<WGPUSurfaceDescriptor>
): WGPUSurface {
    let arg0;

    if (
        instance &&
        typeof instance === "object" &&
        !("BYTES_PER_ELEMENT" in instance) &&
        // @ts-ignore
        WGPUInstance_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = instance as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUSurfaceDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuInstanceCreateSurface(arg0!, arg1!);
    return result as any;
}

export function wgpuInstanceProcessEvents(instance: WGPUInstance): void {
    let arg0;

    if (
        instance &&
        typeof instance === "object" &&
        !("BYTES_PER_ELEMENT" in instance) &&
        // @ts-ignore
        WGPUInstance_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = instance as any;
    }

    const result = wgpulib.wgpuInstanceProcessEvents(arg0!);
    return result as any;
}

export function wgpuInstanceRequestAdapter(
    instance: WGPUInstance,
    options:
        | ConstPtrT<WGPURequestAdapterOptions>
        | DeepPartial<WGPURequestAdapterOptions>,
    callback: WGPURequestAdapterCallback,
    userdata: PtrT<void>
): void {
    let arg0;

    if (
        instance &&
        typeof instance === "object" &&
        !("BYTES_PER_ELEMENT" in instance) &&
        // @ts-ignore
        WGPUInstance_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = instance as any;
    }

    let arg1;

    if (
        options &&
        typeof options === "object" &&
        !("BYTES_PER_ELEMENT" in options) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPURequestAdapterOptions(options as any).typed as any;
    } else {
        arg1 = options as any;
    }

    let arg2 = new BunJSCallback(
        callback,
        eval("WGPURequestAdapterCallback_FFI_funcdef")
    ).ptr;
    if (
        callback &&
        typeof callback === "object" &&
        !("BYTES_PER_ELEMENT" in callback) &&
        // @ts-ignore
        WGPURequestAdapterCallback_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = callback as any;
    }

    let arg3;

    if (
        userdata &&
        typeof userdata === "object" &&
        !("BYTES_PER_ELEMENT" in userdata) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg3 = userdata;
    } else {
        arg3 = userdata as any;
    }

    const result = wgpulib.wgpuInstanceRequestAdapter(
        arg0!,
        arg1!,
        arg2!,
        arg3!
    );
    return result as any;
}

export function wgpuInstanceReference(instance: WGPUInstance): void {
    let arg0;

    if (
        instance &&
        typeof instance === "object" &&
        !("BYTES_PER_ELEMENT" in instance) &&
        // @ts-ignore
        WGPUInstance_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = instance as any;
    }

    const result = wgpulib.wgpuInstanceReference(arg0!);
    return result as any;
}

export function wgpuInstanceRelease(instance: WGPUInstance): void {
    let arg0;

    if (
        instance &&
        typeof instance === "object" &&
        !("BYTES_PER_ELEMENT" in instance) &&
        // @ts-ignore
        WGPUInstance_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = instance as any;
    }

    const result = wgpulib.wgpuInstanceRelease(arg0!);
    return result as any;
}

export function wgpuPipelineLayoutSetLabel(
    pipelineLayout: WGPUPipelineLayout,
    label: CString
): void {
    let arg0;

    if (
        pipelineLayout &&
        typeof pipelineLayout === "object" &&
        !("BYTES_PER_ELEMENT" in pipelineLayout) &&
        // @ts-ignore
        WGPUPipelineLayout_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = pipelineLayout as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuPipelineLayoutSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuPipelineLayoutReference(
    pipelineLayout: WGPUPipelineLayout
): void {
    let arg0;

    if (
        pipelineLayout &&
        typeof pipelineLayout === "object" &&
        !("BYTES_PER_ELEMENT" in pipelineLayout) &&
        // @ts-ignore
        WGPUPipelineLayout_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = pipelineLayout as any;
    }

    const result = wgpulib.wgpuPipelineLayoutReference(arg0!);
    return result as any;
}

export function wgpuPipelineLayoutRelease(
    pipelineLayout: WGPUPipelineLayout
): void {
    let arg0;

    if (
        pipelineLayout &&
        typeof pipelineLayout === "object" &&
        !("BYTES_PER_ELEMENT" in pipelineLayout) &&
        // @ts-ignore
        WGPUPipelineLayout_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = pipelineLayout as any;
    }

    const result = wgpulib.wgpuPipelineLayoutRelease(arg0!);
    return result as any;
}

export function wgpuQuerySetDestroy(querySet: WGPUQuerySet): void {
    let arg0;

    if (
        querySet &&
        typeof querySet === "object" &&
        !("BYTES_PER_ELEMENT" in querySet) &&
        // @ts-ignore
        WGPUQuerySet_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = querySet as any;
    }

    const result = wgpulib.wgpuQuerySetDestroy(arg0!);
    return result as any;
}

export function wgpuQuerySetGetCount(querySet: WGPUQuerySet): uint32_t {
    let arg0;

    if (
        querySet &&
        typeof querySet === "object" &&
        !("BYTES_PER_ELEMENT" in querySet) &&
        // @ts-ignore
        WGPUQuerySet_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = querySet as any;
    }

    const result = wgpulib.wgpuQuerySetGetCount(arg0!);
    return result as any;
}

export function wgpuQuerySetGetType(querySet: WGPUQuerySet): WGPUQueryType {
    let arg0;

    if (
        querySet &&
        typeof querySet === "object" &&
        !("BYTES_PER_ELEMENT" in querySet) &&
        // @ts-ignore
        WGPUQuerySet_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = querySet as any;
    }

    const result = wgpulib.wgpuQuerySetGetType(arg0!);
    return result as any;
}

export function wgpuQuerySetSetLabel(
    querySet: WGPUQuerySet,
    label: CString
): void {
    let arg0;

    if (
        querySet &&
        typeof querySet === "object" &&
        !("BYTES_PER_ELEMENT" in querySet) &&
        // @ts-ignore
        WGPUQuerySet_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = querySet as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuQuerySetSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuQuerySetReference(querySet: WGPUQuerySet): void {
    let arg0;

    if (
        querySet &&
        typeof querySet === "object" &&
        !("BYTES_PER_ELEMENT" in querySet) &&
        // @ts-ignore
        WGPUQuerySet_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = querySet as any;
    }

    const result = wgpulib.wgpuQuerySetReference(arg0!);
    return result as any;
}

export function wgpuQuerySetRelease(querySet: WGPUQuerySet): void {
    let arg0;

    if (
        querySet &&
        typeof querySet === "object" &&
        !("BYTES_PER_ELEMENT" in querySet) &&
        // @ts-ignore
        WGPUQuerySet_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = querySet as any;
    }

    const result = wgpulib.wgpuQuerySetRelease(arg0!);
    return result as any;
}

export function wgpuQueueOnSubmittedWorkDone(
    queue: WGPUQueue,
    callback: WGPUQueueWorkDoneCallback,
    userdata: PtrT<void>
): void {
    let arg0;

    if (
        queue &&
        typeof queue === "object" &&
        !("BYTES_PER_ELEMENT" in queue) &&
        // @ts-ignore
        WGPUQueue_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = queue as any;
    }

    let arg1 = new BunJSCallback(
        callback,
        eval("WGPUQueueWorkDoneCallback_FFI_funcdef")
    ).ptr;
    if (
        callback &&
        typeof callback === "object" &&
        !("BYTES_PER_ELEMENT" in callback) &&
        // @ts-ignore
        WGPUQueueWorkDoneCallback_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = callback as any;
    }

    let arg2;

    if (
        userdata &&
        typeof userdata === "object" &&
        !("BYTES_PER_ELEMENT" in userdata) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = userdata;
    } else {
        arg2 = userdata as any;
    }

    const result = wgpulib.wgpuQueueOnSubmittedWorkDone(arg0!, arg1!, arg2!);
    return result as any;
}

export function wgpuQueueSetLabel(queue: WGPUQueue, label: CString): void {
    let arg0;

    if (
        queue &&
        typeof queue === "object" &&
        !("BYTES_PER_ELEMENT" in queue) &&
        // @ts-ignore
        WGPUQueue_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = queue as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuQueueSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuQueueSubmit(
    queue: WGPUQueue,
    commandCount: size_t,
    commands: ConstPtrT<WGPUCommandBuffer> | DeepPartial<WGPUCommandBuffer>
): void {
    let arg0;

    if (
        queue &&
        typeof queue === "object" &&
        !("BYTES_PER_ELEMENT" in queue) &&
        // @ts-ignore
        WGPUQueue_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = queue as any;
    }

    let arg1;

    if (
        commandCount &&
        typeof commandCount === "object" &&
        !("BYTES_PER_ELEMENT" in commandCount) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = commandCount as any;
    }

    let arg2;

    if (
        commands &&
        typeof commands === "object" &&
        !("BYTES_PER_ELEMENT" in commands) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = writeWGPUCommandBuffer(commands as any).typed as any;
    } else {
        arg2 = commands as any;
    }

    const result = wgpulib.wgpuQueueSubmit(arg0!, arg1!, arg2!);
    return result as any;
}

export function wgpuQueueWriteBuffer(
    queue: WGPUQueue,
    buffer: WGPUBuffer,
    bufferOffset: uint64_t,
    data: ConstPtrT<void> | DeepPartial<void>,
    size: size_t
): void {
    let arg0;

    if (
        queue &&
        typeof queue === "object" &&
        !("BYTES_PER_ELEMENT" in queue) &&
        // @ts-ignore
        WGPUQueue_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = queue as any;
    }

    let arg1;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = buffer as any;
    }

    let arg2;

    if (
        bufferOffset &&
        typeof bufferOffset === "object" &&
        !("BYTES_PER_ELEMENT" in bufferOffset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = bufferOffset as any;
    }

    let arg3;

    if (
        data &&
        typeof data === "object" &&
        !("BYTES_PER_ELEMENT" in data) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
    } else {
        arg3 = data as any;
    }

    let arg4;

    if (
        size &&
        typeof size === "object" &&
        !("BYTES_PER_ELEMENT" in size) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = size as any;
    }

    const result = wgpulib.wgpuQueueWriteBuffer(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!
    );
    return result as any;
}

export function wgpuQueueWriteTexture(
    queue: WGPUQueue,
    destination:
        | ConstPtrT<WGPUImageCopyTexture>
        | DeepPartial<WGPUImageCopyTexture>,
    data: ConstPtrT<void> | DeepPartial<void>,
    dataSize: size_t,
    dataLayout:
        | ConstPtrT<WGPUTextureDataLayout>
        | DeepPartial<WGPUTextureDataLayout>,
    writeSize: ConstPtrT<WGPUExtent3D> | DeepPartial<WGPUExtent3D>
): void {
    let arg0;

    if (
        queue &&
        typeof queue === "object" &&
        !("BYTES_PER_ELEMENT" in queue) &&
        // @ts-ignore
        WGPUQueue_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = queue as any;
    }

    let arg1;

    if (
        destination &&
        typeof destination === "object" &&
        !("BYTES_PER_ELEMENT" in destination) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUImageCopyTexture(destination as any).typed as any;
    } else {
        arg1 = destination as any;
    }

    let arg2;

    if (
        data &&
        typeof data === "object" &&
        !("BYTES_PER_ELEMENT" in data) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
    } else {
        arg2 = data as any;
    }

    let arg3;

    if (
        dataSize &&
        typeof dataSize === "object" &&
        !("BYTES_PER_ELEMENT" in dataSize) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = dataSize as any;
    }

    let arg4;

    if (
        dataLayout &&
        typeof dataLayout === "object" &&
        !("BYTES_PER_ELEMENT" in dataLayout) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg4 = writeWGPUTextureDataLayout(dataLayout as any).typed as any;
    } else {
        arg4 = dataLayout as any;
    }

    let arg5;

    if (
        writeSize &&
        typeof writeSize === "object" &&
        !("BYTES_PER_ELEMENT" in writeSize) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg5 = writeWGPUExtent3D(writeSize as any).typed as any;
    } else {
        arg5 = writeSize as any;
    }

    const result = wgpulib.wgpuQueueWriteTexture(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!,
        arg5!
    );
    return result as any;
}

export function wgpuQueueReference(queue: WGPUQueue): void {
    let arg0;

    if (
        queue &&
        typeof queue === "object" &&
        !("BYTES_PER_ELEMENT" in queue) &&
        // @ts-ignore
        WGPUQueue_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = queue as any;
    }

    const result = wgpulib.wgpuQueueReference(arg0!);
    return result as any;
}

export function wgpuQueueRelease(queue: WGPUQueue): void {
    let arg0;

    if (
        queue &&
        typeof queue === "object" &&
        !("BYTES_PER_ELEMENT" in queue) &&
        // @ts-ignore
        WGPUQueue_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = queue as any;
    }

    const result = wgpulib.wgpuQueueRelease(arg0!);
    return result as any;
}

export function wgpuRenderBundleSetLabel(
    renderBundle: WGPURenderBundle,
    label: CString
): void {
    let arg0;

    if (
        renderBundle &&
        typeof renderBundle === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundle) &&
        // @ts-ignore
        WGPURenderBundle_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundle as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuRenderBundleSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuRenderBundleReference(
    renderBundle: WGPURenderBundle
): void {
    let arg0;

    if (
        renderBundle &&
        typeof renderBundle === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundle) &&
        // @ts-ignore
        WGPURenderBundle_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundle as any;
    }

    const result = wgpulib.wgpuRenderBundleReference(arg0!);
    return result as any;
}

export function wgpuRenderBundleRelease(renderBundle: WGPURenderBundle): void {
    let arg0;

    if (
        renderBundle &&
        typeof renderBundle === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundle) &&
        // @ts-ignore
        WGPURenderBundle_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundle as any;
    }

    const result = wgpulib.wgpuRenderBundleRelease(arg0!);
    return result as any;
}

export function wgpuRenderBundleEncoderDraw(
    renderBundleEncoder: WGPURenderBundleEncoder,
    vertexCount: uint32_t,
    instanceCount: uint32_t,
    firstVertex: uint32_t,
    firstInstance: uint32_t
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    let arg1;

    if (
        vertexCount &&
        typeof vertexCount === "object" &&
        !("BYTES_PER_ELEMENT" in vertexCount) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = vertexCount as any;
    }

    let arg2;

    if (
        instanceCount &&
        typeof instanceCount === "object" &&
        !("BYTES_PER_ELEMENT" in instanceCount) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = instanceCount as any;
    }

    let arg3;

    if (
        firstVertex &&
        typeof firstVertex === "object" &&
        !("BYTES_PER_ELEMENT" in firstVertex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = firstVertex as any;
    }

    let arg4;

    if (
        firstInstance &&
        typeof firstInstance === "object" &&
        !("BYTES_PER_ELEMENT" in firstInstance) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = firstInstance as any;
    }

    const result = wgpulib.wgpuRenderBundleEncoderDraw(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!
    );
    return result as any;
}

export function wgpuRenderBundleEncoderDrawIndexed(
    renderBundleEncoder: WGPURenderBundleEncoder,
    indexCount: uint32_t,
    instanceCount: uint32_t,
    firstIndex: uint32_t,
    baseVertex: int32_t,
    firstInstance: uint32_t
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    let arg1;

    if (
        indexCount &&
        typeof indexCount === "object" &&
        !("BYTES_PER_ELEMENT" in indexCount) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = indexCount as any;
    }

    let arg2;

    if (
        instanceCount &&
        typeof instanceCount === "object" &&
        !("BYTES_PER_ELEMENT" in instanceCount) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = instanceCount as any;
    }

    let arg3;

    if (
        firstIndex &&
        typeof firstIndex === "object" &&
        !("BYTES_PER_ELEMENT" in firstIndex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = firstIndex as any;
    }

    let arg4;

    if (
        baseVertex &&
        typeof baseVertex === "object" &&
        !("BYTES_PER_ELEMENT" in baseVertex) &&
        // @ts-ignore
        int32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = baseVertex as any;
    }

    let arg5;

    if (
        firstInstance &&
        typeof firstInstance === "object" &&
        !("BYTES_PER_ELEMENT" in firstInstance) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg5 = firstInstance as any;
    }

    const result = wgpulib.wgpuRenderBundleEncoderDrawIndexed(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!,
        arg5!
    );
    return result as any;
}

export function wgpuRenderBundleEncoderDrawIndexedIndirect(
    renderBundleEncoder: WGPURenderBundleEncoder,
    indirectBuffer: WGPUBuffer,
    indirectOffset: uint64_t
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    let arg1;

    if (
        indirectBuffer &&
        typeof indirectBuffer === "object" &&
        !("BYTES_PER_ELEMENT" in indirectBuffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = indirectBuffer as any;
    }

    let arg2;

    if (
        indirectOffset &&
        typeof indirectOffset === "object" &&
        !("BYTES_PER_ELEMENT" in indirectOffset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = indirectOffset as any;
    }

    const result = wgpulib.wgpuRenderBundleEncoderDrawIndexedIndirect(
        arg0!,
        arg1!,
        arg2!
    );
    return result as any;
}

export function wgpuRenderBundleEncoderDrawIndirect(
    renderBundleEncoder: WGPURenderBundleEncoder,
    indirectBuffer: WGPUBuffer,
    indirectOffset: uint64_t
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    let arg1;

    if (
        indirectBuffer &&
        typeof indirectBuffer === "object" &&
        !("BYTES_PER_ELEMENT" in indirectBuffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = indirectBuffer as any;
    }

    let arg2;

    if (
        indirectOffset &&
        typeof indirectOffset === "object" &&
        !("BYTES_PER_ELEMENT" in indirectOffset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = indirectOffset as any;
    }

    const result = wgpulib.wgpuRenderBundleEncoderDrawIndirect(
        arg0!,
        arg1!,
        arg2!
    );
    return result as any;
}

export function wgpuRenderBundleEncoderFinish(
    renderBundleEncoder: WGPURenderBundleEncoder,
    descriptor:
        | ConstPtrT<WGPURenderBundleDescriptor>
        | DeepPartial<WGPURenderBundleDescriptor>
): WGPURenderBundle {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPURenderBundleDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuRenderBundleEncoderFinish(arg0!, arg1!);
    return result as any;
}

export function wgpuRenderBundleEncoderInsertDebugMarker(
    renderBundleEncoder: WGPURenderBundleEncoder,
    markerLabel: CString
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    let arg1 = markerLabel;
    const result = wgpulib.wgpuRenderBundleEncoderInsertDebugMarker(
        arg0!,
        arg1!
    );
    return result as any;
}

export function wgpuRenderBundleEncoderPopDebugGroup(
    renderBundleEncoder: WGPURenderBundleEncoder
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    const result = wgpulib.wgpuRenderBundleEncoderPopDebugGroup(arg0!);
    return result as any;
}

export function wgpuRenderBundleEncoderPushDebugGroup(
    renderBundleEncoder: WGPURenderBundleEncoder,
    groupLabel: CString
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    let arg1 = groupLabel;
    const result = wgpulib.wgpuRenderBundleEncoderPushDebugGroup(arg0!, arg1!);
    return result as any;
}

export function wgpuRenderBundleEncoderSetBindGroup(
    renderBundleEncoder: WGPURenderBundleEncoder,
    groupIndex: uint32_t,
    group: WGPUBindGroup,
    dynamicOffsetCount: size_t,
    dynamicOffsets: ConstPtrT<uint32_t> | DeepPartial<uint32_t>
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    let arg1;

    if (
        groupIndex &&
        typeof groupIndex === "object" &&
        !("BYTES_PER_ELEMENT" in groupIndex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = groupIndex as any;
    }

    let arg2;

    if (
        group &&
        typeof group === "object" &&
        !("BYTES_PER_ELEMENT" in group) &&
        // @ts-ignore
        WGPUBindGroup_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = group as any;
    }

    let arg3;

    if (
        dynamicOffsetCount &&
        typeof dynamicOffsetCount === "object" &&
        !("BYTES_PER_ELEMENT" in dynamicOffsetCount) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = dynamicOffsetCount as any;
    }

    let arg4;

    if (
        dynamicOffsets &&
        typeof dynamicOffsets === "object" &&
        !("BYTES_PER_ELEMENT" in dynamicOffsets) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg4 = writeuint32_t(dynamicOffsets as any).typed as any;
    } else {
        arg4 = dynamicOffsets as any;
    }

    const result = wgpulib.wgpuRenderBundleEncoderSetBindGroup(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!
    );
    return result as any;
}

export function wgpuRenderBundleEncoderSetIndexBuffer(
    renderBundleEncoder: WGPURenderBundleEncoder,
    buffer: WGPUBuffer,
    format: WGPUIndexFormat,
    offset: uint64_t,
    size: uint64_t
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    let arg1;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = buffer as any;
    }

    let arg2;

    if (
        format &&
        typeof format === "object" &&
        !("BYTES_PER_ELEMENT" in format) &&
        // @ts-ignore
        WGPUIndexFormat_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = format as any;
    }

    let arg3;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = offset as any;
    }

    let arg4;

    if (
        size &&
        typeof size === "object" &&
        !("BYTES_PER_ELEMENT" in size) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = size as any;
    }

    const result = wgpulib.wgpuRenderBundleEncoderSetIndexBuffer(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!
    );
    return result as any;
}

export function wgpuRenderBundleEncoderSetLabel(
    renderBundleEncoder: WGPURenderBundleEncoder,
    label: CString
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuRenderBundleEncoderSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuRenderBundleEncoderSetPipeline(
    renderBundleEncoder: WGPURenderBundleEncoder,
    pipeline: WGPURenderPipeline
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    let arg1;

    if (
        pipeline &&
        typeof pipeline === "object" &&
        !("BYTES_PER_ELEMENT" in pipeline) &&
        // @ts-ignore
        WGPURenderPipeline_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = pipeline as any;
    }

    const result = wgpulib.wgpuRenderBundleEncoderSetPipeline(arg0!, arg1!);
    return result as any;
}

export function wgpuRenderBundleEncoderSetVertexBuffer(
    renderBundleEncoder: WGPURenderBundleEncoder,
    slot: uint32_t,
    buffer: WGPUBuffer,
    offset: uint64_t,
    size: uint64_t
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    let arg1;

    if (
        slot &&
        typeof slot === "object" &&
        !("BYTES_PER_ELEMENT" in slot) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = slot as any;
    }

    let arg2;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = buffer as any;
    }

    let arg3;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = offset as any;
    }

    let arg4;

    if (
        size &&
        typeof size === "object" &&
        !("BYTES_PER_ELEMENT" in size) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = size as any;
    }

    const result = wgpulib.wgpuRenderBundleEncoderSetVertexBuffer(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!
    );
    return result as any;
}

export function wgpuRenderBundleEncoderReference(
    renderBundleEncoder: WGPURenderBundleEncoder
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    const result = wgpulib.wgpuRenderBundleEncoderReference(arg0!);
    return result as any;
}

export function wgpuRenderBundleEncoderRelease(
    renderBundleEncoder: WGPURenderBundleEncoder
): void {
    let arg0;

    if (
        renderBundleEncoder &&
        typeof renderBundleEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderBundleEncoder) &&
        // @ts-ignore
        WGPURenderBundleEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderBundleEncoder as any;
    }

    const result = wgpulib.wgpuRenderBundleEncoderRelease(arg0!);
    return result as any;
}

export function wgpuRenderPassEncoderBeginOcclusionQuery(
    renderPassEncoder: WGPURenderPassEncoder,
    queryIndex: uint32_t
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        queryIndex &&
        typeof queryIndex === "object" &&
        !("BYTES_PER_ELEMENT" in queryIndex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = queryIndex as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderBeginOcclusionQuery(
        arg0!,
        arg1!
    );
    return result as any;
}

export function wgpuRenderPassEncoderDraw(
    renderPassEncoder: WGPURenderPassEncoder,
    vertexCount: uint32_t,
    instanceCount: uint32_t,
    firstVertex: uint32_t,
    firstInstance: uint32_t
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        vertexCount &&
        typeof vertexCount === "object" &&
        !("BYTES_PER_ELEMENT" in vertexCount) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = vertexCount as any;
    }

    let arg2;

    if (
        instanceCount &&
        typeof instanceCount === "object" &&
        !("BYTES_PER_ELEMENT" in instanceCount) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = instanceCount as any;
    }

    let arg3;

    if (
        firstVertex &&
        typeof firstVertex === "object" &&
        !("BYTES_PER_ELEMENT" in firstVertex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = firstVertex as any;
    }

    let arg4;

    if (
        firstInstance &&
        typeof firstInstance === "object" &&
        !("BYTES_PER_ELEMENT" in firstInstance) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = firstInstance as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderDraw(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!
    );
    return result as any;
}

export function wgpuRenderPassEncoderDrawIndexed(
    renderPassEncoder: WGPURenderPassEncoder,
    indexCount: uint32_t,
    instanceCount: uint32_t,
    firstIndex: uint32_t,
    baseVertex: int32_t,
    firstInstance: uint32_t
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        indexCount &&
        typeof indexCount === "object" &&
        !("BYTES_PER_ELEMENT" in indexCount) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = indexCount as any;
    }

    let arg2;

    if (
        instanceCount &&
        typeof instanceCount === "object" &&
        !("BYTES_PER_ELEMENT" in instanceCount) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = instanceCount as any;
    }

    let arg3;

    if (
        firstIndex &&
        typeof firstIndex === "object" &&
        !("BYTES_PER_ELEMENT" in firstIndex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = firstIndex as any;
    }

    let arg4;

    if (
        baseVertex &&
        typeof baseVertex === "object" &&
        !("BYTES_PER_ELEMENT" in baseVertex) &&
        // @ts-ignore
        int32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = baseVertex as any;
    }

    let arg5;

    if (
        firstInstance &&
        typeof firstInstance === "object" &&
        !("BYTES_PER_ELEMENT" in firstInstance) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg5 = firstInstance as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderDrawIndexed(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!,
        arg5!
    );
    return result as any;
}

export function wgpuRenderPassEncoderDrawIndexedIndirect(
    renderPassEncoder: WGPURenderPassEncoder,
    indirectBuffer: WGPUBuffer,
    indirectOffset: uint64_t
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        indirectBuffer &&
        typeof indirectBuffer === "object" &&
        !("BYTES_PER_ELEMENT" in indirectBuffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = indirectBuffer as any;
    }

    let arg2;

    if (
        indirectOffset &&
        typeof indirectOffset === "object" &&
        !("BYTES_PER_ELEMENT" in indirectOffset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = indirectOffset as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderDrawIndexedIndirect(
        arg0!,
        arg1!,
        arg2!
    );
    return result as any;
}

export function wgpuRenderPassEncoderDrawIndirect(
    renderPassEncoder: WGPURenderPassEncoder,
    indirectBuffer: WGPUBuffer,
    indirectOffset: uint64_t
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        indirectBuffer &&
        typeof indirectBuffer === "object" &&
        !("BYTES_PER_ELEMENT" in indirectBuffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = indirectBuffer as any;
    }

    let arg2;

    if (
        indirectOffset &&
        typeof indirectOffset === "object" &&
        !("BYTES_PER_ELEMENT" in indirectOffset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = indirectOffset as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderDrawIndirect(
        arg0!,
        arg1!,
        arg2!
    );
    return result as any;
}

export function wgpuRenderPassEncoderEnd(
    renderPassEncoder: WGPURenderPassEncoder
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderEnd(arg0!);
    return result as any;
}

export function wgpuRenderPassEncoderEndOcclusionQuery(
    renderPassEncoder: WGPURenderPassEncoder
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderEndOcclusionQuery(arg0!);
    return result as any;
}

export function wgpuRenderPassEncoderExecuteBundles(
    renderPassEncoder: WGPURenderPassEncoder,
    bundleCount: size_t,
    bundles: ConstPtrT<WGPURenderBundle> | DeepPartial<WGPURenderBundle>
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        bundleCount &&
        typeof bundleCount === "object" &&
        !("BYTES_PER_ELEMENT" in bundleCount) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = bundleCount as any;
    }

    let arg2;

    if (
        bundles &&
        typeof bundles === "object" &&
        !("BYTES_PER_ELEMENT" in bundles) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = writeWGPURenderBundle(bundles as any).typed as any;
    } else {
        arg2 = bundles as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderExecuteBundles(
        arg0!,
        arg1!,
        arg2!
    );
    return result as any;
}

export function wgpuRenderPassEncoderInsertDebugMarker(
    renderPassEncoder: WGPURenderPassEncoder,
    markerLabel: CString
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1 = markerLabel;
    const result = wgpulib.wgpuRenderPassEncoderInsertDebugMarker(arg0!, arg1!);
    return result as any;
}

export function wgpuRenderPassEncoderPopDebugGroup(
    renderPassEncoder: WGPURenderPassEncoder
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderPopDebugGroup(arg0!);
    return result as any;
}

export function wgpuRenderPassEncoderPushDebugGroup(
    renderPassEncoder: WGPURenderPassEncoder,
    groupLabel: CString
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1 = groupLabel;
    const result = wgpulib.wgpuRenderPassEncoderPushDebugGroup(arg0!, arg1!);
    return result as any;
}

export function wgpuRenderPassEncoderSetBindGroup(
    renderPassEncoder: WGPURenderPassEncoder,
    groupIndex: uint32_t,
    group: WGPUBindGroup,
    dynamicOffsetCount: size_t,
    dynamicOffsets: ConstPtrT<uint32_t> | DeepPartial<uint32_t>
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        groupIndex &&
        typeof groupIndex === "object" &&
        !("BYTES_PER_ELEMENT" in groupIndex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = groupIndex as any;
    }

    let arg2;

    if (
        group &&
        typeof group === "object" &&
        !("BYTES_PER_ELEMENT" in group) &&
        // @ts-ignore
        WGPUBindGroup_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = group as any;
    }

    let arg3;

    if (
        dynamicOffsetCount &&
        typeof dynamicOffsetCount === "object" &&
        !("BYTES_PER_ELEMENT" in dynamicOffsetCount) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = dynamicOffsetCount as any;
    }

    let arg4;

    if (
        dynamicOffsets &&
        typeof dynamicOffsets === "object" &&
        !("BYTES_PER_ELEMENT" in dynamicOffsets) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg4 = writeuint32_t(dynamicOffsets as any).typed as any;
    } else {
        arg4 = dynamicOffsets as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderSetBindGroup(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!
    );
    return result as any;
}

export function wgpuRenderPassEncoderSetBlendConstant(
    renderPassEncoder: WGPURenderPassEncoder,
    color: ConstPtrT<WGPUColor> | DeepPartial<WGPUColor>
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        color &&
        typeof color === "object" &&
        !("BYTES_PER_ELEMENT" in color) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUColor(color as any).typed as any;
    } else {
        arg1 = color as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderSetBlendConstant(arg0!, arg1!);
    return result as any;
}

export function wgpuRenderPassEncoderSetIndexBuffer(
    renderPassEncoder: WGPURenderPassEncoder,
    buffer: WGPUBuffer,
    format: WGPUIndexFormat,
    offset: uint64_t,
    size: uint64_t
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = buffer as any;
    }

    let arg2;

    if (
        format &&
        typeof format === "object" &&
        !("BYTES_PER_ELEMENT" in format) &&
        // @ts-ignore
        WGPUIndexFormat_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = format as any;
    }

    let arg3;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = offset as any;
    }

    let arg4;

    if (
        size &&
        typeof size === "object" &&
        !("BYTES_PER_ELEMENT" in size) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = size as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderSetIndexBuffer(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!
    );
    return result as any;
}

export function wgpuRenderPassEncoderSetLabel(
    renderPassEncoder: WGPURenderPassEncoder,
    label: CString
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuRenderPassEncoderSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuRenderPassEncoderSetPipeline(
    renderPassEncoder: WGPURenderPassEncoder,
    pipeline: WGPURenderPipeline
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        pipeline &&
        typeof pipeline === "object" &&
        !("BYTES_PER_ELEMENT" in pipeline) &&
        // @ts-ignore
        WGPURenderPipeline_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = pipeline as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderSetPipeline(arg0!, arg1!);
    return result as any;
}

export function wgpuRenderPassEncoderSetScissorRect(
    renderPassEncoder: WGPURenderPassEncoder,
    x: uint32_t,
    y: uint32_t,
    width: uint32_t,
    height: uint32_t
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        x &&
        typeof x === "object" &&
        !("BYTES_PER_ELEMENT" in x) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = x as any;
    }

    let arg2;

    if (
        y &&
        typeof y === "object" &&
        !("BYTES_PER_ELEMENT" in y) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = y as any;
    }

    let arg3;

    if (
        width &&
        typeof width === "object" &&
        !("BYTES_PER_ELEMENT" in width) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = width as any;
    }

    let arg4;

    if (
        height &&
        typeof height === "object" &&
        !("BYTES_PER_ELEMENT" in height) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = height as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderSetScissorRect(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!
    );
    return result as any;
}

export function wgpuRenderPassEncoderSetStencilReference(
    renderPassEncoder: WGPURenderPassEncoder,
    reference: uint32_t
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        reference &&
        typeof reference === "object" &&
        !("BYTES_PER_ELEMENT" in reference) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = reference as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderSetStencilReference(
        arg0!,
        arg1!
    );
    return result as any;
}

export function wgpuRenderPassEncoderSetVertexBuffer(
    renderPassEncoder: WGPURenderPassEncoder,
    slot: uint32_t,
    buffer: WGPUBuffer,
    offset: uint64_t,
    size: uint64_t
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        slot &&
        typeof slot === "object" &&
        !("BYTES_PER_ELEMENT" in slot) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = slot as any;
    }

    let arg2;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = buffer as any;
    }

    let arg3;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = offset as any;
    }

    let arg4;

    if (
        size &&
        typeof size === "object" &&
        !("BYTES_PER_ELEMENT" in size) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = size as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderSetVertexBuffer(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!
    );
    return result as any;
}

export function wgpuRenderPassEncoderSetViewport(
    renderPassEncoder: WGPURenderPassEncoder,
    x: float,
    y: float,
    width: float,
    height: float,
    minDepth: float,
    maxDepth: float
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        x &&
        typeof x === "object" &&
        !("BYTES_PER_ELEMENT" in x) &&
        // @ts-ignore
        float_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = x as any;
    }

    let arg2;

    if (
        y &&
        typeof y === "object" &&
        !("BYTES_PER_ELEMENT" in y) &&
        // @ts-ignore
        float_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = y as any;
    }

    let arg3;

    if (
        width &&
        typeof width === "object" &&
        !("BYTES_PER_ELEMENT" in width) &&
        // @ts-ignore
        float_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = width as any;
    }

    let arg4;

    if (
        height &&
        typeof height === "object" &&
        !("BYTES_PER_ELEMENT" in height) &&
        // @ts-ignore
        float_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = height as any;
    }

    let arg5;

    if (
        minDepth &&
        typeof minDepth === "object" &&
        !("BYTES_PER_ELEMENT" in minDepth) &&
        // @ts-ignore
        float_FFI === FFIType.ptr
    ) {
    } else {
        arg5 = minDepth as any;
    }

    let arg6;

    if (
        maxDepth &&
        typeof maxDepth === "object" &&
        !("BYTES_PER_ELEMENT" in maxDepth) &&
        // @ts-ignore
        float_FFI === FFIType.ptr
    ) {
    } else {
        arg6 = maxDepth as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderSetViewport(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!,
        arg5!,
        arg6!
    );
    return result as any;
}

export function wgpuRenderPassEncoderReference(
    renderPassEncoder: WGPURenderPassEncoder
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderReference(arg0!);
    return result as any;
}

export function wgpuRenderPassEncoderRelease(
    renderPassEncoder: WGPURenderPassEncoder
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderRelease(arg0!);
    return result as any;
}

export function wgpuRenderPipelineGetBindGroupLayout(
    renderPipeline: WGPURenderPipeline,
    groupIndex: uint32_t
): WGPUBindGroupLayout {
    let arg0;

    if (
        renderPipeline &&
        typeof renderPipeline === "object" &&
        !("BYTES_PER_ELEMENT" in renderPipeline) &&
        // @ts-ignore
        WGPURenderPipeline_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPipeline as any;
    }

    let arg1;

    if (
        groupIndex &&
        typeof groupIndex === "object" &&
        !("BYTES_PER_ELEMENT" in groupIndex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = groupIndex as any;
    }

    const result = wgpulib.wgpuRenderPipelineGetBindGroupLayout(arg0!, arg1!);
    return result as any;
}

export function wgpuRenderPipelineSetLabel(
    renderPipeline: WGPURenderPipeline,
    label: CString
): void {
    let arg0;

    if (
        renderPipeline &&
        typeof renderPipeline === "object" &&
        !("BYTES_PER_ELEMENT" in renderPipeline) &&
        // @ts-ignore
        WGPURenderPipeline_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPipeline as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuRenderPipelineSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuRenderPipelineReference(
    renderPipeline: WGPURenderPipeline
): void {
    let arg0;

    if (
        renderPipeline &&
        typeof renderPipeline === "object" &&
        !("BYTES_PER_ELEMENT" in renderPipeline) &&
        // @ts-ignore
        WGPURenderPipeline_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPipeline as any;
    }

    const result = wgpulib.wgpuRenderPipelineReference(arg0!);
    return result as any;
}

export function wgpuRenderPipelineRelease(
    renderPipeline: WGPURenderPipeline
): void {
    let arg0;

    if (
        renderPipeline &&
        typeof renderPipeline === "object" &&
        !("BYTES_PER_ELEMENT" in renderPipeline) &&
        // @ts-ignore
        WGPURenderPipeline_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPipeline as any;
    }

    const result = wgpulib.wgpuRenderPipelineRelease(arg0!);
    return result as any;
}

export function wgpuSamplerSetLabel(
    sampler: WGPUSampler,
    label: CString
): void {
    let arg0;

    if (
        sampler &&
        typeof sampler === "object" &&
        !("BYTES_PER_ELEMENT" in sampler) &&
        // @ts-ignore
        WGPUSampler_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = sampler as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuSamplerSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuSamplerReference(sampler: WGPUSampler): void {
    let arg0;

    if (
        sampler &&
        typeof sampler === "object" &&
        !("BYTES_PER_ELEMENT" in sampler) &&
        // @ts-ignore
        WGPUSampler_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = sampler as any;
    }

    const result = wgpulib.wgpuSamplerReference(arg0!);
    return result as any;
}

export function wgpuSamplerRelease(sampler: WGPUSampler): void {
    let arg0;

    if (
        sampler &&
        typeof sampler === "object" &&
        !("BYTES_PER_ELEMENT" in sampler) &&
        // @ts-ignore
        WGPUSampler_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = sampler as any;
    }

    const result = wgpulib.wgpuSamplerRelease(arg0!);
    return result as any;
}

export function wgpuShaderModuleGetCompilationInfo(
    shaderModule: WGPUShaderModule,
    callback: WGPUCompilationInfoCallback,
    userdata: PtrT<void>
): void {
    let arg0;

    if (
        shaderModule &&
        typeof shaderModule === "object" &&
        !("BYTES_PER_ELEMENT" in shaderModule) &&
        // @ts-ignore
        WGPUShaderModule_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = shaderModule as any;
    }

    let arg1 = new BunJSCallback(
        callback,
        eval("WGPUCompilationInfoCallback_FFI_funcdef")
    ).ptr;
    if (
        callback &&
        typeof callback === "object" &&
        !("BYTES_PER_ELEMENT" in callback) &&
        // @ts-ignore
        WGPUCompilationInfoCallback_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = callback as any;
    }

    let arg2;

    if (
        userdata &&
        typeof userdata === "object" &&
        !("BYTES_PER_ELEMENT" in userdata) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = userdata;
    } else {
        arg2 = userdata as any;
    }

    const result = wgpulib.wgpuShaderModuleGetCompilationInfo(
        arg0!,
        arg1!,
        arg2!
    );
    return result as any;
}

export function wgpuShaderModuleSetLabel(
    shaderModule: WGPUShaderModule,
    label: CString
): void {
    let arg0;

    if (
        shaderModule &&
        typeof shaderModule === "object" &&
        !("BYTES_PER_ELEMENT" in shaderModule) &&
        // @ts-ignore
        WGPUShaderModule_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = shaderModule as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuShaderModuleSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuShaderModuleReference(
    shaderModule: WGPUShaderModule
): void {
    let arg0;

    if (
        shaderModule &&
        typeof shaderModule === "object" &&
        !("BYTES_PER_ELEMENT" in shaderModule) &&
        // @ts-ignore
        WGPUShaderModule_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = shaderModule as any;
    }

    const result = wgpulib.wgpuShaderModuleReference(arg0!);
    return result as any;
}

export function wgpuShaderModuleRelease(shaderModule: WGPUShaderModule): void {
    let arg0;

    if (
        shaderModule &&
        typeof shaderModule === "object" &&
        !("BYTES_PER_ELEMENT" in shaderModule) &&
        // @ts-ignore
        WGPUShaderModule_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = shaderModule as any;
    }

    const result = wgpulib.wgpuShaderModuleRelease(arg0!);
    return result as any;
}

export function wgpuSurfaceConfigure(
    surface: WGPUSurface,
    config:
        | ConstPtrT<WGPUSurfaceConfiguration>
        | DeepPartial<WGPUSurfaceConfiguration>
): void {
    let arg0;

    if (
        surface &&
        typeof surface === "object" &&
        !("BYTES_PER_ELEMENT" in surface) &&
        // @ts-ignore
        WGPUSurface_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = surface as any;
    }

    let arg1;

    if (
        config &&
        typeof config === "object" &&
        !("BYTES_PER_ELEMENT" in config) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUSurfaceConfiguration(config as any).typed as any;
    } else {
        arg1 = config as any;
    }

    const result = wgpulib.wgpuSurfaceConfigure(arg0!, arg1!);
    return result as any;
}

export function wgpuSurfaceGetCapabilities(
    surface: WGPUSurface,
    adapter: WGPUAdapter,
    capabilities: PtrT<WGPUSurfaceCapabilities>
): void {
    let arg0;

    if (
        surface &&
        typeof surface === "object" &&
        !("BYTES_PER_ELEMENT" in surface) &&
        // @ts-ignore
        WGPUSurface_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = surface as any;
    }

    let arg1;

    if (
        adapter &&
        typeof adapter === "object" &&
        !("BYTES_PER_ELEMENT" in adapter) &&
        // @ts-ignore
        WGPUAdapter_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = adapter as any;
    }

    let arg2;

    if (
        capabilities &&
        typeof capabilities === "object" &&
        !("BYTES_PER_ELEMENT" in capabilities) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = capabilities;
    } else {
        arg2 = capabilities as any;
    }

    const result = wgpulib.wgpuSurfaceGetCapabilities(arg0!, arg1!, arg2!);
    return result as any;
}

export function wgpuSurfaceGetCurrentTexture(
    surface: WGPUSurface,
    surfaceTexture: PtrT<WGPUSurfaceTexture>
): void {
    let arg0;

    if (
        surface &&
        typeof surface === "object" &&
        !("BYTES_PER_ELEMENT" in surface) &&
        // @ts-ignore
        WGPUSurface_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = surface as any;
    }

    let arg1;

    if (
        surfaceTexture &&
        typeof surfaceTexture === "object" &&
        !("BYTES_PER_ELEMENT" in surfaceTexture) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = surfaceTexture;
    } else {
        arg1 = surfaceTexture as any;
    }

    const result = wgpulib.wgpuSurfaceGetCurrentTexture(arg0!, arg1!);
    return result as any;
}

export function wgpuSurfaceGetPreferredFormat(
    surface: WGPUSurface,
    adapter: WGPUAdapter
): WGPUTextureFormat {
    let arg0;

    if (
        surface &&
        typeof surface === "object" &&
        !("BYTES_PER_ELEMENT" in surface) &&
        // @ts-ignore
        WGPUSurface_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = surface as any;
    }

    let arg1;

    if (
        adapter &&
        typeof adapter === "object" &&
        !("BYTES_PER_ELEMENT" in adapter) &&
        // @ts-ignore
        WGPUAdapter_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = adapter as any;
    }

    const result = wgpulib.wgpuSurfaceGetPreferredFormat(arg0!, arg1!);
    return result as any;
}

export function wgpuSurfacePresent(surface: WGPUSurface): void {
    let arg0;

    if (
        surface &&
        typeof surface === "object" &&
        !("BYTES_PER_ELEMENT" in surface) &&
        // @ts-ignore
        WGPUSurface_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = surface as any;
    }

    const result = wgpulib.wgpuSurfacePresent(arg0!);
    return result as any;
}

export function wgpuSurfaceUnconfigure(surface: WGPUSurface): void {
    let arg0;

    if (
        surface &&
        typeof surface === "object" &&
        !("BYTES_PER_ELEMENT" in surface) &&
        // @ts-ignore
        WGPUSurface_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = surface as any;
    }

    const result = wgpulib.wgpuSurfaceUnconfigure(arg0!);
    return result as any;
}

export function wgpuSurfaceReference(surface: WGPUSurface): void {
    let arg0;

    if (
        surface &&
        typeof surface === "object" &&
        !("BYTES_PER_ELEMENT" in surface) &&
        // @ts-ignore
        WGPUSurface_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = surface as any;
    }

    const result = wgpulib.wgpuSurfaceReference(arg0!);
    return result as any;
}

export function wgpuSurfaceRelease(surface: WGPUSurface): void {
    let arg0;

    if (
        surface &&
        typeof surface === "object" &&
        !("BYTES_PER_ELEMENT" in surface) &&
        // @ts-ignore
        WGPUSurface_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = surface as any;
    }

    const result = wgpulib.wgpuSurfaceRelease(arg0!);
    return result as any;
}

export function wgpuSurfaceCapabilitiesFreeMembers(
    capabilities: WGPUSurfaceCapabilities
): void {
    let arg0;

    if (
        capabilities &&
        typeof capabilities === "object" &&
        !("BYTES_PER_ELEMENT" in capabilities) &&
        // @ts-ignore
        WGPUSurfaceCapabilities_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = capabilities as any;
    }

    const result = wgpulib.wgpuSurfaceCapabilitiesFreeMembers(arg0!);
    return result as any;
}

export function wgpuTextureCreateView(
    texture: WGPUTexture,
    descriptor:
        | ConstPtrT<WGPUTextureViewDescriptor>
        | DeepPartial<WGPUTextureViewDescriptor>
): WGPUTextureView {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    let arg1;

    if (
        descriptor &&
        typeof descriptor === "object" &&
        !("BYTES_PER_ELEMENT" in descriptor) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUTextureViewDescriptor(descriptor as any).typed as any;
    } else {
        arg1 = descriptor as any;
    }

    const result = wgpulib.wgpuTextureCreateView(arg0!, arg1!);
    return result as any;
}

export function wgpuTextureDestroy(texture: WGPUTexture): void {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    const result = wgpulib.wgpuTextureDestroy(arg0!);
    return result as any;
}

export function wgpuTextureGetDepthOrArrayLayers(
    texture: WGPUTexture
): uint32_t {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    const result = wgpulib.wgpuTextureGetDepthOrArrayLayers(arg0!);
    return result as any;
}

export function wgpuTextureGetDimension(
    texture: WGPUTexture
): WGPUTextureDimension {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    const result = wgpulib.wgpuTextureGetDimension(arg0!);
    return result as any;
}

export function wgpuTextureGetFormat(texture: WGPUTexture): WGPUTextureFormat {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    const result = wgpulib.wgpuTextureGetFormat(arg0!);
    return result as any;
}

export function wgpuTextureGetHeight(texture: WGPUTexture): uint32_t {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    const result = wgpulib.wgpuTextureGetHeight(arg0!);
    return result as any;
}

export function wgpuTextureGetMipLevelCount(texture: WGPUTexture): uint32_t {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    const result = wgpulib.wgpuTextureGetMipLevelCount(arg0!);
    return result as any;
}

export function wgpuTextureGetSampleCount(texture: WGPUTexture): uint32_t {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    const result = wgpulib.wgpuTextureGetSampleCount(arg0!);
    return result as any;
}

export function wgpuTextureGetUsage(
    texture: WGPUTexture
): WGPUTextureUsageFlags {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    const result = wgpulib.wgpuTextureGetUsage(arg0!);
    return result as any;
}

export function wgpuTextureGetWidth(texture: WGPUTexture): uint32_t {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    const result = wgpulib.wgpuTextureGetWidth(arg0!);
    return result as any;
}

export function wgpuTextureSetLabel(
    texture: WGPUTexture,
    label: CString
): void {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuTextureSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuTextureReference(texture: WGPUTexture): void {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    const result = wgpulib.wgpuTextureReference(arg0!);
    return result as any;
}

export function wgpuTextureRelease(texture: WGPUTexture): void {
    let arg0;

    if (
        texture &&
        typeof texture === "object" &&
        !("BYTES_PER_ELEMENT" in texture) &&
        // @ts-ignore
        WGPUTexture_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = texture as any;
    }

    const result = wgpulib.wgpuTextureRelease(arg0!);
    return result as any;
}

export function wgpuTextureViewSetLabel(
    textureView: WGPUTextureView,
    label: CString
): void {
    let arg0;

    if (
        textureView &&
        typeof textureView === "object" &&
        !("BYTES_PER_ELEMENT" in textureView) &&
        // @ts-ignore
        WGPUTextureView_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = textureView as any;
    }

    let arg1 = label;
    const result = wgpulib.wgpuTextureViewSetLabel(arg0!, arg1!);
    return result as any;
}

export function wgpuTextureViewReference(textureView: WGPUTextureView): void {
    let arg0;

    if (
        textureView &&
        typeof textureView === "object" &&
        !("BYTES_PER_ELEMENT" in textureView) &&
        // @ts-ignore
        WGPUTextureView_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = textureView as any;
    }

    const result = wgpulib.wgpuTextureViewReference(arg0!);
    return result as any;
}

export function wgpuTextureViewRelease(textureView: WGPUTextureView): void {
    let arg0;

    if (
        textureView &&
        typeof textureView === "object" &&
        !("BYTES_PER_ELEMENT" in textureView) &&
        // @ts-ignore
        WGPUTextureView_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = textureView as any;
    }

    const result = wgpulib.wgpuTextureViewRelease(arg0!);
    return result as any;
}

export enum WGPUNativeSType {
    WGPUSType_DeviceExtras = 196609,
    WGPUSType_RequiredLimitsExtras = 196610,
    WGPUSType_PipelineLayoutExtras = 196611,
    WGPUSType_ShaderModuleGLSLDescriptor = 196612,
    WGPUSType_SupportedLimitsExtras = 196613,
    WGPUSType_InstanceExtras = 196614,
    WGPUSType_BindGroupEntryExtras = 196615,
    WGPUSType_BindGroupLayoutEntryExtras = 196616,
    WGPUSType_QuerySetDescriptorExtras = 196617,
    WGPUSType_SurfaceConfigurationExtras = 196618,
    WGPUNativeSType_Force32 = 2147483647,
}
export const WGPUNativeSType_FFI = FFIType.int32_t;

export enum WGPUNativeFeature {
    WGPUNativeFeature_PushConstants = 196609,
    WGPUNativeFeature_TextureAdapterSpecificFormatFeatures = 196610,
    WGPUNativeFeature_MultiDrawIndirect = 196611,
    WGPUNativeFeature_MultiDrawIndirectCount = 196612,
    WGPUNativeFeature_VertexWritableStorage = 196613,
    WGPUNativeFeature_TextureBindingArray = 196614,
    WGPUNativeFeature_SampledTextureAndStorageBufferArrayNonUniformIndexing = 196615,
    WGPUNativeFeature_PipelineStatisticsQuery = 196616,
    WGPUNativeFeature_StorageResourceBindingArray = 196617,
    WGPUNativeFeature_PartiallyBoundBindingArray = 196618,
    WGPUNativeFeature_Force32 = 2147483647,
}
export const WGPUNativeFeature_FFI = FFIType.int32_t;

export enum WGPULogLevel {
    WGPULogLevel_Off = 0,
    WGPULogLevel_Error = 1,
    WGPULogLevel_Warn = 2,
    WGPULogLevel_Info = 3,
    WGPULogLevel_Debug = 4,
    WGPULogLevel_Trace = 5,
    WGPULogLevel_Force32 = 2147483647,
}
export const WGPULogLevel_FFI = FFIType.int32_t;

export enum WGPUInstanceBackend {
    WGPUInstanceBackend_All = 0,
    WGPUInstanceBackend_Vulkan = 1 << 0,
    WGPUInstanceBackend_GL = 1 << 1,
    WGPUInstanceBackend_Metal = 1 << 2,
    WGPUInstanceBackend_DX12 = 1 << 3,
    WGPUInstanceBackend_DX11 = 1 << 4,
    WGPUInstanceBackend_BrowserWebGPU = 1 << 5,
    WGPUInstanceBackend_Primary = WGPUInstanceBackend_Vulkan |
        WGPUInstanceBackend_Metal |
        WGPUInstanceBackend_DX12 |
        WGPUInstanceBackend_BrowserWebGPU,
    WGPUInstanceBackend_Secondary = WGPUInstanceBackend_GL |
        WGPUInstanceBackend_DX11,
    WGPUInstanceBackend_Force32 = 2147483647,
}
export const WGPUInstanceBackend_FFI = FFIType.int32_t;

export type WGPUInstanceBackendFlags = WGPUFlags;
export const WGPUInstanceBackendFlags_FFI = WGPUFlags_FFI;
export enum WGPUInstanceFlag {
    WGPUInstanceFlag_Default = 0,
    WGPUInstanceFlag_Debug = 1 << 0,
    WGPUInstanceFlag_Validation = 1 << 1,
    WGPUInstanceFlag_DiscardHalLabels = 1 << 2,
    WGPUInstanceFlag_Force32 = 2147483647,
}
export const WGPUInstanceFlag_FFI = FFIType.int32_t;

export type WGPUInstanceFlags = WGPUFlags;
export const WGPUInstanceFlags_FFI = WGPUFlags_FFI;
export enum WGPUDx12Compiler {
    WGPUDx12Compiler_Undefined = 0,
    WGPUDx12Compiler_Fxc = 1,
    WGPUDx12Compiler_Dxc = 2,
    WGPUDx12Compiler_Force32 = 2147483647,
}
export const WGPUDx12Compiler_FFI = FFIType.int32_t;

export enum WGPUGles3MinorVersion {
    WGPUGles3MinorVersion_Automatic = 0,
    WGPUGles3MinorVersion_Version0 = 1,
    WGPUGles3MinorVersion_Version1 = 2,
    WGPUGles3MinorVersion_Version2 = 3,
    WGPUGles3MinorVersion_Force32 = 2147483647,
}
export const WGPUGles3MinorVersion_FFI = FFIType.int32_t;

export enum WGPUPipelineStatisticName {
    WGPUPipelineStatisticName_VertexShaderInvocations = 0,
    WGPUPipelineStatisticName_ClipperInvocations = 1,
    WGPUPipelineStatisticName_ClipperPrimitivesOut = 2,
    WGPUPipelineStatisticName_FragmentShaderInvocations = 3,
    WGPUPipelineStatisticName_ComputeShaderInvocations = 4,
    WGPUPipelineStatisticName_Force32 = 2147483647,
}
export const WGPUPipelineStatisticName_FFI = FFIType.int32_t;

export enum WGPUNativeQueryType {
    WGPUNativeQueryType_PipelineStatistics = 196608,
    WGPUNativeQueryType_Force32 = 2147483647,
}
export const WGPUNativeQueryType_FFI = FFIType.int32_t;

export type WGPUInstanceExtras = {
    chain: WGPUChainedStruct;
    backends: WGPUInstanceBackendFlags;
    flags: WGPUInstanceFlags;
    dx12ShaderCompiler: WGPUDx12Compiler;
    gles3MinorVersion: WGPUGles3MinorVersion;
    dxilPath: CString;
    dxcPath: CString;
};
export const WGPUInstanceExtras_FFI = Pointer as PtrT<WGPUInstanceExtras>;

export type WGPUDeviceExtras = {
    chain: WGPUChainedStruct;
    tracePath: CString;
};
export const WGPUDeviceExtras_FFI = Pointer as PtrT<WGPUDeviceExtras>;

export type WGPUNativeLimits = {
    maxPushConstantSize: uint32_t;
    maxNonSamplerBindings: uint32_t;
};
export const WGPUNativeLimits_FFI = Pointer as PtrT<WGPUNativeLimits>;

export type WGPURequiredLimitsExtras = {
    chain: WGPUChainedStruct;
    limits: WGPUNativeLimits;
};
export const WGPURequiredLimitsExtras_FFI =
    Pointer as PtrT<WGPURequiredLimitsExtras>;

export type WGPUSupportedLimitsExtras = {
    chain: WGPUChainedStructOut;
    limits: WGPUNativeLimits;
};
export const WGPUSupportedLimitsExtras_FFI =
    Pointer as PtrT<WGPUSupportedLimitsExtras>;

export type WGPUPushConstantRange = {
    stages: WGPUShaderStageFlags;
    start: uint32_t;
    end: uint32_t;
};
export const WGPUPushConstantRange_FFI = Pointer as PtrT<WGPUPushConstantRange>;

export type WGPUPipelineLayoutExtras = {
    chain: WGPUChainedStruct;
    pushConstantRangeCount: size_t;
    pushConstantRanges: ConstPtrT<WGPUPushConstantRange>;
};
export const WGPUPipelineLayoutExtras_FFI =
    Pointer as PtrT<WGPUPipelineLayoutExtras>;

export type WGPUSubmissionIndex = uint64_t;
export const WGPUSubmissionIndex_FFI = uint64_t_FFI;
export type WGPUWrappedSubmissionIndex = {
    queue: WGPUQueue;
    submissionIndex: WGPUSubmissionIndex;
};
export const WGPUWrappedSubmissionIndex_FFI =
    Pointer as PtrT<WGPUWrappedSubmissionIndex>;

export type WGPUShaderDefine = {
    name: CString;
    value: CString;
};
export const WGPUShaderDefine_FFI = Pointer as PtrT<WGPUShaderDefine>;

export type WGPUShaderModuleGLSLDescriptor = {
    chain: WGPUChainedStruct;
    stage: WGPUShaderStage;
    code: CString;
    defineCount: uint32_t;
    defines: PtrT<WGPUShaderDefine>;
};
export const WGPUShaderModuleGLSLDescriptor_FFI =
    Pointer as PtrT<WGPUShaderModuleGLSLDescriptor>;

export type WGPURegistryReport = {
    numAllocated: size_t;
    numKeptFromUser: size_t;
    numReleasedFromUser: size_t;
    numError: size_t;
    elementSize: size_t;
};
export const WGPURegistryReport_FFI = Pointer as PtrT<WGPURegistryReport>;

export type WGPUHubReport = {
    adapters: WGPURegistryReport;
    devices: WGPURegistryReport;
    queues: WGPURegistryReport;
    pipelineLayouts: WGPURegistryReport;
    shaderModules: WGPURegistryReport;
    bindGroupLayouts: WGPURegistryReport;
    bindGroups: WGPURegistryReport;
    commandBuffers: WGPURegistryReport;
    renderBundles: WGPURegistryReport;
    renderPipelines: WGPURegistryReport;
    computePipelines: WGPURegistryReport;
    querySets: WGPURegistryReport;
    buffers: WGPURegistryReport;
    textures: WGPURegistryReport;
    textureViews: WGPURegistryReport;
    samplers: WGPURegistryReport;
};
export const WGPUHubReport_FFI = Pointer as PtrT<WGPUHubReport>;

export type WGPUGlobalReport = {
    surfaces: WGPURegistryReport;
    backendType: WGPUBackendType;
    vulkan: WGPUHubReport;
    metal: WGPUHubReport;
    dx12: WGPUHubReport;
    gl: WGPUHubReport;
};
export const WGPUGlobalReport_FFI = Pointer as PtrT<WGPUGlobalReport>;

export type WGPUInstanceEnumerateAdapterOptions = {
    nextInChain: ConstPtrT<WGPUChainedStruct>;
    backends: WGPUInstanceBackendFlags;
};
export const WGPUInstanceEnumerateAdapterOptions_FFI =
    Pointer as PtrT<WGPUInstanceEnumerateAdapterOptions>;

export type WGPUBindGroupEntryExtras = {
    chain: WGPUChainedStruct;
    buffers: ConstPtrT<WGPUBuffer>;
    bufferCount: size_t;
    samplers: ConstPtrT<WGPUSampler>;
    samplerCount: size_t;
    textureViews: ConstPtrT<WGPUTextureView>;
    textureViewCount: size_t;
};
export const WGPUBindGroupEntryExtras_FFI =
    Pointer as PtrT<WGPUBindGroupEntryExtras>;

export type WGPUBindGroupLayoutEntryExtras = {
    chain: WGPUChainedStruct;
    count: uint32_t;
};
export const WGPUBindGroupLayoutEntryExtras_FFI =
    Pointer as PtrT<WGPUBindGroupLayoutEntryExtras>;

export type WGPUQuerySetDescriptorExtras = {
    chain: WGPUChainedStruct;
    pipelineStatistics: ConstPtrT<WGPUPipelineStatisticName>;
    pipelineStatisticCount: size_t;
};
export const WGPUQuerySetDescriptorExtras_FFI =
    Pointer as PtrT<WGPUQuerySetDescriptorExtras>;

export type WGPUSurfaceConfigurationExtras = {
    chain: WGPUChainedStruct;
    desiredMaximumFrameLatency: WGPUBool;
};
export const WGPUSurfaceConfigurationExtras_FFI =
    Pointer as PtrT<WGPUSurfaceConfigurationExtras>;

export type WGPULogCallback = (
    arg0: WGPULogLevel,
    arg1: CString,
    arg2: PtrT<void>
) => void;
export const WGPULogCallback_FFI = Pointer as PtrT<WGPULogCallback>;

export function wgpuGenerateReport(
    instance: WGPUInstance,
    report: PtrT<WGPUGlobalReport>
): void {
    let arg0;

    if (
        instance &&
        typeof instance === "object" &&
        !("BYTES_PER_ELEMENT" in instance) &&
        // @ts-ignore
        WGPUInstance_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = instance as any;
    }

    let arg1;

    if (
        report &&
        typeof report === "object" &&
        !("BYTES_PER_ELEMENT" in report) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = report;
    } else {
        arg1 = report as any;
    }

    const result = wgpulib.wgpuGenerateReport(arg0!, arg1!);
    return result as any;
}

export function wgpuInstanceEnumerateAdapters(
    instance: WGPUInstance,
    options:
        | ConstPtrT<WGPUInstanceEnumerateAdapterOptions>
        | DeepPartial<WGPUInstanceEnumerateAdapterOptions>,
    adapters: PtrT<WGPUAdapter>
): size_t {
    let arg0;

    if (
        instance &&
        typeof instance === "object" &&
        !("BYTES_PER_ELEMENT" in instance) &&
        // @ts-ignore
        WGPUInstance_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = instance as any;
    }

    let arg1;

    if (
        options &&
        typeof options === "object" &&
        !("BYTES_PER_ELEMENT" in options) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = writeWGPUInstanceEnumerateAdapterOptions(options as any)
            .typed as any;
    } else {
        arg1 = options as any;
    }

    let arg2;

    if (
        adapters &&
        typeof adapters === "object" &&
        !("BYTES_PER_ELEMENT" in adapters) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = adapters;
    } else {
        arg2 = adapters as any;
    }

    const result = wgpulib.wgpuInstanceEnumerateAdapters(arg0!, arg1!, arg2!);
    return result as any;
}

export function wgpuQueueSubmitForIndex(
    queue: WGPUQueue,
    commandCount: size_t,
    commands: ConstPtrT<WGPUCommandBuffer> | DeepPartial<WGPUCommandBuffer>
): WGPUSubmissionIndex {
    let arg0;

    if (
        queue &&
        typeof queue === "object" &&
        !("BYTES_PER_ELEMENT" in queue) &&
        // @ts-ignore
        WGPUQueue_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = queue as any;
    }

    let arg1;

    if (
        commandCount &&
        typeof commandCount === "object" &&
        !("BYTES_PER_ELEMENT" in commandCount) &&
        // @ts-ignore
        size_t_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = commandCount as any;
    }

    let arg2;

    if (
        commands &&
        typeof commands === "object" &&
        !("BYTES_PER_ELEMENT" in commands) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = writeWGPUCommandBuffer(commands as any).typed as any;
    } else {
        arg2 = commands as any;
    }

    const result = wgpulib.wgpuQueueSubmitForIndex(arg0!, arg1!, arg2!);
    return result as any;
}

export function wgpuDevicePoll(
    device: WGPUDevice,
    wait: WGPUBool,
    wrappedSubmissionIndex:
        | ConstPtrT<WGPUWrappedSubmissionIndex>
        | DeepPartial<WGPUWrappedSubmissionIndex>
): WGPUBool {
    let arg0;

    if (
        device &&
        typeof device === "object" &&
        !("BYTES_PER_ELEMENT" in device) &&
        // @ts-ignore
        WGPUDevice_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = device as any;
    }

    let arg1;

    if (
        wait &&
        typeof wait === "object" &&
        !("BYTES_PER_ELEMENT" in wait) &&
        // @ts-ignore
        WGPUBool_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = wait as any;
    }

    let arg2;

    if (
        wrappedSubmissionIndex &&
        typeof wrappedSubmissionIndex === "object" &&
        !("BYTES_PER_ELEMENT" in wrappedSubmissionIndex) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg2 = writeWGPUWrappedSubmissionIndex(wrappedSubmissionIndex as any)
            .typed as any;
    } else {
        arg2 = wrappedSubmissionIndex as any;
    }

    const result = wgpulib.wgpuDevicePoll(arg0!, arg1!, arg2!);
    return result as any;
}

export function wgpuSetLogCallback(
    callback: WGPULogCallback,
    userdata: PtrT<void>
): void {
    let arg0 = new BunJSCallback(callback, eval("WGPULogCallback_FFI_funcdef"))
        .ptr;
    if (
        callback &&
        typeof callback === "object" &&
        !("BYTES_PER_ELEMENT" in callback) &&
        // @ts-ignore
        WGPULogCallback_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = callback as any;
    }

    let arg1;

    if (
        userdata &&
        typeof userdata === "object" &&
        !("BYTES_PER_ELEMENT" in userdata) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
        arg1 = userdata;
    } else {
        arg1 = userdata as any;
    }

    const result = wgpulib.wgpuSetLogCallback(arg0!, arg1!);
    return result as any;
}

export function wgpuSetLogLevel(level: WGPULogLevel): void {
    let arg0;

    if (
        level &&
        typeof level === "object" &&
        !("BYTES_PER_ELEMENT" in level) &&
        // @ts-ignore
        WGPULogLevel_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = level as any;
    }

    const result = wgpulib.wgpuSetLogLevel(arg0!);
    return result as any;
}

export function wgpuGetVersion(): uint32_t {
    const result = wgpulib.wgpuGetVersion();
    return result as any;
}

export function wgpuRenderPassEncoderSetPushConstants(
    encoder: WGPURenderPassEncoder,
    stages: WGPUShaderStageFlags,
    offset: uint32_t,
    sizeBytes: uint32_t,
    data: ConstPtrT<void> | DeepPartial<void>
): void {
    let arg0;

    if (
        encoder &&
        typeof encoder === "object" &&
        !("BYTES_PER_ELEMENT" in encoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = encoder as any;
    }

    let arg1;

    if (
        stages &&
        typeof stages === "object" &&
        !("BYTES_PER_ELEMENT" in stages) &&
        // @ts-ignore
        WGPUShaderStageFlags_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = stages as any;
    }

    let arg2;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = offset as any;
    }

    let arg3;

    if (
        sizeBytes &&
        typeof sizeBytes === "object" &&
        !("BYTES_PER_ELEMENT" in sizeBytes) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = sizeBytes as any;
    }

    let arg4;

    if (
        data &&
        typeof data === "object" &&
        !("BYTES_PER_ELEMENT" in data) &&
        // @ts-ignore
        Pointer === FFIType.ptr
    ) {
    } else {
        arg4 = data as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderSetPushConstants(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!
    );
    return result as any;
}

export function wgpuRenderPassEncoderMultiDrawIndirect(
    encoder: WGPURenderPassEncoder,
    buffer: WGPUBuffer,
    offset: uint64_t,
    count: uint32_t
): void {
    let arg0;

    if (
        encoder &&
        typeof encoder === "object" &&
        !("BYTES_PER_ELEMENT" in encoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = encoder as any;
    }

    let arg1;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = buffer as any;
    }

    let arg2;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = offset as any;
    }

    let arg3;

    if (
        count &&
        typeof count === "object" &&
        !("BYTES_PER_ELEMENT" in count) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = count as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderMultiDrawIndirect(
        arg0!,
        arg1!,
        arg2!,
        arg3!
    );
    return result as any;
}

export function wgpuRenderPassEncoderMultiDrawIndexedIndirect(
    encoder: WGPURenderPassEncoder,
    buffer: WGPUBuffer,
    offset: uint64_t,
    count: uint32_t
): void {
    let arg0;

    if (
        encoder &&
        typeof encoder === "object" &&
        !("BYTES_PER_ELEMENT" in encoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = encoder as any;
    }

    let arg1;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = buffer as any;
    }

    let arg2;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = offset as any;
    }

    let arg3;

    if (
        count &&
        typeof count === "object" &&
        !("BYTES_PER_ELEMENT" in count) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = count as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderMultiDrawIndexedIndirect(
        arg0!,
        arg1!,
        arg2!,
        arg3!
    );
    return result as any;
}

export function wgpuRenderPassEncoderMultiDrawIndirectCount(
    encoder: WGPURenderPassEncoder,
    buffer: WGPUBuffer,
    offset: uint64_t,
    count_buffer: WGPUBuffer,
    count_buffer_offset: uint64_t,
    max_count: uint32_t
): void {
    let arg0;

    if (
        encoder &&
        typeof encoder === "object" &&
        !("BYTES_PER_ELEMENT" in encoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = encoder as any;
    }

    let arg1;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = buffer as any;
    }

    let arg2;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = offset as any;
    }

    let arg3;

    if (
        count_buffer &&
        typeof count_buffer === "object" &&
        !("BYTES_PER_ELEMENT" in count_buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = count_buffer as any;
    }

    let arg4;

    if (
        count_buffer_offset &&
        typeof count_buffer_offset === "object" &&
        !("BYTES_PER_ELEMENT" in count_buffer_offset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = count_buffer_offset as any;
    }

    let arg5;

    if (
        max_count &&
        typeof max_count === "object" &&
        !("BYTES_PER_ELEMENT" in max_count) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg5 = max_count as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderMultiDrawIndirectCount(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!,
        arg5!
    );
    return result as any;
}

export function wgpuRenderPassEncoderMultiDrawIndexedIndirectCount(
    encoder: WGPURenderPassEncoder,
    buffer: WGPUBuffer,
    offset: uint64_t,
    count_buffer: WGPUBuffer,
    count_buffer_offset: uint64_t,
    max_count: uint32_t
): void {
    let arg0;

    if (
        encoder &&
        typeof encoder === "object" &&
        !("BYTES_PER_ELEMENT" in encoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = encoder as any;
    }

    let arg1;

    if (
        buffer &&
        typeof buffer === "object" &&
        !("BYTES_PER_ELEMENT" in buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = buffer as any;
    }

    let arg2;

    if (
        offset &&
        typeof offset === "object" &&
        !("BYTES_PER_ELEMENT" in offset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = offset as any;
    }

    let arg3;

    if (
        count_buffer &&
        typeof count_buffer === "object" &&
        !("BYTES_PER_ELEMENT" in count_buffer) &&
        // @ts-ignore
        WGPUBuffer_FFI === FFIType.ptr
    ) {
    } else {
        arg3 = count_buffer as any;
    }

    let arg4;

    if (
        count_buffer_offset &&
        typeof count_buffer_offset === "object" &&
        !("BYTES_PER_ELEMENT" in count_buffer_offset) &&
        // @ts-ignore
        uint64_t_FFI === FFIType.ptr
    ) {
    } else {
        arg4 = count_buffer_offset as any;
    }

    let arg5;

    if (
        max_count &&
        typeof max_count === "object" &&
        !("BYTES_PER_ELEMENT" in max_count) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg5 = max_count as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderMultiDrawIndexedIndirectCount(
        arg0!,
        arg1!,
        arg2!,
        arg3!,
        arg4!,
        arg5!
    );
    return result as any;
}

export function wgpuComputePassEncoderBeginPipelineStatisticsQuery(
    computePassEncoder: WGPUComputePassEncoder,
    querySet: WGPUQuerySet,
    queryIndex: uint32_t
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    let arg1;

    if (
        querySet &&
        typeof querySet === "object" &&
        !("BYTES_PER_ELEMENT" in querySet) &&
        // @ts-ignore
        WGPUQuerySet_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = querySet as any;
    }

    let arg2;

    if (
        queryIndex &&
        typeof queryIndex === "object" &&
        !("BYTES_PER_ELEMENT" in queryIndex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = queryIndex as any;
    }

    const result = wgpulib.wgpuComputePassEncoderBeginPipelineStatisticsQuery(
        arg0!,
        arg1!,
        arg2!
    );
    return result as any;
}

export function wgpuComputePassEncoderEndPipelineStatisticsQuery(
    computePassEncoder: WGPUComputePassEncoder
): void {
    let arg0;

    if (
        computePassEncoder &&
        typeof computePassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in computePassEncoder) &&
        // @ts-ignore
        WGPUComputePassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = computePassEncoder as any;
    }

    const result = wgpulib.wgpuComputePassEncoderEndPipelineStatisticsQuery(
        arg0!
    );
    return result as any;
}

export function wgpuRenderPassEncoderBeginPipelineStatisticsQuery(
    renderPassEncoder: WGPURenderPassEncoder,
    querySet: WGPUQuerySet,
    queryIndex: uint32_t
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    let arg1;

    if (
        querySet &&
        typeof querySet === "object" &&
        !("BYTES_PER_ELEMENT" in querySet) &&
        // @ts-ignore
        WGPUQuerySet_FFI === FFIType.ptr
    ) {
    } else {
        arg1 = querySet as any;
    }

    let arg2;

    if (
        queryIndex &&
        typeof queryIndex === "object" &&
        !("BYTES_PER_ELEMENT" in queryIndex) &&
        // @ts-ignore
        uint32_t_FFI === FFIType.ptr
    ) {
    } else {
        arg2 = queryIndex as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderBeginPipelineStatisticsQuery(
        arg0!,
        arg1!,
        arg2!
    );
    return result as any;
}

export function wgpuRenderPassEncoderEndPipelineStatisticsQuery(
    renderPassEncoder: WGPURenderPassEncoder
): void {
    let arg0;

    if (
        renderPassEncoder &&
        typeof renderPassEncoder === "object" &&
        !("BYTES_PER_ELEMENT" in renderPassEncoder) &&
        // @ts-ignore
        WGPURenderPassEncoder_FFI === FFIType.ptr
    ) {
    } else {
        arg0 = renderPassEncoder as any;
    }

    const result = wgpulib.wgpuRenderPassEncoderEndPipelineStatisticsQuery(
        arg0!
    );
    return result as any;
}

const platform = process.platform;
let path: string = "";

if (platform == "darwin") {
    path = import.meta.dir + "/../deps/wgpu/libwgpu_native.dylib";
} else {
    throw new Error("not supported wgpu bindings platform");
}

const wgpulib = dlopen(path, {
    wgpuCreateInstance: {
        returns: WGPUInstance_FFI,
        args: [
            // descriptor
            Pointer,
        ],
    },
    wgpuGetProcAddress: {
        returns: WGPUProc_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // procName
            CString,
        ],
    },
    wgpuAdapterEnumerateFeatures: {
        returns: size_t_FFI,
        args: [
            // adapter
            WGPUAdapter_FFI,
            // features
            Pointer,
        ],
    },
    wgpuAdapterGetLimits: {
        returns: WGPUBool_FFI,
        args: [
            // adapter
            WGPUAdapter_FFI,
            // limits
            Pointer,
        ],
    },
    wgpuAdapterGetProperties: {
        returns: void_FFI,
        args: [
            // adapter
            WGPUAdapter_FFI,
            // properties
            Pointer,
        ],
    },
    wgpuAdapterHasFeature: {
        returns: WGPUBool_FFI,
        args: [
            // adapter
            WGPUAdapter_FFI,
            // feature
            WGPUFeatureName_FFI,
        ],
    },
    wgpuAdapterRequestDevice: {
        returns: void_FFI,
        args: [
            // adapter
            WGPUAdapter_FFI,
            // descriptor
            Pointer,
            // callback
            FFIType.ptr,
            // userdata
            Pointer,
        ],
    },
    wgpuAdapterReference: {
        returns: void_FFI,
        args: [
            // adapter
            WGPUAdapter_FFI,
        ],
    },
    wgpuAdapterRelease: {
        returns: void_FFI,
        args: [
            // adapter
            WGPUAdapter_FFI,
        ],
    },
    wgpuBindGroupSetLabel: {
        returns: void_FFI,
        args: [
            // bindGroup
            WGPUBindGroup_FFI,
            // label
            CString,
        ],
    },
    wgpuBindGroupReference: {
        returns: void_FFI,
        args: [
            // bindGroup
            WGPUBindGroup_FFI,
        ],
    },
    wgpuBindGroupRelease: {
        returns: void_FFI,
        args: [
            // bindGroup
            WGPUBindGroup_FFI,
        ],
    },
    wgpuBindGroupLayoutSetLabel: {
        returns: void_FFI,
        args: [
            // bindGroupLayout
            WGPUBindGroupLayout_FFI,
            // label
            CString,
        ],
    },
    wgpuBindGroupLayoutReference: {
        returns: void_FFI,
        args: [
            // bindGroupLayout
            WGPUBindGroupLayout_FFI,
        ],
    },
    wgpuBindGroupLayoutRelease: {
        returns: void_FFI,
        args: [
            // bindGroupLayout
            WGPUBindGroupLayout_FFI,
        ],
    },
    wgpuBufferDestroy: {
        returns: void_FFI,
        args: [
            // buffer
            WGPUBuffer_FFI,
        ],
    },
    wgpuBufferGetConstMappedRange: {
        returns: Pointer,
        args: [
            // buffer
            WGPUBuffer_FFI,
            // offset
            size_t_FFI,
            // size
            size_t_FFI,
        ],
    },
    wgpuBufferGetMapState: {
        returns: WGPUBufferMapState_FFI,
        args: [
            // buffer
            WGPUBuffer_FFI,
        ],
    },
    wgpuBufferGetMappedRange: {
        returns: Pointer,
        args: [
            // buffer
            WGPUBuffer_FFI,
            // offset
            size_t_FFI,
            // size
            size_t_FFI,
        ],
    },
    wgpuBufferGetSize: {
        returns: uint64_t_FFI,
        args: [
            // buffer
            WGPUBuffer_FFI,
        ],
    },
    wgpuBufferGetUsage: {
        returns: WGPUBufferUsageFlags_FFI,
        args: [
            // buffer
            WGPUBuffer_FFI,
        ],
    },
    wgpuBufferMapAsync: {
        returns: void_FFI,
        args: [
            // buffer
            WGPUBuffer_FFI,
            // mode
            WGPUMapModeFlags_FFI,
            // offset
            size_t_FFI,
            // size
            size_t_FFI,
            // callback
            FFIType.ptr,
            // userdata
            Pointer,
        ],
    },
    wgpuBufferSetLabel: {
        returns: void_FFI,
        args: [
            // buffer
            WGPUBuffer_FFI,
            // label
            CString,
        ],
    },
    wgpuBufferUnmap: {
        returns: void_FFI,
        args: [
            // buffer
            WGPUBuffer_FFI,
        ],
    },
    wgpuBufferReference: {
        returns: void_FFI,
        args: [
            // buffer
            WGPUBuffer_FFI,
        ],
    },
    wgpuBufferRelease: {
        returns: void_FFI,
        args: [
            // buffer
            WGPUBuffer_FFI,
        ],
    },
    wgpuCommandBufferSetLabel: {
        returns: void_FFI,
        args: [
            // commandBuffer
            WGPUCommandBuffer_FFI,
            // label
            CString,
        ],
    },
    wgpuCommandBufferReference: {
        returns: void_FFI,
        args: [
            // commandBuffer
            WGPUCommandBuffer_FFI,
        ],
    },
    wgpuCommandBufferRelease: {
        returns: void_FFI,
        args: [
            // commandBuffer
            WGPUCommandBuffer_FFI,
        ],
    },
    wgpuCommandEncoderBeginComputePass: {
        returns: WGPUComputePassEncoder_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuCommandEncoderBeginRenderPass: {
        returns: WGPURenderPassEncoder_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuCommandEncoderClearBuffer: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // buffer
            WGPUBuffer_FFI,
            // offset
            uint64_t_FFI,
            // size
            uint64_t_FFI,
        ],
    },
    wgpuCommandEncoderCopyBufferToBuffer: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // source
            WGPUBuffer_FFI,
            // sourceOffset
            uint64_t_FFI,
            // destination
            WGPUBuffer_FFI,
            // destinationOffset
            uint64_t_FFI,
            // size
            uint64_t_FFI,
        ],
    },
    wgpuCommandEncoderCopyBufferToTexture: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // source
            Pointer,
            // destination
            Pointer,
            // copySize
            Pointer,
        ],
    },
    wgpuCommandEncoderCopyTextureToBuffer: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // source
            Pointer,
            // destination
            Pointer,
            // copySize
            Pointer,
        ],
    },
    wgpuCommandEncoderCopyTextureToTexture: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // source
            Pointer,
            // destination
            Pointer,
            // copySize
            Pointer,
        ],
    },
    wgpuCommandEncoderFinish: {
        returns: WGPUCommandBuffer_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuCommandEncoderInsertDebugMarker: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // markerLabel
            CString,
        ],
    },
    wgpuCommandEncoderPopDebugGroup: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
        ],
    },
    wgpuCommandEncoderPushDebugGroup: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // groupLabel
            CString,
        ],
    },
    wgpuCommandEncoderResolveQuerySet: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // querySet
            WGPUQuerySet_FFI,
            // firstQuery
            uint32_t_FFI,
            // queryCount
            uint32_t_FFI,
            // destination
            WGPUBuffer_FFI,
            // destinationOffset
            uint64_t_FFI,
        ],
    },
    wgpuCommandEncoderSetLabel: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // label
            CString,
        ],
    },
    wgpuCommandEncoderWriteTimestamp: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
            // querySet
            WGPUQuerySet_FFI,
            // queryIndex
            uint32_t_FFI,
        ],
    },
    wgpuCommandEncoderReference: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
        ],
    },
    wgpuCommandEncoderRelease: {
        returns: void_FFI,
        args: [
            // commandEncoder
            WGPUCommandEncoder_FFI,
        ],
    },
    wgpuComputePassEncoderDispatchWorkgroups: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
            // workgroupCountX
            uint32_t_FFI,
            // workgroupCountY
            uint32_t_FFI,
            // workgroupCountZ
            uint32_t_FFI,
        ],
    },
    wgpuComputePassEncoderDispatchWorkgroupsIndirect: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
            // indirectBuffer
            WGPUBuffer_FFI,
            // indirectOffset
            uint64_t_FFI,
        ],
    },
    wgpuComputePassEncoderEnd: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
        ],
    },
    wgpuComputePassEncoderInsertDebugMarker: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
            // markerLabel
            CString,
        ],
    },
    wgpuComputePassEncoderPopDebugGroup: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
        ],
    },
    wgpuComputePassEncoderPushDebugGroup: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
            // groupLabel
            CString,
        ],
    },
    wgpuComputePassEncoderSetBindGroup: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
            // groupIndex
            uint32_t_FFI,
            // group
            WGPUBindGroup_FFI,
            // dynamicOffsetCount
            size_t_FFI,
            // dynamicOffsets
            Pointer,
        ],
    },
    wgpuComputePassEncoderSetLabel: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
            // label
            CString,
        ],
    },
    wgpuComputePassEncoderSetPipeline: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
            // pipeline
            WGPUComputePipeline_FFI,
        ],
    },
    wgpuComputePassEncoderReference: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
        ],
    },
    wgpuComputePassEncoderRelease: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
        ],
    },
    wgpuComputePipelineGetBindGroupLayout: {
        returns: WGPUBindGroupLayout_FFI,
        args: [
            // computePipeline
            WGPUComputePipeline_FFI,
            // groupIndex
            uint32_t_FFI,
        ],
    },
    wgpuComputePipelineSetLabel: {
        returns: void_FFI,
        args: [
            // computePipeline
            WGPUComputePipeline_FFI,
            // label
            CString,
        ],
    },
    wgpuComputePipelineReference: {
        returns: void_FFI,
        args: [
            // computePipeline
            WGPUComputePipeline_FFI,
        ],
    },
    wgpuComputePipelineRelease: {
        returns: void_FFI,
        args: [
            // computePipeline
            WGPUComputePipeline_FFI,
        ],
    },
    wgpuDeviceCreateBindGroup: {
        returns: WGPUBindGroup_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuDeviceCreateBindGroupLayout: {
        returns: WGPUBindGroupLayout_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuDeviceCreateBuffer: {
        returns: WGPUBuffer_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuDeviceCreateCommandEncoder: {
        returns: WGPUCommandEncoder_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuDeviceCreateComputePipeline: {
        returns: WGPUComputePipeline_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuDeviceCreateComputePipelineAsync: {
        returns: void_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
            // callback
            FFIType.ptr,
            // userdata
            Pointer,
        ],
    },
    wgpuDeviceCreatePipelineLayout: {
        returns: WGPUPipelineLayout_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuDeviceCreateQuerySet: {
        returns: WGPUQuerySet_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuDeviceCreateRenderBundleEncoder: {
        returns: WGPURenderBundleEncoder_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuDeviceCreateRenderPipeline: {
        returns: WGPURenderPipeline_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuDeviceCreateRenderPipelineAsync: {
        returns: void_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
            // callback
            FFIType.ptr,
            // userdata
            Pointer,
        ],
    },
    wgpuDeviceCreateSampler: {
        returns: WGPUSampler_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuDeviceCreateShaderModule: {
        returns: WGPUShaderModule_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuDeviceCreateTexture: {
        returns: WGPUTexture_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuDeviceDestroy: {
        returns: void_FFI,
        args: [
            // device
            WGPUDevice_FFI,
        ],
    },
    wgpuDeviceEnumerateFeatures: {
        returns: size_t_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // features
            Pointer,
        ],
    },
    wgpuDeviceGetLimits: {
        returns: WGPUBool_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // limits
            Pointer,
        ],
    },
    wgpuDeviceGetQueue: {
        returns: WGPUQueue_FFI,
        args: [
            // device
            WGPUDevice_FFI,
        ],
    },
    wgpuDeviceHasFeature: {
        returns: WGPUBool_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // feature
            WGPUFeatureName_FFI,
        ],
    },
    wgpuDevicePopErrorScope: {
        returns: void_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // callback
            FFIType.ptr,
            // userdata
            Pointer,
        ],
    },
    wgpuDevicePushErrorScope: {
        returns: void_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // filter
            WGPUErrorFilter_FFI,
        ],
    },
    wgpuDeviceSetLabel: {
        returns: void_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // label
            CString,
        ],
    },
    wgpuDeviceSetUncapturedErrorCallback: {
        returns: void_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // callback
            FFIType.ptr,
            // userdata
            Pointer,
        ],
    },
    wgpuDeviceReference: {
        returns: void_FFI,
        args: [
            // device
            WGPUDevice_FFI,
        ],
    },
    wgpuDeviceRelease: {
        returns: void_FFI,
        args: [
            // device
            WGPUDevice_FFI,
        ],
    },
    wgpuInstanceCreateSurface: {
        returns: WGPUSurface_FFI,
        args: [
            // instance
            WGPUInstance_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuInstanceProcessEvents: {
        returns: void_FFI,
        args: [
            // instance
            WGPUInstance_FFI,
        ],
    },
    wgpuInstanceRequestAdapter: {
        returns: void_FFI,
        args: [
            // instance
            WGPUInstance_FFI,
            // options
            Pointer,
            // callback
            FFIType.ptr,
            // userdata
            Pointer,
        ],
    },
    wgpuInstanceReference: {
        returns: void_FFI,
        args: [
            // instance
            WGPUInstance_FFI,
        ],
    },
    wgpuInstanceRelease: {
        returns: void_FFI,
        args: [
            // instance
            WGPUInstance_FFI,
        ],
    },
    wgpuPipelineLayoutSetLabel: {
        returns: void_FFI,
        args: [
            // pipelineLayout
            WGPUPipelineLayout_FFI,
            // label
            CString,
        ],
    },
    wgpuPipelineLayoutReference: {
        returns: void_FFI,
        args: [
            // pipelineLayout
            WGPUPipelineLayout_FFI,
        ],
    },
    wgpuPipelineLayoutRelease: {
        returns: void_FFI,
        args: [
            // pipelineLayout
            WGPUPipelineLayout_FFI,
        ],
    },
    wgpuQuerySetDestroy: {
        returns: void_FFI,
        args: [
            // querySet
            WGPUQuerySet_FFI,
        ],
    },
    wgpuQuerySetGetCount: {
        returns: uint32_t_FFI,
        args: [
            // querySet
            WGPUQuerySet_FFI,
        ],
    },
    wgpuQuerySetGetType: {
        returns: WGPUQueryType_FFI,
        args: [
            // querySet
            WGPUQuerySet_FFI,
        ],
    },
    wgpuQuerySetSetLabel: {
        returns: void_FFI,
        args: [
            // querySet
            WGPUQuerySet_FFI,
            // label
            CString,
        ],
    },
    wgpuQuerySetReference: {
        returns: void_FFI,
        args: [
            // querySet
            WGPUQuerySet_FFI,
        ],
    },
    wgpuQuerySetRelease: {
        returns: void_FFI,
        args: [
            // querySet
            WGPUQuerySet_FFI,
        ],
    },
    wgpuQueueOnSubmittedWorkDone: {
        returns: void_FFI,
        args: [
            // queue
            WGPUQueue_FFI,
            // callback
            FFIType.ptr,
            // userdata
            Pointer,
        ],
    },
    wgpuQueueSetLabel: {
        returns: void_FFI,
        args: [
            // queue
            WGPUQueue_FFI,
            // label
            CString,
        ],
    },
    wgpuQueueSubmit: {
        returns: void_FFI,
        args: [
            // queue
            WGPUQueue_FFI,
            // commandCount
            size_t_FFI,
            // commands
            Pointer,
        ],
    },
    wgpuQueueWriteBuffer: {
        returns: void_FFI,
        args: [
            // queue
            WGPUQueue_FFI,
            // buffer
            WGPUBuffer_FFI,
            // bufferOffset
            uint64_t_FFI,
            // data
            Pointer,
            // size
            size_t_FFI,
        ],
    },
    wgpuQueueWriteTexture: {
        returns: void_FFI,
        args: [
            // queue
            WGPUQueue_FFI,
            // destination
            Pointer,
            // data
            Pointer,
            // dataSize
            size_t_FFI,
            // dataLayout
            Pointer,
            // writeSize
            Pointer,
        ],
    },
    wgpuQueueReference: {
        returns: void_FFI,
        args: [
            // queue
            WGPUQueue_FFI,
        ],
    },
    wgpuQueueRelease: {
        returns: void_FFI,
        args: [
            // queue
            WGPUQueue_FFI,
        ],
    },
    wgpuRenderBundleSetLabel: {
        returns: void_FFI,
        args: [
            // renderBundle
            WGPURenderBundle_FFI,
            // label
            CString,
        ],
    },
    wgpuRenderBundleReference: {
        returns: void_FFI,
        args: [
            // renderBundle
            WGPURenderBundle_FFI,
        ],
    },
    wgpuRenderBundleRelease: {
        returns: void_FFI,
        args: [
            // renderBundle
            WGPURenderBundle_FFI,
        ],
    },
    wgpuRenderBundleEncoderDraw: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
            // vertexCount
            uint32_t_FFI,
            // instanceCount
            uint32_t_FFI,
            // firstVertex
            uint32_t_FFI,
            // firstInstance
            uint32_t_FFI,
        ],
    },
    wgpuRenderBundleEncoderDrawIndexed: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
            // indexCount
            uint32_t_FFI,
            // instanceCount
            uint32_t_FFI,
            // firstIndex
            uint32_t_FFI,
            // baseVertex
            int32_t_FFI,
            // firstInstance
            uint32_t_FFI,
        ],
    },
    wgpuRenderBundleEncoderDrawIndexedIndirect: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
            // indirectBuffer
            WGPUBuffer_FFI,
            // indirectOffset
            uint64_t_FFI,
        ],
    },
    wgpuRenderBundleEncoderDrawIndirect: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
            // indirectBuffer
            WGPUBuffer_FFI,
            // indirectOffset
            uint64_t_FFI,
        ],
    },
    wgpuRenderBundleEncoderFinish: {
        returns: WGPURenderBundle_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuRenderBundleEncoderInsertDebugMarker: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
            // markerLabel
            CString,
        ],
    },
    wgpuRenderBundleEncoderPopDebugGroup: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
        ],
    },
    wgpuRenderBundleEncoderPushDebugGroup: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
            // groupLabel
            CString,
        ],
    },
    wgpuRenderBundleEncoderSetBindGroup: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
            // groupIndex
            uint32_t_FFI,
            // group
            WGPUBindGroup_FFI,
            // dynamicOffsetCount
            size_t_FFI,
            // dynamicOffsets
            Pointer,
        ],
    },
    wgpuRenderBundleEncoderSetIndexBuffer: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
            // buffer
            WGPUBuffer_FFI,
            // format
            WGPUIndexFormat_FFI,
            // offset
            uint64_t_FFI,
            // size
            uint64_t_FFI,
        ],
    },
    wgpuRenderBundleEncoderSetLabel: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
            // label
            CString,
        ],
    },
    wgpuRenderBundleEncoderSetPipeline: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
            // pipeline
            WGPURenderPipeline_FFI,
        ],
    },
    wgpuRenderBundleEncoderSetVertexBuffer: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
            // slot
            uint32_t_FFI,
            // buffer
            WGPUBuffer_FFI,
            // offset
            uint64_t_FFI,
            // size
            uint64_t_FFI,
        ],
    },
    wgpuRenderBundleEncoderReference: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
        ],
    },
    wgpuRenderBundleEncoderRelease: {
        returns: void_FFI,
        args: [
            // renderBundleEncoder
            WGPURenderBundleEncoder_FFI,
        ],
    },
    wgpuRenderPassEncoderBeginOcclusionQuery: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // queryIndex
            uint32_t_FFI,
        ],
    },
    wgpuRenderPassEncoderDraw: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // vertexCount
            uint32_t_FFI,
            // instanceCount
            uint32_t_FFI,
            // firstVertex
            uint32_t_FFI,
            // firstInstance
            uint32_t_FFI,
        ],
    },
    wgpuRenderPassEncoderDrawIndexed: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // indexCount
            uint32_t_FFI,
            // instanceCount
            uint32_t_FFI,
            // firstIndex
            uint32_t_FFI,
            // baseVertex
            int32_t_FFI,
            // firstInstance
            uint32_t_FFI,
        ],
    },
    wgpuRenderPassEncoderDrawIndexedIndirect: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // indirectBuffer
            WGPUBuffer_FFI,
            // indirectOffset
            uint64_t_FFI,
        ],
    },
    wgpuRenderPassEncoderDrawIndirect: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // indirectBuffer
            WGPUBuffer_FFI,
            // indirectOffset
            uint64_t_FFI,
        ],
    },
    wgpuRenderPassEncoderEnd: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
        ],
    },
    wgpuRenderPassEncoderEndOcclusionQuery: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
        ],
    },
    wgpuRenderPassEncoderExecuteBundles: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // bundleCount
            size_t_FFI,
            // bundles
            Pointer,
        ],
    },
    wgpuRenderPassEncoderInsertDebugMarker: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // markerLabel
            CString,
        ],
    },
    wgpuRenderPassEncoderPopDebugGroup: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
        ],
    },
    wgpuRenderPassEncoderPushDebugGroup: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // groupLabel
            CString,
        ],
    },
    wgpuRenderPassEncoderSetBindGroup: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // groupIndex
            uint32_t_FFI,
            // group
            WGPUBindGroup_FFI,
            // dynamicOffsetCount
            size_t_FFI,
            // dynamicOffsets
            Pointer,
        ],
    },
    wgpuRenderPassEncoderSetBlendConstant: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // color
            Pointer,
        ],
    },
    wgpuRenderPassEncoderSetIndexBuffer: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // buffer
            WGPUBuffer_FFI,
            // format
            WGPUIndexFormat_FFI,
            // offset
            uint64_t_FFI,
            // size
            uint64_t_FFI,
        ],
    },
    wgpuRenderPassEncoderSetLabel: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // label
            CString,
        ],
    },
    wgpuRenderPassEncoderSetPipeline: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // pipeline
            WGPURenderPipeline_FFI,
        ],
    },
    wgpuRenderPassEncoderSetScissorRect: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // x
            uint32_t_FFI,
            // y
            uint32_t_FFI,
            // width
            uint32_t_FFI,
            // height
            uint32_t_FFI,
        ],
    },
    wgpuRenderPassEncoderSetStencilReference: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // reference
            uint32_t_FFI,
        ],
    },
    wgpuRenderPassEncoderSetVertexBuffer: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // slot
            uint32_t_FFI,
            // buffer
            WGPUBuffer_FFI,
            // offset
            uint64_t_FFI,
            // size
            uint64_t_FFI,
        ],
    },
    wgpuRenderPassEncoderSetViewport: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // x
            float_FFI,
            // y
            float_FFI,
            // width
            float_FFI,
            // height
            float_FFI,
            // minDepth
            float_FFI,
            // maxDepth
            float_FFI,
        ],
    },
    wgpuRenderPassEncoderReference: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
        ],
    },
    wgpuRenderPassEncoderRelease: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
        ],
    },
    wgpuRenderPipelineGetBindGroupLayout: {
        returns: WGPUBindGroupLayout_FFI,
        args: [
            // renderPipeline
            WGPURenderPipeline_FFI,
            // groupIndex
            uint32_t_FFI,
        ],
    },
    wgpuRenderPipelineSetLabel: {
        returns: void_FFI,
        args: [
            // renderPipeline
            WGPURenderPipeline_FFI,
            // label
            CString,
        ],
    },
    wgpuRenderPipelineReference: {
        returns: void_FFI,
        args: [
            // renderPipeline
            WGPURenderPipeline_FFI,
        ],
    },
    wgpuRenderPipelineRelease: {
        returns: void_FFI,
        args: [
            // renderPipeline
            WGPURenderPipeline_FFI,
        ],
    },
    wgpuSamplerSetLabel: {
        returns: void_FFI,
        args: [
            // sampler
            WGPUSampler_FFI,
            // label
            CString,
        ],
    },
    wgpuSamplerReference: {
        returns: void_FFI,
        args: [
            // sampler
            WGPUSampler_FFI,
        ],
    },
    wgpuSamplerRelease: {
        returns: void_FFI,
        args: [
            // sampler
            WGPUSampler_FFI,
        ],
    },
    wgpuShaderModuleGetCompilationInfo: {
        returns: void_FFI,
        args: [
            // shaderModule
            WGPUShaderModule_FFI,
            // callback
            FFIType.ptr,
            // userdata
            Pointer,
        ],
    },
    wgpuShaderModuleSetLabel: {
        returns: void_FFI,
        args: [
            // shaderModule
            WGPUShaderModule_FFI,
            // label
            CString,
        ],
    },
    wgpuShaderModuleReference: {
        returns: void_FFI,
        args: [
            // shaderModule
            WGPUShaderModule_FFI,
        ],
    },
    wgpuShaderModuleRelease: {
        returns: void_FFI,
        args: [
            // shaderModule
            WGPUShaderModule_FFI,
        ],
    },
    wgpuSurfaceConfigure: {
        returns: void_FFI,
        args: [
            // surface
            WGPUSurface_FFI,
            // config
            Pointer,
        ],
    },
    wgpuSurfaceGetCapabilities: {
        returns: void_FFI,
        args: [
            // surface
            WGPUSurface_FFI,
            // adapter
            WGPUAdapter_FFI,
            // capabilities
            Pointer,
        ],
    },
    wgpuSurfaceGetCurrentTexture: {
        returns: void_FFI,
        args: [
            // surface
            WGPUSurface_FFI,
            // surfaceTexture
            Pointer,
        ],
    },
    wgpuSurfaceGetPreferredFormat: {
        returns: WGPUTextureFormat_FFI,
        args: [
            // surface
            WGPUSurface_FFI,
            // adapter
            WGPUAdapter_FFI,
        ],
    },
    wgpuSurfacePresent: {
        returns: void_FFI,
        args: [
            // surface
            WGPUSurface_FFI,
        ],
    },
    wgpuSurfaceUnconfigure: {
        returns: void_FFI,
        args: [
            // surface
            WGPUSurface_FFI,
        ],
    },
    wgpuSurfaceReference: {
        returns: void_FFI,
        args: [
            // surface
            WGPUSurface_FFI,
        ],
    },
    wgpuSurfaceRelease: {
        returns: void_FFI,
        args: [
            // surface
            WGPUSurface_FFI,
        ],
    },
    wgpuSurfaceCapabilitiesFreeMembers: {
        returns: void_FFI,
        args: [
            // capabilities
            FFIType.ptr,
        ],
    },
    wgpuTextureCreateView: {
        returns: WGPUTextureView_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
            // descriptor
            Pointer,
        ],
    },
    wgpuTextureDestroy: {
        returns: void_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
        ],
    },
    wgpuTextureGetDepthOrArrayLayers: {
        returns: uint32_t_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
        ],
    },
    wgpuTextureGetDimension: {
        returns: WGPUTextureDimension_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
        ],
    },
    wgpuTextureGetFormat: {
        returns: WGPUTextureFormat_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
        ],
    },
    wgpuTextureGetHeight: {
        returns: uint32_t_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
        ],
    },
    wgpuTextureGetMipLevelCount: {
        returns: uint32_t_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
        ],
    },
    wgpuTextureGetSampleCount: {
        returns: uint32_t_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
        ],
    },
    wgpuTextureGetUsage: {
        returns: WGPUTextureUsageFlags_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
        ],
    },
    wgpuTextureGetWidth: {
        returns: uint32_t_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
        ],
    },
    wgpuTextureSetLabel: {
        returns: void_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
            // label
            CString,
        ],
    },
    wgpuTextureReference: {
        returns: void_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
        ],
    },
    wgpuTextureRelease: {
        returns: void_FFI,
        args: [
            // texture
            WGPUTexture_FFI,
        ],
    },
    wgpuTextureViewSetLabel: {
        returns: void_FFI,
        args: [
            // textureView
            WGPUTextureView_FFI,
            // label
            CString,
        ],
    },
    wgpuTextureViewReference: {
        returns: void_FFI,
        args: [
            // textureView
            WGPUTextureView_FFI,
        ],
    },
    wgpuTextureViewRelease: {
        returns: void_FFI,
        args: [
            // textureView
            WGPUTextureView_FFI,
        ],
    },
    wgpuGenerateReport: {
        returns: void_FFI,
        args: [
            // instance
            WGPUInstance_FFI,
            // report
            Pointer,
        ],
    },
    wgpuInstanceEnumerateAdapters: {
        returns: size_t_FFI,
        args: [
            // instance
            WGPUInstance_FFI,
            // options
            Pointer,
            // adapters
            Pointer,
        ],
    },
    wgpuQueueSubmitForIndex: {
        returns: WGPUSubmissionIndex_FFI,
        args: [
            // queue
            WGPUQueue_FFI,
            // commandCount
            size_t_FFI,
            // commands
            Pointer,
        ],
    },
    wgpuDevicePoll: {
        returns: WGPUBool_FFI,
        args: [
            // device
            WGPUDevice_FFI,
            // wait
            WGPUBool_FFI,
            // wrappedSubmissionIndex
            Pointer,
        ],
    },
    wgpuSetLogCallback: {
        returns: void_FFI,
        args: [
            // callback
            FFIType.ptr,
            // userdata
            Pointer,
        ],
    },
    wgpuSetLogLevel: {
        returns: void_FFI,
        args: [
            // level
            WGPULogLevel_FFI,
        ],
    },
    wgpuGetVersion: {
        returns: uint32_t_FFI,
        args: [],
    },
    wgpuRenderPassEncoderSetPushConstants: {
        returns: void_FFI,
        args: [
            // encoder
            WGPURenderPassEncoder_FFI,
            // stages
            WGPUShaderStageFlags_FFI,
            // offset
            uint32_t_FFI,
            // sizeBytes
            uint32_t_FFI,
            // data
            Pointer,
        ],
    },
    wgpuRenderPassEncoderMultiDrawIndirect: {
        returns: void_FFI,
        args: [
            // encoder
            WGPURenderPassEncoder_FFI,
            // buffer
            WGPUBuffer_FFI,
            // offset
            uint64_t_FFI,
            // count
            uint32_t_FFI,
        ],
    },
    wgpuRenderPassEncoderMultiDrawIndexedIndirect: {
        returns: void_FFI,
        args: [
            // encoder
            WGPURenderPassEncoder_FFI,
            // buffer
            WGPUBuffer_FFI,
            // offset
            uint64_t_FFI,
            // count
            uint32_t_FFI,
        ],
    },
    wgpuRenderPassEncoderMultiDrawIndirectCount: {
        returns: void_FFI,
        args: [
            // encoder
            WGPURenderPassEncoder_FFI,
            // buffer
            WGPUBuffer_FFI,
            // offset
            uint64_t_FFI,
            // count_buffer
            WGPUBuffer_FFI,
            // count_buffer_offset
            uint64_t_FFI,
            // max_count
            uint32_t_FFI,
        ],
    },
    wgpuRenderPassEncoderMultiDrawIndexedIndirectCount: {
        returns: void_FFI,
        args: [
            // encoder
            WGPURenderPassEncoder_FFI,
            // buffer
            WGPUBuffer_FFI,
            // offset
            uint64_t_FFI,
            // count_buffer
            WGPUBuffer_FFI,
            // count_buffer_offset
            uint64_t_FFI,
            // max_count
            uint32_t_FFI,
        ],
    },
    wgpuComputePassEncoderBeginPipelineStatisticsQuery: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
            // querySet
            WGPUQuerySet_FFI,
            // queryIndex
            uint32_t_FFI,
        ],
    },
    wgpuComputePassEncoderEndPipelineStatisticsQuery: {
        returns: void_FFI,
        args: [
            // computePassEncoder
            WGPUComputePassEncoder_FFI,
        ],
    },
    wgpuRenderPassEncoderBeginPipelineStatisticsQuery: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
            // querySet
            WGPUQuerySet_FFI,
            // queryIndex
            uint32_t_FFI,
        ],
    },
    wgpuRenderPassEncoderEndPipelineStatisticsQuery: {
        returns: void_FFI,
        args: [
            // renderPassEncoder
            WGPURenderPassEncoder_FFI,
        ],
    },
}).symbols;

export function writesize_t(
    x: size_t,
    dataView?: DataViewExt
): DataViewExt<size_t> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x), true);
    return dataView as any;
}
export function writeint32_t(
    x: int32_t,
    dataView?: DataViewExt
): DataViewExt<int32_t> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    if (!x) return dataView as any;
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeuint16_t(
    x: uint16_t,
    dataView?: DataViewExt
): DataViewExt<uint16_t> {
    if (!dataView) dataView = DataViewExt.alloc(2);
    if (!x) return dataView as any;
    dataView.setUint16(0, x, true);
    return dataView as any;
}
export function writeuint32_t(
    x: uint32_t,
    dataView?: DataViewExt
): DataViewExt<uint32_t> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    if (!x) return dataView as any;
    dataView.setUint32(0, x, true);
    return dataView as any;
}
export function writeuint64_t(
    x: uint64_t,
    dataView?: DataViewExt
): DataViewExt<uint64_t> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x), true);
    return dataView as any;
}
export function writedouble(
    x: double,
    dataView?: DataViewExt
): DataViewExt<double> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    if (!x) return dataView as any;
    dataView.setFloat64(0, x, true);
    return dataView as any;
}
export function writefloat(
    x: float,
    dataView?: DataViewExt
): DataViewExt<float> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    if (!x) return dataView as any;
    dataView.setFloat32(0, x, true);
    return dataView as any;
}
export const writeWGPUFlags = writeuint32_t;
export const writeWGPUBool = writeuint32_t;
export function writeWGPUAdapter(
    x: WGPUAdapter,
    dataView?: DataViewExt
): DataViewExt<WGPUAdapter> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUBindGroup(
    x: WGPUBindGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUBindGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUBindGroupLayout(
    x: WGPUBindGroupLayout,
    dataView?: DataViewExt
): DataViewExt<WGPUBindGroupLayout> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUBuffer(
    x: WGPUBuffer,
    dataView?: DataViewExt
): DataViewExt<WGPUBuffer> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUCommandBuffer(
    x: WGPUCommandBuffer,
    dataView?: DataViewExt
): DataViewExt<WGPUCommandBuffer> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUCommandEncoder(
    x: WGPUCommandEncoder,
    dataView?: DataViewExt
): DataViewExt<WGPUCommandEncoder> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUComputePassEncoder(
    x: WGPUComputePassEncoder,
    dataView?: DataViewExt
): DataViewExt<WGPUComputePassEncoder> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUComputePipeline(
    x: WGPUComputePipeline,
    dataView?: DataViewExt
): DataViewExt<WGPUComputePipeline> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUDevice(
    x: WGPUDevice,
    dataView?: DataViewExt
): DataViewExt<WGPUDevice> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUInstance(
    x: WGPUInstance,
    dataView?: DataViewExt
): DataViewExt<WGPUInstance> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUPipelineLayout(
    x: WGPUPipelineLayout,
    dataView?: DataViewExt
): DataViewExt<WGPUPipelineLayout> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUQuerySet(
    x: WGPUQuerySet,
    dataView?: DataViewExt
): DataViewExt<WGPUQuerySet> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUQueue(
    x: WGPUQueue,
    dataView?: DataViewExt
): DataViewExt<WGPUQueue> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPURenderBundle(
    x: WGPURenderBundle,
    dataView?: DataViewExt
): DataViewExt<WGPURenderBundle> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPURenderBundleEncoder(
    x: WGPURenderBundleEncoder,
    dataView?: DataViewExt
): DataViewExt<WGPURenderBundleEncoder> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPURenderPassEncoder(
    x: WGPURenderPassEncoder,
    dataView?: DataViewExt
): DataViewExt<WGPURenderPassEncoder> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPURenderPipeline(
    x: WGPURenderPipeline,
    dataView?: DataViewExt
): DataViewExt<WGPURenderPipeline> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUSampler(
    x: WGPUSampler,
    dataView?: DataViewExt
): DataViewExt<WGPUSampler> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUShaderModule(
    x: WGPUShaderModule,
    dataView?: DataViewExt
): DataViewExt<WGPUShaderModule> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUSurface(
    x: WGPUSurface,
    dataView?: DataViewExt
): DataViewExt<WGPUSurface> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUTexture(
    x: WGPUTexture,
    dataView?: DataViewExt
): DataViewExt<WGPUTexture> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUTextureView(
    x: WGPUTextureView,
    dataView?: DataViewExt
): DataViewExt<WGPUTextureView> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    dataView.setBigUint64(0, BigInt(x!), true);
    return dataView as any;
}
export function writeWGPUAdapterType(
    x: WGPUAdapterType,
    dataView?: DataViewExt
): DataViewExt<WGPUAdapterType> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUAddressMode(
    x: WGPUAddressMode,
    dataView?: DataViewExt
): DataViewExt<WGPUAddressMode> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUBackendType(
    x: WGPUBackendType,
    dataView?: DataViewExt
): DataViewExt<WGPUBackendType> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUBlendFactor(
    x: WGPUBlendFactor,
    dataView?: DataViewExt
): DataViewExt<WGPUBlendFactor> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUBlendOperation(
    x: WGPUBlendOperation,
    dataView?: DataViewExt
): DataViewExt<WGPUBlendOperation> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUBufferBindingType(
    x: WGPUBufferBindingType,
    dataView?: DataViewExt
): DataViewExt<WGPUBufferBindingType> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUBufferMapAsyncStatus(
    x: WGPUBufferMapAsyncStatus,
    dataView?: DataViewExt
): DataViewExt<WGPUBufferMapAsyncStatus> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUBufferMapState(
    x: WGPUBufferMapState,
    dataView?: DataViewExt
): DataViewExt<WGPUBufferMapState> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUCompareFunction(
    x: WGPUCompareFunction,
    dataView?: DataViewExt
): DataViewExt<WGPUCompareFunction> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUCompilationInfoRequestStatus(
    x: WGPUCompilationInfoRequestStatus,
    dataView?: DataViewExt
): DataViewExt<WGPUCompilationInfoRequestStatus> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUCompilationMessageType(
    x: WGPUCompilationMessageType,
    dataView?: DataViewExt
): DataViewExt<WGPUCompilationMessageType> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUCompositeAlphaMode(
    x: WGPUCompositeAlphaMode,
    dataView?: DataViewExt
): DataViewExt<WGPUCompositeAlphaMode> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUCreatePipelineAsyncStatus(
    x: WGPUCreatePipelineAsyncStatus,
    dataView?: DataViewExt
): DataViewExt<WGPUCreatePipelineAsyncStatus> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUCullMode(
    x: WGPUCullMode,
    dataView?: DataViewExt
): DataViewExt<WGPUCullMode> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUDeviceLostReason(
    x: WGPUDeviceLostReason,
    dataView?: DataViewExt
): DataViewExt<WGPUDeviceLostReason> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUErrorFilter(
    x: WGPUErrorFilter,
    dataView?: DataViewExt
): DataViewExt<WGPUErrorFilter> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUErrorType(
    x: WGPUErrorType,
    dataView?: DataViewExt
): DataViewExt<WGPUErrorType> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUFeatureName(
    x: WGPUFeatureName,
    dataView?: DataViewExt
): DataViewExt<WGPUFeatureName> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUFilterMode(
    x: WGPUFilterMode,
    dataView?: DataViewExt
): DataViewExt<WGPUFilterMode> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUFrontFace(
    x: WGPUFrontFace,
    dataView?: DataViewExt
): DataViewExt<WGPUFrontFace> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUIndexFormat(
    x: WGPUIndexFormat,
    dataView?: DataViewExt
): DataViewExt<WGPUIndexFormat> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPULoadOp(
    x: WGPULoadOp,
    dataView?: DataViewExt
): DataViewExt<WGPULoadOp> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUMipmapFilterMode(
    x: WGPUMipmapFilterMode,
    dataView?: DataViewExt
): DataViewExt<WGPUMipmapFilterMode> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUPowerPreference(
    x: WGPUPowerPreference,
    dataView?: DataViewExt
): DataViewExt<WGPUPowerPreference> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUPresentMode(
    x: WGPUPresentMode,
    dataView?: DataViewExt
): DataViewExt<WGPUPresentMode> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUPrimitiveTopology(
    x: WGPUPrimitiveTopology,
    dataView?: DataViewExt
): DataViewExt<WGPUPrimitiveTopology> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUQueryType(
    x: WGPUQueryType,
    dataView?: DataViewExt
): DataViewExt<WGPUQueryType> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUQueueWorkDoneStatus(
    x: WGPUQueueWorkDoneStatus,
    dataView?: DataViewExt
): DataViewExt<WGPUQueueWorkDoneStatus> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPURequestAdapterStatus(
    x: WGPURequestAdapterStatus,
    dataView?: DataViewExt
): DataViewExt<WGPURequestAdapterStatus> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPURequestDeviceStatus(
    x: WGPURequestDeviceStatus,
    dataView?: DataViewExt
): DataViewExt<WGPURequestDeviceStatus> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUSType(
    x: WGPUSType,
    dataView?: DataViewExt
): DataViewExt<WGPUSType> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUSamplerBindingType(
    x: WGPUSamplerBindingType,
    dataView?: DataViewExt
): DataViewExt<WGPUSamplerBindingType> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUStencilOperation(
    x: WGPUStencilOperation,
    dataView?: DataViewExt
): DataViewExt<WGPUStencilOperation> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUStorageTextureAccess(
    x: WGPUStorageTextureAccess,
    dataView?: DataViewExt
): DataViewExt<WGPUStorageTextureAccess> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUStoreOp(
    x: WGPUStoreOp,
    dataView?: DataViewExt
): DataViewExt<WGPUStoreOp> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUSurfaceGetCurrentTextureStatus(
    x: WGPUSurfaceGetCurrentTextureStatus,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceGetCurrentTextureStatus> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUTextureAspect(
    x: WGPUTextureAspect,
    dataView?: DataViewExt
): DataViewExt<WGPUTextureAspect> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUTextureDimension(
    x: WGPUTextureDimension,
    dataView?: DataViewExt
): DataViewExt<WGPUTextureDimension> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUTextureFormat(
    x: WGPUTextureFormat,
    dataView?: DataViewExt
): DataViewExt<WGPUTextureFormat> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUTextureSampleType(
    x: WGPUTextureSampleType,
    dataView?: DataViewExt
): DataViewExt<WGPUTextureSampleType> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUTextureViewDimension(
    x: WGPUTextureViewDimension,
    dataView?: DataViewExt
): DataViewExt<WGPUTextureViewDimension> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUVertexFormat(
    x: WGPUVertexFormat,
    dataView?: DataViewExt
): DataViewExt<WGPUVertexFormat> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUVertexStepMode(
    x: WGPUVertexStepMode,
    dataView?: DataViewExt
): DataViewExt<WGPUVertexStepMode> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUBufferUsage(
    x: WGPUBufferUsage,
    dataView?: DataViewExt
): DataViewExt<WGPUBufferUsage> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export const writeWGPUBufferUsageFlags = writeWGPUFlags;
export function writeWGPUColorWriteMask(
    x: WGPUColorWriteMask,
    dataView?: DataViewExt
): DataViewExt<WGPUColorWriteMask> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export const writeWGPUColorWriteMaskFlags = writeWGPUFlags;
export function writeWGPUMapMode(
    x: WGPUMapMode,
    dataView?: DataViewExt
): DataViewExt<WGPUMapMode> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export const writeWGPUMapModeFlags = writeWGPUFlags;
export function writeWGPUShaderStage(
    x: WGPUShaderStage,
    dataView?: DataViewExt
): DataViewExt<WGPUShaderStage> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export const writeWGPUShaderStageFlags = writeWGPUFlags;
export function writeWGPUTextureUsage(
    x: WGPUTextureUsage,
    dataView?: DataViewExt
): DataViewExt<WGPUTextureUsage> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export const writeWGPUTextureUsageFlags = writeWGPUFlags;

export function writeWGPUBufferMapCallback(
    x: WGPUBufferMapCallback,
    dataView?: DataViewExt
): DataViewExt<WGPUBufferMapCallback> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPUBufferMapCallback_funcdef"))
        .ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUCompilationInfoCallback(
    x: WGPUCompilationInfoCallback,
    dataView?: DataViewExt
): DataViewExt<WGPUCompilationInfoCallback> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUCompilationInfoCallback_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUCreateComputePipelineAsyncCallback(
    x: WGPUCreateComputePipelineAsyncCallback,
    dataView?: DataViewExt
): DataViewExt<WGPUCreateComputePipelineAsyncCallback> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUCreateComputePipelineAsyncCallback_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUCreateRenderPipelineAsyncCallback(
    x: WGPUCreateRenderPipelineAsyncCallback,
    dataView?: DataViewExt
): DataViewExt<WGPUCreateRenderPipelineAsyncCallback> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUCreateRenderPipelineAsyncCallback_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUDeviceLostCallback(
    x: WGPUDeviceLostCallback,
    dataView?: DataViewExt
): DataViewExt<WGPUDeviceLostCallback> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUDeviceLostCallback_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUErrorCallback(
    x: WGPUErrorCallback,
    dataView?: DataViewExt
): DataViewExt<WGPUErrorCallback> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPUErrorCallback_funcdef"))
        .ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProc(
    x: WGPUProc,
    dataView?: DataViewExt
): DataViewExt<WGPUProc> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPUProc_funcdef")).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUQueueWorkDoneCallback(
    x: WGPUQueueWorkDoneCallback,
    dataView?: DataViewExt
): DataViewExt<WGPUQueueWorkDoneCallback> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUQueueWorkDoneCallback_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPURequestAdapterCallback(
    x: WGPURequestAdapterCallback,
    dataView?: DataViewExt
): DataViewExt<WGPURequestAdapterCallback> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPURequestAdapterCallback_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPURequestDeviceCallback(
    x: WGPURequestDeviceCallback,
    dataView?: DataViewExt
): DataViewExt<WGPURequestDeviceCallback> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPURequestDeviceCallback_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}
export function writeWGPUChainedStruct(
    x: DeepPartial<WGPUChainedStruct>,
    dataView?: DataViewExt
): DataViewExt<WGPUChainedStruct> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.next)) {
        writeConstPtrT<WGPUChainedStruct>(x.next! as any, dataView.subview(0));
    } else {
        writeWGPUChainedStruct(x.next! as any, dataView.subview(0));
    }

    writeWGPUSType(x.sType! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUChainedStructOut(
    x: DeepPartial<WGPUChainedStructOut>,
    dataView?: DataViewExt
): DataViewExt<WGPUChainedStructOut> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.next)) {
        writePtrT<WGPUChainedStructOut>(x.next! as any, dataView.subview(0));
    } else {
        writeWGPUChainedStructOut(x.next! as any, dataView.subview(0));
    }

    writeWGPUSType(x.sType! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUAdapterProperties(
    x: DeepPartial<WGPUAdapterProperties>,
    dataView?: DataViewExt
): DataViewExt<WGPUAdapterProperties> {
    if (!dataView) dataView = DataViewExt.alloc(64);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writePtrT<WGPUChainedStructOut>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStructOut(x.nextInChain! as any, dataView.subview(0));
    }

    writeuint32_t(x.vendorID! as any, dataView.subview(8));
    writeCString(x.vendorName! as any, dataView.subview(16));
    writeCString(x.architecture! as any, dataView.subview(24));
    writeuint32_t(x.deviceID! as any, dataView.subview(32));
    writeCString(x.name! as any, dataView.subview(40));
    writeCString(x.driverDescription! as any, dataView.subview(48));
    writeWGPUAdapterType(x.adapterType! as any, dataView.subview(56));
    writeWGPUBackendType(x.backendType! as any, dataView.subview(60));
    return dataView as any;
}
export function writeWGPUBindGroupEntry(
    x: DeepPartial<WGPUBindGroupEntry>,
    dataView?: DataViewExt
): DataViewExt<WGPUBindGroupEntry> {
    if (!dataView) dataView = DataViewExt.alloc(56);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeuint32_t(x.binding! as any, dataView.subview(8));
    writeWGPUBuffer(x.buffer! as any, dataView.subview(16));
    writeuint64_t(x.offset! as any, dataView.subview(24));
    writeuint64_t(x.size! as any, dataView.subview(32));
    writeWGPUSampler(x.sampler! as any, dataView.subview(40));
    writeWGPUTextureView(x.textureView! as any, dataView.subview(48));
    return dataView as any;
}
export function writeWGPUBlendComponent(
    x: DeepPartial<WGPUBlendComponent>,
    dataView?: DataViewExt
): DataViewExt<WGPUBlendComponent> {
    if (!dataView) dataView = DataViewExt.alloc(12);
    if (!x) return dataView as any;
    writeWGPUBlendOperation(x.operation! as any, dataView.subview(0));
    writeWGPUBlendFactor(x.srcFactor! as any, dataView.subview(4));
    writeWGPUBlendFactor(x.dstFactor! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUBufferBindingLayout(
    x: DeepPartial<WGPUBufferBindingLayout>,
    dataView?: DataViewExt
): DataViewExt<WGPUBufferBindingLayout> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUBufferBindingType(x.type! as any, dataView.subview(8));
    writeWGPUBool(x.hasDynamicOffset! as any, dataView.subview(12));
    writeuint64_t(x.minBindingSize! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUBufferDescriptor(
    x: DeepPartial<WGPUBufferDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUBufferDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(40);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writeWGPUBufferUsageFlags(x.usage! as any, dataView.subview(16));
    writeuint64_t(x.size! as any, dataView.subview(24));
    writeWGPUBool(x.mappedAtCreation! as any, dataView.subview(32));
    return dataView as any;
}
export function writeWGPUColor(
    x: DeepPartial<WGPUColor>,
    dataView?: DataViewExt
): DataViewExt<WGPUColor> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;
    writedouble(x.r! as any, dataView.subview(0));
    writedouble(x.g! as any, dataView.subview(8));
    writedouble(x.b! as any, dataView.subview(16));
    writedouble(x.a! as any, dataView.subview(24));
    return dataView as any;
}
export function writeWGPUCommandBufferDescriptor(
    x: DeepPartial<WGPUCommandBufferDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUCommandBufferDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUCommandEncoderDescriptor(
    x: DeepPartial<WGPUCommandEncoderDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUCommandEncoderDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUCompilationMessage(
    x: DeepPartial<WGPUCompilationMessage>,
    dataView?: DataViewExt
): DataViewExt<WGPUCompilationMessage> {
    if (!dataView) dataView = DataViewExt.alloc(80);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.message! as any, dataView.subview(8));
    writeWGPUCompilationMessageType(x.type! as any, dataView.subview(16));
    writeuint64_t(x.lineNum! as any, dataView.subview(24));
    writeuint64_t(x.linePos! as any, dataView.subview(32));
    writeuint64_t(x.offset! as any, dataView.subview(40));
    writeuint64_t(x.length! as any, dataView.subview(48));
    writeuint64_t(x.utf16LinePos! as any, dataView.subview(56));
    writeuint64_t(x.utf16Offset! as any, dataView.subview(64));
    writeuint64_t(x.utf16Length! as any, dataView.subview(72));
    return dataView as any;
}
export function writeWGPUComputePassTimestampWrites(
    x: DeepPartial<WGPUComputePassTimestampWrites>,
    dataView?: DataViewExt
): DataViewExt<WGPUComputePassTimestampWrites> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;
    writeWGPUQuerySet(x.querySet! as any, dataView.subview(0));
    writeuint32_t(x.beginningOfPassWriteIndex! as any, dataView.subview(8));
    writeuint32_t(x.endOfPassWriteIndex! as any, dataView.subview(12));
    return dataView as any;
}
export function writeWGPUConstantEntry(
    x: DeepPartial<WGPUConstantEntry>,
    dataView?: DataViewExt
): DataViewExt<WGPUConstantEntry> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.key! as any, dataView.subview(8));
    writedouble(x.value! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUExtent3D(
    x: DeepPartial<WGPUExtent3D>,
    dataView?: DataViewExt
): DataViewExt<WGPUExtent3D> {
    if (!dataView) dataView = DataViewExt.alloc(12);
    if (!x) return dataView as any;
    writeuint32_t(x.width! as any, dataView.subview(0));
    writeuint32_t(x.height! as any, dataView.subview(4));
    writeuint32_t(x.depthOrArrayLayers! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUInstanceDescriptor(
    x: DeepPartial<WGPUInstanceDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUInstanceDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    return dataView as any;
}
export function writeWGPULimits(
    x: DeepPartial<WGPULimits>,
    dataView?: DataViewExt
): DataViewExt<WGPULimits> {
    if (!dataView) dataView = DataViewExt.alloc(144);
    if (!x) return dataView as any;
    writeuint32_t(x.maxTextureDimension1D! as any, dataView.subview(0));
    writeuint32_t(x.maxTextureDimension2D! as any, dataView.subview(4));
    writeuint32_t(x.maxTextureDimension3D! as any, dataView.subview(8));
    writeuint32_t(x.maxTextureArrayLayers! as any, dataView.subview(12));
    writeuint32_t(x.maxBindGroups! as any, dataView.subview(16));
    writeuint32_t(
        x.maxBindGroupsPlusVertexBuffers! as any,
        dataView.subview(20)
    );
    writeuint32_t(x.maxBindingsPerBindGroup! as any, dataView.subview(24));
    writeuint32_t(
        x.maxDynamicUniformBuffersPerPipelineLayout! as any,
        dataView.subview(28)
    );
    writeuint32_t(
        x.maxDynamicStorageBuffersPerPipelineLayout! as any,
        dataView.subview(32)
    );
    writeuint32_t(
        x.maxSampledTexturesPerShaderStage! as any,
        dataView.subview(36)
    );
    writeuint32_t(x.maxSamplersPerShaderStage! as any, dataView.subview(40));
    writeuint32_t(
        x.maxStorageBuffersPerShaderStage! as any,
        dataView.subview(44)
    );
    writeuint32_t(
        x.maxStorageTexturesPerShaderStage! as any,
        dataView.subview(48)
    );
    writeuint32_t(
        x.maxUniformBuffersPerShaderStage! as any,
        dataView.subview(52)
    );
    writeuint64_t(x.maxUniformBufferBindingSize! as any, dataView.subview(56));
    writeuint64_t(x.maxStorageBufferBindingSize! as any, dataView.subview(64));
    writeuint32_t(
        x.minUniformBufferOffsetAlignment! as any,
        dataView.subview(72)
    );
    writeuint32_t(
        x.minStorageBufferOffsetAlignment! as any,
        dataView.subview(76)
    );
    writeuint32_t(x.maxVertexBuffers! as any, dataView.subview(80));
    writeuint64_t(x.maxBufferSize! as any, dataView.subview(88));
    writeuint32_t(x.maxVertexAttributes! as any, dataView.subview(96));
    writeuint32_t(x.maxVertexBufferArrayStride! as any, dataView.subview(100));
    writeuint32_t(
        x.maxInterStageShaderComponents! as any,
        dataView.subview(104)
    );
    writeuint32_t(
        x.maxInterStageShaderVariables! as any,
        dataView.subview(108)
    );
    writeuint32_t(x.maxColorAttachments! as any, dataView.subview(112));
    writeuint32_t(
        x.maxColorAttachmentBytesPerSample! as any,
        dataView.subview(116)
    );
    writeuint32_t(
        x.maxComputeWorkgroupStorageSize! as any,
        dataView.subview(120)
    );
    writeuint32_t(
        x.maxComputeInvocationsPerWorkgroup! as any,
        dataView.subview(124)
    );
    writeuint32_t(x.maxComputeWorkgroupSizeX! as any, dataView.subview(128));
    writeuint32_t(x.maxComputeWorkgroupSizeY! as any, dataView.subview(132));
    writeuint32_t(x.maxComputeWorkgroupSizeZ! as any, dataView.subview(136));
    writeuint32_t(
        x.maxComputeWorkgroupsPerDimension! as any,
        dataView.subview(140)
    );
    return dataView as any;
}
export function writeWGPUMultisampleState(
    x: DeepPartial<WGPUMultisampleState>,
    dataView?: DataViewExt
): DataViewExt<WGPUMultisampleState> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeuint32_t(x.count! as any, dataView.subview(8));
    writeuint32_t(x.mask! as any, dataView.subview(12));
    writeWGPUBool(x.alphaToCoverageEnabled! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUOrigin3D(
    x: DeepPartial<WGPUOrigin3D>,
    dataView?: DataViewExt
): DataViewExt<WGPUOrigin3D> {
    if (!dataView) dataView = DataViewExt.alloc(12);
    if (!x) return dataView as any;
    writeuint32_t(x.x! as any, dataView.subview(0));
    writeuint32_t(x.y! as any, dataView.subview(4));
    writeuint32_t(x.z! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUPipelineLayoutDescriptor(
    x: DeepPartial<WGPUPipelineLayoutDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUPipelineLayoutDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writesize_t(x.bindGroupLayoutCount! as any, dataView.subview(16));

    if (isPtrOrConstPtr(x.bindGroupLayouts)) {
        writeConstPtrT<WGPUBindGroupLayout>(
            x.bindGroupLayouts! as any,
            dataView.subview(24)
        );
    } else {
        writeWGPUBindGroupLayout(
            x.bindGroupLayouts! as any,
            dataView.subview(24)
        );
    }

    return dataView as any;
}
export function writeWGPUPrimitiveDepthClipControl(
    x: DeepPartial<WGPUPrimitiveDepthClipControl>,
    dataView?: DataViewExt
): DataViewExt<WGPUPrimitiveDepthClipControl> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));
    writeWGPUBool(x.unclippedDepth! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUPrimitiveState(
    x: DeepPartial<WGPUPrimitiveState>,
    dataView?: DataViewExt
): DataViewExt<WGPUPrimitiveState> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUPrimitiveTopology(x.topology! as any, dataView.subview(8));
    writeWGPUIndexFormat(x.stripIndexFormat! as any, dataView.subview(12));
    writeWGPUFrontFace(x.frontFace! as any, dataView.subview(16));
    writeWGPUCullMode(x.cullMode! as any, dataView.subview(20));
    return dataView as any;
}
export function writeWGPUQuerySetDescriptor(
    x: DeepPartial<WGPUQuerySetDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUQuerySetDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writeWGPUQueryType(x.type! as any, dataView.subview(16));
    writeuint32_t(x.count! as any, dataView.subview(20));
    return dataView as any;
}
export function writeWGPUQueueDescriptor(
    x: DeepPartial<WGPUQueueDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUQueueDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPURenderBundleDescriptor(
    x: DeepPartial<WGPURenderBundleDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPURenderBundleDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPURenderBundleEncoderDescriptor(
    x: DeepPartial<WGPURenderBundleEncoderDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPURenderBundleEncoderDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(48);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writesize_t(x.colorFormatCount! as any, dataView.subview(16));

    if (isPtrOrConstPtr(x.colorFormats)) {
        writeConstPtrT<WGPUTextureFormat>(
            x.colorFormats! as any,
            dataView.subview(24)
        );
    } else {
        writeWGPUTextureFormat(x.colorFormats! as any, dataView.subview(24));
    }

    writeWGPUTextureFormat(x.depthStencilFormat! as any, dataView.subview(32));
    writeuint32_t(x.sampleCount! as any, dataView.subview(36));
    writeWGPUBool(x.depthReadOnly! as any, dataView.subview(40));
    writeWGPUBool(x.stencilReadOnly! as any, dataView.subview(44));
    return dataView as any;
}
export function writeWGPURenderPassDepthStencilAttachment(
    x: DeepPartial<WGPURenderPassDepthStencilAttachment>,
    dataView?: DataViewExt
): DataViewExt<WGPURenderPassDepthStencilAttachment> {
    if (!dataView) dataView = DataViewExt.alloc(40);
    if (!x) return dataView as any;
    writeWGPUTextureView(x.view! as any, dataView.subview(0));
    writeWGPULoadOp(x.depthLoadOp! as any, dataView.subview(8));
    writeWGPUStoreOp(x.depthStoreOp! as any, dataView.subview(12));
    writefloat(x.depthClearValue! as any, dataView.subview(16));
    writeWGPUBool(x.depthReadOnly! as any, dataView.subview(20));
    writeWGPULoadOp(x.stencilLoadOp! as any, dataView.subview(24));
    writeWGPUStoreOp(x.stencilStoreOp! as any, dataView.subview(28));
    writeuint32_t(x.stencilClearValue! as any, dataView.subview(32));
    writeWGPUBool(x.stencilReadOnly! as any, dataView.subview(36));
    return dataView as any;
}
export function writeWGPURenderPassDescriptorMaxDrawCount(
    x: DeepPartial<WGPURenderPassDescriptorMaxDrawCount>,
    dataView?: DataViewExt
): DataViewExt<WGPURenderPassDescriptorMaxDrawCount> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));
    writeuint64_t(x.maxDrawCount! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPURenderPassTimestampWrites(
    x: DeepPartial<WGPURenderPassTimestampWrites>,
    dataView?: DataViewExt
): DataViewExt<WGPURenderPassTimestampWrites> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;
    writeWGPUQuerySet(x.querySet! as any, dataView.subview(0));
    writeuint32_t(x.beginningOfPassWriteIndex! as any, dataView.subview(8));
    writeuint32_t(x.endOfPassWriteIndex! as any, dataView.subview(12));
    return dataView as any;
}
export function writeWGPURequestAdapterOptions(
    x: DeepPartial<WGPURequestAdapterOptions>,
    dataView?: DataViewExt
): DataViewExt<WGPURequestAdapterOptions> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUSurface(x.compatibleSurface! as any, dataView.subview(8));
    writeWGPUPowerPreference(x.powerPreference! as any, dataView.subview(16));
    writeWGPUBackendType(x.backendType! as any, dataView.subview(20));
    writeWGPUBool(x.forceFallbackAdapter! as any, dataView.subview(24));
    return dataView as any;
}
export function writeWGPUSamplerBindingLayout(
    x: DeepPartial<WGPUSamplerBindingLayout>,
    dataView?: DataViewExt
): DataViewExt<WGPUSamplerBindingLayout> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUSamplerBindingType(x.type! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUSamplerDescriptor(
    x: DeepPartial<WGPUSamplerDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUSamplerDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(56);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writeWGPUAddressMode(x.addressModeU! as any, dataView.subview(16));
    writeWGPUAddressMode(x.addressModeV! as any, dataView.subview(20));
    writeWGPUAddressMode(x.addressModeW! as any, dataView.subview(24));
    writeWGPUFilterMode(x.magFilter! as any, dataView.subview(28));
    writeWGPUFilterMode(x.minFilter! as any, dataView.subview(32));
    writeWGPUMipmapFilterMode(x.mipmapFilter! as any, dataView.subview(36));
    writefloat(x.lodMinClamp! as any, dataView.subview(40));
    writefloat(x.lodMaxClamp! as any, dataView.subview(44));
    writeWGPUCompareFunction(x.compare! as any, dataView.subview(48));
    writeuint16_t(x.maxAnisotropy! as any, dataView.subview(52));
    return dataView as any;
}
export function writeWGPUShaderModuleCompilationHint(
    x: DeepPartial<WGPUShaderModuleCompilationHint>,
    dataView?: DataViewExt
): DataViewExt<WGPUShaderModuleCompilationHint> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.entryPoint! as any, dataView.subview(8));
    writeWGPUPipelineLayout(x.layout! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUShaderModuleSPIRVDescriptor(
    x: DeepPartial<WGPUShaderModuleSPIRVDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUShaderModuleSPIRVDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));
    writeuint32_t(x.codeSize! as any, dataView.subview(16));

    if (isPtrOrConstPtr(x.code)) {
        writeConstPtrT<uint32_t>(x.code! as any, dataView.subview(24));
    } else {
        writeuint32_t(x.code! as any, dataView.subview(24));
    }

    return dataView as any;
}
export function writeWGPUShaderModuleWGSLDescriptor(
    x: DeepPartial<WGPUShaderModuleWGSLDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUShaderModuleWGSLDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));
    writeCString(x.code! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUStencilFaceState(
    x: DeepPartial<WGPUStencilFaceState>,
    dataView?: DataViewExt
): DataViewExt<WGPUStencilFaceState> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;
    writeWGPUCompareFunction(x.compare! as any, dataView.subview(0));
    writeWGPUStencilOperation(x.failOp! as any, dataView.subview(4));
    writeWGPUStencilOperation(x.depthFailOp! as any, dataView.subview(8));
    writeWGPUStencilOperation(x.passOp! as any, dataView.subview(12));
    return dataView as any;
}
export function writeWGPUStorageTextureBindingLayout(
    x: DeepPartial<WGPUStorageTextureBindingLayout>,
    dataView?: DataViewExt
): DataViewExt<WGPUStorageTextureBindingLayout> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUStorageTextureAccess(x.access! as any, dataView.subview(8));
    writeWGPUTextureFormat(x.format! as any, dataView.subview(12));
    writeWGPUTextureViewDimension(
        x.viewDimension! as any,
        dataView.subview(16)
    );
    return dataView as any;
}
export function writeWGPUSurfaceCapabilities(
    x: DeepPartial<WGPUSurfaceCapabilities>,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceCapabilities> {
    if (!dataView) dataView = DataViewExt.alloc(56);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writePtrT<WGPUChainedStructOut>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStructOut(x.nextInChain! as any, dataView.subview(0));
    }

    writesize_t(x.formatCount! as any, dataView.subview(8));

    if (isPtrOrConstPtr(x.formats)) {
        writePtrT<WGPUTextureFormat>(x.formats! as any, dataView.subview(16));
    } else {
        writeWGPUTextureFormat(x.formats! as any, dataView.subview(16));
    }

    writesize_t(x.presentModeCount! as any, dataView.subview(24));

    if (isPtrOrConstPtr(x.presentModes)) {
        writePtrT<WGPUPresentMode>(
            x.presentModes! as any,
            dataView.subview(32)
        );
    } else {
        writeWGPUPresentMode(x.presentModes! as any, dataView.subview(32));
    }

    writesize_t(x.alphaModeCount! as any, dataView.subview(40));

    if (isPtrOrConstPtr(x.alphaModes)) {
        writePtrT<WGPUCompositeAlphaMode>(
            x.alphaModes! as any,
            dataView.subview(48)
        );
    } else {
        writeWGPUCompositeAlphaMode(x.alphaModes! as any, dataView.subview(48));
    }

    return dataView as any;
}
export function writeWGPUSurfaceConfiguration(
    x: DeepPartial<WGPUSurfaceConfiguration>,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceConfiguration> {
    if (!dataView) dataView = DataViewExt.alloc(56);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUDevice(x.device! as any, dataView.subview(8));
    writeWGPUTextureFormat(x.format! as any, dataView.subview(16));
    writeWGPUTextureUsageFlags(x.usage! as any, dataView.subview(20));
    writesize_t(x.viewFormatCount! as any, dataView.subview(24));

    if (isPtrOrConstPtr(x.viewFormats)) {
        writeConstPtrT<WGPUTextureFormat>(
            x.viewFormats! as any,
            dataView.subview(32)
        );
    } else {
        writeWGPUTextureFormat(x.viewFormats! as any, dataView.subview(32));
    }

    writeWGPUCompositeAlphaMode(x.alphaMode! as any, dataView.subview(40));
    writeuint32_t(x.width! as any, dataView.subview(44));
    writeuint32_t(x.height! as any, dataView.subview(48));
    writeWGPUPresentMode(x.presentMode! as any, dataView.subview(52));
    return dataView as any;
}
export function writeWGPUSurfaceDescriptor(
    x: DeepPartial<WGPUSurfaceDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUSurfaceDescriptorFromAndroidNativeWindow(
    x: DeepPartial<WGPUSurfaceDescriptorFromAndroidNativeWindow>,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceDescriptorFromAndroidNativeWindow> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));

    if (isPtrOrConstPtr(x.window)) {
        writePtrT<void>(x.window! as any, dataView.subview(16));
    } else {
        writevoid(x.window! as any, dataView.subview(16));
    }

    return dataView as any;
}
export function writeWGPUSurfaceDescriptorFromCanvasHTMLSelector(
    x: DeepPartial<WGPUSurfaceDescriptorFromCanvasHTMLSelector>,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceDescriptorFromCanvasHTMLSelector> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));
    writeCString(x.selector! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUSurfaceDescriptorFromMetalLayer(
    x: DeepPartial<WGPUSurfaceDescriptorFromMetalLayer>,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceDescriptorFromMetalLayer> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));

    if (isPtrOrConstPtr(x.layer)) {
        writePtrT<void>(x.layer! as any, dataView.subview(16));
    } else {
        writevoid(x.layer! as any, dataView.subview(16));
    }

    return dataView as any;
}
export function writeWGPUSurfaceDescriptorFromWaylandSurface(
    x: DeepPartial<WGPUSurfaceDescriptorFromWaylandSurface>,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceDescriptorFromWaylandSurface> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));

    if (isPtrOrConstPtr(x.display)) {
        writePtrT<void>(x.display! as any, dataView.subview(16));
    } else {
        writevoid(x.display! as any, dataView.subview(16));
    }

    if (isPtrOrConstPtr(x.surface)) {
        writePtrT<void>(x.surface! as any, dataView.subview(24));
    } else {
        writevoid(x.surface! as any, dataView.subview(24));
    }

    return dataView as any;
}
export function writeWGPUSurfaceDescriptorFromWindowsHWND(
    x: DeepPartial<WGPUSurfaceDescriptorFromWindowsHWND>,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceDescriptorFromWindowsHWND> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));

    if (isPtrOrConstPtr(x.hinstance)) {
        writePtrT<void>(x.hinstance! as any, dataView.subview(16));
    } else {
        writevoid(x.hinstance! as any, dataView.subview(16));
    }

    if (isPtrOrConstPtr(x.hwnd)) {
        writePtrT<void>(x.hwnd! as any, dataView.subview(24));
    } else {
        writevoid(x.hwnd! as any, dataView.subview(24));
    }

    return dataView as any;
}
export function writeWGPUSurfaceDescriptorFromXcbWindow(
    x: DeepPartial<WGPUSurfaceDescriptorFromXcbWindow>,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceDescriptorFromXcbWindow> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));

    if (isPtrOrConstPtr(x.connection)) {
        writePtrT<void>(x.connection! as any, dataView.subview(16));
    } else {
        writevoid(x.connection! as any, dataView.subview(16));
    }

    writeuint32_t(x.window! as any, dataView.subview(24));
    return dataView as any;
}
export function writeWGPUSurfaceDescriptorFromXlibWindow(
    x: DeepPartial<WGPUSurfaceDescriptorFromXlibWindow>,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceDescriptorFromXlibWindow> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));

    if (isPtrOrConstPtr(x.display)) {
        writePtrT<void>(x.display! as any, dataView.subview(16));
    } else {
        writevoid(x.display! as any, dataView.subview(16));
    }

    writeuint64_t(x.window! as any, dataView.subview(24));
    return dataView as any;
}
export function writeWGPUSurfaceTexture(
    x: DeepPartial<WGPUSurfaceTexture>,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceTexture> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;
    writeWGPUTexture(x.texture! as any, dataView.subview(0));
    writeWGPUBool(x.suboptimal! as any, dataView.subview(8));
    writeWGPUSurfaceGetCurrentTextureStatus(
        x.status! as any,
        dataView.subview(12)
    );
    return dataView as any;
}
export function writeWGPUTextureBindingLayout(
    x: DeepPartial<WGPUTextureBindingLayout>,
    dataView?: DataViewExt
): DataViewExt<WGPUTextureBindingLayout> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUTextureSampleType(x.sampleType! as any, dataView.subview(8));
    writeWGPUTextureViewDimension(
        x.viewDimension! as any,
        dataView.subview(12)
    );
    writeWGPUBool(x.multisampled! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUTextureDataLayout(
    x: DeepPartial<WGPUTextureDataLayout>,
    dataView?: DataViewExt
): DataViewExt<WGPUTextureDataLayout> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeuint64_t(x.offset! as any, dataView.subview(8));
    writeuint32_t(x.bytesPerRow! as any, dataView.subview(16));
    writeuint32_t(x.rowsPerImage! as any, dataView.subview(20));
    return dataView as any;
}
export function writeWGPUTextureViewDescriptor(
    x: DeepPartial<WGPUTextureViewDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUTextureViewDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(48);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writeWGPUTextureFormat(x.format! as any, dataView.subview(16));
    writeWGPUTextureViewDimension(x.dimension! as any, dataView.subview(20));
    writeuint32_t(x.baseMipLevel! as any, dataView.subview(24));
    writeuint32_t(x.mipLevelCount! as any, dataView.subview(28));
    writeuint32_t(x.baseArrayLayer! as any, dataView.subview(32));
    writeuint32_t(x.arrayLayerCount! as any, dataView.subview(36));
    writeWGPUTextureAspect(x.aspect! as any, dataView.subview(40));
    return dataView as any;
}
export function writeWGPUVertexAttribute(
    x: DeepPartial<WGPUVertexAttribute>,
    dataView?: DataViewExt
): DataViewExt<WGPUVertexAttribute> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUVertexFormat(x.format! as any, dataView.subview(0));
    writeuint64_t(x.offset! as any, dataView.subview(8));
    writeuint32_t(x.shaderLocation! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUBindGroupDescriptor(
    x: DeepPartial<WGPUBindGroupDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUBindGroupDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(40);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writeWGPUBindGroupLayout(x.layout! as any, dataView.subview(16));
    writesize_t(x.entryCount! as any, dataView.subview(24));

    if (isPtrOrConstPtr(x.entries)) {
        writeConstPtrT<WGPUBindGroupEntry>(
            x.entries! as any,
            dataView.subview(32)
        );
    } else {
        writeWGPUBindGroupEntry(x.entries! as any, dataView.subview(32));
    }

    return dataView as any;
}
export function writeWGPUBindGroupLayoutEntry(
    x: DeepPartial<WGPUBindGroupLayoutEntry>,
    dataView?: DataViewExt
): DataViewExt<WGPUBindGroupLayoutEntry> {
    if (!dataView) dataView = DataViewExt.alloc(104);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeuint32_t(x.binding! as any, dataView.subview(8));
    writeWGPUShaderStageFlags(x.visibility! as any, dataView.subview(12));
    writeWGPUBufferBindingLayout(x.buffer! as any, dataView.subview(16));
    writeWGPUSamplerBindingLayout(x.sampler! as any, dataView.subview(40));
    writeWGPUTextureBindingLayout(x.texture! as any, dataView.subview(56));
    writeWGPUStorageTextureBindingLayout(
        x.storageTexture! as any,
        dataView.subview(80)
    );
    return dataView as any;
}
export function writeWGPUBlendState(
    x: DeepPartial<WGPUBlendState>,
    dataView?: DataViewExt
): DataViewExt<WGPUBlendState> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUBlendComponent(x.color! as any, dataView.subview(0));
    writeWGPUBlendComponent(x.alpha! as any, dataView.subview(12));
    return dataView as any;
}
export function writeWGPUCompilationInfo(
    x: DeepPartial<WGPUCompilationInfo>,
    dataView?: DataViewExt
): DataViewExt<WGPUCompilationInfo> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writesize_t(x.messageCount! as any, dataView.subview(8));

    if (isPtrOrConstPtr(x.messages)) {
        writeConstPtrT<WGPUCompilationMessage>(
            x.messages! as any,
            dataView.subview(16)
        );
    } else {
        writeWGPUCompilationMessage(x.messages! as any, dataView.subview(16));
    }

    return dataView as any;
}
export function writeWGPUComputePassDescriptor(
    x: DeepPartial<WGPUComputePassDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUComputePassDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));

    if (isPtrOrConstPtr(x.timestampWrites)) {
        writeConstPtrT<WGPUComputePassTimestampWrites>(
            x.timestampWrites! as any,
            dataView.subview(16)
        );
    } else {
        writeWGPUComputePassTimestampWrites(
            x.timestampWrites! as any,
            dataView.subview(16)
        );
    }

    return dataView as any;
}
export function writeWGPUDepthStencilState(
    x: DeepPartial<WGPUDepthStencilState>,
    dataView?: DataViewExt
): DataViewExt<WGPUDepthStencilState> {
    if (!dataView) dataView = DataViewExt.alloc(72);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUTextureFormat(x.format! as any, dataView.subview(8));
    writeWGPUBool(x.depthWriteEnabled! as any, dataView.subview(12));
    writeWGPUCompareFunction(x.depthCompare! as any, dataView.subview(16));
    writeWGPUStencilFaceState(x.stencilFront! as any, dataView.subview(20));
    writeWGPUStencilFaceState(x.stencilBack! as any, dataView.subview(36));
    writeuint32_t(x.stencilReadMask! as any, dataView.subview(52));
    writeuint32_t(x.stencilWriteMask! as any, dataView.subview(56));
    writeint32_t(x.depthBias! as any, dataView.subview(60));
    writefloat(x.depthBiasSlopeScale! as any, dataView.subview(64));
    writefloat(x.depthBiasClamp! as any, dataView.subview(68));
    return dataView as any;
}
export function writeWGPUImageCopyBuffer(
    x: DeepPartial<WGPUImageCopyBuffer>,
    dataView?: DataViewExt
): DataViewExt<WGPUImageCopyBuffer> {
    if (!dataView) dataView = DataViewExt.alloc(40);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUTextureDataLayout(x.layout! as any, dataView.subview(8));
    writeWGPUBuffer(x.buffer! as any, dataView.subview(32));
    return dataView as any;
}
export function writeWGPUImageCopyTexture(
    x: DeepPartial<WGPUImageCopyTexture>,
    dataView?: DataViewExt
): DataViewExt<WGPUImageCopyTexture> {
    if (!dataView) dataView = DataViewExt.alloc(40);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUTexture(x.texture! as any, dataView.subview(8));
    writeuint32_t(x.mipLevel! as any, dataView.subview(16));
    writeWGPUOrigin3D(x.origin! as any, dataView.subview(20));
    writeWGPUTextureAspect(x.aspect! as any, dataView.subview(32));
    return dataView as any;
}
export function writeWGPUProgrammableStageDescriptor(
    x: DeepPartial<WGPUProgrammableStageDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUProgrammableStageDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(40);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUShaderModule(x.module! as any, dataView.subview(8));
    writeCString(x.entryPoint! as any, dataView.subview(16));
    writesize_t(x.constantCount! as any, dataView.subview(24));

    if (isPtrOrConstPtr(x.constants)) {
        writeConstPtrT<WGPUConstantEntry>(
            x.constants! as any,
            dataView.subview(32)
        );
    } else {
        writeWGPUConstantEntry(x.constants! as any, dataView.subview(32));
    }

    return dataView as any;
}
export function writeWGPURenderPassColorAttachment(
    x: DeepPartial<WGPURenderPassColorAttachment>,
    dataView?: DataViewExt
): DataViewExt<WGPURenderPassColorAttachment> {
    if (!dataView) dataView = DataViewExt.alloc(64);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUTextureView(x.view! as any, dataView.subview(8));
    writeWGPUTextureView(x.resolveTarget! as any, dataView.subview(16));
    writeWGPULoadOp(x.loadOp! as any, dataView.subview(24));
    writeWGPUStoreOp(x.storeOp! as any, dataView.subview(28));
    writeWGPUColor(x.clearValue! as any, dataView.subview(32));
    return dataView as any;
}
export function writeWGPURequiredLimits(
    x: DeepPartial<WGPURequiredLimits>,
    dataView?: DataViewExt
): DataViewExt<WGPURequiredLimits> {
    if (!dataView) dataView = DataViewExt.alloc(152);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPULimits(x.limits! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUShaderModuleDescriptor(
    x: DeepPartial<WGPUShaderModuleDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUShaderModuleDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writesize_t(x.hintCount! as any, dataView.subview(16));

    if (isPtrOrConstPtr(x.hints)) {
        writeConstPtrT<WGPUShaderModuleCompilationHint>(
            x.hints! as any,
            dataView.subview(24)
        );
    } else {
        writeWGPUShaderModuleCompilationHint(
            x.hints! as any,
            dataView.subview(24)
        );
    }

    return dataView as any;
}
export function writeWGPUSupportedLimits(
    x: DeepPartial<WGPUSupportedLimits>,
    dataView?: DataViewExt
): DataViewExt<WGPUSupportedLimits> {
    if (!dataView) dataView = DataViewExt.alloc(152);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writePtrT<WGPUChainedStructOut>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStructOut(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPULimits(x.limits! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUTextureDescriptor(
    x: DeepPartial<WGPUTextureDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUTextureDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(64);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writeWGPUTextureUsageFlags(x.usage! as any, dataView.subview(16));
    writeWGPUTextureDimension(x.dimension! as any, dataView.subview(20));
    writeWGPUExtent3D(x.size! as any, dataView.subview(24));
    writeWGPUTextureFormat(x.format! as any, dataView.subview(36));
    writeuint32_t(x.mipLevelCount! as any, dataView.subview(40));
    writeuint32_t(x.sampleCount! as any, dataView.subview(44));
    writesize_t(x.viewFormatCount! as any, dataView.subview(48));

    if (isPtrOrConstPtr(x.viewFormats)) {
        writeConstPtrT<WGPUTextureFormat>(
            x.viewFormats! as any,
            dataView.subview(56)
        );
    } else {
        writeWGPUTextureFormat(x.viewFormats! as any, dataView.subview(56));
    }

    return dataView as any;
}
export function writeWGPUVertexBufferLayout(
    x: DeepPartial<WGPUVertexBufferLayout>,
    dataView?: DataViewExt
): DataViewExt<WGPUVertexBufferLayout> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;
    writeuint64_t(x.arrayStride! as any, dataView.subview(0));
    writeWGPUVertexStepMode(x.stepMode! as any, dataView.subview(8));
    writesize_t(x.attributeCount! as any, dataView.subview(16));

    if (isPtrOrConstPtr(x.attributes)) {
        writeConstPtrT<WGPUVertexAttribute>(
            x.attributes! as any,
            dataView.subview(24)
        );
    } else {
        writeWGPUVertexAttribute(x.attributes! as any, dataView.subview(24));
    }

    return dataView as any;
}
export function writeWGPUBindGroupLayoutDescriptor(
    x: DeepPartial<WGPUBindGroupLayoutDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUBindGroupLayoutDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writesize_t(x.entryCount! as any, dataView.subview(16));

    if (isPtrOrConstPtr(x.entries)) {
        writeConstPtrT<WGPUBindGroupLayoutEntry>(
            x.entries! as any,
            dataView.subview(24)
        );
    } else {
        writeWGPUBindGroupLayoutEntry(x.entries! as any, dataView.subview(24));
    }

    return dataView as any;
}
export function writeWGPUColorTargetState(
    x: DeepPartial<WGPUColorTargetState>,
    dataView?: DataViewExt
): DataViewExt<WGPUColorTargetState> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUTextureFormat(x.format! as any, dataView.subview(8));

    if (isPtrOrConstPtr(x.blend)) {
        writeConstPtrT<WGPUBlendState>(x.blend! as any, dataView.subview(16));
    } else {
        writeWGPUBlendState(x.blend! as any, dataView.subview(16));
    }

    writeWGPUColorWriteMaskFlags(x.writeMask! as any, dataView.subview(24));
    return dataView as any;
}
export function writeWGPUComputePipelineDescriptor(
    x: DeepPartial<WGPUComputePipelineDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUComputePipelineDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(64);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writeWGPUPipelineLayout(x.layout! as any, dataView.subview(16));
    writeWGPUProgrammableStageDescriptor(
        x.compute! as any,
        dataView.subview(24)
    );
    return dataView as any;
}
export function writeWGPUDeviceDescriptor(
    x: DeepPartial<WGPUDeviceDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUDeviceDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(72);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writesize_t(x.requiredFeatureCount! as any, dataView.subview(16));

    if (isPtrOrConstPtr(x.requiredFeatures)) {
        writeConstPtrT<WGPUFeatureName>(
            x.requiredFeatures! as any,
            dataView.subview(24)
        );
    } else {
        writeWGPUFeatureName(x.requiredFeatures! as any, dataView.subview(24));
    }

    if (isPtrOrConstPtr(x.requiredLimits)) {
        writeConstPtrT<WGPURequiredLimits>(
            x.requiredLimits! as any,
            dataView.subview(32)
        );
    } else {
        writeWGPURequiredLimits(x.requiredLimits! as any, dataView.subview(32));
    }

    writeWGPUQueueDescriptor(x.defaultQueue! as any, dataView.subview(40));
    writeWGPUDeviceLostCallback(
        x.deviceLostCallback! as any,
        dataView.subview(56)
    );

    if (isPtrOrConstPtr(x.deviceLostUserdata)) {
        writePtrT<void>(x.deviceLostUserdata! as any, dataView.subview(64));
    } else {
        writevoid(x.deviceLostUserdata! as any, dataView.subview(64));
    }

    return dataView as any;
}
export function writeWGPURenderPassDescriptor(
    x: DeepPartial<WGPURenderPassDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPURenderPassDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(56);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writesize_t(x.colorAttachmentCount! as any, dataView.subview(16));

    if (isPtrOrConstPtr(x.colorAttachments)) {
        writeConstPtrT<WGPURenderPassColorAttachment>(
            x.colorAttachments! as any,
            dataView.subview(24)
        );
    } else {
        writeWGPURenderPassColorAttachment(
            x.colorAttachments! as any,
            dataView.subview(24)
        );
    }

    if (isPtrOrConstPtr(x.depthStencilAttachment)) {
        writeConstPtrT<WGPURenderPassDepthStencilAttachment>(
            x.depthStencilAttachment! as any,
            dataView.subview(32)
        );
    } else {
        writeWGPURenderPassDepthStencilAttachment(
            x.depthStencilAttachment! as any,
            dataView.subview(32)
        );
    }

    writeWGPUQuerySet(x.occlusionQuerySet! as any, dataView.subview(40));

    if (isPtrOrConstPtr(x.timestampWrites)) {
        writeConstPtrT<WGPURenderPassTimestampWrites>(
            x.timestampWrites! as any,
            dataView.subview(48)
        );
    } else {
        writeWGPURenderPassTimestampWrites(
            x.timestampWrites! as any,
            dataView.subview(48)
        );
    }

    return dataView as any;
}
export function writeWGPUVertexState(
    x: DeepPartial<WGPUVertexState>,
    dataView?: DataViewExt
): DataViewExt<WGPUVertexState> {
    if (!dataView) dataView = DataViewExt.alloc(56);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUShaderModule(x.module! as any, dataView.subview(8));
    writeCString(x.entryPoint! as any, dataView.subview(16));
    writesize_t(x.constantCount! as any, dataView.subview(24));

    if (isPtrOrConstPtr(x.constants)) {
        writeConstPtrT<WGPUConstantEntry>(
            x.constants! as any,
            dataView.subview(32)
        );
    } else {
        writeWGPUConstantEntry(x.constants! as any, dataView.subview(32));
    }

    writesize_t(x.bufferCount! as any, dataView.subview(40));

    if (isPtrOrConstPtr(x.buffers)) {
        writeConstPtrT<WGPUVertexBufferLayout>(
            x.buffers! as any,
            dataView.subview(48)
        );
    } else {
        writeWGPUVertexBufferLayout(x.buffers! as any, dataView.subview(48));
    }

    return dataView as any;
}
export function writeWGPUFragmentState(
    x: DeepPartial<WGPUFragmentState>,
    dataView?: DataViewExt
): DataViewExt<WGPUFragmentState> {
    if (!dataView) dataView = DataViewExt.alloc(56);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUShaderModule(x.module! as any, dataView.subview(8));
    writeCString(x.entryPoint! as any, dataView.subview(16));
    writesize_t(x.constantCount! as any, dataView.subview(24));

    if (isPtrOrConstPtr(x.constants)) {
        writeConstPtrT<WGPUConstantEntry>(
            x.constants! as any,
            dataView.subview(32)
        );
    } else {
        writeWGPUConstantEntry(x.constants! as any, dataView.subview(32));
    }

    writesize_t(x.targetCount! as any, dataView.subview(40));

    if (isPtrOrConstPtr(x.targets)) {
        writeConstPtrT<WGPUColorTargetState>(
            x.targets! as any,
            dataView.subview(48)
        );
    } else {
        writeWGPUColorTargetState(x.targets! as any, dataView.subview(48));
    }

    return dataView as any;
}
export function writeWGPURenderPipelineDescriptor(
    x: DeepPartial<WGPURenderPipelineDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPURenderPipelineDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(144);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeCString(x.label! as any, dataView.subview(8));
    writeWGPUPipelineLayout(x.layout! as any, dataView.subview(16));
    writeWGPUVertexState(x.vertex! as any, dataView.subview(24));
    writeWGPUPrimitiveState(x.primitive! as any, dataView.subview(80));

    if (isPtrOrConstPtr(x.depthStencil)) {
        writeConstPtrT<WGPUDepthStencilState>(
            x.depthStencil! as any,
            dataView.subview(104)
        );
    } else {
        writeWGPUDepthStencilState(
            x.depthStencil! as any,
            dataView.subview(104)
        );
    }

    writeWGPUMultisampleState(x.multisample! as any, dataView.subview(112));

    if (isPtrOrConstPtr(x.fragment)) {
        writeConstPtrT<WGPUFragmentState>(
            x.fragment! as any,
            dataView.subview(136)
        );
    } else {
        writeWGPUFragmentState(x.fragment! as any, dataView.subview(136));
    }

    return dataView as any;
}

export function writeWGPUProcCreateInstance(
    x: WGPUProcCreateInstance,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCreateInstance> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCreateInstance_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcGetProcAddress(
    x: WGPUProcGetProcAddress,
    dataView?: DataViewExt
): DataViewExt<WGPUProcGetProcAddress> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcGetProcAddress_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcAdapterEnumerateFeatures(
    x: WGPUProcAdapterEnumerateFeatures,
    dataView?: DataViewExt
): DataViewExt<WGPUProcAdapterEnumerateFeatures> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcAdapterEnumerateFeatures_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcAdapterGetLimits(
    x: WGPUProcAdapterGetLimits,
    dataView?: DataViewExt
): DataViewExt<WGPUProcAdapterGetLimits> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcAdapterGetLimits_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcAdapterGetProperties(
    x: WGPUProcAdapterGetProperties,
    dataView?: DataViewExt
): DataViewExt<WGPUProcAdapterGetProperties> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcAdapterGetProperties_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcAdapterHasFeature(
    x: WGPUProcAdapterHasFeature,
    dataView?: DataViewExt
): DataViewExt<WGPUProcAdapterHasFeature> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcAdapterHasFeature_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcAdapterRequestDevice(
    x: WGPUProcAdapterRequestDevice,
    dataView?: DataViewExt
): DataViewExt<WGPUProcAdapterRequestDevice> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcAdapterRequestDevice_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcAdapterReference(
    x: WGPUProcAdapterReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcAdapterReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcAdapterReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcAdapterRelease(
    x: WGPUProcAdapterRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcAdapterRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcAdapterRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBindGroupSetLabel(
    x: WGPUProcBindGroupSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBindGroupSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBindGroupSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBindGroupReference(
    x: WGPUProcBindGroupReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBindGroupReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBindGroupReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBindGroupRelease(
    x: WGPUProcBindGroupRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBindGroupRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBindGroupRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBindGroupLayoutSetLabel(
    x: WGPUProcBindGroupLayoutSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBindGroupLayoutSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBindGroupLayoutSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBindGroupLayoutReference(
    x: WGPUProcBindGroupLayoutReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBindGroupLayoutReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBindGroupLayoutReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBindGroupLayoutRelease(
    x: WGPUProcBindGroupLayoutRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBindGroupLayoutRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBindGroupLayoutRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBufferDestroy(
    x: WGPUProcBufferDestroy,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBufferDestroy> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPUProcBufferDestroy_funcdef"))
        .ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBufferGetConstMappedRange(
    x: WGPUProcBufferGetConstMappedRange,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBufferGetConstMappedRange> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBufferGetConstMappedRange_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBufferGetMapState(
    x: WGPUProcBufferGetMapState,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBufferGetMapState> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBufferGetMapState_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBufferGetMappedRange(
    x: WGPUProcBufferGetMappedRange,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBufferGetMappedRange> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBufferGetMappedRange_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBufferGetSize(
    x: WGPUProcBufferGetSize,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBufferGetSize> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPUProcBufferGetSize_funcdef"))
        .ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBufferGetUsage(
    x: WGPUProcBufferGetUsage,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBufferGetUsage> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBufferGetUsage_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBufferMapAsync(
    x: WGPUProcBufferMapAsync,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBufferMapAsync> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBufferMapAsync_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBufferSetLabel(
    x: WGPUProcBufferSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBufferSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBufferSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBufferUnmap(
    x: WGPUProcBufferUnmap,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBufferUnmap> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPUProcBufferUnmap_funcdef"))
        .ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBufferReference(
    x: WGPUProcBufferReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBufferReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcBufferReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcBufferRelease(
    x: WGPUProcBufferRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcBufferRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPUProcBufferRelease_funcdef"))
        .ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandBufferSetLabel(
    x: WGPUProcCommandBufferSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandBufferSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandBufferSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandBufferReference(
    x: WGPUProcCommandBufferReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandBufferReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandBufferReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandBufferRelease(
    x: WGPUProcCommandBufferRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandBufferRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandBufferRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderBeginComputePass(
    x: WGPUProcCommandEncoderBeginComputePass,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderBeginComputePass> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderBeginComputePass_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderBeginRenderPass(
    x: WGPUProcCommandEncoderBeginRenderPass,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderBeginRenderPass> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderBeginRenderPass_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderClearBuffer(
    x: WGPUProcCommandEncoderClearBuffer,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderClearBuffer> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderClearBuffer_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderCopyBufferToBuffer(
    x: WGPUProcCommandEncoderCopyBufferToBuffer,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderCopyBufferToBuffer> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderCopyBufferToBuffer_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderCopyBufferToTexture(
    x: WGPUProcCommandEncoderCopyBufferToTexture,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderCopyBufferToTexture> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderCopyBufferToTexture_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderCopyTextureToBuffer(
    x: WGPUProcCommandEncoderCopyTextureToBuffer,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderCopyTextureToBuffer> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderCopyTextureToBuffer_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderCopyTextureToTexture(
    x: WGPUProcCommandEncoderCopyTextureToTexture,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderCopyTextureToTexture> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderCopyTextureToTexture_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderFinish(
    x: WGPUProcCommandEncoderFinish,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderFinish> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderFinish_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderInsertDebugMarker(
    x: WGPUProcCommandEncoderInsertDebugMarker,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderInsertDebugMarker> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderInsertDebugMarker_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderPopDebugGroup(
    x: WGPUProcCommandEncoderPopDebugGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderPopDebugGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderPopDebugGroup_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderPushDebugGroup(
    x: WGPUProcCommandEncoderPushDebugGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderPushDebugGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderPushDebugGroup_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderResolveQuerySet(
    x: WGPUProcCommandEncoderResolveQuerySet,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderResolveQuerySet> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderResolveQuerySet_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderSetLabel(
    x: WGPUProcCommandEncoderSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderWriteTimestamp(
    x: WGPUProcCommandEncoderWriteTimestamp,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderWriteTimestamp> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderWriteTimestamp_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderReference(
    x: WGPUProcCommandEncoderReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcCommandEncoderRelease(
    x: WGPUProcCommandEncoderRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcCommandEncoderRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcCommandEncoderRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePassEncoderDispatchWorkgroups(
    x: WGPUProcComputePassEncoderDispatchWorkgroups,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePassEncoderDispatchWorkgroups> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePassEncoderDispatchWorkgroups_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePassEncoderDispatchWorkgroupsIndirect(
    x: WGPUProcComputePassEncoderDispatchWorkgroupsIndirect,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePassEncoderDispatchWorkgroupsIndirect> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePassEncoderDispatchWorkgroupsIndirect_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePassEncoderEnd(
    x: WGPUProcComputePassEncoderEnd,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePassEncoderEnd> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePassEncoderEnd_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePassEncoderInsertDebugMarker(
    x: WGPUProcComputePassEncoderInsertDebugMarker,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePassEncoderInsertDebugMarker> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePassEncoderInsertDebugMarker_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePassEncoderPopDebugGroup(
    x: WGPUProcComputePassEncoderPopDebugGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePassEncoderPopDebugGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePassEncoderPopDebugGroup_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePassEncoderPushDebugGroup(
    x: WGPUProcComputePassEncoderPushDebugGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePassEncoderPushDebugGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePassEncoderPushDebugGroup_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePassEncoderSetBindGroup(
    x: WGPUProcComputePassEncoderSetBindGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePassEncoderSetBindGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePassEncoderSetBindGroup_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePassEncoderSetLabel(
    x: WGPUProcComputePassEncoderSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePassEncoderSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePassEncoderSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePassEncoderSetPipeline(
    x: WGPUProcComputePassEncoderSetPipeline,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePassEncoderSetPipeline> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePassEncoderSetPipeline_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePassEncoderReference(
    x: WGPUProcComputePassEncoderReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePassEncoderReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePassEncoderReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePassEncoderRelease(
    x: WGPUProcComputePassEncoderRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePassEncoderRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePassEncoderRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePipelineGetBindGroupLayout(
    x: WGPUProcComputePipelineGetBindGroupLayout,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePipelineGetBindGroupLayout> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePipelineGetBindGroupLayout_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePipelineSetLabel(
    x: WGPUProcComputePipelineSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePipelineSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePipelineSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePipelineReference(
    x: WGPUProcComputePipelineReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePipelineReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePipelineReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcComputePipelineRelease(
    x: WGPUProcComputePipelineRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcComputePipelineRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcComputePipelineRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateBindGroup(
    x: WGPUProcDeviceCreateBindGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateBindGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateBindGroup_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateBindGroupLayout(
    x: WGPUProcDeviceCreateBindGroupLayout,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateBindGroupLayout> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateBindGroupLayout_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateBuffer(
    x: WGPUProcDeviceCreateBuffer,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateBuffer> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateBuffer_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateCommandEncoder(
    x: WGPUProcDeviceCreateCommandEncoder,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateCommandEncoder> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateCommandEncoder_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateComputePipeline(
    x: WGPUProcDeviceCreateComputePipeline,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateComputePipeline> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateComputePipeline_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateComputePipelineAsync(
    x: WGPUProcDeviceCreateComputePipelineAsync,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateComputePipelineAsync> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateComputePipelineAsync_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreatePipelineLayout(
    x: WGPUProcDeviceCreatePipelineLayout,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreatePipelineLayout> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreatePipelineLayout_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateQuerySet(
    x: WGPUProcDeviceCreateQuerySet,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateQuerySet> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateQuerySet_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateRenderBundleEncoder(
    x: WGPUProcDeviceCreateRenderBundleEncoder,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateRenderBundleEncoder> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateRenderBundleEncoder_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateRenderPipeline(
    x: WGPUProcDeviceCreateRenderPipeline,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateRenderPipeline> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateRenderPipeline_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateRenderPipelineAsync(
    x: WGPUProcDeviceCreateRenderPipelineAsync,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateRenderPipelineAsync> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateRenderPipelineAsync_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateSampler(
    x: WGPUProcDeviceCreateSampler,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateSampler> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateSampler_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateShaderModule(
    x: WGPUProcDeviceCreateShaderModule,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateShaderModule> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateShaderModule_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceCreateTexture(
    x: WGPUProcDeviceCreateTexture,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceCreateTexture> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceCreateTexture_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceDestroy(
    x: WGPUProcDeviceDestroy,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceDestroy> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPUProcDeviceDestroy_funcdef"))
        .ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceEnumerateFeatures(
    x: WGPUProcDeviceEnumerateFeatures,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceEnumerateFeatures> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceEnumerateFeatures_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceGetLimits(
    x: WGPUProcDeviceGetLimits,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceGetLimits> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceGetLimits_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceGetQueue(
    x: WGPUProcDeviceGetQueue,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceGetQueue> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceGetQueue_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceHasFeature(
    x: WGPUProcDeviceHasFeature,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceHasFeature> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceHasFeature_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDevicePopErrorScope(
    x: WGPUProcDevicePopErrorScope,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDevicePopErrorScope> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDevicePopErrorScope_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDevicePushErrorScope(
    x: WGPUProcDevicePushErrorScope,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDevicePushErrorScope> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDevicePushErrorScope_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceSetLabel(
    x: WGPUProcDeviceSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceSetUncapturedErrorCallback(
    x: WGPUProcDeviceSetUncapturedErrorCallback,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceSetUncapturedErrorCallback> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceSetUncapturedErrorCallback_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceReference(
    x: WGPUProcDeviceReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcDeviceReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcDeviceRelease(
    x: WGPUProcDeviceRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcDeviceRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPUProcDeviceRelease_funcdef"))
        .ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcInstanceCreateSurface(
    x: WGPUProcInstanceCreateSurface,
    dataView?: DataViewExt
): DataViewExt<WGPUProcInstanceCreateSurface> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcInstanceCreateSurface_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcInstanceProcessEvents(
    x: WGPUProcInstanceProcessEvents,
    dataView?: DataViewExt
): DataViewExt<WGPUProcInstanceProcessEvents> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcInstanceProcessEvents_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcInstanceRequestAdapter(
    x: WGPUProcInstanceRequestAdapter,
    dataView?: DataViewExt
): DataViewExt<WGPUProcInstanceRequestAdapter> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcInstanceRequestAdapter_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcInstanceReference(
    x: WGPUProcInstanceReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcInstanceReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcInstanceReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcInstanceRelease(
    x: WGPUProcInstanceRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcInstanceRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcInstanceRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcPipelineLayoutSetLabel(
    x: WGPUProcPipelineLayoutSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcPipelineLayoutSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcPipelineLayoutSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcPipelineLayoutReference(
    x: WGPUProcPipelineLayoutReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcPipelineLayoutReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcPipelineLayoutReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcPipelineLayoutRelease(
    x: WGPUProcPipelineLayoutRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcPipelineLayoutRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcPipelineLayoutRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQuerySetDestroy(
    x: WGPUProcQuerySetDestroy,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQuerySetDestroy> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcQuerySetDestroy_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQuerySetGetCount(
    x: WGPUProcQuerySetGetCount,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQuerySetGetCount> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcQuerySetGetCount_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQuerySetGetType(
    x: WGPUProcQuerySetGetType,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQuerySetGetType> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcQuerySetGetType_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQuerySetSetLabel(
    x: WGPUProcQuerySetSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQuerySetSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcQuerySetSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQuerySetReference(
    x: WGPUProcQuerySetReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQuerySetReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcQuerySetReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQuerySetRelease(
    x: WGPUProcQuerySetRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQuerySetRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcQuerySetRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQueueOnSubmittedWorkDone(
    x: WGPUProcQueueOnSubmittedWorkDone,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQueueOnSubmittedWorkDone> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcQueueOnSubmittedWorkDone_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQueueSetLabel(
    x: WGPUProcQueueSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQueueSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPUProcQueueSetLabel_funcdef"))
        .ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQueueSubmit(
    x: WGPUProcQueueSubmit,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQueueSubmit> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPUProcQueueSubmit_funcdef"))
        .ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQueueWriteBuffer(
    x: WGPUProcQueueWriteBuffer,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQueueWriteBuffer> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcQueueWriteBuffer_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQueueWriteTexture(
    x: WGPUProcQueueWriteTexture,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQueueWriteTexture> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcQueueWriteTexture_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQueueReference(
    x: WGPUProcQueueReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQueueReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcQueueReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcQueueRelease(
    x: WGPUProcQueueRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcQueueRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPUProcQueueRelease_funcdef"))
        .ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleSetLabel(
    x: WGPUProcRenderBundleSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleReference(
    x: WGPUProcRenderBundleReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleRelease(
    x: WGPUProcRenderBundleRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderDraw(
    x: WGPUProcRenderBundleEncoderDraw,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderDraw> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderDraw_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderDrawIndexed(
    x: WGPUProcRenderBundleEncoderDrawIndexed,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderDrawIndexed> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderDrawIndexed_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderDrawIndexedIndirect(
    x: WGPUProcRenderBundleEncoderDrawIndexedIndirect,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderDrawIndexedIndirect> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderDrawIndexedIndirect_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderDrawIndirect(
    x: WGPUProcRenderBundleEncoderDrawIndirect,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderDrawIndirect> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderDrawIndirect_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderFinish(
    x: WGPUProcRenderBundleEncoderFinish,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderFinish> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderFinish_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderInsertDebugMarker(
    x: WGPUProcRenderBundleEncoderInsertDebugMarker,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderInsertDebugMarker> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderInsertDebugMarker_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderPopDebugGroup(
    x: WGPUProcRenderBundleEncoderPopDebugGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderPopDebugGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderPopDebugGroup_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderPushDebugGroup(
    x: WGPUProcRenderBundleEncoderPushDebugGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderPushDebugGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderPushDebugGroup_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderSetBindGroup(
    x: WGPUProcRenderBundleEncoderSetBindGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderSetBindGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderSetBindGroup_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderSetIndexBuffer(
    x: WGPUProcRenderBundleEncoderSetIndexBuffer,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderSetIndexBuffer> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderSetIndexBuffer_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderSetLabel(
    x: WGPUProcRenderBundleEncoderSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderSetPipeline(
    x: WGPUProcRenderBundleEncoderSetPipeline,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderSetPipeline> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderSetPipeline_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderSetVertexBuffer(
    x: WGPUProcRenderBundleEncoderSetVertexBuffer,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderSetVertexBuffer> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderSetVertexBuffer_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderReference(
    x: WGPUProcRenderBundleEncoderReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderBundleEncoderRelease(
    x: WGPUProcRenderBundleEncoderRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderBundleEncoderRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderBundleEncoderRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderBeginOcclusionQuery(
    x: WGPUProcRenderPassEncoderBeginOcclusionQuery,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderBeginOcclusionQuery> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderBeginOcclusionQuery_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderDraw(
    x: WGPUProcRenderPassEncoderDraw,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderDraw> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderDraw_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderDrawIndexed(
    x: WGPUProcRenderPassEncoderDrawIndexed,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderDrawIndexed> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderDrawIndexed_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderDrawIndexedIndirect(
    x: WGPUProcRenderPassEncoderDrawIndexedIndirect,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderDrawIndexedIndirect> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderDrawIndexedIndirect_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderDrawIndirect(
    x: WGPUProcRenderPassEncoderDrawIndirect,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderDrawIndirect> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderDrawIndirect_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderEnd(
    x: WGPUProcRenderPassEncoderEnd,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderEnd> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderEnd_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderEndOcclusionQuery(
    x: WGPUProcRenderPassEncoderEndOcclusionQuery,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderEndOcclusionQuery> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderEndOcclusionQuery_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderExecuteBundles(
    x: WGPUProcRenderPassEncoderExecuteBundles,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderExecuteBundles> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderExecuteBundles_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderInsertDebugMarker(
    x: WGPUProcRenderPassEncoderInsertDebugMarker,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderInsertDebugMarker> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderInsertDebugMarker_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderPopDebugGroup(
    x: WGPUProcRenderPassEncoderPopDebugGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderPopDebugGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderPopDebugGroup_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderPushDebugGroup(
    x: WGPUProcRenderPassEncoderPushDebugGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderPushDebugGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderPushDebugGroup_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderSetBindGroup(
    x: WGPUProcRenderPassEncoderSetBindGroup,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderSetBindGroup> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderSetBindGroup_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderSetBlendConstant(
    x: WGPUProcRenderPassEncoderSetBlendConstant,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderSetBlendConstant> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderSetBlendConstant_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderSetIndexBuffer(
    x: WGPUProcRenderPassEncoderSetIndexBuffer,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderSetIndexBuffer> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderSetIndexBuffer_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderSetLabel(
    x: WGPUProcRenderPassEncoderSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderSetPipeline(
    x: WGPUProcRenderPassEncoderSetPipeline,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderSetPipeline> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderSetPipeline_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderSetScissorRect(
    x: WGPUProcRenderPassEncoderSetScissorRect,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderSetScissorRect> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderSetScissorRect_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderSetStencilReference(
    x: WGPUProcRenderPassEncoderSetStencilReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderSetStencilReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderSetStencilReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderSetVertexBuffer(
    x: WGPUProcRenderPassEncoderSetVertexBuffer,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderSetVertexBuffer> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderSetVertexBuffer_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderSetViewport(
    x: WGPUProcRenderPassEncoderSetViewport,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderSetViewport> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderSetViewport_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderReference(
    x: WGPUProcRenderPassEncoderReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPassEncoderRelease(
    x: WGPUProcRenderPassEncoderRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPassEncoderRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPassEncoderRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPipelineGetBindGroupLayout(
    x: WGPUProcRenderPipelineGetBindGroupLayout,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPipelineGetBindGroupLayout> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPipelineGetBindGroupLayout_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPipelineSetLabel(
    x: WGPUProcRenderPipelineSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPipelineSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPipelineSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPipelineReference(
    x: WGPUProcRenderPipelineReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPipelineReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPipelineReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcRenderPipelineRelease(
    x: WGPUProcRenderPipelineRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcRenderPipelineRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcRenderPipelineRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcSamplerSetLabel(
    x: WGPUProcSamplerSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcSamplerSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcSamplerSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcSamplerReference(
    x: WGPUProcSamplerReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcSamplerReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcSamplerReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcSamplerRelease(
    x: WGPUProcSamplerRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcSamplerRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcSamplerRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcShaderModuleGetCompilationInfo(
    x: WGPUProcShaderModuleGetCompilationInfo,
    dataView?: DataViewExt
): DataViewExt<WGPUProcShaderModuleGetCompilationInfo> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcShaderModuleGetCompilationInfo_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcShaderModuleSetLabel(
    x: WGPUProcShaderModuleSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcShaderModuleSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcShaderModuleSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcShaderModuleReference(
    x: WGPUProcShaderModuleReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcShaderModuleReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcShaderModuleReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcShaderModuleRelease(
    x: WGPUProcShaderModuleRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcShaderModuleRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcShaderModuleRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcSurfaceConfigure(
    x: WGPUProcSurfaceConfigure,
    dataView?: DataViewExt
): DataViewExt<WGPUProcSurfaceConfigure> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcSurfaceConfigure_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcSurfaceGetCapabilities(
    x: WGPUProcSurfaceGetCapabilities,
    dataView?: DataViewExt
): DataViewExt<WGPUProcSurfaceGetCapabilities> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcSurfaceGetCapabilities_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcSurfaceGetCurrentTexture(
    x: WGPUProcSurfaceGetCurrentTexture,
    dataView?: DataViewExt
): DataViewExt<WGPUProcSurfaceGetCurrentTexture> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcSurfaceGetCurrentTexture_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcSurfaceGetPreferredFormat(
    x: WGPUProcSurfaceGetPreferredFormat,
    dataView?: DataViewExt
): DataViewExt<WGPUProcSurfaceGetPreferredFormat> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcSurfaceGetPreferredFormat_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcSurfacePresent(
    x: WGPUProcSurfacePresent,
    dataView?: DataViewExt
): DataViewExt<WGPUProcSurfacePresent> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcSurfacePresent_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcSurfaceUnconfigure(
    x: WGPUProcSurfaceUnconfigure,
    dataView?: DataViewExt
): DataViewExt<WGPUProcSurfaceUnconfigure> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcSurfaceUnconfigure_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcSurfaceReference(
    x: WGPUProcSurfaceReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcSurfaceReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcSurfaceReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcSurfaceRelease(
    x: WGPUProcSurfaceRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcSurfaceRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcSurfaceRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcSurfaceCapabilitiesFreeMembers(
    x: WGPUProcSurfaceCapabilitiesFreeMembers,
    dataView?: DataViewExt
): DataViewExt<WGPUProcSurfaceCapabilitiesFreeMembers> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcSurfaceCapabilitiesFreeMembers_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureCreateView(
    x: WGPUProcTextureCreateView,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureCreateView> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureCreateView_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureDestroy(
    x: WGPUProcTextureDestroy,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureDestroy> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureDestroy_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureGetDepthOrArrayLayers(
    x: WGPUProcTextureGetDepthOrArrayLayers,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureGetDepthOrArrayLayers> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureGetDepthOrArrayLayers_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureGetDimension(
    x: WGPUProcTextureGetDimension,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureGetDimension> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureGetDimension_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureGetFormat(
    x: WGPUProcTextureGetFormat,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureGetFormat> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureGetFormat_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureGetHeight(
    x: WGPUProcTextureGetHeight,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureGetHeight> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureGetHeight_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureGetMipLevelCount(
    x: WGPUProcTextureGetMipLevelCount,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureGetMipLevelCount> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureGetMipLevelCount_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureGetSampleCount(
    x: WGPUProcTextureGetSampleCount,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureGetSampleCount> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureGetSampleCount_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureGetUsage(
    x: WGPUProcTextureGetUsage,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureGetUsage> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureGetUsage_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureGetWidth(
    x: WGPUProcTextureGetWidth,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureGetWidth> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureGetWidth_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureSetLabel(
    x: WGPUProcTextureSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureReference(
    x: WGPUProcTextureReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureRelease(
    x: WGPUProcTextureRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureViewSetLabel(
    x: WGPUProcTextureViewSetLabel,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureViewSetLabel> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureViewSetLabel_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureViewReference(
    x: WGPUProcTextureViewReference,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureViewReference> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureViewReference_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}

export function writeWGPUProcTextureViewRelease(
    x: WGPUProcTextureViewRelease,
    dataView?: DataViewExt
): DataViewExt<WGPUProcTextureViewRelease> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(
        x,
        eval("WGPUProcTextureViewRelease_funcdef")
    ).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}
export function writeWGPUNativeSType(
    x: WGPUNativeSType,
    dataView?: DataViewExt
): DataViewExt<WGPUNativeSType> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUNativeFeature(
    x: WGPUNativeFeature,
    dataView?: DataViewExt
): DataViewExt<WGPUNativeFeature> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPULogLevel(
    x: WGPULogLevel,
    dataView?: DataViewExt
): DataViewExt<WGPULogLevel> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUInstanceBackend(
    x: WGPUInstanceBackend,
    dataView?: DataViewExt
): DataViewExt<WGPUInstanceBackend> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export const writeWGPUInstanceBackendFlags = writeWGPUFlags;
export function writeWGPUInstanceFlag(
    x: WGPUInstanceFlag,
    dataView?: DataViewExt
): DataViewExt<WGPUInstanceFlag> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export const writeWGPUInstanceFlags = writeWGPUFlags;
export function writeWGPUDx12Compiler(
    x: WGPUDx12Compiler,
    dataView?: DataViewExt
): DataViewExt<WGPUDx12Compiler> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUGles3MinorVersion(
    x: WGPUGles3MinorVersion,
    dataView?: DataViewExt
): DataViewExt<WGPUGles3MinorVersion> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUPipelineStatisticName(
    x: WGPUPipelineStatisticName,
    dataView?: DataViewExt
): DataViewExt<WGPUPipelineStatisticName> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUNativeQueryType(
    x: WGPUNativeQueryType,
    dataView?: DataViewExt
): DataViewExt<WGPUNativeQueryType> {
    if (!dataView) dataView = DataViewExt.alloc(4);
    dataView.setInt32(0, x, true);
    return dataView as any;
}
export function writeWGPUInstanceExtras(
    x: DeepPartial<WGPUInstanceExtras>,
    dataView?: DataViewExt
): DataViewExt<WGPUInstanceExtras> {
    if (!dataView) dataView = DataViewExt.alloc(48);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));
    writeWGPUInstanceBackendFlags(x.backends! as any, dataView.subview(16));
    writeWGPUInstanceFlags(x.flags! as any, dataView.subview(20));
    writeWGPUDx12Compiler(x.dx12ShaderCompiler! as any, dataView.subview(24));
    writeWGPUGles3MinorVersion(
        x.gles3MinorVersion! as any,
        dataView.subview(28)
    );
    writeCString(x.dxilPath! as any, dataView.subview(32));
    writeCString(x.dxcPath! as any, dataView.subview(40));
    return dataView as any;
}
export function writeWGPUDeviceExtras(
    x: DeepPartial<WGPUDeviceExtras>,
    dataView?: DataViewExt
): DataViewExt<WGPUDeviceExtras> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));
    writeCString(x.tracePath! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUNativeLimits(
    x: DeepPartial<WGPUNativeLimits>,
    dataView?: DataViewExt
): DataViewExt<WGPUNativeLimits> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    writeuint32_t(x.maxPushConstantSize! as any, dataView.subview(0));
    writeuint32_t(x.maxNonSamplerBindings! as any, dataView.subview(4));
    return dataView as any;
}
export function writeWGPURequiredLimitsExtras(
    x: DeepPartial<WGPURequiredLimitsExtras>,
    dataView?: DataViewExt
): DataViewExt<WGPURequiredLimitsExtras> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));
    writeWGPUNativeLimits(x.limits! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUSupportedLimitsExtras(
    x: DeepPartial<WGPUSupportedLimitsExtras>,
    dataView?: DataViewExt
): DataViewExt<WGPUSupportedLimitsExtras> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUChainedStructOut(x.chain! as any, dataView.subview(0));
    writeWGPUNativeLimits(x.limits! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUPushConstantRange(
    x: DeepPartial<WGPUPushConstantRange>,
    dataView?: DataViewExt
): DataViewExt<WGPUPushConstantRange> {
    if (!dataView) dataView = DataViewExt.alloc(12);
    if (!x) return dataView as any;
    writeWGPUShaderStageFlags(x.stages! as any, dataView.subview(0));
    writeuint32_t(x.start! as any, dataView.subview(4));
    writeuint32_t(x.end! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUPipelineLayoutExtras(
    x: DeepPartial<WGPUPipelineLayoutExtras>,
    dataView?: DataViewExt
): DataViewExt<WGPUPipelineLayoutExtras> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));
    writesize_t(x.pushConstantRangeCount! as any, dataView.subview(16));

    if (isPtrOrConstPtr(x.pushConstantRanges)) {
        writeConstPtrT<WGPUPushConstantRange>(
            x.pushConstantRanges! as any,
            dataView.subview(24)
        );
    } else {
        writeWGPUPushConstantRange(
            x.pushConstantRanges! as any,
            dataView.subview(24)
        );
    }

    return dataView as any;
}
export const writeWGPUSubmissionIndex = writeuint64_t;
export function writeWGPUWrappedSubmissionIndex(
    x: DeepPartial<WGPUWrappedSubmissionIndex>,
    dataView?: DataViewExt
): DataViewExt<WGPUWrappedSubmissionIndex> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;
    writeWGPUQueue(x.queue! as any, dataView.subview(0));
    writeWGPUSubmissionIndex(x.submissionIndex! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUShaderDefine(
    x: DeepPartial<WGPUShaderDefine>,
    dataView?: DataViewExt
): DataViewExt<WGPUShaderDefine> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;
    writeCString(x.name! as any, dataView.subview(0));
    writeCString(x.value! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUShaderModuleGLSLDescriptor(
    x: DeepPartial<WGPUShaderModuleGLSLDescriptor>,
    dataView?: DataViewExt
): DataViewExt<WGPUShaderModuleGLSLDescriptor> {
    if (!dataView) dataView = DataViewExt.alloc(48);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));
    writeWGPUShaderStage(x.stage! as any, dataView.subview(16));
    writeCString(x.code! as any, dataView.subview(24));
    writeuint32_t(x.defineCount! as any, dataView.subview(32));

    if (isPtrOrConstPtr(x.defines)) {
        writePtrT<WGPUShaderDefine>(x.defines! as any, dataView.subview(40));
    } else {
        writeWGPUShaderDefine(x.defines! as any, dataView.subview(40));
    }

    return dataView as any;
}
export function writeWGPURegistryReport(
    x: DeepPartial<WGPURegistryReport>,
    dataView?: DataViewExt
): DataViewExt<WGPURegistryReport> {
    if (!dataView) dataView = DataViewExt.alloc(40);
    if (!x) return dataView as any;
    writesize_t(x.numAllocated! as any, dataView.subview(0));
    writesize_t(x.numKeptFromUser! as any, dataView.subview(8));
    writesize_t(x.numReleasedFromUser! as any, dataView.subview(16));
    writesize_t(x.numError! as any, dataView.subview(24));
    writesize_t(x.elementSize! as any, dataView.subview(32));
    return dataView as any;
}
export function writeWGPUHubReport(
    x: DeepPartial<WGPUHubReport>,
    dataView?: DataViewExt
): DataViewExt<WGPUHubReport> {
    if (!dataView) dataView = DataViewExt.alloc(640);
    if (!x) return dataView as any;
    writeWGPURegistryReport(x.adapters! as any, dataView.subview(0));
    writeWGPURegistryReport(x.devices! as any, dataView.subview(40));
    writeWGPURegistryReport(x.queues! as any, dataView.subview(80));
    writeWGPURegistryReport(x.pipelineLayouts! as any, dataView.subview(120));
    writeWGPURegistryReport(x.shaderModules! as any, dataView.subview(160));
    writeWGPURegistryReport(x.bindGroupLayouts! as any, dataView.subview(200));
    writeWGPURegistryReport(x.bindGroups! as any, dataView.subview(240));
    writeWGPURegistryReport(x.commandBuffers! as any, dataView.subview(280));
    writeWGPURegistryReport(x.renderBundles! as any, dataView.subview(320));
    writeWGPURegistryReport(x.renderPipelines! as any, dataView.subview(360));
    writeWGPURegistryReport(x.computePipelines! as any, dataView.subview(400));
    writeWGPURegistryReport(x.querySets! as any, dataView.subview(440));
    writeWGPURegistryReport(x.buffers! as any, dataView.subview(480));
    writeWGPURegistryReport(x.textures! as any, dataView.subview(520));
    writeWGPURegistryReport(x.textureViews! as any, dataView.subview(560));
    writeWGPURegistryReport(x.samplers! as any, dataView.subview(600));
    return dataView as any;
}
export function writeWGPUGlobalReport(
    x: DeepPartial<WGPUGlobalReport>,
    dataView?: DataViewExt
): DataViewExt<WGPUGlobalReport> {
    if (!dataView) dataView = DataViewExt.alloc(2608);
    if (!x) return dataView as any;
    writeWGPURegistryReport(x.surfaces! as any, dataView.subview(0));
    writeWGPUBackendType(x.backendType! as any, dataView.subview(40));
    writeWGPUHubReport(x.vulkan! as any, dataView.subview(48));
    writeWGPUHubReport(x.metal! as any, dataView.subview(688));
    writeWGPUHubReport(x.dx12! as any, dataView.subview(1328));
    writeWGPUHubReport(x.gl! as any, dataView.subview(1968));
    return dataView as any;
}
export function writeWGPUInstanceEnumerateAdapterOptions(
    x: DeepPartial<WGPUInstanceEnumerateAdapterOptions>,
    dataView?: DataViewExt
): DataViewExt<WGPUInstanceEnumerateAdapterOptions> {
    if (!dataView) dataView = DataViewExt.alloc(16);
    if (!x) return dataView as any;

    if (isPtrOrConstPtr(x.nextInChain)) {
        writeConstPtrT<WGPUChainedStruct>(
            x.nextInChain! as any,
            dataView.subview(0)
        );
    } else {
        writeWGPUChainedStruct(x.nextInChain! as any, dataView.subview(0));
    }

    writeWGPUInstanceBackendFlags(x.backends! as any, dataView.subview(8));
    return dataView as any;
}
export function writeWGPUBindGroupEntryExtras(
    x: DeepPartial<WGPUBindGroupEntryExtras>,
    dataView?: DataViewExt
): DataViewExt<WGPUBindGroupEntryExtras> {
    if (!dataView) dataView = DataViewExt.alloc(64);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));

    if (isPtrOrConstPtr(x.buffers)) {
        writeConstPtrT<WGPUBuffer>(x.buffers! as any, dataView.subview(16));
    } else {
        writeWGPUBuffer(x.buffers! as any, dataView.subview(16));
    }

    writesize_t(x.bufferCount! as any, dataView.subview(24));

    if (isPtrOrConstPtr(x.samplers)) {
        writeConstPtrT<WGPUSampler>(x.samplers! as any, dataView.subview(32));
    } else {
        writeWGPUSampler(x.samplers! as any, dataView.subview(32));
    }

    writesize_t(x.samplerCount! as any, dataView.subview(40));

    if (isPtrOrConstPtr(x.textureViews)) {
        writeConstPtrT<WGPUTextureView>(
            x.textureViews! as any,
            dataView.subview(48)
        );
    } else {
        writeWGPUTextureView(x.textureViews! as any, dataView.subview(48));
    }

    writesize_t(x.textureViewCount! as any, dataView.subview(56));
    return dataView as any;
}
export function writeWGPUBindGroupLayoutEntryExtras(
    x: DeepPartial<WGPUBindGroupLayoutEntryExtras>,
    dataView?: DataViewExt
): DataViewExt<WGPUBindGroupLayoutEntryExtras> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));
    writeuint32_t(x.count! as any, dataView.subview(16));
    return dataView as any;
}
export function writeWGPUQuerySetDescriptorExtras(
    x: DeepPartial<WGPUQuerySetDescriptorExtras>,
    dataView?: DataViewExt
): DataViewExt<WGPUQuerySetDescriptorExtras> {
    if (!dataView) dataView = DataViewExt.alloc(32);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));

    if (isPtrOrConstPtr(x.pipelineStatistics)) {
        writeConstPtrT<WGPUPipelineStatisticName>(
            x.pipelineStatistics! as any,
            dataView.subview(16)
        );
    } else {
        writeWGPUPipelineStatisticName(
            x.pipelineStatistics! as any,
            dataView.subview(16)
        );
    }

    writesize_t(x.pipelineStatisticCount! as any, dataView.subview(24));
    return dataView as any;
}
export function writeWGPUSurfaceConfigurationExtras(
    x: DeepPartial<WGPUSurfaceConfigurationExtras>,
    dataView?: DataViewExt
): DataViewExt<WGPUSurfaceConfigurationExtras> {
    if (!dataView) dataView = DataViewExt.alloc(24);
    if (!x) return dataView as any;
    writeWGPUChainedStruct(x.chain! as any, dataView.subview(0));
    writeWGPUBool(x.desiredMaximumFrameLatency! as any, dataView.subview(16));
    return dataView as any;
}

export function writeWGPULogCallback(
    x: WGPULogCallback,
    dataView?: DataViewExt
): DataViewExt<WGPULogCallback> {
    if (!dataView) dataView = DataViewExt.alloc(8);
    if (!x) return dataView as any;
    const jscb_ptr = new BunJSCallback(x, eval("WGPULogCallback_funcdef")).ptr;
    dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
    return dataView as any;
}
export function readsize_t(from: BunPointer, offset: number): size_t {
    let out = bunRead.u64(from!, offset);
    return out;
}
export function readint32_t(from: BunPointer, offset: number): int32_t {
    let out = bunRead.i32(from!, offset);
    return out;
}
export function readuint16_t(from: BunPointer, offset: number): uint16_t {
    let out = bunRead.u16(from!, offset);
    return out;
}
export function readuint32_t(from: BunPointer, offset: number): uint32_t {
    let out = bunRead.u32(from!, offset);
    return out;
}
export function readuint64_t(from: BunPointer, offset: number): uint64_t {
    let out = bunRead.u64(from!, offset);
    return out;
}
export function readdouble(from: BunPointer, offset: number): double {
    let out = bunRead.f64(from!, offset);
    return out;
}
export function readfloat(from: BunPointer, offset: number): float {
    let out = bunRead.f32(from!, offset);
    return out;
}
export const readWGPUFlags = readuint32_t;
export const readWGPUBool = readuint32_t;
export function readWGPUAdapter(from: BunPointer, offset: number): WGPUAdapter {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUBindGroup(
    from: BunPointer,
    offset: number
): WGPUBindGroup {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUBindGroupLayout(
    from: BunPointer,
    offset: number
): WGPUBindGroupLayout {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUBuffer(from: BunPointer, offset: number): WGPUBuffer {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUCommandBuffer(
    from: BunPointer,
    offset: number
): WGPUCommandBuffer {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUCommandEncoder(
    from: BunPointer,
    offset: number
): WGPUCommandEncoder {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUComputePassEncoder(
    from: BunPointer,
    offset: number
): WGPUComputePassEncoder {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUComputePipeline(
    from: BunPointer,
    offset: number
): WGPUComputePipeline {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUDevice(from: BunPointer, offset: number): WGPUDevice {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUInstance(
    from: BunPointer,
    offset: number
): WGPUInstance {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUPipelineLayout(
    from: BunPointer,
    offset: number
): WGPUPipelineLayout {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUQuerySet(
    from: BunPointer,
    offset: number
): WGPUQuerySet {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUQueue(from: BunPointer, offset: number): WGPUQueue {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPURenderBundle(
    from: BunPointer,
    offset: number
): WGPURenderBundle {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPURenderBundleEncoder(
    from: BunPointer,
    offset: number
): WGPURenderBundleEncoder {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPURenderPassEncoder(
    from: BunPointer,
    offset: number
): WGPURenderPassEncoder {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPURenderPipeline(
    from: BunPointer,
    offset: number
): WGPURenderPipeline {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUSampler(from: BunPointer, offset: number): WGPUSampler {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUShaderModule(
    from: BunPointer,
    offset: number
): WGPUShaderModule {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUSurface(from: BunPointer, offset: number): WGPUSurface {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUTexture(from: BunPointer, offset: number): WGPUTexture {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUTextureView(
    from: BunPointer,
    offset: number
): WGPUTextureView {
    let out = bunRead.ptr(from!, offset);
    return out;
}
export function readWGPUAdapterType(
    from: Pointer,
    offset: number
): WGPUAdapterType {
    return bunRead.i32(from!, offset) as any as WGPUAdapterType;
}
export function readWGPUAddressMode(
    from: Pointer,
    offset: number
): WGPUAddressMode {
    return bunRead.i32(from!, offset) as any as WGPUAddressMode;
}
export function readWGPUBackendType(
    from: Pointer,
    offset: number
): WGPUBackendType {
    return bunRead.i32(from!, offset) as any as WGPUBackendType;
}
export function readWGPUBlendFactor(
    from: Pointer,
    offset: number
): WGPUBlendFactor {
    return bunRead.i32(from!, offset) as any as WGPUBlendFactor;
}
export function readWGPUBlendOperation(
    from: Pointer,
    offset: number
): WGPUBlendOperation {
    return bunRead.i32(from!, offset) as any as WGPUBlendOperation;
}
export function readWGPUBufferBindingType(
    from: Pointer,
    offset: number
): WGPUBufferBindingType {
    return bunRead.i32(from!, offset) as any as WGPUBufferBindingType;
}
export function readWGPUBufferMapAsyncStatus(
    from: Pointer,
    offset: number
): WGPUBufferMapAsyncStatus {
    return bunRead.i32(from!, offset) as any as WGPUBufferMapAsyncStatus;
}
export function readWGPUBufferMapState(
    from: Pointer,
    offset: number
): WGPUBufferMapState {
    return bunRead.i32(from!, offset) as any as WGPUBufferMapState;
}
export function readWGPUCompareFunction(
    from: Pointer,
    offset: number
): WGPUCompareFunction {
    return bunRead.i32(from!, offset) as any as WGPUCompareFunction;
}
export function readWGPUCompilationInfoRequestStatus(
    from: Pointer,
    offset: number
): WGPUCompilationInfoRequestStatus {
    return bunRead.i32(
        from!,
        offset
    ) as any as WGPUCompilationInfoRequestStatus;
}
export function readWGPUCompilationMessageType(
    from: Pointer,
    offset: number
): WGPUCompilationMessageType {
    return bunRead.i32(from!, offset) as any as WGPUCompilationMessageType;
}
export function readWGPUCompositeAlphaMode(
    from: Pointer,
    offset: number
): WGPUCompositeAlphaMode {
    return bunRead.i32(from!, offset) as any as WGPUCompositeAlphaMode;
}
export function readWGPUCreatePipelineAsyncStatus(
    from: Pointer,
    offset: number
): WGPUCreatePipelineAsyncStatus {
    return bunRead.i32(from!, offset) as any as WGPUCreatePipelineAsyncStatus;
}
export function readWGPUCullMode(from: Pointer, offset: number): WGPUCullMode {
    return bunRead.i32(from!, offset) as any as WGPUCullMode;
}
export function readWGPUDeviceLostReason(
    from: Pointer,
    offset: number
): WGPUDeviceLostReason {
    return bunRead.i32(from!, offset) as any as WGPUDeviceLostReason;
}
export function readWGPUErrorFilter(
    from: Pointer,
    offset: number
): WGPUErrorFilter {
    return bunRead.i32(from!, offset) as any as WGPUErrorFilter;
}
export function readWGPUErrorType(
    from: Pointer,
    offset: number
): WGPUErrorType {
    return bunRead.i32(from!, offset) as any as WGPUErrorType;
}
export function readWGPUFeatureName(
    from: Pointer,
    offset: number
): WGPUFeatureName {
    return bunRead.i32(from!, offset) as any as WGPUFeatureName;
}
export function readWGPUFilterMode(
    from: Pointer,
    offset: number
): WGPUFilterMode {
    return bunRead.i32(from!, offset) as any as WGPUFilterMode;
}
export function readWGPUFrontFace(
    from: Pointer,
    offset: number
): WGPUFrontFace {
    return bunRead.i32(from!, offset) as any as WGPUFrontFace;
}
export function readWGPUIndexFormat(
    from: Pointer,
    offset: number
): WGPUIndexFormat {
    return bunRead.i32(from!, offset) as any as WGPUIndexFormat;
}
export function readWGPULoadOp(from: Pointer, offset: number): WGPULoadOp {
    return bunRead.i32(from!, offset) as any as WGPULoadOp;
}
export function readWGPUMipmapFilterMode(
    from: Pointer,
    offset: number
): WGPUMipmapFilterMode {
    return bunRead.i32(from!, offset) as any as WGPUMipmapFilterMode;
}
export function readWGPUPowerPreference(
    from: Pointer,
    offset: number
): WGPUPowerPreference {
    return bunRead.i32(from!, offset) as any as WGPUPowerPreference;
}
export function readWGPUPresentMode(
    from: Pointer,
    offset: number
): WGPUPresentMode {
    return bunRead.i32(from!, offset) as any as WGPUPresentMode;
}
export function readWGPUPrimitiveTopology(
    from: Pointer,
    offset: number
): WGPUPrimitiveTopology {
    return bunRead.i32(from!, offset) as any as WGPUPrimitiveTopology;
}
export function readWGPUQueryType(
    from: Pointer,
    offset: number
): WGPUQueryType {
    return bunRead.i32(from!, offset) as any as WGPUQueryType;
}
export function readWGPUQueueWorkDoneStatus(
    from: Pointer,
    offset: number
): WGPUQueueWorkDoneStatus {
    return bunRead.i32(from!, offset) as any as WGPUQueueWorkDoneStatus;
}
export function readWGPURequestAdapterStatus(
    from: Pointer,
    offset: number
): WGPURequestAdapterStatus {
    return bunRead.i32(from!, offset) as any as WGPURequestAdapterStatus;
}
export function readWGPURequestDeviceStatus(
    from: Pointer,
    offset: number
): WGPURequestDeviceStatus {
    return bunRead.i32(from!, offset) as any as WGPURequestDeviceStatus;
}
export function readWGPUSType(from: Pointer, offset: number): WGPUSType {
    return bunRead.i32(from!, offset) as any as WGPUSType;
}
export function readWGPUSamplerBindingType(
    from: Pointer,
    offset: number
): WGPUSamplerBindingType {
    return bunRead.i32(from!, offset) as any as WGPUSamplerBindingType;
}
export function readWGPUStencilOperation(
    from: Pointer,
    offset: number
): WGPUStencilOperation {
    return bunRead.i32(from!, offset) as any as WGPUStencilOperation;
}
export function readWGPUStorageTextureAccess(
    from: Pointer,
    offset: number
): WGPUStorageTextureAccess {
    return bunRead.i32(from!, offset) as any as WGPUStorageTextureAccess;
}
export function readWGPUStoreOp(from: Pointer, offset: number): WGPUStoreOp {
    return bunRead.i32(from!, offset) as any as WGPUStoreOp;
}
export function readWGPUSurfaceGetCurrentTextureStatus(
    from: Pointer,
    offset: number
): WGPUSurfaceGetCurrentTextureStatus {
    return bunRead.i32(
        from!,
        offset
    ) as any as WGPUSurfaceGetCurrentTextureStatus;
}
export function readWGPUTextureAspect(
    from: Pointer,
    offset: number
): WGPUTextureAspect {
    return bunRead.i32(from!, offset) as any as WGPUTextureAspect;
}
export function readWGPUTextureDimension(
    from: Pointer,
    offset: number
): WGPUTextureDimension {
    return bunRead.i32(from!, offset) as any as WGPUTextureDimension;
}
export function readWGPUTextureFormat(
    from: Pointer,
    offset: number
): WGPUTextureFormat {
    return bunRead.i32(from!, offset) as any as WGPUTextureFormat;
}
export function readWGPUTextureSampleType(
    from: Pointer,
    offset: number
): WGPUTextureSampleType {
    return bunRead.i32(from!, offset) as any as WGPUTextureSampleType;
}
export function readWGPUTextureViewDimension(
    from: Pointer,
    offset: number
): WGPUTextureViewDimension {
    return bunRead.i32(from!, offset) as any as WGPUTextureViewDimension;
}
export function readWGPUVertexFormat(
    from: Pointer,
    offset: number
): WGPUVertexFormat {
    return bunRead.i32(from!, offset) as any as WGPUVertexFormat;
}
export function readWGPUVertexStepMode(
    from: Pointer,
    offset: number
): WGPUVertexStepMode {
    return bunRead.i32(from!, offset) as any as WGPUVertexStepMode;
}
export function readWGPUBufferUsage(
    from: Pointer,
    offset: number
): WGPUBufferUsage {
    return bunRead.i32(from!, offset) as any as WGPUBufferUsage;
}
export const readWGPUBufferUsageFlags = readWGPUFlags;
export function readWGPUColorWriteMask(
    from: Pointer,
    offset: number
): WGPUColorWriteMask {
    return bunRead.i32(from!, offset) as any as WGPUColorWriteMask;
}
export const readWGPUColorWriteMaskFlags = readWGPUFlags;
export function readWGPUMapMode(from: Pointer, offset: number): WGPUMapMode {
    return bunRead.i32(from!, offset) as any as WGPUMapMode;
}
export const readWGPUMapModeFlags = readWGPUFlags;
export function readWGPUShaderStage(
    from: Pointer,
    offset: number
): WGPUShaderStage {
    return bunRead.i32(from!, offset) as any as WGPUShaderStage;
}
export const readWGPUShaderStageFlags = readWGPUFlags;
export function readWGPUTextureUsage(
    from: Pointer,
    offset: number
): WGPUTextureUsage {
    return bunRead.i32(from!, offset) as any as WGPUTextureUsage;
}
export const readWGPUTextureUsageFlags = readWGPUFlags;

export function readWGPUBufferMapCallback(
    from: Pointer,
    offset: number
): WGPUBufferMapCallback {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUBufferMapCallback_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUBufferMapCallback;
}

export function readWGPUCompilationInfoCallback(
    from: Pointer,
    offset: number
): WGPUCompilationInfoCallback {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUCompilationInfoCallback_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUCompilationInfoCallback;
}

export function readWGPUCreateComputePipelineAsyncCallback(
    from: Pointer,
    offset: number
): WGPUCreateComputePipelineAsyncCallback {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUCreateComputePipelineAsyncCallback_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUCreateComputePipelineAsyncCallback;
}

export function readWGPUCreateRenderPipelineAsyncCallback(
    from: Pointer,
    offset: number
): WGPUCreateRenderPipelineAsyncCallback {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUCreateRenderPipelineAsyncCallback_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUCreateRenderPipelineAsyncCallback;
}

export function readWGPUDeviceLostCallback(
    from: Pointer,
    offset: number
): WGPUDeviceLostCallback {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUDeviceLostCallback_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUDeviceLostCallback;
}

export function readWGPUErrorCallback(
    from: Pointer,
    offset: number
): WGPUErrorCallback {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUErrorCallback_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUErrorCallback;
}

export function readWGPUProc(from: Pointer, offset: number): WGPUProc {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProc_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProc;
}

export function readWGPUQueueWorkDoneCallback(
    from: Pointer,
    offset: number
): WGPUQueueWorkDoneCallback {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUQueueWorkDoneCallback_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUQueueWorkDoneCallback;
}

export function readWGPURequestAdapterCallback(
    from: Pointer,
    offset: number
): WGPURequestAdapterCallback {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPURequestAdapterCallback_funcdef"),
        ptr: p,
    });
    return cb as any as WGPURequestAdapterCallback;
}

export function readWGPURequestDeviceCallback(
    from: Pointer,
    offset: number
): WGPURequestDeviceCallback {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPURequestDeviceCallback_funcdef"),
        ptr: p,
    });
    return cb as any as WGPURequestDeviceCallback;
}
export function readWGPUChainedStruct(
    from: Pointer,
    offset: number
): WGPUChainedStruct {
    const out: WGPUChainedStruct = {} as any;
    out.next = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.sType = from ? (readWGPUSType(from!, offset + 8) as any) : undefined!;
    return out;
}
export function readWGPUChainedStructOut(
    from: Pointer,
    offset: number
): WGPUChainedStructOut {
    const out: WGPUChainedStructOut = {} as any;
    out.next = from
        ? (readPtrT<WGPUChainedStructOut>(from!, offset + 0) as any)
        : undefined!;
    out.sType = from ? (readWGPUSType(from!, offset + 8) as any) : undefined!;
    return out;
}
export function readWGPUAdapterProperties(
    from: Pointer,
    offset: number
): WGPUAdapterProperties {
    const out: WGPUAdapterProperties = {} as any;
    out.nextInChain = from
        ? (readPtrT<WGPUChainedStructOut>(from!, offset + 0) as any)
        : undefined!;
    out.vendorID = from ? (readuint32_t(from!, offset + 8) as any) : undefined!;
    out.vendorName = from
        ? (readCString(from!, offset + 16) as any)
        : undefined!;
    out.architecture = from
        ? (readCString(from!, offset + 24) as any)
        : undefined!;
    out.deviceID = from
        ? (readuint32_t(from!, offset + 32) as any)
        : undefined!;
    out.name = from ? (readCString(from!, offset + 40) as any) : undefined!;
    out.driverDescription = from
        ? (readCString(from!, offset + 48) as any)
        : undefined!;
    out.adapterType = from
        ? (readWGPUAdapterType(from!, offset + 56) as any)
        : undefined!;
    out.backendType = from
        ? (readWGPUBackendType(from!, offset + 60) as any)
        : undefined!;
    return out;
}
export function readWGPUBindGroupEntry(
    from: Pointer,
    offset: number
): WGPUBindGroupEntry {
    const out: WGPUBindGroupEntry = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.binding = from ? (readuint32_t(from!, offset + 8) as any) : undefined!;
    out.buffer = from
        ? (readWGPUBuffer(from!, offset + 16) as any)
        : undefined!;
    out.offset = from ? (readuint64_t(from!, offset + 24) as any) : undefined!;
    out.size = from ? (readuint64_t(from!, offset + 32) as any) : undefined!;
    out.sampler = from
        ? (readWGPUSampler(from!, offset + 40) as any)
        : undefined!;
    out.textureView = from
        ? (readWGPUTextureView(from!, offset + 48) as any)
        : undefined!;
    return out;
}
export function readWGPUBlendComponent(
    from: Pointer,
    offset: number
): WGPUBlendComponent {
    const out: WGPUBlendComponent = {} as any;
    out.operation = from
        ? (readWGPUBlendOperation(from!, offset + 0) as any)
        : undefined!;
    out.srcFactor = from
        ? (readWGPUBlendFactor(from!, offset + 4) as any)
        : undefined!;
    out.dstFactor = from
        ? (readWGPUBlendFactor(from!, offset + 8) as any)
        : undefined!;
    return out;
}
export function readWGPUBufferBindingLayout(
    from: Pointer,
    offset: number
): WGPUBufferBindingLayout {
    const out: WGPUBufferBindingLayout = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.type = from
        ? (readWGPUBufferBindingType(from!, offset + 8) as any)
        : undefined!;
    out.hasDynamicOffset = from
        ? (readWGPUBool(from!, offset + 12) as any)
        : undefined!;
    out.minBindingSize = from
        ? (readuint64_t(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPUBufferDescriptor(
    from: Pointer,
    offset: number
): WGPUBufferDescriptor {
    const out: WGPUBufferDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.usage = from
        ? (readWGPUBufferUsageFlags(from!, offset + 16) as any)
        : undefined!;
    out.size = from ? (readuint64_t(from!, offset + 24) as any) : undefined!;
    out.mappedAtCreation = from
        ? (readWGPUBool(from!, offset + 32) as any)
        : undefined!;
    return out;
}
export function readWGPUColor(from: Pointer, offset: number): WGPUColor {
    const out: WGPUColor = {} as any;
    out.r = from ? (readdouble(from!, offset + 0) as any) : undefined!;
    out.g = from ? (readdouble(from!, offset + 8) as any) : undefined!;
    out.b = from ? (readdouble(from!, offset + 16) as any) : undefined!;
    out.a = from ? (readdouble(from!, offset + 24) as any) : undefined!;
    return out;
}
export function readWGPUCommandBufferDescriptor(
    from: Pointer,
    offset: number
): WGPUCommandBufferDescriptor {
    const out: WGPUCommandBufferDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    return out;
}
export function readWGPUCommandEncoderDescriptor(
    from: Pointer,
    offset: number
): WGPUCommandEncoderDescriptor {
    const out: WGPUCommandEncoderDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    return out;
}
export function readWGPUCompilationMessage(
    from: Pointer,
    offset: number
): WGPUCompilationMessage {
    const out: WGPUCompilationMessage = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.message = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.type = from
        ? (readWGPUCompilationMessageType(from!, offset + 16) as any)
        : undefined!;
    out.lineNum = from ? (readuint64_t(from!, offset + 24) as any) : undefined!;
    out.linePos = from ? (readuint64_t(from!, offset + 32) as any) : undefined!;
    out.offset = from ? (readuint64_t(from!, offset + 40) as any) : undefined!;
    out.length = from ? (readuint64_t(from!, offset + 48) as any) : undefined!;
    out.utf16LinePos = from
        ? (readuint64_t(from!, offset + 56) as any)
        : undefined!;
    out.utf16Offset = from
        ? (readuint64_t(from!, offset + 64) as any)
        : undefined!;
    out.utf16Length = from
        ? (readuint64_t(from!, offset + 72) as any)
        : undefined!;
    return out;
}
export function readWGPUComputePassTimestampWrites(
    from: Pointer,
    offset: number
): WGPUComputePassTimestampWrites {
    const out: WGPUComputePassTimestampWrites = {} as any;
    out.querySet = from
        ? (readWGPUQuerySet(from!, offset + 0) as any)
        : undefined!;
    out.beginningOfPassWriteIndex = from
        ? (readuint32_t(from!, offset + 8) as any)
        : undefined!;
    out.endOfPassWriteIndex = from
        ? (readuint32_t(from!, offset + 12) as any)
        : undefined!;
    return out;
}
export function readWGPUConstantEntry(
    from: Pointer,
    offset: number
): WGPUConstantEntry {
    const out: WGPUConstantEntry = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.key = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.value = from ? (readdouble(from!, offset + 16) as any) : undefined!;
    return out;
}
export function readWGPUExtent3D(from: Pointer, offset: number): WGPUExtent3D {
    const out: WGPUExtent3D = {} as any;
    out.width = from ? (readuint32_t(from!, offset + 0) as any) : undefined!;
    out.height = from ? (readuint32_t(from!, offset + 4) as any) : undefined!;
    out.depthOrArrayLayers = from
        ? (readuint32_t(from!, offset + 8) as any)
        : undefined!;
    return out;
}
export function readWGPUInstanceDescriptor(
    from: Pointer,
    offset: number
): WGPUInstanceDescriptor {
    const out: WGPUInstanceDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    return out;
}
export function readWGPULimits(from: Pointer, offset: number): WGPULimits {
    const out: WGPULimits = {} as any;
    out.maxTextureDimension1D = from
        ? (readuint32_t(from!, offset + 0) as any)
        : undefined!;
    out.maxTextureDimension2D = from
        ? (readuint32_t(from!, offset + 4) as any)
        : undefined!;
    out.maxTextureDimension3D = from
        ? (readuint32_t(from!, offset + 8) as any)
        : undefined!;
    out.maxTextureArrayLayers = from
        ? (readuint32_t(from!, offset + 12) as any)
        : undefined!;
    out.maxBindGroups = from
        ? (readuint32_t(from!, offset + 16) as any)
        : undefined!;
    out.maxBindGroupsPlusVertexBuffers = from
        ? (readuint32_t(from!, offset + 20) as any)
        : undefined!;
    out.maxBindingsPerBindGroup = from
        ? (readuint32_t(from!, offset + 24) as any)
        : undefined!;
    out.maxDynamicUniformBuffersPerPipelineLayout = from
        ? (readuint32_t(from!, offset + 28) as any)
        : undefined!;
    out.maxDynamicStorageBuffersPerPipelineLayout = from
        ? (readuint32_t(from!, offset + 32) as any)
        : undefined!;
    out.maxSampledTexturesPerShaderStage = from
        ? (readuint32_t(from!, offset + 36) as any)
        : undefined!;
    out.maxSamplersPerShaderStage = from
        ? (readuint32_t(from!, offset + 40) as any)
        : undefined!;
    out.maxStorageBuffersPerShaderStage = from
        ? (readuint32_t(from!, offset + 44) as any)
        : undefined!;
    out.maxStorageTexturesPerShaderStage = from
        ? (readuint32_t(from!, offset + 48) as any)
        : undefined!;
    out.maxUniformBuffersPerShaderStage = from
        ? (readuint32_t(from!, offset + 52) as any)
        : undefined!;
    out.maxUniformBufferBindingSize = from
        ? (readuint64_t(from!, offset + 56) as any)
        : undefined!;
    out.maxStorageBufferBindingSize = from
        ? (readuint64_t(from!, offset + 64) as any)
        : undefined!;
    out.minUniformBufferOffsetAlignment = from
        ? (readuint32_t(from!, offset + 72) as any)
        : undefined!;
    out.minStorageBufferOffsetAlignment = from
        ? (readuint32_t(from!, offset + 76) as any)
        : undefined!;
    out.maxVertexBuffers = from
        ? (readuint32_t(from!, offset + 80) as any)
        : undefined!;
    out.maxBufferSize = from
        ? (readuint64_t(from!, offset + 88) as any)
        : undefined!;
    out.maxVertexAttributes = from
        ? (readuint32_t(from!, offset + 96) as any)
        : undefined!;
    out.maxVertexBufferArrayStride = from
        ? (readuint32_t(from!, offset + 100) as any)
        : undefined!;
    out.maxInterStageShaderComponents = from
        ? (readuint32_t(from!, offset + 104) as any)
        : undefined!;
    out.maxInterStageShaderVariables = from
        ? (readuint32_t(from!, offset + 108) as any)
        : undefined!;
    out.maxColorAttachments = from
        ? (readuint32_t(from!, offset + 112) as any)
        : undefined!;
    out.maxColorAttachmentBytesPerSample = from
        ? (readuint32_t(from!, offset + 116) as any)
        : undefined!;
    out.maxComputeWorkgroupStorageSize = from
        ? (readuint32_t(from!, offset + 120) as any)
        : undefined!;
    out.maxComputeInvocationsPerWorkgroup = from
        ? (readuint32_t(from!, offset + 124) as any)
        : undefined!;
    out.maxComputeWorkgroupSizeX = from
        ? (readuint32_t(from!, offset + 128) as any)
        : undefined!;
    out.maxComputeWorkgroupSizeY = from
        ? (readuint32_t(from!, offset + 132) as any)
        : undefined!;
    out.maxComputeWorkgroupSizeZ = from
        ? (readuint32_t(from!, offset + 136) as any)
        : undefined!;
    out.maxComputeWorkgroupsPerDimension = from
        ? (readuint32_t(from!, offset + 140) as any)
        : undefined!;
    return out;
}
export function readWGPUMultisampleState(
    from: Pointer,
    offset: number
): WGPUMultisampleState {
    const out: WGPUMultisampleState = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.count = from ? (readuint32_t(from!, offset + 8) as any) : undefined!;
    out.mask = from ? (readuint32_t(from!, offset + 12) as any) : undefined!;
    out.alphaToCoverageEnabled = from
        ? (readWGPUBool(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPUOrigin3D(from: Pointer, offset: number): WGPUOrigin3D {
    const out: WGPUOrigin3D = {} as any;
    out.x = from ? (readuint32_t(from!, offset + 0) as any) : undefined!;
    out.y = from ? (readuint32_t(from!, offset + 4) as any) : undefined!;
    out.z = from ? (readuint32_t(from!, offset + 8) as any) : undefined!;
    return out;
}
export function readWGPUPipelineLayoutDescriptor(
    from: Pointer,
    offset: number
): WGPUPipelineLayoutDescriptor {
    const out: WGPUPipelineLayoutDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.bindGroupLayoutCount = from
        ? (readsize_t(from!, offset + 16) as any)
        : undefined!;
    out.bindGroupLayouts = from
        ? (readConstPtrT<WGPUBindGroupLayout>(from!, offset + 24) as any)
        : undefined!;
    return out;
}
export function readWGPUPrimitiveDepthClipControl(
    from: Pointer,
    offset: number
): WGPUPrimitiveDepthClipControl {
    const out: WGPUPrimitiveDepthClipControl = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.unclippedDepth = from
        ? (readWGPUBool(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPUPrimitiveState(
    from: Pointer,
    offset: number
): WGPUPrimitiveState {
    const out: WGPUPrimitiveState = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.topology = from
        ? (readWGPUPrimitiveTopology(from!, offset + 8) as any)
        : undefined!;
    out.stripIndexFormat = from
        ? (readWGPUIndexFormat(from!, offset + 12) as any)
        : undefined!;
    out.frontFace = from
        ? (readWGPUFrontFace(from!, offset + 16) as any)
        : undefined!;
    out.cullMode = from
        ? (readWGPUCullMode(from!, offset + 20) as any)
        : undefined!;
    return out;
}
export function readWGPUQuerySetDescriptor(
    from: Pointer,
    offset: number
): WGPUQuerySetDescriptor {
    const out: WGPUQuerySetDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.type = from
        ? (readWGPUQueryType(from!, offset + 16) as any)
        : undefined!;
    out.count = from ? (readuint32_t(from!, offset + 20) as any) : undefined!;
    return out;
}
export function readWGPUQueueDescriptor(
    from: Pointer,
    offset: number
): WGPUQueueDescriptor {
    const out: WGPUQueueDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    return out;
}
export function readWGPURenderBundleDescriptor(
    from: Pointer,
    offset: number
): WGPURenderBundleDescriptor {
    const out: WGPURenderBundleDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    return out;
}
export function readWGPURenderBundleEncoderDescriptor(
    from: Pointer,
    offset: number
): WGPURenderBundleEncoderDescriptor {
    const out: WGPURenderBundleEncoderDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.colorFormatCount = from
        ? (readsize_t(from!, offset + 16) as any)
        : undefined!;
    out.colorFormats = from
        ? (readConstPtrT<WGPUTextureFormat>(from!, offset + 24) as any)
        : undefined!;
    out.depthStencilFormat = from
        ? (readWGPUTextureFormat(from!, offset + 32) as any)
        : undefined!;
    out.sampleCount = from
        ? (readuint32_t(from!, offset + 36) as any)
        : undefined!;
    out.depthReadOnly = from
        ? (readWGPUBool(from!, offset + 40) as any)
        : undefined!;
    out.stencilReadOnly = from
        ? (readWGPUBool(from!, offset + 44) as any)
        : undefined!;
    return out;
}
export function readWGPURenderPassDepthStencilAttachment(
    from: Pointer,
    offset: number
): WGPURenderPassDepthStencilAttachment {
    const out: WGPURenderPassDepthStencilAttachment = {} as any;
    out.view = from
        ? (readWGPUTextureView(from!, offset + 0) as any)
        : undefined!;
    out.depthLoadOp = from
        ? (readWGPULoadOp(from!, offset + 8) as any)
        : undefined!;
    out.depthStoreOp = from
        ? (readWGPUStoreOp(from!, offset + 12) as any)
        : undefined!;
    out.depthClearValue = from
        ? (readfloat(from!, offset + 16) as any)
        : undefined!;
    out.depthReadOnly = from
        ? (readWGPUBool(from!, offset + 20) as any)
        : undefined!;
    out.stencilLoadOp = from
        ? (readWGPULoadOp(from!, offset + 24) as any)
        : undefined!;
    out.stencilStoreOp = from
        ? (readWGPUStoreOp(from!, offset + 28) as any)
        : undefined!;
    out.stencilClearValue = from
        ? (readuint32_t(from!, offset + 32) as any)
        : undefined!;
    out.stencilReadOnly = from
        ? (readWGPUBool(from!, offset + 36) as any)
        : undefined!;
    return out;
}
export function readWGPURenderPassDescriptorMaxDrawCount(
    from: Pointer,
    offset: number
): WGPURenderPassDescriptorMaxDrawCount {
    const out: WGPURenderPassDescriptorMaxDrawCount = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.maxDrawCount = from
        ? (readuint64_t(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPURenderPassTimestampWrites(
    from: Pointer,
    offset: number
): WGPURenderPassTimestampWrites {
    const out: WGPURenderPassTimestampWrites = {} as any;
    out.querySet = from
        ? (readWGPUQuerySet(from!, offset + 0) as any)
        : undefined!;
    out.beginningOfPassWriteIndex = from
        ? (readuint32_t(from!, offset + 8) as any)
        : undefined!;
    out.endOfPassWriteIndex = from
        ? (readuint32_t(from!, offset + 12) as any)
        : undefined!;
    return out;
}
export function readWGPURequestAdapterOptions(
    from: Pointer,
    offset: number
): WGPURequestAdapterOptions {
    const out: WGPURequestAdapterOptions = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.compatibleSurface = from
        ? (readWGPUSurface(from!, offset + 8) as any)
        : undefined!;
    out.powerPreference = from
        ? (readWGPUPowerPreference(from!, offset + 16) as any)
        : undefined!;
    out.backendType = from
        ? (readWGPUBackendType(from!, offset + 20) as any)
        : undefined!;
    out.forceFallbackAdapter = from
        ? (readWGPUBool(from!, offset + 24) as any)
        : undefined!;
    return out;
}
export function readWGPUSamplerBindingLayout(
    from: Pointer,
    offset: number
): WGPUSamplerBindingLayout {
    const out: WGPUSamplerBindingLayout = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.type = from
        ? (readWGPUSamplerBindingType(from!, offset + 8) as any)
        : undefined!;
    return out;
}
export function readWGPUSamplerDescriptor(
    from: Pointer,
    offset: number
): WGPUSamplerDescriptor {
    const out: WGPUSamplerDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.addressModeU = from
        ? (readWGPUAddressMode(from!, offset + 16) as any)
        : undefined!;
    out.addressModeV = from
        ? (readWGPUAddressMode(from!, offset + 20) as any)
        : undefined!;
    out.addressModeW = from
        ? (readWGPUAddressMode(from!, offset + 24) as any)
        : undefined!;
    out.magFilter = from
        ? (readWGPUFilterMode(from!, offset + 28) as any)
        : undefined!;
    out.minFilter = from
        ? (readWGPUFilterMode(from!, offset + 32) as any)
        : undefined!;
    out.mipmapFilter = from
        ? (readWGPUMipmapFilterMode(from!, offset + 36) as any)
        : undefined!;
    out.lodMinClamp = from
        ? (readfloat(from!, offset + 40) as any)
        : undefined!;
    out.lodMaxClamp = from
        ? (readfloat(from!, offset + 44) as any)
        : undefined!;
    out.compare = from
        ? (readWGPUCompareFunction(from!, offset + 48) as any)
        : undefined!;
    out.maxAnisotropy = from
        ? (readuint16_t(from!, offset + 52) as any)
        : undefined!;
    return out;
}
export function readWGPUShaderModuleCompilationHint(
    from: Pointer,
    offset: number
): WGPUShaderModuleCompilationHint {
    const out: WGPUShaderModuleCompilationHint = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.entryPoint = from
        ? (readCString(from!, offset + 8) as any)
        : undefined!;
    out.layout = from
        ? (readWGPUPipelineLayout(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPUShaderModuleSPIRVDescriptor(
    from: Pointer,
    offset: number
): WGPUShaderModuleSPIRVDescriptor {
    const out: WGPUShaderModuleSPIRVDescriptor = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.codeSize = from
        ? (readuint32_t(from!, offset + 16) as any)
        : undefined!;
    out.code = from
        ? (readConstPtrT<uint32_t>(from!, offset + 24) as any)
        : undefined!;
    return out;
}
export function readWGPUShaderModuleWGSLDescriptor(
    from: Pointer,
    offset: number
): WGPUShaderModuleWGSLDescriptor {
    const out: WGPUShaderModuleWGSLDescriptor = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.code = from ? (readCString(from!, offset + 16) as any) : undefined!;
    return out;
}
export function readWGPUStencilFaceState(
    from: Pointer,
    offset: number
): WGPUStencilFaceState {
    const out: WGPUStencilFaceState = {} as any;
    out.compare = from
        ? (readWGPUCompareFunction(from!, offset + 0) as any)
        : undefined!;
    out.failOp = from
        ? (readWGPUStencilOperation(from!, offset + 4) as any)
        : undefined!;
    out.depthFailOp = from
        ? (readWGPUStencilOperation(from!, offset + 8) as any)
        : undefined!;
    out.passOp = from
        ? (readWGPUStencilOperation(from!, offset + 12) as any)
        : undefined!;
    return out;
}
export function readWGPUStorageTextureBindingLayout(
    from: Pointer,
    offset: number
): WGPUStorageTextureBindingLayout {
    const out: WGPUStorageTextureBindingLayout = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.access = from
        ? (readWGPUStorageTextureAccess(from!, offset + 8) as any)
        : undefined!;
    out.format = from
        ? (readWGPUTextureFormat(from!, offset + 12) as any)
        : undefined!;
    out.viewDimension = from
        ? (readWGPUTextureViewDimension(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPUSurfaceCapabilities(
    from: Pointer,
    offset: number
): WGPUSurfaceCapabilities {
    const out: WGPUSurfaceCapabilities = {} as any;
    out.nextInChain = from
        ? (readPtrT<WGPUChainedStructOut>(from!, offset + 0) as any)
        : undefined!;
    out.formatCount = from
        ? (readsize_t(from!, offset + 8) as any)
        : undefined!;
    out.formats = from
        ? (readPtrT<WGPUTextureFormat>(from!, offset + 16) as any)
        : undefined!;
    out.presentModeCount = from
        ? (readsize_t(from!, offset + 24) as any)
        : undefined!;
    out.presentModes = from
        ? (readPtrT<WGPUPresentMode>(from!, offset + 32) as any)
        : undefined!;
    out.alphaModeCount = from
        ? (readsize_t(from!, offset + 40) as any)
        : undefined!;
    out.alphaModes = from
        ? (readPtrT<WGPUCompositeAlphaMode>(from!, offset + 48) as any)
        : undefined!;
    return out;
}
export function readWGPUSurfaceConfiguration(
    from: Pointer,
    offset: number
): WGPUSurfaceConfiguration {
    const out: WGPUSurfaceConfiguration = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.device = from ? (readWGPUDevice(from!, offset + 8) as any) : undefined!;
    out.format = from
        ? (readWGPUTextureFormat(from!, offset + 16) as any)
        : undefined!;
    out.usage = from
        ? (readWGPUTextureUsageFlags(from!, offset + 20) as any)
        : undefined!;
    out.viewFormatCount = from
        ? (readsize_t(from!, offset + 24) as any)
        : undefined!;
    out.viewFormats = from
        ? (readConstPtrT<WGPUTextureFormat>(from!, offset + 32) as any)
        : undefined!;
    out.alphaMode = from
        ? (readWGPUCompositeAlphaMode(from!, offset + 40) as any)
        : undefined!;
    out.width = from ? (readuint32_t(from!, offset + 44) as any) : undefined!;
    out.height = from ? (readuint32_t(from!, offset + 48) as any) : undefined!;
    out.presentMode = from
        ? (readWGPUPresentMode(from!, offset + 52) as any)
        : undefined!;
    return out;
}
export function readWGPUSurfaceDescriptor(
    from: Pointer,
    offset: number
): WGPUSurfaceDescriptor {
    const out: WGPUSurfaceDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    return out;
}
export function readWGPUSurfaceDescriptorFromAndroidNativeWindow(
    from: Pointer,
    offset: number
): WGPUSurfaceDescriptorFromAndroidNativeWindow {
    const out: WGPUSurfaceDescriptorFromAndroidNativeWindow = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.window = from
        ? (readPtrT<void>(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPUSurfaceDescriptorFromCanvasHTMLSelector(
    from: Pointer,
    offset: number
): WGPUSurfaceDescriptorFromCanvasHTMLSelector {
    const out: WGPUSurfaceDescriptorFromCanvasHTMLSelector = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.selector = from ? (readCString(from!, offset + 16) as any) : undefined!;
    return out;
}
export function readWGPUSurfaceDescriptorFromMetalLayer(
    from: Pointer,
    offset: number
): WGPUSurfaceDescriptorFromMetalLayer {
    const out: WGPUSurfaceDescriptorFromMetalLayer = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.layer = from ? (readPtrT<void>(from!, offset + 16) as any) : undefined!;
    return out;
}
export function readWGPUSurfaceDescriptorFromWaylandSurface(
    from: Pointer,
    offset: number
): WGPUSurfaceDescriptorFromWaylandSurface {
    const out: WGPUSurfaceDescriptorFromWaylandSurface = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.display = from
        ? (readPtrT<void>(from!, offset + 16) as any)
        : undefined!;
    out.surface = from
        ? (readPtrT<void>(from!, offset + 24) as any)
        : undefined!;
    return out;
}
export function readWGPUSurfaceDescriptorFromWindowsHWND(
    from: Pointer,
    offset: number
): WGPUSurfaceDescriptorFromWindowsHWND {
    const out: WGPUSurfaceDescriptorFromWindowsHWND = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.hinstance = from
        ? (readPtrT<void>(from!, offset + 16) as any)
        : undefined!;
    out.hwnd = from ? (readPtrT<void>(from!, offset + 24) as any) : undefined!;
    return out;
}
export function readWGPUSurfaceDescriptorFromXcbWindow(
    from: Pointer,
    offset: number
): WGPUSurfaceDescriptorFromXcbWindow {
    const out: WGPUSurfaceDescriptorFromXcbWindow = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.connection = from
        ? (readPtrT<void>(from!, offset + 16) as any)
        : undefined!;
    out.window = from ? (readuint32_t(from!, offset + 24) as any) : undefined!;
    return out;
}
export function readWGPUSurfaceDescriptorFromXlibWindow(
    from: Pointer,
    offset: number
): WGPUSurfaceDescriptorFromXlibWindow {
    const out: WGPUSurfaceDescriptorFromXlibWindow = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.display = from
        ? (readPtrT<void>(from!, offset + 16) as any)
        : undefined!;
    out.window = from ? (readuint64_t(from!, offset + 24) as any) : undefined!;
    return out;
}
export function readWGPUSurfaceTexture(
    from: Pointer,
    offset: number
): WGPUSurfaceTexture {
    const out: WGPUSurfaceTexture = {} as any;
    out.texture = from
        ? (readWGPUTexture(from!, offset + 0) as any)
        : undefined!;
    out.suboptimal = from
        ? (readWGPUBool(from!, offset + 8) as any)
        : undefined!;
    out.status = from
        ? (readWGPUSurfaceGetCurrentTextureStatus(from!, offset + 12) as any)
        : undefined!;
    return out;
}
export function readWGPUTextureBindingLayout(
    from: Pointer,
    offset: number
): WGPUTextureBindingLayout {
    const out: WGPUTextureBindingLayout = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.sampleType = from
        ? (readWGPUTextureSampleType(from!, offset + 8) as any)
        : undefined!;
    out.viewDimension = from
        ? (readWGPUTextureViewDimension(from!, offset + 12) as any)
        : undefined!;
    out.multisampled = from
        ? (readWGPUBool(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPUTextureDataLayout(
    from: Pointer,
    offset: number
): WGPUTextureDataLayout {
    const out: WGPUTextureDataLayout = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.offset = from ? (readuint64_t(from!, offset + 8) as any) : undefined!;
    out.bytesPerRow = from
        ? (readuint32_t(from!, offset + 16) as any)
        : undefined!;
    out.rowsPerImage = from
        ? (readuint32_t(from!, offset + 20) as any)
        : undefined!;
    return out;
}
export function readWGPUTextureViewDescriptor(
    from: Pointer,
    offset: number
): WGPUTextureViewDescriptor {
    const out: WGPUTextureViewDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.format = from
        ? (readWGPUTextureFormat(from!, offset + 16) as any)
        : undefined!;
    out.dimension = from
        ? (readWGPUTextureViewDimension(from!, offset + 20) as any)
        : undefined!;
    out.baseMipLevel = from
        ? (readuint32_t(from!, offset + 24) as any)
        : undefined!;
    out.mipLevelCount = from
        ? (readuint32_t(from!, offset + 28) as any)
        : undefined!;
    out.baseArrayLayer = from
        ? (readuint32_t(from!, offset + 32) as any)
        : undefined!;
    out.arrayLayerCount = from
        ? (readuint32_t(from!, offset + 36) as any)
        : undefined!;
    out.aspect = from
        ? (readWGPUTextureAspect(from!, offset + 40) as any)
        : undefined!;
    return out;
}
export function readWGPUVertexAttribute(
    from: Pointer,
    offset: number
): WGPUVertexAttribute {
    const out: WGPUVertexAttribute = {} as any;
    out.format = from
        ? (readWGPUVertexFormat(from!, offset + 0) as any)
        : undefined!;
    out.offset = from ? (readuint64_t(from!, offset + 8) as any) : undefined!;
    out.shaderLocation = from
        ? (readuint32_t(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPUBindGroupDescriptor(
    from: Pointer,
    offset: number
): WGPUBindGroupDescriptor {
    const out: WGPUBindGroupDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.layout = from
        ? (readWGPUBindGroupLayout(from!, offset + 16) as any)
        : undefined!;
    out.entryCount = from
        ? (readsize_t(from!, offset + 24) as any)
        : undefined!;
    out.entries = from
        ? (readConstPtrT<WGPUBindGroupEntry>(from!, offset + 32) as any)
        : undefined!;
    return out;
}
export function readWGPUBindGroupLayoutEntry(
    from: Pointer,
    offset: number
): WGPUBindGroupLayoutEntry {
    const out: WGPUBindGroupLayoutEntry = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.binding = from ? (readuint32_t(from!, offset + 8) as any) : undefined!;
    out.visibility = from
        ? (readWGPUShaderStageFlags(from!, offset + 12) as any)
        : undefined!;
    out.buffer = from
        ? (readWGPUBufferBindingLayout(from!, offset + 16) as any)
        : undefined!;
    out.sampler = from
        ? (readWGPUSamplerBindingLayout(from!, offset + 40) as any)
        : undefined!;
    out.texture = from
        ? (readWGPUTextureBindingLayout(from!, offset + 56) as any)
        : undefined!;
    out.storageTexture = from
        ? (readWGPUStorageTextureBindingLayout(from!, offset + 80) as any)
        : undefined!;
    return out;
}
export function readWGPUBlendState(
    from: Pointer,
    offset: number
): WGPUBlendState {
    const out: WGPUBlendState = {} as any;
    out.color = from
        ? (readWGPUBlendComponent(from!, offset + 0) as any)
        : undefined!;
    out.alpha = from
        ? (readWGPUBlendComponent(from!, offset + 12) as any)
        : undefined!;
    return out;
}
export function readWGPUCompilationInfo(
    from: Pointer,
    offset: number
): WGPUCompilationInfo {
    const out: WGPUCompilationInfo = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.messageCount = from
        ? (readsize_t(from!, offset + 8) as any)
        : undefined!;
    out.messages = from
        ? (readConstPtrT<WGPUCompilationMessage>(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPUComputePassDescriptor(
    from: Pointer,
    offset: number
): WGPUComputePassDescriptor {
    const out: WGPUComputePassDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.timestampWrites = from
        ? (readConstPtrT<WGPUComputePassTimestampWrites>(
              from!,
              offset + 16
          ) as any)
        : undefined!;
    return out;
}
export function readWGPUDepthStencilState(
    from: Pointer,
    offset: number
): WGPUDepthStencilState {
    const out: WGPUDepthStencilState = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.format = from
        ? (readWGPUTextureFormat(from!, offset + 8) as any)
        : undefined!;
    out.depthWriteEnabled = from
        ? (readWGPUBool(from!, offset + 12) as any)
        : undefined!;
    out.depthCompare = from
        ? (readWGPUCompareFunction(from!, offset + 16) as any)
        : undefined!;
    out.stencilFront = from
        ? (readWGPUStencilFaceState(from!, offset + 20) as any)
        : undefined!;
    out.stencilBack = from
        ? (readWGPUStencilFaceState(from!, offset + 36) as any)
        : undefined!;
    out.stencilReadMask = from
        ? (readuint32_t(from!, offset + 52) as any)
        : undefined!;
    out.stencilWriteMask = from
        ? (readuint32_t(from!, offset + 56) as any)
        : undefined!;
    out.depthBias = from
        ? (readint32_t(from!, offset + 60) as any)
        : undefined!;
    out.depthBiasSlopeScale = from
        ? (readfloat(from!, offset + 64) as any)
        : undefined!;
    out.depthBiasClamp = from
        ? (readfloat(from!, offset + 68) as any)
        : undefined!;
    return out;
}
export function readWGPUImageCopyBuffer(
    from: Pointer,
    offset: number
): WGPUImageCopyBuffer {
    const out: WGPUImageCopyBuffer = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.layout = from
        ? (readWGPUTextureDataLayout(from!, offset + 8) as any)
        : undefined!;
    out.buffer = from
        ? (readWGPUBuffer(from!, offset + 32) as any)
        : undefined!;
    return out;
}
export function readWGPUImageCopyTexture(
    from: Pointer,
    offset: number
): WGPUImageCopyTexture {
    const out: WGPUImageCopyTexture = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.texture = from
        ? (readWGPUTexture(from!, offset + 8) as any)
        : undefined!;
    out.mipLevel = from
        ? (readuint32_t(from!, offset + 16) as any)
        : undefined!;
    out.origin = from
        ? (readWGPUOrigin3D(from!, offset + 20) as any)
        : undefined!;
    out.aspect = from
        ? (readWGPUTextureAspect(from!, offset + 32) as any)
        : undefined!;
    return out;
}
export function readWGPUProgrammableStageDescriptor(
    from: Pointer,
    offset: number
): WGPUProgrammableStageDescriptor {
    const out: WGPUProgrammableStageDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.module = from
        ? (readWGPUShaderModule(from!, offset + 8) as any)
        : undefined!;
    out.entryPoint = from
        ? (readCString(from!, offset + 16) as any)
        : undefined!;
    out.constantCount = from
        ? (readsize_t(from!, offset + 24) as any)
        : undefined!;
    out.constants = from
        ? (readConstPtrT<WGPUConstantEntry>(from!, offset + 32) as any)
        : undefined!;
    return out;
}
export function readWGPURenderPassColorAttachment(
    from: Pointer,
    offset: number
): WGPURenderPassColorAttachment {
    const out: WGPURenderPassColorAttachment = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.view = from
        ? (readWGPUTextureView(from!, offset + 8) as any)
        : undefined!;
    out.resolveTarget = from
        ? (readWGPUTextureView(from!, offset + 16) as any)
        : undefined!;
    out.loadOp = from
        ? (readWGPULoadOp(from!, offset + 24) as any)
        : undefined!;
    out.storeOp = from
        ? (readWGPUStoreOp(from!, offset + 28) as any)
        : undefined!;
    out.clearValue = from
        ? (readWGPUColor(from!, offset + 32) as any)
        : undefined!;
    return out;
}
export function readWGPURequiredLimits(
    from: Pointer,
    offset: number
): WGPURequiredLimits {
    const out: WGPURequiredLimits = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.limits = from ? (readWGPULimits(from!, offset + 8) as any) : undefined!;
    return out;
}
export function readWGPUShaderModuleDescriptor(
    from: Pointer,
    offset: number
): WGPUShaderModuleDescriptor {
    const out: WGPUShaderModuleDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.hintCount = from ? (readsize_t(from!, offset + 16) as any) : undefined!;
    out.hints = from
        ? (readConstPtrT<WGPUShaderModuleCompilationHint>(
              from!,
              offset + 24
          ) as any)
        : undefined!;
    return out;
}
export function readWGPUSupportedLimits(
    from: Pointer,
    offset: number
): WGPUSupportedLimits {
    const out: WGPUSupportedLimits = {} as any;
    out.nextInChain = from
        ? (readPtrT<WGPUChainedStructOut>(from!, offset + 0) as any)
        : undefined!;
    out.limits = from ? (readWGPULimits(from!, offset + 8) as any) : undefined!;
    return out;
}
export function readWGPUTextureDescriptor(
    from: Pointer,
    offset: number
): WGPUTextureDescriptor {
    const out: WGPUTextureDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.usage = from
        ? (readWGPUTextureUsageFlags(from!, offset + 16) as any)
        : undefined!;
    out.dimension = from
        ? (readWGPUTextureDimension(from!, offset + 20) as any)
        : undefined!;
    out.size = from
        ? (readWGPUExtent3D(from!, offset + 24) as any)
        : undefined!;
    out.format = from
        ? (readWGPUTextureFormat(from!, offset + 36) as any)
        : undefined!;
    out.mipLevelCount = from
        ? (readuint32_t(from!, offset + 40) as any)
        : undefined!;
    out.sampleCount = from
        ? (readuint32_t(from!, offset + 44) as any)
        : undefined!;
    out.viewFormatCount = from
        ? (readsize_t(from!, offset + 48) as any)
        : undefined!;
    out.viewFormats = from
        ? (readConstPtrT<WGPUTextureFormat>(from!, offset + 56) as any)
        : undefined!;
    return out;
}
export function readWGPUVertexBufferLayout(
    from: Pointer,
    offset: number
): WGPUVertexBufferLayout {
    const out: WGPUVertexBufferLayout = {} as any;
    out.arrayStride = from
        ? (readuint64_t(from!, offset + 0) as any)
        : undefined!;
    out.stepMode = from
        ? (readWGPUVertexStepMode(from!, offset + 8) as any)
        : undefined!;
    out.attributeCount = from
        ? (readsize_t(from!, offset + 16) as any)
        : undefined!;
    out.attributes = from
        ? (readConstPtrT<WGPUVertexAttribute>(from!, offset + 24) as any)
        : undefined!;
    return out;
}
export function readWGPUBindGroupLayoutDescriptor(
    from: Pointer,
    offset: number
): WGPUBindGroupLayoutDescriptor {
    const out: WGPUBindGroupLayoutDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.entryCount = from
        ? (readsize_t(from!, offset + 16) as any)
        : undefined!;
    out.entries = from
        ? (readConstPtrT<WGPUBindGroupLayoutEntry>(from!, offset + 24) as any)
        : undefined!;
    return out;
}
export function readWGPUColorTargetState(
    from: Pointer,
    offset: number
): WGPUColorTargetState {
    const out: WGPUColorTargetState = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.format = from
        ? (readWGPUTextureFormat(from!, offset + 8) as any)
        : undefined!;
    out.blend = from
        ? (readConstPtrT<WGPUBlendState>(from!, offset + 16) as any)
        : undefined!;
    out.writeMask = from
        ? (readWGPUColorWriteMaskFlags(from!, offset + 24) as any)
        : undefined!;
    return out;
}
export function readWGPUComputePipelineDescriptor(
    from: Pointer,
    offset: number
): WGPUComputePipelineDescriptor {
    const out: WGPUComputePipelineDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.layout = from
        ? (readWGPUPipelineLayout(from!, offset + 16) as any)
        : undefined!;
    out.compute = from
        ? (readWGPUProgrammableStageDescriptor(from!, offset + 24) as any)
        : undefined!;
    return out;
}
export function readWGPUDeviceDescriptor(
    from: Pointer,
    offset: number
): WGPUDeviceDescriptor {
    const out: WGPUDeviceDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.requiredFeatureCount = from
        ? (readsize_t(from!, offset + 16) as any)
        : undefined!;
    out.requiredFeatures = from
        ? (readConstPtrT<WGPUFeatureName>(from!, offset + 24) as any)
        : undefined!;
    out.requiredLimits = from
        ? (readConstPtrT<WGPURequiredLimits>(from!, offset + 32) as any)
        : undefined!;
    out.defaultQueue = from
        ? (readWGPUQueueDescriptor(from!, offset + 40) as any)
        : undefined!;
    out.deviceLostCallback = from
        ? (readWGPUDeviceLostCallback(from!, offset + 56) as any)
        : undefined!;
    out.deviceLostUserdata = from
        ? (readPtrT<void>(from!, offset + 64) as any)
        : undefined!;
    return out;
}
export function readWGPURenderPassDescriptor(
    from: Pointer,
    offset: number
): WGPURenderPassDescriptor {
    const out: WGPURenderPassDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.colorAttachmentCount = from
        ? (readsize_t(from!, offset + 16) as any)
        : undefined!;
    out.colorAttachments = from
        ? (readConstPtrT<WGPURenderPassColorAttachment>(
              from!,
              offset + 24
          ) as any)
        : undefined!;
    out.depthStencilAttachment = from
        ? (readConstPtrT<WGPURenderPassDepthStencilAttachment>(
              from!,
              offset + 32
          ) as any)
        : undefined!;
    out.occlusionQuerySet = from
        ? (readWGPUQuerySet(from!, offset + 40) as any)
        : undefined!;
    out.timestampWrites = from
        ? (readConstPtrT<WGPURenderPassTimestampWrites>(
              from!,
              offset + 48
          ) as any)
        : undefined!;
    return out;
}
export function readWGPUVertexState(
    from: Pointer,
    offset: number
): WGPUVertexState {
    const out: WGPUVertexState = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.module = from
        ? (readWGPUShaderModule(from!, offset + 8) as any)
        : undefined!;
    out.entryPoint = from
        ? (readCString(from!, offset + 16) as any)
        : undefined!;
    out.constantCount = from
        ? (readsize_t(from!, offset + 24) as any)
        : undefined!;
    out.constants = from
        ? (readConstPtrT<WGPUConstantEntry>(from!, offset + 32) as any)
        : undefined!;
    out.bufferCount = from
        ? (readsize_t(from!, offset + 40) as any)
        : undefined!;
    out.buffers = from
        ? (readConstPtrT<WGPUVertexBufferLayout>(from!, offset + 48) as any)
        : undefined!;
    return out;
}
export function readWGPUFragmentState(
    from: Pointer,
    offset: number
): WGPUFragmentState {
    const out: WGPUFragmentState = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.module = from
        ? (readWGPUShaderModule(from!, offset + 8) as any)
        : undefined!;
    out.entryPoint = from
        ? (readCString(from!, offset + 16) as any)
        : undefined!;
    out.constantCount = from
        ? (readsize_t(from!, offset + 24) as any)
        : undefined!;
    out.constants = from
        ? (readConstPtrT<WGPUConstantEntry>(from!, offset + 32) as any)
        : undefined!;
    out.targetCount = from
        ? (readsize_t(from!, offset + 40) as any)
        : undefined!;
    out.targets = from
        ? (readConstPtrT<WGPUColorTargetState>(from!, offset + 48) as any)
        : undefined!;
    return out;
}
export function readWGPURenderPipelineDescriptor(
    from: Pointer,
    offset: number
): WGPURenderPipelineDescriptor {
    const out: WGPURenderPipelineDescriptor = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.label = from ? (readCString(from!, offset + 8) as any) : undefined!;
    out.layout = from
        ? (readWGPUPipelineLayout(from!, offset + 16) as any)
        : undefined!;
    out.vertex = from
        ? (readWGPUVertexState(from!, offset + 24) as any)
        : undefined!;
    out.primitive = from
        ? (readWGPUPrimitiveState(from!, offset + 80) as any)
        : undefined!;
    out.depthStencil = from
        ? (readConstPtrT<WGPUDepthStencilState>(from!, offset + 104) as any)
        : undefined!;
    out.multisample = from
        ? (readWGPUMultisampleState(from!, offset + 112) as any)
        : undefined!;
    out.fragment = from
        ? (readConstPtrT<WGPUFragmentState>(from!, offset + 136) as any)
        : undefined!;
    return out;
}

export function readWGPUProcCreateInstance(
    from: Pointer,
    offset: number
): WGPUProcCreateInstance {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCreateInstance_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCreateInstance;
}

export function readWGPUProcGetProcAddress(
    from: Pointer,
    offset: number
): WGPUProcGetProcAddress {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcGetProcAddress_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcGetProcAddress;
}

export function readWGPUProcAdapterEnumerateFeatures(
    from: Pointer,
    offset: number
): WGPUProcAdapterEnumerateFeatures {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcAdapterEnumerateFeatures_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcAdapterEnumerateFeatures;
}

export function readWGPUProcAdapterGetLimits(
    from: Pointer,
    offset: number
): WGPUProcAdapterGetLimits {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcAdapterGetLimits_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcAdapterGetLimits;
}

export function readWGPUProcAdapterGetProperties(
    from: Pointer,
    offset: number
): WGPUProcAdapterGetProperties {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcAdapterGetProperties_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcAdapterGetProperties;
}

export function readWGPUProcAdapterHasFeature(
    from: Pointer,
    offset: number
): WGPUProcAdapterHasFeature {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcAdapterHasFeature_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcAdapterHasFeature;
}

export function readWGPUProcAdapterRequestDevice(
    from: Pointer,
    offset: number
): WGPUProcAdapterRequestDevice {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcAdapterRequestDevice_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcAdapterRequestDevice;
}

export function readWGPUProcAdapterReference(
    from: Pointer,
    offset: number
): WGPUProcAdapterReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcAdapterReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcAdapterReference;
}

export function readWGPUProcAdapterRelease(
    from: Pointer,
    offset: number
): WGPUProcAdapterRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcAdapterRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcAdapterRelease;
}

export function readWGPUProcBindGroupSetLabel(
    from: Pointer,
    offset: number
): WGPUProcBindGroupSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBindGroupSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBindGroupSetLabel;
}

export function readWGPUProcBindGroupReference(
    from: Pointer,
    offset: number
): WGPUProcBindGroupReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBindGroupReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBindGroupReference;
}

export function readWGPUProcBindGroupRelease(
    from: Pointer,
    offset: number
): WGPUProcBindGroupRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBindGroupRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBindGroupRelease;
}

export function readWGPUProcBindGroupLayoutSetLabel(
    from: Pointer,
    offset: number
): WGPUProcBindGroupLayoutSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBindGroupLayoutSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBindGroupLayoutSetLabel;
}

export function readWGPUProcBindGroupLayoutReference(
    from: Pointer,
    offset: number
): WGPUProcBindGroupLayoutReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBindGroupLayoutReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBindGroupLayoutReference;
}

export function readWGPUProcBindGroupLayoutRelease(
    from: Pointer,
    offset: number
): WGPUProcBindGroupLayoutRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBindGroupLayoutRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBindGroupLayoutRelease;
}

export function readWGPUProcBufferDestroy(
    from: Pointer,
    offset: number
): WGPUProcBufferDestroy {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBufferDestroy_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBufferDestroy;
}

export function readWGPUProcBufferGetConstMappedRange(
    from: Pointer,
    offset: number
): WGPUProcBufferGetConstMappedRange {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBufferGetConstMappedRange_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBufferGetConstMappedRange;
}

export function readWGPUProcBufferGetMapState(
    from: Pointer,
    offset: number
): WGPUProcBufferGetMapState {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBufferGetMapState_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBufferGetMapState;
}

export function readWGPUProcBufferGetMappedRange(
    from: Pointer,
    offset: number
): WGPUProcBufferGetMappedRange {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBufferGetMappedRange_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBufferGetMappedRange;
}

export function readWGPUProcBufferGetSize(
    from: Pointer,
    offset: number
): WGPUProcBufferGetSize {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBufferGetSize_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBufferGetSize;
}

export function readWGPUProcBufferGetUsage(
    from: Pointer,
    offset: number
): WGPUProcBufferGetUsage {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBufferGetUsage_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBufferGetUsage;
}

export function readWGPUProcBufferMapAsync(
    from: Pointer,
    offset: number
): WGPUProcBufferMapAsync {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBufferMapAsync_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBufferMapAsync;
}

export function readWGPUProcBufferSetLabel(
    from: Pointer,
    offset: number
): WGPUProcBufferSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBufferSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBufferSetLabel;
}

export function readWGPUProcBufferUnmap(
    from: Pointer,
    offset: number
): WGPUProcBufferUnmap {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBufferUnmap_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBufferUnmap;
}

export function readWGPUProcBufferReference(
    from: Pointer,
    offset: number
): WGPUProcBufferReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBufferReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBufferReference;
}

export function readWGPUProcBufferRelease(
    from: Pointer,
    offset: number
): WGPUProcBufferRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcBufferRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcBufferRelease;
}

export function readWGPUProcCommandBufferSetLabel(
    from: Pointer,
    offset: number
): WGPUProcCommandBufferSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandBufferSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandBufferSetLabel;
}

export function readWGPUProcCommandBufferReference(
    from: Pointer,
    offset: number
): WGPUProcCommandBufferReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandBufferReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandBufferReference;
}

export function readWGPUProcCommandBufferRelease(
    from: Pointer,
    offset: number
): WGPUProcCommandBufferRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandBufferRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandBufferRelease;
}

export function readWGPUProcCommandEncoderBeginComputePass(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderBeginComputePass {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderBeginComputePass_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderBeginComputePass;
}

export function readWGPUProcCommandEncoderBeginRenderPass(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderBeginRenderPass {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderBeginRenderPass_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderBeginRenderPass;
}

export function readWGPUProcCommandEncoderClearBuffer(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderClearBuffer {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderClearBuffer_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderClearBuffer;
}

export function readWGPUProcCommandEncoderCopyBufferToBuffer(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderCopyBufferToBuffer {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderCopyBufferToBuffer_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderCopyBufferToBuffer;
}

export function readWGPUProcCommandEncoderCopyBufferToTexture(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderCopyBufferToTexture {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderCopyBufferToTexture_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderCopyBufferToTexture;
}

export function readWGPUProcCommandEncoderCopyTextureToBuffer(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderCopyTextureToBuffer {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderCopyTextureToBuffer_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderCopyTextureToBuffer;
}

export function readWGPUProcCommandEncoderCopyTextureToTexture(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderCopyTextureToTexture {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderCopyTextureToTexture_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderCopyTextureToTexture;
}

export function readWGPUProcCommandEncoderFinish(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderFinish {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderFinish_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderFinish;
}

export function readWGPUProcCommandEncoderInsertDebugMarker(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderInsertDebugMarker {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderInsertDebugMarker_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderInsertDebugMarker;
}

export function readWGPUProcCommandEncoderPopDebugGroup(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderPopDebugGroup {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderPopDebugGroup_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderPopDebugGroup;
}

export function readWGPUProcCommandEncoderPushDebugGroup(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderPushDebugGroup {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderPushDebugGroup_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderPushDebugGroup;
}

export function readWGPUProcCommandEncoderResolveQuerySet(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderResolveQuerySet {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderResolveQuerySet_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderResolveQuerySet;
}

export function readWGPUProcCommandEncoderSetLabel(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderSetLabel;
}

export function readWGPUProcCommandEncoderWriteTimestamp(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderWriteTimestamp {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderWriteTimestamp_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderWriteTimestamp;
}

export function readWGPUProcCommandEncoderReference(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderReference;
}

export function readWGPUProcCommandEncoderRelease(
    from: Pointer,
    offset: number
): WGPUProcCommandEncoderRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcCommandEncoderRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcCommandEncoderRelease;
}

export function readWGPUProcComputePassEncoderDispatchWorkgroups(
    from: Pointer,
    offset: number
): WGPUProcComputePassEncoderDispatchWorkgroups {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePassEncoderDispatchWorkgroups_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePassEncoderDispatchWorkgroups;
}

export function readWGPUProcComputePassEncoderDispatchWorkgroupsIndirect(
    from: Pointer,
    offset: number
): WGPUProcComputePassEncoderDispatchWorkgroupsIndirect {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePassEncoderDispatchWorkgroupsIndirect_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePassEncoderDispatchWorkgroupsIndirect;
}

export function readWGPUProcComputePassEncoderEnd(
    from: Pointer,
    offset: number
): WGPUProcComputePassEncoderEnd {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePassEncoderEnd_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePassEncoderEnd;
}

export function readWGPUProcComputePassEncoderInsertDebugMarker(
    from: Pointer,
    offset: number
): WGPUProcComputePassEncoderInsertDebugMarker {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePassEncoderInsertDebugMarker_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePassEncoderInsertDebugMarker;
}

export function readWGPUProcComputePassEncoderPopDebugGroup(
    from: Pointer,
    offset: number
): WGPUProcComputePassEncoderPopDebugGroup {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePassEncoderPopDebugGroup_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePassEncoderPopDebugGroup;
}

export function readWGPUProcComputePassEncoderPushDebugGroup(
    from: Pointer,
    offset: number
): WGPUProcComputePassEncoderPushDebugGroup {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePassEncoderPushDebugGroup_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePassEncoderPushDebugGroup;
}

export function readWGPUProcComputePassEncoderSetBindGroup(
    from: Pointer,
    offset: number
): WGPUProcComputePassEncoderSetBindGroup {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePassEncoderSetBindGroup_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePassEncoderSetBindGroup;
}

export function readWGPUProcComputePassEncoderSetLabel(
    from: Pointer,
    offset: number
): WGPUProcComputePassEncoderSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePassEncoderSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePassEncoderSetLabel;
}

export function readWGPUProcComputePassEncoderSetPipeline(
    from: Pointer,
    offset: number
): WGPUProcComputePassEncoderSetPipeline {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePassEncoderSetPipeline_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePassEncoderSetPipeline;
}

export function readWGPUProcComputePassEncoderReference(
    from: Pointer,
    offset: number
): WGPUProcComputePassEncoderReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePassEncoderReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePassEncoderReference;
}

export function readWGPUProcComputePassEncoderRelease(
    from: Pointer,
    offset: number
): WGPUProcComputePassEncoderRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePassEncoderRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePassEncoderRelease;
}

export function readWGPUProcComputePipelineGetBindGroupLayout(
    from: Pointer,
    offset: number
): WGPUProcComputePipelineGetBindGroupLayout {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePipelineGetBindGroupLayout_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePipelineGetBindGroupLayout;
}

export function readWGPUProcComputePipelineSetLabel(
    from: Pointer,
    offset: number
): WGPUProcComputePipelineSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePipelineSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePipelineSetLabel;
}

export function readWGPUProcComputePipelineReference(
    from: Pointer,
    offset: number
): WGPUProcComputePipelineReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePipelineReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePipelineReference;
}

export function readWGPUProcComputePipelineRelease(
    from: Pointer,
    offset: number
): WGPUProcComputePipelineRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcComputePipelineRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcComputePipelineRelease;
}

export function readWGPUProcDeviceCreateBindGroup(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateBindGroup {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateBindGroup_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateBindGroup;
}

export function readWGPUProcDeviceCreateBindGroupLayout(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateBindGroupLayout {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateBindGroupLayout_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateBindGroupLayout;
}

export function readWGPUProcDeviceCreateBuffer(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateBuffer {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateBuffer_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateBuffer;
}

export function readWGPUProcDeviceCreateCommandEncoder(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateCommandEncoder {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateCommandEncoder_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateCommandEncoder;
}

export function readWGPUProcDeviceCreateComputePipeline(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateComputePipeline {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateComputePipeline_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateComputePipeline;
}

export function readWGPUProcDeviceCreateComputePipelineAsync(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateComputePipelineAsync {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateComputePipelineAsync_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateComputePipelineAsync;
}

export function readWGPUProcDeviceCreatePipelineLayout(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreatePipelineLayout {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreatePipelineLayout_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreatePipelineLayout;
}

export function readWGPUProcDeviceCreateQuerySet(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateQuerySet {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateQuerySet_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateQuerySet;
}

export function readWGPUProcDeviceCreateRenderBundleEncoder(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateRenderBundleEncoder {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateRenderBundleEncoder_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateRenderBundleEncoder;
}

export function readWGPUProcDeviceCreateRenderPipeline(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateRenderPipeline {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateRenderPipeline_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateRenderPipeline;
}

export function readWGPUProcDeviceCreateRenderPipelineAsync(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateRenderPipelineAsync {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateRenderPipelineAsync_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateRenderPipelineAsync;
}

export function readWGPUProcDeviceCreateSampler(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateSampler {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateSampler_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateSampler;
}

export function readWGPUProcDeviceCreateShaderModule(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateShaderModule {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateShaderModule_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateShaderModule;
}

export function readWGPUProcDeviceCreateTexture(
    from: Pointer,
    offset: number
): WGPUProcDeviceCreateTexture {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceCreateTexture_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceCreateTexture;
}

export function readWGPUProcDeviceDestroy(
    from: Pointer,
    offset: number
): WGPUProcDeviceDestroy {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceDestroy_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceDestroy;
}

export function readWGPUProcDeviceEnumerateFeatures(
    from: Pointer,
    offset: number
): WGPUProcDeviceEnumerateFeatures {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceEnumerateFeatures_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceEnumerateFeatures;
}

export function readWGPUProcDeviceGetLimits(
    from: Pointer,
    offset: number
): WGPUProcDeviceGetLimits {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceGetLimits_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceGetLimits;
}

export function readWGPUProcDeviceGetQueue(
    from: Pointer,
    offset: number
): WGPUProcDeviceGetQueue {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceGetQueue_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceGetQueue;
}

export function readWGPUProcDeviceHasFeature(
    from: Pointer,
    offset: number
): WGPUProcDeviceHasFeature {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceHasFeature_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceHasFeature;
}

export function readWGPUProcDevicePopErrorScope(
    from: Pointer,
    offset: number
): WGPUProcDevicePopErrorScope {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDevicePopErrorScope_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDevicePopErrorScope;
}

export function readWGPUProcDevicePushErrorScope(
    from: Pointer,
    offset: number
): WGPUProcDevicePushErrorScope {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDevicePushErrorScope_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDevicePushErrorScope;
}

export function readWGPUProcDeviceSetLabel(
    from: Pointer,
    offset: number
): WGPUProcDeviceSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceSetLabel;
}

export function readWGPUProcDeviceSetUncapturedErrorCallback(
    from: Pointer,
    offset: number
): WGPUProcDeviceSetUncapturedErrorCallback {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceSetUncapturedErrorCallback_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceSetUncapturedErrorCallback;
}

export function readWGPUProcDeviceReference(
    from: Pointer,
    offset: number
): WGPUProcDeviceReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceReference;
}

export function readWGPUProcDeviceRelease(
    from: Pointer,
    offset: number
): WGPUProcDeviceRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcDeviceRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcDeviceRelease;
}

export function readWGPUProcInstanceCreateSurface(
    from: Pointer,
    offset: number
): WGPUProcInstanceCreateSurface {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcInstanceCreateSurface_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcInstanceCreateSurface;
}

export function readWGPUProcInstanceProcessEvents(
    from: Pointer,
    offset: number
): WGPUProcInstanceProcessEvents {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcInstanceProcessEvents_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcInstanceProcessEvents;
}

export function readWGPUProcInstanceRequestAdapter(
    from: Pointer,
    offset: number
): WGPUProcInstanceRequestAdapter {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcInstanceRequestAdapter_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcInstanceRequestAdapter;
}

export function readWGPUProcInstanceReference(
    from: Pointer,
    offset: number
): WGPUProcInstanceReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcInstanceReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcInstanceReference;
}

export function readWGPUProcInstanceRelease(
    from: Pointer,
    offset: number
): WGPUProcInstanceRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcInstanceRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcInstanceRelease;
}

export function readWGPUProcPipelineLayoutSetLabel(
    from: Pointer,
    offset: number
): WGPUProcPipelineLayoutSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcPipelineLayoutSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcPipelineLayoutSetLabel;
}

export function readWGPUProcPipelineLayoutReference(
    from: Pointer,
    offset: number
): WGPUProcPipelineLayoutReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcPipelineLayoutReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcPipelineLayoutReference;
}

export function readWGPUProcPipelineLayoutRelease(
    from: Pointer,
    offset: number
): WGPUProcPipelineLayoutRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcPipelineLayoutRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcPipelineLayoutRelease;
}

export function readWGPUProcQuerySetDestroy(
    from: Pointer,
    offset: number
): WGPUProcQuerySetDestroy {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQuerySetDestroy_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQuerySetDestroy;
}

export function readWGPUProcQuerySetGetCount(
    from: Pointer,
    offset: number
): WGPUProcQuerySetGetCount {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQuerySetGetCount_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQuerySetGetCount;
}

export function readWGPUProcQuerySetGetType(
    from: Pointer,
    offset: number
): WGPUProcQuerySetGetType {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQuerySetGetType_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQuerySetGetType;
}

export function readWGPUProcQuerySetSetLabel(
    from: Pointer,
    offset: number
): WGPUProcQuerySetSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQuerySetSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQuerySetSetLabel;
}

export function readWGPUProcQuerySetReference(
    from: Pointer,
    offset: number
): WGPUProcQuerySetReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQuerySetReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQuerySetReference;
}

export function readWGPUProcQuerySetRelease(
    from: Pointer,
    offset: number
): WGPUProcQuerySetRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQuerySetRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQuerySetRelease;
}

export function readWGPUProcQueueOnSubmittedWorkDone(
    from: Pointer,
    offset: number
): WGPUProcQueueOnSubmittedWorkDone {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQueueOnSubmittedWorkDone_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQueueOnSubmittedWorkDone;
}

export function readWGPUProcQueueSetLabel(
    from: Pointer,
    offset: number
): WGPUProcQueueSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQueueSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQueueSetLabel;
}

export function readWGPUProcQueueSubmit(
    from: Pointer,
    offset: number
): WGPUProcQueueSubmit {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQueueSubmit_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQueueSubmit;
}

export function readWGPUProcQueueWriteBuffer(
    from: Pointer,
    offset: number
): WGPUProcQueueWriteBuffer {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQueueWriteBuffer_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQueueWriteBuffer;
}

export function readWGPUProcQueueWriteTexture(
    from: Pointer,
    offset: number
): WGPUProcQueueWriteTexture {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQueueWriteTexture_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQueueWriteTexture;
}

export function readWGPUProcQueueReference(
    from: Pointer,
    offset: number
): WGPUProcQueueReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQueueReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQueueReference;
}

export function readWGPUProcQueueRelease(
    from: Pointer,
    offset: number
): WGPUProcQueueRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcQueueRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcQueueRelease;
}

export function readWGPUProcRenderBundleSetLabel(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleSetLabel;
}

export function readWGPUProcRenderBundleReference(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleReference;
}

export function readWGPUProcRenderBundleRelease(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleRelease;
}

export function readWGPUProcRenderBundleEncoderDraw(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderDraw {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderDraw_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderDraw;
}

export function readWGPUProcRenderBundleEncoderDrawIndexed(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderDrawIndexed {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderDrawIndexed_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderDrawIndexed;
}

export function readWGPUProcRenderBundleEncoderDrawIndexedIndirect(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderDrawIndexedIndirect {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderDrawIndexedIndirect_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderDrawIndexedIndirect;
}

export function readWGPUProcRenderBundleEncoderDrawIndirect(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderDrawIndirect {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderDrawIndirect_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderDrawIndirect;
}

export function readWGPUProcRenderBundleEncoderFinish(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderFinish {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderFinish_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderFinish;
}

export function readWGPUProcRenderBundleEncoderInsertDebugMarker(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderInsertDebugMarker {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderInsertDebugMarker_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderInsertDebugMarker;
}

export function readWGPUProcRenderBundleEncoderPopDebugGroup(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderPopDebugGroup {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderPopDebugGroup_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderPopDebugGroup;
}

export function readWGPUProcRenderBundleEncoderPushDebugGroup(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderPushDebugGroup {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderPushDebugGroup_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderPushDebugGroup;
}

export function readWGPUProcRenderBundleEncoderSetBindGroup(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderSetBindGroup {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderSetBindGroup_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderSetBindGroup;
}

export function readWGPUProcRenderBundleEncoderSetIndexBuffer(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderSetIndexBuffer {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderSetIndexBuffer_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderSetIndexBuffer;
}

export function readWGPUProcRenderBundleEncoderSetLabel(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderSetLabel;
}

export function readWGPUProcRenderBundleEncoderSetPipeline(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderSetPipeline {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderSetPipeline_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderSetPipeline;
}

export function readWGPUProcRenderBundleEncoderSetVertexBuffer(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderSetVertexBuffer {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderSetVertexBuffer_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderSetVertexBuffer;
}

export function readWGPUProcRenderBundleEncoderReference(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderReference;
}

export function readWGPUProcRenderBundleEncoderRelease(
    from: Pointer,
    offset: number
): WGPUProcRenderBundleEncoderRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderBundleEncoderRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderBundleEncoderRelease;
}

export function readWGPUProcRenderPassEncoderBeginOcclusionQuery(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderBeginOcclusionQuery {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderBeginOcclusionQuery_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderBeginOcclusionQuery;
}

export function readWGPUProcRenderPassEncoderDraw(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderDraw {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderDraw_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderDraw;
}

export function readWGPUProcRenderPassEncoderDrawIndexed(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderDrawIndexed {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderDrawIndexed_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderDrawIndexed;
}

export function readWGPUProcRenderPassEncoderDrawIndexedIndirect(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderDrawIndexedIndirect {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderDrawIndexedIndirect_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderDrawIndexedIndirect;
}

export function readWGPUProcRenderPassEncoderDrawIndirect(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderDrawIndirect {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderDrawIndirect_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderDrawIndirect;
}

export function readWGPUProcRenderPassEncoderEnd(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderEnd {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderEnd_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderEnd;
}

export function readWGPUProcRenderPassEncoderEndOcclusionQuery(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderEndOcclusionQuery {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderEndOcclusionQuery_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderEndOcclusionQuery;
}

export function readWGPUProcRenderPassEncoderExecuteBundles(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderExecuteBundles {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderExecuteBundles_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderExecuteBundles;
}

export function readWGPUProcRenderPassEncoderInsertDebugMarker(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderInsertDebugMarker {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderInsertDebugMarker_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderInsertDebugMarker;
}

export function readWGPUProcRenderPassEncoderPopDebugGroup(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderPopDebugGroup {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderPopDebugGroup_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderPopDebugGroup;
}

export function readWGPUProcRenderPassEncoderPushDebugGroup(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderPushDebugGroup {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderPushDebugGroup_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderPushDebugGroup;
}

export function readWGPUProcRenderPassEncoderSetBindGroup(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderSetBindGroup {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderSetBindGroup_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderSetBindGroup;
}

export function readWGPUProcRenderPassEncoderSetBlendConstant(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderSetBlendConstant {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderSetBlendConstant_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderSetBlendConstant;
}

export function readWGPUProcRenderPassEncoderSetIndexBuffer(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderSetIndexBuffer {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderSetIndexBuffer_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderSetIndexBuffer;
}

export function readWGPUProcRenderPassEncoderSetLabel(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderSetLabel;
}

export function readWGPUProcRenderPassEncoderSetPipeline(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderSetPipeline {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderSetPipeline_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderSetPipeline;
}

export function readWGPUProcRenderPassEncoderSetScissorRect(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderSetScissorRect {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderSetScissorRect_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderSetScissorRect;
}

export function readWGPUProcRenderPassEncoderSetStencilReference(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderSetStencilReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderSetStencilReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderSetStencilReference;
}

export function readWGPUProcRenderPassEncoderSetVertexBuffer(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderSetVertexBuffer {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderSetVertexBuffer_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderSetVertexBuffer;
}

export function readWGPUProcRenderPassEncoderSetViewport(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderSetViewport {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderSetViewport_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderSetViewport;
}

export function readWGPUProcRenderPassEncoderReference(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderReference;
}

export function readWGPUProcRenderPassEncoderRelease(
    from: Pointer,
    offset: number
): WGPUProcRenderPassEncoderRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPassEncoderRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPassEncoderRelease;
}

export function readWGPUProcRenderPipelineGetBindGroupLayout(
    from: Pointer,
    offset: number
): WGPUProcRenderPipelineGetBindGroupLayout {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPipelineGetBindGroupLayout_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPipelineGetBindGroupLayout;
}

export function readWGPUProcRenderPipelineSetLabel(
    from: Pointer,
    offset: number
): WGPUProcRenderPipelineSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPipelineSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPipelineSetLabel;
}

export function readWGPUProcRenderPipelineReference(
    from: Pointer,
    offset: number
): WGPUProcRenderPipelineReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPipelineReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPipelineReference;
}

export function readWGPUProcRenderPipelineRelease(
    from: Pointer,
    offset: number
): WGPUProcRenderPipelineRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcRenderPipelineRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcRenderPipelineRelease;
}

export function readWGPUProcSamplerSetLabel(
    from: Pointer,
    offset: number
): WGPUProcSamplerSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcSamplerSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcSamplerSetLabel;
}

export function readWGPUProcSamplerReference(
    from: Pointer,
    offset: number
): WGPUProcSamplerReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcSamplerReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcSamplerReference;
}

export function readWGPUProcSamplerRelease(
    from: Pointer,
    offset: number
): WGPUProcSamplerRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcSamplerRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcSamplerRelease;
}

export function readWGPUProcShaderModuleGetCompilationInfo(
    from: Pointer,
    offset: number
): WGPUProcShaderModuleGetCompilationInfo {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcShaderModuleGetCompilationInfo_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcShaderModuleGetCompilationInfo;
}

export function readWGPUProcShaderModuleSetLabel(
    from: Pointer,
    offset: number
): WGPUProcShaderModuleSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcShaderModuleSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcShaderModuleSetLabel;
}

export function readWGPUProcShaderModuleReference(
    from: Pointer,
    offset: number
): WGPUProcShaderModuleReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcShaderModuleReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcShaderModuleReference;
}

export function readWGPUProcShaderModuleRelease(
    from: Pointer,
    offset: number
): WGPUProcShaderModuleRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcShaderModuleRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcShaderModuleRelease;
}

export function readWGPUProcSurfaceConfigure(
    from: Pointer,
    offset: number
): WGPUProcSurfaceConfigure {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcSurfaceConfigure_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcSurfaceConfigure;
}

export function readWGPUProcSurfaceGetCapabilities(
    from: Pointer,
    offset: number
): WGPUProcSurfaceGetCapabilities {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcSurfaceGetCapabilities_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcSurfaceGetCapabilities;
}

export function readWGPUProcSurfaceGetCurrentTexture(
    from: Pointer,
    offset: number
): WGPUProcSurfaceGetCurrentTexture {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcSurfaceGetCurrentTexture_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcSurfaceGetCurrentTexture;
}

export function readWGPUProcSurfaceGetPreferredFormat(
    from: Pointer,
    offset: number
): WGPUProcSurfaceGetPreferredFormat {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcSurfaceGetPreferredFormat_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcSurfaceGetPreferredFormat;
}

export function readWGPUProcSurfacePresent(
    from: Pointer,
    offset: number
): WGPUProcSurfacePresent {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcSurfacePresent_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcSurfacePresent;
}

export function readWGPUProcSurfaceUnconfigure(
    from: Pointer,
    offset: number
): WGPUProcSurfaceUnconfigure {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcSurfaceUnconfigure_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcSurfaceUnconfigure;
}

export function readWGPUProcSurfaceReference(
    from: Pointer,
    offset: number
): WGPUProcSurfaceReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcSurfaceReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcSurfaceReference;
}

export function readWGPUProcSurfaceRelease(
    from: Pointer,
    offset: number
): WGPUProcSurfaceRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcSurfaceRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcSurfaceRelease;
}

export function readWGPUProcSurfaceCapabilitiesFreeMembers(
    from: Pointer,
    offset: number
): WGPUProcSurfaceCapabilitiesFreeMembers {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcSurfaceCapabilitiesFreeMembers_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcSurfaceCapabilitiesFreeMembers;
}

export function readWGPUProcTextureCreateView(
    from: Pointer,
    offset: number
): WGPUProcTextureCreateView {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureCreateView_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureCreateView;
}

export function readWGPUProcTextureDestroy(
    from: Pointer,
    offset: number
): WGPUProcTextureDestroy {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureDestroy_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureDestroy;
}

export function readWGPUProcTextureGetDepthOrArrayLayers(
    from: Pointer,
    offset: number
): WGPUProcTextureGetDepthOrArrayLayers {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureGetDepthOrArrayLayers_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureGetDepthOrArrayLayers;
}

export function readWGPUProcTextureGetDimension(
    from: Pointer,
    offset: number
): WGPUProcTextureGetDimension {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureGetDimension_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureGetDimension;
}

export function readWGPUProcTextureGetFormat(
    from: Pointer,
    offset: number
): WGPUProcTextureGetFormat {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureGetFormat_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureGetFormat;
}

export function readWGPUProcTextureGetHeight(
    from: Pointer,
    offset: number
): WGPUProcTextureGetHeight {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureGetHeight_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureGetHeight;
}

export function readWGPUProcTextureGetMipLevelCount(
    from: Pointer,
    offset: number
): WGPUProcTextureGetMipLevelCount {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureGetMipLevelCount_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureGetMipLevelCount;
}

export function readWGPUProcTextureGetSampleCount(
    from: Pointer,
    offset: number
): WGPUProcTextureGetSampleCount {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureGetSampleCount_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureGetSampleCount;
}

export function readWGPUProcTextureGetUsage(
    from: Pointer,
    offset: number
): WGPUProcTextureGetUsage {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureGetUsage_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureGetUsage;
}

export function readWGPUProcTextureGetWidth(
    from: Pointer,
    offset: number
): WGPUProcTextureGetWidth {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureGetWidth_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureGetWidth;
}

export function readWGPUProcTextureSetLabel(
    from: Pointer,
    offset: number
): WGPUProcTextureSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureSetLabel;
}

export function readWGPUProcTextureReference(
    from: Pointer,
    offset: number
): WGPUProcTextureReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureReference;
}

export function readWGPUProcTextureRelease(
    from: Pointer,
    offset: number
): WGPUProcTextureRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureRelease;
}

export function readWGPUProcTextureViewSetLabel(
    from: Pointer,
    offset: number
): WGPUProcTextureViewSetLabel {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureViewSetLabel_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureViewSetLabel;
}

export function readWGPUProcTextureViewReference(
    from: Pointer,
    offset: number
): WGPUProcTextureViewReference {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureViewReference_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureViewReference;
}

export function readWGPUProcTextureViewRelease(
    from: Pointer,
    offset: number
): WGPUProcTextureViewRelease {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPUProcTextureViewRelease_funcdef"),
        ptr: p,
    });
    return cb as any as WGPUProcTextureViewRelease;
}
export function readWGPUNativeSType(
    from: Pointer,
    offset: number
): WGPUNativeSType {
    return bunRead.i32(from!, offset) as any as WGPUNativeSType;
}
export function readWGPUNativeFeature(
    from: Pointer,
    offset: number
): WGPUNativeFeature {
    return bunRead.i32(from!, offset) as any as WGPUNativeFeature;
}
export function readWGPULogLevel(from: Pointer, offset: number): WGPULogLevel {
    return bunRead.i32(from!, offset) as any as WGPULogLevel;
}
export function readWGPUInstanceBackend(
    from: Pointer,
    offset: number
): WGPUInstanceBackend {
    return bunRead.i32(from!, offset) as any as WGPUInstanceBackend;
}
export const readWGPUInstanceBackendFlags = readWGPUFlags;
export function readWGPUInstanceFlag(
    from: Pointer,
    offset: number
): WGPUInstanceFlag {
    return bunRead.i32(from!, offset) as any as WGPUInstanceFlag;
}
export const readWGPUInstanceFlags = readWGPUFlags;
export function readWGPUDx12Compiler(
    from: Pointer,
    offset: number
): WGPUDx12Compiler {
    return bunRead.i32(from!, offset) as any as WGPUDx12Compiler;
}
export function readWGPUGles3MinorVersion(
    from: Pointer,
    offset: number
): WGPUGles3MinorVersion {
    return bunRead.i32(from!, offset) as any as WGPUGles3MinorVersion;
}
export function readWGPUPipelineStatisticName(
    from: Pointer,
    offset: number
): WGPUPipelineStatisticName {
    return bunRead.i32(from!, offset) as any as WGPUPipelineStatisticName;
}
export function readWGPUNativeQueryType(
    from: Pointer,
    offset: number
): WGPUNativeQueryType {
    return bunRead.i32(from!, offset) as any as WGPUNativeQueryType;
}
export function readWGPUInstanceExtras(
    from: Pointer,
    offset: number
): WGPUInstanceExtras {
    const out: WGPUInstanceExtras = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.backends = from
        ? (readWGPUInstanceBackendFlags(from!, offset + 16) as any)
        : undefined!;
    out.flags = from
        ? (readWGPUInstanceFlags(from!, offset + 20) as any)
        : undefined!;
    out.dx12ShaderCompiler = from
        ? (readWGPUDx12Compiler(from!, offset + 24) as any)
        : undefined!;
    out.gles3MinorVersion = from
        ? (readWGPUGles3MinorVersion(from!, offset + 28) as any)
        : undefined!;
    out.dxilPath = from ? (readCString(from!, offset + 32) as any) : undefined!;
    out.dxcPath = from ? (readCString(from!, offset + 40) as any) : undefined!;
    return out;
}
export function readWGPUDeviceExtras(
    from: Pointer,
    offset: number
): WGPUDeviceExtras {
    const out: WGPUDeviceExtras = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.tracePath = from
        ? (readCString(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPUNativeLimits(
    from: Pointer,
    offset: number
): WGPUNativeLimits {
    const out: WGPUNativeLimits = {} as any;
    out.maxPushConstantSize = from
        ? (readuint32_t(from!, offset + 0) as any)
        : undefined!;
    out.maxNonSamplerBindings = from
        ? (readuint32_t(from!, offset + 4) as any)
        : undefined!;
    return out;
}
export function readWGPURequiredLimitsExtras(
    from: Pointer,
    offset: number
): WGPURequiredLimitsExtras {
    const out: WGPURequiredLimitsExtras = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.limits = from
        ? (readWGPUNativeLimits(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPUSupportedLimitsExtras(
    from: Pointer,
    offset: number
): WGPUSupportedLimitsExtras {
    const out: WGPUSupportedLimitsExtras = {} as any;
    out.chain = from
        ? (readWGPUChainedStructOut(from!, offset + 0) as any)
        : undefined!;
    out.limits = from
        ? (readWGPUNativeLimits(from!, offset + 16) as any)
        : undefined!;
    return out;
}
export function readWGPUPushConstantRange(
    from: Pointer,
    offset: number
): WGPUPushConstantRange {
    const out: WGPUPushConstantRange = {} as any;
    out.stages = from
        ? (readWGPUShaderStageFlags(from!, offset + 0) as any)
        : undefined!;
    out.start = from ? (readuint32_t(from!, offset + 4) as any) : undefined!;
    out.end = from ? (readuint32_t(from!, offset + 8) as any) : undefined!;
    return out;
}
export function readWGPUPipelineLayoutExtras(
    from: Pointer,
    offset: number
): WGPUPipelineLayoutExtras {
    const out: WGPUPipelineLayoutExtras = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.pushConstantRangeCount = from
        ? (readsize_t(from!, offset + 16) as any)
        : undefined!;
    out.pushConstantRanges = from
        ? (readConstPtrT<WGPUPushConstantRange>(from!, offset + 24) as any)
        : undefined!;
    return out;
}
export const readWGPUSubmissionIndex = readuint64_t;
export function readWGPUWrappedSubmissionIndex(
    from: Pointer,
    offset: number
): WGPUWrappedSubmissionIndex {
    const out: WGPUWrappedSubmissionIndex = {} as any;
    out.queue = from ? (readWGPUQueue(from!, offset + 0) as any) : undefined!;
    out.submissionIndex = from
        ? (readWGPUSubmissionIndex(from!, offset + 8) as any)
        : undefined!;
    return out;
}
export function readWGPUShaderDefine(
    from: Pointer,
    offset: number
): WGPUShaderDefine {
    const out: WGPUShaderDefine = {} as any;
    out.name = from ? (readCString(from!, offset + 0) as any) : undefined!;
    out.value = from ? (readCString(from!, offset + 8) as any) : undefined!;
    return out;
}
export function readWGPUShaderModuleGLSLDescriptor(
    from: Pointer,
    offset: number
): WGPUShaderModuleGLSLDescriptor {
    const out: WGPUShaderModuleGLSLDescriptor = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.stage = from
        ? (readWGPUShaderStage(from!, offset + 16) as any)
        : undefined!;
    out.code = from ? (readCString(from!, offset + 24) as any) : undefined!;
    out.defineCount = from
        ? (readuint32_t(from!, offset + 32) as any)
        : undefined!;
    out.defines = from
        ? (readPtrT<WGPUShaderDefine>(from!, offset + 40) as any)
        : undefined!;
    return out;
}
export function readWGPURegistryReport(
    from: Pointer,
    offset: number
): WGPURegistryReport {
    const out: WGPURegistryReport = {} as any;
    out.numAllocated = from
        ? (readsize_t(from!, offset + 0) as any)
        : undefined!;
    out.numKeptFromUser = from
        ? (readsize_t(from!, offset + 8) as any)
        : undefined!;
    out.numReleasedFromUser = from
        ? (readsize_t(from!, offset + 16) as any)
        : undefined!;
    out.numError = from ? (readsize_t(from!, offset + 24) as any) : undefined!;
    out.elementSize = from
        ? (readsize_t(from!, offset + 32) as any)
        : undefined!;
    return out;
}
export function readWGPUHubReport(
    from: Pointer,
    offset: number
): WGPUHubReport {
    const out: WGPUHubReport = {} as any;
    out.adapters = from
        ? (readWGPURegistryReport(from!, offset + 0) as any)
        : undefined!;
    out.devices = from
        ? (readWGPURegistryReport(from!, offset + 40) as any)
        : undefined!;
    out.queues = from
        ? (readWGPURegistryReport(from!, offset + 80) as any)
        : undefined!;
    out.pipelineLayouts = from
        ? (readWGPURegistryReport(from!, offset + 120) as any)
        : undefined!;
    out.shaderModules = from
        ? (readWGPURegistryReport(from!, offset + 160) as any)
        : undefined!;
    out.bindGroupLayouts = from
        ? (readWGPURegistryReport(from!, offset + 200) as any)
        : undefined!;
    out.bindGroups = from
        ? (readWGPURegistryReport(from!, offset + 240) as any)
        : undefined!;
    out.commandBuffers = from
        ? (readWGPURegistryReport(from!, offset + 280) as any)
        : undefined!;
    out.renderBundles = from
        ? (readWGPURegistryReport(from!, offset + 320) as any)
        : undefined!;
    out.renderPipelines = from
        ? (readWGPURegistryReport(from!, offset + 360) as any)
        : undefined!;
    out.computePipelines = from
        ? (readWGPURegistryReport(from!, offset + 400) as any)
        : undefined!;
    out.querySets = from
        ? (readWGPURegistryReport(from!, offset + 440) as any)
        : undefined!;
    out.buffers = from
        ? (readWGPURegistryReport(from!, offset + 480) as any)
        : undefined!;
    out.textures = from
        ? (readWGPURegistryReport(from!, offset + 520) as any)
        : undefined!;
    out.textureViews = from
        ? (readWGPURegistryReport(from!, offset + 560) as any)
        : undefined!;
    out.samplers = from
        ? (readWGPURegistryReport(from!, offset + 600) as any)
        : undefined!;
    return out;
}
export function readWGPUGlobalReport(
    from: Pointer,
    offset: number
): WGPUGlobalReport {
    const out: WGPUGlobalReport = {} as any;
    out.surfaces = from
        ? (readWGPURegistryReport(from!, offset + 0) as any)
        : undefined!;
    out.backendType = from
        ? (readWGPUBackendType(from!, offset + 40) as any)
        : undefined!;
    out.vulkan = from
        ? (readWGPUHubReport(from!, offset + 48) as any)
        : undefined!;
    out.metal = from
        ? (readWGPUHubReport(from!, offset + 688) as any)
        : undefined!;
    out.dx12 = from
        ? (readWGPUHubReport(from!, offset + 1328) as any)
        : undefined!;
    out.gl = from
        ? (readWGPUHubReport(from!, offset + 1968) as any)
        : undefined!;
    return out;
}
export function readWGPUInstanceEnumerateAdapterOptions(
    from: Pointer,
    offset: number
): WGPUInstanceEnumerateAdapterOptions {
    const out: WGPUInstanceEnumerateAdapterOptions = {} as any;
    out.nextInChain = from
        ? (readConstPtrT<WGPUChainedStruct>(from!, offset + 0) as any)
        : undefined!;
    out.backends = from
        ? (readWGPUInstanceBackendFlags(from!, offset + 8) as any)
        : undefined!;
    return out;
}
export function readWGPUBindGroupEntryExtras(
    from: Pointer,
    offset: number
): WGPUBindGroupEntryExtras {
    const out: WGPUBindGroupEntryExtras = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.buffers = from
        ? (readConstPtrT<WGPUBuffer>(from!, offset + 16) as any)
        : undefined!;
    out.bufferCount = from
        ? (readsize_t(from!, offset + 24) as any)
        : undefined!;
    out.samplers = from
        ? (readConstPtrT<WGPUSampler>(from!, offset + 32) as any)
        : undefined!;
    out.samplerCount = from
        ? (readsize_t(from!, offset + 40) as any)
        : undefined!;
    out.textureViews = from
        ? (readConstPtrT<WGPUTextureView>(from!, offset + 48) as any)
        : undefined!;
    out.textureViewCount = from
        ? (readsize_t(from!, offset + 56) as any)
        : undefined!;
    return out;
}
export function readWGPUBindGroupLayoutEntryExtras(
    from: Pointer,
    offset: number
): WGPUBindGroupLayoutEntryExtras {
    const out: WGPUBindGroupLayoutEntryExtras = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.count = from ? (readuint32_t(from!, offset + 16) as any) : undefined!;
    return out;
}
export function readWGPUQuerySetDescriptorExtras(
    from: Pointer,
    offset: number
): WGPUQuerySetDescriptorExtras {
    const out: WGPUQuerySetDescriptorExtras = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.pipelineStatistics = from
        ? (readConstPtrT<WGPUPipelineStatisticName>(from!, offset + 16) as any)
        : undefined!;
    out.pipelineStatisticCount = from
        ? (readsize_t(from!, offset + 24) as any)
        : undefined!;
    return out;
}
export function readWGPUSurfaceConfigurationExtras(
    from: Pointer,
    offset: number
): WGPUSurfaceConfigurationExtras {
    const out: WGPUSurfaceConfigurationExtras = {} as any;
    out.chain = from
        ? (readWGPUChainedStruct(from!, offset + 0) as any)
        : undefined!;
    out.desiredMaximumFrameLatency = from
        ? (readWGPUBool(from!, offset + 16) as any)
        : undefined!;
    return out;
}

export function readWGPULogCallback(
    from: Pointer,
    offset: number
): WGPULogCallback {
    const p = bunRead.ptr(from!, offset);
    const cb = BunCFunction({
        ...eval("WGPULogCallback_funcdef"),
        ptr: p,
    });
    return cb as any as WGPULogCallback;
}
