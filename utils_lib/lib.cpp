#include "../deps/wgpu/wgpu.h"
#include "../deps/glfw3webgpu/glfw3webgpu.h"

#include <ostream>
#include <stdexcept>
#include <string>
#include <iostream>

#if defined(_WIN32)
#define FORCE_EXPORT __declspec(dllexport)
#else  // defined(_WIN32)
#define FORCE_EXPORT __attribute__((visibility("default")))
#endif  // defined(_WIN32)

extern "C" {

WGPU_EXPORT WGPUSurface glfwGetWGPUSurface(WGPUInstance instance, GLFWwindow* window);

void __ebal_export_cpp() {
    glfwGetWGPUSurface(0, 0);
}

bool catchException(void (*cb)()) {
    try {
        cb();
    } catch (std::exception err) {
        std::cout << "exception " << err.what() << std::endl;
        std::flush(std::cout);
        return false;
    } catch (const char* err) {
        std::cout << "exception " << err << std::endl;
        std::flush(std::cout);
        return false;
    }
    return true;
}

WGPUInstance wgpuRequestInstance(WGPUInstanceDescriptor* desc) {
    WGPUInstanceDescriptor desc_ = {};
    if (!desc) desc = &desc_;
    return wgpuCreateInstance(desc);
}

WGPUAdapter wgpuRequestAdapter(WGPUInstance instance, WGPURequestAdapterOptions const * options) {
    // A simple structure holding the local information shared with the
    // onAdapterRequestEnded callback.
    struct UserData {
        WGPUAdapter adapter = nullptr;
        bool requestEnded = false;
    };
    UserData userData;

    WGPURequestAdapterOptions opts_ = {};
    if (!options) {
        options = &opts_;
    }

    auto onAdapterRequestEnded = [](WGPURequestAdapterStatus status, WGPUAdapter adapter, char const * message, void * pUserData) {
        UserData& userData = *reinterpret_cast<UserData*>(pUserData);
        if (status == WGPURequestAdapterStatus_Success) {
            userData.adapter = adapter;
        } else {
            throw std::runtime_error(std::string("Could not get WebGPU adapter: ") + message);
        }
        userData.requestEnded = true;
    };

    wgpuInstanceRequestAdapter(
        instance,
        options,
        onAdapterRequestEnded,
        (void*)&userData
    );

    return userData.adapter;
}

WGPUAdapter wgpuRequestAdapter_window(WGPUInstance instance, GLFWwindow* window, WGPUSurface compatibleSurface) {
    WGPURequestAdapterOptions opts = {};
    opts.compatibleSurface = compatibleSurface;
    return wgpuRequestAdapter(instance, &opts);
}

WGPUDevice wgpuRequestDevice(WGPUAdapter adapter, WGPUDeviceDescriptor const * descriptor) {
    struct UserData {
        WGPUDevice device = nullptr;
        bool requestEnded = false;
    };
    UserData userData;

    WGPUDeviceDescriptor desc_ = {};
    if (!descriptor) descriptor = &desc_;

    auto onDeviceRequestEnded = [](WGPURequestDeviceStatus status, WGPUDevice device, char const * message, void * pUserData) {
        UserData& userData = *reinterpret_cast<UserData*>(pUserData);
        if (status == WGPURequestDeviceStatus_Success) {
            userData.device = device;
        } else {
            throw std::runtime_error(std::string("Could not get WebGPU device: ") + message);
        }
        userData.requestEnded = true;
    };

    wgpuAdapterRequestDevice(
        adapter,
        descriptor,
        onDeviceRequestEnded,
        (void*)&userData
    );

    assert(userData.requestEnded);

    return userData.device;
}

}
