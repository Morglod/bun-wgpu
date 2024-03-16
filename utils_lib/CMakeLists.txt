set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_library(utils SHARED lib.cpp)

# target_include_directories(utils PUBLIC .)
target_link_directories(utils PUBLIC ../deps/glfw/lib-universal)
target_link_directories(utils PUBLIC ../deps/glfw3webgpu/build)
target_link_directories(utils PUBLIC ../deps/wgpu)

target_link_libraries(utils PUBLIC wgpu_native glfw.3 glfw3webgpu)

if (APPLE)
  target_link_libraries(utils PRIVATE "-framework Cocoa" "-framework CoreVideo" "-framework IOKit" "-framework QuartzCore")
endif ()
