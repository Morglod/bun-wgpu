import { execSync as cpExecSync } from "child_process";
import { existsSync, mkdirSync } from "fs";
import { rmSync, renameSync as fsRenameSync } from "node:fs";

const CLEAN = true;
const DELETE_DOWNLOADED = false;

console.log(`platform=${process.platform} arch=${process.arch}`);

const execSync: typeof cpExecSync = (command: string, ...args: any): any => {
    console.log(`exec "${command}"`);
    return cpExecSync(command, ...args);
};

async function downloadFile(link: string, dst: string) {
    console.log(`download "${link}" to "${dst}"`);
    if (await Bun.file(dst).exists()) {
        console.log("exists");
        return;
    }
    const result = await fetch(link);
    await Bun.write(dst, result);
}

async function unzip(srcZip: string, dst: string) {
    console.log(`unzip "${srcZip}" to "${dst}"`);
    if (!existsSync(dst)) {
        mkdirSync(dst, { recursive: true });
    }
    execSync(`unzip -oqd "${dst}" "${srcZip}"`);
}

async function deleteFile(path: string) {
    console.log(`delete file "${path}"`);
    rmSync(path);
}

function platformSwitch<T>(mapping: Partial<Record<Platform, (() => T) | T>>): T {
    if (!(process.platform in mapping)) {
        throw new Error(`unsupported platform "${process.platform}"`);
    }

    try {
        if (typeof mapping[process.platform]! === "function") {
            return (mapping[process.platform] as any)();
        }
        return mapping[process.platform] as any;
    } catch (err) {
        throw new Error(`unsupported platform "${process.platform}"`, {
            cause: err,
        });
    }
}

function archSwitch<T>(mapping: Partial<Record<Architecture, (() => T) | T>>) {
    if (!(process.arch in mapping)) {
        throw new Error(`unsupported arch "${process.arch}"`);
    }

    if (typeof mapping[process.arch]! === "function") {
        return (mapping[process.arch] as any)();
    }
    return mapping[process.arch] as any;
}

// --------------------

console.log("install glfw");

if (process.platform === "linux") {
    console.log("installing glfw3 from source for linux");
    console.log("install deps for glfw linux build: https://www.glfw.org/docs/latest/compile.html");

    await downloadFile(`https://github.com/glfw/glfw/releases/download/3.4/glfw-3.4.zip`, `./deps/glfw-3.4.zip`);
    if (CLEAN) {
        rmSync(`./deps/glfw-3.4`, { recursive: true, force: true });
    }
    await unzip(`./deps/glfw-3.4.zip`, "./deps");

    execSync(`cmake -G "Unix Makefiles"`, { cwd: "./deps/glfw-3.4" });
    execSync("make", { cwd: "./deps/glfw-3.4" });
    execSync("make install", { cwd: "./deps/glfw-3.4" });
} else {
    const glfwZipName =
        "glfw-3.4.bin." +
        platformSwitch({
            darwin: "MACOS",
            win32: () =>
                archSwitch({
                    x64: "WIN64",
                }),
        });

    await downloadFile(`https://github.com/glfw/glfw/releases/download/3.4/${glfwZipName}.zip`, `./deps/${glfwZipName}.zip`);
    if (CLEAN) {
        rmSync(`./deps/${glfwZipName}`, { recursive: true, force: true });
    }
    await unzip(`./deps/${glfwZipName}.zip`, "./deps");
    if (existsSync("./deps/glfw")) rmSync("./deps/glfw", { recursive: true, force: true });
    fsRenameSync(`./deps/${glfwZipName}`, "./deps/glfw");
    if (DELETE_DOWNLOADED) await deleteFile(`./deps/${glfwZipName}.zip`);
}

// --------------------

console.log("install wgpu");

// https://github.com/gfx-rs/wgpu-native/releases/download/v0.19.3.1/

const wgpuPlatformNameZip = platformSwitch({
    darwin: () =>
        archSwitch({
            arm64: "macos-aarch64",
            x64: "macos-x86_64",
        }),
    linux: () =>
        archSwitch({
            arm64: "linux-aarch64",
            x64: "linux-x86_64",
        }),
    win32: () =>
        archSwitch({
            x64: "windows-x86_64",
        }),
});

const wgpuZipName = `wgpu-${wgpuPlatformNameZip}-release`;

await downloadFile(`https://github.com/gfx-rs/wgpu-native/releases/download/v0.19.3.1/${wgpuZipName}.zip`, `./deps/${wgpuZipName}.zip`);

if (CLEAN) {
    rmSync(`./deps/${wgpuZipName}`, { recursive: true, force: true });
}
await unzip(`./deps/${wgpuZipName}.zip`, "./deps/wgpu");
if (DELETE_DOWNLOADED) await deleteFile(`./deps/${wgpuZipName}.zip`);

// --------------------

console.log("build glfw3webgpu");
execSync("sh ./build.sh", { cwd: "./deps/glfw3webgpu", stdio: "pipe" });

// --------------------

console.log("build utils_lib");
execSync("sh ./build.sh", { cwd: "./utils_lib", stdio: "pipe" });

// --------------------

console.log("generate wgpu bindings");
execSync("bun generate-wgpu-bindings.ts", { stdio: "pipe" });

// --------------------

console.log("done");
