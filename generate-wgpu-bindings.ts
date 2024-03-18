import { ClangTypeInfoCache, clangClean, clangGetAstJson } from "bun-ffi-gen";
import { CodeGen } from "bun-ffi-gen";
import { parseClangAst } from "bun-ffi-gen";

const HEADER_PATH = "./deps/wgpu/wgpu.h";

const wgpuAst = clangGetAstJson(HEADER_PATH);
const clangTypeInfoCache = await ClangTypeInfoCache.create("generate-wgpu-bindings_cache");

const result = parseClangAst(wgpuAst, HEADER_PATH, clangTypeInfoCache);
const codeGen = new CodeGen({
    funcSymbolsImportLibPathCode(out) {
        out.push(`
            let _LIB_PATH: string = "";

            if (process.platform == "darwin") {
                _LIB_PATH =
                    import.meta.dir +
                    "/../deps/wgpu/libwgpu_native.dylib";
            } else {
                throw new Error("not supported wgpu bindings platform");
            }
        `);

        return "_LIB_PATH";
    },
});

await clangTypeInfoCache.save();

debugger;

codeGen.generateAll(result);

if (codeGen.failedSymbols.size) {
    console.log("everything done, but have problems with this symbols\n(probably unsupported ffi types like structs in args or returnType)");
    console.log(Array.from(codeGen.failedSymbols));
}

codeGen.writeToFile("./src/wgpu.ts");
await clangTypeInfoCache.save();
clangClean();
