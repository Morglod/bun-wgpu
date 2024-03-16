import { read } from "bun:ffi";
import { execSync } from "child_process";
import { writeFileSync } from "fs";

const HEADER_PATH = "./deps/wgpu/wgpu.h";

export function getJsonAst(headerPath: string) {
    return JSON.parse(
        execSync(
            "clang -Xclang -ast-dump=json -fsyntax-only " + headerPath
        ).toString()
    ).inner;
}

const wgpuAst = getJsonAst(HEADER_PATH);

const outFile = Bun.file("./src/wgpu.ts");
const out = outFile.writer();

function outWriteLn(str: string = "", padding: number = 0) {
    out.write("    ".repeat(padding) + str + "\n");
}

function compileAndRunReadOut(code: string) {
    execSync("clang -x c - -o ./generate-wgpu-bindings_tmp_exec", {
        input: code,
    });
    return execSync("./generate-wgpu-bindings_tmp_exec").toString();
}

let __getSizeOfCache: Record<string, number> = {};
if (await Bun.file("./generate-wgpu-bindings_sizeofcache.json").exists()) {
    __getSizeOfCache = await Bun.file(
        "./generate-wgpu-bindings_sizeofcache.json"
    ).json();
}

function getSizeOf(headerPath: string, cTypeName: string): number {
    const cacheName = `${headerPath}_qweqwe_${cTypeName}`;
    if (cacheName in __getSizeOfCache) {
        return __getSizeOfCache[cacheName];
    }

    const code = `
        #include "${headerPath}"
        #include <stdio.h>

        int main() {
            printf("[ %lu ]", sizeof(${cTypeName}));
            return 0;
        }
    `;

    const result = JSON.parse(compileAndRunReadOut(code))[0];
    __getSizeOfCache[cacheName] = result;
    return result;
}

let __getOffsetOf: Record<string, number> = {};
if (await Bun.file("./generate-wgpu-bindings_offsetofcache.json").exists()) {
    __getOffsetOf = await Bun.file(
        "./generate-wgpu-bindings_offsetofcache.json"
    ).json();
}

function getOffsetOf(
    headerPath: string,
    cTypeName: string,
    fieldName: string
): number {
    const cacheName = `${headerPath}_qweqwe_${cTypeName}___q123213_${fieldName}`;
    if (cacheName in __getOffsetOf) {
        return __getOffsetOf[cacheName];
    }

    const code = `
        #include "${headerPath}"
        #include <stdio.h>

        int main() {
            printf("[ %lu ]", offsetof(${cTypeName}, ${fieldName}));
            return 0;
        }
    `;

    const result = JSON.parse(compileAndRunReadOut(code))[0];
    __getOffsetOf[cacheName] = result;
    return result;
}

out.write(`
import { FFIType, JSCallback as BunJSCallback, CFunction as BunCFunction, dlopen, CString as BunCString, ptr, Pointer as BunPointer, read as bunRead } from "bun:ffi";

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

type ConstPtrT<T> = (Pointer | TypedArray) & { __type: T, __const_ptr: true };
type PtrT<T> = (Pointer | TypedArray) & { __type: T, __const_ptr: false };

function isPtrOrConstPtr(x: any): x is (Pointer | TypedArray) {
    if (!x) return true;
    return typeof x === "number" || typeof x === "object" && "BYTES_PER_ELEMENT" in x;
}

type TypedArrayPtr<T> = TypedArray & { __type: T, __const_ptr: any };

const void_FFI = FFIType.void;
const size_t_FFI = FFIType.uint64_t;

const int32_t_FFI = FFIType.int32_t;
const uint16_t_FFI = FFIType.uint16_t;
const uint32_t_FFI = FFIType.uint32_t;
const uint64_t_FFI = FFIType.uint64_t;
const double_FFI = FFIType.double;
const float_FFI = FFIType.f32;

export const WGPU_NULL = null as any as (Pointer & { __type: any, __const_ptr: any });

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
    if (typeof x === 'object') {
        if ('BYTES_PER_ELEMENT' in x) x = ptr(x) as any;
        else throw new Error('got unknown object in writePtrT');
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
    throw new Error('called stub writevoid');
}

export function readCString(from: Pointer, offset: number): CString {
    return new BunCString(bunRead.ptr(from!, offset));
}

export function makeCString(str: string) {
    return new BunCString(ptr(Buffer.from(str)));
}

export function readArray<T>(from: Pointer, offset: number, cTypeSize: number, itemReader: any, length: number | bigint): T[] {
    if (from === null) throw new Error('readArray null pointer');
    if (typeof from !== "number") from = ptr(from);

    const out = [] as T[];
    for (let i = 0; i < length; ++i) {
        out.push(itemReader(from!, offset + cTypeSize * i));
    }
    return out;
}

`);

let outLibLines: string[] = [];
let outWritersLines: string[] = [];
let outReadersLines: string[] = [];

outLibLines.push(`
const platform = process.platform;
let path: string = "";

if (platform == "darwin") {
    path =
        import.meta.dir +
        "/../deps/wgpu/libwgpu_native.dylib";
} else {
    throw new Error("not supported wgpu bindings platform");
}

const wgpulib = dlopen(path, {
`);

function outWriteLibLn(str: string = "", padding: number = 0) {
    outLibLines.push("    ".repeat(padding) + str + "\n");
}

function outWriteWriterLn(str: string = "", padding: number = 0) {
    outWritersLines.push("    ".repeat(padding) + str + "\n");
}

function outWriteReaderLn(str: string = "", padding: number = 0) {
    outReadersLines.push("    ".repeat(padding) + str + "\n");
}

function outWriteSimpleWriter(
    typeName: string,
    typeSize: number,
    setFunc: string,
    argTransform?: string
) {
    outWriteWriterLn(`export function write${typeName}(x: ${typeName}, dataView?: DataViewExt): DataViewExt<${typeName}> {
        if (!dataView) dataView = DataViewExt.alloc(${typeSize});
        if (!x) return dataView as any;
        dataView.${setFunc}(0, ${argTransform || "x"}, true);
        return dataView as any;
    }`);
}

function outWriteSimpleReader(
    typeName: string,
    readFunc: string,
    outTransform?: string
) {
    outWriteReaderLn(`export function read${typeName}(from: BunPointer, offset: number): ${typeName} {
        let out = bunRead.${readFunc}(from!, offset);
        ${outTransform ? `out = ${outTransform};\n` : ""}return out;
    }`);
}

outWriteSimpleWriter("size_t", 8, "setBigUint64");
outWriteSimpleWriter("int32_t", 4, "setInt32");
outWriteSimpleWriter("uint16_t", 2, "setUint16");
outWriteSimpleWriter("uint32_t", 4, "setUint32");
outWriteSimpleWriter("uint64_t", 4, "setBigUint64", "BigInt(x)");
outWriteSimpleWriter("double", 4, "setFloat64");
outWriteSimpleWriter("float", 4, "setFloat32");

outWriteSimpleReader("size_t", "u64");
outWriteSimpleReader("int32_t", "i32");
outWriteSimpleReader("uint16_t", "u16");
outWriteSimpleReader("uint32_t", "u32");
outWriteSimpleReader("uint64_t", "u64");
outWriteSimpleReader("double", "f64");
outWriteSimpleReader("float", "f32");

function getIntegerLiteralStrOrRefName(ast: any): string {
    if (Array.isArray(ast)) {
        if (ast.length !== 1) {
            debugger;
        }
        return getIntegerLiteralStrOrRefName(ast[0]);
    }

    if (ast.kind === "ConstantExpr") {
        return getIntegerLiteralStrOrRefName(ast.inner);
    }

    if (ast.kind === "IntegerLiteral") {
        return ast.value;
    }

    if (ast.kind === "BinaryOperator") {
        if (ast.opcode === "<<") {
            const a = getIntegerLiteralStrOrRefName(ast.inner[0]);
            const b = getIntegerLiteralStrOrRefName(ast.inner[1]);

            return `(${a} << ${b})`;
        }

        if (ast.opcode === "|") {
            const a = getIntegerLiteralStrOrRefName(ast.inner[0]);
            const b = getIntegerLiteralStrOrRefName(ast.inner[1]);

            return `(${a} | ${b})`;
        }
    }

    if (ast.kind === "DeclRefExpr") {
        return ast.referencedDecl.name;
    }

    debugger;
    return "";
}

function pickBasePtrTypeFromConstPtr(type: string) {
    const parts = type.split(/<|>/g);
    const basePtrType = parts.slice(1, parts.length - 1);
    return basePtrType;
}

function extractQualTypeOrPtr(qualType: string, ffi: string) {
    if (qualType === "const char *") {
        return "CString";
    }

    const constPtrMatch = (qualType as string).match(
        /^const (?:struct\s)?(\w+) \*$/
    );

    if (constPtrMatch) {
        if (ffi) return "Pointer";
        const ptrBaseName = constPtrMatch[1];
        return "ConstPtrT<" + ptrBaseName + ">";
    }

    const ptrMatch = (qualType as string).match(/^(?:struct\s)?(\w+) \*$/);

    if (ptrMatch) {
        if (ffi) return "Pointer";
        const ptrBaseName = ptrMatch[1];
        return "PtrT<" + ptrBaseName + ">";
    }

    return qualType + ffi;
}

let ptrTypeSymbols = new Set<string>();
let funcTypeSymbols = new Set<string>();

for (const statement of wgpuAst) {
    if (
        statement.loc?.includedFrom &&
        !statement.loc.includedFrom.file.endsWith("wgpu.h") &&
        !!statement.loc?.includedFrom
    ) {
        continue;
    }

    // filter builtins
    if (statement.kind === "TypedefDecl") {
        if (
            statement.inner.length === 1 &&
            statement.type.qualType === statement.inner[0].type.qualType &&
            statement.inner[0].kind === "BuiltinType"
        ) {
            continue;
        }
        // filter objc shit
        if (statement.name.startsWith("__NS")) {
            continue;
        }

        if (statement.name.startsWith("__builtin_")) {
            continue;
        }

        if (
            statement.inner[0].kind === "ElaboratedType" &&
            statement.name === statement.inner[0].inner[0].decl.name
        ) {
            continue;
        }
    }

    // enum
    if (statement.kind === "EnumDecl") {
        const enumName = statement.name;

        outWriteLn("export enum " + enumName + " {");

        for (const item of statement.inner) {
            const itemName = item.name;
            const value = getIntegerLiteralStrOrRefName(item.inner);

            outWriteLn(`${itemName} = ${value},`, 1);
        }

        outWriteLn("}");
        outWriteLn(`export const ${enumName}_FFI = FFIType.int32_t;`);
        outWriteWriterLn(`export function write${enumName}(x: ${enumName}, dataView?: DataViewExt): DataViewExt<${enumName}> {
            if (!dataView) dataView = DataViewExt.alloc(4);
            dataView.setInt32(0, x, true);
            return dataView as any;
        }`);
        outWriteReaderLn(`export function read${enumName}(from: Pointer, offset: number): ${enumName} {
            return bunRead.i32(from!, offset) as any as ${enumName};
        }`);
        outWriteLn();

        continue;
    }

    // alias typedecl
    if (statement.kind === "TypedefDecl") {
        const name = statement.name;
        if (!!statement.type?.typeAliasDeclId) {
            const base = statement.type.qualType;
            outWriteLn(`export type ${name} = ${base};`);
            outWriteLn(`export const ${name}_FFI = ${base}_FFI;`);
            outWriteWriterLn(`export const write${name} = write${base};`);
            outWriteReaderLn(`export const read${name} = read${base};`);
            continue;
        }
    }

    // struct
    if (statement.kind === "RecordDecl") {
        const name = statement.name;

        if (!statement.inner) {
            continue;
        }

        const bufferSize = getSizeOf(HEADER_PATH, name);

        // TODO: read* struct utils similar to write*

        outWriteLn("export type " + name + " = {");
        outWriteWriterLn(
            `export function write${name}(x: DeepPartial<${name}>, dataView?: DataViewExt): DataViewExt<${name}> {`
        );
        outWriteWriterLn(
            `if (!dataView) dataView = DataViewExt.alloc(${bufferSize});`,
            1
        );
        outWriteWriterLn(`if (!x) return dataView as any;`, 1);

        outWriteReaderLn(
            `export function read${name}(from: Pointer, offset: number): ${name} {`
        );
        outWriteReaderLn(`const out: ${name} = {} as any;`, 1);

        for (const item of statement.inner) {
            if (item.kind !== "FieldDecl") {
                debugger;
            }

            const fieldName = item.name;
            const fieldOffet = getOffsetOf(HEADER_PATH, name, fieldName);

            if (item.type.typeAliasDeclId) {
                const refType = item.type.qualType;
                outWriteLn(fieldName + ": " + refType + ";", 1);

                if (refType.includes("PtrT<")) {
                    const basePtrT = pickBasePtrTypeFromConstPtr(refType);

                    outWriteWriterLn(
                        `
                        if (isPtrOrConstPtr(x.${fieldName})) {
                            write${refType}(x.${fieldName}! as any, dataView.subview(${fieldOffet}));
                        } else {
                            write${basePtrT}(x.${fieldName}! as any, dataView.subview(${fieldOffet}));
                        }
                    `,
                        1
                    );
                } else {
                    outWriteWriterLn(
                        `write${refType}(x.${fieldName}! as any, dataView.subview(${fieldOffet}));`,
                        1
                    );
                }

                outWriteReaderLn(
                    `out.${fieldName} = from ? read${refType}(from!, offset + ${fieldOffet}) as any : undefined!;`,
                    1
                );
                continue;
            }

            const qualType = extractQualTypeOrPtr(item.type.qualType, "");

            outWriteLn(fieldName + ": " + qualType + ";", 1);

            if (qualType.includes("PtrT<")) {
                const basePtrT = pickBasePtrTypeFromConstPtr(qualType);

                outWriteWriterLn(
                    `
                    if (isPtrOrConstPtr(x.${fieldName})) {
                        write${qualType}(x.${fieldName}! as any, dataView.subview(${fieldOffet}));
                    } else {
                        write${basePtrT}(x.${fieldName}! as any, dataView.subview(${fieldOffet}));
                    }
                `,
                    1
                );
            } else {
                outWriteWriterLn(
                    `write${qualType}(x.${fieldName}! as any, dataView.subview(${fieldOffet}));`,
                    1
                );
            }

            outWriteReaderLn(
                `out.${fieldName} = from ? read${qualType}(from!, offset + ${fieldOffet}) as any : undefined!;`,
                1
            );
            continue;
        }

        outWriteWriterLn(`return dataView as any;\n}`, 1);
        outWriteReaderLn(`return out;\n}`, 1);
        outWriteLn("};");
        outWriteLn(`export const ${name}_FFI = Pointer as PtrT<${name}>;`);
        ptrTypeSymbols.add(name);
        ptrTypeSymbols.add(`${name}_FFI`);
        outWriteLn();

        continue;
    }

    // func callback type
    if (
        statement.kind === "TypedefDecl" &&
        statement.inner[0].kind === "PointerType" &&
        statement.inner[0].inner[0].kind === "ParenType" &&
        statement.inner[0].inner[0].inner[0].kind === "FunctionProtoType"
    ) {
        const declName = statement.name;
        const funcProto = statement.inner[0].inner[0].inner[0];

        const returnTypeAst = funcProto.inner[0];
        const argsAst = funcProto.inner.slice(1);

        function extractPrintableType(ast: any): string {
            if (ast.kind === "BuiltinType") {
                return ast.type.qualType;
            }

            if (ast.kind === "TypedefType") {
                if (ast.type.qualType.includes("*")) {
                    debugger;
                }
                return ast.decl.name;
            }

            if (ast.kind === "PointerType") {
                if (ast.type.qualType === "const char *") {
                    return "CString";
                }
                return "PtrT<" + extractPrintableType(ast.inner[0]) + ">";
            }

            if (ast.kind === "RecordType") {
                return ast.decl.name;
            }

            if (ast.kind === "ElaboratedType") {
                return extractPrintableType(ast.inner[0]);
            }

            if (ast.kind === "QualType") {
                return extractPrintableType(ast.inner[0]);
            }

            debugger;
            return "";
        }

        const returnType = extractPrintableType(returnTypeAst);

        const argsTypes = argsAst.map(
            (x: any, i: number) =>
                (x.name || `arg${i}`) + ": " + extractPrintableType(x)
        );

        outWriteLn(
            `export type ${declName} = (${argsTypes.join(
                ", "
            )}) => ${returnType};`
        );
        outWriteLn(
            `export const ${declName}_FFI = Pointer as PtrT<${declName}>;`
        );
        ptrTypeSymbols.add(declName);
        ptrTypeSymbols.add(`${declName}_FFI`);
        funcTypeSymbols.add(declName);
        funcTypeSymbols.add(`${declName}_FFI`);
        outWriteLn();

        outWriteWriterLn(`
        export function write${declName}(x: ${declName}, dataView?: DataViewExt): DataViewExt<${declName}> {
            if (!dataView) dataView = DataViewExt.alloc(8);
            if (!x) return dataView as any;
            const jscb_ptr = new BunJSCallback(x, eval('${declName}_funcdef')).ptr;
            dataView.setBigUint64(0, BigInt(jscb_ptr!), true);
            return dataView as any;
        }`);

        outWriteReaderLn(`
        export function read${declName}(from: Pointer, offset: number): ${declName} {
            const p = bunRead.ptr(from!, offset);
            const cb = BunCFunction({
                ...eval('${declName}_funcdef'),
                ptr: p,
            });
            return cb as any as ${declName};
        }`);

        continue;
    }

    // func decl
    if (statement.kind === "FunctionDecl") {
        const declName = statement.name;

        const returnType = extractQualTypeOrPtr(
            statement.type.qualType.split("(")[0].trim(),
            ""
        );
        const returnTypeFFI = extractQualTypeOrPtr(
            statement.type.qualType.split("(")[0].trim(),
            "_FFI"
        );

        outWriteLn(`export function ${declName} (`);

        outWriteLibLn(`${declName}: {`, 1);
        outWriteLibLn(`returns: ${returnTypeFFI},`, 2);
        outWriteLibLn(`args: [`, 2);

        let argsNames: string[] = [];
        let argsTypeFFINames: string[] = [];
        let argsTypeNames: string[] = [];

        if (!statement.inner) {
            if (declName === "wgpuGetVersion") {
            } else {
                debugger;
            }
        } else {
            for (const item of statement.inner) {
                if (item.kind === "ParmVarDecl") {
                    const paramName = item.name;
                    const type = extractQualTypeOrPtr(item.type.qualType, "");
                    const typeFFI = extractQualTypeOrPtr(
                        item.type.qualType,
                        "_FFI"
                    );

                    if (type.startsWith("ConstPtrT<")) {
                        const basePtrType = pickBasePtrTypeFromConstPtr(type);
                        outWriteLn(
                            `${paramName}: ${type} | DeepPartial<${basePtrType}>,`,
                            1
                        );
                    } else {
                        outWriteLn(`${paramName}: ${type},`, 1);
                    }

                    outWriteLibLn(`// ${paramName}`, 3);

                    if (
                        ptrTypeSymbols.has(type) ||
                        ptrTypeSymbols.has(typeFFI)
                    ) {
                        outWriteLibLn(`FFIType.ptr,`, 3);
                    } else {
                        outWriteLibLn(`${typeFFI},`, 3);
                    }

                    argsNames.push(paramName);

                    argsTypeFFINames.push(typeFFI);
                    argsTypeNames.push(type);
                } else {
                    debugger;
                }
            }
        }

        outWriteLibLn(`],`, 2); // close args
        outWriteLibLn(`},`, 1); // close func def

        outWriteLn(`): ${returnType} {`);
        argsNames.forEach((argName, argI) => {
            const argTypeFFI = argsTypeFFINames[argI];
            const argType = argsTypeNames[argI];

            if (argTypeFFI === "CString") {
                outWriteLn(`let arg${argI} = ${argName};`, 3);
            } else {
                out.write(`      let arg${argI} `);

                if (
                    funcTypeSymbols.has(argType) ||
                    funcTypeSymbols.has(argTypeFFI)
                ) {
                    outWriteLn(
                        `= new BunJSCallback(${argName}, eval('${argTypeFFI}_funcdef')).ptr;`,
                        3
                    );
                }
                {
                    out.write(";\n");
                    outWriteLn(
                        `
                    if (${argName} && typeof ${argName} === 'object' && !('BYTES_PER_ELEMENT' in ${argName}) &&
                        // @ts-ignore
                        ${argTypeFFI} === FFIType.ptr
                    ) {
                        ${[
                            // its typed buffer
                            argType.startsWith("PtrT<") &&
                                `arg${argI} = ${argName};`,
                            argType.startsWith("ConstPtrT<") &&
                                argType !== "ConstPtrT<void>" &&
                                `arg${argI} = write${pickBasePtrTypeFromConstPtr(
                                    argType
                                )}(${argName} as any).typed as any;`,
                            //     argType.startsWith("PtrT<") ||
                            //     argType.startsWith("ConstPtrT<")
                            //         ? `arg${argI} = ${argName};`
                            //         : `
                            // arg${argI} = write${argType}(${argName}).typed as any;`
                            // }
                        ]
                            .filter(Boolean)
                            .join("\n")}
                    } else {
                        arg${argI} = ${argName} as any;
                    }
                    `,
                        3
                    );
                    // out.write(`= write${argType}(${argName});\n`);
                }
            }
        });
        if (
            funcTypeSymbols.has(returnType) ||
            funcTypeSymbols.has(returnTypeFFI)
        ) {
            // TODO: return proc address
        }
        outWriteLn(
            `const result = wgpulib.${declName}(${argsNames
                .map((_, i) => `arg${i}!`)
                .join(", ")});
            return result as any;`,
            3
        );
        outWriteLn(`}`);
        outWriteLn();
        continue;
    }

    // opque pointer type
    if (
        statement.kind === "TypedefDecl" &&
        statement.type.qualType.startsWith("struct ") &&
        statement.type.qualType.endsWith("Impl *")
    ) {
        outWriteLn(`export type ${statement.name} = Pointer;`);
        outWriteLn(`export const ${statement.name}_FFI = Pointer;`);
        outWriteSimpleWriter(statement.name, 8, "setBigUint64", "BigInt(x!)");
        outWriteSimpleReader(statement.name, "ptr");
        continue;
    }
    debugger;
}

outWriteLibLn(`
}).symbols;
`);

for (const l of outLibLines) {
    out.write(l);
}
for (const l of outWritersLines) {
    out.write(l);
}
for (const l of outReadersLines) {
    out.write(l);
}
out.end();

{
    const w = Bun.file("./generate-wgpu-bindings_sizeofcache.json").writer();
    w.write(JSON.stringify(__getSizeOfCache));
    w.end();
}

{
    const w = Bun.file("./generate-wgpu-bindings_offsetofcache.json").writer();
    w.write(JSON.stringify(__getOffsetOf));
    w.end();
}
