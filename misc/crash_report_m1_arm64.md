```
DFG ASSERTION FAILED: Edge verification error: D@50->Check:Untyped:D@49 was expected to have type BytecodeTop but has type Int52Any (51539607552)
/Users/runner/work/WebKit/WebKit/Source/JavaScriptCore/dfg/DFGAbstractInterpreterInlines.h(187) : void JSC::DFG::AbstractInterpreter<JSC::DFG::InPlaceAbstractState>::verifyEdge(Node *, Edge) [AbstractStateType = JSC::DFG::InPlaceAbstractState]

While handling node D@50

Graph at time of failure:

       10: DFG for readWGPUTexture#<no-hash>:[0x130a97b50->0x130a7b010->0x13089e800, DFGFunctionCall, 42 (StrictMode)]:
       10:   Fixpoint state: FixpointNotConverged; Form: ThreadedCPS; Unification state: GloballyUnified; Ref count state: EverythingIsLive
       10:   Arguments for block#0: D@0, D@1, D@2

     0 10: Block #0 (bc#0): (OSR target)
     0 10:   Execution count: 1.000000
     0 10:   Predecessors:
     0 10:   Successors: #1
     0 10:   Dominated by: #root #0
     0 10:   Dominates: #0 #1
     0 10:   Dominance Frontier:
     0 10:   Iterated Dominance Frontier:
     0 10:   States: StructuresAreWatched
     0 10:   Vars Before: arg2:(Int32, none:StructuresAreClobbered) arg1:(BytecodeTop, TOP, TOP, none:StructuresAreClobbered) arg0:(BytecodeTop, TOP, TOP, none:StructuresAreClobbered)
     0 10:   Intersected Vars Before: arg2:(FullTop, TOP, TOP, none:StructuresAreClobbered) arg1:(FullTop, TOP, TOP, none:StructuresAreClobbered) arg0:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc0:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc1:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc2:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc3:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc4:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc5:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc6:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc7:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc8:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc9:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc10:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc11:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc12:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc13:(FullTop, TOP, TOP, none:StructuresAreClobbered)
     0 10:   Var Links: arg2:D@2 arg1:D@1 arg0:D@0
  0  0 10:    D@0:< 1:->	SetArgumentDefinitely(IsFlushed, this(A~<Object>/FlushedJSValue), W:SideState, bc#0, ExitValid)  predicting OtherObj
  1  0 10:    D@1:< 1:->	SetArgumentDefinitely(IsFlushed, arg1(B~<AnyIntAsDouble>/FlushedJSValue), W:SideState, bc#0, ExitValid)  predicting AnyIntAsDouble
  2  0 10:    D@2:< 1:->	SetArgumentDefinitely(IsFlushed, arg2(C<BoolInt32>/FlushedInt32), W:SideState, bc#0, ExitValid)  predicting BoolInt32
  3  0 10:    D@3:< 1:->	JSConstant(JS|PureInt, Other, Undefined, bc#0, ExitValid)
  4  0 10:    D@4:<!0:->	MovHint(Check:Untyped:D@3, MustGen, loc0, W:SideState, ClobbersExit, bc#0, ExitValid)
  5  0 10:    D@5:< 1:->	SetLocal(Check:Untyped:D@3, loc0(D~<Other>/FlushedJSValue), W:Stack(loc0), bc#0, ExitInvalid)  predicting Other
  6  0 10:    D@6:<!0:->	MovHint(Check:Untyped:D@3, MustGen, loc1, W:SideState, ClobbersExit, bc#0, ExitInvalid)
  7  0 10:    D@7:< 1:->	SetLocal(Check:Untyped:D@3, loc1(E~<Other>/FlushedJSValue), W:Stack(loc1), bc#0, ExitInvalid)  predicting Other
  8  0 10:    D@8:<!0:->	MovHint(Check:Untyped:D@3, MustGen, loc2, W:SideState, ClobbersExit, bc#0, ExitInvalid)
  9  0 10:    D@9:< 1:->	SetLocal(Check:Untyped:D@3, loc2(F~<Other>/FlushedJSValue), W:Stack(loc2), bc#0, ExitInvalid)  predicting Other
 10  0 10:   D@10:<!0:->	MovHint(Check:Untyped:D@3, MustGen, loc3, W:SideState, ClobbersExit, bc#0, ExitInvalid)
 11  0 10:   D@11:< 1:->	SetLocal(Check:Untyped:D@3, loc3(G~<Other>/FlushedJSValue), W:Stack(loc3), bc#0, ExitInvalid)  predicting Other
 12  0 10:   D@12:<!0:->	MovHint(Check:Untyped:D@3, MustGen, loc4, W:SideState, ClobbersExit, bc#0, ExitInvalid)
 13  0 10:   D@13:< 1:->	SetLocal(Check:Untyped:D@3, loc4(H~<Other>/FlushedJSValue), W:Stack(loc4), bc#0, ExitInvalid)  predicting Other
 14  0 10:   D@14:<!0:->	Jump(MustGen, T:#1, W:SideState, bc#0, ExitInvalid)
     0 10:   States: InvalidBranchDirection, StructuresAreWatched
     0 10:   Vars After: arg2:(Int32, none:StructuresAreClobbered) arg1:(BytecodeTop, TOP, TOP, none:StructuresAreClobbered) arg0:(BytecodeTop, TOP, TOP, none:StructuresAreClobbered) loc0:(Other, Undefined, 1:StructuresAreWatched) loc1:(Other, Undefined, 1:StructuresAreWatched) loc2:(Other, Undefined, 1:StructuresAreWatched) loc3:(Other, Undefined, 1:StructuresAreWatched) loc4:(Other, Undefined, 1:StructuresAreWatched)
     0 10:   Var Links: arg2:D@2 arg1:D@1 arg0:D@0 loc0:D@5 loc1:D@7 loc2:D@9 loc3:D@11 loc4:D@13

     1 10: Block #1 (bc#0):
     1 10:   Execution count: 1.000000
     1 10:   Predecessors: #0
     1 10:   Successors:
     1 10:   Dominated by: #root #0 #1
     1 10:   Dominates: #1
     1 10:   Dominance Frontier:
     1 10:   Iterated Dominance Frontier:
     1 10:   Phi Nodes: D@60<arg1,1, IsFlushed>->(D@1), D@61<arg2,1, IsFlushed>->(D@2), D@62<this,1, IsFlushed>->(D@0)
     1 10:   States: StructuresAreWatched
     1 10:   Vars Before: arg2:(Int32, none:StructuresAreClobbered) arg1:(BytecodeTop, TOP, TOP, none:StructuresAreClobbered) arg0:(BytecodeTop, TOP, TOP, none:StructuresAreClobbered)
     1 10:   Intersected Vars Before: arg2:(FullTop, TOP, TOP, none:StructuresAreClobbered) arg1:(FullTop, TOP, TOP, none:StructuresAreClobbered) arg0:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc0:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc1:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc2:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc3:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc4:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc5:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc6:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc7:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc8:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc9:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc10:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc11:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc12:(FullTop, TOP, TOP, none:StructuresAreClobbered) loc13:(FullTop, TOP, TOP, none:StructuresAreClobbered)
     1 10:   Var Links: arg2:D@61 arg1:D@60 arg0:D@62
  0  1 10:   D@15:< 1:->	JSConstant(JS|PureInt, Function, Weak:Object: 0x130896760 with butterfly 0x0(base=0xfffffffffffffff8) (Structure %Dg:Function), StructureID: 39968, bc#0, ExitInvalid)
  1  1 10:   D@16:< 1:->	JSConstant(JS|PureInt, OtherObj, Weak:Object: 0x1308a0088 with butterfly 0x0(base=0xfffffffffffffff8) (Structure %BZ:JSModuleEnvironment), StructureID: 34256, bc#0, ExitInvalid)
  2  1 10:   D@17:<!0:->	MovHint(Check:Untyped:D@16, MustGen, loc4, W:SideState, ClobbersExit, bc#0, ExitInvalid)
  3  1 10:   D@18:<!0:->	ExitOK(MustGen, W:SideState, bc#0, ExitValid)
  4  1 10:   D@19:<!0:->	InvalidationPoint(MustGen, W:SideState, Exits, bc#0, ExitValid)
  5  1 10:   D@20:< 1:->	SetLocal(Check:Untyped:D@16, loc4(I~<Object>/FlushedJSValue), W:Stack(loc4), bc#0, exit: bc#1, ExitValid)  predicting OtherObj
  6  1 10:   D@21:< 1:->	JSConstant(JS|PureNum|NeedsNegZero|NeedsNaNOrInfinity|UseAsOther, OtherObj, Weak:Object: 0x130580220 with butterfly 0x0(base=0xfffffffffffffff8) (Structure %BZ:JSModuleEnvironment), StructureID: 34256, bc#1, ExitValid)
  7  1 10:   D@22:<!0:->	MovHint(Check:Untyped:D@21, MustGen, loc8, W:SideState, ClobbersExit, bc#1, ExitValid)
  8  1 10:   D@23:< 1:->	SetLocal(Check:Untyped:D@21, loc8(J~<Object>/FlushedJSValue), W:Stack(loc8), bc#1, exit: bc#8, ExitValid)  predicting OtherObj
  9  1 10:   D@24:<!0:->	Check(MustGen, bc#8, ExitValid)
 10  1 10:   D@25:< 1:->	GetClosureVar(KnownCell:D@21, JS|PureNum|NeedsNegZero|NeedsNaNOrInfinity|UseAsOther, Final, scope9, R:ScopeProperties(9), bc#8, ExitValid)  predicting Final
 11  1 10:   D@26:<!0:->	MovHint(Check:Untyped:D@25, MustGen, loc9, W:SideState, ClobbersExit, bc#8, ExitValid)
 12  1 10:   D@27:< 1:->	SetLocal(Check:Untyped:D@25, loc9(K~<Final>/FlushedJSValue), W:Stack(loc9), bc#8, exit: bc#17, ExitValid)  predicting Final
 13  1 10:   D@28:<!0:->	CheckNotEmpty(Check:Untyped:D@25, MustGen, Exits, bc#17, ExitValid)
 14  1 10:   D@29:<!0:->	MovHint(Check:Untyped:D@25, MustGen, loc8, W:SideState, ClobbersExit, bc#19, ExitValid)
 15  1 10:   D@30:< 1:->	SetLocal(Check:Untyped:D@25, loc8(L~<Final>/FlushedJSValue), W:Stack(loc8), bc#19, exit: bc#22, ExitValid)  predicting Final
 16  1 10:   D@31:<!0:->	FilterGetByStatus(Check:Untyped:D@25, MustGen, (Simple, <id='uid:(ptr)', [0x300007200:[0x7200/29184, Object, (12/12, 0/0){u8:0, u16:1, u32:2, ptr:3, i8:4, i16:5, i32:6, i64:7, u64:8, intptr:9, f32:10, f64:11}, NonArray, Proto:0x1304e4280, Leaf (Watched)]], [], offset = 3>, seenInJIT = true), W:SideState, bc#22, ExitValid)
 17  1 10:   D@32:<!0:->	Check(MustGen, bc#22, ExitValid)
 18  1 10:   D@33:<!0:->	CheckStructure(Check:Cell:D@25, MustGen, [%At:Object], R:JSCell_structureID, Exits, bc#22, ExitValid)
 19  1 10:   D@34:< 1:->	GetByOffset(KnownCell:D@25, KnownCell:D@25, JS|PureNum|NeedsNegZero|NeedsNaNOrInfinity|UseAsOther, Function, id1{ptr}, 3, R:NamedProperties(1), bc#22, ExitValid)  predicting Function
 20  1 10:   D@35:<!0:->	MovHint(Check:Untyped:D@34, MustGen, loc5, W:SideState, ClobbersExit, bc#22, ExitValid)
 21  1 10:   D@36:< 1:->	SetLocal(Check:Untyped:D@34, loc5(M~<Function>/FlushedJSValue), W:Stack(loc5), bc#22, exit: bc#28, ExitValid)  predicting Function
 22  1 10:   D@37:<!0:->	GetLocal(Check:Untyped:D@60, JS|MustGen|PureNum|NeedsNegZero|NeedsNaNOrInfinity|UseAsOther, AnyIntAsDouble, arg1(B~<AnyIntAsDouble>/FlushedJSValue), R:Stack(arg1), bc#28, ExitValid)  predicting AnyIntAsDouble
 23  1 10:   D@38:<!0:->	MovHint(Check:Untyped:D@37, MustGen, loc7, W:SideState, ClobbersExit, bc#28, ExitValid)
 24  1 10:   D@39:< 1:->	SetLocal(Check:Untyped:D@37, loc7(O~<AnyIntAsDouble>/FlushedJSValue), W:Stack(loc7), bc#28, exit: bc#31, ExitValid)  predicting AnyIntAsDouble
 25  1 10:   D@40:<!0:->	GetLocal(Check:Untyped:D@61, JS|MustGen|PureNum|NeedsNegZero|NeedsNaNOrInfinity|UseAsOther, BoolInt32, arg2(C<BoolInt32>/FlushedInt32), R:Stack(arg2), bc#31, ExitValid)  predicting BoolInt32
 26  1 10:   D@41:<!0:->	MovHint(Check:Untyped:D@40, MustGen, loc6, W:SideState, ClobbersExit, bc#31, ExitValid)
 27  1 10:   D@42:< 1:->	SetLocal(Check:Untyped:D@40, loc6(Q~<BoolInt32>/FlushedJSValue), W:Stack(loc6), bc#31, exit: bc#34, ExitValid)  predicting BoolInt32
 28  1 10:   D@43:<!0:->	Flush(Check:Untyped:D@61, MustGen|IsFlushed, arg2(C<BoolInt32>/FlushedInt32), R:Stack(arg2), W:SideState, bc#34, ExitValid)  predicting BoolInt32
 29  1 10:   D@44:<!0:->	Flush(Check:Untyped:D@60, MustGen|IsFlushed, arg1(B~<AnyIntAsDouble>/FlushedJSValue), R:Stack(arg1), W:SideState, bc#34, ExitValid)  predicting AnyIntAsDouble
 30  1 10:   D@45:<!0:->	Flush(Check:Untyped:D@62, MustGen|IsFlushed, this(A~<Object>/FlushedJSValue), R:Stack(this), W:SideState, bc#34, ExitValid)  predicting OtherObj
 31  1 10:   D@46:<!0:->	FilterCallLinkStatus(Check:Untyped:D@34, MustGen, (Function: Object: 0x13044cd80 with butterfly 0x806d000448(base=0x806d000420) (Structure 0x300009e50:[0x9e50/40528, Function, (0/0, 2/4){length:64, name:65}, NonArray, Proto:0x1304e8370, Leaf (Watched)]), StructureID: 40528; Executable: NativeExecutable:0x101114f2c/0x101114f2c), W:SideState, bc#34, ExitValid)
 32  1 10:   D@47:<!0:->	CheckIsConstant(Check:Cell:D@34, MustGen, <0x13044cd80, Function>, <host function>, Exits, bc#34, ExitValid)
 33  1 10:   D@48:<!0:->	Check(MustGen, bc#34, ExitValid)
 34  1 10:   D@63:<!0:->	CheckJSCast(Cell:D@25, MustGen, 0x104d2a878:[Object], Exits, bc#34, ExitValid)
 35  1 10:   D@64:< 1:->	Int52Rep(Check:AnyInt:D@37, Int52|PureInt, Int52Any, Exits, bc#34, ExitValid)
 36  1 10:   D@49:<!0:->	CallDOM(Cell:D@25, Int52Rep:D@64<Int52>, Int32:D@40, JS|MustGen|PureInt, FullTop, R:World, W:Heap, Exits, ClobbersExit, bc#34, ExitValid)  predicting FullTop
 37  1 10:   D@50:<!0:->	MovHint(Check:Untyped:D@49, MustGen, loc5, W:SideState, ClobbersExit, bc#34, ExitInvalid)
 38  1 10:   D@51:<!0:->	Check(MustGen, bc#34, ExitInvalid)
 39  1 10:   D@52:<!0:->	Check(MustGen, bc#34, ExitInvalid)
 40  1 10:   D@53:<!0:->	Check(MustGen, bc#34, ExitInvalid)
 41  1 10:   D@54:<!0:->	Check(MustGen, bc#34, ExitInvalid)
 42  1 10:   D@65:<!0:->	InvalidationPoint(MustGen, W:SideState, Exits, bc#34, exit: bc#40, ExitValid)
 43  1 10:   D@55:< 1:->	SetLocal(Check:Untyped:D@49, loc5(S~/FlushedJSValue), W:Stack(loc5), bc#34, exit: bc#40, ExitValid)  predicting FullTop
 44  1 10:   D@56:<!0:->	Return(Check:Untyped:D@49, MustGen, W:SideState, Exits, bc#40, ExitValid)
 45  1 10:   D@57:<!0:->	Flush(Check:Untyped:D@61, MustGen|IsFlushed, arg2(C<BoolInt32>/FlushedInt32), R:Stack(arg2), W:SideState, bc#40, ExitValid)  predicting BoolInt32
 46  1 10:   D@58:<!0:->	Flush(Check:Untyped:D@60, MustGen|IsFlushed, arg1(B~<AnyIntAsDouble>/FlushedJSValue), R:Stack(arg1), W:SideState, bc#40, ExitValid)  predicting AnyIntAsDouble
 47  1 10:   D@59:<!0:->	Flush(Check:Untyped:D@62, MustGen|IsFlushed, this(A~<Object>/FlushedJSValue), R:Stack(this), W:SideState, bc#40, ExitValid)  predicting OtherObj
     1 10:   States: InvalidBranchDirection, StructuresAreWatched
     1 10:   Vars After:
     1 10:   Var Links: arg2:D@40 arg1:D@37 arg0:D@45 loc4:D@20 loc5:D@55 loc6:D@42 loc7:D@39 loc8:D@30 loc9:D@27

       10: GC Values:
       10:     Weak:Object: 0x130896760 with butterfly 0x0(base=0xfffffffffffffff8) (Structure %Dg:Function), StructureID: 39968
       10:     Weak:Object: 0x1308a0088 with butterfly 0x0(base=0xfffffffffffffff8) (Structure %BZ:JSModuleEnvironment), StructureID: 34256
       10:     Weak:Object: 0x130580220 with butterfly 0x0(base=0xfffffffffffffff8) (Structure %BZ:JSModuleEnvironment), StructureID: 34256
       10:     Weak:Object: 0x13044cd80 with butterfly 0x806d000448(base=0x806d000420) (Structure %BR:Function), StructureID: 40528
       10: Desired watchpoints:
       10:     Watchpoint sets:
       10:     Inline watchpoint sets: 0x3000041d8, 0x300004868, 0x3000042b8, 0x300007268, 0x300009eb8, 0x300008638
       10:     SymbolTables:
       10:     FunctionExecutables: 0x13089e800
       10:     Buffer views:
       10:     Object property conditions:
       10: Structures:
       10:     %At:Object              = 0x300007200:[0x7200/29184, Object, (12/12, 0/0){u8:0, u16:1, u32:2, ptr:3, i8:4, i16:5, i32:6, i64:7, u64:8, intptr:9, f32:10, f64:11}, NonArray, Proto:0x1304e4280, Leaf (Watched)]
       10:     %BR:Function            = 0x300009e50:[0x9e50/40528, Function, (0/0, 2/4){length:64, name:65}, NonArray, Proto:0x1304e8370, Leaf (Watched)]
       10:     %BZ:JSModuleEnvironment = 0x3000085d0:[0x85d0/34256, JSModuleEnvironment, (0/0, 0/0){}, NonArray, Leaf (Watched)]
       10:     %Dg:Function            = 0x300009c20:[0x9c20/39968, Function, (0/0, 0/0){}, NonArray, Proto:0x1304e8370]


DFG ASSERTION FAILED: Edge verification error: D@50->Check:Untyped:D@49 was expected to have type BytecodeTop but has type Int52Any (51539607552)
/Users/runner/work/WebKit/WebKit/Source/JavaScriptCore/dfg/DFGAbstractInterpreterInlines.h(187) : void JSC::DFG::AbstractInterpreter<JSC::DFG::InPlaceAbstractState>::verifyEdge(Node *, Edge) [AbstractStateType = JSC::DFG::InPlaceAbstractState]
```
