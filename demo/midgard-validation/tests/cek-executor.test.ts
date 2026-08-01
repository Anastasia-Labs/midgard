import { verifyMidgardCekProgramMaterialV1 } from "@al-ft/midgard-core";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  Case,
  Constr,
  constT,
  Delay,
  Force,
  Lambda,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  decodeMidgardCekConstantWitnessV1,
  encodeMidgardCekPlutusDataV1,
} from "../src/cek-constant.js";
import {
  buildMidgardCekExecutionGraphV1,
  executeMidgardCekStructuralProgramV1,
} from "../src/cek-executor.js";
import { verifyMidgardCekCoreStepV1 } from "../src/cek-machine.js";
import { buildMidgardCanonicalCekProgramV1 } from "../src/cek-program.js";

const compileIdentity = (): Buffer =>
  compile(new Lambda(new UPLCVar(0)));

const compile = (term: UPLCTerm): Buffer =>
  Buffer.from(
    UPLCEncoder.compile(
      new UPLCProgram([1, 1, 0], term),
    ).toBuffer().buffer,
  );

describe("V1 CEK trace generator", () => {
  it("derives the script-context application and proves every structural step", () => {
    const program = buildMidgardCanonicalCekProgramV1(compileIdentity());
    const graph = buildMidgardCekExecutionGraphV1(
      program.envelope,
      program.material.values(),
      Buffer.from("d87980", "hex"),
    );

    const execution = executeMidgardCekStructuralProgramV1({
      root: graph.root,
      material: graph.material.values(),
      constantWitnesses: graph.constantWitnesses,
      maxSteps: 32,
    });

    expect(execution.terminalState.mode).toBe("haltSuccess");
    expect(execution.terminalState.focusRoot).toEqual(graph.contextValueRoot);
    expect(execution.steps.map((step) => step.witness.kind)).toEqual([
      "computeApplication",
      "computeLambda",
      "returnApplyArgument",
      "computeContextConstant",
      "returnApplyLambda",
      "computeVariable",
      "lookupEnvironment",
      "returnEmptyContinuation",
    ]);
    expect(
      execution.steps.every((step) =>
        verifyMidgardCekCoreStepV1(step.pre, step.post, step.witness),
      ),
    ).toBe(true);
    expect(execution.terminalState.cpu).toBe(4n * 16_000n);
    expect(execution.terminalState.memory).toBe(4n * 100n);
  });

  it("fails closed at the declared trace-step bound", () => {
    const program = buildMidgardCanonicalCekProgramV1(compileIdentity());
    const graph = buildMidgardCekExecutionGraphV1(
      program.envelope,
      program.material.values(),
      Buffer.from("d87980", "hex"),
    );
    expect(() =>
      executeMidgardCekStructuralProgramV1({
        root: graph.root,
        material: graph.material.values(),
        constantWitnesses: graph.constantWitnesses,
        maxSteps: 7,
      }),
    ).toThrow(/7-step bound/u);
  });

  it("stops after retaining the first transition over the execution budget", () => {
    const selfApplication = new Lambda(
      new Application(new UPLCVar(0), new UPLCVar(0)),
    );
    const program = buildMidgardCanonicalCekProgramV1(
      compile(new Application(selfApplication, selfApplication)),
    );
    const graph = buildMidgardCekExecutionGraphV1(
      program.envelope,
      program.material.values(),
      Buffer.from("d87980", "hex"),
    );
    const execution = executeMidgardCekStructuralProgramV1({
      root: graph.root,
      material: graph.material.values(),
      constantWitnesses: graph.constantWitnesses,
      maxSteps: 64,
      executionBudget: { cpu: 0n, memory: 0n },
    });

    expect(execution.stopReason).toBe("budgetExceeded");
    expect(execution.steps.length).toBeGreaterThan(0);
    expect(execution.steps.length).toBeLessThan(64);
    expect(execution.terminalState.mode).not.toMatch(/^halt/u);
    expect(
      execution.terminalState.cpu > 0n ||
        execution.terminalState.memory > 0n,
    ).toBe(true);
    expect(
      execution.steps
        .slice(0, -1)
        .every((step) => step.post.cpu <= 0n && step.post.memory <= 0n),
    ).toBe(true);
    expect(
      execution.steps.every((step) =>
        verifyMidgardCekCoreStepV1(step.pre, step.post, step.witness),
      ),
    ).toBe(true);
  });

  it("rejects source constants above the independently revealed L1 preimage bound", () => {
    const payload = Buffer.alloc(9_216, 0x5a);
    expect(() =>
      buildMidgardCanonicalCekProgramV1(
        compile(UPLCConst.byteString(new DataB(payload).bytes)),
      ),
    ).toThrow(/9,215-byte L1 proof envelope/u);
  });

  it("streams constructor fields through case selection and application", () => {
    const program = buildMidgardCanonicalCekProgramV1(
      compile(
        new Lambda(
          new Case(
            new Constr(0n, [UPLCConst.int(42)]),
            [new Lambda(new UPLCVar(0))],
          ),
        ),
      ),
    );
    const graph = buildMidgardCekExecutionGraphV1(
      program.envelope,
      program.material.values(),
      Buffer.from("d87980", "hex"),
    );
    const execution = executeMidgardCekStructuralProgramV1({
      root: graph.root,
      material: graph.material.values(),
      constantWitnesses: graph.constantWitnesses,
      maxSteps: 64,
    });
    expect(execution.terminalState.mode).toBe("haltSuccess");
    expect(
      execution.steps.some(
        (step) => step.witness.kind === "selectCaseBranch",
      ),
    ).toBe(true);
    expect(
      execution.steps.some(
        (step) => step.witness.kind === "applyCaseValue",
      ),
    ).toBe(true);
  });

  it("executes delayed terms and authenticates explicit errors", () => {
    const delayed = buildMidgardCanonicalCekProgramV1(
      compile(new Force(new Delay(UPLCConst.unit))),
    );
    const execution = executeMidgardCekStructuralProgramV1({
      root: delayed.envelope.termRoot,
      material: delayed.material.values(),
      constantWitnesses: delayed.constantWitnesses,
      maxSteps: 16,
    });
    expect(execution.terminalState.mode).toBe("haltSuccess");
    expect(execution.steps.map((step) => step.witness.kind)).toContain(
      "returnForceDelay",
    );

    const unbound = buildMidgardCanonicalCekProgramV1(
      compile(new UPLCVar(0)),
    );
    const rejected = executeMidgardCekStructuralProgramV1({
      root: unbound.envelope.termRoot,
      material: unbound.material.values(),
      constantWitnesses: unbound.constantWitnesses,
      maxSteps: 8,
    });
    expect(rejected.terminalState.mode).toBe("haltError");
    expect(rejected.terminalState.auxiliary).toBe(1n);
  });

  it("authenticates builtin runtime-type failures without charging a builtin", () => {
    const program = buildMidgardCanonicalCekProgramV1(
      compile(
        new Lambda(
          new Application(
            new Application(Builtin.addInteger, new UPLCVar(0)),
            UPLCConst.int(1),
          ),
        ),
      ),
    );
    const graph = buildMidgardCekExecutionGraphV1(
      program.envelope,
      program.material.values(),
      Buffer.from("d87980", "hex"),
    );
    const execution = executeMidgardCekStructuralProgramV1({
      root: graph.root,
      material: graph.material.values(),
      constantWitnesses: graph.constantWitnesses,
      maxSteps: 64,
    });
    expect(execution.terminalState.mode).toBe("haltError");
    expect(execution.terminalState.auxiliary).toBe(7n);
    expect(execution.steps.at(-1)?.witness.kind).toBe(
      "executeBuiltinTypeFailure",
    );
    expect(execution.steps.at(-1)?.post.cpu).toBe(
      execution.steps.at(-1)?.pre.cpu,
    );
    expect(execution.steps.at(-1)?.post.memory).toBe(
      execution.steps.at(-1)?.pre.memory,
    );
  });

  it("generates direct builtin success and semantic-failure steps", () => {
    const successfulProgram = buildMidgardCanonicalCekProgramV1(
      compile(
        new Lambda(
          new Application(
            new Application(Builtin.addInteger, UPLCConst.int(41)),
            UPLCConst.int(1),
          ),
        ),
      ),
    );
    const successfulGraph = buildMidgardCekExecutionGraphV1(
      successfulProgram.envelope,
      successfulProgram.material.values(),
      Buffer.from("d87980", "hex"),
    );
    const successful = executeMidgardCekStructuralProgramV1({
      root: successfulGraph.root,
      material: successfulGraph.material.values(),
      constantWitnesses: successfulGraph.constantWitnesses,
      maxSteps: 64,
    });
    expect(successful.terminalState.mode).toBe("haltSuccess");
    const builtinStep = successful.steps.find(
      (step) => step.witness.kind === "executeBuiltinDirect",
    );
    expect(builtinStep).toBeDefined();
    if (builtinStep?.witness.kind === "executeBuiltinDirect") {
      expect(builtinStep.witness.result.kind).toBe("constant");
      if (builtinStep.witness.result.kind === "constant") {
        expect(
          decodeMidgardCekConstantWitnessV1(
            builtinStep.witness.result.witness,
          ).payload,
        ).toMatchObject({ int: 42n });
      }
    }

    const failingProgram = buildMidgardCanonicalCekProgramV1(
      compile(
        new Application(
          new Application(Builtin.quotientInteger, UPLCConst.int(1)),
          UPLCConst.int(0),
        ),
      ),
    );
    const verified = verifyMidgardCekProgramMaterialV1(
      failingProgram.envelope,
      failingProgram.material.values(),
    );
    const constants = new Map(
      verified.constants.map((constant) => [
        Buffer.from(constant.valueRoot).toString("hex"),
        {
          kind: "constant" as const,
          witness: {
            typeCbor: constant.typeCbor,
            payloadCbor: constant.payloadCbor,
          },
        },
      ]),
    );
    const failed = executeMidgardCekStructuralProgramV1({
      root: failingProgram.envelope.termRoot,
      material: failingProgram.material.values(),
      constantWitnesses: constants,
      maxSteps: 32,
    });
    expect(failed.terminalState.mode).toBe("haltError");
    expect(failed.steps.at(-1)?.witness.kind).toBe(
      "executeBuiltinFailure",
    );
  });

  it("routes tag-51 direct results to semantic witnesses only past the boundary", () => {
    const emptyDirectProgram = buildMidgardCanonicalCekProgramV1(
      compile(
        new Application(
          Builtin.serialiseData,
          UPLCConst.data(new DataList([])),
        ),
      ),
    );
    const emptyDirect = executeMidgardCekStructuralProgramV1({
      root: emptyDirectProgram.envelope.termRoot,
      material: emptyDirectProgram.material.values(),
      constantWitnesses: emptyDirectProgram.constantWitnesses,
      maxSteps: 32,
    });
    const emptyDirectStep = emptyDirect.steps.find(
      (step) => step.witness.kind === "executeBuiltinDirect",
    );
    expect(emptyDirectStep?.witness.kind).toBe("executeBuiltinDirect");
    if (emptyDirectStep?.witness.kind === "executeBuiltinDirect") {
      expect(emptyDirectStep.witness.tag).toBe(51n);
      expect(emptyDirectStep.witness.result.kind).toBe("constant");
      if (emptyDirectStep.witness.result.kind === "constant") {
        const decoded = decodeMidgardCekConstantWitnessV1(
          emptyDirectStep.witness.result.witness,
        );
        expect(decoded.payload).toMatchObject({ bytes: expect.anything() });
        if (decoded.payload instanceof DataB) {
          expect(decoded.payload.bytes.toBuffer()).toEqual(Buffer.from([0x80]));
        }
      }
    }
    expect(emptyDirect.terminalState.mode).toBe("haltSuccess");

    const largeData = new DataList(
      Array.from({ length: 9_000 }, () => new DataI(0n)),
    );
    const largeProgram = buildMidgardCanonicalCekProgramV1(
      compile(
        new Application(Builtin.serialiseData, UPLCConst.data(largeData)),
      ),
    );
    const large = executeMidgardCekStructuralProgramV1({
      root: largeProgram.envelope.termRoot,
      material: largeProgram.material.values(),
      constantWitnesses: largeProgram.constantWitnesses,
      maxSteps: 32,
    });
    const largeSemanticStep = large.steps.find(
      (step) => step.witness.kind === "executeBuiltinSemantic",
    );
    expect(largeSemanticStep?.witness.kind).toBe("executeBuiltinSemantic");
    if (largeSemanticStep?.witness.kind === "executeBuiltinSemantic") {
      expect(largeSemanticStep.witness.tag).toBe(51n);
      expect(largeSemanticStep.witness.result.kind).toBe("semanticConstant");
      expect(largeSemanticStep.witness.material.dataNodes).toHaveLength(0);
      expect(largeSemanticStep.witness.material.listNodes).toHaveLength(0);
      expect(largeSemanticStep.witness.material.pairNodes).toHaveLength(0);
      expect(
        largeSemanticStep.witness.material.scalarPreimages[0]?.length,
      ).toBe(9_002);
      expect(largeSemanticStep.post.cpu - largeSemanticStep.pre.cpu).toBe(
        9_600_848_754n,
      );
      expect(largeSemanticStep.post.memory - largeSemanticStep.pre.memory).toBe(
        90_008n,
      );
      if (largeSemanticStep.witness.result.kind === "semanticConstant") {
        expect(
          largeSemanticStep.witness.result.witness.payload.cborLength,
        ).toBe(9_286n);
      }
    }
    expect(large.terminalState.mode).toBe("haltSuccess");
    expect(
      large.steps.every((step) =>
        verifyMidgardCekCoreStepV1(step.pre, step.post, step.witness),
      ),
    ).toBe(true);
  });

  it("proves semantic tag-51 zero-count control with its exact result and budget", () => {
    const program = buildMidgardCanonicalCekProgramV1(
      compile(
        new Lambda(new Application(Builtin.serialiseData, new UPLCVar(0))),
      ),
    );
    const graph = buildMidgardCekExecutionGraphV1(
      program.envelope,
      program.material.values(),
      encodeMidgardCekPlutusDataV1(new DataList([])),
    );
    const execution = executeMidgardCekStructuralProgramV1({
      root: graph.root,
      material: graph.material.values(),
      constantWitnesses: graph.constantWitnesses,
      maxSteps: 64,
    });
    const semantic = execution.steps.find(
      (step) => step.witness.kind === "executeBuiltinSemantic",
    );
    expect(semantic?.witness.kind).toBe("executeBuiltinSemantic");
    if (semantic?.witness.kind === "executeBuiltinSemantic") {
      expect(semantic.witness.tag).toBe(51n);
      expect(semantic.witness.material.dataNodes).toHaveLength(0);
      expect(semantic.witness.material.listNodes).toHaveLength(0);
      expect(semantic.witness.material.pairNodes).toHaveLength(0);
      expect(semantic.witness.material.scalarPreimages).toEqual([
        Buffer.from([0x80]),
      ]);
      expect(semantic.witness.result.kind).toBe("constant");
      if (semantic.witness.result.kind === "constant") {
        const decoded = decodeMidgardCekConstantWitnessV1(
          semantic.witness.result.witness,
        );
        expect(decoded.payload).toMatchObject({ bytes: expect.anything() });
        if (decoded.payload instanceof DataB) {
          expect(decoded.payload.bytes.toBuffer()).toEqual(Buffer.from([0x80]));
        }
      }
      expect(semantic.post.cpu - semantic.pre.cpu).toBe(1_808_754n);
      expect(semantic.post.memory - semantic.pre.memory).toBe(8n);
    }
    expect(execution.terminalState.mode).toBe("haltSuccess");
    expect(
      execution.steps.every((step) =>
        verifyMidgardCekCoreStepV1(step.pre, step.post, step.witness),
      ),
    ).toBe(true);
  });

  it("proves nested list memory from the exact bounded source constant", () => {
    const nested = UPLCConst.listOf(constT.listOf(constT.int))([
      [1n, 2n],
      [3n],
    ]);
    const program = buildMidgardCanonicalCekProgramV1(
      compile(
        new Application(
          Builtin.headList,
          nested,
        ),
      ),
    );
    const execution = executeMidgardCekStructuralProgramV1({
      root: program.envelope.termRoot,
      material: program.material.values(),
      constantWitnesses: program.constantWitnesses,
      maxSteps: 32,
    });
    expect(execution.terminalState.mode).toBe("haltSuccess");
    expect(
      execution.steps.filter(
        (candidate) =>
          candidate.witness.kind === "returnForceBuiltin",
      ),
    ).toHaveLength(1);
    const step = execution.steps.find(
      (candidate) =>
        candidate.witness.kind === "executeBuiltinSemantic",
    );
    expect(step?.witness.kind).toBe("executeBuiltinSemantic");
    if (step?.witness.kind === "executeBuiltinSemantic") {
      expect(step.witness.result.kind).toBe("constant");
      if (step.witness.result.kind === "constant") {
        expect(
          decodeMidgardCekConstantWitnessV1(
            step.witness.result.witness,
          ).payload,
        ).toMatchObject({ list: [{ int: 1n }, { int: 2n }] });
      }
    }
  });

  it("proves unMapData one map pair per semantic CEK step", () => {
    const program = buildMidgardCanonicalCekProgramV1(
      compile(
        new Lambda(
          new Application(Builtin.unMapData, new UPLCVar(0)),
        ),
      ),
    );
    const context = new DataMap<Data, Data>([
      new DataPair(new DataI(1n), new DataB(Buffer.alloc(9_000, 0x2a))),
      new DataPair(new DataI(2n), new DataI(3n)),
    ]);
    const graph = buildMidgardCekExecutionGraphV1(
      program.envelope,
      program.material.values(),
      encodeMidgardCekPlutusDataV1(context),
    );
    const execution = executeMidgardCekStructuralProgramV1({
      root: graph.root,
      material: graph.material.values(),
      constantWitnesses: graph.constantWitnesses,
      maxSteps: 64,
    });
    const semanticKinds = execution.steps
      .map((step) => step.witness.kind)
      .filter((kind) => kind.includes("MapConversion") || kind.includes("MapToList"));
    expect(semanticKinds).toEqual([
      "startBuiltinMapConversion",
      "stepBuiltinMapToList",
      "stepBuiltinMapToList",
      "finishBuiltinMapConversion",
    ]);
    expect(execution.terminalState.mode).toBe("haltSuccess");
    expect(
      execution.steps.every((step) =>
        verifyMidgardCekCoreStepV1(step.pre, step.post, step.witness),
      ),
    ).toBe(true);
  });

  it("proves large structured Data traversal without revealing the whole constant", () => {
    const program = buildMidgardCanonicalCekProgramV1(
      compile(
        new Lambda(
          new Application(Builtin.unConstrData, new UPLCVar(0)),
        ),
      ),
    );
    const context = new DataConstr(128n, [
      new DataB(Buffer.alloc(9_000, 0x4d)),
    ]);
    const contextCbor = encodeMidgardCekPlutusDataV1(context);
    expect(contextCbor.length).toBeGreaterThan(9_000);
    const graph = buildMidgardCekExecutionGraphV1(
      program.envelope,
      program.material.values(),
      contextCbor,
    );
    const execution = executeMidgardCekStructuralProgramV1({
      root: graph.root,
      material: graph.material.values(),
      constantWitnesses: graph.constantWitnesses,
      maxSteps: 64,
    });
    const semantic = execution.steps.find(
      (step) => step.witness.kind === "executeBuiltinSemantic",
    );
    expect(semantic?.witness.kind).toBe("executeBuiltinSemantic");
    if (semantic?.witness.kind === "executeBuiltinSemantic") {
      expect(semantic.witness.tag).toBe(42n);
      expect(semantic.witness.material.dataNodes).toHaveLength(1);
      expect(semantic.witness.material.scalarPreimages).toHaveLength(1);
      expect(
        semantic.witness.material.scalarPreimages[0]?.length,
      ).toBeLessThan(16);
    }
    expect(execution.terminalState.mode).toBe("haltSuccess");
    expect(
      execution.steps.every((step) =>
        verifyMidgardCekCoreStepV1(step.pre, step.post, step.witness),
      ),
    ).toBe(true);
  });

  it("proves a wrong semantic Data variant as a zero-budget CEK failure", () => {
    const program = buildMidgardCanonicalCekProgramV1(
      compile(
        new Lambda(
          new Application(Builtin.unConstrData, new UPLCVar(0)),
        ),
      ),
    );
    const context = new DataMap<Data, Data>([
      new DataPair(
        new DataI(1n),
        new DataB(Buffer.alloc(9_000, 0x71)),
      ),
    ]);
    const graph = buildMidgardCekExecutionGraphV1(
      program.envelope,
      program.material.values(),
      encodeMidgardCekPlutusDataV1(context),
    );
    const execution = executeMidgardCekStructuralProgramV1({
      root: graph.root,
      material: graph.material.values(),
      constantWitnesses: graph.constantWitnesses,
      maxSteps: 64,
    });
    const failure = execution.steps.at(-1);
    expect(failure?.witness.kind).toBe(
      "executeBuiltinSemanticFailure",
    );
    expect(execution.terminalState.mode).toBe("haltError");
    expect(failure?.post.cpu).toBe(failure?.pre.cpu);
    expect(failure?.post.memory).toBe(failure?.pre.memory);
    expect(
      failure === undefined
        ? false
        : verifyMidgardCekCoreStepV1(
            failure.pre,
            failure.post,
            failure.witness,
          ),
    ).toBe(true);
  });

  it("carries BLS expression provenance through finalVerify", () => {
    const g1Bytes = UPLCConst.byteString(
      new DataB(
        Buffer.from(
          "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
          "hex",
        ),
      ).bytes,
    );
    const g2Bytes = UPLCConst.byteString(
      new DataB(
        Buffer.from(
          "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8",
          "hex",
        ),
      ).bytes,
    );
    const millerLoop = (): UPLCTerm =>
      new Application(
        new Application(
          Builtin.bls12_381_millerLoop,
          new Application(Builtin.bls12_381_G1_uncompress, g1Bytes),
        ),
        new Application(Builtin.bls12_381_G2_uncompress, g2Bytes),
      );
    const program = buildMidgardCanonicalCekProgramV1(
      compile(
        new Lambda(
          new Application(
            new Application(
              Builtin.bls12_381_finalVerify,
              millerLoop(),
            ),
            millerLoop(),
          ),
        ),
      ),
    );
    const graph = buildMidgardCekExecutionGraphV1(
      program.envelope,
      program.material.values(),
      Buffer.from("d87980", "hex"),
    );
    const execution = executeMidgardCekStructuralProgramV1({
      root: graph.root,
      material: graph.material.values(),
      constantWitnesses: graph.constantWitnesses,
      maxSteps: 256,
    });
    const kinds = execution.steps.map((step) => step.witness.kind);
    expect(kinds.join(",")).toContain("executeBuiltinBlsFinal");
    expect(execution.terminalState.mode).toBe("haltSuccess");
    expect(
      execution.steps.every((step) =>
        verifyMidgardCekCoreStepV1(step.pre, step.post, step.witness),
      ),
    ).toBe(true);
  });
});
