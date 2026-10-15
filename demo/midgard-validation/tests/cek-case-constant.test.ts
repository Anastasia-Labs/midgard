import {
  hashMidgardCekBlobChunk,
  hashMidgardCekSequenceNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  type MidgardCekMachineState,
  type MidgardCekValueNode,
} from "@al-ft/midgard-core";
import { type Data, DataConstr, DataI } from "@harmoniclabs/plutus-data";
import {
  Case,
  Constr,
  Lambda,
  UPLCConst,
  UPLCProgram,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  decodeMidgardCekConstantWitness,
  encodeMidgardCekConstantTypeCbor,
  encodeMidgardCekPlutusData,
  midgardCekConstantMemorySize,
  type MidgardCekConstantType,
} from "../src/cek-constant.js";
import { commitMidgardCekDataTree } from "../src/cek-data-tree.js";
import {
  errorSuccessor,
  exactState,
} from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
  type MidgardCekExecutionStep,
  type MidgardCekStructuralExecution,
} from "../src/cek-executor.js";
import {
  type MidgardCekCoreStepWitness,
  verifyMidgardCekCoreStep,
} from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { buildMidgardCanonicalCekProgram } from "../src/cek-program.js";
import { type CaseConstantCorpusEntry } from "./fixtures/cek-case-constant-v1.corpus.js";
import { CASE_CONSTANT_CORPUS } from "./fixtures/cek-case-constant-v1.edges.js";

/** A closed program runs under `lam ctx`: application, lambda, context. */
const CONTEXT_PRELUDE_CPU = 3n * 16_000n;
const CONTEXT_PRELUDE_MEMORY = 3n * 100n;

const run = (
  entry: Pick<CaseConstantCorpusEntry, "term" | "context">,
): MidgardCekStructuralExecution => {
  const program = buildMidgardCanonicalCekProgram(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram([1, 1, 0], new Lambda(entry.term())),
    ),
  );
  const graph = buildMidgardCekExecutionGraph(
    program.envelope,
    program.material.values(),
    encodeMidgardCekPlutusData(entry.context?.() ?? new DataConstr(0n, [])),
  );
  return executeMidgardCekStructuralProgram({
    root: graph.root,
    material: graph.material.values(),
    constantWitnesses: graph.constantWitnesses,
    maxSteps: 64,
  });
};

const constantRoot = (type: MidgardCekConstantType, payload: Data) => {
  const tree = commitMidgardCekDataTree(payload);
  return hashMidgardCekValueNode({
    kind: "constant",
    typeRoot: hashMidgardCekBlobChunk(encodeMidgardCekConstantTypeCbor(type)),
    payloadRoot: tree.root,
    payloadLength: tree.cborLength,
    semanticRoot: tree.root,
    memory: midgardCekConstantMemorySize(type, payload),
  });
};

const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

/** The machine value a case-on-constant scrutinee witness opens to. */
const scrutineeNode = (
  scrutinee: Extract<
    MidgardCekCoreStepWitness,
    { readonly kind: "executeBuiltinSemantic" }
  >["result"],
): MidgardCekValueNode => {
  if (scrutinee.kind === "constant") {
    const decoded = decodeMidgardCekConstantWitness(scrutinee.witness);
    const tree = commitMidgardCekDataTree(decoded.payload);
    return {
      kind: "constant",
      typeRoot: hashMidgardCekBlobChunk(scrutinee.witness.typeCbor),
      payloadRoot: tree.root,
      payloadLength: tree.cborLength,
      semanticRoot: tree.root,
      memory: midgardCekConstantMemorySize(decoded.type, decoded.payload),
    };
  }
  if (scrutinee.kind === "semanticConstant") {
    return {
      kind: "constant",
      typeRoot: hashMidgardCekBlobChunk(scrutinee.witness.typeCbor),
      payloadRoot: scrutinee.witness.payload.root,
      payloadLength: scrutinee.witness.payload.cborLength,
      semanticRoot: scrutinee.witness.payload.root,
      memory: scrutinee.witness.memory,
    };
  }
  throw new Error("a case-on-constant scrutinee is a constant");
};

const refuses = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
  witness: MidgardCekCoreStepWitness,
): boolean => !verifyMidgardCekCoreStep(pre, post, witness);

const caseStep = (trace: MidgardCekStructuralExecution) =>
  trace.steps.find(
    (step) =>
      step.witness.kind === "returnCaseConstant" ||
      step.witness.kind === "returnCaseInvalid",
  );

const executionCache = new Map<string, MidgardCekStructuralExecution>();
const execution = (label: string): MidgardCekStructuralExecution => {
  const cached = executionCache.get(label);
  if (cached !== undefined) return cached;
  const entry = CASE_CONSTANT_CORPUS.find(
    (candidate) => candidate.label === label,
  );
  if (entry === undefined) throw new Error(`no corpus entry ${label}`);
  const result = run(entry);
  executionCache.set(label, result);
  return result;
};

describe("case on a constant (protocol version 11)", () => {
  it.each(CASE_CONSTANT_CORPUS.map((entry) => [entry.label, entry] as const))(
    "%s matches the reference evaluator",
    (_, entry) => {
      const trace = execution(entry.label);
      for (const step of trace.steps) {
        expect(
          verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
        ).toBe(true);
      }
      const scrutinee = caseStep(trace);
      expect(scrutinee?.witness.kind).toBe(
        entry.label === "delay" ? "returnCaseInvalid" : "returnCaseConstant",
      );
      const terminal = trace.terminalState;
      const expected = entry.expected;
      if (expected.kind === "failure") {
        expect(terminal.mode).toBe("haltError");
        expect(terminal.auxiliary).toBe(expected.errorCode);
        return;
      }
      const halt = trace.steps.at(-1)!;
      if (halt.witness.kind !== "returnEmptyContinuation") {
        throw new Error(`${entry.label} did not return a value`);
      }
      const value = halt.witness.value;
      if (expected.kind === "constant") {
        expect(hex(hashMidgardCekValueNode(value))).toBe(
          hex(constantRoot(expected.type, expected.payload)),
        );
      } else {
        expect(value).toMatchObject({ kind: "builtin", tag: expected.tag });
      }
      const prelude = entry.context === undefined;
      expect(terminal.cpu).toBe(
        expected.cpu + (prelude ? CONTEXT_PRELUDE_CPU : 0n),
      );
      expect(terminal.memory).toBe(
        expected.memory + (prelude ? CONTEXT_PRELUDE_MEMORY : 0n),
      );
    },
  );

  it("reaches the same verdict without per-step evidence", () => {
    for (const entry of CASE_CONSTANT_CORPUS) {
      const program = buildMidgardCanonicalCekProgram(
        encodeMidgardCekCardanoFlatProgram(
          new UPLCProgram([1, 1, 0], new Lambda(entry.term())),
        ),
      );
      const graph = buildMidgardCekExecutionGraph(
        program.envelope,
        program.material.values(),
        encodeMidgardCekPlutusData(entry.context?.() ?? new DataConstr(0n, [])),
      );
      const verdict = executeMidgardCekStructuralProgram({
        root: graph.root,
        material: graph.material.values(),
        constantWitnesses: graph.constantWitnesses,
        maxSteps: 64,
        mode: "verdict",
      });
      expect(verdict.terminalState).toEqual(
        execution(entry.label).terminalState,
      );
    }
  });
});

type CaseConstantStep = MidgardCekExecutionStep & {
  readonly witness: Extract<
    MidgardCekCoreStepWitness,
    { readonly kind: "returnCaseConstant" }
  >;
};

const caseConstantStep = (label: string): CaseConstantStep => {
  const step = caseStep(execution(label));
  if (step?.witness.kind !== "returnCaseConstant") {
    throw new Error(`${label} has no case-on-constant step`);
  }
  return step as CaseConstantStep;
};

const caseConstantLabels = CASE_CONSTANT_CORPUS.filter(
  (entry) => entry.label !== "delay",
).map((entry) => entry.label);

/** The successor a step would have if it took branch `tag` with no values. */
const branchSuccessor = (step: CaseConstantStep, tag: bigint) =>
  exactState(step.pre, {
    mode: "caseSelect",
    focusRoot: step.witness.branchesRoot,
    environmentRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
    continuationRoot: step.post.continuationRoot,
    auxiliary: tag,
  });

describe("case branch selection", () => {
  // False, an empty list and every integer below the last branch select a
  // branch before the last one; the walk to it is shared with constructors.
  it("selects an earlier branch of several", () => {
    const trace = run({
      term: () =>
        new Case(new Constr(0n, []), [UPLCConst.int(7), UPLCConst.int(8)]),
    });
    for (const step of trace.steps) {
      expect(verifyMidgardCekCoreStep(step.pre, step.post, step.witness)).toBe(
        true,
      );
    }
    const halt = trace.steps.at(-1)!;
    if (halt.witness.kind !== "returnEmptyContinuation") {
      throw new Error("the constructor case did not return a value");
    }
    expect(hex(hashMidgardCekValueNode(halt.witness.value))).toBe(
      hex(constantRoot({ kind: "integer" }, new DataI(7n))),
    );
  });
});

describe("case on a constant refuses every other successor", () => {
  it.each(caseConstantLabels)("%s: wrong successors are refused", (label) => {
    const { pre, post, witness } = caseConstantStep(label);
    expect(verifyMidgardCekCoreStep(pre, post, witness)).toBe(true);
    // Each other outcome: the two halts and a different branch.
    for (const reason of [5n, 6n, 7n]) {
      const halt = errorSuccessor(pre, reason);
      if (post.mode !== "haltError" || post.auxiliary !== reason) {
        expect(refuses(pre, halt, witness)).toBe(true);
      }
    }
    expect(
      refuses(pre, { ...post, auxiliary: post.auxiliary + 1n }, witness),
    ).toBe(true);
    expect(refuses(pre, { ...post, cpu: post.cpu + 1n }, witness)).toBe(true);
    if (post.mode === "haltError") {
      expect(
        refuses(pre, branchSuccessor({ pre, post, witness }, 0n), witness),
      ).toBe(true);
    } else {
      expect(
        refuses(pre, { ...post, environmentRoot: pre.focusRoot }, witness),
      ).toBe(true);
      expect(refuses(pre, { ...post, focusRoot: pre.focusRoot }, witness)).toBe(
        true,
      );
    }
    // The frame and the scrutinee are bound to the pre-state.
    for (const delta of [-1n, 1n]) {
      expect(
        refuses(pre, post, {
          ...witness,
          branchesCount: witness.branchesCount + delta,
        }),
      ).toBe(true);
    }
    expect(refuses(pre, post, { ...witness, tail: witness.branchesRoot })).toBe(
      true,
    );
    expect(
      refuses(pre, post, {
        ...witness,
        scrutinee: {
          kind: "constant",
          witness: {
            typeCbor: Buffer.from("9f00ff", "hex"),
            payloadCbor: Buffer.from("182a", "hex"),
          },
        },
      }),
    ).toBe(true);
    expect(
      refuses(pre, post, {
        ...witness,
        scrutinee: { kind: "opaque", root: pre.focusRoot },
      }),
    ).toBe(true);
  });

  it("applies a list's head before its tail and a pair's first component first", () => {
    for (const label of [
      "list-01",
      "list-tail",
      "pair-01",
      "wide-list-tail",
      "wide-pair-second",
    ]) {
      const { pre, post, witness } = caseConstantStep(label);
      expect(post.mode).toBe("caseSelect");
      expect(post.auxiliary).toBe(0n);
      // Swapping the two values gives a different, refused sequence.
      const trace = execution(label);
      const select = trace.steps.find(
        (step) => step.witness.kind === "applyCaseValue",
      );
      if (select?.witness.kind !== "applyCaseValue") {
        throw new Error(`${label} applies no case value`);
      }
      const last = select.witness.value;
      const firstApplied = select.witness.remainingValuesRoot;
      const swapped = hashMidgardCekSequenceNode({
        head: firstApplied,
        tail: hashMidgardCekSequenceNode({
          head: last,
          tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          length: 1n,
        }),
        length: 2n,
      });
      expect(refuses(pre, { ...post, environmentRoot: swapped }, witness)).toBe(
        true,
      );
    }
  });

  it("refuses a wide list or pair without its material", () => {
    for (const label of [
      "wide-list-head",
      "wide-list-tail",
      "wide-pair-first",
      "wide-pair-second",
    ]) {
      const { pre, post, witness } = caseConstantStep(label);
      expect(witness.scrutinee.kind).toBe("semanticConstant");
      expect(witness.material.dataNodes.length).toBeGreaterThan(0);
      expect(
        refuses(pre, post, {
          ...witness,
          material: { ...witness.material, dataNodes: [] },
        }),
      ).toBe(true);
      expect(
        refuses(pre, post, {
          ...witness,
          material: {
            ...witness.material,
            dataNodes: [...witness.material.dataNodes].reverse(),
          },
        }),
      ).toBe(true);
    }
  });

  it("halts on a committed integer of two or more words with no branch", () => {
    const integer = caseConstantStep("integer-02");
    const { pre, witness } = integer;
    const wide = (memory: bigint) => {
      const scrutinee = {
        kind: "semanticConstant",
        witness: {
          typeCbor: encodeMidgardCekConstantTypeCbor({ kind: "integer" }),
          payload: {
            root: Buffer.alloc(32, 2),
            cborLength: 9300n,
            memory: 4n + memory,
          },
          memory,
        },
      } as const;
      const at = {
        ...pre,
        focusRoot: hashMidgardCekValueNode(scrutineeNode(scrutinee)),
      };
      return { at, step: { ...witness, scrutinee } };
    };
    const twoWords = wide(2n);
    expect(
      verifyMidgardCekCoreStep(
        twoWords.at,
        errorSuccessor(twoWords.at, 6n),
        twoWords.step,
      ),
    ).toBe(true);
    for (const tag of [0n, 1n]) {
      expect(
        refuses(
          twoWords.at,
          branchSuccessor({ ...integer, pre: twoWords.at }, tag),
          twoWords.step,
        ),
      ).toBe(true);
    }
    expect(
      refuses(twoWords.at, errorSuccessor(twoWords.at, 5n), twoWords.step),
    ).toBe(true);
    // The executor proves an integer of two words or more out of range from
    // its committed form, which never decodes the integer.
    for (const label of [
      "integer-two-to-the-64",
      "integer-minus-two-to-the-64",
    ]) {
      const step = caseConstantStep(label);
      expect(step.witness.scrutinee.kind).toBe("semanticConstant");
      expect(step.post).toEqual(errorSuccessor(step.pre, 6n));
    }
    // One word is always carried inline, so its committed form is refused.
    const oneWord = wide(1n);
    expect(
      refuses(oneWord.at, errorSuccessor(oneWord.at, 6n), oneWord.step),
    ).toBe(true);
  });

  it("refuses the invalid-scrutinee arm for a constant", () => {
    for (const entry of CASE_CONSTANT_CORPUS) {
      const step = caseStep(execution(entry.label))!;
      const witness = step.witness;
      if (
        witness.kind !== "returnCaseConstant" &&
        witness.kind !== "returnCaseInvalid"
      ) {
        throw new Error("unreachable");
      }
      const value =
        witness.kind === "returnCaseInvalid"
          ? witness.value
          : scrutineeNode(witness.scrutinee);
      expect(hex(hashMidgardCekValueNode(value))).toBe(hex(step.pre.focusRoot));
      const invalid: MidgardCekCoreStepWitness = {
        kind: "returnCaseInvalid",
        value,
        branchesCount: witness.branchesCount,
        branchesRoot: witness.branchesRoot,
        capturedEnvironment: witness.capturedEnvironment,
        tail: witness.tail,
      };
      expect(
        verifyMidgardCekCoreStep(
          step.pre,
          errorSuccessor(step.pre, 5n),
          invalid,
        ),
      ).toBe(value.kind !== "constant");
    }
  });
});
