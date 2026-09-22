import {
  hashMidgardCekContinuationFrame,
  hashMidgardCekSequenceNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  MidgardCekMachineModes,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  hashMidgardCekRuntimeArguments,
  type MidgardCekRuntimeValueWitness,
} from "../src/cek-builtin.js";
import {
  MidgardCekErrorCodes,
  verifyMidgardCekCoreStep,
} from "../src/cek-machine.js";

const hash = (fill: number): Buffer => Buffer.alloc(32, fill);

const state = (
  mode: MidgardCekMachineState["mode"],
  focusRoot: Uint8Array,
  environmentRoot: Uint8Array,
  continuationRoot: Uint8Array,
  auxiliary: bigint,
  cpu: bigint,
  memory: bigint,
): MidgardCekMachineState => ({
  mode,
  executionIndex: 0n,
  focusRoot,
  environmentRoot,
  continuationRoot,
  auxiliary,
  cpu,
  memory,
});

describe("V1 structural CEK machine", () => {
  it("pins every shared machine-mode and terminal-error tag", () => {
    expect(Object.values(MidgardCekMachineModes)).toEqual([
      0n,
      1n,
      2n,
      3n,
      4n,
      5n,
      6n,
      7n,
      8n,
    ]);
    expect(Object.values(MidgardCekErrorCodes)).toEqual([
      0n,
      1n,
      2n,
      3n,
      4n,
      5n,
      6n,
      7n,
    ]);
  });

  it("matches the Aiken application transition and rejects a budget drift", () => {
    const application = hashMidgardCekTermNode({
      kind: "application",
      function: hash(1),
      argument: hash(2),
    });
    const pre = state(
      "compute",
      application,
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
      0n,
      100n,
      100n,
    );
    const continuation = hashMidgardCekContinuationFrame({
      kind: "applyArgument",
      argument: hash(2),
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
    });
    const post = state(
      "compute",
      hash(1),
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      continuation,
      0n,
      16_100n,
      200n,
    );
    expect(
      verifyMidgardCekCoreStep(pre, post, {
        kind: "computeApplication",
        function: hash(1),
        argument: hash(2),
      }),
    ).toBe(true);
    expect(
      verifyMidgardCekCoreStep(
        pre,
        { ...post, cpu: post.cpu - 1n },
        {
          kind: "computeApplication",
          function: hash(1),
          argument: hash(2),
        },
      ),
    ).toBe(false);
  });

  it("streams constructor fields and case arguments in the same order as L1", () => {
    const empty = MIDGARD_CEK_EMPTY_SEQUENCE_ROOT;
    const firstTerm = hashMidgardCekTermNode({
      kind: "constant",
      value: hash(1),
    });
    const secondTerm = hashMidgardCekTermNode({
      kind: "constant",
      value: hash(2),
    });
    const termsTail = hashMidgardCekSequenceNode({
      head: secondTerm,
      tail: empty,
      length: 1n,
    });
    const termsRoot = hashMidgardCekSequenceNode({
      head: firstTerm,
      tail: termsTail,
      length: 2n,
    });
    const firstFrame = hashMidgardCekContinuationFrame({
      kind: "constr",
      tag: 7n,
      remainingTermsCount: 1n,
      remainingTermsRoot: termsTail,
      valuesCount: 0n,
      valuesRoot: empty,
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
    });
    const pre = state(
      "compute",
      hashMidgardCekTermNode({
        kind: "constr",
        tag: 7n,
        termsCount: 2n,
        termsRoot,
      }),
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
      0n,
      100n,
      100n,
    );
    const computingFirst = state(
      "compute",
      firstTerm,
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      firstFrame,
      0n,
      16_100n,
      200n,
    );
    expect(
      verifyMidgardCekCoreStep(pre, computingFirst, {
        kind: "computeConstrNonempty",
        tag: 7n,
        termsCount: 2n,
        firstTerm,
        remainingTermsRoot: termsTail,
      }),
    ).toBe(true);

    const firstValues = hashMidgardCekSequenceNode({
      head: hash(1),
      tail: empty,
      length: 1n,
    });
    const finalFrame = hashMidgardCekContinuationFrame({
      kind: "constr",
      tag: 7n,
      remainingTermsCount: 0n,
      remainingTermsRoot: empty,
      valuesCount: 1n,
      valuesRoot: firstValues,
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
    });
    expect(
      verifyMidgardCekCoreStep(
        state(
          "return",
          hash(1),
          MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          firstFrame,
          0n,
          16_100n,
          200n,
        ),
        state(
          "compute",
          secondTerm,
          MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          finalFrame,
          0n,
          16_100n,
          200n,
        ),
        {
          kind: "returnConstrNext",
          tag: 7n,
          remainingTermsCount: 1n,
          nextTerm: secondTerm,
          remainingTermsTail: empty,
          valuesCount: 0n,
          valuesRoot: empty,
          capturedEnvironment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
        },
      ),
    ).toBe(true);

    const valuesRoot = hashMidgardCekSequenceNode({
      head: hash(2),
      tail: firstValues,
      length: 2n,
    });
    const constrValue = hashMidgardCekValueNode({
      kind: "constr",
      tag: 7n,
      valuesCount: 2n,
      valuesRoot,
    });
    expect(
      verifyMidgardCekCoreStep(
        state(
          "return",
          hash(2),
          MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          finalFrame,
          0n,
          16_100n,
          200n,
        ),
        state(
          "return",
          constrValue,
          MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
          0n,
          16_100n,
          200n,
        ),
        {
          kind: "returnConstrDone",
          tag: 7n,
          valuesCount: 1n,
          valuesRoot: firstValues,
          capturedEnvironment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
        },
      ),
    ).toBe(true);

    const branchTail = hashMidgardCekSequenceNode({
      head: hash(4),
      tail: empty,
      length: 1n,
    });
    const branchesRoot = hashMidgardCekSequenceNode({
      head: hash(3),
      tail: branchTail,
      length: 2n,
    });
    const caseFrame = hashMidgardCekContinuationFrame({
      kind: "case",
      branchesCount: 2n,
      branchesRoot,
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
    });
    const selectWork = hashMidgardCekContinuationFrame({
      kind: "caseSelect",
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
      valuesCount: 2n,
    });
    const selecting = state(
      "caseSelect",
      branchesRoot,
      valuesRoot,
      selectWork,
      1n,
      16_100n,
      200n,
    );
    expect(
      verifyMidgardCekCoreStep(
        state(
          "return",
          hashMidgardCekValueNode({
            kind: "constr",
            tag: 1n,
            valuesCount: 2n,
            valuesRoot,
          }),
          MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          caseFrame,
          0n,
          16_100n,
          200n,
        ),
        selecting,
        {
          kind: "returnCaseConstr",
          tag: 1n,
          valuesCount: 2n,
          valuesRoot,
          branchesCount: 2n,
          branchesRoot,
          capturedEnvironment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
        },
      ),
    ).toBe(true);

    const selected = state(
      "caseSelect",
      branchTail,
      valuesRoot,
      selectWork,
      0n,
      16_100n,
      200n,
    );
    expect(
      verifyMidgardCekCoreStep(selecting, selected, {
        kind: "selectCaseBranch",
        branch: hash(3),
        remainingBranchesRoot: branchTail,
        length: 2n,
        capturedEnvironment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
        tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
        valuesCount: 2n,
      }),
    ).toBe(true);

    const applying = state(
      "caseApply",
      valuesRoot,
      hash(4),
      hashMidgardCekContinuationFrame({
        kind: "caseApply",
        environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
        builtContinuation: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
      }),
      2n,
      16_100n,
      200n,
    );
    expect(
      verifyMidgardCekCoreStep(selected, applying, {
        kind: "selectCaseBranch",
        branch: hash(4),
        remainingBranchesRoot: empty,
        length: 1n,
        capturedEnvironment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
        tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
        valuesCount: 2n,
      }),
    ).toBe(true);
  });

  it("leaves builtin execution closed to its tag-specific micro-machine", () => {
    const pre = state(
      "builtin",
      hash(1),
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
      0n,
      0n,
      0n,
    );
    expect(
      verifyMidgardCekCoreStep(pre, pre, {
        kind: "computeBuiltin",
        tag: 0n,
      }),
    ).toBe(false);
  });

  it("halts an authenticated builtin runtime-type failure without charging", () => {
    const arguments_: readonly MidgardCekRuntimeValueWitness[] = [
      {
        kind: "lambda",
        body: hash(2),
        environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      },
      {
        kind: "constant",
        witness: {
          typeCbor: Buffer.from("9f00ff", "hex"),
          payloadCbor: Buffer.from("01", "hex"),
        },
      },
    ];
    const { root, count } = hashMidgardCekRuntimeArguments(arguments_);
    const pre = state(
      "builtin",
      hashMidgardCekValueNode({
        kind: "builtin",
        tag: 0n,
        forcesRemaining: 0n,
        argumentsCount: count,
        argumentsRoot: root,
      }),
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      hash(1),
      0n,
      100n,
      100n,
    );
    const post = state(
      "haltError",
      hashMidgardCekTermNode({ kind: "error" }),
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
      7n,
      100n,
      100n,
    );
    expect(
      verifyMidgardCekCoreStep(pre, post, {
        kind: "executeBuiltinTypeFailure",
        tag: 0n,
        arguments: arguments_,
      }),
    ).toBe(true);
  });
});
