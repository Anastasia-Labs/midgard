import {
  hashMidgardCekContinuationFrameV1,
  hashMidgardCekSequenceNodeV1,
  hashMidgardCekTermNodeV1,
  hashMidgardCekValueNodeV1,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
  type MidgardCekMachineStateV1,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  hashMidgardCekRuntimeArgumentsV1,
  type MidgardCekRuntimeValueWitnessV1,
} from "../src/cek-builtin.js";
import { verifyMidgardCekCoreStepV1 } from "../src/cek-machine.js";

const hash = (fill: number): Buffer => Buffer.alloc(32, fill);

const state = (
  mode: MidgardCekMachineStateV1["mode"],
  focusRoot: Uint8Array,
  environmentRoot: Uint8Array,
  continuationRoot: Uint8Array,
  auxiliary: bigint,
  cpu: bigint,
  memory: bigint,
): MidgardCekMachineStateV1 => ({
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
  it("matches the Aiken application transition and rejects a budget drift", () => {
    const application = hashMidgardCekTermNodeV1({
      kind: "application",
      function: hash(1),
      argument: hash(2),
    });
    const pre = state(
      "compute",
      application,
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
      0n,
      100n,
      100n,
    );
    const continuation = hashMidgardCekContinuationFrameV1({
      kind: "applyArgument",
      argument: hash(2),
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
    });
    const post = state(
      "compute",
      hash(1),
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      continuation,
      0n,
      16_100n,
      200n,
    );
    expect(
      verifyMidgardCekCoreStepV1(pre, post, {
        kind: "computeApplication",
        function: hash(1),
        argument: hash(2),
      }),
    ).toBe(true);
    expect(
      verifyMidgardCekCoreStepV1(
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
    const empty = MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1;
    const firstTerm = hashMidgardCekTermNodeV1({
      kind: "constant",
      value: hash(1),
    });
    const secondTerm = hashMidgardCekTermNodeV1({
      kind: "constant",
      value: hash(2),
    });
    const termsTail = hashMidgardCekSequenceNodeV1({
      head: secondTerm,
      tail: empty,
      length: 1n,
    });
    const termsRoot = hashMidgardCekSequenceNodeV1({
      head: firstTerm,
      tail: termsTail,
      length: 2n,
    });
    const firstFrame = hashMidgardCekContinuationFrameV1({
      kind: "constr",
      tag: 7n,
      remainingTermsCount: 1n,
      remainingTermsRoot: termsTail,
      valuesCount: 0n,
      valuesRoot: empty,
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
    });
    const pre = state(
      "compute",
      hashMidgardCekTermNodeV1({
        kind: "constr",
        tag: 7n,
        termsCount: 2n,
        termsRoot,
      }),
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
      0n,
      100n,
      100n,
    );
    const computingFirst = state(
      "compute",
      firstTerm,
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      firstFrame,
      0n,
      16_100n,
      200n,
    );
    expect(
      verifyMidgardCekCoreStepV1(pre, computingFirst, {
        kind: "computeConstrNonempty",
        tag: 7n,
        termsCount: 2n,
        firstTerm,
        remainingTermsRoot: termsTail,
      }),
    ).toBe(true);

    const firstValues = hashMidgardCekSequenceNodeV1({
      head: hash(1),
      tail: empty,
      length: 1n,
    });
    const finalFrame = hashMidgardCekContinuationFrameV1({
      kind: "constr",
      tag: 7n,
      remainingTermsCount: 0n,
      remainingTermsRoot: empty,
      valuesCount: 1n,
      valuesRoot: firstValues,
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
    });
    expect(
      verifyMidgardCekCoreStepV1(
        state(
          "return",
          hash(1),
          MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
          firstFrame,
          0n,
          16_100n,
          200n,
        ),
        state(
          "compute",
          secondTerm,
          MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
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
          capturedEnvironment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
          tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
        },
      ),
    ).toBe(true);

    const valuesRoot = hashMidgardCekSequenceNodeV1({
      head: hash(2),
      tail: firstValues,
      length: 2n,
    });
    const constrValue = hashMidgardCekValueNodeV1({
      kind: "constr",
      tag: 7n,
      valuesCount: 2n,
      valuesRoot,
    });
    expect(
      verifyMidgardCekCoreStepV1(
        state(
          "return",
          hash(2),
          MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
          finalFrame,
          0n,
          16_100n,
          200n,
        ),
        state(
          "return",
          constrValue,
          MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
          MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
          0n,
          16_100n,
          200n,
        ),
        {
          kind: "returnConstrDone",
          tag: 7n,
          valuesCount: 1n,
          valuesRoot: firstValues,
          capturedEnvironment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
          tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
        },
      ),
    ).toBe(true);

    const branchTail = hashMidgardCekSequenceNodeV1({
      head: hash(4),
      tail: empty,
      length: 1n,
    });
    const branchesRoot = hashMidgardCekSequenceNodeV1({
      head: hash(3),
      tail: branchTail,
      length: 2n,
    });
    const caseFrame = hashMidgardCekContinuationFrameV1({
      kind: "case",
      branchesCount: 2n,
      branchesRoot,
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
    });
    const selectWork = hashMidgardCekContinuationFrameV1({
      kind: "caseSelect",
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
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
      verifyMidgardCekCoreStepV1(
        state(
          "return",
          hashMidgardCekValueNodeV1({
            kind: "constr",
            tag: 1n,
            valuesCount: 2n,
            valuesRoot,
          }),
          MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
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
          capturedEnvironment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
          tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
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
      verifyMidgardCekCoreStepV1(selecting, selected, {
        kind: "selectCaseBranch",
        branch: hash(3),
        remainingBranchesRoot: branchTail,
        length: 2n,
        capturedEnvironment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
        tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
        valuesCount: 2n,
      }),
    ).toBe(true);

    const applying = state(
      "caseApply",
      valuesRoot,
      hash(4),
      hashMidgardCekContinuationFrameV1({
        kind: "caseApply",
        environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
        builtContinuation: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
      }),
      2n,
      16_100n,
      200n,
    );
    expect(
      verifyMidgardCekCoreStepV1(selected, applying, {
        kind: "selectCaseBranch",
        branch: hash(4),
        remainingBranchesRoot: empty,
        length: 1n,
        capturedEnvironment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
        tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
        valuesCount: 2n,
      }),
    ).toBe(true);
  });

  it("leaves builtin execution closed to its tag-specific micro-machine", () => {
    const pre = state(
      "builtin",
      hash(1),
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
      0n,
      0n,
      0n,
    );
    expect(
      verifyMidgardCekCoreStepV1(pre, pre, {
        kind: "computeBuiltin",
        tag: 0n,
      }),
    ).toBe(false);
  });

  it("halts an authenticated builtin runtime-type failure without charging", () => {
    const arguments_: readonly MidgardCekRuntimeValueWitnessV1[] = [
      {
        kind: "lambda",
        body: hash(2),
        environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      },
      {
        kind: "constant",
        witness: {
          typeCbor: Buffer.from("9f00ff", "hex"),
          payloadCbor: Buffer.from("01", "hex"),
        },
      },
    ];
    const { root, count } =
      hashMidgardCekRuntimeArgumentsV1(arguments_);
    const pre = state(
      "builtin",
      hashMidgardCekValueNodeV1({
        kind: "builtin",
        tag: 0n,
        forcesRemaining: 0n,
        argumentsCount: count,
        argumentsRoot: root,
      }),
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      hash(1),
      0n,
      100n,
      100n,
    );
    const post = state(
      "haltError",
      hashMidgardCekTermNodeV1({ kind: "error" }),
      MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
      7n,
      100n,
      100n,
    );
    expect(
      verifyMidgardCekCoreStepV1(pre, post, {
        kind: "executeBuiltinTypeFailure",
        tag: 0n,
        arguments: arguments_,
      }),
    ).toBe(true);
  });
});
