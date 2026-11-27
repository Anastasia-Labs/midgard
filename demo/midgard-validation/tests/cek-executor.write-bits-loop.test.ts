import {
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  builtinValueToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "../src/cek-builtin.evaluate-direct-builtin.js";
import { hashMidgardCekDirectValueWitness } from "../src/cek-builtin.js";
import { midgardCekWriteBitsTakesLoop } from "../src/cek-executor.write-bits-builtin.js";
import {
  errorSuccessor,
  exactState,
} from "../src/cek-machine.midgard-cek-builtin-argument-count.js";
import { MidgardCekErrorCodes } from "../src/cek-machine.midgard-cek-core-step-witness.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { run } from "./helpers/cek-staged-route-terms.js";
import {
  compactedArguments,
  compactedBytes,
  evaluatorWriteBits,
  indicesConstant,
  pattern,
  spread,
  stepCharge,
  writeBitsOver,
  writeBitsSteps,
} from "./helpers/cek-write-bits-runs.js";

/**
 * writeBits over an index list longer than the direct route proves on L1
 * takes the writeBits loop: a success posts the PV11 evaluator's result and
 * charge, a list with an index outside the input fails uncharged, and each
 * has exactly one provable result.
 */

const charged = (
  post: MidgardCekMachineState,
  by: { readonly cpu: bigint; readonly memory: bigint },
): MidgardCekMachineState => ({
  ...post,
  cpu: post.cpu + by.cpu,
  memory: post.memory + by.memory,
});

describe("writeBits over a long index list", () => {
  it.each([
    [64, 65, true],
    [4_096, 200, false],
    [512, 1_000, true],
  ] as const)(
    "over %i bytes and %i indices is the loop's success, charged as the PV11 evaluator charges it",
    (length, count, set) => {
      const input = pattern(length);
      const indices = spread(count, length);
      const body = writeBitsOver(input, indices, set);
      expect(run(body, "verdict").result.kind).toBe("accepted");
      const proof = run(body, "proof");
      expect(proof.result.kind).toBe("accepted");
      const steps = writeBitsSteps(proof.execution!.steps);
      expect(steps).toHaveLength(1);
      const [step] = steps;
      expect(step!.witness.kind).toBe("executeBuiltinWriteBits");
      const evaluator = evaluatorWriteBits(input, indices, set);
      expect(evaluator.ok && stepCharge(step!)).toEqual(
        evaluator.ok && evaluator.charge,
      );
      expect(
        verifyMidgardCekCoreStep(step!.pre, step!.post, step!.witness),
      ).toBe(true);
      for (const by of [
        { cpu: 1n, memory: 0n },
        { cpu: 0n, memory: -1n },
      ])
        expect(
          verifyMidgardCekCoreStep(
            step!.pre,
            charged(step!.post, by),
            step!.witness,
          ),
        ).toBe(false);
    },
  );

  it("of seven source indices stays on the direct route, and of eight takes the loop", () => {
    const kinds = (count: number) =>
      run(writeBitsOver(pattern(64), spread(count, 64), true), "proof")
        .execution!.steps.map(({ witness }) => witness.kind)
        .filter((kind) => kind.startsWith("executeBuiltin"));
    expect(kinds(7)).toEqual(["executeBuiltinDirect"]);
    expect(kinds(8)).toEqual(["executeBuiltinWriteBits"]);
  });

  it("presented inline takes the loop past 64 indices", () => {
    const presented = (count: number, compacted: boolean) =>
      [
        UPLCConst.byteString(pattern(64)),
        indicesConstant(spread(count, 64)),
        UPLCConst.bool(true),
      ].map((value) => {
        const witness = builtinValueToDirectWitness(value, true);
        return compacted
          ? semanticizeMidgardCekDirectConstant(witness)
          : witness;
      });
    expect(midgardCekWriteBitsTakesLoop(presented(64, false))).toBe(false);
    expect(midgardCekWriteBitsTakesLoop(presented(65, false))).toBe(true);
    expect(midgardCekWriteBitsTakesLoop(presented(2, true))).toBe(true);
  });

  it("over an input past 4,096 bytes fails on the staged failure hop", () => {
    const proof = run(
      writeBitsOver(pattern(4_097), spread(200, 4_097), true),
      "proof",
    );
    expect(proof.result.kind).toBe("script_invalid");
    expect(proof.execution!.steps.at(-1)!.witness.kind).toBe(
      "executeBuiltinStagedFailure",
    );
  });
});

describe("writeBits over a long index list with an index outside the input", () => {
  it.each([
    ["past the last bit", 150, 512n],
    ["negative", 0, -1n],
    ["far past the input", 199, 2n ** 64n],
  ] as const)(
    "%s fails uncharged as the loop's failure, as the PV11 evaluator fails",
    (_, at, bad) => {
      const input = pattern(64);
      const indices = spread(200, 64);
      indices[at] = bad;
      expect(evaluatorWriteBits(input, indices, true).ok).toBe(false);
      const body = writeBitsOver(input, indices, true);
      expect(run(body, "verdict").result.kind).toBe("script_invalid");
      const proof = run(body, "proof");
      expect(proof.result.kind).toBe("script_invalid");
      const last = proof.execution!.steps.at(-1)!;
      expect(last.witness.kind).toBe("executeBuiltinWriteBitsFailure");
      expect(stepCharge(last)).toEqual({ cpu: 0n, memory: 0n });
      expect(verifyMidgardCekCoreStep(last.pre, last.post, last.witness)).toBe(
        true,
      );
      expect(
        verifyMidgardCekCoreStep(
          last.pre,
          charged(last.post, { cpu: 1n, memory: 0n }),
          last.witness,
        ),
      ).toBe(false);
    },
  );
});

describe("a writeBits step of the loop has exactly one provable result", () => {
  const input = pattern(64);

  it("a list with every index in range is refused as a failure", () => {
    const proof = run(writeBitsOver(input, spread(200, 64), true), "proof");
    const [step] = writeBitsSteps(proof.execution!.steps);
    if (step?.witness.kind !== "executeBuiltinWriteBits")
      throw new Error("not the loop's success");
    const { result: _, ...rest } = step.witness;
    expect(
      verifyMidgardCekCoreStep(
        step.pre,
        errorSuccessor(step.pre, MidgardCekErrorCodes.BuiltinFailure),
        { ...rest, kind: "executeBuiltinWriteBitsFailure" },
      ),
    ).toBe(false);
  });

  it("a list with an index outside the input is refused as a success", () => {
    const indices = spread(200, 64);
    indices[150] = 512n;
    const proof = run(writeBitsOver(input, indices, true), "proof");
    const last = proof.execution!.steps.at(-1)!;
    if (last.witness.kind !== "executeBuiltinWriteBitsFailure")
      throw new Error("not the loop's failure");
    for (const result of [input, Buffer.alloc(64)]) {
      const witnessed = compactedBytes(result);
      expect(
        verifyMidgardCekCoreStep(
          last.pre,
          exactState(last.pre, {
            mode: "return",
            focusRoot: hashMidgardCekDirectValueWitness(witnessed),
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
            continuationRoot: last.pre.continuationRoot,
            auxiliary: 0n,
            cpuDelta: 1_000_000n,
            memoryDelta: 8n,
          }),
          {
            ...last.witness,
            kind: "executeBuiltinWriteBits",
            result: witnessed,
          },
        ),
      ).toBe(false);
    }
  });

  it("a failure over an input past 4,096 bytes is not the loop's", () => {
    const long = pattern(4_097);
    const indices = spread(200, 4_097);
    indices[150] = 8n * 4_097n;
    const proof = run(writeBitsOver(long, indices, true), "proof");
    const last = proof.execution!.steps.at(-1)!;
    expect(last.witness.kind).toBe("executeBuiltinStagedFailure");
    expect(
      verifyMidgardCekCoreStep(last.pre, last.post, {
        kind: "executeBuiltinWriteBitsFailure",
        arguments: compactedArguments(long, indices, true),
        input: long,
        indices,
      }),
    ).toBe(false);
  });

  it("a step over other arguments is refused", () => {
    const proof = run(writeBitsOver(input, spread(200, 64), true), "proof");
    const [step] = writeBitsSteps(proof.execution!.steps);
    const other = writeBitsSteps(
      run(writeBitsOver(input, spread(200, 64, 1), true), "proof").execution!
        .steps,
    )[0]!;
    expect(verifyMidgardCekCoreStep(step!.pre, step!.post, other.witness)).toBe(
      false,
    );
  });

  it("a step over other arguments with the same result and charge is refused", () => {
    const indices = spread(200, 64);
    const proof = run(writeBitsOver(input, indices, true), "proof");
    const [step] = writeBitsSteps(proof.execution!.steps);
    const other = writeBitsSteps(
      run(writeBitsOver(input, [...indices].reverse(), true), "proof")
        .execution!.steps,
    )[0]!;
    expect(stepCharge(other)).toEqual(stepCharge(step!));
    expect(other.post.focusRoot).toEqual(step!.post.focusRoot);
    expect(verifyMidgardCekCoreStep(step!.pre, step!.post, step!.witness)).toBe(
      true,
    );
    expect(verifyMidgardCekCoreStep(step!.pre, step!.post, other.witness)).toBe(
      false,
    );
  });

  it("a success posting another result is refused", () => {
    const proof = run(writeBitsOver(input, spread(200, 64), true), "proof");
    const [step] = writeBitsSteps(proof.execution!.steps);
    for (const result of [input, Buffer.alloc(64)]) {
      expect(
        verifyMidgardCekCoreStep(
          step!.pre,
          {
            ...step!.post,
            focusRoot: hashMidgardCekDirectValueWitness(compactedBytes(result)),
          },
          step!.witness,
        ),
      ).toBe(false);
    }
  });
});
