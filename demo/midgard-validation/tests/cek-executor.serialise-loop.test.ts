import { describe, expect, it } from "vitest";

import type { MidgardCekExecutionStep } from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import {
  midgardCekSerialiseFactsOf,
  type MidgardCekSerialiseWitness,
} from "../src/cek-serialise.js";
import {
  blake2b256,
  evaluatorSerialiseCharge,
  evaluatorSerialisesTo,
  runOver as run,
  SERIALISE,
  serialiseSteps,
  stepCharge,
  valueNamed as byName,
} from "./helpers/cek-serialise-runs.js";

/**
 * serialiseData (51) past the direct carriage bound or past the one-step
 * budget is proved by the serialise loop: the proof-mode run records one
 * serialise step whose staged bytes are the argument's exact CBOR, reaches
 * the PV11 evaluator's verdict and budget, charges the evaluator's
 * serialiseData cost, and the step verifies only with the bytes, argument,
 * result and charge the loop binds.
 */

/** The values the loop proves below the maxima, each a separate row. */
const LOOP_VALUES = [
  "list_of_128_items",
  "nested_128_deep",
  "eight_input_records",
  "past_the_bound_9216",
  "bignum_of_5000_bytes",
  "leaf_of_20000_bytes",
] as const;

/** The one serialise step of the proof-mode run of serialiseData over `hex`. */
const serialiseStep = (hex: string) => {
  const steps = serialiseSteps(run(SERIALISE, hex, "proof"));
  expect(steps).toHaveLength(1);
  return steps[0]!;
};

const verifies = (
  step: MidgardCekExecutionStep,
  witness: MidgardCekSerialiseWitness,
): boolean => verifyMidgardCekCoreStep(step.pre, step.post, witness);

describe("serialiseData proved by the serialise loop", () => {
  it.each(LOOP_VALUES)(
    "proves %s with the PV11 evaluator's verdict and budget",
    (name) => {
      const { hex } = byName(name);
      const verdict = run(SERIALISE, hex, "verdict");
      const proof = run(SERIALISE, hex, "proof");
      expect(verdict.result.kind).toBe("accepted");
      expect(proof.result).toEqual(verdict.result);
      const steps = proof.execution?.steps ?? [];
      const loop = serialiseSteps(proof);
      expect(loop).toHaveLength(1);
      expect(Buffer.from(loop[0]!.witness.serialised).toString("hex")).toBe(
        hex,
      );
      expect(stepCharge(loop[0]!)).toEqual(evaluatorSerialiseCharge(hex));
      for (const step of steps) {
        expect(
          verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
        ).toBe(true);
      }
    },
    120_000,
  );

  it.each(LOOP_VALUES)(
    "gives %s the bytes the PV11 evaluator gives",
    (name) => {
      const { hex } = byName(name);
      const hash = blake2b256(hex);
      expect(evaluatorSerialisesTo(hex, hash)).toBe(true);
      hash[0] ^= 1;
      expect(evaluatorSerialisesTo(hex, hash)).toBe(false);
    },
    120_000,
  );

  it("does not verify a step whose staged bytes are not the argument's", () => {
    const step = serialiseStep(byName("past_the_bound_9216").hex);
    const bytes = Buffer.from(step.witness.serialised);
    const other = Buffer.from(byName("leaf_of_20000_bytes").hex, "hex");
    const flipped = Buffer.from(bytes);
    flipped[flipped.length - 2] ^= 1;
    for (const serialised of [
      Buffer.concat([bytes, Buffer.from([0])]),
      bytes.subarray(0, bytes.length - 1),
      flipped,
      other,
    ]) {
      expect(verifies(step, { ...step.witness, serialised })).toBe(false);
    }
    expect(verifies(step, step.witness)).toBe(true);
  });

  it("does not verify a step whose result is not the staged bytes' leaf", () => {
    const step = serialiseStep(byName("past_the_bound_9216").hex);
    const other = Buffer.from(byName("leaf_of_20000_bytes").hex, "hex");
    const [argument] = step.witness.arguments;
    if (argument?.kind !== "semanticConstant") throw new Error("not semantic");
    const { facts } = midgardCekSerialiseFactsOf(
      argument.witness.payload,
      other,
    );
    expect(verifies(step, { ...step.witness, result: facts.result })).toBe(
      false,
    );
    // The same result staged with its own bytes reads back to another value.
    expect(
      verifies(step, {
        ...step.witness,
        result: facts.result,
        serialised: other,
      }),
    ).toBe(false);
  });

  it("does not verify a step over another argument", () => {
    const step = serialiseStep(byName("past_the_bound_9216").hex);
    const other = serialiseStep(byName("leaf_of_20000_bytes").hex);
    expect(verifies(step, other.witness)).toBe(false);
    expect(verifies(other, other.witness)).toBe(true);
  });

  it("does not verify a step with no staged bytes", () => {
    const step = serialiseStep(byName("past_the_bound_9216").hex);
    expect(
      verifies(step, { ...step.witness, serialised: Buffer.alloc(0) }),
    ).toBe(false);
  });

  it("does not verify a step whose post holds another value", () => {
    const step = serialiseStep(byName("past_the_bound_9216").hex);
    const other = serialiseStep(byName("leaf_of_20000_bytes").hex);
    expect(
      verifyMidgardCekCoreStep(
        step.pre,
        { ...step.post, focusRoot: other.post.focusRoot },
        step.witness,
      ),
    ).toBe(false);
  });

  /**
   * Another argument's whole step (its staged bytes and result) over this
   * step's pre-state, with the post that step would reach from it: only the
   * argument the pre-state holds tells them apart.
   */
  it("does not verify another argument's step from this pre-state", () => {
    const step = serialiseStep(byName("past_the_bound_9216").hex);
    const other = serialiseStep(byName("leaf_of_20000_bytes").hex);
    const post = {
      ...step.post,
      focusRoot: other.post.focusRoot,
      cpu: step.pre.cpu + (other.post.cpu - other.pre.cpu),
      memory: step.pre.memory + (other.post.memory - other.pre.memory),
    };
    expect(verifyMidgardCekCoreStep(step.pre, post, other.witness)).toBe(false);
  });

  it("does not verify a step that charges another budget", () => {
    const step = serialiseStep(byName("past_the_bound_9216").hex);
    expect(
      verifyMidgardCekCoreStep(
        step.pre,
        { ...step.post, cpu: step.post.cpu + 1n },
        step.witness,
      ),
    ).toBe(false);
  });
});
