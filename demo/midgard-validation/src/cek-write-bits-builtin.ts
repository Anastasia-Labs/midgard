/**
 * writeBits (tag 80) as the writeBits loop proves it, over an input of at
 * most 4,096 bytes and an index list of any length: the step's witness names
 * its compacted arguments (and, for a success, its result), and the loop
 * (`cek_core_staged_write_bits_loop`) reveals the input and the indices hop
 * by hop. A success's last hop checks the result and gives the charge; a
 * failure's walk ends, uncharged, on the hop that opens an index outside the
 * input.
 *
 * Hook points for driving the loop in a dispute:
 *
 *   * `midgardCekWriteBitsLoopOfWitness` gives the loop of a step's witness;
 *   * `planMidgardCekWriteBitsHops` (`cek-write-bits-loop.hops.ts`) gives
 *     each hop's redeemer (`midgardCekWriteBitsHopData`), cursor and next
 *     state (`midgardCekWriteBitsStateData`, `midgardCekWriteBitsWalkData`);
 *     planning is deterministic, so a restart replays it to the cursor the
 *     chain holds.
 */
import {
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";

import { hashMidgardCekDirectValueWitness } from "./cek-builtin.js";
import type { MidgardCekBuiltinBudget } from "./cek-cost.js";
import {
  errorSuccessor,
  exactState,
  sameState,
} from "./cek-machine.midgard-cek-builtin-argument-count.js";
import {
  type MidgardCekCoreStepWitness,
  MidgardCekErrorCodes,
} from "./cek-machine.midgard-cek-core-step-witness.js";
import { builtinRootMatches } from "./cek-machine.verify-case-select.js";
import {
  type MidgardCekWriteBitsLoop,
  midgardCekWriteBitsLoopOf,
  planMidgardCekWriteBitsHops,
} from "./cek-write-bits-loop.hops.js";

export type MidgardCekWriteBitsWitness = Extract<
  MidgardCekCoreStepWitness,
  {
    readonly kind: "executeBuiltinWriteBits" | "executeBuiltinWriteBitsFailure";
  }
>;

/** The longest input the failure roots hop routes to the loop, in words. */
export const MIDGARD_CEK_WRITE_BITS_LOOP_INPUT_WORDS = 512n;

/** The loop of a writeBits step's witness. */
export const midgardCekWriteBitsLoopOfWitness = (
  witness: MidgardCekWriteBitsWitness,
): MidgardCekWriteBitsLoop =>
  midgardCekWriteBitsLoopOf({
    failure: witness.kind === "executeBuiltinWriteBitsFailure",
    arguments: witness.arguments,
    result: witness.kind === "executeBuiltinWriteBits" ? witness.result : null,
    input: witness.input,
    indices: witness.indices,
  });

/**
 * The charge of a writeBits success whose loop ends over the step's input
 * and indices, or null when a planned hop is refused. The planner returns
 * only once a hop ends the loop and throws on any hop the loop rejects.
 */
export const verifyMidgardCekWriteBitsBuiltin = (
  witness: Extract<
    MidgardCekWriteBitsWitness,
    { readonly kind: "executeBuiltinWriteBits" }
  >,
): MidgardCekBuiltinBudget | null => {
  try {
    return (
      planMidgardCekWriteBitsHops(midgardCekWriteBitsLoopOfWitness(witness)).at(
        -1,
      )?.charge ?? null
    );
  } catch {
    return null;
  }
};

/**
 * Whether a writeBits failure is the loop's: the failure roots hop routes it
 * there (a compacted input of at most 512 words), and the walk over the
 * step's input and indices ends on a hop that opens an index outside it.
 */
export const verifyMidgardCekWriteBitsFailure = (
  witness: Extract<
    MidgardCekWriteBitsWitness,
    { readonly kind: "executeBuiltinWriteBitsFailure" }
  >,
): boolean => {
  const [input] = witness.arguments;
  if (
    input?.kind !== "semanticConstant" ||
    input.witness.memory > MIDGARD_CEK_WRITE_BITS_LOOP_INPUT_WORDS
  )
    return false;
  try {
    return (
      planMidgardCekWriteBitsHops(midgardCekWriteBitsLoopOfWitness(witness)).at(
        -1,
      )?.next === null
    );
  } catch {
    return false;
  }
};

/**
 * Whether a writeBits step of the loop is sound: its arguments are the
 * builtin's at the pre-state, and a success posts the result and the charge
 * its loop gives, a failure the uncharged builtin failure.
 */
export const verifyMidgardCekWriteBitsStep = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
  witness: MidgardCekWriteBitsWitness,
): boolean => {
  if (!builtinRootMatches(pre, 80n, witness.arguments)) return false;
  if (witness.kind === "executeBuiltinWriteBitsFailure")
    return (
      verifyMidgardCekWriteBitsFailure(witness) &&
      sameState(post, errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure))
    );
  const budget = verifyMidgardCekWriteBitsBuiltin(witness);
  return (
    budget !== null &&
    sameState(
      post,
      exactState(pre, {
        mode: "return",
        focusRoot: hashMidgardCekDirectValueWitness(witness.result),
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
        cpuDelta: budget.cpu,
        memoryDelta: budget.memory,
      }),
    )
  );
};
