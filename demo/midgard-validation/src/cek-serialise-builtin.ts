/**
 * serialiseData (tag 51) as the serialise loop proves it: the step's
 * witness names its argument and result, and its result bytes R are staged
 * as 4,095-byte chunks, three a run, one staging output a run. The loop
 * (`cek_core_semantic_serialise_loop`) reads R hop by hop and, on its last
 * hop, checks that R reads back to the argument's summary and that the
 * result is R's byte leaf.
 *
 * Hook points for driving the loop in a dispute:
 *
 *   * `midgardCekSerialiseStagedRuns` gives the runs to stage, in byte order;
 *   * `midgardCekSerialiseLoopOf` gives the loop over the staging outputs
 *     they were staged on;
 *   * `planMidgardCekSerialiseHops` (`cek-serialise-loop.hops.ts`) gives
 *     each hop's redeemer, reference inputs, cursor and next state, and
 *     `planMidgardCekSerialiseHop` resumes from the cursor the chain holds.
 */
import type { MidgardCekStagedOutRef } from "@al-ft/midgard-core";

import type { MidgardCekCoreStepWitness } from "./cek-machine.midgard-cek-core-step-witness.js";
import {
  type MidgardCekSerialiseLoop,
  planMidgardCekSerialiseHops,
} from "./cek-serialise-loop.hops.js";
import {
  midgardCekPlaceholderStaging,
  midgardCekStagedOperandRuns,
} from "./cek-staged-builtin.material.js";

export type MidgardCekSerialiseWitness = Extract<
  MidgardCekCoreStepWitness,
  { readonly kind: "executeBuiltinSerialise" }
>;

/** R's runs, in byte order: 4,095-byte chunks, three a run. */
export const midgardCekSerialiseStagedRuns = (
  witness: MidgardCekSerialiseWitness,
): Buffer[][] => midgardCekStagedOperandRuns([witness.serialised])[0]!;

/**
 * The loop of a serialiseData step over the staging outputs `refs` (one per
 * run, in byte order), or over placeholder outputs when R is not yet staged.
 */
export const midgardCekSerialiseLoopOf = (
  witness: MidgardCekSerialiseWitness,
  refs: readonly MidgardCekStagedOutRef[] = midgardCekPlaceholderStaging([
    witness.serialised,
  ])[0]!,
): MidgardCekSerialiseLoop => {
  const runs = midgardCekSerialiseStagedRuns(witness);
  if (refs.length !== runs.length) {
    throw new Error("serialise staging names one output per run");
  }
  return {
    facts: { arguments: witness.arguments, result: witness.result },
    runs,
    refs,
  };
};

/**
 * Whether the serialise loop ends over the step's staged R: every planned
 * hop is accepted and the last one ends the loop, so R reads back to the
 * argument and the result is R's byte leaf. The planner returns only once a
 * hop ends the loop and throws on any hop the loop rejects.
 */
export const verifyMidgardCekSerialiseBuiltin = (
  witness: MidgardCekSerialiseWitness,
): boolean => {
  try {
    planMidgardCekSerialiseHops(midgardCekSerialiseLoopOf(witness));
    return true;
  } catch {
    return false;
  }
};
