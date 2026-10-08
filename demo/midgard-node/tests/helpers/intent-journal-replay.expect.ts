/**
 * The expectations a real flow's replayed intents meet
 * (`replayJournaledOnFollower`, I1-fix F5):
 *
 * - every intent is recorded: each input, reference input and collateral
 *   is a tracked fact or a journaled parent's output, at the tip before it
 *   landed;
 * - a family whose §8.4 predicate reads the follower's facts is judged
 *   wanted there (`resubmit`), unless it is chained on a parent landing in
 *   the same block, when S6 waits on its inputs;
 * - a family whose target state is not in the facts is held there
 *   (`wait_transient`), never resent or abandoned;
 * - once its block applies, it is landed (`follow`).
 */
import { expect } from "vitest";

import type { NodeIntentFamily } from "../../src/services/intent-journal.js";
import type { ReplayConfig, ReplayedIntent } from "./intent-journal-replay.js";

/** The families whose target state the follower does not hold (`FamilyPredicateUnavailable`). */
export const FAMILIES_WITHOUT_FACTS: readonly NodeIntentFamily[] = [
  "reference_funding",
  "script_reward_registration",
  "phas_membership",
];

/**
 * Asserts each replayed intent's outcome, and that the families replayed
 * are exactly `families` plus any of `optional`.
 */
export const expectReplayedFamilies = (
  replayed: readonly ReplayedIntent[],
  families: readonly NodeIntentFamily[],
  optional: readonly NodeIntentFamily[] = [],
): Readonly<Partial<Record<NodeIntentFamily, ReplayedIntent[]>>> => {
  const byFamily: Partial<Record<NodeIntentFamily, ReplayedIntent[]>> = {};
  for (const entry of replayed) (byFamily[entry.family] ??= []).push(entry);
  const seen = Object.keys(byFamily) as NodeIntentFamily[];
  expect(seen.filter((family) => !optional.includes(family)).sort()).toEqual(
    [...families].sort(),
  );
  for (const entry of replayed) {
    expect(entry, entry.family).toMatchObject({
      outcome: "recorded",
      after: "follow",
    });
    if (FAMILIES_WITHOUT_FACTS.includes(entry.family))
      expect(entry, entry.family).toMatchObject({
        verdict: expect.stringContaining(
          "target state is not in the follower's facts",
        ),
        before: "wait_transient",
      });
    else if (entry.chained)
      expect(entry, entry.family).toMatchObject({ before: "wait_inputs" });
    else
      expect(entry, entry.family).toMatchObject({
        verdict: true,
        before: "resubmit",
      });
  }
  return byFamily;
};

/**
 * A replay configuration for a flow whose node signs with `operatorSeed`
 * and keeps its reference scripts in the `referenceScriptsSeed` wallet.
 */
export const walletReplayConfig = (input: {
  readonly operatorSeed: string;
  readonly referenceScriptsSeed: string;
  readonly referenceScriptsAddress: string;
}): ReplayConfig => ({
  NETWORK: "Preprod",
  L1_OPERATOR_SEED_PHRASE: input.operatorSeed,
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: input.operatorSeed,
  L1_REFERENCE_SCRIPT_SEED_PHRASE: input.referenceScriptsSeed,
  L1_REFERENCE_SCRIPT_ADDRESS: input.referenceScriptsAddress,
  L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: input.referenceScriptsAddress,
  HISTORY_COMMIT_HORIZON_LAG_BLOCKS: 0,
});
