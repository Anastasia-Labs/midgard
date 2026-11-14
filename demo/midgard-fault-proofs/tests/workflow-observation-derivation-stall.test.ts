/** A family whose stage cannot be derived from the admitted L1 view stalls
 * that workflow alone: the run returns `stalled`, the journal records the
 * cause once however often it repeats, and the next run observes again and
 * proceeds once the view derives. Before and after the workflow's first
 * confirmation alike. An L1 wait still reaches the caller unchanged. */
import { describe, expect, it } from "vitest";

import {
  type FraudProofWorkflowJournalEntry,
  MemoryFraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import { FraudProofL1UnavailableError } from "../src/workflow/l1-source.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import { FraudProofRawL1DerivationError } from "../src/workflow/raw-l1-family-derivation.js";
import {
  canonicalEvidence,
  makeAdapter,
  PROOF_TX_HASH,
  REMOVAL_TX_HASH,
  run,
} from "./workflow.make-adapter.js";

const stalls = (entries: readonly FraudProofWorkflowJournalEntry[]) =>
  entries.flatMap(({ event }) =>
    event.kind === "stalled" ? [event.reason] : [],
  );

const confirmed =
  (txHash: string) => (entries: readonly FraudProofWorkflowJournalEntry[]) =>
    entries.some(
      ({ event }) => event.kind === "confirmed" && event.txHash === txHash,
    );
const proofConfirmed = confirmed(PROOF_TX_HASH);
const removalConfirmed = confirmed(REMOVAL_TX_HASH);

/** `makeAdapter`'s family, whose observation fails with `failure` while
 * `failing` says so for the journal it is given. */
const failingWhile = (
  failing: (entries: readonly FraudProofWorkflowJournalEntry[]) => boolean,
  failure: () => Error,
): FraudProofFamilyWorkflowAdapter => {
  const adapter = makeAdapter();
  return {
    ...adapter,
    observe: async (context) => {
      if (failing(context.entries)) throw failure();
      return await adapter.observe(context);
    },
  };
};

const derivationFailure = () =>
  new FraudProofRawL1DerivationError(
    "doubleSpend",
    new Error("raw L1 view holds a stage no derivation reads"),
  );

describe("workflow observation: a stage the family cannot derive", () => {
  for (const [when, before] of [
    ["before the first confirmation", true],
    ["after the first confirmation", false],
  ] as const)
    it(`stalls this workflow alone ${when}, records it once, and proceeds once the view derives`, async () => {
      const evidence = await canonicalEvidence();
      const journal = new MemoryFraudProofWorkflowJournalStore();
      // Before: the view fails while nothing is confirmed. After: the proof
      // lands and the view fails while the removal is still to come.
      let viewFails = true;
      const adapter = failingWhile(
        (entries) =>
          viewFails &&
          (before
            ? !proofConfirmed(entries)
            : proofConfirmed(entries) && !removalConfirmed(entries)),
        derivationFailure,
      );
      const first = await run({ evidence, adapter, journal });
      expect(first.kind).toBe("stalled");
      if (!("reason" in first)) throw new Error("no reason");
      expect(first.reason).toMatch(
        /^observation failed: .*raw L1 view holds a stage no derivation reads/u,
      );
      const again = await run({ evidence, adapter, journal });
      expect(again.kind).toBe("stalled");
      if (!("entries" in again)) throw new Error("no journal");
      expect(stalls(again.entries)).toEqual([first.reason]);
      expect(proofConfirmed(again.entries)).toBe(!before);

      viewFails = false;
      let outcome = await run({ evidence, adapter, journal });
      for (
        let attempt = 0;
        attempt < 4 && outcome.kind !== "completed";
        attempt += 1
      )
        outcome = await run({ evidence, adapter, journal });
      expect(outcome.kind).toBe("completed");
    });

  it("hands an L1 wait to the caller unchanged and records nothing for it", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const unavailable = new FraudProofL1UnavailableError("follower is down");
    let followerDown = true;
    const adapter = failingWhile(
      () => followerDown,
      () => unavailable,
    );
    await expect(run({ evidence, adapter, journal })).rejects.toBe(unavailable);
    followerDown = false;
    const resumed = await run({ evidence, adapter, journal });
    if (!("entries" in resumed)) throw new Error("no journal");
    expect(stalls(resumed.entries)).toEqual([]);
  });
});
