/**
 * The timeout-correction workflow journals a step's bytes only when it
 * decides to send them (I1-fix F8a): a freshly prepared step is journaled
 * before the save that leads to its send; a step a rollback reopens in place
 * (`confirmed` or `superseded` back to `prepared`) is not.
 */
import type {
  TimeoutCorrectionJournal,
  TimeoutCorrectionJournalStep,
  TimeoutCorrectionJournalStore,
} from "@al-ft/midgard-fault-proofs";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { withCorrectionIntentJournal } from "../src/fibers/attestation-timeout-correction.reconcile-state-queue-corrections.js";
import { IntentJournal } from "../src/services/intent-journal.js";
import { recordingIntentJournal, TEST_PLAN } from "./helpers/intent-journal.js";

const step = (
  n: number,
  status: TimeoutCorrectionJournalStep["status"],
): TimeoutCorrectionJournalStep => ({
  kind: n === 0 ? "remove-block" : "prune-descendant",
  removedHeaderHash: n.toString(16).padStart(2, "0").repeat(28),
  inputOutRefs: [`${"ab".repeat(32)}#${n}`],
  txHash: (n + 1).toString(16).padStart(2, "0").repeat(32),
  signedCbor: `84a0${n.toString(16).padStart(2, "0")}`,
  validFromSlot: "0",
  validToSlot: "100",
  status,
});

const correction = (
  steps: readonly TimeoutCorrectionJournalStep[],
): TimeoutCorrectionJournal => ({
  version: 1,
  targetHeaderHash: "cd".repeat(28),
  targetDeadlineMs: "0",
  steps,
  completed: false,
});

/** The workflow pass's plan and slot clock. */
const PASS = { plan: TEST_PLAN, slotTime: (slot: number) => slot * 1000 };

const memoryStore = (initial: TimeoutCorrectionJournal) => {
  let saved: TimeoutCorrectionJournal | undefined = initial;
  const store: TimeoutCorrectionJournalStore = {
    load: () => Promise.resolve(saved),
    save: (journal) => {
      saved = journal;
      return Promise.resolve();
    },
  };
  return store;
};

describe("correction steps are journaled only on the send decision", () => {
  it.each(["confirmed", "superseded"] as const)(
    "a step a rollback reopens from %s is not journaled; a step appended after it is",
    async (landed) => {
      const { recorded, layer } = recordingIntentJournal();
      const journal = Effect.runSync(Effect.provide(IntentJournal, layer));
      const store = withCorrectionIntentJournal(
        memoryStore(correction([step(0, landed)])),
        journal,
        PASS,
      );
      const loaded = await store.load();
      expect(loaded?.steps[0]?.status).toBe(landed);
      // The rollback reopens step 0 in place: same bytes, back to prepared.
      await store.save(correction([step(0, "prepared")]));
      expect(recorded).toEqual([]);
      // The workflow decides to send a new step: it is journaled, alone.
      await store.save(correction([step(0, "prepared"), step(1, "prepared")]));
      expect(recorded.map((r) => [r.txHash, r.signedTxCbor])).toEqual([
        [step(1, "prepared").txHash, step(1, "prepared").signedCbor],
      ]);
      expect(recorded[0]?.intent).toMatchObject({
        kind: "journaled",
        family: "correction",
        workflowKey: `correction:${"cd".repeat(28)}:prune-descendant:${step(1, "prepared").removedHeaderHash}`,
        plan: TEST_PLAN,
        contentRef: Buffer.from(step(1, "prepared").removedHeaderHash, "hex"),
      });
      // Saving it again (submitted, then a status refresh) journals nothing more.
      await store.save(correction([step(0, "prepared"), step(1, "submitted")]));
      expect(recorded).toHaveLength(1);
    },
  );

  it("journals a first prepared step before the save that leads to its send", async () => {
    const { recorded, layer } = recordingIntentJournal();
    const journal = Effect.runSync(Effect.provide(IntentJournal, layer));
    let savedWhenRecorded: number | undefined;
    const inner = memoryStore(correction([]));
    const store = withCorrectionIntentJournal(
      {
        ...inner,
        save: async (next) => {
          savedWhenRecorded ??= recorded.length;
          await inner.save(next);
        },
      },
      journal,
      PASS,
    );
    await store.load();
    await store.save(correction([step(0, "prepared")]));
    expect(recorded.map((r) => r.txHash)).toEqual([step(0, "prepared").txHash]);
    expect(savedWhenRecorded).toBe(1);
  });
});
