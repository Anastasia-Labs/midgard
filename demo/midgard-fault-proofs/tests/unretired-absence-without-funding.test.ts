import { afterEach, expect, it, vi } from "vitest";

import * as fundingAuthority from "../src/workflow/funding-reservation-permit.js";
import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import {
  canonicalEvidence,
  makeAdapter,
  PROOF_TX_HASH,
  REMOVAL_TX_HASH,
  retiredNotFound,
  run,
} from "./workflow.make-adapter.js";

afterEach(() => {
  vi.restoreAllMocks();
});

/** The proof attempt is submitted and left pending, then the chain reports
 * it absent without retirement. */
const submitThenLose = async () => {
  const evidence = await canonicalEvidence();
  const journal = new MemoryFraudProofWorkflowJournalStore();
  const adapter = makeAdapter({
    reconcile: async ({ txHash }) => ({ kind: "pending", txHash }),
  });
  const first = await run({ evidence, adapter, journal });
  if (first.kind !== "pending") throw new Error("expected a pending attempt");
  expect(adapter.submit).toHaveBeenCalledTimes(1);
  // Until a replacement is submitted, the first attempt reads as absent.
  const replacedYet = () => vi.mocked(adapter.submit).mock.calls.length > 1;
  adapter.reconcile = vi.fn(async ({ txHash }) =>
    replacedYet()
      ? { kind: "confirmed" as const, txHash: txHash! }
      : { kind: "not_found" as const },
  );
  return {
    evidence,
    journal,
    adapter,
    replacedYet,
    notFound: async () =>
      (await journal.load(first.workflowId)).filter(
        ({ event }) =>
          event.kind === "reconciled" && event.outcome === "not_found",
      ),
  };
};

it("holds an attempt absent without retirement when no funding reservation keeps a replacement exclusive", async () => {
  const { evidence, journal, adapter, replacedYet, notFound } =
    await submitThenLose();
  for (let pass = 0; pass < 2; pass += 1) {
    const result = await run({ evidence, adapter, journal });
    expect(result).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      reason: expect.stringContaining("absent without retirement"),
    });
    // Nothing replaces it and nothing records it as resolved.
    expect(adapter.preflight).toHaveBeenCalledTimes(1);
    expect(adapter.submit).toHaveBeenCalledTimes(1);
    expect(await notFound()).toEqual([]);
  }
  // Past k its retirement is proved, and the workflow replaces it.
  adapter.reconcile = vi.fn(async ({ txHash }) =>
    replacedYet()
      ? { kind: "confirmed" as const, txHash: txHash! }
      : retiredNotFound(txHash!),
  );
  const replaced = await run({ evidence, adapter, journal });
  expect(replaced.kind).toBe("completed");
  // The proof again, then the removal.
  expect(
    vi
      .mocked(adapter.submit)
      .mock.calls.map(([{ preflight }]) => preflight.txHash),
  ).toEqual([PROOF_TX_HASH, PROOF_TX_HASH, REMOVAL_TX_HASH]);
  expect(await notFound()).toMatchObject([
    { event: { txHash: PROOF_TX_HASH, retirement: { reason: "expired" } } },
  ]);
});

it("replaces an attempt absent without retirement at once when a funding reservation is bound", async () => {
  const { evidence, journal, adapter, notFound } = await submitThenLose();
  // The reservation store keeps the replacement exclusive; the watcher's
  // superseded-attempt suites exercise that store.
  vi.spyOn(
    fundingAuthority,
    "workflowJournalHasFundingReservation",
  ).mockReturnValue(true);
  const result = await run({ evidence, adapter, journal });
  expect(result.kind).toBe("completed");
  expect(adapter.submit).toHaveBeenCalledTimes(3);
  const [lost] = await notFound();
  expect(lost?.event).toMatchObject({ txHash: PROOF_TX_HASH });
  expect(lost?.event).not.toHaveProperty("retirement");
});
