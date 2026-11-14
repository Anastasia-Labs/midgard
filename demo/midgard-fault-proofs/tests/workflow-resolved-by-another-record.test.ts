/** A header removed against a fraud-proof record this run did not create
 * ends the run resolved by another record once every signed attempt of the
 * run is settled. That is never our conviction: no terminal is journaled and
 * no reward is claimed. Another prover's record reaches the orchestrator as
 * the stage derivation's typed error; a sibling run's record on our key as a
 * completed terminal whose record this journal did not create. */
import { afterEach, describe, expect, it, vi } from "vitest";

import * as fundingAuthority from "../src/workflow/funding-reservation-permit.js";
import {
  type FraudProofWorkflowJournalEntry,
  MemoryFraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import { FraudProofResolvedByAnotherRecordError } from "../src/workflow/resolved-by-another-record.js";
import {
  canonicalEvidence,
  makeAdapter,
  PROOF_TX_HASH,
  RELEASE_FINALITY_POLICY,
  REMOVAL_TX_HASH,
  retiredNotFound,
  run,
  terminal,
} from "./workflow.make-adapter.js";

const OTHER_RECORD = `${"c4".repeat(32)}#0`;

/** Past rollback recovery: the depth a completed terminal needs. */
const FINAL_DEPTH = RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;

const resolution = {
  removalTxHash: "c5".repeat(32),
  referencedProofTokenOutRef: OTHER_RECORD,
  confirmationDepth: FINAL_DEPTH,
};

afterEach(() => {
  vi.restoreAllMocks();
});

const kinds = (entries: readonly FraudProofWorkflowJournalEntry[]) =>
  entries.map(({ event }) => event.kind);

/** `makeAdapter`'s family, whose header is gone through another record's
 * removal while `removed()` says so. */
const removedWhile = (
  removed: () => boolean,
  controls: Parameters<typeof makeAdapter>[0] = {},
  depth: () => number = () => FINAL_DEPTH,
): FraudProofFamilyWorkflowAdapter => {
  const adapter = makeAdapter(controls);
  return {
    ...adapter,
    observe: async (context) => {
      if (removed())
        throw new FraudProofResolvedByAnotherRecordError({
          ...resolution,
          confirmationDepth: depth(),
        });
      return await adapter.observe(context);
    },
  };
};

const expectResolved = (
  outcome: Awaited<ReturnType<typeof run>>,
  expected: typeof resolution,
) => {
  expect(outcome.kind).toBe("resolved_by_another_record");
  if (outcome.kind !== "resolved_by_another_record")
    throw new Error("not resolved");
  expect(outcome.resolution).toEqual(expected);
  expect("terminal" in outcome).toBe(false);
  expect(kinds(outcome.entries)).not.toContain("completed");
  expect(kinds(outcome.entries)).not.toContain("terminal_included");
  return outcome;
};

describe("workflow: header removed against another record", () => {
  it("ends resolved by another record, submitting nothing, when the header is gone before any attempt", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = removedWhile(() => true);
    const outcome = await run({ evidence, adapter, journal });
    expectResolved(outcome, resolution);
    expect(adapter.submit).not.toHaveBeenCalled();
  });

  it("waits while the removal is still within rollback recovery, and resolves once it is past it", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    let depth = FINAL_DEPTH - 1;
    const adapter = removedWhile(
      () => true,
      {},
      () => depth,
    );
    const waiting = await run({ evidence, adapter, journal });
    expect(waiting).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      reason: expect.stringMatching(/awaits release finality/u),
    });
    depth = FINAL_DEPTH;
    expectResolved(await run({ evidence, adapter, journal }), resolution);
  });

  it("reconciles this run's signed attempt first, and resolves once it is retired, never replacing it", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    let removed = false;
    let retired = false;
    const adapter = removedWhile(() => removed, {
      reconcile: async ({ txHash }) =>
        retired
          ? retiredNotFound(txHash!)
          : { kind: "pending" as const, txHash: txHash! },
    });
    // Our proof is signed and submitted; it has not landed.
    const first = await run({ evidence, adapter, journal });
    expect(first.kind).toBe("pending");
    removed = true;
    const waiting = await run({ evidence, adapter, journal });
    expect(waiting.kind).toBe("pending");
    retired = true;
    expectResolved(await run({ evidence, adapter, journal }), resolution);
    expect(adapter.submit).toHaveBeenCalledTimes(1);
  });

  it("waits while a superseded attempt of this run is absent but not retired, and replaces nothing", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    let removed = false;
    const adapter = removedWhile(() => removed, {
      reconcile: async ({ txHash }) => ({
        kind: "pending" as const,
        txHash: txHash!,
      }),
    });
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    // Absent at the tip, not retired: a bound funding reservation supersedes
    // it at once, yet it may still land until its retirement past k.
    vi.spyOn(
      fundingAuthority,
      "workflowJournalHasFundingReservation",
    ).mockReturnValue(true);
    adapter.reconcile = vi.fn(async () => ({ kind: "not_found" as const }));
    removed = true;
    for (let pass = 0; pass < 2; pass += 1) {
      const waiting = await run({ evidence, adapter, journal });
      expect(waiting).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
        reason: expect.stringMatching(/not yet settled/u),
      });
    }
    expect(adapter.submit).toHaveBeenCalledTimes(1);
  });

  it("ends resolved by another record when the removal cites a sibling run's record, not one this journal created", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const siblingProof = "c6".repeat(32);
    const ours = makeAdapter();
    // Our proof lands; the header is then removed against the sibling's
    // record, whose creation this journal never saw.
    const adapter: FraudProofFamilyWorkflowAdapter = {
      ...ours,
      observe: async (context) => {
        const observed = await ours.observe(context);
        if (
          !context.entries.some(
            ({ event }) =>
              event.kind === "confirmed" && event.txHash === PROOF_TX_HASH,
          )
        )
          return observed;
        const sibling = terminal(String(context.artifact.headerHash));
        return {
          kind: "completed",
          terminal: {
            ...sibling,
            proofToken: {
              ...sibling.proofToken,
              outRef: `${siblingProof}#0`,
              createdByTxHash: siblingProof,
            },
            correction: {
              ...sibling.correction,
              referencedProofTokenOutRef: `${siblingProof}#0`,
            },
          },
        };
      },
    };
    let outcome = await run({ evidence, adapter, journal });
    for (let step = 0; step < 4 && outcome.kind === "pending"; step += 1)
      outcome = await run({ evidence, adapter, journal });
    const resolved = expectResolved(outcome, {
      removalTxHash: REMOVAL_TX_HASH,
      referencedProofTokenOutRef: `${siblingProof}#0`,
      confirmationDepth: FINAL_DEPTH,
    });
    expect(
      resolved.entries.some(
        ({ event }) =>
          event.kind === "confirmed" && event.txHash === PROOF_TX_HASH,
      ),
    ).toBe(true);
  });
});
