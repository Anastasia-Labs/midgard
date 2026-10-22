import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect, it, vi } from "vitest";

import { reconcileSupersededAttemptRecords } from "../src/workflow/funding-reservation-permit.reconcile-superseded-attempts.js";
import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
} from "../src/workflow/journal.js";
import { reconcileSignedWorkflowTransaction } from "../src/workflow/signed-transaction-reconciliation.js";
import { createSupersededAttemptReadSchedule } from "../src/workflow/superseded-attempt-read-schedule.js";
import {
  signedRecoveryObservation,
  signedWorkflowTransactionFixture,
} from "./support/signed-workflow-transaction.js";

it("never holds on a superseded attempt, then certifies the exact old bytes without replacing a later same-action cursor", async () => {
  const fixture = await signedWorkflowTransactionFixture({ ttl: 100 });
  const txHash = fixture.input.transactionHash;
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: "11".repeat(32),
    category: "doubleSpend" as const,
    target: {
      kind: "state_queue_header" as const,
      headerHash: "22".repeat(28),
    },
  };
  const oldIntent = {
    kind: "submission_intent" as const,
    actionId: "proof.init",
    actionInput: {},
    attempt: 1,
    txHash,
  };
  const replacement = { ...oldIntent, attempt: 2, txHash: "33".repeat(32) };
  const entry = (
    event: FraudProofWorkflowJournalEvent,
    sequence: number,
  ): FraudProofWorkflowJournalEntry => ({
    schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
    workflowId: "44".repeat(32),
    identity,
    sequence,
    recordedAt: "2026-10-02T00:00:00.000Z",
    event,
  });
  const entries = [
    entry(oldIntent, 0),
    entry(
      {
        kind: "reconciled",
        actionId: oldIntent.actionId,
        txHash,
        outcome: "not_found",
      },
      1,
    ),
    entry(replacement, 2),
  ];
  const saved = {
    transition: {
      ...fixture.input,
      actionKind: "proof.init",
      transactionBodySha256: "55".repeat(32),
      consumedOutRefs: [],
      producedInputs: [],
    },
    workflowId: "44".repeat(32),
    intentSequence: 0,
    actionId: oldIntent.actionId,
  };
  const retire = vi.fn(async (_hash: string, _retirement: unknown) => {});
  const append = vi.fn(async (_event: FraudProofWorkflowJournalEvent) => {});
  const unknown = vi.fn(async () => ({
    kind: "unknown" as const,
    reason: "canonical source unavailable",
  }));
  await expect(
    reconcileSupersededAttemptRecords({
      savedAttempts: [saved],
      entries,
      append,
      retire,
      reconcile: unknown,
    }),
  ).resolves.toBeNull();
  expect(retire).not.toHaveBeenCalled();
  expect(append).not.toHaveBeenCalled();
  await expect(
    reconcileSupersededAttemptRecords({
      savedAttempts: [saved],
      entries,
      append,
      retire,
      reconcile: async () => ({ kind: "not_found" }),
    }),
  ).resolves.toBeNull();
  expect(retire).not.toHaveBeenCalled();
  expect(append).not.toHaveBeenCalled();
  const proof = await reconcileSignedWorkflowTransaction({
    ...fixture.input,
    observe: async (input) => signedRecoveryObservation(input, "expired"),
  });
  if (proof.kind === "unknown") throw new Error(proof.reason);
  expect(proof).toMatchObject({
    kind: "not_found",
    retirement: { transactionHash: txHash, reason: "expired" },
  });
  await expect(
    reconcileSupersededAttemptRecords({
      savedAttempts: [saved],
      entries,
      append,
      retire,
      reconcile: async (retained) => {
        expect(retained.transition.signedTransactionCborHex).toBe(
          fixture.input.signedTransactionCborHex,
        );
        return proof;
      },
    }),
  ).resolves.toBeNull();
  if (proof.kind !== "not_found" || proof.retirement === undefined)
    throw new Error("missing proven receipt");
  // The retirement and its journal record commit together, in `retire`.
  expect(retire).toHaveBeenCalledExactlyOnceWith(
    txHash,
    expect.objectContaining({ transactionHash: txHash, reason: "expired" }),
  );
  expect(append).not.toHaveBeenCalled();
  expect(entries.at(-1)?.event).toEqual(replacement);
  // A retired attempt's retirement committed with its journal record: it is
  // never read, retired or recorded again.
  append.mockClear();
  retire.mockClear();
  unknown.mockClear();
  await expect(
    reconcileSupersededAttemptRecords({
      savedAttempts: [{ ...saved, retirement: proof.retirement }],
      entries,
      append,
      retire,
      reconcile: unknown,
    }),
  ).resolves.toBeNull();
  expect(append).not.toHaveBeenCalled();
  expect(retire).not.toHaveBeenCalled();
  expect(unknown).not.toHaveBeenCalled();
});

it("returns a superseded attempt that landed within k for adoption, and nothing for one still unresolved", async () => {
  const fixture = await signedWorkflowTransactionFixture({ ttl: 100 });
  const txHash = fixture.input.transactionHash;
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: "11".repeat(32),
    category: "doubleSpend" as const,
    target: {
      kind: "state_queue_header" as const,
      headerHash: "22".repeat(28),
    },
  };
  const oldIntent = {
    kind: "submission_intent" as const,
    actionId: "proof.init",
    actionInput: {},
    attempt: 1,
    txHash,
  };
  const replacement = { ...oldIntent, attempt: 2, txHash: "33".repeat(32) };
  const entry = (
    event: FraudProofWorkflowJournalEvent,
    sequence: number,
  ): FraudProofWorkflowJournalEntry => ({
    schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
    workflowId: "44".repeat(32),
    identity,
    sequence,
    recordedAt: "2026-10-02T00:00:00.000Z",
    event,
  });
  const entries = [
    entry(oldIntent, 0),
    entry(
      {
        kind: "reconciled",
        actionId: oldIntent.actionId,
        txHash,
        outcome: "not_found",
      },
      1,
    ),
    entry(replacement, 2),
  ];
  const saved = {
    transition: {
      ...fixture.input,
      actionKind: "proof.init",
      transactionBodySha256: "55".repeat(32),
      consumedOutRefs: [],
      producedInputs: [],
    },
    workflowId: "44".repeat(32),
    intentSequence: 0,
    actionId: oldIntent.actionId,
  };
  const retire = vi.fn(async () => {});
  const append = vi.fn(async (_event: FraudProofWorkflowJournalEvent) => {});
  for (const result of [
    { kind: "pending" as const, txHash },
    { kind: "conflict" as const, reason: "spent by an unrelated transaction" },
    { kind: "not_found" as const },
  ])
    await expect(
      reconcileSupersededAttemptRecords({
        savedAttempts: [saved],
        entries,
        append,
        retire,
        reconcile: async () => result,
      }),
    ).resolves.toBeNull();
  await expect(
    reconcileSupersededAttemptRecords({
      savedAttempts: [saved],
      entries,
      append,
      retire,
      reconcile: async () => ({ kind: "confirmed", txHash }),
    }),
  ).resolves.toBe(saved);
  expect(retire).not.toHaveBeenCalled();
  expect(append).not.toHaveBeenCalled();
});

/** Two superseded attempts of one action, journaled and both landed, and
 * one reconciliation pass over them in which only the later is due for a read. */
const twoLandedAttempts = async (
  closing: readonly FraudProofWorkflowJournalEvent[] = [],
) => {
  const fixture = await signedWorkflowTransactionFixture({ ttl: 100 });
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: "11".repeat(32),
    category: "doubleSpend" as const,
    target: {
      kind: "state_queue_header" as const,
      headerHash: "22".repeat(28),
    },
  };
  const attempt = (txHash: string, number: number) => {
    const intent = {
      kind: "submission_intent" as const,
      actionId: "proof.init",
      actionInput: {},
      attempt: number,
      txHash,
    };
    const reconciliation = {
      kind: "reconciled" as const,
      actionId: intent.actionId,
      txHash,
      outcome: "not_found" as const,
    };
    return {
      intent,
      reconciliation,
      saved: {
        transition: {
          ...fixture.input,
          transactionHash: txHash,
          actionKind: "proof.init",
          transactionBodySha256: "55".repeat(32),
          consumedOutRefs: [],
          producedInputs: [],
        },
        workflowId: "44".repeat(32),
        intentSequence: 2 * (number - 1),
        actionId: intent.actionId,
      },
    };
  };
  const first = attempt(fixture.input.transactionHash, 1);
  const second = attempt("33".repeat(32), 2);
  let entries = [
    first.intent,
    first.reconciliation,
    second.intent,
    second.reconciliation,
    ...closing,
  ].map(
    (event, sequence): FraudProofWorkflowJournalEntry => ({
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId: "44".repeat(32),
      identity,
      sequence,
      recordedAt: "2026-10-02T00:00:00.000Z",
      event,
    }),
  );
  const append = vi.fn(async (event: FraudProofWorkflowJournalEvent) => {
    entries = [...entries, { ...entries[0]!, sequence: entries.length, event }];
  });
  const schedule = createSupersededAttemptReadSchedule();
  schedule.unresolved(first.intent.txHash, 0);
  const pass = async () =>
    await reconcileSupersededAttemptRecords({
      savedAttempts: [first.saved, second.saved],
      entries,
      append,
      retire: async () => {},
      reconcile: async ({ transition }) => ({
        kind: "confirmed",
        txHash: transition.transactionHash,
      }),
      reads: { schedule, nowMs: 1 },
    });
  return {
    first,
    second,
    append,
    pass,
    schedule,
    reason: `attempts ${first.intent.txHash} and ${second.intent.txHash} of proof.init both landed; ${first.intent.txHash} is its result`,
  };
};

it("returns the earliest of two landed attempts of one action, and records the pair once", async () => {
  const { first, second, append, pass, schedule, reason } =
    await twoLandedAttempts();
  await expect(pass()).resolves.toBe(first.saved);
  expect(append).toHaveBeenCalledTimes(1);
  expect(append.mock.calls[0]![0]).toEqual({ kind: "stalled", reason });
  schedule.forget(second.intent.txHash);
  await expect(pass()).resolves.toBe(first.saved);
  expect(append).toHaveBeenCalledTimes(1);
});

it("records two landed attempts of one action after the correction's inclusion, and nothing once the workflow's completion is final", async () => {
  // The terminal of a correction observed `depth` blocks deep.
  const terminal = (kind: "terminal_included" | "completed", depth: number) =>
    ({
      kind,
      terminal: { observedAt: { confirmationDepth: depth } },
      terminalDigest: "77".repeat(32),
    }) as unknown as FraudProofWorkflowJournalEvent;
  const { automaticRecoveryMaxDepth } = DEPLOYMENT_MANIFEST_L1_FINALITY;
  // Inclusion within the recovery window: a rollback may still reopen it.
  const included = await twoLandedAttempts([
    terminal("terminal_included", automaticRecoveryMaxDepth),
  ]);
  await expect(included.pass()).resolves.toBe(included.first.saved);
  expect(included.append.mock.calls).toEqual([
    [{ kind: "stalled", reason: included.reason }],
  ]);
  // Final completion: the journal takes no diagnostic.
  const final = await twoLandedAttempts([
    terminal("completed", automaticRecoveryMaxDepth + 2),
  ]);
  await expect(final.pass()).resolves.toBe(final.first.saved);
  expect(final.append).not.toHaveBeenCalled();
});
