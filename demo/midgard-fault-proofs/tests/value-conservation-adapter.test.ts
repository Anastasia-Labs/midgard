import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it, vi } from "vitest";

import { createValueConservationAdapter } from "../src/value-not-preserved/adapter.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
} from "../src/workflow/journal.js";
import type { LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";
const hash = (byte: string) => byte.repeat(32);
const outRef = (byte: string, index = 0) => `${hash(byte)}#${index}`;
const txHash = hash("44");
const referenceOutRef = outRef("55");
const signed = ({
  submittedHash = txHash,
  includedReferenceOutRef = referenceOutRef,
  inlineScriptKind,
}: {
  readonly submittedHash?: string;
  readonly includedReferenceOutRef?: string;
  readonly inlineScriptKind?: "native" | "plutusV1" | "plutusV2" | "plutusV3";
} = {}): LocallyEvaluatedTransaction["signed"] => {
  const [referenceTxHash, referenceIndex] = includedReferenceOutRef.split("#");
  return {
    toHash: () => txHash,
    submit: async () => submittedHash,
    toTransaction: () => ({
      witness_set: () => ({
        native_scripts: () =>
          inlineScriptKind === "native" ? { len: () => 1 } : undefined,
        plutus_v1_scripts: () =>
          inlineScriptKind === "plutusV1" ? { len: () => 1 } : undefined,
        plutus_v2_scripts: () =>
          inlineScriptKind === "plutusV2" ? { len: () => 1 } : undefined,
        plutus_v3_scripts: () =>
          inlineScriptKind === "plutusV3" ? { len: () => 1 } : undefined,
      }),
      body: () => ({
        reference_inputs: () => ({
          len: () => 1,
          get: () => ({
            transaction_id: () => ({ to_hex: () => referenceTxHash! }),
            index: () => BigInt(referenceIndex!),
          }),
        }),
      }),
    }),
  } as unknown as LocallyEvaluatedTransaction["signed"];
};

const transaction = (
  overrides: Partial<LocallyEvaluatedTransaction> = {},
): LocallyEvaluatedTransaction => ({
  txHash,
  signed: signed(),
  referenceScripts: [
    {
      role: "V1 fraud-proof value-not-preserved step-01",
      outRef: referenceOutRef,
      scriptHash: "66".repeat(28),
    },
  ],
  ...overrides,
});

const identity = {
  schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  deploymentFingerprint: hash("aa"),
  category: "valueNotPreserved",
  target: { kind: "state_queue_header", headerHash: "ab".repeat(28) },
} as const;
const action = {
  actionId: "fold:9:thread",
  input: { stage: "fold", index: 9 },
} as const;
const context = {
  identity,
  workflowId: computeFraudProofWorkflowId(identity),
  artifact: { retained: "authenticated-value-source" },
  entries: [],
  action,
} as const;
const fixture = () => {
  const lease = {
    token: "leased-exact-outref",
    source: "state-queue",
    renew: vi.fn(async () => undefined),
    release: vi.fn(async () => undefined),
    fail: vi.fn(async (_reason: string) => undefined),
  };
  const resume = vi.fn(async () => lease);
  const confirmed = vi.fn(async () => false);
  const capture = vi.fn(async () => ({
    transaction: transaction(),
    mutationLease: lease,
  }));
  const make = () =>
    createValueConservationAdapter({
      prepare: async () => context.artifact,
      current: async () => ({ kind: "action_required", action }),
      capture,
      confirmed,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => lease,
        resume,
      },
    });
  return { make, lease, resume, confirmed, capture };
};
describe("value conservation durable adapter", () => {
  it("resumes a fsynced intent and exact mutation lease through a fresh adapter", async () => {
    const f = fixture();
    const adapter = f.make();
    const preflight = await adapter.preflight(context);
    const directory = await mkdtemp(
      join(tmpdir(), "value-conservation-journal-"),
    );
    try {
      const journal = new DirectoryFraudProofWorkflowJournalStore(directory);
      const events: FraudProofWorkflowJournalEvent[] = [
        { kind: "started" },
        {
          kind: "prepared",
          artifact: context.artifact,
          artifactDigest: journalJsonDigest(context.artifact),
        },
        {
          kind: "preflight_passed",
          actionId: action.actionId,
          txHash: preflight.txHash,
          localEvaluator: preflight.localUplcEvaluation.evaluator,
          referenceScripts: preflight.referenceScripts,
        },
        {
          kind: "submission_intent",
          actionId: action.actionId,
          actionInput: action.input,
          attempt: 1,
          txHash: preflight.txHash,
          durableRecovery: preflight.durableRecovery,
        },
      ];
      for (const [sequence, event] of events.entries())
        await journal.append(
          {
            schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
            identity,
            workflowId: context.workflowId,
            sequence,
            recordedAt: "2026-09-05T00:00:00.000Z",
            event,
          },
          sequence,
        );
      expect(await adapter.submit({ ...context, preflight })).toEqual({
        kind: "submitted",
        txHash,
      });
      const entries = await new DirectoryFraudProofWorkflowJournalStore(
        directory,
      ).load(context.workflowId);
      const intent = entries[3]!.event;
      if (intent.kind !== "submission_intent")
        throw new Error("missing durable intent");
      const fresh = f.make();
      const restart = {
        ...context,
        entries,
        txHash: intent.txHash,
        durableRecovery: intent.durableRecovery,
      };
      expect(await fresh.reconcile(restart)).toEqual({
        kind: "pending",
        txHash,
      });
      expect(f.resume).toHaveBeenCalledWith({
        token: f.lease.token,
        source: f.lease.source,
      });
      expect(f.lease.renew).toHaveBeenCalledOnce();
      f.confirmed.mockResolvedValue(true);
      expect(await fresh.reconcile(restart)).toEqual({
        kind: "confirmed",
        txHash,
      });
      expect(f.lease.release).toHaveBeenCalledOnce();
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
  it("refuses substituted cursors before capture and duplicate preflight", async () => {
    const f = fixture();
    const adapter = f.make();
    await expect(
      adapter.preflight({
        ...context,
        action: { ...action, input: { ...action.input, index: 10 } },
      }),
    ).rejects.toThrow("authenticated current checkpoint");
    expect(f.capture).not.toHaveBeenCalled();
    await adapter.preflight({
      ...context,
      action: { input: { index: 9, stage: "fold" }, actionId: action.actionId },
    });
    await expect(adapter.preflight(context)).rejects.toThrow(
      "outstanding preflight",
    );
  });
  it("refuses substituted body hashes and durable lease identities", async () => {
    const f = fixture();
    const adapter = f.make();
    const preflight = await adapter.preflight(context);
    await expect(
      adapter.submit({
        ...context,
        preflight: { ...preflight, txHash: hash("66") },
      }),
    ).rejects.toThrow("matching locally evaluated body");
    await expect(
      adapter.submit({
        ...context,
        preflight: {
          ...preflight,
          durableRecovery: {
            stateQueueMutationLease: {
              token: "another",
              source: f.lease.source,
            },
          },
        },
      }),
    ).rejects.toThrow("lease differs");
  });
  it("fails the resumed lease when authenticated confirmation detects mutation", async () => {
    const f = fixture();
    f.confirmed.mockRejectedValue(new Error("changed output checkpoint"));
    expect(
      await f.make().reconcile({
        ...context,
        txHash,
        durableRecovery: {
          stateQueueMutationLease: {
            token: f.lease.token,
            source: f.lease.source,
          },
        },
      }),
    ).toEqual({ kind: "conflict", reason: "Error: changed output checkpoint" });
    expect(f.lease.fail).toHaveBeenCalledOnce();
    expect(f.lease.release).not.toHaveBeenCalled();
  });
});
