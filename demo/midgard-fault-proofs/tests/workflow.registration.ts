import "./workflow.canonical-reobservation-with-pending-descendants.js";

import { expect, it, vi } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { reconcileWorkflowFundingSubmissionHandoff } from "../src/workflow/funding-reservation-permit.js";
import {
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
  MemoryFraudProofWorkflowJournalStore,
  validateFraudProofWorkflowJournal,
} from "../src/workflow/journal.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  canonicalEvidence,
  makeAdapter,
  PROOF_TX_HASH,
  REMOVAL_TX_HASH,
  run,
} from "./workflow.make-adapter.js";

it.each(["prove", "remove", "superseded"])(
  "reconciles a restored funding cursor after a journal-write crash: %s",
  async (scenario) => {
    const actionId = scenario === "remove" ? "remove" : "prove";
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) =>
        txHash === REMOVAL_TX_HASH
          ? { kind: "pending", txHash }
          : { kind: "confirmed", txHash: txHash! },
    });
    const first = await run({ evidence, adapter, journal });
    if (first.kind !== "pending")
      throw new Error("fixture did not leave its child pending");
    const entries = [...first.entries];
    const append = (event: FraudProofWorkflowJournalEvent) =>
      entries.push({
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId: first.workflowId,
        identity: first.identity,
        sequence: entries.length,
        recordedAt: "2026-08-29T00:00:00.000Z",
        event,
      });
    if (actionId === "remove") {
      // The parent has already been recovered; the pending child is being selected again.
      append({ kind: "reobserved", actionId: "prove", txHash: PROOF_TX_HASH });
      append({
        kind: "reconciled",
        actionId: "prove",
        outcome: "confirmed",
        txHash: PROOF_TX_HASH,
      });
      append({ kind: "confirmed", actionId: "prove", txHash: PROOF_TX_HASH });
    }
    const prepared = entries[1]!.event;
    const index = entries.findIndex(
      ({ event }) =>
        event.kind === "preflight_passed" && event.actionId === actionId,
    );
    const preflight = entries[index]!.event;
    const submissionIntent = entries[index + 1]!.event;
    if (
      prepared.kind !== "prepared" ||
      preflight.kind !== "preflight_passed" ||
      submissionIntent.kind !== "submission_intent"
    )
      throw new Error("fixture lacks its exact prepared submission");
    if (scenario === "superseded") {
      const replacementHash = "b3".repeat(32);
      append({ kind: "reobserved", actionId: "prove", txHash: PROOF_TX_HASH });
      append({
        kind: "reconciled",
        actionId: "prove",
        outcome: "not_found",
        txHash: PROOF_TX_HASH,
      });
      append({ ...preflight, txHash: replacementHash });
      append({ ...submissionIntent, attempt: 2, txHash: replacementHash });
    }
    const recover = () =>
      reconcileWorkflowFundingSubmissionHandoff({
        handoff: {
          workflowId: first.workflowId,
          identity: first.identity,
          preparedArtifactDigest: prepared.artifactDigest,
          expectedJournalSequence: index,
          preflight,
          submissionIntent,
        },
        entries,
      });
    if (scenario === "superseded") {
      expect(recover).toThrow("superseded by a later submission intent");
      return;
    }
    const recovered = recover();
    expect(recovered).toEqual([
      { kind: "reobserved", actionId, txHash: submissionIntent.txHash },
    ]);
    for (const event of recovered) append(event);
    expect(() =>
      validateFraudProofWorkflowJournal({
        workflowId: first.workflowId,
        entries,
        expectedIdentity: first.identity,
      }),
    ).not.toThrow();
  },
);

it("reuses proof material after the same authenticated commitment is re-included at a new L1 point", async () => {
  const fixture = await buildCanonicalBlockFixture({ transactions: [] });
  const evidenceAt = (slot: bigint, blockHash: string) =>
    canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture, {
        chainPoint: { slot, blockHash },
      }),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/peer-a",
        grade: "security",
      },
    });
  const original = await evidenceAt(4242n, "31".repeat(32));
  const relocated = await evidenceAt(4300n, "32".repeat(32));
  const journal = new MemoryFraudProofWorkflowJournalStore();
  const adapter = makeAdapter({
    reconcile: async ({ txHash }) => ({ kind: "pending", txHash }),
  });
  const validate = vi.fn(async () => undefined);
  adapter.validatePreparedArtifact = validate;
  const first = await run({ evidence: original, adapter, journal });
  expect(first.kind).toBe("pending");
  if (first.kind !== "pending")
    throw new Error("expected pending original intent");
  const prepared = first.entries.find(
    ({ event }) => event.kind === "prepared",
  )!;
  const resumed = await run({ evidence: relocated, adapter, journal });
  expect(resumed.kind).toBe("pending");
  expect(validate).toHaveBeenCalledWith(
    expect.objectContaining({ evidence: relocated }),
  );
  expect(adapter.prepare).toHaveBeenCalledOnce();
  expect(adapter.submit).toHaveBeenCalledOnce();
  expect(
    (await journal.load(first.workflowId)).find(
      ({ event }) => event.kind === "prepared",
    ),
  ).toEqual(prepared);
  // The original source locator remains in the immutable artifact for audit.
  expect(prepared.event).toMatchObject({
    artifact: {
      evidenceBinding: { l1Slot: "4242", l1BlockHash: "31".repeat(32) },
    },
  });
});

it.each(["headerHash", "payloadEnvelopeSha256", "payloadSha256"])(
  "rejects persisted %s substitution when reusing relocated evidence",
  async (field) => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => ({ kind: "pending", txHash }),
    });
    const first = await run({ evidence, adapter, journal });
    if (first.kind !== "pending")
      throw new Error("expected pending original intent");
    const forged = structuredClone(first.entries);
    const prepared = forged.find(
      ({ event }) => event.kind === "prepared",
    )!.event;
    if (prepared.kind !== "prepared")
      throw new Error("expected prepared evidence");
    const originalBinding = prepared.artifact.evidenceBinding;
    if (
      typeof originalBinding !== "object" ||
      originalBinding === null ||
      Array.isArray(originalBinding)
    )
      throw new Error("missing evidence binding");
    const replacement = {
      ...prepared.artifact,
      evidenceBinding: {
        ...originalBinding,
        [field]: "ef".repeat(field === "headerHash" ? 28 : 32),
      },
    };
    const entries = forged.map((entry) =>
      entry.event.kind === "prepared"
        ? {
            ...entry,
            event: {
              ...entry.event,
              artifact: replacement,
              artifactDigest: journalJsonDigest(replacement),
            },
          }
        : entry,
    );
    const hostileJournal: FraudProofWorkflowJournalStore = {
      load: async () => entries,
      append: async () => {
        throw new Error("must not append");
      },
    };
    await expect(
      run({ evidence, adapter, journal: hostileJournal }),
    ).rejects.toThrow("does not match the proof-critical artifact");
    expect(adapter.submit).toHaveBeenCalledOnce();
  },
);
