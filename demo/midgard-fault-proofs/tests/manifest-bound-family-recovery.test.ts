import { expect, it, vi } from "vitest";

import {
  bindWorkflowActuationJournal,
  createWorkflowReconciliationPermitController,
} from "../src/workflow/actuation-permit.js";
import {
  COMPLETE_CANONICAL_REPLAY,
  FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT } from "../src/workflow/family-l1-observation.js";
import {
  HEADER_CLASSIFIER,
  HEADER_DECISION,
  type HeaderFaultDecision,
} from "../src/workflow/header-classifier.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
  type JournalJsonObject,
  MemoryFraudProofWorkflowJournalStore,
  normalizeJournalJson,
} from "../src/workflow/journal.js";
import {
  createCanonicalFamilyArtifactPort,
  executeManifestBoundFamilyRecovery,
} from "../src/workflow/manifest-bound-family-recovery.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
} from "../src/workflow/orchestrator.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";

const hash = (byte: string) => byte.repeat(32);
const category = "fieldItemWidthIllegal";
const headerHash = "11".repeat(28);
const deploymentFingerprint = hash("22");
const transactionHash = hash("33");
const policy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: deploymentFingerprint,
  blueprintHash: hash("44"),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};

// Local saved-decision/journal fixture. This test exercises the real opaque
// read-only permit and orchestrator; it does not claim native L1 evidence.
const recovery = async (needsAnotherAction = false) => {
  const unsealed: Omit<HeaderFaultDecision, "decisionDigest"> = {
    schemaVersion: HEADER_DECISION,
    classifierVersion: HEADER_CLASSIFIER,
    deploymentFingerprint,
    headerHash,
    authenticatedObservationDigest: hash("55"),
    payloadEnvelopeSha256: hash("66"),
    payloadSha256: hash("77"),
    replayVersion: COMPLETE_CANONICAL_REPLAY,
    replayDigest: hash("88"),
    launchScope: [category],
    launchScopeDigest: hash("99"),
    classificationDigest: hash("aa"),
    decision: "fault_detected",
    category,
    violationId: "ScriptIntegrityHashMissing",
    detectionId: "retained-local-fault",
    position: "0",
  };
  const decision = {
    ...unsealed,
    decisionDigest: journalJsonDigest(normalizeJournalJson(unsealed)),
  };
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint,
    category,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest: decision.decisionDigest,
  } as const;
  const workflowId = computeFraudProofWorkflowId(identity);
  const artifact: JournalJsonObject = {
    evidenceBinding: {
      route: "canonical_block",
      headerHash,
      payloadEnvelopeSha256: decision.payloadEnvelopeSha256,
      payloadSha256: decision.payloadSha256,
      l1BlockHash: hash("bb"),
      l1Slot: "1",
    },
    releaseFinality,
    familyArtifact: {},
  };
  const events: FraudProofWorkflowJournalEvent[] = [
    { kind: "started" },
    { kind: "prepared", artifact, artifactDigest: journalJsonDigest(artifact) },
    {
      kind: "preflight_passed",
      actionId: "retained-action",
      txHash: transactionHash,
      localEvaluator: "lucid-evolution-local-uplc-v1",
      referenceScripts: [
        {
          role: "step",
          outRef: `${hash("cc")}#0`,
          scriptHash: "dd".repeat(28),
        },
      ],
    },
    {
      kind: "submission_intent",
      actionId: "retained-action",
      actionInput: { stage: "step" },
      attempt: 1,
      txHash: transactionHash,
    },
    {
      kind: "submitted",
      actionId: "retained-action",
      attempt: 1,
      txHash: transactionHash,
    },
  ];
  const store = new MemoryFraudProofWorkflowJournalStore();
  for (const [sequence, event] of events.entries())
    await store.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId,
        identity,
        sequence,
        recordedAt: "2026-09-11T00:00:00.000Z",
        event,
      },
      sequence,
    );
  const controller = createWorkflowReconciliationPermitController({
    decision,
    deploymentFingerprint,
    rollbackGeneration: "0",
    entries: await store.load(workflowId),
  });
  const journal = bindWorkflowActuationJournal({
    journal: store,
    permit: controller.permit,
    decisionDigest: decision.decisionDigest,
    deploymentFingerprint,
    category,
    headerHash,
  });
  const forbidden = vi.fn(async (): Promise<never> => {
    throw new Error("read-only recovery entered construction or submission");
  });
  const observeHeader = vi.fn(async (): Promise<never> => {
    throw new Error("removed target cannot be fetched");
  });
  const reconcile = vi.fn(async (input: { txHash?: string }) => {
    expect(input.txHash).toBe(transactionHash);
    return { kind: "confirmed", txHash: transactionHash } as const;
  });
  const workflow: Parameters<typeof executeManifestBoundFamilyRecovery>[0] = {
    sources: [],
    journal,
    replayer: FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
    binding: {
      deploymentFingerprint,
      definition: { headerHash, category },
    } as Parameters<typeof executeManifestBoundFamilyRecovery>[0]["binding"],
    l1: {
      portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
      category,
      observeHeader,
      observe: forbidden,
      transactionConfirmed: forbidden,
      publications: {
        observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
        observeExact: forbidden,
      },
    },
    decisionDigest: decision.decisionDigest,
    adapter: {
      adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
      category,
      safety: FRAUD_PROOF_WORKFLOW_SAFETY,
      prepare: forbidden,
      preflight: forbidden,
      submit: forbidden,
      reconcile,
      observe: async () =>
        needsAnotherAction
          ? ({
              kind: "action_required",
              action: { actionId: "new-action", input: {} },
            } as const)
          : ({
              kind: "pending",
              reason: "awaiting canonical terminal evidence",
            } as const),
    },
    terminalVerifier: {
      verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
      verify: forbidden,
    },
    releaseFinalityAuthority: {
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      verifyForWorkflow: async () => releaseFinality,
    },
  };
  return {
    journal,
    workflow,
    workflowId,
    controller,
    forbidden,
    observeHeader,
    reconcile,
  };
};

it.each([false, true])(
  "reconciles the exact saved intent without a live header or new action (%s)",
  async (needsAnotherAction) => {
    const fixture = await recovery(needsAnotherAction);
    const result = await executeManifestBoundFamilyRecovery(fixture.workflow);
    expect(result.kind).toBe(needsAnotherAction ? "stalled" : "pending");
    expect(fixture.reconcile).toHaveBeenCalledTimes(1);
    expect(fixture.observeHeader).not.toHaveBeenCalled();
    expect(fixture.forbidden).not.toHaveBeenCalled();
    const entries = await fixture.journal.load(fixture.workflowId);
    expect(
      entries.filter(({ event }) => event.kind === "submission_intent"),
    ).toHaveLength(1);
    expect(
      entries.some(
        ({ event }) =>
          event.kind === "confirmed" && event.txHash === transactionHash,
      ),
    ).toBe(true);
  },
);

it("refuses reconciliation after actual rollback revokes the permit", async () => {
  const fixture = await recovery();
  fixture.controller.revoke("native_chain_rollback");
  await expect(
    executeManifestBoundFamilyRecovery(fixture.workflow),
  ).rejects.toThrow("revoked");
  expect(fixture.reconcile).not.toHaveBeenCalled();
  expect(fixture.observeHeader).not.toHaveBeenCalled();
});

it("refuses a different decision before opening a removed target", async () => {
  const fixture = await recovery();
  await expect(
    executeManifestBoundFamilyRecovery({
      ...fixture.workflow,
      decisionDigest: hash("ef"),
    }),
  ).rejects.toThrow("changed decision digest");
  expect(fixture.reconcile).not.toHaveBeenCalled();
  expect(fixture.observeHeader).not.toHaveBeenCalled();
});

it("refuses a foreign adapter before reconciling the recorded transaction", async () => {
  const fixture = await recovery();
  await expect(
    executeManifestBoundFamilyRecovery({
      ...fixture.workflow,
      adapter: { ...fixture.workflow.adapter, category: "doubleSpend" },
    }),
  ).rejects.toThrow("changed deployment category");
  expect(fixture.reconcile).not.toHaveBeenCalled();
  expect(fixture.observeHeader).not.toHaveBeenCalled();
});

it("re-admits exact typed material after restart without deserializing saved JSON", async () => {
  const fresh = () => ({
    value: 9_007_199_254_740_993n,
    bytes: Buffer.from("cafe", "hex"),
  });
  // This isolated port test supplies the derivation result; family tests own canonical replay.
  const input = {} as Parameters<
    ReturnType<typeof createCanonicalFamilyArtifactPort>["prepare"]
  >[0];
  const first = createCanonicalFamilyArtifactPort(async () => fresh());
  const artifact = await first.prepare(input);
  const saved = JSON.parse(JSON.stringify(artifact)) as JournalJsonObject;
  const restarted = createCanonicalFamilyArtifactPort(async () => fresh());
  expect(() => restarted.require(saved)).toThrow("was not admitted");
  await restarted.validatePreparedArtifact({ ...input, artifact: saved });
  expect(restarted.require(saved)).toEqual(fresh());
  expect(Buffer.isBuffer(restarted.require(saved).bytes)).toBe(true);
});

it("refuses changed replay material and discards prior admission after a failed recheck", async () => {
  let value = 1n;
  const port = createCanonicalFamilyArtifactPort(async () => ({ value }));
  const input = {} as Parameters<typeof port.prepare>[0];
  const artifact = await port.prepare(input);
  value = 2n;
  await expect(
    port.validatePreparedArtifact({ ...input, artifact }),
  ).rejects.toThrow("freshly authenticated material");
  expect(() => port.require(artifact)).toThrow("was not admitted");
});

it("compares an explicit durable projection while retaining only newly derived typed material", async () => {
  class Authentication {
    constructor(readonly bytes: Buffer) {}
  }
  const derive = async () => ({
    authentication: new Authentication(Buffer.from("cafe", "hex")),
  });
  const port = () =>
    createCanonicalFamilyArtifactPort(derive, (material) => ({
      authentication: material.authentication.bytes.toString("hex"),
    }));
  const first = port();
  const input = {} as Parameters<typeof first.prepare>[0];
  const saved = JSON.parse(JSON.stringify(await first.prepare(input)));
  const restarted = port();
  await restarted.validatePreparedArtifact({ ...input, artifact: saved });
  expect(restarted.require(saved).authentication).toBeInstanceOf(
    Authentication,
  );
  expect(restarted.require(saved).authentication.bytes.toString("hex")).toBe(
    "cafe",
  );
});
