import { createHash } from "node:crypto";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import type { ValidationTraceDisputeChainStage } from "../../src/validation-dispute/workflow-chain-state.js";
import { validationTraceFieldCarriageAction } from "../../src/validation-dispute/workflow-field-carriage.js";
import type { ManifestBoundValidationTraceDisputeWorkflow } from "../../src/validation-dispute/workflow-v1.create-manifest-bound-validation-trace-dispute-workflow.js";
import { runOrResumeManifestBoundValidationTraceDisputeWorkflow } from "../../src/validation-dispute/workflow-v1.js";
import { createValidationTraceDisputeRecoveryAdapter } from "../../src/validation-dispute/workflow-v1.recovery-adapter.js";
import {
  bindWorkflowActuationJournal,
  createWorkflowReconciliationPermitController,
} from "../../src/workflow/actuation-permit.js";
import { COMPLETE_CANONICAL_REPLAY } from "../../src/workflow/complete-replay.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
} from "../../src/workflow/family-l1-observation.js";
import { FIELD_CARRIAGE_PREREQUISITE } from "../../src/workflow/field-carriage-prerequisite.js";
import * as funding from "../../src/workflow/funding-reservation-permit.js";
import {
  HEADER_CLASSIFIER,
  HEADER_DECISION,
  type HeaderFaultDecision,
} from "../../src/workflow/header-classifier.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  MemoryFraudProofWorkflowJournalStore,
  normalizeJournalJson,
} from "../../src/workflow/journal.js";
import type { FraudProofWorkflowAdapterContext } from "../../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../../src/workflow/raw-l1-publication-observation.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-finality-policy.js";
import type {
  SignedTransactionRecoveryObservation,
  SignedWorkflowTransaction,
} from "../../src/workflow/signed-transaction-reconciliation.js";

// These tests exercise the interactive adapter/common lifecycle boundary with
// real signed CML bytes and opaque recovery authority. Builders and native L1
// derivation remain covered by the installed emulator lifecycle tests.
const hash = (byte: string) => byte.repeat(32);
const category = "validationTraceDispute" as const;
const headerHash = "11".repeat(28);
const deploymentFingerprint = hash("22");
const target = `${hash("34")}#0`;
const child = `${hash("35")}#0`;
const proofHash = hash("36");
const proof = `${proofHash}#0`;
const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x33));
const inputs = CML.TransactionInputList.new();
for (const byte of ["34", "35"])
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(hash(byte)), 0n),
  );
const outputs = CML.TransactionOutputList.new();
outputs.add(
  CML.TransactionOutput.new(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(key.to_public().hash()),
    ).to_address(),
    CML.Value.from_coin(2_000_000n),
  ),
);
const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
const witnesses = CML.TransactionWitnessSet.new();
const vkeys = CML.VkeywitnessList.new();
vkeys.add(
  CML.Vkeywitness.new(
    key.to_public(),
    key.sign(CML.hash_transaction(body).to_raw_bytes()),
  ),
);
witnesses.set_vkeywitnesses(vkeys);
const transaction = CML.Transaction.new(body, witnesses, true);
export const signed: SignedWorkflowTransaction = {
  transactionHash: CML.hash_transaction(body).to_hex(),
  signedTransactionCborHex: transaction.to_cbor_hex(),
};
export const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: deploymentFingerprint,
  blueprintHash: hash("44"),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};
const familyArtifact = {
  schemaVersion: "midgard-validation-trace-dispute-prepared-v1",
  challengeDigest: hash("45"),
  claimCbor: "00",
  challengerDescriptorCbor: "00",
};
export const action = validationTraceFieldCarriageAction({
  stage: "remove",
  stateQueueBlockOutRef: target,
  nextRemovalOutRef: child,
  fraudProofOutRef: proof,
});
export const durableRecovery = {
  challengeDigest: familyArtifact.challengeDigest,
  actionDigest: journalJsonDigest(action),
  stateQueueMutationLease: { token: "retained-lease", source: "controlled-L1" },
};

export const observation = (
  status: SignedTransactionRecoveryObservation["status"],
): SignedTransactionRecoveryObservation => {
  const point = {
    slot: "1000",
    blockNo: "50",
    blockHash: hash("ab"),
    pointId: hash("cd"),
  };
  return {
    ...signed,
    status,
    canonicalPoint: point,
    releaseFinalPoint: point,
    inputs: [],
    reason: "controlled canonical signed-input outcome",
  };
};

export const mechanics = () => {
  const forbidden = vi.fn(async (): Promise<never> => {
    throw new Error("recovery tried to sign, publish or fetch old evidence");
  });
  const lease = {
    token: "retained-lease",
    source: "controlled-L1",
    renew: vi.fn(async () => {}),
    release: vi.fn(async () => {}),
    fail: vi.fn(async () => {}),
  };
  const resume = vi.fn(async () => lease);
  let stage: ValidationTraceDisputeChainStage = {
    kind: "proof_token",
    stateQueueBlockOutRef: target,
    nextRemovalOutRef: child,
    fraudProofOutRef: proof,
  };
  let terminal: FraudProofWorkflowTerminal | undefined;
  const confirmed = vi.fn(async () => false);
  const observeSignedTransaction = vi.fn(async () => observation("unknown"));
  const rebroadcastSignedTransaction = vi.fn(
    async ({
      authorizeResubmission,
      ...tx
    }: SignedWorkflowTransaction & {
      authorizeResubmission: (tx: SignedWorkflowTransaction) => Promise<void>;
    }) => {
      await authorizeResubmission(tx);
      return tx.transactionHash;
    },
  );
  const l1 = {
    portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
    category,
    observeHeader: forbidden,
    transactionConfirmed: confirmed,
    observeSignedTransaction,
    rebroadcastSignedTransaction,
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: forbidden,
    },
    observe: vi.fn(async () => ({
      provenance: {
        trustClass: "authenticated_cardano_l1" as const,
        sourceId: "controlled-L1",
        grade: "security" as const,
      },
      stage:
        terminal === undefined
          ? {
              kind: "proof_token" as const,
              stateQueueBlockOutRef: target,
              nextRemovalOutRef: child,
              fraudProofOutRef: proof,
            }
          : { kind: "removed" as const, terminal },
    })),
  };
  const binding = {
    deploymentFingerprint,
    definition: { category, headerHash },
  };
  const workflow = {
    binding: binding as Parameters<
      typeof createValidationTraceDisputeRecoveryAdapter
    >[0]["workflow"]["binding"],
    challenge: undefined,
    material: undefined,
    l1,
    actuator: { capture: forbidden },
    fieldCarriage: {
      prerequisite: {
        portVersion: FIELD_CARRIAGE_PREREQUISITE,
        category,
        inspect: async () => ({ kind: "not_required" as const }),
        capture: forbidden,
        reconcile: forbidden,
        resolveAuthenticated: forbidden,
      },
      resolve: forbidden,
    },
    deriveStage: async () => stage,
  };
  const adapter = createValidationTraceDisputeRecoveryAdapter({
    workflow,
    stateQueueMutationLeaseCoordinator: { acquire: forbidden, resume },
  });
  const context: FraudProofWorkflowAdapterContext = {
    workflowId: "controlled-workflow",
    identity: {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint,
      category,
      target: { kind: "state_queue_header", headerHash },
    },
    artifact: familyArtifact,
    entries: [],
  };
  return {
    workflow,
    adapter,
    context,
    forbidden,
    lease,
    resume,
    confirmed,
    l1,
    setStage: (value: ValidationTraceDisputeChainStage) => {
      stage = value;
    },
    setTerminal: (value: FraudProofWorkflowTerminal | undefined) => {
      terminal = value;
    },
  };
};

export const terminal = (depth: number): FraudProofWorkflowTerminal => ({
  schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  category,
  headerHash,
  proofToken: {
    unit: "11".repeat(28),
    outRef: proof,
    createdByTxHash: proofHash,
    retainedAtFinalState: true,
  },
  correction: {
    removalTxHash: signed.transactionHash,
    removedStateQueueOutRef: target,
    fraudulentHeaderAbsent: true,
    referencedProofTokenOutRef: proof,
  },
  economics: {
    operatorCredential: "55".repeat(28),
    proverCredential: "66".repeat(28),
    operatorBondInputOutRef: `${hash("77")}#0`,
    operatorBondInputLovelace: "10000000",
    slashedLovelace: "10000000",
    proverRewardOutputOutRef: `${signed.transactionHash}#0`,
    proverRewardLovelace: "5000000",
    removalFeeLovelace: "200000",
    duplicateRewardAbsent: true,
  },
  observedAt: { slot: "4242", blockHash: hash("88"), confirmationDepth: depth },
});

export const recordedRecovery = async () => {
  const f = mechanics();
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
    violationId: "validation-trace-dispute",
    detectionId: "retained-validation-fault",
    position: "0",
  };
  const decision = {
    ...unsealed,
    decisionDigest: journalJsonDigest(normalizeJournalJson(unsealed)),
  };
  const identity = {
    ...f.context.identity,
    decisionDigest: decision.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const artifact = {
    evidenceBinding: {
      route: "canonical_block",
      headerHash,
      payloadEnvelopeSha256: decision.payloadEnvelopeSha256,
      payloadSha256: decision.payloadSha256,
      l1BlockHash: hash("bb"),
      l1Slot: "1",
    },
    releaseFinality,
    familyArtifact,
  };
  const events: FraudProofWorkflowJournalEvent[] = [
    { kind: "started" },
    { kind: "prepared", artifact, artifactDigest: journalJsonDigest(artifact) },
    {
      kind: "preflight_passed",
      actionId: "award-proof",
      txHash: proofHash,
      localEvaluator: "lucid-evolution-local-uplc-v1",
      referenceScripts: [
        {
          role: "award",
          outRef: `${hash("cc")}#0`,
          scriptHash: "dd".repeat(28),
        },
      ],
    },
    {
      kind: "submission_intent",
      actionId: "award-proof",
      actionInput: { stage: "award" },
      attempt: 1,
      txHash: proofHash,
    },
    {
      kind: "reconciled",
      actionId: "award-proof",
      txHash: proofHash,
      outcome: "confirmed",
    },
    { kind: "confirmed", actionId: "award-proof", txHash: proofHash },
    {
      kind: "preflight_passed",
      actionId: action.actionId,
      txHash: signed.transactionHash,
      localEvaluator: "lucid-evolution-local-uplc-v1",
      referenceScripts: [
        {
          role: "remove",
          outRef: `${hash("cc")}#0`,
          scriptHash: "dd".repeat(28),
        },
      ],
    },
    {
      kind: "submission_intent",
      actionId: action.actionId,
      actionInput: action.input,
      durableRecovery,
      attempt: 1,
      txHash: signed.transactionHash,
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
        recordedAt: "2026-10-04T00:00:00.000Z",
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
  vi.spyOn(funding, "readWorkflowFundingRecovery").mockResolvedValue({
    transition: {
      actionKind: "remove",
      ...signed,
      transactionBodySha256: createHash("sha256")
        .update(Buffer.from(body.to_cbor_hex(), "hex"))
        .digest("hex"),
      consumedOutRefs: [],
      producedInputs: [],
    },
    submissionHandoff: null,
    completionHandoff: null,
    abandonmentHandoff: null,
  });
  const releaseFunding = vi.spyOn(funding, "releaseWorkflowFundingReservation");
  const workflow: ManifestBoundValidationTraceDisputeWorkflow = {
    ...f.workflow,
    get lucid(): ManifestBoundValidationTraceDisputeWorkflow["lucid"] {
      throw new Error(
        "reconciliation-only workflow tried to access its signer Lucid",
      );
    },
    get signer(): ManifestBoundValidationTraceDisputeWorkflow["signer"] {
      throw new Error(
        "reconciliation-only workflow tried to access its signer",
      );
    },
    decisionDigest: decision.decisionDigest,
    adapter: f.adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(
      f.l1,
    ),
    releaseFinalityAuthority: {
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      verifyForWorkflow: async () => releaseFinality,
    },
  };
  const execute = () =>
    runOrResumeManifestBoundValidationTraceDisputeWorkflow({
      workflow,
      journal,
      sources: [],
    });
  return { ...f, execute, journal, workflowId, controller, releaseFunding };
};
