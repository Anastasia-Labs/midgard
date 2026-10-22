import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML } from "@lucid-evolution/lucid";

import { createWorkflowActuationPermitController } from "../src/workflow/actuation-permit.js";
import { DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  unsafeCreateWorkflowFundingReservationPermitForTest,
  type WorkflowJournalWriter,
} from "../src/workflow/funding-reservation-permit.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "../src/workflow/header-classifier.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

export const DEPLOYMENT = "d7".repeat(32);

const RELEASE_FINALITY_POLICY = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };

export const admittedActuation = async () => {
  const sharedInput = outRefCbor(91, 0n);
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({ spendInputs: [sharedInput], fee: 1n }),
      buildFixtureTransaction({ spendInputs: [sharedInput], fee: 2n }),
    ],
  });
  const observation = authenticatedHeaderObservation(fixture);
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: DEPLOYMENT,
    replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority: {
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      verifyForWorkflow: async () => ({
        schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
        deploymentIdentityDigest: DEPLOYMENT,
        blueprintHash: "f7".repeat(32),
        policyDigest: computeFraudProofReleaseFinalityPolicyDigest(
          RELEASE_FINALITY_POLICY,
        ),
        policy: RELEASE_FINALITY_POLICY,
      }),
    },
  });
  const decision = await classifyHeader({
    classifier,
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth:
          DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
      }),
    sources: [
      {
        sourceId: "libp2p-test",
        fetchPayloadByHeaderHash: async () => ({
          ok: true as const,
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: "libp2p-test/peer-a",
            grade: "security" as const,
          },
          sourceId: "libp2p-test",
          sourcePeerId: "peer-a",
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          attempts: [],
        }),
      },
    ],
  });
  if (decision.decision !== "fault_detected") {
    throw new Error("runtime test failed to classify its fault fixture");
  }
  const controller = createWorkflowActuationPermitController({
    decision,
    rollbackGeneration: "7",
  });
  const fundingReservationPermit =
    unsafeCreateWorkflowFundingReservationPermitForTest({
      category: "doubleSpend",
      actuationPermit: controller.permit,
      deploymentFingerprint: DEPLOYMENT,
      decisionDigest: decision.decisionDigest,
      rollbackGeneration: "7",
    });
  return Object.freeze({
    decisionDigest: decision.decisionDigest,
    actuationPermit: controller.permit,
    fundingReservationPermit,
    headerHash: decision.headerHash,
    revoke: controller.revoke,
    restrictToReconciliation: controller.restrictToReconciliation,
  });
};

/** The preflight and intent a submission of `action` records, over the
 * fixture's real CML bytes. */
export const fundingAttemptEvents = (
  action: FraudProofWorkflowAction,
  preflight: object,
): readonly [
  FraudProofWorkflowJournalEvent,
  FraudProofWorkflowJournalEvent,
] => {
  const signed = workflowPreflightTransaction(preflight)!;
  return [
    {
      kind: "preflight_passed",
      actionId: action.actionId,
      txHash: signed.toHash(),
      localEvaluator: "funding-boundary-fixture",
      referenceScripts: [],
    },
    {
      kind: "submission_intent",
      actionId: action.actionId,
      actionInput: action.input,
      txHash: signed.toHash(),
      attempt: 1,
    },
  ];
};

/** A workflow journal that keeps nothing: funding-boundary fixtures check
 * the reservation, not the journal. */
export const discardingWorkflowJournal = (): FraudProofWorkflowJournalStore =>
  Object.freeze({ load: async () => [], append: async () => undefined });

/** A writer over a funding-bound fixture journal: its entries follow the
 * workflow's started and prepared records. */
export const fixtureJournalWriter = (
  journal: object,
  headerHash = "ab".repeat(28),
): WorkflowJournalWriter => {
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: DEPLOYMENT,
    category: "doubleSpend",
    target: { kind: "state_queue_header", headerHash },
    decisionDigest: "ac".repeat(32),
  };
  let next = 2;
  return Object.freeze({
    journal: journal as FraudProofWorkflowJournalStore,
    entriesFor: async (events) =>
      events.map((event, index) => ({
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId: computeFraudProofWorkflowId(identity),
        identity,
        sequence: next + index,
        recordedAt: new Date(0).toISOString(),
        event,
      })),
    recorded: (entries) => {
      next += entries.length;
    },
  });
};

export const fundingKey = CML.PrivateKey.from_normal_bytes(
  Buffer.alloc(32, 0x51),
);

export const fundingAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(fundingKey.to_public().hash()),
)
  .to_address()
  .to_bech32();

export const fundingReferenceOutRef = `${"74".repeat(32)}#0`;

export const fundingReferenceScript = Object.freeze({
  type: "PlutusV3" as const,
  script: "4d01000033222220051200120011",
});

/** A fresh directory journal store for one invocation: each invocation binds
 * its own actuation authority to the store object it is given. */
export const directoryJournal = (directory: string) =>
  new DirectoryFraudProofWorkflowJournalStore(directory);
