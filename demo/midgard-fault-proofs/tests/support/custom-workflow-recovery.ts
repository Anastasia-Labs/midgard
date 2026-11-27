import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { createWorkflowReconciliationPermitController } from "../../src/workflow/actuation-permit.create-workflow-reconciliation-permit-controller.js";
import { COMPLETE_CANONICAL_REPLAY } from "../../src/workflow/complete-replay.replay-context-identity.js";
import { CURSOR_FAMILY_TRANSACTION_PORT } from "../../src/workflow/cursor-family-adapter.admit-reference-scripts.js";
import { createCursorFamilyWorkflowAdapter } from "../../src/workflow/cursor-family-adapter.create-cursor-family-workflow-adapter.js";
import {
  cursorFamilyObservation,
  type CursorFamilySpec,
} from "../../src/workflow/cursor-family-state.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
} from "../../src/workflow/family-l1-observation.js";
import {
  HEADER_CLASSIFIER,
  HEADER_DECISION,
  type HeaderFaultDecision,
} from "../../src/workflow/header-classifier.authenticated-state-queue-observation-digest.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  normalizeWorkflowJson,
  workflowJsonDigest,
} from "../../src/workflow/identity.js";
import type { InstalledFraudProofCategory } from "../../src/workflow/installed-categories.js";
import type { FraudProofRawL1FamilyStage } from "../../src/workflow/raw-l1-family-derivation.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../../src/workflow/raw-l1-publication-observation.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-finality-policy.js";
import type { LocallyEvaluatedTransaction } from "../../src/workflow/transaction-boundary.js";

const hash = (byte: string) => byte.repeat(32);
const headerHash = "11".repeat(28),
  hubOraclePolicyId = "22".repeat(28),
  queueOutRef = `${hash("33")}#0`;
const provenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-transport-test",
  grade: "security",
} as const;
const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  hubOraclePolicyId,
  blueprintHash: hash("55"),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};

// These are local transport fixtures. Signed CML bytes are real; observations
// deliberately model canonical transitions and do not claim native-node evidence.
const signedTransaction = () => {
  const key = CML.PrivateKey.generate_ed25519();
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(key.to_public().hash()),
  ).to_address();
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(hash("33")), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(address, CML.Value.from_coin(3_000_000n)),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  const references = CML.TransactionInputList.new();
  references.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(hash("66")), 0n),
  );
  body.set_reference_inputs(references);
  const witnesses = CML.TransactionWitnessSet.new(),
    vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      key.to_public(),
      key.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  const transaction = CML.Transaction.new(body, witnesses, true, undefined),
    txHash = CML.hash_transaction(body).to_hex();
  const submit = vi.fn(async () => txHash);
  const signed = {
    toHash: () => txHash,
    toTransaction: () => transaction,
    submit,
  } as unknown as LocallyEvaluatedTransaction["signed"];
  return {
    txHash,
    signed,
    submit,
    transaction,
    referenceScripts: [
      { role: "step", outRef: `${hash("66")}#0`, scriptHash: "77".repeat(28) },
    ],
  };
};
export const customWorkflowRecoveryFixture = async (
  spec: CursorFamilySpec<InstalledFraudProofCategory>,
) => {
  const category = spec.category,
    built = signedTransaction();
  const initial: FraudProofRawL1FamilyStage = {
    kind: "not_started",
    stateQueueBlockOutRef: queueOutRef,
  };
  const observed = cursorFamilyObservation({
    spec,
    headerHash,
    provenance,
    stage: initial,
  });
  if (observed.kind !== "action_required")
    throw new Error("fixture omitted action");
  const action = observed.action;
  const stage: { value: FraudProofRawL1FamilyStage } = { value: initial };
  const forbidden = vi.fn(async (): Promise<never> => {
    throw new Error("unexpected new construction");
  });
  const observeHeader = vi.fn(async (): Promise<never> => {
    throw new Error("target is absent");
  });
  const l1 = {
    portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
    category,
    observeHeader,
    observe: async () => ({ provenance, stage: stage.value }),
    transactionConfirmed: vi.fn(async () => true),
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: forbidden,
    },
  };
  const lease = { acquire: forbidden };
  const capture = vi.fn(async () => built);
  const adapter = createCursorFamilyWorkflowAdapter({
    spec,
    l1,
    transactions: {
      portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
      category,
      prepare: forbidden,
      capture: async () => ({ transaction: await capture() }),
    },
  });
  const unsealed: Omit<HeaderFaultDecision, "decisionDigest"> = {
    schemaVersion: HEADER_DECISION,
    classifierVersion: HEADER_CLASSIFIER,
    hubOraclePolicyId,
    headerHash,
    authenticatedObservationDigest: hash("12"),
    payloadEnvelopeSha256: hash("13"),
    payloadSha256: hash("14"),
    replayVersion: COMPLETE_CANONICAL_REPLAY,
    replayDigest: hash("15"),
    launchScope: [category],
    launchScopeDigest: hash("16"),
    classificationDigest: hash("17"),
    decision: "fault_detected",
    category,
    violationId: "local-fault",
    detectionId: "local-retained-fault",
    position: "0",
  };
  const decision = {
    ...unsealed,
    decisionDigest: workflowJsonDigest(normalizeWorkflowJson(unsealed)),
  };
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    hubOraclePolicyId,
    category,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest: decision.decisionDigest,
  } as const;
  const workflowId = computeFraudProofWorkflowId(identity);
  const controller = createWorkflowReconciliationPermitController({
    decision,
    hubOraclePolicyId,
    rollbackGeneration: "0",
  });
  const workflow = {
    binding: {
      hubOraclePolicyId,
      definition: { category, headerHash },
      releaseFinality,
    },
    l1,
    adapter,
    decisionDigest: decision.decisionDigest,
    stateQueueMutationLeaseCoordinator: lease,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority: {
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      verifyForWorkflow: async () => releaseFinality,
    },
  };
  const advance = () => {
    stage.value = {
      kind: "step",
      step: 1,
      threadOutRef: `${built.txHash}#0`,
      stateQueueBlockOutRef: queueOutRef,
    };
  };
  return {
    workflow,
    decision,
    controller,
    workflowId,
    built,
    stage,
    advance,
    forbidden,
    observeHeader,
    capture,
    adapter,
    action,
    identity,
    artifact: { local: true },
  };
};
