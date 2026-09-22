import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import {
  bindWorkflowActuationJournal,
  createWorkflowReconciliationPermitController,
} from "../../src/workflow/actuation-permit.js";
import { COMPLETE_CANONICAL_REPLAY } from "../../src/workflow/complete-replay.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
} from "../../src/workflow/cursor-family-adapter.js";
import {
  cursorFamilyObservation,
  type CursorFamilySpec,
} from "../../src/workflow/cursor-family-state.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
} from "../../src/workflow/family-l1-observation.js";
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
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  MemoryFraudProofWorkflowJournalStore,
  normalizeJournalJson,
} from "../../src/workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.js";
import { type FraudProofRawL1FamilyStage } from "../../src/workflow/raw-l1-family-derivation.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../../src/workflow/raw-l1-publication-observation.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-finality-policy.js";
import type { LocallyEvaluatedTransaction } from "../../src/workflow/transaction-boundary.js";

const hash = (byte: string) => byte.repeat(32);
const headerHash = "11".repeat(28),
  deploymentFingerprint = hash("22"),
  queueOutRef = `${hash("33")}#0`,
  proofOutRef = `${hash("44")}#0`;
const provenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-transport-test",
  grade: "security",
} as const;
const policy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: deploymentFingerprint,
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
const terminal = (
  category: FraudProofCatalogueCategoryName,
  txHash: string,
): FraudProofWorkflowTerminal => ({
  schemaVersion: "midgard-fraud-proof-workflow-terminal-v1",
  category,
  headerHash,
  proofToken: {
    unit: "88".repeat(28),
    outRef: proofOutRef,
    createdByTxHash: hash("44"),
    retainedAtFinalState: true,
  },
  correction: {
    removalTxHash: txHash,
    removedStateQueueOutRef: queueOutRef,
    fraudulentHeaderAbsent: true,
    referencedProofTokenOutRef: proofOutRef,
  },
  economics: {
    operatorCredential: "99".repeat(28),
    proverCredential: "aa".repeat(28),
    operatorBondInputOutRef: `${hash("bb")}#0`,
    operatorBondInputLovelace: "10000000",
    slashedLovelace: "10000000",
    proverRewardOutputOutRef: `${txHash}#0`,
    proverRewardLovelace: "5000000",
    removalFeeLovelace: "200000",
    duplicateRewardAbsent: true,
  },
  observedAt: { slot: "1234", blockHash: hash("cc"), confirmationDepth: 30 },
});
export const customWorkflowRecoveryFixture = async (
  spec: CursorFamilySpec,
  removed = false,
) => {
  const category = spec.category,
    built = signedTransaction();
  const initial: FraudProofRawL1FamilyStage = removed
    ? {
        kind: "proof_token",
        stateQueueBlockOutRef: queueOutRef,
        nextRemovalOutRef: queueOutRef,
        fraudProofOutRef: proofOutRef,
      }
    : { kind: "not_started", stateQueueBlockOutRef: queueOutRef };
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
    stateQueueMutationLeaseCoordinator: lease,
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
    deploymentFingerprint,
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
  const artifact = {
    evidenceBinding: {
      route: "canonical_block",
      headerHash,
      payloadEnvelopeSha256: decision.payloadEnvelopeSha256,
      payloadSha256: decision.payloadSha256,
      l1BlockHash: hash("18"),
      l1Slot: "1",
    },
    releaseFinality,
    familyArtifact: { local: true },
  };
  const events: FraudProofWorkflowJournalEvent[] = [
    { kind: "started" },
    { kind: "prepared", artifact, artifactDigest: journalJsonDigest(artifact) },
    {
      kind: "preflight_passed",
      actionId: action.actionId,
      txHash: built.txHash,
      localEvaluator: "lucid-evolution-local-uplc-v1",
      referenceScripts: built.referenceScripts,
    },
    {
      kind: "submission_intent",
      actionId: action.actionId,
      actionInput: action.input,
      attempt: 1,
      txHash: built.txHash,
    },
  ];
  if (removed) {
    const proofAction = {
      actionId: `step_${String(spec.stepCount).padStart(2, "0")}:proof-thread`,
      input: {
        category,
        stage: `step_${String(spec.stepCount).padStart(2, "0")}`,
      },
    };
    events.splice(
      2,
      0,
      {
        kind: "preflight_passed",
        actionId: proofAction.actionId,
        txHash: hash("44"),
        localEvaluator: "lucid-evolution-local-uplc-v1",
        referenceScripts: built.referenceScripts,
      },
      {
        kind: "submission_intent",
        actionId: proofAction.actionId,
        actionInput: proofAction.input,
        attempt: 1,
        txHash: hash("44"),
      },
      {
        kind: "reconciled",
        actionId: proofAction.actionId,
        outcome: "confirmed",
        txHash: hash("44"),
      },
      { kind: "confirmed", actionId: proofAction.actionId, txHash: hash("44") },
    );
  }
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
  const workflow = {
    binding: {
      deploymentFingerprint,
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
    stage.value = removed
      ? { kind: "removed", terminal: terminal(category, built.txHash) }
      : {
          kind: "step",
          step: 1,
          threadOutRef: `${built.txHash}#0`,
          stateQueueBlockOutRef: queueOutRef,
        };
  };
  return {
    workflow,
    journal,
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
    artifact: artifact.familyArtifact,
  };
};

/** Model a crash at the real terminal handoff boundary; atomic-store persistence has separate tests. */
export const verifyCompletionHandoffRestart = async (
  fixture: Awaited<ReturnType<typeof customWorkflowRecoveryFixture>>,
  run: (
    input: Awaited<ReturnType<typeof customWorkflowRecoveryFixture>>,
  ) => Promise<FraudProofWorkflowRunResult>,
) => {
  fixture.advance();
  let durable: funding.WorkflowFundingCompletionHandoff | undefined;
  const release = vi
    .spyOn(funding, "releaseWorkflowFundingReservation")
    .mockImplementation(async ({ handoff }) => {
      if (durable !== undefined) expect(handoff).toEqual(durable);
      durable = handoff;
    });
  const append = fixture.journal.append.bind(fixture.journal);
  const interrupted = vi
    .spyOn(fixture.journal, "append")
    .mockImplementation(async (entry, sequence) => {
      if (entry.event.kind === "completed")
        throw new Error("interrupted after durable funding release");
      await append(entry, sequence);
    });
  const read = vi.spyOn(funding, "readWorkflowFundingRecovery");
  try {
    await expect(run(fixture)).rejects.toThrow("after durable funding release");
    expect(durable).toBeDefined();
    interrupted.mockRestore();
    read.mockResolvedValue({
      transition: null,
      submissionHandoff: null,
      completionHandoff: durable!,
      abandonmentHandoff: null,
    });
    expect((await run(fixture)).kind).toBe("completed");
    expect(release).toHaveBeenCalledTimes(2);
    expect(fixture.observeHeader).not.toHaveBeenCalled();
    expect(fixture.built.submit).not.toHaveBeenCalled();
    expect(
      (await fixture.journal.load(fixture.workflowId)).filter(
        ({ event }) => event.kind === "completed",
      ),
    ).toHaveLength(1);
  } finally {
    interrupted.mockRestore();
    read.mockRestore();
    release.mockRestore();
  }
};
