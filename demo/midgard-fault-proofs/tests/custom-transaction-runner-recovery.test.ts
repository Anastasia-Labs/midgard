import { createHash } from "node:crypto";

import { CML } from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import { executeManifestBoundDistinctAssetAccumulationWorkflow } from "../src/distinct-asset-accumulation-limit/v1.js";
import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { executeManifestBoundMintDeclaredAssetLimitWorkflow } from "../src/mint-declared-asset-limit/v1.js";
import { executeManifestBoundObserverOrderInvalidWorkflow } from "../src/observer-order-invalid/v1.js";
import { executeManifestBoundObserversForbiddenWorkflow } from "../src/observers-forbidden-on-untagged-network/v1.js";
import { executeManifestBoundRedeemerCanonicityWorkflow } from "../src/redeemer-canonicity/runtime.js";
import {
  bindWorkflowActuationJournal,
  createWorkflowReconciliationPermitController,
} from "../src/workflow/actuation-permit.js";
import { COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.js";
import { CURSOR_FAMILY_TRANSACTION_PORT } from "../src/workflow/cursor-family-adapter.js";
import { FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT } from "../src/workflow/family-l1-observation.js";
import * as funding from "../src/workflow/funding-reservation-permit.js";
import {
  HEADER_CLASSIFIER,
  HEADER_DECISION,
  type HeaderFaultDecision,
} from "../src/workflow/header-classifier.js";
import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowTerminal,
} from "../src/workflow/journal.js";
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
  createFraudProofWorkflowRegistry,
  runFraudProofWorkflow,
} from "../src/workflow/orchestrator.js";
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
import type { LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

const hash = (byte: string) => byte.repeat(32);
const categories = [
  "mintDeclaredAssetLimit",
  "observersForbiddenOnUntaggedNetwork",
  "observerOrderInvalid",
  "redeemerCanonicity",
  "distinctAssetAccumulationLimit",
] as const;
type Category = (typeof categories)[number];
const executors = {
  mintDeclaredAssetLimit: executeManifestBoundMintDeclaredAssetLimitWorkflow,
  observersForbiddenOnUntaggedNetwork:
    executeManifestBoundObserversForbiddenWorkflow,
  observerOrderInvalid: executeManifestBoundObserverOrderInvalidWorkflow,
  distinctAssetAccumulationLimit:
    executeManifestBoundDistinctAssetAccumulationWorkflow,
};
const execute = (
  category: Category,
  input: {
    workflow: unknown;
    journal: MemoryFraudProofWorkflowJournalStore;
    sources: readonly [];
  },
) =>
  category === "redeemerCanonicity"
    ? executeManifestBoundRedeemerCanonicityWorkflow({
        workflow: input.workflow,
        sources: input.sources,
        runtime: { journal: input.journal },
      } as Parameters<typeof executeManifestBoundRedeemerCanonicityWorkflow>[0])
    : executors[category](
        input as Parameters<
          typeof executeManifestBoundMintDeclaredAssetLimitWorkflow
        >[0] &
          Parameters<typeof executeManifestBoundObserversForbiddenWorkflow>[0] &
          Parameters<
            typeof executeManifestBoundObserverOrderInvalidWorkflow
          >[0] &
          Parameters<
            typeof executeManifestBoundDistinctAssetAccumulationWorkflow
          >[0],
      );
const headerHash = "11".repeat(28);
const deploymentFingerprint = hash("22");
const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x33));
const inputs = CML.TransactionInputList.new();
inputs.add(
  CML.TransactionInput.new(CML.TransactionHash.from_hex(hash("34")), 0n),
);
const outputs = CML.TransactionOutputList.new();
outputs.add(
  CML.TransactionOutput.new(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(key.to_public().hash()),
    ).to_address(),
    CML.Value.from_coin(2000000n),
  ),
);
const body = CML.TransactionBody.new(inputs, outputs, 200000n);
const witnesses = CML.TransactionWitnessSet.new();
const vkeys = CML.VkeywitnessList.new();
vkeys.add(
  CML.Vkeywitness.new(
    key.to_public(),
    key.sign(CML.hash_transaction(body).to_raw_bytes()),
  ),
);
witnesses.set_vkeywitnesses(vkeys);
const signed = CML.Transaction.new(body, witnesses, true);
const transactionHash = CML.hash_transaction(body).to_hex();
const signedTransactionCborHex = signed.to_cbor_hex();
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
const recovery = async (
  category: Category,
  needsAnotherAction = false,
  preparedOverride?: JournalJsonObject,
  proofCreation = false,
) => {
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
    familyArtifact: { category },
  };
  const events: FraudProofWorkflowJournalEvent[] = [
    { kind: "started" },
    {
      kind: "prepared",
      artifact: preparedOverride ?? artifact,
      artifactDigest: journalJsonDigest(preparedOverride ?? artifact),
    },
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
  ];
  if (proofCreation)
    events.splice(
      2,
      0,
      {
        kind: "preflight_passed",
        actionId: "proof-creation",
        txHash: hash("22"),
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
        actionId: "proof-creation",
        actionInput: { stage: "step_04" },
        attempt: 1,
        txHash: hash("22"),
      },
      {
        kind: "reconciled",
        actionId: "proof-creation",
        outcome: "confirmed",
        txHash: hash("22"),
      },
      { kind: "confirmed", actionId: "proof-creation", txHash: hash("22") },
    );
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
  const reconcile = vi.fn(
    async (input: {
      txHash?: string;
      signedTransactionCborHex?: string;
      authorizeResubmission?: unknown;
    }) => {
      expect(input.signedTransactionCborHex).toBe(signedTransactionCborHex);

      expect(input.txHash).toBe(transactionHash);
      return { kind: "confirmed", txHash: transactionHash } as const;
    },
  );
  vi.spyOn(funding, "readWorkflowFundingRecovery").mockResolvedValue({
    transition: {
      actionKind: "step",
      signedTransactionCborHex,
      transactionHash,
      transactionBodySha256: createHash("sha256")
        .update(Buffer.from(body.to_cbor_hex(), "hex"))
        .digest("hex"),
      consumedOutRefs: [`${hash("34")}#0`],
      producedInputs: [],
    },
    submissionHandoff: null,
    completionHandoff: null,
    abandonmentHandoff: null,
  });
  const workflow = {
    binding: {
      deploymentFingerprint,
      definition: { headerHash },
    },
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
    transactions: {
      portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
      category,
      prepare: forbidden,
      capture: forbidden,
    },
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
    decision,
    workflow,
    workflowId,
    controller,
    forbidden,
    observeHeader,
    reconcile,
  };
};

afterEach(() => vi.restoreAllMocks());

it.each(categories)(
  "%s reconciles the exact signed intent after acknowledgement loss without a live target",
  async (category) => {
    const fixture = await recovery(category);
    const result = await execute(category, { ...fixture, sources: [] });
    expect(result.kind).toBe("pending");
    expect(fixture.reconcile).toHaveBeenCalledOnce();
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
it.each(categories)(
  "%s refuses another action under saved-decision reconciliation authority",
  async (category) => {
    const fixture = await recovery(category, true);
    const result = await execute(category, { ...fixture, sources: [] });
    expect(result).toMatchObject({
      kind: "pending",
      reason: "Canonical workflow requires fresh submission authority",
    });
    expect(fixture.forbidden).not.toHaveBeenCalled();
    expect(fixture.observeHeader).not.toHaveBeenCalled();
  },
);
it.each(categories)(
  "%s revokes recovery on native rollback before observing or reconciling",
  async (category) => {
    const fixture = await recovery(category);
    fixture.controller.revoke("native_chain_rollback");
    await expect(
      execute(category, { ...fixture, sources: [] }),
    ).rejects.toThrow("revoked");
    expect(fixture.reconcile).not.toHaveBeenCalled();
    expect(fixture.observeHeader).not.toHaveBeenCalled();
  },
);
it.each(categories)(
  "%s refuses an old family artifact without fabricating a generic envelope",
  async (category) => {
    await expect(
      recovery(category, false, {
        schemaVersion: "legacy-family-artifact",
        headerHash,
      }),
    ).rejects.toThrow(/prepared|binding|signed workflow/);
  },
);

// The family builders are covered by their emulator suites. This local transport
// test drives actual common cursor/orchestrator continuation with real CML bytes.
it.each(categories)(
  "%s continues a normal cursor through durable preflight and submission",
  async (category) => {
    const fixture = await buildCanonicalBlockFixture({
      transactions: [
        buildFixtureTransaction({ spendInputs: [outRefCbor(7, 0n)], fee: 1n }),
      ],
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "local-transport-test",
        grade: "security",
      },
    });
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const referenceInputs = CML.TransactionInputList.new();
    referenceInputs.add(
      CML.TransactionInput.new(CML.TransactionHash.from_hex(hash("cc")), 0n),
    );
    const transactionBody = CML.TransactionBody.from_cbor_hex(
      body.to_cbor_hex(),
    );
    transactionBody.set_reference_inputs(referenceInputs);
    const continuationWitnesses = CML.TransactionWitnessSet.new();
    const continuationVkeys = CML.VkeywitnessList.new();
    continuationVkeys.add(
      CML.Vkeywitness.new(
        key.to_public(),
        key.sign(CML.hash_transaction(transactionBody).to_raw_bytes()),
      ),
    );
    continuationWitnesses.set_vkeywitnesses(continuationVkeys);
    const transaction = CML.Transaction.new(
      transactionBody,
      continuationWitnesses,
      true,
    );
    const txHash = CML.hash_transaction(transactionBody).to_hex();
    vi.spyOn(funding, "readWorkflowFundingRecovery").mockResolvedValue({
      transition: {
        actionKind: "init",
        transactionHash: txHash,
        signedTransactionCborHex: transaction.to_cbor_hex(),
        transactionBodySha256: createHash("sha256")
          .update(Buffer.from(transactionBody.to_cbor_hex(), "hex"))
          .digest("hex"),
        consumedOutRefs: [`${hash("34")}#0`],
        producedInputs: [],
      },
      submissionHandoff: null,
      completionHandoff: null,
      abandonmentHandoff: null,
    });
    let workflowId = "";
    const submit = vi.fn(async () => {
      const entries = await journal.load(workflowId);
      expect(entries.at(-1)?.event.kind).toBe("submission_intent");
      expect(entries[1]?.event.kind).toBe("prepared");
      if (entries[1]?.event.kind === "prepared")
        expect(entries[1].event.artifact.evidenceBinding).toMatchObject({
          headerHash: fixture.headerHash,
          payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
        });
      return txHash;
    });
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: { category, stepCount: 1, successors: { 1: ["proof_token"] } },
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => {
          throw new Error("init cannot acquire a removal lease");
        },
      },
      l1: {
        portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
        category,
        observeHeader: async () => authenticatedHeaderObservation(fixture),
        observe: async () => ({
          provenance: {
            trustClass: "authenticated_cardano_l1",
            sourceId: "local-transport-test",
            grade: "security",
          },
          stage: {
            kind: "not_started",
            stateQueueBlockOutRef: `${hash("34")}#0`,
          },
        }),
        transactionConfirmed: async () => false,
        observeSignedTransaction: async (input) => ({
          ...input,
          status: "pending" as const,
          canonicalPoint: {
            slot: "100",
            blockNo: "100",
            blockHash: hash("ef"),
            pointId: "test-tip",
          },
          releaseFinalPoint: {
            slot: "70",
            blockNo: "70",
            blockHash: hash("ee"),
            pointId: "test-final",
          },
          inputs: [],
          reason: "local mempool confirmation pending",
        }),
        publications: {
          observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
          observeExact: async () => ({ kind: "not_found" }),
        },
      },
      transactions: {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category,
        prepare: async () => ({ category, headerHash: fixture.headerHash }),
        capture: async () => ({
          transaction: {
            txHash,
            signed: {
              toHash: () => txHash,
              toTransaction: () => transaction,
              submit,
            } as unknown as LocallyEvaluatedTransaction["signed"],
            referenceScripts: [
              {
                role: "step",
                outRef: `${hash("cc")}#0`,
                scriptHash: "dd".repeat(28),
              },
            ],
          },
        }),
      },
    });
    const wrapped = {
      ...adapter,
      preflight: async (input: Parameters<typeof adapter.preflight>[0]) => {
        workflowId = input.workflowId;
        return adapter.preflight(input);
      },
    };
    const violationId = {
      mintDeclaredAssetLimit: "mint-declared-asset-limit",
      observersForbiddenOnUntaggedNetwork:
        "observers-forbidden-on-untagged-network",
      observerOrderInvalid: "observer-order-invalid",
      redeemerCanonicity: "redeemer-malformed",
      distinctAssetAccumulationLimit: "mint-asset-accumulation-limit",
    }[category];
    const result = await runFraudProofWorkflow({
      deploymentFingerprint,
      evidence,
      detections: [
        {
          headerHash: fixture.headerHash,
          detectionId: "local-driver-fixture",
          violationId,
          position: 0n,
        },
      ],
      registry: createFraudProofWorkflowRegistry({
        adapters: [wrapped],
        launchScope: [category],
      }),
      journal,
      terminalVerifier: {
        verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
        verify: async () => {
          throw new Error("unconfirmed init has no terminal");
        },
      },
      releaseFinalityAuthority: {
        authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
        verifyForWorkflow: async () => releaseFinality,
      },
      maxActions: 2,
    });
    if (result.kind === "stalled") throw new Error(result.reason);
    expect(result).toMatchObject({ kind: "pending" });
    expect(submit).toHaveBeenCalledOnce();
    expect(
      (await journal.load(workflowId)).filter(
        ({ event }) => event.kind === "submission_intent",
      ),
    ).toHaveLength(1);
  },
);

it.each(categories)(
  "%s resumes a durable funding release after the target disappeared",
  async (category) => {
    const fixture = await recovery(category, false, undefined, true);
    await execute(category, { ...fixture, sources: [] });
    const entries = await fixture.journal.load(fixture.workflowId);
    const prepared = entries[1]!.event;
    if (prepared.kind !== "prepared")
      throw new Error("missing prepared fixture");
    const terminal: FraudProofWorkflowTerminal = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
      category,
      headerHash,
      proofToken: {
        unit: "11".repeat(28),
        outRef: `${hash("22")}#0`,
        createdByTxHash: hash("22"),
        retainedAtFinalState: true,
      },
      correction: {
        removalTxHash: transactionHash,
        removedStateQueueOutRef: `${hash("34")}#0`,
        fraudulentHeaderAbsent: true,
        referencedProofTokenOutRef: `${hash("22")}#0`,
      },
      economics: {
        operatorCredential: "55".repeat(28),
        proverCredential: "66".repeat(28),
        operatorBondInputOutRef: `${hash("77")}#0`,
        operatorBondInputLovelace: "10000000",
        slashedLovelace: "10000000",
        proverRewardOutputOutRef: `${transactionHash}#0`,
        proverRewardLovelace: "5000000",
        removalFeeLovelace: "200000",
        duplicateRewardAbsent: true,
      },
      observedAt: {
        slot: "4242",
        blockHash: hash("88"),
        confirmationDepth: 30,
      },
    };
    const handoff: funding.WorkflowFundingCompletionHandoff = {
      workflowId: fixture.workflowId,
      identity: entries[0]!.identity,
      preparedArtifactDigest: prepared.artifactDigest,
      expectedJournalSequence: entries.length,
      completion: {
        kind: "completed",
        terminal,
        terminalDigest: journalJsonDigest(normalizeJournalJson(terminal)),
      },
    };
    vi.mocked(funding.readWorkflowFundingRecovery).mockResolvedValue({
      transition: null,
      submissionHandoff: null,
      completionHandoff: handoff,
      abandonmentHandoff: null,
    });
    const verify = vi.fn(async () => terminal);
    const result = await execute(category, {
      ...fixture,
      workflow: {
        ...fixture.workflow,
        terminalVerifier: {
          verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
          verify,
        },
      },
      sources: [],
    });
    if (result.kind === "stalled") throw new Error(result.reason);
    expect(result.kind).toBe("completed");
    expect(verify).toHaveBeenCalledOnce();
    expect(fixture.observeHeader).not.toHaveBeenCalled();
    expect(fixture.forbidden).not.toHaveBeenCalled();
    expect(
      (await fixture.journal.load(fixture.workflowId)).filter(
        ({ event }) => event.kind === "completed",
      ),
    ).toHaveLength(1);
    expect(() =>
      createWorkflowReconciliationPermitController({
        decision: fixture.decision,
        deploymentFingerprint,
        rollbackGeneration: "0",
        entries: result.kind === "completed" ? result.entries : [],
      }),
    ).toThrow("existing signed workflow");
  },
);

it.each(categories)(
  "%s rejects a different prepared artifact at the durable funding handoff",
  async (category) => {
    const fixture = await recovery(category);
    const entries = await fixture.journal.load(fixture.workflowId);
    const prepared = entries[1]!.event;
    if (prepared.kind !== "prepared")
      throw new Error("missing prepared fixture");
    vi.mocked(funding.readWorkflowFundingRecovery).mockResolvedValue({
      transition: null,
      completionHandoff: null,
      abandonmentHandoff: null,
      submissionHandoff: {
        workflowId: fixture.workflowId,
        identity: entries[0]!.identity,
        preparedArtifactDigest: hash("ef"),
        expectedJournalSequence: 2,
        preflight: entries[2]!
          .event as funding.WorkflowFundingSubmissionHandoff["preflight"],
        submissionIntent: entries[3]!
          .event as funding.WorkflowFundingSubmissionHandoff["submissionIntent"],
      },
    });
    await expect(
      execute(category, { ...fixture, sources: [] }),
    ).rejects.toThrow(/prepared|artifact|handoff/);
    expect(fixture.reconcile).not.toHaveBeenCalled();
    expect(fixture.observeHeader).not.toHaveBeenCalled();
  },
);
