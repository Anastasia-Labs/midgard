import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  decodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxProofFieldLengths,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, Lucid, type TxSigned } from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import { fetchFraudProofEvidence } from "../src/evidence/fraud-proof-evidence.js";
import {
  executeManifestBoundFieldPreimageLengthWorkflow,
  type ManifestBoundFieldPreimageLengthWorkflow,
} from "../src/field-preimage-length-mismatch/authenticated-workflow.js";
import { createFieldPreimageLengthRecoveryPorts } from "../src/field-preimage-length-mismatch/recovery.js";
import * as submitters from "../src/field-preimage-length-mismatch/submit-lucid.js";
import { FIELD_PREIMAGE_LENGTH_CURSOR_SPEC } from "../src/field-preimage-length-mismatch/workflow-spec.js";
import { DaLibp2pRetainedDaSource } from "../src/transition-trace/fetch.js";
import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import {
  bindWorkflowActuationJournal,
  bindWorkflowActuationRecoveryIdentity,
  createWorkflowActuationPermitController,
  createWorkflowReconciliationPermitController,
} from "../src/workflow/actuation-permit.js";
import {
  COMPLETE_CANONICAL_REPLAY,
  FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.js";
import { cursorFamilyObservation } from "../src/workflow/cursor-family-state.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "../src/workflow/family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "../src/workflow/field-carriage-prerequisite.js";
import { unsafeCreateWorkflowFundingReservationPermitForTest } from "../src/workflow/funding-reservation-permit.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
  HEADER_CLASSIFIER,
  HEADER_DECISION,
  type HeaderFaultDecision,
} from "../src/workflow/header-classifier.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  type JournalJsonObject,
  normalizeJournalJson,
} from "../src/workflow/journal.js";
import { FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER } from "../src/workflow/orchestrator.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  createManifestBoundWorkflowRunner,
  WORKFLOW_RUNTIME_CONFIG,
} from "../src/workflow/runtime.js";
import type { LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

// These fixtures provide an authenticated workflow directly; keep their adapter
// wiring local so the production constructor is assembled from its definition.
const createFieldPreimageLengthRecoveryAdapter = (
  workflow: Parameters<typeof createFieldPreimageLengthRecoveryPorts>[0],
) => {
  const { transactions, requirementForAction } =
    createFieldPreimageLengthRecoveryPorts(workflow);
  return {
    transactions,
    adapter: withFieldCarriagePrerequisite({
      category: "fieldPreimageLengthMismatch",
      base: createCursorFamilyWorkflowAdapter({
        spec: FIELD_PREIMAGE_LENGTH_CURSOR_SPEC,
        l1: workflow.l1,
        transactions,
        stateQueueMutationLeaseCoordinator:
          workflow.stateQueueMutationLeaseCoordinator,
      }),
      prerequisite: createAuthenticatedFieldCarriagePrerequisitePort({
        category: "fieldPreimageLengthMismatch",
        lucid: workflow.config.lucid,
        network: workflow.binding.network,
        signer: workflow.config.signer,
        publications: workflow.l1.publications,
        transactionConfirmed: async (input) =>
          await workflow.l1.transactionConfirmed(input),
        requirementForAction,
      }),
    }),
  };
};

const category = "fieldPreimageLengthMismatch";
const hash = (value: string) => value.repeat(32);
const outRef = (value: string) => `${hash(value)}#0`;
const deploymentFingerprint = hash("aa");
const provenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "scripted-local-observation",
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
  blueprintHash: hash("bb"),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};
const forbidden = (): never => {
  throw new Error("unexpected builder or live-header call");
};
afterEach(() => vi.restoreAllMocks());

const rawFixture = async () => {
  const transaction = buildFixtureTransaction({
    spendInputs: [outRefCbor(7, 0n)],
    fee: 1n,
  });
  const lengths = [
    ...decodeMidgardNativeTxProofFieldLengths(
      Buffer.from(transaction.source.source.field_preimage_lengths_cbor, "hex"),
    ),
  ];
  lengths[0] = lengths[0]! + 1;
  const source = {
    ...transaction.source,
    source: {
      ...transaction.source.source,
      field_preimage_lengths_cbor:
        encodeMidgardNativeTxProofFieldLengths(lengths).toString("hex"),
    },
  };
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      {
        ...transaction,
        source,
        sourceValueBytes: Buffer.from(
          Data.to(source, SDK.L2TransactionSource),
          "hex",
        ),
      },
    ],
  });
  return fixture;
};
const material = async (provided?: Awaited<ReturnType<typeof rawFixture>>) => {
  const fixture = provided ?? (await rawFixture());
  const routed = await fetchFraudProofEvidence({
    observation: authenticatedHeaderObservation(fixture),
    sources: [
      {
        sourceId: "recovery-fixture",
        fetchPayloadByHeaderHash: async () => ({
          ok: true,
          sourceId: "recovery-fixture",
          sourcePeerId: "peer",
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          attempts: [],
          provenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "recovery-fixture/peer",
            grade: "security",
          },
        }),
      },
    ],
  });
  if (routed.kind !== "field_preimage_length_mismatch")
    throw new Error("expected real malformed-length route");
  return routed;
};

const bound = async (
  headerHash: string,
  stage: { value: FraudProofRawL1FamilyStage },
) => {
  const observeHeader =
    vi.fn<FraudProofFamilyL1ObservationPort<typeof category>["observeHeader"]>(
      forbidden,
    );
  const l1: FraudProofFamilyL1ObservationPort<typeof category> = {
    portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
    category,
    observeHeader,
    transactionConfirmed: async () => true,
    observe: async () => ({ provenance, stage: stage.value }),
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: async () => {
        throw new Error("no non-inline prerequisite expected");
      },
    },
  };
  const binding = {
    deploymentFingerprint,
    definition: { category, headerHash },
    network: "Custom",
    releaseFinality,
    releaseEconomics: { policy: { fraudProverRewardLovelace: "2000000" } },
  } as ManifestBoundFieldPreimageLengthWorkflow["binding"];
  const lucid = await Lucid(undefined, "Custom", {
    slotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1000 },
  });
  vi.spyOn(lucid, "utxosAt").mockResolvedValue([]);
  const config: ManifestBoundFieldPreimageLengthWorkflow["config"] = {
    schemaVersion:
      "midgard-field-preimage-length-mismatch-production-config-v1",
    binding,
    signer: {
      source: "test",
      address: "test-address",
      paymentKeyHash: "11".repeat(28),
      selectWallet: forbidden,
    },
    lucid,
    contracts: {
      fieldPreimageCertificate: {
        policyId: "22".repeat(28),
        mintingScript: { type: "PlutusV3", script: "00" },
        mintingScriptCBOR: "00",
      },
      get computationThread() {
        return forbidden();
      },
      get fraudProof() {
        return forbidden();
      },
      get fieldPreimageLengthMismatch() {
        return forbidden();
      },
    },
    referenceScripts: {
      get step01() {
        return forbidden();
      },
      get step02Accepted() {
        return forbidden();
      },
      get step02Forced() {
        return forbidden();
      },
      get step03() {
        return forbidden();
      },
      get fieldPreimageCertificateMint() {
        return forbidden();
      },
      get witnesses() {
        return forbidden();
      },
    },
  };
  return {
    workflow: {
      binding,
      config,
      l1,
      decisionDigest: hash("33"),
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => {
          throw new Error("no lease expected");
        },
      },
    },
    observeHeader,
  };
};
const context = (
  headerHash: string,
  artifact: Parameters<
    ReturnType<
      typeof createFieldPreimageLengthRecoveryAdapter
    >["adapter"]["observe"]
  >[0]["artifact"],
) => ({
  identity: {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint,
    category,
    target: { kind: "state_queue_header" as const, headerHash },
  } satisfies FraudProofWorkflowIdentity,
  workflowId: hash("44"),
  artifact,
  entries: [],
});
const required = (headerHash: string, stage: FraudProofRawL1FamilyStage) => {
  const result = cursorFamilyObservation({
    spec: FIELD_PREIMAGE_LENGTH_CURSOR_SPEC,
    headerHash,
    provenance,
    stage,
  });
  if (result.kind !== "action_required")
    throw new Error("expected selected cursor action");
  return result.action;
};
const captured = (): LocallyEvaluatedTransaction => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(hash("55")), 0n),
  );
  const references = CML.TransactionInputList.new();
  references.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(hash("66")), 0n),
  );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    0n,
  );
  body.set_reference_inputs(references);
  const transaction = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  );
  const txHash = CML.hash_transaction(body).to_hex();
  const signed = {
    toHash: () => txHash,
    toTransaction: () => transaction,
  } as TxSigned;
  return {
    txHash,
    signed,
    referenceScripts: [
      {
        role: "fixture reference",
        outRef: outRef("66"),
        scriptHash: "77".repeat(28),
      },
    ],
  };
};

it("readmits exact raw proof material after durable JSON roundtrip and clears rejected material", async () => {
  const routed = await material();
  const headerHash = routed.evidence.prepared.headerHash;
  const fixture = await bound(headerHash, {
    value: { kind: "not_started", stateQueueBlockOutRef: outRef("66") },
  });
  const first = createFieldPreimageLengthRecoveryAdapter(fixture.workflow);
  const artifact = await first.transactions.prepareRaw!(routed);
  const restarted = createFieldPreimageLengthRecoveryAdapter(fixture.workflow);
  await restarted.transactions.validatePreparedRawArtifact!({
    routed,
    artifact: JSON.parse(JSON.stringify(artifact)),
  });
  await expect(
    restarted.adapter.observe(context(headerHash, artifact)),
  ).resolves.toMatchObject({
    kind: "action_required",
    action: { input: { stage: "init" } },
  });
  await expect(
    restarted.transactions.validatePreparedRawArtifact!({
      routed,
      artifact: { ...artifact, changed: true },
    }),
  ).rejects.toThrow("differs from freshly authenticated");
  await expect(
    restarted.transactions.capture({
      action: required(headerHash, {
        kind: "not_started",
        stateQueueBlockOutRef: outRef("66"),
      }),
      artifact,
    }),
  ).rejects.toThrow("not freshly authenticated");
});

it.each(["init", "dispatch", "authenticate", "finalize"] as const)(
  "captures the existing accepted %s builder from exact cursor state",
  async (selected) => {
    const routed = await material();
    const headerHash = routed.evidence.prepared.headerHash;
    const ordinal =
      selected === "dispatch" ? 1 : selected === "authenticate" ? 2 : 4;
    const stage: FraudProofRawL1FamilyStage =
      selected === "init"
        ? { kind: "not_started", stateQueueBlockOutRef: outRef("66") }
        : {
            kind: "step",
            step: ordinal,
            threadOutRef: outRef("55"),
            stateQueueBlockOutRef: outRef("66"),
          };
    const fixture = await bound(headerHash, { value: stage });
    const recovery = createFieldPreimageLengthRecoveryAdapter(fixture.workflow);
    const artifact = await recovery.transactions.prepareRaw!(routed);
    const transaction = captured();
    const target =
      selected === "init"
        ? "submitFieldPreimageLengthInit"
        : selected === "dispatch"
          ? "submitFieldPreimageLengthAcceptedDispatch"
          : selected === "authenticate"
            ? "submitFieldPreimageLengthAcceptedAuthentication"
            : "submitFieldPreimageLengthTerminal";
    const submit = vi
      .spyOn(submitters, target)
      .mockImplementation(async (input) => {
        await input.preSubmitBoundary?.(transaction);
        throw new Error("capture did not interrupt before submission");
      });
    const result = await recovery.transactions.capture({
      action: required(headerHash, stage),
      artifact,
    });
    expect(result.transaction).toBe(transaction);
    expect(submit).toHaveBeenCalledOnce();
  },
);

it("rejects a mismatched physical branch and yields a changed selected header before any builder", async () => {
  const routed = await material();
  const headerHash = routed.evidence.prepared.headerHash;
  const stage = {
    value: {
      kind: "step",
      step: 3,
      threadOutRef: outRef("55"),
      stateQueueBlockOutRef: outRef("66"),
    } as FraudProofRawL1FamilyStage,
  };
  const fixture = await bound(headerHash, stage);
  const recovery = createFieldPreimageLengthRecoveryAdapter(fixture.workflow);
  const artifact = await recovery.transactions.prepareRaw!(routed);
  await expect(
    recovery.transactions.capture({
      action: required(headerHash, stage.value),
      artifact,
    }),
  ).rejects.toThrow("changed proof direction");
  const action = required(headerHash, {
    kind: "not_started",
    stateQueueBlockOutRef: outRef("66"),
  });
  stage.value = { kind: "not_started", stateQueueBlockOutRef: outRef("88") };
  const submit = vi.spyOn(submitters, "submitFieldPreimageLengthInit");
  await expect(
    recovery.transactions.capture({ action, artifact }),
  ).rejects.toBeInstanceOf(WorkflowActionChangedError);
  expect(submit).not.toHaveBeenCalled();
});

it.each([
  "reconciliation-only",
  "fresh authorizing decision",
  "wrong authorizing decision",
] as const)("resumes retained field removal through %s", async (mode) => {
  const directory = await mkdtemp(
    join(tmpdir(), "field-length-shared-recovery-"),
  );
  try {
    const nativeFixture = await rawFixture();
    const routed = await material(nativeFixture);
    const headerHash = routed.evidence.prepared.headerHash;
    const proofHash = hash("22");
    const removalHash = hash("33");
    const terminal: FraudProofWorkflowTerminal = {
      schemaVersion: "midgard-fraud-proof-workflow-terminal-v1",
      category,
      headerHash,
      proofToken: {
        unit: "11".repeat(28) + "22".repeat(28),
        outRef: `${proofHash}#0`,
        createdByTxHash: proofHash,
        retainedAtFinalState: true,
      },
      correction: {
        removalTxHash: removalHash,
        removedStateQueueOutRef: outRef("66"),
        fraudulentHeaderAbsent: true,
        referencedProofTokenOutRef: `${proofHash}#0`,
      },
      economics: {
        operatorCredential: "66".repeat(28),
        proverCredential: "77".repeat(28),
        operatorBondInputOutRef: outRef("88"),
        operatorBondInputLovelace: "4000000",
        slashedLovelace: "2000000",
        proverRewardOutputOutRef: `${removalHash}#0`,
        proverRewardLovelace: "2000000",
        removalFeeLovelace: "2000000",
        duplicateRewardAbsent: true,
      },
      observedAt: { slot: "1000", blockHash: hash("aa"), confirmationDepth: 1 },
    };
    const stage: { value: FraudProofRawL1FamilyStage } = {
      value: { kind: "removed", terminal },
    };
    const fixture = await bound(headerHash, stage);
    const first = createFieldPreimageLengthRecoveryAdapter(fixture.workflow);
    const familyArtifact = await first.transactions.prepareRaw!(routed);
    const unsealed: Omit<HeaderFaultDecision, "decisionDigest"> = {
      schemaVersion: HEADER_DECISION,
      classifierVersion: HEADER_CLASSIFIER,
      deploymentFingerprint,
      headerHash,
      authenticatedObservationDigest: hash("55"),
      payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
      payloadSha256: routed.evidence.payloadSha256,
      replayVersion: COMPLETE_CANONICAL_REPLAY,
      replayDigest: hash("88"),
      launchScope: [category],
      launchScopeDigest: hash("99"),
      classificationDigest: hash("aa"),
      decision: "fault_detected",
      category,
      violationId: "field-preimage-length-mismatch",
      detectionId: "retained-test-fault",
      position: "0",
    };
    let decision: HeaderFaultDecision = {
      ...unsealed,
      decisionDigest: journalJsonDigest(normalizeJournalJson(unsealed)),
    };
    const publicSource = new DaLibp2pRetainedDaSource({
      deploymentFingerprint,
      peers: [{ peerId: "12D3KooWfieldRecoveryTest" }],
      transport: { request: async () => forbidden() },
    });
    vi.spyOn(publicSource, "fetchPayloadByHeaderHash").mockResolvedValue({
      ok: true,
      sourceId: "field-runtime-fixture",
      sourcePeerId: "peer",
      payloadEnvelopeCbor: nativeFixture.payloadEnvelopeCbor,
      attempts: [],
      provenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "field-runtime-fixture/peer",
        grade: "security",
      },
    });
    const classifier = await createHeaderClassifier({
      deploymentFingerprint,
      replayer: FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
      releaseFinalityAuthority: {
        authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
        verifyForWorkflow: async () => releaseFinality,
      },
    });
    const classify = async (confirmationDepth: number) => {
      const observation = authenticatedHeaderObservation(nativeFixture, {
        confirmationDepth,
      });
      const result = await classifyHeader({
        classifier,
        observation,
        authenticatedObservationDigest:
          await authenticatedStateQueueObservationDigest({
            observation,
            minimumConfirmationDepth: 30,
          }),
        sources: [publicSource],
      });
      if (result.decision !== "fault_detected")
        throw new Error("expected classified length mismatch");
      return result;
    };
    if (mode !== "reconciliation-only") decision = await classify(30);
    const identity = {
      ...context(headerHash, familyArtifact).identity,
      decisionDigest: decision.decisionDigest,
    };
    const workflowId = computeFraudProofWorkflowId(identity);
    const artifact: JournalJsonObject = {
      evidenceBinding: {
        route: "authenticated_raw_family",
        category,
        headerHash,
        payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
        payloadSha256: routed.evidence.payloadSha256,
        l1BlockHash:
          authenticatedHeaderObservation(nativeFixture).chainPoint.blockHash,
        l1Slot:
          authenticatedHeaderObservation(
            nativeFixture,
          ).chainPoint.slot.toString(),
      },
      releaseFinality,
      familyArtifact,
    };
    const removeAction = required(headerHash, {
      kind: "proof_token",
      stateQueueBlockOutRef: outRef("66"),
      fraudProofOutRef: `${proofHash}#0`,
      nextRemovalOutRef: outRef("66"),
    });
    const events: FraudProofWorkflowJournalEvent[] = [
      { kind: "started" },
      {
        kind: "prepared",
        artifact,
        artifactDigest: journalJsonDigest(artifact),
      },
    ];
    for (const [actionId, txHash, input] of [
      ["proof", proofHash, { stage: "step_04" }],
      [removeAction.actionId, removalHash, removeAction.input],
    ] as const) {
      events.push(
        {
          kind: "preflight_passed",
          actionId,
          txHash,
          localEvaluator: "local-uplc-test",
          referenceScripts: [],
        },
        {
          kind: "submission_intent",
          actionId,
          txHash,
          actionInput: input,
          attempt: 1,
        },
        { kind: "submitted", actionId, txHash, attempt: 1 },
      );
      if (txHash === proofHash || mode !== "reconciliation-only")
        events.push(
          { kind: "reconciled", actionId, txHash, outcome: "confirmed" },
          { kind: "confirmed", actionId, txHash },
        );
    }
    if (mode !== "reconciliation-only")
      events.push({
        kind: "terminal_included",
        terminal,
        terminalDigest: journalJsonDigest(normalizeJournalJson(terminal)),
      });
    const store = new DirectoryFraudProofWorkflowJournalStore(directory);
    for (const [sequence, event] of events.entries())
      await store.append(
        {
          schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
          workflowId,
          identity,
          sequence,
          recordedAt: "2026-09-14T00:00:00.000Z",
          event,
        },
        sequence,
      );
    const originalEntries = await store.load(workflowId);
    const freshDecision =
      mode === "reconciliation-only" ? decision : await classify(31);
    const controller =
      mode === "reconciliation-only"
        ? createWorkflowReconciliationPermitController({
            decision,
            deploymentFingerprint,
            rollbackGeneration: "0",
            entries: originalEntries,
          })
        : createWorkflowActuationPermitController({
            decision: freshDecision,
            rollbackGeneration: "0",
          });
    if (mode !== "reconciliation-only") {
      expect(freshDecision.decisionDigest).not.toBe(decision.decisionDigest);
      bindWorkflowActuationRecoveryIdentity({
        permit: controller.permit,
        category,
        rollbackGeneration: "0",
        originalDecision: decision,
      });
    }
    const journal = bindWorkflowActuationJournal({
      journal: new DirectoryFraudProofWorkflowJournalStore(directory),
      permit: controller.permit,
      decisionDigest: freshDecision.decisionDigest,
      deploymentFingerprint,
      category,
      headerHash,
    });
    // This checks shared recovery wiring with scripted L1 observations, not native receipt validation.
    const restarted = createFieldPreimageLengthRecoveryAdapter(
      fixture.workflow,
    );
    const workflow: ManifestBoundFieldPreimageLengthWorkflow = {
      ...fixture.workflow,
      ...restarted,
      workflowVersion:
        "midgard-field-preimage-length-mismatch-production-workflow-v1",
      decisionDigest: decision.decisionDigest,
      terminalVerifier: {
        verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
        verifyIncluded: async ({ candidate }) => candidate,
        verify: async ({ candidate }) => candidate,
      },
      releaseFinalityAuthority: {
        authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
        verifyForWorkflow: async () => releaseFinality,
      },
    };
    if (mode !== "reconciliation-only") {
      fixture.observeHeader.mockResolvedValue(
        authenticatedHeaderObservation(nativeFixture, {
          confirmationDepth: 31,
        }),
      );
      const close = vi.fn(async () => undefined);
      const runner = createManifestBoundWorkflowRunner({
        category,
        loadRuntimeConfig: async ({ invocation }) => {
          if (
            !("decisionDigest" in invocation) ||
            typeof invocation.decisionDigest !== "string"
          )
            throw new Error(
              "expected execution invocation with its fresh decision",
            );
          return {
            schemaVersion: WORKFLOW_RUNTIME_CONFIG,
            config: {
              decisionDigest:
                mode === "wrong authorizing decision"
                  ? decision.decisionDigest
                  : invocation.decisionDigest,
            },
            retainedDaSources: [publicSource],
            close,
          };
        },
        constructWorkflow: async (config) => ({
          ...workflow,
          decisionDigest: config.decisionDigest,
        }),
        execute: executeManifestBoundFieldPreimageLengthWorkflow,
      });
      const run = runner.runOrResume({
        mode: "resume",
        category,
        deploymentFingerprint,
        headerHash,
        decisionDigest: freshDecision.decisionDigest,
        actuationPermit: controller.permit,
        fundingReservationPermit:
          unsafeCreateWorkflowFundingReservationPermitForTest({
            category,
            actuationPermit: controller.permit,
            deploymentFingerprint,
            decisionDigest: freshDecision.decisionDigest,
            rollbackGeneration: "0",
          }),
        journalDirectory: directory,
        runtimeConfigPath: "/fixture/field-runtime.json",
      });
      if (mode === "wrong authorizing decision") {
        await expect(run).rejects.toThrow(
          "journal actuation permit changed decision digest",
        );
        expect(fixture.observeHeader).not.toHaveBeenCalled();
        expect(await store.load(workflowId)).toEqual(originalEntries);
      } else {
        await expect(run).resolves.toMatchObject({
          kind: "terminal_included",
          workflowId,
          identity,
        });
        expect(fixture.observeHeader).toHaveBeenCalledOnce();
        const entries = await store.load(workflowId);
        expect(entries.slice(0, originalEntries.length)).toEqual(
          originalEntries,
        );
        expect(
          entries.every(
            (entry) =>
              entry.workflowId === workflowId &&
              entry.identity.decisionDigest === decision.decisionDigest,
          ),
        ).toBe(true);
        expect(
          entries.filter(({ event }) => event.kind === "submission_intent"),
        ).toHaveLength(2);
      }
      expect(close).toHaveBeenCalledOnce();
      return;
    }
    const result = await executeManifestBoundFieldPreimageLengthWorkflow({
      workflow,
      sources: [],
      journal,
    });
    expect(result.kind).toBe("terminal_included");
    expect(fixture.observeHeader).not.toHaveBeenCalled();
    expect(
      (await journal.load(workflowId)).filter(
        ({ event }) => event.kind === "submission_intent",
      ),
    ).toHaveLength(2);
    expect(
      (await journal.load(workflowId)).some(
        ({ event }) => event.kind === "completed",
      ),
    ).toBe(false);
    stage.value = {
      kind: "removed",
      terminal: {
        ...terminal,
        observedAt: { ...terminal.observedAt, confirmationDepth: 30 },
      },
    };
    expect(
      (
        await executeManifestBoundFieldPreimageLengthWorkflow({
          workflow,
          sources: [],
          journal,
        })
      ).kind,
    ).toBe("completed");
    expect(fixture.observeHeader).not.toHaveBeenCalled();
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});
