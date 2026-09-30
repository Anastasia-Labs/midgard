import "node:fs/promises";
import "node:os";
import "node:path";
import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/evidence/fraud-proof-evidence.js";
import "../src/field-preimage-length-mismatch/authenticated-workflow.js";
import "../src/field-preimage-length-mismatch/recovery.js";
import "../src/field-preimage-length-mismatch/submit-lucid.js";
import "../src/field-preimage-length-mismatch/workflow-spec.js";
import "../src/transition-trace/fetch.js";
import "../src/workflow/action-changed.js";
import "../src/workflow/actuation-permit.js";
import "../src/workflow/complete-replay.js";
import "../src/workflow/cursor-family-adapter.js";
import "../src/workflow/cursor-family-state.js";
import "../src/workflow/family-application.js";
import "../src/workflow/family-l1-observation.js";
import "../src/workflow/field-carriage-prerequisite.js";
import "../src/workflow/funding-reservation-permit.js";
import "../src/workflow/header-classifier.js";
import "../src/workflow/journal.js";
import "../src/workflow/orchestrator.js";
import "../src/workflow/raw-l1-publication-observation.js";
import "../src/workflow/release-finality-policy.js";
import "../src/workflow/runtime.js";
import "./helpers/canonical-block-evidence-fixture.js";
import "./support/family-common-infrastructure.js";
import "./field-preimage-length-recovery.bound.js";

import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { afterEach, expect, it, vi } from "vitest";

import {
  executeManifestBoundFieldPreimageLengthWorkflow,
  type ManifestBoundFieldPreimageLengthWorkflow,
} from "../src/field-preimage-length-mismatch/authenticated-workflow.js";
import * as submitters from "../src/field-preimage-length-mismatch/submit-lucid.js";
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
import { defineFamilyApplication } from "../src/workflow/family-application.js";
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
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  type JournalJsonObject,
  normalizeJournalJson,
} from "../src/workflow/journal.js";
import { FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER } from "../src/workflow/orchestrator.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import { FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY } from "../src/workflow/release-finality-policy.js";
import {
  createManifestBoundWorkflowRunner,
  WORKFLOW_RUNTIME_CONFIG,
} from "../src/workflow/runtime.js";
import {
  bound,
  captured,
  category,
  context,
  createFieldPreimageLengthRecoveryAdapter,
  deploymentFingerprint,
  forbidden,
  hash,
  material,
  outRef,
  rawFixture,
  releaseFinality,
  required,
} from "./field-preimage-length-recovery.bound.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  emptyRosterReferenceScriptResolver,
  familyCommonInfrastructureForTest,
} from "./support/family-common-infrastructure.js";

afterEach(() => vi.restoreAllMocks());

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
            minimumConfirmationDepth:
              DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
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
        record: defineFamilyApplication({
          category,
          roster: {},
          requires: [],
          // The record binds the authorizing decision out of the loaded
          // infrastructure; the wrong-decision mode binds the stale one.
          bindConfig: ({ infrastructure }) => ({
            decisionDigest:
              mode === "wrong authorizing decision"
                ? decision.decisionDigest
                : infrastructure.decisionDigest!,
          }),
          constructWorkflow: async (config) => ({
            ...workflow,
            decisionDigest: config.decisionDigest,
          }),
          execute: executeManifestBoundFieldPreimageLengthWorkflow,
          bindsDecisionDigest: false,
        }),
        loadRuntime: async ({ invocation }) => {
          if (
            !("decisionDigest" in invocation) ||
            typeof invocation.decisionDigest !== "string"
          )
            throw new Error(
              "expected execution invocation with its fresh decision",
            );
          return {
            schemaVersion: WORKFLOW_RUNTIME_CONFIG,
            infrastructure: familyCommonInfrastructureForTest({
              headerHash: invocation.headerHash,
              decisionDigest: invocation.decisionDigest,
            }),
            resolveReferenceScript: emptyRosterReferenceScriptResolver,
            retainedDaSources: [publicSource],
            close,
          };
        },
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
