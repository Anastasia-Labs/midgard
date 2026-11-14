/**
 * The byte transcript of a direct-walk family's installed lifecycle: the
 * cursor adapter drives the family's port from a genuine Init to the proof
 * token, exactly as the family's installed-lifecycle suite does, and every
 * transaction the emulator accepts is kept as its two step-transcript digests.
 */
import { toUnit, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import { type LedgerEntry, singleLedgerAt } from "../../src/ledger-at.js";
import type { StateQueueMutationLeaseCoordinator } from "../../src/remove-fraudulent-block.js";
import { classifyCanonicalBlockViolations } from "../../src/workflow/classification.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  type CompleteCanonicalReplay,
  type CompleteCanonicalReplayContext,
} from "../../src/workflow/complete-replay.js";
import {
  createCursorFamilyWorkflowAdapter,
  type CursorFamilyTransactionPort,
} from "../../src/workflow/cursor-family-adapter.js";
import type { CursorFamilySpec } from "../../src/workflow/cursor-family-state.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "../../src/workflow/family-l1-observation.js";
import {
  computeFraudProofWorkflowId,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
  MemoryFraudProofWorkflowJournalStore,
} from "../../src/workflow/journal.js";
import { authenticatedHeaderObservation } from "../helpers/canonical-block-evidence-fixture.js";
import type { DirectWalkCategory } from "./direct-walk-emulator.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
import { recordStepSubmissions, sha256 } from "./step-transcript.js";
import {
  type FaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
} from "./submit-init-emulator-shared.js";

/** The installed lifecycle's bound on cursor actions. */
const MAX_ACTIONS = 120;

type DirectWalkAction = Readonly<{ stage: string; txHash: string }>;

/**
 * Runs `drive` under a step recording seeded by `seed` and renders every
 * emulator submission: the setup transactions as one count and digest of
 * each kind, then each cursor action as its stage, full digest and step
 * digest.
 */
export const recordDirectWalkTranscript = async (
  seed: string,
  drive: () => Promise<readonly DirectWalkAction[]>,
): Promise<string> => {
  const recording = recordStepSubmissions(seed);
  try {
    const actions = await drive();
    const byHash = new Map(actions.map((action) => [action.txHash, action]));
    const setup = recording.submitted.filter(
      ({ txHash }) => !byHash.has(txHash),
    );
    const lines = recording.submitted.flatMap(({ txHash, cbor, step }) => {
      const action = byHash.get(txHash);
      return action === undefined
        ? []
        : [`${action.stage} ${txHash} ${cbor} ${step}`];
    });
    expect(lines).toHaveLength(actions.length);
    const digest = (rows: readonly string[]) =>
      sha256(rows.join("\n")).toString("hex");
    return [
      seed,
      `setup ${setup.length.toString()} ${digest(
        setup.map(({ txHash, cbor }) => `${txHash} ${cbor}`),
      )} ${digest(setup.map(({ step }) => step))}`,
      ...lines,
    ].join("\n  ");
  } finally {
    recording.restore();
  }
};

/** What a family's installed lifecycle stands up before its cursor runs. */
export type DirectWalkScene = Readonly<{
  harness: FaultProofEmulatorHarness &
    Readonly<{ category: Readonly<{ categoryId: string }> }>;
  /** The family's thread contracts. */
  contracts: Readonly<{
    steps: readonly Readonly<{ spendingScriptAddress: string }>[];
    computationThread: Readonly<{ policyId: string }>;
    fraudProof: Readonly<{ policyId: string; spendingScriptAddress: string }>;
  }>;
  scenario: Readonly<{
    setup: Readonly<{ headerHash: string; fraudulentBlockOutRef: string }>;
    block: Readonly<{
      header: Parameters<typeof authenticatedHeaderObservation>[0]["header"];
      reconstruction: Readonly<{ payloadEnvelopeCbor: Uint8Array }>;
    }>;
  }>;
  /** The ledger the subject's inputs resolve against; absent where the
   * lifecycle replays without a predecessor. */
  predecessor?: readonly LedgerEntry[];
  publishRoles: () => Promise<ReadonlyMap<string, Readonly<{ utxo: UTxO }>>>;
}>;

export type DirectWalkSceneFamily<Category extends DirectWalkCategory> =
  Readonly<{
    /** The family's kebab label, as its lifecycle names its fixtures. */
    label: string;
    category: Category;
    spec: CursorFamilySpec<Category>;
    replay: CompleteCanonicalReplay;
    roleOrder: readonly string[];
    /** The family's port factory; its config is the lifecycle's. */
    createPort: (config: never) => CursorFamilyTransactionPort<Category>;
  }>;

const outRef = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const provenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "lucid-emulator",
  grade: "security",
} as const;

/**
 * The installed lifecycle: complete replay detects the fault, the port
 * prepares a journaled artifact, and the cursor adapter drives every action up
 * to removal across an emulated restart after each broadcast. Returns the
 * actions in order; removal itself is not the family's.
 */
export const driveDirectWalkLifecycle = async <
  Category extends DirectWalkCategory,
>(
  family: DirectWalkSceneFamily<Category>,
  scene: DirectWalkScene,
): Promise<readonly DirectWalkAction[]> => {
  const { harness: h, contracts, scenario } = scene;
  const { setup } = scenario;
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({
      header: scenario.block.header,
      headerHash: setup.headerHash,
    }),
    payloadEnvelopeCbor: scenario.block.reconstruction.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: `test/${family.label}-retained-da`,
      grade: "security",
    },
  });
  const replayContext: CompleteCanonicalReplayContext | undefined =
    scene.predecessor === undefined
      ? undefined
      : {
          predecessor: await admitCompleteCanonicalReplayPredecessor({
            ledgerAt: singleLedgerAt(
              evidence.header.prevHeaderHash,
              scene.predecessor,
            ),
            currentEvidence: evidence,
          }),
        };
  const published = await scene.publishRoles();
  const certificate = await publishPlainReferenceScriptUtxo({
    lucid: h.proverLucid,
    script: h.contracts.fieldPreimageCertificate.mintingScript,
    label: `${family.label} field certificate`,
  });
  const leaseCoordinator: StateQueueMutationLeaseCoordinator = {
    acquire: async () => ({
      token: `${family.label}-lease`,
      source: "emulator",
      renew: async () => {},
      release: async () => {},
      fail: async () => {},
    }),
  };
  const port = family.createPort({
    binding: {
      definition: { headerHash: setup.headerHash },
      blueprint: h.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue),
      network: "Custom",
      resolvedContracts: { category: h.category },
      releaseEconomics: { policy: { fraudProverRewardLovelace: "0" } },
    },
    lucid: h.proverLucid,
    signer: h.proverSigner,
    contracts,
    references: {
      steps: family.roleOrder.map((role) => published.get(role)!.utxo),
      witnesses: h.witnessReferenceScripts,
      fieldPreimageCertificateMint: certificate.utxo,
    },
    ...(replayContext === undefined ? {} : { replayContext }),
    stateQueueMutationLeaseCoordinator: leaseCoordinator,
  } as never);
  const decision = await family.replay.replay(evidence, replayContext);
  const classification = await classifyCanonicalBlockViolations({
    evidence,
    detections: decision.detections,
    minimumConfirmationDepth: 1,
  });
  if (
    classification.decision !== "fault_detected" ||
    classification.category !== family.category
  )
    throw new Error(`${family.label} block did not detect`);
  const artifact = await port.prepare({
    evidence,
    classification: { ...classification, category: family.category },
  });
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
    deploymentFingerprint: "aa".repeat(32),
    category: family.category,
    target: { kind: "state_queue_header", headerHash: setup.headerHash },
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const store = new MemoryFraudProofWorkflowJournalStore();
  const append = async (event: FraudProofWorkflowJournalEvent) => {
    const entries = await store.load(workflowId);
    await store.append(
      {
        schemaVersion: "midgard-fraud-proof-workflow-journal-entry-v1",
        workflowId,
        identity,
        sequence: entries.length,
        recordedAt: new Date().toISOString(),
        event,
      },
      entries.length,
    );
  };
  await append({ kind: "started" });
  await append({
    kind: "prepared",
    artifact,
    artifactDigest: journalJsonDigest(artifact),
  });
  const threadUnit = toUnit(
    contracts.computationThread.policyId,
    h.category.categoryId + setup.headerHash,
  );
  const proofUnit = toUnit(
    contracts.fraudProof.policyId,
    h.category.categoryId + setup.headerHash,
  );
  const confirmed = new Set<string>();
  const l1: FraudProofFamilyL1ObservationPort<Category> = {
    portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
    category: family.category,
    publications: {} as never,
    observeHeader: async () => evidence.observation,
    transactionConfirmed: async ({ txHash }) => confirmed.has(txHash),
    observe: async () => {
      for (const [index, step] of contracts.steps.entries()) {
        const outputs = await h.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          threadUnit,
        );
        if (outputs.length === 1)
          return {
            provenance,
            stage: {
              kind: "step",
              step: index + 1,
              threadOutRef: outRef(outputs[0]!),
              stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            },
          } as never;
      }
      const [proof, ...others] = await h.proverLucid.utxosAtWithUnit(
        contracts.fraudProof.spendingScriptAddress,
        proofUnit,
      );
      return {
        provenance,
        stage:
          proof !== undefined && others.length === 0
            ? {
                kind: "proof_token",
                fraudProofOutRef: outRef(proof),
                stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
                nextRemovalOutRef: setup.fraudulentBlockOutRef,
              }
            : {
                kind: "not_started",
                stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
              },
      };
    },
  };
  const adapter = () =>
    createCursorFamilyWorkflowAdapter({
      spec: family.spec,
      l1,
      transactions: port,
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
  // The journal hands each restart its artifact as stored JSON.
  const restored = JSON.parse(JSON.stringify(artifact));
  const actions: DirectWalkAction[] = [];
  for (let n = 0; n < MAX_ACTIONS; n++) {
    const context = {
      identity,
      workflowId,
      artifact: restored,
      entries: await store.load(workflowId),
    };
    const current = adapter();
    const observation = await current.observe(context);
    if (observation.kind !== "action_required")
      throw new Error(`${family.label}: no action at ${n.toString()}`);
    const { action } = observation;
    if (action.input.stage === "remove") return actions;
    const preflight = await current.preflight({ ...context, action });
    const { actionId } = action;
    const { txHash } = preflight;
    await append({
      kind: "preflight_passed",
      actionId,
      txHash,
      localEvaluator: preflight.localUplcEvaluation.evaluator,
      referenceScripts: preflight.referenceScripts,
    });
    await append({
      kind: "submission_intent",
      actionId,
      actionInput: action.input,
      attempt: 1,
      txHash,
    });
    await current.submit({ ...context, action, preflight });
    h.emulator.awaitBlock();
    confirmed.add(txHash);
    await append({
      kind: "submission_ambiguous",
      actionId,
      attempt: 1,
      txHash,
      detail: "emulated restart after broadcast before receipt",
    });
    const recovered = await adapter().reconcile({ ...context, action, txHash });
    if (recovered.kind !== "confirmed" || recovered.txHash !== txHash)
      throw new Error(`${family.label}: ${txHash} did not reconcile`);
    await append({
      kind: "reconciled",
      actionId,
      outcome: "confirmed",
      txHash,
    });
    await append({ kind: "confirmed", actionId, txHash });
    actions.push({ stage: String(action.input.stage), txHash });
  }
  throw new Error(
    `${family.label}: no removal after ${MAX_ACTIONS.toString()} actions`,
  );
};
