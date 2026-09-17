import { VALIDATION_TRACE_DISPUTE_STEP_COUNT } from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { DaLibp2pRetainedDaSource } from "../transition-trace/fetch.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
} from "../workflow/actuation-permit.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/adapters.js";
import {
  requireValidationTraceChallenge,
  type ValidationTraceChallenge,
  validationTraceMaterial,
} from "../workflow/challenge-authority.js";
import {
  assertManifestBoundWorkflowSigner,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { continuePendingWorkflow } from "../workflow/pending-continuation.js";
import { fraudProofRawL1SnapshotRequestForFamily } from "../workflow/raw-l1-family-derivation.js";
import { admitFraudProofRawL1Snapshot } from "../workflow/raw-l1-snapshot.js";
import { submitCapturedTransaction } from "../workflow/transaction-boundary.js";
import {
  bindValidationTraceDisputeWorkflowDeployment,
  type ValidationTraceDisputeWorkflowDeploymentBinding,
} from "./workflow-binding.js";
import {
  deriveValidationTraceDisputeChainStage,
  type ValidationTraceDisputeChainStage,
} from "./workflow-chain-state.js";
import {
  createValidationTraceDisputeActuator,
  decodeOperatorRevealProofsFromWitnessSet,
  planValidationTraceDisputeMove,
  type ValidationTraceDisputeActuationMaterial,
  type ValidationTraceDisputeRetainedRouteInput,
} from "./workflow-engine.js";
import {
  assertValidationTraceDisputeRosterIsManifestBound,
  VALIDATION_TRACE_DISPUTE_CATEGORY,
  VALIDATION_TRACE_DISPUTE_CATEGORY_ID,
  VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES,
  VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES,
  VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES,
} from "./workflow-family.js";

export const VALIDATION_TRACE_DISPUTE_WORKFLOW =
  "midgard-validation-trace-dispute-production-workflow-v1" as const;

export type ValidationTraceDisputeControlReferences = Readonly<{
  opener: UTxO;
  source: UTxO;
  game: UTxO;
  boundary: UTxO;
  timeout: UTxO;
  award: UTxO;
}>;

export type ValidationTraceDisputeRemovalReferences = Readonly<
  Record<keyof typeof VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES, UTxO>
>;

export type ValidationTraceDisputeReferences = Readonly<{
  control: ValidationTraceDisputeControlReferences;
  witnesses: Readonly<{
    computationThreadMint: UTxO;
    fraudProofMint: UTxO;
    phasMembershipWithdraw: UTxO;
  }>;
  removal: ValidationTraceDisputeRemovalReferences;
}>;

export const VALIDATION_TRACE_DISPUTE_CONFIG_KEYS = Object.freeze([
  "manifest",
  "blueprintJson",
  "deploymentInfo",
  "headerHash",
  "lucid",
  "signer",
  "source",
  "decisionDigest",
  "referenceScripts",
  "stateQueueMutationLeaseCoordinator",
] as const);

const configKeyJoin = (keys: readonly string[]): string =>
  [...keys].sort().join("\0");
const REQUIRED_CONFIG_KEY_JOIN = configKeyJoin(
  VALIDATION_TRACE_DISPUTE_CONFIG_KEYS,
);
const EXECUTION_CONFIG_KEY_JOIN = configKeyJoin([
  ...VALIDATION_TRACE_DISPUTE_CONFIG_KEYS,
  "challenge",
]);

export type ManifestBoundValidationTraceDisputeWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  /**
   * The freshly admitted W25 validation-trace challenge: the operator's
   * root-bound claim witness plus the challenger's deterministic replay,
   * rebuilt in this process by `admitValidationTraceChallenge`. A journal
   * copy or caller-authored object is refused by the admission registry.
   *
   * Optional at CONSTRUCTION so startup readiness can bind the manifest,
   * signer, and reference roster before any dispute exists — the same
   * contract as the other production families. EXECUTION fail-closes
   * without it: no transaction is planned or actuated challenge-free.
   */
  challenge?: ValidationTraceChallenge;
  referenceScripts: ValidationTraceDisputeReferences;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundValidationTraceDisputeWorkflow = Readonly<{
  binding: ValidationTraceDisputeWorkflowDeploymentBinding;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  decisionDigest: string;
  challenge: ValidationTraceChallenge | undefined;
  material: ValidationTraceDisputeActuationMaterial | undefined;
  l1: FraudProofFamilyL1ObservationPort<"validationTraceDispute">;
  actuator: ReturnType<typeof createValidationTraceDisputeActuator>;
  deriveStage: (
    currentTime: number,
  ) => Promise<ValidationTraceDisputeChainStage>;
}>;

/**
 * Manifest/reference/signer-bound construction for the sole interactive
 * family (ruling R6, "installed" semantics): from every derived chain stage
 * the workflow owns exactly one legal transaction, is waiting on the
 * operator's response clock with the timeout claim armed at its lapse, or
 * the journey is complete — detect, initiate, play every honest response,
 * claim timeout when the operator stalls, take the award, and remove the
 * fraudulent block. The dispute cursor is re-derived exclusively from live
 * chain state on every invocation, so an interrupted runner resumes from
 * where the chain — not local memory — says the dispute stands.
 */
export const createManifestBoundValidationTraceDisputeWorkflow = async (
  config: ManifestBoundValidationTraceDisputeWorkflowConfig,
): Promise<ManifestBoundValidationTraceDisputeWorkflow> => {
  const keyJoin = configKeyJoin(Object.keys(config));
  if (
    keyJoin !== REQUIRED_CONFIG_KEY_JOIN &&
    keyJoin !== EXECUTION_CONFIG_KEY_JOIN
  )
    throw new Error(
      "validationTraceDispute production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("validationTraceDispute decision digest is malformed");
  assertValidationTraceDisputeRosterIsManifestBound();
  const challenge =
    config.challenge === undefined
      ? undefined
      : requireValidationTraceChallenge(config.challenge);
  if (
    challenge !== undefined &&
    challenge.coordinate.headerHash !== config.headerHash
  )
    throw new Error(
      "validationTraceDispute challenge targets a different header",
    );
  const binding = await bindValidationTraceDisputeWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
  });
  if (
    challenge !== undefined &&
    challenge.coordinate.deploymentFingerprint !== binding.deploymentFingerprint
  )
    throw new Error(
      "validationTraceDispute challenge was admitted against a different deployment",
    );
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.validationTraceDispute;
  if (chain.steps.length !== VALIDATION_TRACE_DISPUTE_STEP_COUNT)
    throw new Error(
      "validationTraceDispute manifest omitted required deployed steps",
    );
  const bind = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo,
    });
  const control = Object.fromEntries(
    Object.entries(VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES).map(
      ([role, name]) => [
        role,
        bind(
          name,
          config.referenceScripts.control[
            role as keyof ValidationTraceDisputeControlReferences
          ],
        ),
      ],
    ),
  ) as ValidationTraceDisputeControlReferences;
  const witnesses = Object.freeze({
    computationThreadMint: bind(
      VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES.computationThreadMint,
      config.referenceScripts.witnesses.computationThreadMint,
    ),
    fraudProofMint: bind(
      VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES.fraudProofMint,
      config.referenceScripts.witnesses.fraudProofMint,
    ),
    phasMembershipWithdraw: bind(
      VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES.phasMembershipWithdraw,
      config.referenceScripts.witnesses.phasMembershipWithdraw,
    ),
  });
  for (const [role, name] of Object.entries(
    VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES,
  ))
    bind(
      name,
      config.referenceScripts.removal[
        role as keyof ValidationTraceDisputeRemovalReferences
      ],
    );
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const material: ValidationTraceDisputeActuationMaterial | undefined =
    challenge === undefined
      ? undefined
      : Object.freeze({
          headerHash: config.headerHash,
          ...validationTraceMaterial(challenge),
        });
  const rawL1 = l1.rawL1;
  if (rawL1 === undefined)
    throw new Error(
      "validationTraceDispute requires the raw L1 snapshot authority",
    );
  const snapshotRequest = fraudProofRawL1SnapshotRequestForFamily({
    definition: binding.definition,
    releaseFinality: binding.releaseFinality,
  });
  const operatorProofs = Object.freeze({
    /**
     * Operator bisection reveals harvested from the admitted raw thread-unit
     * transaction history: every `Continue(RevealOperator)` redeemer the
     * operator has published on-chain for this dispute.
     */
    collect: async () => {
      const snapshot = admitFraudProofRawL1Snapshot({
        value: await rawL1.capture(snapshotRequest),
        request: snapshotRequest,
        releaseFinality: binding.releaseFinality,
        observationDepth: "inclusion",
      });
      return snapshot.transactions.flatMap((transaction) =>
        decodeOperatorRevealProofsFromWitnessSet(transaction.witnessSetCbor),
      );
    },
  });
  const actuator = createValidationTraceDisputeActuator({
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    categoryId: VALIDATION_TRACE_DISPUTE_CATEGORY_ID,
    resolved: binding.resolvedContracts,
    references: {
      control: {
        source: control.source,
        game: control.game,
        boundary: control.boundary,
        timeout: control.timeout,
        award: control.award,
      },
      witnesses,
    },
    operatorProofs,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  });
  const deriveStage = async (currentTime: number) =>
    await deriveValidationTraceDisputeChainStage({
      lucid: config.lucid,
      chain,
      computationThreadPolicyId:
        binding.resolvedContracts.contracts.computationThread.policyId,
      fraudProofPolicyId:
        binding.resolvedContracts.contracts.fraudProof.policyId,
      fraudProofSpendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
      stateQueue: binding.definition.stateQueue,
      categoryId: VALIDATION_TRACE_DISPUTE_CATEGORY_ID,
      headerHash: config.headerHash,
      currentTime,
    });
  return Object.freeze({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    decisionDigest: config.decisionDigest,
    challenge,
    material,
    l1,
    actuator,
    deriveStage,
  });
};

const appendEvent = async (
  journal: FraudProofWorkflowJournalStore,
  workflowId: string,
  identity: FraudProofWorkflowIdentity,
  event: FraudProofWorkflowJournalEvent,
) => {
  const sequence = (await journal.load(workflowId)).length;
  await journal.append(
    {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence,
      recordedAt: new Date().toISOString(),
      event,
    },
    sequence,
  );
};

const preparedChallengeArtifact = (challenge: ValidationTraceChallenge) =>
  Object.freeze({
    schemaVersion: "midgard-validation-trace-dispute-prepared-v1" as const,
    challengeDigest: challenge.challengeDigest,
    claimCbor: challenge.claimCbor,
    challengerDescriptorCbor: challenge.challengerDescriptorCbor,
  });

/**
 * One chain-state-derived, locally evaluated, intent-journaled dispute move —
 * or the deliberate decision to wait on the operator's response clock.
 */
export const executeManifestBoundValidationTraceDisputeWorkflow = async ({
  workflow,
  journal,
}: {
  workflow: ManifestBoundValidationTraceDisputeWorkflow;
  journal: FraudProofWorkflowJournalStore;
}) => {
  const headerHash = workflow.binding.definition.headerHash;
  const challenge = workflow.challenge;
  const material = workflow.material;
  if (challenge === undefined || material === undefined)
    throw new Error(
      "validationTraceDispute execution requires the freshly admitted validation-trace challenge; construction without one serves startup readiness only",
    );
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: VALIDATION_TRACE_DISPUTE_CATEGORY,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest: workflow.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const preparedArtifact = preparedChallengeArtifact(challenge);
  const artifactDigest = journalJsonDigest(preparedArtifact);
  let entries = await journal.load(workflowId);
  if (entries.length === 0) {
    await appendEvent(journal, workflowId, identity, { kind: "started" });
    await appendEvent(journal, workflowId, identity, {
      kind: "prepared",
      artifact: preparedArtifact,
      artifactDigest,
    });
    entries = await journal.load(workflowId);
  }
  if (entries.length === 1) {
    await appendEvent(journal, workflowId, identity, {
      kind: "prepared",
      artifact: preparedArtifact,
      artifactDigest,
    });
    entries = await journal.load(workflowId);
  }
  const prepared = entries.find(({ event }) => event.kind === "prepared");
  if (
    prepared?.event.kind !== "prepared" ||
    prepared.event.artifactDigest !== artifactDigest
  )
    throw new Error(
      "validationTraceDispute journal was prepared for a different admitted challenge",
    );
  const pending = [...entries]
    .reverse()
    .find(({ event }) => event.kind === "submission_intent");
  const intent =
    pending?.event.kind === "submission_intent" ? pending.event : undefined;
  if (
    intent !== undefined &&
    !entries.some(
      ({ event }) =>
        event.kind === "confirmed" && event.actionId === intent.actionId,
    )
  ) {
    if (
      !(await workflow.l1.transactionConfirmed({
        headerHash,
        txHash: intent.txHash,
      }))
    )
      return { kind: "pending" as const, workflowId, txHash: intent.txHash };
    await appendEvent(journal, workflowId, identity, {
      kind: "reconciled",
      actionId: intent.actionId,
      txHash: intent.txHash,
      outcome: "confirmed",
    });
    await appendEvent(journal, workflowId, identity, {
      kind: "confirmed",
      actionId: intent.actionId,
      txHash: intent.txHash,
    });
  }
  const now = Date.now();
  const stage = await workflow.deriveStage(now);
  const retainedInput =
    intent !== undefined &&
    typeof intent.actionInput === "object" &&
    intent.actionInput !== null &&
    "durableRouteInput" in intent.actionInput
      ? ((
          intent.actionInput as {
            durableRouteInput?: ValidationTraceDisputeRetainedRouteInput;
          }
        ).durableRouteInput ?? undefined)
      : undefined;
  const move = planValidationTraceDisputeMove({
    stage,
    ...(retainedInput === undefined ? {} : { retained: retainedInput }),
  });
  if (move.kind === "completed")
    return { kind: "completed" as const, workflowId };
  if (move.kind === "await_counterparty")
    return {
      kind: "awaiting_counterparty" as const,
      workflowId,
      responseDeadline: move.responseDeadline,
    };
  const captured = await workflow.actuator.capture({
    action: move.action,
    material,
    ...(retainedInput === undefined ? {} : { retained: retainedInput }),
  });
  const actionId = `${move.action.stage}:${captured.transaction.txHash}`;
  await appendEvent(journal, workflowId, identity, {
    kind: "preflight_passed",
    actionId,
    txHash: captured.transaction.txHash,
    localEvaluator: "lucid-evolution-local-uplc-v1",
    referenceScripts: captured.transaction.referenceScripts,
  });
  await appendEvent(journal, workflowId, identity, {
    kind: "submission_intent",
    actionId,
    actionInput: {
      schemaVersion: "midgard-validation-trace-dispute-action-v1",
      stage: move.action.stage,
      challengeDigest: challenge.challengeDigest,
      ...(captured.durableRouteInput === undefined
        ? {}
        : { durableRouteInput: captured.durableRouteInput }),
    },
    ...(captured.mutationLease === undefined
      ? {}
      : {
          durableRecovery: {
            stateQueueMutationLease: {
              token: captured.mutationLease.token,
              source: captured.mutationLease.source,
            },
          },
        }),
    attempt: 1,
    txHash: captured.transaction.txHash,
  });
  const submitted = await submitCapturedTransaction(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error("validationTraceDispute provider substituted transaction");
  await appendEvent(journal, workflowId, identity, {
    kind: "submitted",
    actionId,
    attempt: 1,
    txHash: submitted,
  });
  return { kind: "pending" as const, workflowId, txHash: submitted };
};

export const runOrResumeManifestBoundValidationTraceDisputeWorkflow =
  async (input: {
    workflow: ManifestBoundValidationTraceDisputeWorkflow;
    journal: FraudProofWorkflowJournalStore;
  }) => {
    if (Object.keys(input).sort().join(",") !== "journal,workflow")
      throw new Error(
        "validationTraceDispute runner rejects caller-authored evidence",
      );
    return await executeManifestBoundValidationTraceDisputeWorkflow(input);
  };

export type LoadedValidationTraceDisputeWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundValidationTraceDisputeWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadValidationTraceDisputeWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedValidationTraceDisputeWorkflow>;

/** Standard strict loader-based surface consumed by ProductionWorkflowAdapter. */
export const createValidationTraceDisputeWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadValidationTraceDisputeWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== VALIDATION_TRACE_DISPUTE_CATEGORY)
        throw new Error("validationTraceDispute runner category changed");
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: VALIDATION_TRACE_DISPUTE_CATEGORY,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: VALIDATION_TRACE_DISPUTE_CATEGORY,
        headerHash: invocation.headerHash,
        checkpoint: "runner_start",
      });
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      try {
        if (
          loaded.schemaVersion !==
            "midgard-production-fraud-proof-runtime-config-v1" ||
          loaded.retainedDaSources.length === 0 ||
          loaded.retainedDaSources.some(
            (source) => !(source instanceof DaLibp2pRetainedDaSource),
          )
        )
          throw new Error(
            "validationTraceDispute requires concrete public retained DA",
          );
        const workflow =
          await createManifestBoundValidationTraceDisputeWorkflow(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "validationTraceDispute runtime binding changed invocation",
          );
        // The admitted challenge is root-bound to the freshly authenticated
        // canonical block for this header: re-fetch the retained payload and
        // require exact payload identity before actuating. A challenge-free
        // construction reaches execution below, which fail-closes with the
        // precise requirement.
        if (workflow.challenge !== undefined) {
          const block = await fetchCanonicalBlockEvidence({
            observation: await observeFraudProofWorkflowHeader(workflow.l1, {
              headerHash: invocation.headerHash,
            }),
            sources: loaded.retainedDaSources,
          });
          if (
            workflow.challenge.coordinate.payloadEnvelopeSha256 !==
              block.payloadEnvelopeSha256 ||
            workflow.challenge.coordinate.payloadSha256 !== block.payloadSha256
          )
            throw new Error(
              "validationTraceDispute challenge diverged from the authenticated canonical block",
            );
        }
        return await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            runOrResumeManifestBoundValidationTraceDisputeWorkflow({
              workflow,
              journal,
            }),
        });
      } finally {
        await loaded.close();
      }
    },
  });
