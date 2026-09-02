import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "../workflow/family-l1-observation-v1.js";
import {
  computeFraudProofWorkflowIdV1,
  DirectoryFraudProofWorkflowJournalStoreV1,
  FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
  type FraudProofWorkflowIdentityV1,
  type FraudProofWorkflowJournalEntryV1,
  type FraudProofWorkflowJournalEventV1,
  type FraudProofWorkflowJournalStoreV1,
  journalJsonDigestV1,
  type JournalJsonObjectV1,
} from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import type { FraudProofWorkflowActionV1 } from "../workflow/orchestrator-v1.js";
import {
  assertProductionWorkflowJournalActuationV1,
  bindProductionWorkflowActuationJournalV1,
} from "../workflow/production-actuation-permit-v1.js";
import {
  PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
  type ProductionWorkflowAdapterReadinessInputV1,
  type ProductionWorkflowAdapterRunnerV1,
} from "../workflow/production-adapters-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePortV1,
  type ProductionFieldCarriagePrerequisitePortV1,
} from "../workflow/production-field-carriage-prerequisite-v1.js";
import { bindProductionWorkflowFundingReservationJournalV1 } from "../workflow/production-funding-reservation-permit-v1.js";
import {
  submitCapturedTransactionV1,
  workflowTransactionInputOutRefsV1,
} from "../workflow/transaction-boundary-v1.js";
import {
  MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES_V1,
  type MintDeclaredAssetLimitContractsV1,
} from "./contracts-v1.js";
import {
  createMintDeclaredAssetLimitActuatorV1,
  type MintDeclaredAssetLimitActuatorActionV1,
  mintDeclaredAssetLimitFieldRequirementV1,
  type MintDeclaredAssetLimitWorkflowReferenceScriptsV1,
} from "./production-actuator-v1.js";
import {
  admitProductionMintDeclaredAssetLimitArtifactV1,
  productionMintDeclaredAssetLimitArtifactDigestV1,
  type ProductionMintDeclaredAssetLimitArtifactV1,
} from "./production-artifact-v1.js";
import {
  detectMintDeclaredAssetLimitAcceptedRawReplayV1,
  mintDeclaredAssetLimitRawBlockEvidenceFromVerifiedPayloadV1,
  prepareProductionMintDeclaredAssetLimitAcceptedArtifactV1,
  prepareProductionMintDeclaredAssetLimitForcedArtifactV1,
} from "./replay-v1.js";
import {
  MintDeclaredAssetLimitStep02DatumV1Schema,
  MintDeclaredAssetLimitStep03DatumV1Schema,
  MintDeclaredAssetLimitStep04DatumV1Schema,
} from "./schemas-v1.js";
import {
  hashMintDeclaredGrammarCheckpointV1,
  hashMintDeclaredWalkCheckpointV1,
} from "./staged-plan-v1.js";

export const MINT_DECLARED_ASSET_LIMIT_PRODUCTION_WORKFLOW_V1 =
  "midgard-mint-declared-asset-limit-production-workflow-v1" as const;

export type MintDeclaredAssetLimitRemovalReferenceScriptsV1 = Readonly<{
  correctionLockSpend: UTxO;
  stateQueueSpend: UTxO;
  stateQueueMint: UTxO;
  stateQueueFraudRemovalWithdraw: UTxO;
  activeOperatorsSpend: UTxO;
  activeOperatorsMint: UTxO;
  retiredOperatorsSpend: UTxO;
  retiredOperatorsMint: UTxO;
  schedulerSpend: UTxO;
}>;

export type ManifestBoundMintDeclaredAssetLimitWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: MintDeclaredAssetLimitWorkflowReferenceScriptsV1 &
    Readonly<{ removal: MintDeclaredAssetLimitRemovalReferenceScriptsV1 }>;
}>;

export type ManifestBoundMintDeclaredAssetLimitWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"mintDeclaredAssetLimit">;
  l1: FraudProofFamilyL1ObservationPortV1<"mintDeclaredAssetLimit">;
  actuator: ReturnType<typeof createMintDeclaredAssetLimitActuatorV1>;
  prerequisite: ProductionFieldCarriagePrerequisitePortV1<"mintDeclaredAssetLimit">;
  lucid: LucidEvolution;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const CATEGORY = "mintDeclaredAssetLimit" as const;

export const createManifestBoundMintDeclaredAssetLimitWorkflowV1 = async (
  config: ManifestBoundMintDeclaredAssetLimitWorkflowConfigV1,
): Promise<ManifestBoundMintDeclaredAssetLimitWorkflowV1> => {
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("mintDeclaredAssetLimit decision digest is malformed");
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: CATEGORY,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      MintDeclaredAssetLimitStep02DatumV1Schema,
      MintDeclaredAssetLimitStep02DatumV1Schema,
      MintDeclaredAssetLimitStep03DatumV1Schema,
      MintDeclaredAssetLimitStep04DatumV1Schema,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts[CATEGORY];
  const certificate = binding.fieldPreimageCertificate;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 4 ||
    certificate === null ||
    stateQueuePolicyId === undefined
  )
    throw new Error(
      "mintDeclaredAssetLimit manifest omitted required contracts",
    );
  const bindReference = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName: name,
      utxo,
    });
  const steps = MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES_V1.map(
    (_title, index) =>
      bindReference(
        [
          "fraudProofMintDeclaredAssetLimit",
          "fraudProofMintDeclaredAssetLimitStep02",
          "fraudProofMintDeclaredAssetLimitStep03",
          "fraudProofMintDeclaredAssetLimitStep04",
        ][index]!,
        config.referenceScripts.steps[index]!,
      ),
  ) as unknown as MintDeclaredAssetLimitWorkflowReferenceScriptsV1["steps"];
  const witnessNames = {
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
    pexcludesWithdraw: "pexcludesWithdraw",
  } as const;
  const witnesses = Object.fromEntries(
    Object.entries(witnessNames).map(([role, name]) => [
      role,
      bindReference(
        name,
        config.referenceScripts.witnesses[
          role as keyof FaultProofWitnessReferenceScriptsV1
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScriptsV1>;
  const references = Object.freeze({
    steps: Object.freeze(steps),
    witnesses: Object.freeze(witnesses),
    fieldPreimageCertificateMint: bindReference(
      "fieldPreimageCertificateMint",
      config.referenceScripts.fieldPreimageCertificateMint,
    ),
  });
  const contracts: MintDeclaredAssetLimitContractsV1 = Object.freeze({
    steps: chain.steps.map((step, index) => ({
      blueprintTitle: MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES_V1[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as MintDeclaredAssetLimitContractsV1["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
    fieldPreimageCertificateMintingScript: certificate.mintingScript,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const actuator = createMintDeclaredAssetLimitActuatorV1({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const prerequisite = createAuthenticatedFieldCarriagePrerequisitePortV1({
    category: CATEGORY,
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) =>
      mintDeclaredAssetLimitFieldRequirementV1({
        action:
          action.input as unknown as MintDeclaredAssetLimitActuatorActionV1,
        artifact,
        owner: config.signer.paymentKeyHash,
        certificate: {
          policyId: certificate.policyId,
          mintingScript: certificate.mintingScript,
          referenceScriptUtxo: references.fieldPreimageCertificateMint,
        },
      }),
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  return Object.freeze({
    binding,
    l1,
    lucid: config.lucid,
    actuator,
    prerequisite,
    decisionDigest: config.decisionDigest,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
};

const appendEvent = async ({
  journal,
  workflowId,
  identity,
  event,
}: {
  readonly journal: FraudProofWorkflowJournalStoreV1;
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentityV1;
  readonly event: FraudProofWorkflowJournalEventV1;
}) => {
  const sequence = (await journal.load(workflowId)).length;
  const entry: FraudProofWorkflowJournalEntryV1 = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
    workflowId,
    identity,
    sequence,
    recordedAt: new Date().toISOString(),
    event,
  };
  await journal.append(entry, sequence);
};

export const mintDeclaredAssetLimitActionIdV1 = (
  action: MintDeclaredAssetLimitActuatorActionV1,
): string =>
  `mintDeclaredAssetLimit:${Buffer.from(JSON.stringify(action)).toString("hex")}`;

const workflowAction = (
  action: MintDeclaredAssetLimitActuatorActionV1,
): FraudProofWorkflowActionV1 => ({
  actionId: mintDeclaredAssetLimitActionIdV1(action),
  input: {
    schemaVersion: "midgard-production-cursor-family-action-v1",
    category: "mintDeclaredAssetLimit",
    ...action,
  },
});

const currentAction = async ({
  workflow,
  artifact,
}: {
  readonly workflow: ManifestBoundMintDeclaredAssetLimitWorkflowV1;
  readonly artifact: ProductionMintDeclaredAssetLimitArtifactV1;
}): Promise<MintDeclaredAssetLimitActuatorActionV1 | "removed"> => {
  const stage = (await workflow.l1.observe({ headerHash: artifact.headerHash }))
    .stage;
  const admitted = admitProductionMintDeclaredAssetLimitArtifactV1(artifact);
  if (stage.kind === "not_started")
    return {
      stage: "init",
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
    };
  if (stage.kind === "proof_token")
    return {
      stage: "remove",
      nextRemovalOutRef: stage.nextRemovalOutRef,
      fraudProofOutRef: stage.fraudProofOutRef,
    };
  if (stage.kind === "removed") return "removed";
  if (stage.step === 1)
    return {
      stage: "step_01",
      threadOutRef: stage.threadOutRef,
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
    };
  if (stage.step === 2) {
    const [txHash, outputIndex] = stage.threadOutRef.split("#");
    const [utxo] = await workflow.lucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    if (utxo?.datum == null)
      throw new Error("mintDeclaredAssetLimit step-02 datum disappeared");
    const state = Data.from(
      utxo.datum,
      MintDeclaredAssetLimitStep02DatumV1Schema as never,
    ) as { data: Record<string, unknown> };
    if ("Bound" in state.data)
      return admitted.evidence.carriage === "Certified"
        ? {
            stage: "step_02",
            threadOutRef: stage.threadOutRef,
            action: { kind: "grammar_start" },
          }
        : {
            stage: "step_02",
            threadOutRef: stage.threadOutRef,
            action: { kind: "direct" },
          };
    const grammar = (state.data.Grammar as { checkpoint_hash: string })
      .checkpoint_hash;
    const ordinal = admitted.staged.grammar.findIndex(
      (checkpoint) =>
        hashMintDeclaredGrammarCheckpointV1(checkpoint) === grammar,
    );
    if (ordinal < 0)
      throw new Error("mintDeclaredAssetLimit grammar checkpoint substitution");
    return ordinal === admitted.staged.grammar.length - 1
      ? {
          stage: "step_02",
          threadOutRef: stage.threadOutRef,
          action: { kind: "grammar_finish" },
        }
      : {
          stage: "step_02",
          threadOutRef: stage.threadOutRef,
          action: { kind: "grammar_resume", nextOrdinal: ordinal + 1 },
        };
  }
  if (stage.step === 3) {
    const [txHash, outputIndex] = stage.threadOutRef.split("#");
    const [utxo] = await workflow.lucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    if (utxo?.datum == null)
      throw new Error("mintDeclaredAssetLimit step-03 datum disappeared");
    const state = Data.from(
      utxo.datum,
      MintDeclaredAssetLimitStep03DatumV1Schema as never,
    ) as { data: { checkpoint_hash: string } };
    const hash = state.data.checkpoint_hash;
    const ordinal = [
      admitted.staged.initialWalk,
      ...admitted.staged.walk,
    ].findIndex(
      (checkpoint) => hashMintDeclaredWalkCheckpointV1(checkpoint) === hash,
    );
    if (ordinal < 0 || ordinal >= admitted.staged.walk.length)
      throw new Error("mintDeclaredAssetLimit fold checkpoint substitution");
    return {
      stage: "step_03",
      threadOutRef: stage.threadOutRef,
      walkOrdinal: ordinal,
    };
  }
  if (stage.step === 4)
    return { stage: "step_04", threadOutRef: stage.threadOutRef };
  throw new Error("mintDeclaredAssetLimit observed an impossible step");
};

export type MintDeclaredAssetLimitWorkflowRunResultV1 = Readonly<{
  kind: "pending" | "completed";
  workflowId: string;
  txHash?: string;
}>;

export const reconcileMintDeclaredAssetLimitSubmissionIntentV1 = ({
  intendedActionId,
  txHash,
  transactionConfirmed,
  observedAction,
}: {
  readonly intendedActionId: string;
  readonly txHash: string;
  readonly transactionConfirmed: boolean;
  readonly observedAction: MintDeclaredAssetLimitActuatorActionV1 | "removed";
}):
  | Readonly<{ kind: "confirmed"; txHash: string }>
  | Readonly<{ kind: "pending"; txHash: string }>
  | Readonly<{ kind: "conflict"; txHash: string }> => {
  if (transactionConfirmed) return { kind: "confirmed", txHash };
  if (
    observedAction === "removed" ||
    mintDeclaredAssetLimitActionIdV1(observedAction) !== intendedActionId
  )
    return { kind: "conflict", txHash };
  return { kind: "pending", txHash };
};

/** Deterministic durability prelude shared by prerequisite and proof actions. */
export const mintDeclaredAssetLimitSubmissionPreludeV1 = ({
  actionId,
  actionInput,
  txHash,
  referenceScripts,
  durableRecovery,
}: {
  readonly actionId: string;
  readonly actionInput: JournalJsonObjectV1;
  readonly txHash: string;
  readonly referenceScripts: Extract<
    FraudProofWorkflowJournalEventV1,
    { readonly kind: "preflight_passed" }
  >["referenceScripts"];
  readonly durableRecovery?: JournalJsonObjectV1;
}): readonly [
  Extract<
    FraudProofWorkflowJournalEventV1,
    { readonly kind: "preflight_passed" }
  >,
  Extract<
    FraudProofWorkflowJournalEventV1,
    { readonly kind: "submission_intent" }
  >,
] =>
  Object.freeze([
    Object.freeze({
      kind: "preflight_passed",
      actionId,
      txHash,
      localEvaluator: "lucid-evolution-local-uplc-v1",
      referenceScripts,
    }),
    Object.freeze({
      kind: "submission_intent",
      actionId,
      actionInput,
      ...(durableRecovery === undefined ? {} : { durableRecovery }),
      attempt: 1,
      txHash,
    }),
  ]);

/** One crash-safe action per call; callers resume by invoking the same runner. */
export const executeManifestBoundMintDeclaredAssetLimitWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMintDeclaredAssetLimitWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<MintDeclaredAssetLimitWorkflowRunResultV1> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  const fetched = await fetchRetainedDaPayloadByHeaderHash({
    headerHash: observation.headerHash,
    sources,
  });
  const raw = await mintDeclaredAssetLimitRawBlockEvidenceFromVerifiedPayloadV1(
    {
      observation,
      payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
      daProvenance: fetched.provenance,
    },
  );
  const artifact =
    detectMintDeclaredAssetLimitAcceptedRawReplayV1(raw).length > 0
      ? await prepareProductionMintDeclaredAssetLimitAcceptedArtifactV1(raw)
      : await prepareProductionMintDeclaredAssetLimitForcedArtifactV1(
          await fetchCanonicalBlockEvidenceV1({ observation, sources }),
        );
  const identity: FraudProofWorkflowIdentityV1 = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: CATEGORY,
    target: { kind: "state_queue_header", headerHash: artifact.headerHash },
    decisionDigest: workflow.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowIdV1(identity);
  let entries = await journal.load(workflowId);
  if (entries.length === 0) {
    await appendEvent({
      journal,
      workflowId,
      identity,
      event: { kind: "started" },
    });
    await appendEvent({
      journal,
      workflowId,
      identity,
      event: {
        kind: "prepared",
        artifact,
        artifactDigest:
          productionMintDeclaredAssetLimitArtifactDigestV1(artifact),
      },
    });
    entries = await journal.load(workflowId);
  } else {
    const prepared = entries.find((entry) => entry.event.kind === "prepared");
    if (
      prepared?.event.kind !== "prepared" ||
      prepared.event.artifactDigest !==
        productionMintDeclaredAssetLimitArtifactDigestV1(artifact)
    )
      throw new Error("mintDeclaredAssetLimit durable artifact substitution");
  }
  const lastIntent = [...entries]
    .reverse()
    .find((entry) => entry.event.kind === "submission_intent");
  if (lastIntent?.event.kind === "submission_intent") {
    const intent = lastIntent.event;
    const confirmed = entries.some(
      (entry) =>
        entry.event.kind === "confirmed" &&
        entry.event.actionId === intent.actionId,
    );
    if (!confirmed) {
      const onChain = await workflow.l1.transactionConfirmed({
        headerHash: artifact.headerHash,
        txHash: intent.txHash,
      });
      const recovery = intent.durableRecovery?.stateQueueMutationLease;
      const resumed =
        typeof recovery === "object" &&
        recovery !== null &&
        !Array.isArray(recovery) &&
        workflow.stateQueueMutationLeaseCoordinator.resume !== undefined
          ? await workflow.stateQueueMutationLeaseCoordinator.resume(
              recovery as { token: string; source: string },
            )
          : undefined;
      if (!onChain) {
        const observedAction = await currentAction({ workflow, artifact });
        const reconciliation =
          reconcileMintDeclaredAssetLimitSubmissionIntentV1({
            intendedActionId: intent.actionId,
            txHash: intent.txHash,
            transactionConfirmed: false,
            observedAction,
          });
        if (reconciliation.kind === "conflict") {
          await resumed?.fail(
            "mintDeclaredAssetLimit observed a different transaction at the durable cursor",
          );
          throw new Error(
            "mintDeclaredAssetLimit transaction substitution changed the durable cursor",
          );
        }
        await resumed?.renew();
        return { kind: "pending", workflowId, txHash: intent.txHash };
      }
      await resumed?.release();
      await appendEvent({
        journal,
        workflowId,
        identity,
        event: {
          kind: "reconciled",
          actionId: intent.actionId,
          outcome: "confirmed",
          txHash: intent.txHash,
        },
      });
      await appendEvent({
        journal,
        workflowId,
        identity,
        event: {
          kind: "confirmed",
          actionId: intent.actionId,
          txHash: intent.txHash,
        },
      });
    }
  }
  const action = await currentAction({ workflow, artifact });
  if (action === "removed") {
    const terminalStage = (
      await workflow.l1.observe({ headerHash: artifact.headerHash })
    ).stage;
    if (terminalStage.kind !== "removed")
      throw new Error("mintDeclaredAssetLimit terminal observation changed");
    const complete = (await journal.load(workflowId)).some(
      (entry) => entry.event.kind === "completed",
    );
    if (!complete)
      await appendEvent({
        journal,
        workflowId,
        identity,
        event: {
          kind: "completed",
          terminal: terminalStage.terminal,
          terminalDigest: journalJsonDigestV1(
            terminalStage.terminal as unknown as JournalJsonObjectV1,
          ),
        },
      });
    return { kind: "completed", workflowId };
  }
  const baseAction = workflowAction(action);
  const prerequisite = await workflow.prerequisite.inspect({
    headerHash: artifact.headerHash,
    baseAction,
    artifact,
    entries: await journal.load(workflowId),
  });
  if (prerequisite.kind === "pending") return { kind: "pending", workflowId };
  if (prerequisite.kind === "required") {
    const captured = await workflow.prerequisite.capture({
      headerHash: artifact.headerHash,
      action: prerequisite.action,
      artifact,
    });
    const prelude = mintDeclaredAssetLimitSubmissionPreludeV1({
      actionId: prerequisite.action.actionId,
      actionInput: prerequisite.action.input,
      txHash: captured.transaction.txHash,
      referenceScripts: captured.transaction.referenceScripts,
      durableRecovery: captured.durableRecovery,
    });
    for (const event of prelude)
      await appendEvent({ journal, workflowId, identity, event });
    const submitted = await submitCapturedTransactionV1(captured.transaction);
    await appendEvent({
      journal,
      workflowId,
      identity,
      event: {
        kind: "submitted",
        actionId: prerequisite.action.actionId,
        attempt: 1,
        txHash: submitted,
      },
    });
    return { kind: "pending", workflowId, txHash: submitted };
  }
  const id = baseAction.actionId;
  const captured = await workflow.actuator.capture({ action, artifact });
  const prelude = mintDeclaredAssetLimitSubmissionPreludeV1({
    actionId: id,
    actionInput: baseAction.input,
    txHash: captured.transaction.txHash,
    referenceScripts: captured.transaction.referenceScripts,
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
  });
  for (const event of prelude)
    await appendEvent({ journal, workflowId, identity, event });
  const submitted = await submitCapturedTransactionV1(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error("mintDeclaredAssetLimit provider substituted transaction");
  await appendEvent({
    journal,
    workflowId,
    identity,
    event: { kind: "submitted", actionId: id, attempt: 1, txHash: submitted },
  });
  if (
    action.stage === "remove" &&
    !workflowTransactionInputOutRefsV1(captured.transaction.signed).includes(
      action.nextRemovalOutRef,
    )
  )
    throw new Error("mintDeclaredAssetLimit removal changed mutation target");
  return { kind: "pending", workflowId, txHash: submitted };
};

export type LoadedMintDeclaredAssetLimitProductionWorkflowV1 = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundMintDeclaredAssetLimitWorkflowConfigV1;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadMintDeclaredAssetLimitProductionWorkflowV1 = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: ProductionWorkflowAdapterReadinessInputV1;
}) => Promise<LoadedMintDeclaredAssetLimitProductionWorkflowV1>;

/** Standard runtime-loader-compatible package runner; core config has no callbacks. */
export const createMintDeclaredAssetLimitProductionWorkflowRunnerSurfaceV1 = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadMintDeclaredAssetLimitProductionWorkflowV1;
}): ProductionWorkflowAdapterRunnerV1 =>
  Object.freeze({
    runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
    runOrResume: async (invocation) => {
      if (invocation.category !== (CATEGORY as string))
        throw new Error("mintDeclaredAssetLimit runner category changed");
      const journal = bindProductionWorkflowFundingReservationJournalV1({
        permit: invocation.fundingReservationPermit,
        journal: bindProductionWorkflowActuationJournalV1({
          journal: new DirectoryFraudProofWorkflowJournalStoreV1(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: CATEGORY,
          headerHash: invocation.headerHash,
        }),
      });
      assertProductionWorkflowJournalActuationV1({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: CATEGORY,
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
            "mintDeclaredAssetLimit runtime requires concrete public retained DA",
          );
        const workflow =
          await createManifestBoundMintDeclaredAssetLimitWorkflowV1(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "mintDeclaredAssetLimit runtime binding changed invocation",
          );
        return (await executeManifestBoundMintDeclaredAssetLimitWorkflowV1({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        })) as never;
      } finally {
        await loaded.close();
      }
    },
  });
