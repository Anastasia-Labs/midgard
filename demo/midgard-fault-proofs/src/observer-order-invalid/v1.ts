import { deriveMidgardNativeTxFaultEvidenceMaterial } from "@al-ft/midgard-core";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence.js";
import {
  buildTrieView,
  requireTransactionsRootMatch,
} from "../prepare-double-spend.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
  workflowActuationAuthorizingDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "../workflow/actuation-permit.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/adapters.js";
import { OBSERVER_ORDER_INVALID_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
  type ManifestBoundFamilyWorkflow,
} from "../workflow/family-definition.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FieldCarriagePrerequisitePort } from "../workflow/field-carriage-prerequisite.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  resumeRecordedFraudProofWorkflow,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import { continuePendingWorkflow } from "../workflow/pending-continuation.js";
import type { FraudProofRawL1FamilyStage } from "../workflow/raw-l1-family-derivation.js";
import {
  createObserverOrderInvalidActuator,
  type ObserverOrderInvalidActuatorAction,
  observerOrderInvalidFieldRequirement,
  type ObserverOrderInvalidWorkflowReferenceScripts,
} from "./actuator.js";
import {
  admitObserverOrderInvalidArtifact,
  type ObserverOrderInvalidArtifact,
} from "./artifact.js";
import {
  OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES,
  type ObserverOrderInvalidContracts,
} from "./contracts.js";
import {
  detectObserverOrderInvalidAcceptedRawReplay,
  OBSERVER_ORDER_INVALID_RAW_EVIDENCE,
  prepareObserverOrderInvalidAcceptedArtifact,
  prepareObserverOrderInvalidForcedArtifact,
} from "./replay.js";
import {
  ObserverOrderInvalidStep02DatumSchema,
  ObserverOrderInvalidStep03DatumSchema,
  ObserverOrderInvalidStep04DatumSchema,
} from "./schemas.js";
import { hashObserverOrderWalkCheckpoint } from "./staged-plan.js";

const prepareObserverOrderWorkflowArtifact = async (
  evidence: CanonicalBlockEvidence,
): Promise<ObserverOrderInvalidArtifact> => {
  const transactions = blockTransactionsFromCanonicalEvidence(evidence).map(
    (tx, index) => ({
      index,
      nodeTxId: tx.nodeTxId,
      l2TransactionSourceCbor: tx.l2TransactionSourceCbor,
      fullTransactionCbor: tx.txCbor,
      material: deriveMidgardNativeTxFaultEvidenceMaterial(
        Buffer.from(tx.txCbor, "hex"),
      ),
    }),
  );
  const trie = await buildTrieView(
    transactions.map((tx) => ({
      key: Buffer.from(tx.nodeTxId, "hex"),
      value: Buffer.from(tx.l2TransactionSourceCbor, "hex"),
    })),
  );
  await requireTransactionsRootMatch({
    sourceRoot: trie.root,
    expectedTransactionsRoot: evidence.header.transactionsRoot,
    count: evidence.header.l2TransactionCount,
  });
  const raw = {
    schemaVersion: OBSERVER_ORDER_INVALID_RAW_EVIDENCE,
    headerHash: evidence.headerHash,
    committedTransactionsRoot: evidence.header.transactionsRoot,
    l2TransactionCount: evidence.header.l2TransactionCount,
    transactionsPhasRoot: trie.root,
    payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    payloadSha256: evidence.payloadSha256,
    transactions,
  };
  return detectObserverOrderInvalidAcceptedRawReplay(raw).length > 0
    ? await prepareObserverOrderInvalidAcceptedArtifact(raw)
    : await prepareObserverOrderInvalidForcedArtifact(evidence);
};

export const OBSERVER_ORDER_INVALID_WORKFLOW =
  "midgard-observer-order-invalid-production-workflow-v1" as const;

export type ObserverOrderInvalidRemovalReferenceScripts = Readonly<{
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

export type ManifestBoundObserverOrderInvalidWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ObserverOrderInvalidWorkflowReferenceScripts &
    Readonly<{ removal: ObserverOrderInvalidRemovalReferenceScripts }>;
}>;

export type ManifestBoundObserverOrderInvalidWorkflow =
  ManifestBoundFamilyWorkflow<"observerOrderInvalid", true, 4> &
    WorkflowExtension &
    Readonly<{ decisionDigest: string }>;

const CATEGORY = "observerOrderInvalid" as const;

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;
type BoundContext = FamilyAssemblyContext<
  "observerOrderInvalid",
  (typeof WITNESS_ROLES)[number],
  true,
  4
>;
type WorkflowExtension = Readonly<{
  actuator: ReturnType<typeof createObserverOrderInvalidActuator>;
  prerequisite: FieldCarriagePrerequisitePort<"observerOrderInvalid">;
  lucid: LucidEvolution;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
const bindFamily = (context: BoundContext) => {
  const { binding, references } = context;
  const { steps } = references;
  const chain = binding.resolvedContracts.contracts[CATEGORY];
  const certificate = binding.fieldPreimageCertificate;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 4 ||
    certificate === null ||
    stateQueuePolicyId === undefined
  )
    throw new Error("observerOrderInvalid manifest omitted required contracts");
  const contracts: ObserverOrderInvalidContracts = Object.freeze({
    steps: chain.steps.map((step, index) => ({
      blueprintTitle: OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as ObserverOrderInvalidContracts["steps"],
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
  const actuator = createObserverOrderInvalidActuator({
    binding,
    lucid: context.lucid,
    signer: context.signer,
    contracts,
    references,
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  });
  return actuator;
};
const bound = new WeakMap<BoundContext, ReturnType<typeof bindFamily>>();
const boundFor = (context: BoundContext) => {
  const existing = bound.get(context);
  if (existing !== undefined) return existing;
  const created = bindFamily(context);
  bound.set(context, created);
  return created;
};
const createTransactionPort = (
  context: BoundContext,
): CursorFamilyTransactionPort<"observerOrderInvalid"> => {
  const actuator = boundFor(context);
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: CATEGORY,
    validatePreparedArtifact: async ({ evidence, artifact }) => {
      if (
        journalJsonDigest(
          await prepareObserverOrderWorkflowArtifact(evidence),
        ) !== journalJsonDigest(artifact)
      )
        throw new Error(
          "prepared family artifact differs from retained evidence",
        );
    },
    prepare: async ({ evidence }) =>
      await prepareObserverOrderWorkflowArtifact(evidence),
    capture: async ({ action, artifact }) =>
      await actuator.capture({
        action: action.input as unknown as ObserverOrderInvalidActuatorAction,
        artifact,
      }),
  };
};
export const OBSERVER_ORDER_INVALID_FAMILY_DEFINITION = defineFamily<
  "observerOrderInvalid",
  (typeof WITNESS_ROLES)[number],
  true,
  4
>({
  category: CATEGORY,
  stepDatumSchemas: [
    ObserverOrderInvalidStep02DatumSchema,
    ObserverOrderInvalidStep02DatumSchema,
    ObserverOrderInvalidStep03DatumSchema,
    ObserverOrderInvalidStep04DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => OBSERVER_ORDER_INVALID_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: {
      category: CATEGORY,
      stepCount: 4,
      successors: { 1: [2], 2: [3, 4], 3: [3, 4], 4: ["proof_token"] },
    },
    stepContractNames: [
      "fraudProofObserverOrderInvalid",
      "fraudProofObserverOrderInvalidStep02",
      "fraudProofObserverOrderInvalidStep03",
      "fraudProofObserverOrderInvalidStep04",
    ],
    createRefineAction:
      (context) =>
      async ({ observed, artifact }) => {
        const selected = await currentAction({
          workflow: { lucid: context.lucid },
          artifact: admitObserverOrderInvalidArtifact(artifact).artifact,
          stage: observed.stage,
        });
        if (selected === "removed")
          throw new Error("observerOrderInvalid action became terminal");
        return Object.fromEntries(
          Object.entries(selected).filter(
            ([key]) => key === "action" || key === "walkOrdinal",
          ),
        );
      },
    transactionPort: createTransactionPort,
  },
  fieldCarriage: [
    {
      requirementForAction: (context, { action, artifact }) => {
        const { certificate, references } = context;
        return observerOrderInvalidFieldRequirement({
          action: action.input as unknown as ObserverOrderInvalidActuatorAction,
          artifact,
          owner: context.signer.paymentKeyHash,
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: references.fieldPreimageCertificateMint,
          },
        });
      },
    },
  ],
  extend: (context): WorkflowExtension => ({
    actuator: boundFor(context),
    prerequisite: context.fieldCarriagePrerequisites[0]!,
    lucid: context.lucid,
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  }),
});

export const createManifestBoundObserverOrderInvalidWorkflow = async (
  config: ManifestBoundObserverOrderInvalidWorkflowConfig,
): Promise<ManifestBoundObserverOrderInvalidWorkflow> => {
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("observerOrderInvalid decision digest is malformed");
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    OBSERVER_ORDER_INVALID_FAMILY_DEFINITION,
    assemblyConfig,
  );
  return Object.freeze({
    ...(workflow as typeof workflow & WorkflowExtension),
    decisionDigest,
  });
};

const currentAction = async ({
  workflow,
  artifact,
  stage,
}: {
  readonly workflow: Pick<ManifestBoundObserverOrderInvalidWorkflow, "lucid">;
  readonly artifact: ObserverOrderInvalidArtifact;
  readonly stage: FraudProofRawL1FamilyStage;
}): Promise<ObserverOrderInvalidActuatorAction | "removed"> => {
  const admitted = admitObserverOrderInvalidArtifact(artifact);
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
    return {
      stage: "step_02",
      threadOutRef: stage.threadOutRef,
      action: { kind: "authenticate" },
    };
  }
  if (stage.step === 3) {
    const [txHash, outputIndex] = stage.threadOutRef.split("#");
    const [utxo] = await workflow.lucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    if (utxo?.datum == null)
      throw new Error("observerOrderInvalid step-03 datum disappeared");
    const state = Data.from(
      utxo.datum,
      ObserverOrderInvalidStep03DatumSchema as never,
    ) as { data: { checkpoint_hash: string } };
    const hash = state.data.checkpoint_hash;
    const ordinal = [
      admitted.staged.initialWalk,
      ...admitted.staged.walk,
    ].findIndex(
      (checkpoint) => hashObserverOrderWalkCheckpoint(checkpoint) === hash,
    );
    if (ordinal < 0 || ordinal >= admitted.staged.walk.length)
      throw new Error("observerOrderInvalid scan checkpoint substitution");
    return {
      stage: "step_03",
      threadOutRef: stage.threadOutRef,
      walkOrdinal: ordinal,
    };
  }
  if (stage.step === 4)
    return { stage: "step_04", threadOutRef: stage.threadOutRef };
  throw new Error("observerOrderInvalid observed an impossible step");
};

export type ObserverOrderInvalidWorkflowRunResult = FraudProofWorkflowRunResult;

export const executeManifestBoundObserverOrderInvalidWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundObserverOrderInvalidWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  if (
    workflowActuationAuthorizingDecisionDigest(journal) !==
    workflow.decisionDigest
  )
    throw new Error("observerOrderInvalid journal changed decision digest");
  if (workflowJournalIsReconciliationOnly(journal))
    return await resumeRecordedFraudProofWorkflow({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      category: CATEGORY,
      headerHash: workflow.binding.definition.headerHash,
      journal,
      adapter: workflow.adapter,
      terminalVerifier: workflow.terminalVerifier,
      releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    });
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: OBSERVER_ORDER_INVALID_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: [CATEGORY],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export type LoadedObserverOrderInvalidWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundObserverOrderInvalidWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadObserverOrderInvalidWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedObserverOrderInvalidWorkflow>;

/** Standard runtime-loader-compatible package runner; core config has no callbacks. */
export const createObserverOrderInvalidWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadObserverOrderInvalidWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== (CATEGORY as string))
        throw new Error("observerOrderInvalid runner category changed");
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: CATEGORY,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
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
            "observerOrderInvalid runtime requires concrete public retained DA",
          );
        const workflow = await createManifestBoundObserverOrderInvalidWorkflow(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "observerOrderInvalid runtime binding changed invocation",
          );
        return (await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            executeManifestBoundObserverOrderInvalidWorkflow({
              workflow,
              sources: loaded.retainedDaSources,
              journal,
            }),
        })) as never;
      } finally {
        await loaded.close();
      }
    },
  });
