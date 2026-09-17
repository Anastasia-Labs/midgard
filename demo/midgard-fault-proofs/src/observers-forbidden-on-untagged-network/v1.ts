import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

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
import { OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
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
  createObserversForbiddenActuator,
  type ObserversForbiddenActuatorAction,
  observersForbiddenFieldRequirement,
  type ObserversForbiddenWorkflowReferenceScripts,
} from "./actuator.js";
import {
  admitObserversForbiddenArtifact,
  type ObserversForbiddenArtifact,
} from "./artifact.js";
import {
  OBSERVERS_FORBIDDEN_BLUEPRINT_TITLES,
  type ObserversForbiddenContracts,
} from "./contracts.js";
import { OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY } from "./family.js";
import {
  prepareObserversForbiddenAcceptedArtifact,
  prepareObserversForbiddenForcedArtifact,
} from "./replay.js";
import { ObserversForbiddenStep02DatumSchema } from "./schemas.js";

const CATEGORY = OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY;
export const OBSERVERS_FORBIDDEN_WORKFLOW =
  "midgard-observers-forbidden-on-untagged-network-production-workflow-v1" as const;

export type ObserversForbiddenRemovalReferenceScripts = Readonly<{
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

export type ManifestBoundObserversForbiddenWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ObserversForbiddenWorkflowReferenceScripts &
    Readonly<{ removal: ObserversForbiddenRemovalReferenceScripts }>;
}>;

export type ManifestBoundObserversForbiddenWorkflow =
  ManifestBoundFamilyWorkflow<"observersForbiddenOnUntaggedNetwork", true, 2> &
    WorkflowExtension &
    Readonly<{ decisionDigest: string }>;

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;
type BoundContext = FamilyAssemblyContext<
  "observersForbiddenOnUntaggedNetwork",
  (typeof WITNESS_ROLES)[number],
  true,
  2
>;
type WorkflowExtension = Readonly<{
  actuator: ReturnType<typeof createObserversForbiddenActuator>;
  prerequisite: FieldCarriagePrerequisitePort<"observersForbiddenOnUntaggedNetwork">;
  lucid: LucidEvolution;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
const bindFamily = (context: BoundContext) => {
  const { binding, references } = context;
  const { steps } = references;
  const resolved = binding.resolvedContracts as unknown as {
    category: { categoryId: string };
    hubOraclePolicyId: string;
    stateQueuePolicyId?: string;
    contracts: Record<string, unknown> & {
      computationThread: ObserversForbiddenContracts["computationThread"];
      fraudProof: ObserversForbiddenContracts["fraudProof"] & {
        spendingScriptHash: string;
      };
      observersForbiddenOnUntaggedNetwork?: {
        steps: ObserversForbiddenContracts["steps"];
      };
    };
  };
  const chain = resolved.contracts.observersForbiddenOnUntaggedNetwork;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    chain.steps.length !== 2 ||
    certificate === null ||
    resolved.stateQueuePolicyId === undefined
  )
    throw new Error("observersForbidden manifest omitted required contracts");
  const contracts: ObserversForbiddenContracts = Object.freeze({
    steps: chain.steps.map((step, index) => ({
      blueprintTitle: OBSERVERS_FORBIDDEN_BLUEPRINT_TITLES[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as ObserversForbiddenContracts["steps"],
    computationThread: resolved.contracts.computationThread,
    fraudProof: resolved.contracts.fraudProof,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
    fieldPreimageCertificateMintingScript: certificate.mintingScript,
  });
  const actuator = createObserversForbiddenActuator({
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
): CursorFamilyTransactionPort<"observersForbiddenOnUntaggedNetwork"> => {
  const actuator = boundFor(context);
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: CATEGORY,
    prepareRaw: async (routed) => {
      if (routed.kind !== "observers_forbidden_on_untagged_network")
        throw new Error("raw family route changed");
      return await prepareObserversForbiddenAcceptedArtifact(routed.evidence);
    },
    validatePreparedRawArtifact: async ({ routed, artifact }) => {
      if (routed.kind !== "observers_forbidden_on_untagged_network")
        throw new Error("raw family route changed");
      if (
        journalJsonDigest(
          await prepareObserversForbiddenAcceptedArtifact(routed.evidence),
        ) !== journalJsonDigest(artifact)
      )
        throw new Error("prepared raw artifact differs from retained evidence");
    },
    validatePreparedArtifact: async ({ evidence, artifact }) => {
      if (
        journalJsonDigest(
          await prepareObserversForbiddenForcedArtifact(evidence),
        ) !== journalJsonDigest(artifact)
      )
        throw new Error(
          "prepared family artifact differs from retained evidence",
        );
    },
    prepare: async ({ evidence }) =>
      await prepareObserversForbiddenForcedArtifact(evidence),
    capture: async ({ action, artifact }) =>
      await actuator.capture({
        action: action.input as unknown as ObserversForbiddenActuatorAction,
        artifact,
      }),
  };
};
export const OBSERVERS_FORBIDDEN_FAMILY_DEFINITION = defineFamily<
  "observersForbiddenOnUntaggedNetwork",
  (typeof WITNESS_ROLES)[number],
  true,
  2
>({
  category: CATEGORY,
  stepDatumSchemas: [
    ObserversForbiddenStep02DatumSchema,
    ObserversForbiddenStep02DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () =>
    OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: {
      category: CATEGORY,
      stepCount: 2,
      successors: { 1: [2], 2: ["proof_token"] },
    },
    stepContractNames: [
      "fraudProofObserversForbiddenOnUntaggedNetwork",
      "fraudProofObserversForbiddenOnUntaggedNetworkStep02",
    ],
    createRefineAction:
      (context) =>
      async ({ observed, artifact }) => {
        const selected = await currentAction({
          workflow: { lucid: context.lucid },
          artifact: admitObserversForbiddenArtifact(artifact).artifact,
          stage: observed.stage,
        });
        if (selected === "removed")
          throw new Error("observersForbidden action became terminal");
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
        return observersForbiddenFieldRequirement({
          action: action.input as unknown as ObserversForbiddenActuatorAction,
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

/** Config contains infrastructure and authenticated references only. */
export const createManifestBoundObserversForbiddenWorkflow = async (
  config: ManifestBoundObserversForbiddenWorkflowConfig,
): Promise<ManifestBoundObserversForbiddenWorkflow> => {
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("observersForbidden decision digest is malformed");
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    OBSERVERS_FORBIDDEN_FAMILY_DEFINITION,
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
  readonly workflow: Pick<ManifestBoundObserversForbiddenWorkflow, "lucid">;
  readonly artifact: ObserversForbiddenArtifact;
  readonly stage: FraudProofRawL1FamilyStage;
}): Promise<ObserversForbiddenActuatorAction | "removed"> => {
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
      throw new Error("observersForbidden step-02 datum disappeared");
    const state = Data.from(
      utxo.datum,
      ObserversForbiddenStep02DatumSchema as never,
    ) as { data: { subject: { transaction_id: string }; network_id: bigint } };
    if (
      state.data.subject.transaction_id !== artifact.transactionId ||
      state.data.network_id !== BigInt(artifact.networkId)
    )
      throw new Error("observersForbidden restart datum substitution");
    return { stage: "step_02", threadOutRef: stage.threadOutRef };
  }
  throw new Error("observersForbidden observed an impossible step");
};

export type ObserversForbiddenWorkflowRunResult = FraudProofWorkflowRunResult;

export const executeManifestBoundObserversForbiddenWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundObserversForbiddenWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  if (
    workflowActuationAuthorizingDecisionDigest(journal) !==
    workflow.decisionDigest
  )
    throw new Error("observersForbidden journal changed decision digest");
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
    replayer: OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: [CATEGORY],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export type LoadedObserversForbiddenWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundObserversForbiddenWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadObserversForbiddenWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedObserversForbiddenWorkflow>;

/** Standard runtime loader surface; no evidence, stage, submit, or journal callbacks. */
export const createObserversForbiddenWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadObserversForbiddenWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== CATEGORY)
        throw new Error("observersForbidden runner category changed");
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: CATEGORY as never,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: CATEGORY as never,
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
            "observersForbidden runtime requires concrete public retained DA",
          );
        const workflow = await createManifestBoundObserversForbiddenWorkflow(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "observersForbidden runtime binding changed invocation",
          );
        return (await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            executeManifestBoundObserversForbiddenWorkflow({
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
