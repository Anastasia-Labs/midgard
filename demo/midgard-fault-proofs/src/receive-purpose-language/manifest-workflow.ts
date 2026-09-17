import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
} from "../workflow/actuation-permit.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/adapters.js";
import { RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
  type ManifestBoundFamilyWorkflow,
  type ManifestBoundFamilyWorkflowConfig,
} from "../workflow/family-definition.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createCanonicalFamilyArtifactPort,
  executeManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import { type FraudProofWorkflowRunResult } from "../workflow/orchestrator.js";
import { continuePendingWorkflow } from "../workflow/pending-continuation.js";
import {
  createReceivePurposeLanguageActuator,
  type ReceivePurposeLanguageWorkflowReferences,
} from "./actuator.js";
import { prepareReceivePurposeLanguageArtifact } from "./authenticated-replay.js";
import {
  RECEIVE_PURPOSE_LANGUAGE_BLUEPRINT_TITLES,
  type ReceivePurposeLanguageContracts,
} from "./contracts.js";
import {
  ReceivePurposeStep02DatumSchema,
  ReceivePurposeStep03DatumSchema,
} from "./schemas.js";

export const RECEIVE_PURPOSE_LANGUAGE_WORKFLOW =
  "midgard-receive-purpose-language-production-workflow-v1" as const;
export const RECEIVE_PURPOSE_LANGUAGE_CONFIG_KEYS = Object.freeze([
  "manifest",
  "blueprintJson",
  "deploymentInfo",
  "headerHash",
  "lucid",
  "signer",
  "source",
  "decisionDigest",
  "stateQueueMutationLeaseCoordinator",
  "referenceScripts",
] as const);
export const RECEIVE_PURPOSE_LANGUAGE_STEP_DATUM_SCHEMAS = Object.freeze([
  FraudProofComputationThreadStepDatum,
  ReceivePurposeStep02DatumSchema,
  ReceivePurposeStep03DatumSchema,
] as const);
export const RECEIVE_PURPOSE_LANGUAGE_CURSOR_SPEC = Object.freeze({
  category: "receivePurposeLanguage",
  stepCount: 3,
  successors: Object.freeze({
    1: Object.freeze([2] as const),
    2: Object.freeze([3] as const),
    3: Object.freeze(["proof_token"] as const),
  }),
}) satisfies CursorFamilySpec<"receivePurposeLanguage">;

const STEP_CONTRACT_NAMES = [
  "fraudProofReceivePurposeLanguage",
  "fraudProofReceivePurposeLanguageStep02",
  "fraudProofReceivePurposeLanguageStep03",
] as const;
const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;

type BoundContext = FamilyAssemblyContext<
  "receivePurposeLanguage",
  (typeof WITNESS_ROLES)[number],
  false,
  3
>;

export type ReceivePurposeLanguageRemovalReferences = Readonly<{
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
export type ManifestBoundReceivePurposeLanguageWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ReceivePurposeLanguageWorkflowReferences &
    Readonly<{ removal: ReceivePurposeLanguageRemovalReferences }>;
}>;
/** The members `extend` adds to the assembled workflow. */
type ReceivePurposeLanguageWorkflowExtension = Readonly<{
  actuator: ReturnType<typeof createReceivePurposeLanguageActuator>;
  lucid: LucidEvolution;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
export type ManifestBoundReceivePurposeLanguageWorkflow =
  ManifestBoundFamilyWorkflow<"receivePurposeLanguage", false, 3> &
    ReceivePurposeLanguageWorkflowExtension &
    Readonly<{ decisionDigest: string }>;

/**
 * The family's contracts and actuator over one assembly context. Built once
 * per context: the transaction port captures through the actuator, and
 * `extend` exposes that same actuator on the workflow.
 */
const bound = new WeakMap<
  BoundContext,
  ReturnType<typeof bindReceivePurposeLanguage>
>();
const bindReceivePurposeLanguage = (context: BoundContext) => {
  const { binding, references } = context;
  const chain = binding.resolvedContracts.contracts.receivePurposeLanguage;
  const { hubOraclePolicyId, stateQueuePolicyId } = binding.resolvedContracts;
  if (
    chain === undefined ||
    chain.steps.length !== 3 ||
    stateQueuePolicyId === undefined
  )
    throw new Error("receivePurposeLanguage manifest omitted three-step chain");
  const contracts: ReceivePurposeLanguageContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: RECEIVE_PURPOSE_LANGUAGE_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${references.steps[index]!.txHash}#${references.steps[index]!.outputIndex.toString()}`,
    })) as unknown as ReceivePurposeLanguageContracts["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId,
    stateQueuePolicyId,
  };
  return {
    actuator: createReceivePurposeLanguageActuator({
      binding,
      lucid: context.lucid,
      signer: context.signer,
      contracts,
      references,
      stateQueueMutationLeaseCoordinator:
        context.stateQueueMutationLeaseCoordinator,
    }),
    artifacts: createCanonicalFamilyArtifactPort(({ evidence }) =>
      prepareReceivePurposeLanguageArtifact(evidence),
    ),
  };
};
const boundFor = (context: BoundContext) => {
  const existing = bound.get(context);
  if (existing !== undefined) return existing;
  const created = bindReceivePurposeLanguage(context);
  bound.set(context, created);
  return created;
};

const createTransactionPort = (
  context: BoundContext,
): CursorFamilyTransactionPort<"receivePurposeLanguage"> => {
  const { actuator, artifacts } = boundFor(context);
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: "receivePurposeLanguage",
    prepare: artifacts.prepare,
    validatePreparedArtifact: artifacts.validatePreparedArtifact,
    capture: async ({ action, artifact }) => {
      const input = cursorFamilyActionInput({
        category: "receivePurposeLanguage",
        action,
      });
      const restored = artifacts.require(artifact);
      if (input.stage === "init")
        return actuator.capture({
          artifact: restored,
          action: {
            stage: "init",
            stateQueueBlockOutRef: cursorStringField(
              input,
              "stateQueueBlockOutRef",
            ),
          },
        });
      if (input.stage === "remove")
        return actuator.capture({
          artifact: restored,
          action: {
            stage: "remove",
            nextRemovalOutRef: cursorStringField(input, "nextRemovalOutRef"),
            fraudProofOutRef: cursorStringField(input, "fraudProofOutRef"),
          },
        });
      const stages = ["step_01", "step_02", "step_03"] as const;
      const stage = stages[Number(input.ordinal) - 1];
      if (stage === undefined)
        throw new Error("receivePurposeLanguage cursor ordinal changed");
      const threadOutRef = cursorStringField(input, "threadOutRef");
      return actuator.capture({
        artifact: restored,
        action:
          stage === "step_01"
            ? {
                stage,
                threadOutRef,
                stateQueueBlockOutRef: cursorStringField(
                  input,
                  "stateQueueBlockOutRef",
                ),
              }
            : { stage, threadOutRef },
      });
    },
  };
};

export const RECEIVE_PURPOSE_LANGUAGE_FAMILY_DEFINITION = defineFamily({
  category: "receivePurposeLanguage",
  stepDatumSchemas: RECEIVE_PURPOSE_LANGUAGE_STEP_DATUM_SCHEMAS,
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  replayer: () => RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: RECEIVE_PURPOSE_LANGUAGE_CURSOR_SPEC,
    stepContractNames: STEP_CONTRACT_NAMES,
    transactionPort: createTransactionPort,
  },
  extend: (context): ReceivePurposeLanguageWorkflowExtension => ({
    actuator: boundFor(context).actuator,
    lucid: context.lucid,
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  }),
});

/**
 * Strict manifest/reference binding; its input admits no callback authority.
 * The decision digest is invocation-bound rather than assembly-bound, so the
 * constructor attaches it after the assembly.
 */
export const createManifestBoundReceivePurposeLanguageWorkflow = async (
  config: ManifestBoundReceivePurposeLanguageWorkflowConfig,
): Promise<ManifestBoundReceivePurposeLanguageWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...RECEIVE_PURPOSE_LANGUAGE_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "receivePurposeLanguage production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("receivePurposeLanguage decision digest is malformed");
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    RECEIVE_PURPOSE_LANGUAGE_FAMILY_DEFINITION,
    assemblyConfig satisfies ManifestBoundFamilyWorkflowConfig<
      "receivePurposeLanguage",
      (typeof WITNESS_ROLES)[number],
      false,
      3
    >,
  );
  return Object.freeze({
    ...(workflow as typeof workflow & ReceivePurposeLanguageWorkflowExtension),
    decisionDigest,
  });
};
export const executeManifestBoundReceivePurposeLanguageWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundReceivePurposeLanguageWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  return executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    replayer: RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  });
};

export const runOrResumeManifestBoundReceivePurposeLanguageWorkflow =
  async (input: {
    workflow: ManifestBoundReceivePurposeLanguageWorkflow;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStore;
  }) => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow")
      throw new Error(
        "receivePurposeLanguage runner rejects caller-authored evidence",
      );
    return await executeManifestBoundReceivePurposeLanguageWorkflow(input);
  };
export type LoadedReceivePurposeLanguageWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundReceivePurposeLanguageWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;
export type LoadReceivePurposeLanguageWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedReceivePurposeLanguageWorkflow>;
export const createReceivePurposeLanguageWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadReceivePurposeLanguageWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "receivePurposeLanguage")
        throw new Error("receivePurposeLanguage runner category changed");
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "receivePurposeLanguage" as never,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: "receivePurposeLanguage" as never,
        headerHash: invocation.headerHash,
        checkpoint: "runner_start",
      });
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      try {
        if (
          loaded.retainedDaSources.length === 0 ||
          loaded.retainedDaSources.some(
            (source) => !(source instanceof DaLibp2pRetainedDaSource),
          )
        )
          throw new Error(
            "receivePurposeLanguage requires concrete public retained DA",
          );
        const workflow =
          await createManifestBoundReceivePurposeLanguageWorkflow(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "receivePurposeLanguage runtime binding changed invocation",
          );
        return await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            runOrResumeManifestBoundReceivePurposeLanguageWorkflow({
              workflow,
              sources: loaded.retainedDaSources,
              journal,
            }),
        });
      } finally {
        await loaded.close();
      }
    },
  });
