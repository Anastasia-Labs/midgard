import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { bindWorkflowActuationJournal } from "../workflow/actuation-permit.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/adapters.js";
import { UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
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
import { createUnusedScriptWitnessActuator } from "./actuator.js";
import {
  UNUSED_SCRIPT_WITNESS_BLUEPRINT_TITLES,
  type UnusedScriptWitnessContracts,
} from "./contracts.js";
import { prepareUnusedScriptWitnessArtifact } from "./replay.js";
import {
  UnusedScriptStep02DatumSchema,
  UnusedScriptStep03DatumSchema,
  UnusedScriptStep04DatumSchema,
  UnusedScriptStep05DatumSchema,
  UnusedScriptStep06DatumSchema,
} from "./schemas.js";

export const UNUSED_SCRIPT_WITNESS_CONFIG_KEYS = Object.freeze([
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

export const UNUSED_SCRIPT_WITNESS_STEP_DATUM_SCHEMAS = Object.freeze([
  FraudProofComputationThreadStepDatum,
  UnusedScriptStep02DatumSchema,
  UnusedScriptStep03DatumSchema,
  UnusedScriptStep04DatumSchema,
  UnusedScriptStep05DatumSchema,
  UnusedScriptStep06DatumSchema,
] as const);

export const UNUSED_SCRIPT_WITNESS_CURSOR_SPEC: CursorFamilySpec<"unusedScriptWitness"> &
  Readonly<{ stepCount: 6 }> = Object.freeze<
  CursorFamilySpec<"unusedScriptWitness"> & Readonly<{ stepCount: 6 }>
>({
  category: "unusedScriptWitness",
  stepCount: 6,
  successors: {
    1: [2],
    2: [3],
    3: [4],
    4: [4, 5],
    5: [5, 6],
    6: ["proof_token"],
  },
});

export type UnusedScriptWitnessRemovalReferenceScripts = Readonly<{
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

export type UnusedScriptWitnessWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  removal: UnusedScriptWitnessRemovalReferenceScripts;
}>;

export type ManifestBoundUnusedScriptWitnessWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: UnusedScriptWitnessWorkflowReferenceScripts;
}>;

export type ManifestBoundUnusedScriptWitnessWorkflow =
  ManifestBoundFamilyWorkflow<"unusedScriptWitness", false, 6> &
    WorkflowExtension &
    Readonly<{ decisionDigest: string }>;

const contracts = Object.freeze({
  steps: [
    "fraudProofUnusedScriptWitness",
    "fraudProofUnusedScriptWitnessStep02",
    "fraudProofUnusedScriptWitnessStep03",
    "fraudProofUnusedScriptWitnessStep04",
    "fraudProofUnusedScriptWitnessStep05",
    "fraudProofUnusedScriptWitnessStep06",
  ],
  witnesses: {
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
    pexcludesWithdraw: "pexcludesWithdraw",
  },
  removal: {
    correctionLockSpend: "correctionLockSpend",
    stateQueueSpend: "stateQueueSpend",
    stateQueueMint: "stateQueueMint",
    stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
    activeOperatorsSpend: "activeOperatorsSpend",
    activeOperatorsMint: "activeOperatorsMint",
    retiredOperatorsSpend: "retiredOperatorsSpend",
    retiredOperatorsMint: "retiredOperatorsMint",
    schedulerSpend: "schedulerSpend",
  },
} as const);

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;

type BoundContext = FamilyAssemblyContext<
  "unusedScriptWitness",
  (typeof WITNESS_ROLES)[number],
  false,
  6
>;
type WorkflowExtension = Readonly<{
  actuator: ReturnType<typeof createUnusedScriptWitnessActuator>;
  lucid: LucidEvolution;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
const bindFamily = (context: BoundContext) => {
  const { binding } = context;
  const chain = binding.resolvedContracts.contracts.unusedScriptWitness;
  if (chain === undefined || chain.steps.length !== 6)
    throw new Error("unusedScriptWitness manifest omitted six-step chain");
  const { steps, witnesses } = context.references;
  const hubOraclePolicyId = binding.contractEntries.hubOracleMint?.scriptHash;
  if (hubOraclePolicyId === undefined)
    throw new Error("unusedScriptWitness manifest omitted hub oracle");
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (stateQueuePolicyId === undefined)
    throw new Error("unusedScriptWitness manifest omitted state queue policy");
  const familyContracts: UnusedScriptWitnessContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: UNUSED_SCRIPT_WITNESS_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as UnusedScriptWitnessContracts["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId,
    stateQueuePolicyId,
  };
  const actuator = createUnusedScriptWitnessActuator({
    binding,
    lucid: context.lucid,
    signer: context.signer,
    contracts: familyContracts,
    references: { steps, witnesses },
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  });
  const artifacts = createCanonicalFamilyArtifactPort(({ evidence }) =>
    prepareUnusedScriptWitnessArtifact(evidence),
  );
  return { actuator, artifacts };
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
): CursorFamilyTransactionPort<"unusedScriptWitness"> => {
  const { actuator, artifacts } = boundFor(context);
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: "unusedScriptWitness",
    prepare: artifacts.prepare,
    validatePreparedArtifact: artifacts.validatePreparedArtifact,
    capture: async ({ action, artifact }) => {
      const input = cursorFamilyActionInput({
        category: "unusedScriptWitness",
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
      const stages = [
        "step_01",
        "step_02",
        "step_03",
        "step_04",
        "step_05",
        "step_06",
      ] as const;
      const stage = stages[Number(input.ordinal) - 1];
      if (stage === undefined)
        throw new Error("unusedScriptWitness cursor ordinal changed");
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

export const UNUSED_SCRIPT_WITNESS_FAMILY_DEFINITION = defineFamily<
  "unusedScriptWitness",
  (typeof WITNESS_ROLES)[number],
  false,
  6
>({
  category: "unusedScriptWitness",
  stepDatumSchemas: UNUSED_SCRIPT_WITNESS_STEP_DATUM_SCHEMAS,
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  auxiliaryReferenceScripts: contracts.removal,
  replayer: () => UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: UNUSED_SCRIPT_WITNESS_CURSOR_SPEC,
    stepContractNames: contracts.steps,
    transactionPort: createTransactionPort,
  },
  extend: (context): WorkflowExtension => ({
    actuator: boundFor(context).actuator,
    lucid: context.lucid,
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  }),
});

/** Strict infrastructure-only manifest/reference binding. */
export const createManifestBoundUnusedScriptWitnessWorkflow = async (
  config: ManifestBoundUnusedScriptWitnessWorkflowConfig,
): Promise<ManifestBoundUnusedScriptWitnessWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...UNUSED_SCRIPT_WITNESS_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "unusedScriptWitness production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("unusedScriptWitness decision digest is malformed");
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    UNUSED_SCRIPT_WITNESS_FAMILY_DEFINITION,
    {
      ...assemblyConfig,
      auxiliaryReferenceScripts: config.referenceScripts.removal,
    },
  );
  return Object.freeze({
    ...(workflow as typeof workflow & WorkflowExtension),
    decisionDigest,
  });
};

export type LoadedUnusedScriptWitnessWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundUnusedScriptWitnessWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadUnusedScriptWitnessWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedUnusedScriptWitnessWorkflow>;

export const executeManifestBoundUnusedScriptWitnessWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundUnusedScriptWitnessWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  return executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    replayer: UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
  });
};

export const createUnusedScriptWitnessWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadUnusedScriptWitnessWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "unusedScriptWitness")
        throw new Error("unusedScriptWitness runner category changed");
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
            "unusedScriptWitness requires concrete public retained DA",
          );
        const workflow = await createManifestBoundUnusedScriptWitnessWorkflow(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "unusedScriptWitness runtime binding changed invocation",
          );
        const journal = bindWorkflowFundingReservationJournal({
          permit: invocation.fundingReservationPermit,
          journal: bindWorkflowActuationJournal({
            journal: new DirectoryFraudProofWorkflowJournalStore(
              invocation.journalDirectory,
            ),
            permit: invocation.actuationPermit,
            decisionDigest: invocation.decisionDigest,
            deploymentFingerprint: invocation.deploymentFingerprint,
            category: "unusedScriptWitness",
            headerHash: invocation.headerHash,
          }),
        });
        return await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            executeManifestBoundUnusedScriptWitnessWorkflow({
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
