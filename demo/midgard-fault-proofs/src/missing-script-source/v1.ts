import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  cursorFamilyActionInput,
  cursorStringField,
  STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
} from "../workflow/cursor-family-runtime.js";
import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
  type ManifestBoundFamilyWorkflow,
} from "../workflow/family-definition.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createCanonicalFamilyArtifactPort,
  executeManifestBoundFamilyRecovery,
} from "../workflow/manifest-bound-family-recovery.js";
import { type FraudProofWorkflowRunResult } from "../workflow/orchestrator.js";
import {
  createMissingScriptSourceActuator,
  type MissingScriptSourceArtifact,
  type MissingScriptSourceWorkflowReferences,
} from "./actuator.js";
import { prepareMissingScriptSourceArtifact } from "./authenticated-replay.js";
import {
  MISSING_SCRIPT_SOURCE_BLUEPRINT_TITLES,
  type MissingScriptSourceContracts,
} from "./contracts.js";
import {
  ExecutionSourceStep02DatumSchema,
  ExecutionSourceStep03DatumSchema,
  ExecutionSourceStep04DatumSchema,
  ExecutionSourceStep05DatumSchema,
  ExecutionSourceStep06DatumSchema,
} from "./schemas.js";

/** Persist the exact Plutus control bytes; capture retains the freshly authenticated Data object. */
export const missingScriptSourceRecoveryMaterial = (
  artifact: MissingScriptSourceArtifact,
) => ({
  ...artifact,
  authentication: {
    ...artifact.authentication,
    control_data: Data.to(artifact.authentication.control_data),
  },
});

export const MISSING_SCRIPT_SOURCE_WORKFLOW =
  "midgard-missing-script-source-production-workflow-v1" as const;
export const MISSING_SCRIPT_SOURCE_CONFIG_KEYS = Object.freeze([
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
export const MISSING_SCRIPT_SOURCE_STEP_DATUM_SCHEMAS = Object.freeze([
  FraudProofComputationThreadStepDatum,
  ExecutionSourceStep02DatumSchema,
  ExecutionSourceStep03DatumSchema,
  ExecutionSourceStep04DatumSchema,
  ExecutionSourceStep05DatumSchema,
  ExecutionSourceStep06DatumSchema,
] as const);

export const MISSING_SCRIPT_SOURCE_CURSOR_SPEC: CursorFamilySpec<"missingScriptSource"> &
  Readonly<{ stepCount: 6 }> = Object.freeze<
  CursorFamilySpec<"missingScriptSource"> & Readonly<{ stepCount: 6 }>
>({
  category: "missingScriptSource",
  stepCount: 6,
  successors: {
    1: [2],
    2: [3],
    3: [4],
    4: [5],
    5: [5, 6],
    6: ["proof_token"],
  },
});

export type MissingScriptSourceRemovalReferences = Readonly<{
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

export type ManifestBoundMissingScriptSourceWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: MissingScriptSourceWorkflowReferences &
    Readonly<{
      removal: MissingScriptSourceRemovalReferences;
    }>;
}>;

export type ManifestBoundMissingScriptSourceWorkflow =
  ManifestBoundFamilyWorkflow<"missingScriptSource", false, 6> &
    WorkflowExtension &
    Readonly<{
      decisionDigest: string;
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
    }>;

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;
const STEP_CONTRACT_NAMES = [
  "fraudProofMissingScriptSource",
  "fraudProofMissingScriptSourceStep02",
  "fraudProofMissingScriptSourceStep03",
  "fraudProofMissingScriptSourceStep04",
  "fraudProofMissingScriptSourceStep05",
  "fraudProofMissingScriptSourceStep06",
] as const;
type BoundContext = FamilyAssemblyContext<
  "missingScriptSource",
  (typeof WITNESS_ROLES)[number],
  false,
  6
>;
type WorkflowExtension = Readonly<{
  actuator: ReturnType<typeof createMissingScriptSourceActuator>;
  lucid: LucidEvolution;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
const bindFamily = (context: BoundContext) => {
  const {
    binding,
    references: { steps, witnesses },
  } = context;
  const chain = binding.resolvedContracts.contracts.missingScriptSource;
  const hubOraclePolicyId = binding.contractEntries.hubOracleMint?.scriptHash;
  if (
    chain === undefined ||
    chain.steps.length !== 6 ||
    hubOraclePolicyId === undefined
  )
    throw new Error("missingScriptSource manifest omitted six-step chain");
  const contracts: MissingScriptSourceContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: MISSING_SCRIPT_SOURCE_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as MissingScriptSourceContracts["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId,
  };
  const actuator = createMissingScriptSourceActuator({
    binding,
    lucid: context.lucid,
    signer: context.signer,
    contracts,
    references: { steps, witnesses },
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  });
  const artifacts = createCanonicalFamilyArtifactPort(
    ({ evidence }) => prepareMissingScriptSourceArtifact(evidence),
    missingScriptSourceRecoveryMaterial,
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
): CursorFamilyTransactionPort<"missingScriptSource"> => {
  const { actuator, artifacts } = boundFor(context);
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: "missingScriptSource",
    prepare: artifacts.prepare,
    validatePreparedArtifact: artifacts.validatePreparedArtifact,
    capture: async ({ action, artifact }) => {
      const input = cursorFamilyActionInput({
        category: "missingScriptSource",
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
        "scan",
        "prove",
        "finalize",
      ] as const;
      const stage = stages[Number(input.ordinal) - 1];
      if (stage === undefined)
        throw new Error("missingScriptSource cursor ordinal changed");
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
export const MISSING_SCRIPT_SOURCE_FAMILY_DEFINITION = defineFamily<
  "missingScriptSource",
  (typeof WITNESS_ROLES)[number],
  false,
  6
>({
  category: "missingScriptSource",
  stepDatumSchemas: MISSING_SCRIPT_SOURCE_STEP_DATUM_SCHEMAS,
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  auxiliaryReferenceScripts: STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
  replayer: () => MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: MISSING_SCRIPT_SOURCE_CURSOR_SPEC,
    stepContractNames: STEP_CONTRACT_NAMES,
    transactionPort: createTransactionPort,
  },
  extend: (context): WorkflowExtension => ({
    actuator: boundFor(context).actuator,
    lucid: context.lucid,
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  }),
});
/**
 * Manifest-bound family construction. Config contains infrastructure and
 * authenticated references only: no evidence, stage, submit, or journal
 * callbacks are accepted.
 */
export const createManifestBoundMissingScriptSourceWorkflow = async (
  config: ManifestBoundMissingScriptSourceWorkflowConfig,
): Promise<ManifestBoundMissingScriptSourceWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...MISSING_SCRIPT_SOURCE_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "missingScriptSource production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("missingScriptSource decision digest is malformed");
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    MISSING_SCRIPT_SOURCE_FAMILY_DEFINITION,
    {
      ...assemblyConfig,
      auxiliaryReferenceScripts: config.referenceScripts.removal,
    },
  );
  return Object.freeze({
    ...(workflow as typeof workflow & WorkflowExtension),
    source: config.source,
    decisionDigest,
  });
};

export const executeManifestBoundMissingScriptSourceWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundMissingScriptSourceWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  return executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    replayer: MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  });
};
