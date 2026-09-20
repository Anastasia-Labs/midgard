import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
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
import { createUnusedRedeemerActuator } from "./actuator.js";
import {
  UNUSED_REDEEMER_BLUEPRINT_TITLES,
  type UnusedRedeemerContracts,
} from "./contracts.js";
import { prepareUnusedRedeemerArtifact } from "./replay.js";
import {
  UnusedRedeemerStep02aDatumSchema,
  UnusedRedeemerStep02bDatumSchema,
  UnusedRedeemerStep02cDatumSchema,
  UnusedRedeemerStep02DatumSchema,
  UnusedRedeemerStep03DatumSchema,
  UnusedRedeemerStep04DatumSchema,
  UnusedRedeemerStep05DatumSchema,
  UnusedRedeemerStep06DatumSchema,
} from "./schemas.js";

export const UNUSED_REDEEMER_CONFIG_KEYS = Object.freeze([
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

export const UNUSED_REDEEMER_STEP_DATUM_SCHEMAS = Object.freeze([
  FraudProofComputationThreadStepDatum,
  UnusedRedeemerStep02DatumSchema,
  UnusedRedeemerStep02aDatumSchema,
  UnusedRedeemerStep02bDatumSchema,
  UnusedRedeemerStep02cDatumSchema,
  UnusedRedeemerStep03DatumSchema,
  UnusedRedeemerStep04DatumSchema,
  UnusedRedeemerStep05DatumSchema,
  UnusedRedeemerStep06DatumSchema,
] as const);

export const UNUSED_REDEEMER_CURSOR_SPEC: CursorFamilySpec<"unusedRedeemer"> &
  Readonly<{ stepCount: 9 }> = Object.freeze<
  CursorFamilySpec<"unusedRedeemer"> & Readonly<{ stepCount: 9 }>
>({
  category: "unusedRedeemer",
  stepCount: 9,
  successors: {
    1: [2],
    2: [3],
    3: [4],
    4: [5],
    5: [6],
    6: [7],
    7: [8],
    8: [8, 9],
    9: ["proof_token"],
  },
});

export type UnusedRedeemerRemovalReferenceScripts = Readonly<{
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

export type UnusedRedeemerWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  removal: UnusedRedeemerRemovalReferenceScripts;
}>;

export type ManifestBoundUnusedRedeemerWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: UnusedRedeemerWorkflowReferenceScripts;
}>;

export type ManifestBoundUnusedRedeemerWorkflow = ManifestBoundFamilyWorkflow<
  "unusedRedeemer",
  false,
  9
> &
  WorkflowExtension &
  Readonly<{ decisionDigest: string }>;

const contracts = Object.freeze({
  steps: [
    "fraudProofUnusedRedeemer",
    "fraudProofUnusedRedeemerStep02",
    "fraudProofUnusedRedeemerStep02a",
    "fraudProofUnusedRedeemerStep02b",
    "fraudProofUnusedRedeemerStep02c",
    "fraudProofUnusedRedeemerStep03",
    "fraudProofUnusedRedeemerStep04",
    "fraudProofUnusedRedeemerStep05",
    "fraudProofUnusedRedeemerStep06",
  ],
  witnesses: {
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
    pexcludesWithdraw: "pexcludesWithdraw",
  },
  removal: STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
} as const);

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;

type BoundContext = FamilyAssemblyContext<
  "unusedRedeemer",
  (typeof WITNESS_ROLES)[number],
  false,
  9
>;
type WorkflowExtension = Readonly<{
  actuator: ReturnType<typeof createUnusedRedeemerActuator>;
  lucid: LucidEvolution;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
const bindFamily = (context: BoundContext) => {
  const { binding } = context;
  const chain = binding.resolvedContracts.contracts.unusedRedeemer;
  if (chain === undefined || chain.steps.length !== 9)
    throw new Error("unusedRedeemer manifest omitted nine-step chain");
  const { steps, witnesses } = context.references;
  const hubOraclePolicyId = binding.contractEntries.hubOracleMint?.scriptHash;
  if (hubOraclePolicyId === undefined)
    throw new Error("unusedRedeemer manifest omitted hub oracle");
  const stateQueuePolicyId = binding.contractEntries.stateQueueMint?.scriptHash;
  if (stateQueuePolicyId === undefined)
    throw new Error("unusedRedeemer manifest omitted state queue");
  const familyContracts: UnusedRedeemerContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: UNUSED_REDEEMER_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as UnusedRedeemerContracts["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId,
    stateQueuePolicyId,
  };
  const actuator = createUnusedRedeemerActuator({
    binding,
    lucid: context.lucid,
    signer: context.signer,
    contracts: familyContracts,
    references: { steps, witnesses },
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  });
  const artifacts = createCanonicalFamilyArtifactPort(({ evidence }) =>
    prepareUnusedRedeemerArtifact(evidence),
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
): CursorFamilyTransactionPort<"unusedRedeemer"> => {
  const { actuator, artifacts } = boundFor(context);
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: "unusedRedeemer",
    prepare: artifacts.prepare,
    validatePreparedArtifact: artifacts.validatePreparedArtifact,
    capture: async ({ action, artifact }) => {
      const input = cursorFamilyActionInput({
        category: "unusedRedeemer",
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
        "step_02a",
        "step_02b",
        "step_02c",
        "step_03",
        "step_04",
        "step_05",
        "step_06",
      ] as const;
      const stage = stages[Number(input.ordinal) - 1];
      if (stage === undefined)
        throw new Error("unusedRedeemer cursor ordinal changed");
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

export const UNUSED_REDEEMER_FAMILY_DEFINITION = defineFamily<
  "unusedRedeemer",
  (typeof WITNESS_ROLES)[number],
  false,
  9
>({
  category: "unusedRedeemer",
  stepDatumSchemas: UNUSED_REDEEMER_STEP_DATUM_SCHEMAS,
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  auxiliaryReferenceScripts: contracts.removal,
  replayer: () => UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: UNUSED_REDEEMER_CURSOR_SPEC,
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
export const createManifestBoundUnusedRedeemerWorkflow = async (
  config: ManifestBoundUnusedRedeemerWorkflowConfig,
): Promise<ManifestBoundUnusedRedeemerWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...UNUSED_REDEEMER_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "unusedRedeemer production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("unusedRedeemer decision digest is malformed");
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    UNUSED_REDEEMER_FAMILY_DEFINITION,
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

export const executeManifestBoundUnusedRedeemerWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundUnusedRedeemerWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  return executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    replayer: UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  });
};
