import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
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
import {
  encodeWorkflowArtifact,
  requireWorkflowArtifactMatches,
} from "../workflow/artifact-codec.js";
import { MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
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
import { type FieldCarriagePrerequisitePort } from "../workflow/field-carriage-prerequisite.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import { executeManifestBoundFamilyRecovery } from "../workflow/manifest-bound-family-recovery.js";
import type { FraudProofWorkflowAction } from "../workflow/orchestrator.js";
import { type FraudProofWorkflowRunResult } from "../workflow/orchestrator.js";
import { continuePendingWorkflow } from "../workflow/pending-continuation.js";
import {
  createMissingRedeemerActuator,
  type MissingRedeemerActuatorAction,
  missingRedeemerFieldRequirement,
  type MissingRedeemerWorkflowReferences,
} from "./actuator.js";
import {
  MISSING_REDEEMER_BLUEPRINT_TITLES,
  type MissingRedeemerContracts,
} from "./contracts.js";
import { MISSING_REDEEMER_CATEGORY } from "./family.js";
import {
  type MissingRedeemerArtifact,
  replayMissingRedeemer,
} from "./replay.js";
import {
  MissingRedeemerAuthenticationStateSchema,
  MissingRedeemerStep02aDatumSchema,
  MissingRedeemerStep02bDatumSchema,
  MissingRedeemerStep02DatumSchema,
  MissingRedeemerStep03DatumSchema,
  MissingRedeemerStep04DatumSchema,
  MissingRedeemerStep05DatumSchema,
} from "./schemas.js";
import {
  createMissingRedeemerStagedPlanner,
  hashMissingRedeemerGrammarCheckpoint,
} from "./staged-plan.js";

export const MISSING_REDEEMER_CONFIG_KEYS = Object.freeze([
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

export const MISSING_REDEEMER_STEP_DATUM_SCHEMAS = Object.freeze([
  FraudProofComputationThreadStepDatum,
  MissingRedeemerStep02DatumSchema,
  MissingRedeemerStep02aDatumSchema,
  MissingRedeemerStep02bDatumSchema,
  MissingRedeemerStep03DatumSchema,
  MissingRedeemerStep04DatumSchema,
  MissingRedeemerStep05DatumSchema,
] as const);

export const MISSING_REDEEMER_CURSOR_SPEC: CursorFamilySpec<"missingRedeemer"> &
  Readonly<{ stepCount: 7 }> = {
  category: "missingRedeemer",
  stepCount: 7,
  successors: {
    1: [2],
    2: [3],
    3: [4],
    4: [5],
    5: [5, 6],
    6: [6, 7],
    7: ["proof_token"],
  },
};

export type MissingRedeemerRemovalReferenceScripts = Readonly<{
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

export type MissingRedeemerWorkflowReferenceScripts = Readonly<{
  steps: MissingRedeemerWorkflowReferences["steps"];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
  removal: MissingRedeemerRemovalReferenceScripts;
}>;

export type ManifestBoundMissingRedeemerWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: MissingRedeemerWorkflowReferenceScripts;
}>;

export type ManifestBoundMissingRedeemerWorkflow = ManifestBoundFamilyWorkflow<
  "missingRedeemer",
  true,
  7
> &
  WorkflowExtension &
  Readonly<{ decisionDigest: string }>;

const manifestContracts = Object.freeze({
  steps: [
    "fraudProofMissingRedeemer",
    "fraudProofMissingRedeemerStep02",
    "fraudProofMissingRedeemerStep02a",
    "fraudProofMissingRedeemerStep02b",
    "fraudProofMissingRedeemerStep03",
    "fraudProofMissingRedeemerStep04",
    "fraudProofMissingRedeemerStep05",
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
  "missingRedeemer",
  (typeof WITNESS_ROLES)[number],
  true,
  7
>;
type WorkflowExtension = Readonly<{
  actuator: ReturnType<typeof createMissingRedeemerActuator>;
  prerequisite: FieldCarriagePrerequisitePort<"missingRedeemer">;
  lucid: LucidEvolution;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
const bindFamily = (context: BoundContext) => {
  const {
    binding,
    references: { steps, witnesses },
  } = context;
  const chain = binding.resolvedContracts.contracts.missingRedeemer;
  const certificate = binding.fieldPreimageCertificate;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 7 ||
    certificate === null ||
    stateQueuePolicyId === undefined
  )
    throw new Error("missingRedeemer manifest omitted required contracts");
  const contracts: MissingRedeemerContracts = Object.freeze({
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: MISSING_REDEEMER_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as MissingRedeemerContracts["steps"],
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
  const planStagedWalk = createMissingRedeemerStagedPlanner();
  const actuator = createMissingRedeemerActuator({
    planStagedWalk,
    binding,
    lucid: context.lucid,
    signer: context.signer,
    contracts,
    references: { steps, witnesses },
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  });
  const actionFor = async (
    action: FraudProofWorkflowAction,
    artifact: MissingRedeemerArtifact,
  ): Promise<MissingRedeemerActuatorAction> => {
    const input = cursorFamilyActionInput({
      category: "missingRedeemer",
      action,
    });
    if (input.stage === "init")
      return {
        stage: "init",
        stateQueueBlockOutRef: cursorStringField(
          input,
          "stateQueueBlockOutRef",
        ),
      };
    if (input.stage === "remove")
      return {
        stage: "remove",
        nextRemovalOutRef: cursorStringField(input, "nextRemovalOutRef"),
        fraudProofOutRef: cursorStringField(input, "fraudProofOutRef"),
      };
    const threadOutRef = cursorStringField(input, "threadOutRef");
    const fixed = (["step_01", "step_02", "step_02a", "step_02b"] as const)[
      Number(input.ordinal) - 1
    ];
    if (fixed !== undefined)
      return fixed === "step_01"
        ? {
            stage: fixed,
            threadOutRef,
            stateQueueBlockOutRef: cursorStringField(
              input,
              "stateQueueBlockOutRef",
            ),
          }
        : { stage: fixed, threadOutRef };
    if (input.ordinal === 6) return { stage: "scan", threadOutRef };
    if (input.ordinal === 7) return { stage: "finalize", threadOutRef };
    if (input.ordinal !== 5)
      throw new Error("missingRedeemer cursor ordinal changed");
    const { threadUtxo } = await requireLinearFaultThreadUtxo({
      lucid: context.lucid,
      contracts,
      categoryId: binding.definition.categoryId,
      family: "missing-redeemer",
      stepIndex: 4,
      threadOutRef,
    });
    const state = requireLinearFaultStepState<
      Data.Static<typeof MissingRedeemerAuthenticationStateSchema>
    >({
      threadUtxo,
      signer: context.signer,
      schema: MissingRedeemerStep03DatumSchema as never,
      family: "missing-redeemer",
      stepIndex: 4,
    });
    if ("Ready" in state)
      return {
        stage: "field",
        threadOutRef,
        action: {
          kind:
            artifact.evidence.carriage === "Certified"
              ? "grammar_start"
              : "direct",
        },
      };
    const staged = planStagedWalk({
      transactionId: artifact.evidence.subject.transaction_id,
      fieldPreimageCbor: artifact.evidence.fieldPreimageHex,
    });
    const index = staged.grammar.findIndex(
      (checkpoint) =>
        hashMissingRedeemerGrammarCheckpoint(checkpoint) ===
        state.Grammar.checkpoint_hash,
    );
    if (index < 0)
      throw new Error(
        "missingRedeemer authenticated grammar checkpoint is absent from exact plan",
      );
    return {
      stage: "field",
      threadOutRef,
      action:
        index === staged.grammar.length - 1
          ? { kind: "grammar_finish" }
          : { kind: "grammar_resume", ordinal: index + 1 },
    };
  };
  let freshArtifact: MissingRedeemerArtifact | undefined;
  const restore = (
    artifact: import("../workflow/journal.js").JournalJsonObject,
  ) => {
    if (freshArtifact === undefined)
      throw new Error(
        "missingRedeemer capture requires current authenticated artifact validation",
      );
    return requireWorkflowArtifactMatches(artifact, freshArtifact);
  };
  const prepareArtifact = async (
    evidence: import("../evidence/canonical-block-evidence.js").CanonicalBlockEvidence,
  ) => {
    const selected = (await replayMissingRedeemer(evidence))[0];
    if (selected === undefined)
      throw new Error("missingRedeemer replay has no selected artifact");
    return selected.artifact;
  };
  const transactions: CursorFamilyTransactionPort<"missingRedeemer"> = {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: "missingRedeemer",
    prepare: async ({ evidence }) => {
      freshArtifact = await prepareArtifact(evidence);
      return encodeWorkflowArtifact(freshArtifact);
    },
    validatePreparedArtifact: async ({ evidence, artifact }) => {
      freshArtifact = requireWorkflowArtifactMatches(
        artifact,
        await prepareArtifact(evidence),
      );
    },
    capture: async ({ action, artifact }) => {
      const restored = restore(artifact);
      return actuator.capture({
        action: await actionFor(action, restored),
        artifact: restored,
      });
    },
  };
  return { actuator, transactions, actionFor, restore, planStagedWalk };
};
const bound = new WeakMap<BoundContext, ReturnType<typeof bindFamily>>();
const boundFor = (context: BoundContext) => {
  const existing = bound.get(context);
  if (existing !== undefined) return existing;
  const created = bindFamily(context);
  bound.set(context, created);
  return created;
};
export const MISSING_REDEEMER_FAMILY_DEFINITION = defineFamily<
  "missingRedeemer",
  (typeof WITNESS_ROLES)[number],
  true,
  7
>({
  category: MISSING_REDEEMER_CATEGORY,
  stepDatumSchemas: MISSING_REDEEMER_STEP_DATUM_SCHEMAS,
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  auxiliaryReferenceScripts: manifestContracts.removal,
  replayer: () => MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: MISSING_REDEEMER_CURSOR_SPEC,
    stepContractNames: manifestContracts.steps,
    transactionPort: (context) => boundFor(context).transactions,
  },
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        const { actionFor, restore, planStagedWalk } = boundFor(context);
        const { certificate } = context;
        return missingRedeemerFieldRequirement({
          planStagedWalk,
          action: await actionFor(action, restore(artifact)),
          artifact: restore(artifact),
          owner: context.signer.paymentKeyHash,
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo:
              context.references.fieldPreimageCertificateMint,
          },
        });
      },
    },
  ],
  extend: (context): WorkflowExtension => ({
    actuator: boundFor(context).actuator,
    prerequisite: context.fieldCarriagePrerequisites[0]!,
    lucid: context.lucid,
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  }),
});
/** Strict manifest-bound construction; proof evidence is never configurable. */
export const createManifestBoundMissingRedeemerWorkflow = async (
  config: ManifestBoundMissingRedeemerWorkflowConfig,
): Promise<ManifestBoundMissingRedeemerWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...MISSING_REDEEMER_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "missingRedeemer production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("missingRedeemer decision digest is malformed");
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    MISSING_REDEEMER_FAMILY_DEFINITION,
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

export type LoadedMissingRedeemerWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundMissingRedeemerWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadMissingRedeemerWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedMissingRedeemerWorkflow>;

export const executeManifestBoundMissingRedeemerWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundMissingRedeemerWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> =>
  executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    replayer: MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  });

export const runOrResumeManifestBoundMissingRedeemerWorkflow = async (input: {
  workflow: ManifestBoundMissingRedeemerWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}) => {
  if (Object.keys(input).sort().join(",") !== "journal,sources,workflow")
    throw new Error("missingRedeemer runner rejects caller-authored evidence");
  return await executeManifestBoundMissingRedeemerWorkflow(input);
};

/** Loader-compatible surface; central admission remains fixed-category only. */
export const createMissingRedeemerWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadMissingRedeemerWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== MISSING_REDEEMER_CATEGORY)
        throw new Error("missingRedeemer runner category changed");
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
            "missingRedeemer requires concrete public retained DA",
          );
        const workflow = await createManifestBoundMissingRedeemerWorkflow(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error("missingRedeemer runtime binding changed invocation");
        const journal = bindWorkflowFundingReservationJournal({
          permit: invocation.fundingReservationPermit,
          journal: bindWorkflowActuationJournal({
            journal: new DirectoryFraudProofWorkflowJournalStore(
              invocation.journalDirectory,
            ),
            permit: invocation.actuationPermit,
            decisionDigest: invocation.decisionDigest,
            deploymentFingerprint: invocation.deploymentFingerprint,
            category: MISSING_REDEEMER_CATEGORY,
            headerHash: invocation.headerHash,
          }),
        });
        return await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            executeManifestBoundMissingRedeemerWorkflow({
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
