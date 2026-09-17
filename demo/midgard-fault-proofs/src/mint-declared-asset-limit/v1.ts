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
import { MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
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
  createMintDeclaredAssetLimitActuator,
  type MintDeclaredAssetLimitActuatorAction,
  mintDeclaredAssetLimitFieldRequirement,
  type MintDeclaredAssetLimitWorkflowReferenceScripts,
} from "./actuator.js";
import {
  admitMintDeclaredAssetLimitArtifact,
  type MintDeclaredAssetLimitArtifact,
} from "./artifact.js";
import {
  MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES,
  type MintDeclaredAssetLimitContracts,
} from "./contracts.js";
import {
  type MintDeclaredAssetLimitFoldStateData,
  mintDeclaredFoldDataMatches,
} from "./family.js";
import {
  prepareMintDeclaredAssetLimitAcceptedArtifact,
  prepareMintDeclaredAssetLimitForcedArtifact,
} from "./replay.js";
import {
  MintDeclaredAssetLimitStep02DatumSchema,
  MintDeclaredAssetLimitStep03DatumSchema,
  MintDeclaredAssetLimitStep04DatumSchema,
} from "./schemas.js";
import {
  hashMintDeclaredGrammarCheckpoint,
  initialMintDeclaredFoldSnapshot,
} from "./staged-plan.js";
import { mintDeclaredFoldSnapshotData } from "./submit-step-03.js";

export const MINT_DECLARED_ASSET_LIMIT_WORKFLOW =
  "midgard-mint-declared-asset-limit-production-workflow-v1" as const;

export type MintDeclaredAssetLimitRemovalReferenceScripts = Readonly<{
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

export type ManifestBoundMintDeclaredAssetLimitWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: MintDeclaredAssetLimitWorkflowReferenceScripts &
    Readonly<{ removal: MintDeclaredAssetLimitRemovalReferenceScripts }>;
}>;

export type ManifestBoundMintDeclaredAssetLimitWorkflow =
  ManifestBoundFamilyWorkflow<"mintDeclaredAssetLimit", true, 4> &
    WorkflowExtension &
    Readonly<{ decisionDigest: string }>;

const CATEGORY = "mintDeclaredAssetLimit" as const;

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;
type BoundContext = FamilyAssemblyContext<
  "mintDeclaredAssetLimit",
  (typeof WITNESS_ROLES)[number],
  true,
  4
>;
type WorkflowExtension = Readonly<{
  actuator: ReturnType<typeof createMintDeclaredAssetLimitActuator>;
  prerequisite: FieldCarriagePrerequisitePort<"mintDeclaredAssetLimit">;
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
    throw new Error(
      "mintDeclaredAssetLimit manifest omitted required contracts",
    );
  const contracts: MintDeclaredAssetLimitContracts = Object.freeze({
    steps: chain.steps.map((step, index) => ({
      blueprintTitle: MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as MintDeclaredAssetLimitContracts["steps"],
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
  const actuator = createMintDeclaredAssetLimitActuator({
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
): CursorFamilyTransactionPort<"mintDeclaredAssetLimit"> => {
  const actuator = boundFor(context);
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: CATEGORY,
    prepareRaw: async (routed) => {
      if (routed.kind !== "mint_declared_asset_limit")
        throw new Error("raw family route changed");
      return await prepareMintDeclaredAssetLimitAcceptedArtifact(
        routed.evidence,
      );
    },
    validatePreparedRawArtifact: async ({ routed, artifact }) => {
      if (routed.kind !== "mint_declared_asset_limit")
        throw new Error("raw family route changed");
      if (
        journalJsonDigest(
          await prepareMintDeclaredAssetLimitAcceptedArtifact(routed.evidence),
        ) !== journalJsonDigest(artifact)
      )
        throw new Error("prepared raw artifact differs from retained evidence");
    },
    validatePreparedArtifact: async ({ evidence, artifact }) => {
      if (
        journalJsonDigest(
          await prepareMintDeclaredAssetLimitForcedArtifact(evidence),
        ) !== journalJsonDigest(artifact)
      )
        throw new Error(
          "prepared family artifact differs from retained evidence",
        );
    },
    prepare: async ({ evidence }) =>
      await prepareMintDeclaredAssetLimitForcedArtifact(evidence),
    capture: async ({ action, artifact }) =>
      await actuator.capture({
        action: action.input as unknown as MintDeclaredAssetLimitActuatorAction,
        artifact,
      }),
  };
};
export const MINT_DECLARED_ASSET_LIMIT_FAMILY_DEFINITION = defineFamily<
  "mintDeclaredAssetLimit",
  (typeof WITNESS_ROLES)[number],
  true,
  4
>({
  category: CATEGORY,
  stepDatumSchemas: [
    MintDeclaredAssetLimitStep02DatumSchema,
    MintDeclaredAssetLimitStep02DatumSchema,
    MintDeclaredAssetLimitStep03DatumSchema,
    MintDeclaredAssetLimitStep04DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: {
      category: CATEGORY,
      stepCount: 4,
      successors: { 1: [2], 2: [2, 3, 4], 3: [3, 4], 4: ["proof_token"] },
    },
    stepContractNames: [
      "fraudProofMintDeclaredAssetLimit",
      "fraudProofMintDeclaredAssetLimitStep02",
      "fraudProofMintDeclaredAssetLimitStep03",
      "fraudProofMintDeclaredAssetLimitStep04",
    ],
    createRefineAction:
      (context) =>
      async ({ observed, artifact }) => {
        const selected = await currentAction({
          workflow: { lucid: context.lucid },
          artifact: admitMintDeclaredAssetLimitArtifact(artifact).artifact,
          stage: observed.stage,
        });
        if (selected === "removed")
          throw new Error("mintDeclaredAssetLimit action became terminal");
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
        return mintDeclaredAssetLimitFieldRequirement({
          action:
            action.input as unknown as MintDeclaredAssetLimitActuatorAction,
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

export const createManifestBoundMintDeclaredAssetLimitWorkflow = async (
  config: ManifestBoundMintDeclaredAssetLimitWorkflowConfig,
): Promise<ManifestBoundMintDeclaredAssetLimitWorkflow> => {
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("mintDeclaredAssetLimit decision digest is malformed");
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    MINT_DECLARED_ASSET_LIMIT_FAMILY_DEFINITION,
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
  readonly workflow: Pick<ManifestBoundMintDeclaredAssetLimitWorkflow, "lucid">;
  readonly artifact: MintDeclaredAssetLimitArtifact;
  readonly stage: FraudProofRawL1FamilyStage;
}): Promise<MintDeclaredAssetLimitActuatorAction | "removed"> => {
  const admitted = admitMintDeclaredAssetLimitArtifact(artifact);
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
      MintDeclaredAssetLimitStep02DatumSchema as never,
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
      (checkpoint) => hashMintDeclaredGrammarCheckpoint(checkpoint) === grammar,
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
      MintDeclaredAssetLimitStep03DatumSchema as never,
    ) as { data: MintDeclaredAssetLimitFoldStateData };
    const ordinal = [
      initialMintDeclaredFoldSnapshot(admitted.staged),
      ...admitted.staged.walk,
    ].findIndex((snapshot) =>
      mintDeclaredFoldDataMatches(
        state.data,
        mintDeclaredFoldSnapshotData({
          evidence: admitted.evidence,
          staged: admitted.staged,
          snapshot,
        }),
      ),
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

export type MintDeclaredAssetLimitWorkflowRunResult =
  FraudProofWorkflowRunResult;

export const executeManifestBoundMintDeclaredAssetLimitWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMintDeclaredAssetLimitWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  if (
    workflowActuationAuthorizingDecisionDigest(journal) !==
    workflow.decisionDigest
  )
    throw new Error("mintDeclaredAssetLimit journal changed decision digest");
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
    replayer: MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: [CATEGORY],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export type LoadedMintDeclaredAssetLimitWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundMintDeclaredAssetLimitWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadMintDeclaredAssetLimitWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedMintDeclaredAssetLimitWorkflow>;

/** Standard runtime-loader-compatible package runner; core config has no callbacks. */
export const createMintDeclaredAssetLimitWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadMintDeclaredAssetLimitWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== (CATEGORY as string))
        throw new Error("mintDeclaredAssetLimit runner category changed");
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
            "mintDeclaredAssetLimit runtime requires concrete public retained DA",
          );
        const workflow =
          await createManifestBoundMintDeclaredAssetLimitWorkflow(
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
        return (await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            executeManifestBoundMintDeclaredAssetLimitWorkflow({
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
