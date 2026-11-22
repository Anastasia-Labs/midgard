import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { workflowActuationAuthorizingDecisionDigest } from "../workflow/actuation-permit.assert-workflow-actuation-permit-identity.js";
import { MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.admit-reference-scripts.js";
import { STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS } from "../workflow/cursor-family-runtime.js";
import {
  type FamilyAssemblyContext,
  MEMBERSHIP_WITNESS_ROLES,
} from "../workflow/family-definition.family-adapter-arm.js";
import {
  cursorStepContractNames,
  defineFamily,
  type ManifestBoundFamilyWorkflow,
} from "../workflow/family-definition.family-definition.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import type { FieldCarriagePrerequisitePort } from "../workflow/field-carriage-prerequisite.js";
import { workflowJsonDigest } from "../workflow/identity.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.assemble-bound-manifest-bound-family-workflow.js";
import type { FraudProofWorkflowRunResult } from "../workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  createFraudProofWorkflowRegistry,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import { proofMaterialsAccess } from "../workflow/proof-materials-view.js";
import type { FraudProofRawL1FamilyStage } from "../workflow/raw-l1-family-derivation.js";
import type { WorkflowRun } from "../workflow/workflow-run.js";
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
import type { MintDeclaredAssetLimitFoldStateData } from "./family.advance-mint-declared-fold.js";
import { mintDeclaredFoldDataMatches } from "./family.mint-declared-fold-state-data.js";
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
  l1Source: FraudProofL1Source;
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

const WITNESS_ROLES = MEMBERSHIP_WITNESS_ROLES;
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
        workflowJsonDigest(
          await prepareMintDeclaredAssetLimitAcceptedArtifact(routed.evidence),
        ) !== workflowJsonDigest(artifact)
      )
        throw new Error("prepared raw artifact differs from retained evidence");
    },
    validatePreparedArtifact: async ({ evidence, artifact }) => {
      if (
        workflowJsonDigest(
          await prepareMintDeclaredAssetLimitForcedArtifact(evidence),
        ) !== workflowJsonDigest(artifact)
      )
        throw new Error(
          "prepared family artifact differs from retained evidence",
        );
    },
    prepare: async ({ evidence }) =>
      await prepareMintDeclaredAssetLimitForcedArtifact(evidence),
    capture: async ({ action, artifact, holder }) =>
      await actuator.capture({
        action: action.input as unknown as MintDeclaredAssetLimitActuatorAction,
        artifact,
        materials: proofMaterialsAccess({
          signer: context.signer,
          lucid: context.lucid,
          holder,
        }),
      }),
  };
};
const CURSOR_SPEC = {
  category: CATEGORY,
  stepCount: 4,
  successors: { 1: [2], 2: [2, 3, 4], 3: [3, 4], 4: ["proof_token"] },
} as const;

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
  auxiliaryReferenceScripts: STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
  replayer: () => MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: CURSOR_SPEC,
    stepContractNames: cursorStepContractNames(CURSOR_SPEC),
    createRefineAction:
      (context) =>
      async ({ observed, artifact }) => {
        const selected = await currentAction({
          workflow: { lucid: context.lucid },
          artifact: admitMintDeclaredAssetLimitArtifact(artifact).artifact,
          stage: observed.stage,
        });
        if (selected === "none")
          throw new Error("mintDeclaredAssetLimit stage requires no action");
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

const currentAction = async ({
  workflow,
  artifact,
  stage,
}: {
  readonly workflow: Pick<ManifestBoundMintDeclaredAssetLimitWorkflow, "lucid">;
  readonly artifact: MintDeclaredAssetLimitArtifact;
  readonly stage: FraudProofRawL1FamilyStage;
}): Promise<MintDeclaredAssetLimitActuatorAction | "none"> => {
  const admitted = admitMintDeclaredAssetLimitArtifact(artifact);
  if (stage.kind === "not_started")
    return {
      stage: "init",
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
    };
  if (stage.kind === "proof_token")
    return {
      stage: "remove",
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      nextRemovalOutRef: stage.nextRemovalOutRef,
      fraudProofOutRef: stage.fraudProofOutRef,
    };
  if (stage.kind === "removed" || stage.kind === "pending") return "none";
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
    // An absent policy closes at once when negative or when the field opens
    // directly; past a certified field's last item it closes after the
    // grammar walk.
    const absent = admitted.evidence.absent === true;
    const absentAtOnce =
      absent &&
      (admitted.evidence.policyIndex < 0n ||
        admitted.evidence.carriage !== "Certified");
    if ("Bound" in state.data)
      return absentAtOnce
        ? {
            stage: "step_02",
            threadOutRef: stage.threadOutRef,
            action: { kind: "absent", fromGrammar: false },
          }
        : admitted.evidence.carriage === "Certified"
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
          action: absent
            ? { kind: "absent", fromGrammar: true }
            : { kind: "grammar_finish" },
        }
      : {
          stage: "step_02",
          threadOutRef: stage.threadOutRef,
          action: { kind: "grammar_resume", nextOrdinal: ordinal + 1 },
        };
  }
  if (stage.step === 3) {
    const staged = admitted.staged;
    if (!("walk" in staged))
      throw new Error("mintDeclaredAssetLimit absent policy reached the fold");
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
      initialMintDeclaredFoldSnapshot(staged),
      ...staged.walk,
    ].findIndex((snapshot) =>
      mintDeclaredFoldDataMatches(
        state.data,
        mintDeclaredFoldSnapshotData({
          evidence: admitted.evidence,
          staged,
          snapshot,
        }),
      ),
    );
    if (ordinal < 0 || ordinal >= staged.walk.length)
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

export const executeManifestBoundMintDeclaredAssetLimitWorkflow = async ({
  workflow,
  sources,
  run,
}: {
  readonly workflow: ManifestBoundMintDeclaredAssetLimitWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly run: WorkflowRun;
}): Promise<FraudProofWorkflowRunResult> => {
  if (
    workflowActuationAuthorizingDecisionDigest(run) !== workflow.decisionDigest
  )
    throw new Error("mintDeclaredAssetLimit run changed decision digest");
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
    run,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
