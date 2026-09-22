import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  workflowActuationAuthorizingDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "../workflow/actuation-permit.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  cursorStringField,
  STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
} from "../workflow/cursor-family-runtime.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
  type ManifestBoundFamilyWorkflow,
} from "../workflow/family-definition.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import {
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
  normalizeJournalJson,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  resumeRecordedFraudProofWorkflow,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import {
  createDistinctAssetAccumulationActuator,
  type DistinctAssetAccumulationActuationArtifact,
  type DistinctAssetAccumulationActuatorAction,
  type DistinctAssetAccumulationWorkflowReferences,
} from "./actuator.js";
import { prepareDistinctAssetAccumulationArtifact } from "./authenticated-replay.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_BLUEPRINT_TITLES,
  type DistinctAssetAccumulationContracts,
} from "./contracts.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID,
  type DistinctAssetAccumulationEvidence,
  prepareDistinctAssetAccumulationEvidence,
} from "./family.js";
import { distinctAssetPreparedJournalArtifact } from "./proof-carriage.js";
import {
  DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS,
  DistinctAssetStep01RedeemerSchema,
  DistinctAssetStep02RedeemerSchema,
  DistinctAssetStep03RedeemerSchema,
  DistinctAssetStep04RedeemerSchema,
  DistinctAssetStep05RedeemerSchema,
  DistinctAssetVerdictSubjectSchema,
} from "./schemas.js";

export const DISTINCT_ASSET_ACCUMULATION_WORKFLOW =
  "midgard-distinct-asset-accumulation-production-workflow-v1" as const;

export type DistinctAssetAccumulationRemovalReferences = Readonly<{
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

export type DistinctAssetAccumulationReferences =
  DistinctAssetAccumulationWorkflowReferences &
    Readonly<{ removal: DistinctAssetAccumulationRemovalReferences }>;

export const DISTINCT_ASSET_ACCUMULATION_CONFIG_KEYS = Object.freeze([
  "manifest",
  "blueprintJson",
  "deploymentInfo",
  "headerHash",
  "lucid",
  "signer",
  "source",
  "decisionDigest",
  "referenceScripts",
  "stateQueueMutationLeaseCoordinator",
] as const);

export type ManifestBoundDistinctAssetAccumulationWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  referenceScripts: DistinctAssetAccumulationReferences;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundDistinctAssetAccumulationWorkflow =
  ManifestBoundFamilyWorkflow<"distinctAssetAccumulationLimit", false, 6> &
    WorkflowExtension &
    Readonly<{ decisionDigest: string }>;

const manifestContracts = Object.freeze({
  steps: [
    "fraudProofDistinctAssetAccumulationLimit",
    "fraudProofDistinctAssetAccumulationLimitStep02",
    "fraudProofDistinctAssetAccumulationLimitStep03",
    "fraudProofDistinctAssetAccumulationLimitStep04",
    "fraudProofDistinctAssetAccumulationLimitStep05",
    "fraudProofDistinctAssetAccumulationLimitStep06",
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
  "distinctAssetAccumulationLimit",
  (typeof WITNESS_ROLES)[number],
  false,
  6
>;
type WorkflowExtension = Readonly<{
  actuator: ReturnType<typeof createDistinctAssetAccumulationActuator>;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
}>;
const bindFamily = (context: BoundContext) => {
  const {
    binding,
    references: { steps, witnesses },
  } = context;
  const chain =
    binding.resolvedContracts.contracts.distinctAssetAccumulationLimit;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 6 ||
    stateQueuePolicyId === undefined
  )
    throw new Error(
      "distinctAssetAccumulationLimit manifest omitted required contracts",
    );
  const contracts: DistinctAssetAccumulationContracts = Object.freeze({
    steps: chain.steps.map((step, index) => ({
      blueprintTitle:
        DISTINCT_ASSET_ACCUMULATION_LIMIT_BLUEPRINT_TITLES[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
    })) as unknown as DistinctAssetAccumulationContracts["steps"],
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
  });
  const actuator = createDistinctAssetAccumulationActuator({
    lucid: context.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: context.signer,
    categoryId: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID,
    contracts,
    references: { steps, witnesses },
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
    fraudProofSpendingScriptHash:
      binding.resolvedContracts.contracts.fraudProof.spendingScriptHash,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
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
): CursorFamilyTransactionPort<"distinctAssetAccumulationLimit"> => {
  const actuator = boundFor(context);
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
    validatePreparedArtifact: async ({ evidence, artifact }) => {
      if (
        journalJsonDigest(
          distinctAssetWorkflowArtifact(
            await prepareDistinctAssetAccumulationArtifact(evidence),
          ),
        ) !== journalJsonDigest(artifact)
      )
        throw new Error(
          "prepared family artifact differs from retained evidence",
        );
    },
    prepare: async ({ evidence }) =>
      distinctAssetWorkflowArtifact(
        await prepareDistinctAssetAccumulationArtifact(evidence),
      ),
    capture: async ({ action, artifact: serialized }) => {
      const artifact = admitDistinctAssetWorkflowArtifact(serialized);
      const input = action.input;
      let selected: DistinctAssetAccumulationActuatorAction;
      if (input.stage === "init")
        selected = {
          stage: "init",
          stateQueueBlockOutRef: cursorStringField(
            input,
            "stateQueueBlockOutRef",
          ),
        };
      else if (input.stage === "remove")
        selected = {
          stage: "remove",
          nextRemovalOutRef: cursorStringField(input, "nextRemovalOutRef"),
          fraudProofOutRef: cursorStringField(input, "fraudProofOutRef"),
        };
      else if (input.ordinal === 1)
        selected = {
          stage: "step01",
          threadOutRef: cursorStringField(input, "threadOutRef"),
          stateQueueBlockOutRef: cursorStringField(
            input,
            "stateQueueBlockOutRef",
          ),
        };
      else if (input.ordinal === 2)
        selected = {
          stage: "step02",
          threadOutRef: cursorStringField(input, "threadOutRef"),
        };
      else if (input.ordinal === 6)
        selected = {
          stage: "step06",
          threadOutRef: cursorStringField(input, "threadOutRef"),
        };
      else if (
        input.ordinal === 3 ||
        input.ordinal === 4 ||
        input.ordinal === 5
      )
        selected = {
          stage: "fold",
          threadOutRef: cursorStringField(input, "threadOutRef"),
          stepIndex: (input.ordinal - 1) as 2 | 3 | 4,
        };
      else throw new Error("distinctAssetAccumulationLimit action changed");
      const publishedProofChunks =
        selected.stage === "step01" && artifact.accepted !== undefined
          ? await resolvePublishedProofChunks({
              lucid: context.lucid,
              address: context.signer.address,
              proofCbor: artifact.accepted.txInclusion.txMembershipProofCbor,
            })
          : undefined;
      return await actuator.capture({
        action: selected,
        artifact,
        publishedProofChunks,
      });
    },
  };
};
export const DISTINCT_ASSET_ACCUMULATION_FAMILY_DEFINITION = defineFamily<
  "distinctAssetAccumulationLimit",
  (typeof WITNESS_ROLES)[number],
  false,
  6
>({
  category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
  stepDatumSchemas: DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS,
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  auxiliaryReferenceScripts: manifestContracts.removal,
  replayer: () => DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: {
      category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
      stepCount: 6,
      successors: {
        1: [2],
        2: [3],
        3: [4],
        4: [5],
        5: [6],
        6: ["proof_token"],
      },
    },
    stepContractNames: manifestContracts.steps,
    transactionPort: createTransactionPort,
  },
  proofChunk: (_context, { action, artifact }) =>
    action.input.stage === "step_01"
      ? (admitDistinctAssetWorkflowArtifact(artifact).accepted?.txInclusion
          .txMembershipProofCbor ?? null)
      : null,
  extend: (context): WorkflowExtension => ({
    actuator: boundFor(context),
    lucid: context.lucid,
    signer: context.signer,
  }),
});
/** Manifest/reference/signer-bound workflow construction with no evidence input. */
export const createManifestBoundDistinctAssetAccumulationWorkflow = async (
  config: ManifestBoundDistinctAssetAccumulationWorkflowConfig,
): Promise<ManifestBoundDistinctAssetAccumulationWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...DISTINCT_ASSET_ACCUMULATION_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "distinctAssetAccumulationLimit production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error(
      "distinctAssetAccumulationLimit decision digest is malformed",
    );
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    DISTINCT_ASSET_ACCUMULATION_FAMILY_DEFINITION,
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

/** Exact schema-carried witnesses and scalar diagnostics, admitted again at capture. */
export const distinctAssetWorkflowArtifact = (
  artifact: DistinctAssetAccumulationActuationArtifact,
): JournalJsonObject => ({
  ...distinctAssetPreparedJournalArtifact(artifact),
  evidence: normalizeJournalJson({
    traceStateHashHex: artifact.evidence.traceStateHashHex,
    workRootHex: artifact.evidence.workRootHex,
    pre: artifact.evidence.pre,
    post: artifact.evidence.post,
    mutationWasPresent: artifact.evidence.mutationWasPresent,
  }),
  accepted:
    artifact.accepted === undefined
      ? null
      : {
          validationTracesRoot: artifact.accepted.validationTracesRoot,
          validationTraceCount:
            artifact.accepted.validationTraceCount.toString(),
        },
});

export const admitDistinctAssetWorkflowArtifact = (
  serialized: JournalJsonObject,
): DistinctAssetAccumulationActuationArtifact => {
  const subject = Data.from(
    cursorStringField(serialized, "subjectCbor"),
    DistinctAssetVerdictSubjectSchema as never,
  ) as DistinctAssetAccumulationActuationArtifact["finding"]["subject"];
  const finding = {
    subject,
    coordinate:
      serialized.coordinate as unknown as DistinctAssetAccumulationActuationArtifact["finding"]["coordinate"],
  };
  const evidence = prepareDistinctAssetAccumulationEvidence({
    ...(serialized.evidence as unknown as DistinctAssetAccumulationEvidence),
    finding,
  });
  const source = serialized.source as JournalJsonObject;
  const accepted = serialized.accepted as {
    validationTracesRoot: string;
    validationTraceCount: string;
  } | null;
  const forced =
    accepted !== null
      ? undefined
      : (
          Data.from(
            cursorStringField(source, "forcedSourceCbor"),
            DistinctAssetStep01RedeemerSchema as never,
          ) as {
            Continue: [
              {
                source: {
                  ForcedSource: DistinctAssetAccumulationActuationArtifact["forcedSource"];
                };
              },
            ];
          }
        ).Continue[0].source.ForcedSource;
  const authentication = (
    Data.from(
      cursorStringField(serialized, "authenticationCbor"),
      DistinctAssetStep02RedeemerSchema as never,
    ) as {
      Continue: [DistinctAssetAccumulationActuationArtifact["authentication"]];
    }
  ).Continue[0];
  const schemas = [
    DistinctAssetStep03RedeemerSchema,
    DistinctAssetStep04RedeemerSchema,
    DistinctAssetStep05RedeemerSchema,
  ];
  const folds = (serialized.foldCbors as string[]).map((cbor, index) => {
    const decoded = (
      Data.from(cbor, schemas[index] as never) as {
        Continue: [{ Skip?: unknown; Authenticate?: { evidence: unknown } }];
      }
    ).Continue[0];
    return "Skip" in decoded
      ? { kind: "skip" as const }
      : {
          kind: "authenticate" as const,
          evidence: decoded.Authenticate!.evidence,
        };
  }) as unknown as DistinctAssetAccumulationActuationArtifact["folds"];
  const artifact: DistinctAssetAccumulationActuationArtifact = {
    headerHash: cursorStringField(serialized, "headerHash"),
    finding,
    evidence,
    authentication,
    folds,
    ...(accepted === null
      ? { forcedSource: forced }
      : {
          accepted: {
            validationTracesRoot: accepted.validationTracesRoot,
            validationTraceCount: BigInt(accepted.validationTraceCount),
            txInclusion: parseSubmitStep01TxInclusion({
              ...source,
              nativeTx: nativeTxFromCoreCompact(
                decodeMidgardNativeTxCompact(
                  Buffer.from(
                    cursorStringField(source, "nativeTxCompactCbor"),
                    "hex",
                  ),
                ),
              ),
            }),
          },
        }),
  };
  if (
    journalJsonDigest(distinctAssetWorkflowArtifact(artifact)) !==
    journalJsonDigest(serialized)
  )
    throw new Error("distinctAssetAccumulationLimit prepared artifact changed");
  return artifact;
};

export const executeManifestBoundDistinctAssetAccumulationWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundDistinctAssetAccumulationWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  if (
    workflowActuationAuthorizingDecisionDigest(journal) !==
    workflow.decisionDigest
  )
    throw new Error(
      "distinctAssetAccumulationLimit journal changed decision digest",
    );
  if (workflowJournalIsReconciliationOnly(journal))
    return await resumeRecordedFraudProofWorkflow({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
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
    replayer: DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: [DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
