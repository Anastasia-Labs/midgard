import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import {
  cursorStringField,
  STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
} from "../workflow/cursor-family-runtime.js";
import {
  type FamilyAssemblyContext,
  type ManifestBoundFamilyWorkflow,
} from "../workflow/family-definition.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import {
  journalJsonDigest,
  normalizeJournalJson,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import {
  createDistinctAssetAccumulationActuator,
  type DistinctAssetAccumulationActuationArtifact,
  type DistinctAssetAccumulationWorkflowReferences,
} from "./actuator.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_BLUEPRINT_TITLES,
  type DistinctAssetAccumulationContracts,
} from "./contracts.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID,
  type DistinctAssetAccumulationEvidence,
  prepareDistinctAssetAccumulationEvidence,
} from "./family.js";
import { distinctAssetPreparedJournalArtifact } from "./proof-carriage.js";
import {
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

export const manifestContracts = Object.freeze({
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

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;

export type BoundContext = FamilyAssemblyContext<
  "distinctAssetAccumulationLimit",
  (typeof WITNESS_ROLES)[number],
  false,
  6
>;

export type WorkflowExtension = Readonly<{
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

export const bound = new WeakMap<BoundContext, ReturnType<typeof bindFamily>>();

export const boundFor = (context: BoundContext) => {
  const existing = bound.get(context);
  if (existing !== undefined) return existing;
  const created = bindFamily(context);
  bound.set(context, created);
  return created;
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
