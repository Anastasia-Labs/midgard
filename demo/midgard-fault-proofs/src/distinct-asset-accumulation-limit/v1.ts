import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
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
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
} from "../workflow/cursor-family-adapter.js";
import { cursorStringField } from "../workflow/cursor-family-runtime.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
  normalizeJournalJson,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  resumeRecordedFraudProofWorkflow,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import { continuePendingWorkflow } from "../workflow/pending-continuation.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  withProofChunkPrerequisite,
} from "../workflow/proof-chunk-prerequisite.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
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

export type ManifestBoundDistinctAssetAccumulationWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"distinctAssetAccumulationLimit">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  decisionDigest: string;
  l1: FraudProofFamilyL1ObservationPort<"distinctAssetAccumulationLimit">;
  actuator: ReturnType<typeof createDistinctAssetAccumulationActuator>;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

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
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
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
  const bind = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo,
    });
  const steps = manifestContracts.steps.map((name, index) =>
    bind(name, config.referenceScripts.steps[index]!),
  ) as unknown as DistinctAssetAccumulationWorkflowReferences["steps"];
  const witnesses = Object.fromEntries(
    Object.entries(manifestContracts.witnesses).map(([role, name]) => [
      role,
      bind(
        name,
        config.referenceScripts.witnesses[
          role as keyof FaultProofWitnessReferenceScripts
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScripts>;
  for (const [role, name] of Object.entries(manifestContracts.removal))
    bind(
      name,
      config.referenceScripts.removal[
        role as keyof DistinctAssetAccumulationRemovalReferences
      ],
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
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const actuator = createDistinctAssetAccumulationActuator({
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    categoryId: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID,
    contracts,
    references: { steps, witnesses },
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProofSpendingScriptHash:
      binding.resolvedContracts.contracts.fraudProof.spendingScriptHash,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  });
  const adapter = withProofChunkPrerequisite({
    category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
    prerequisite: createAuthenticatedProofChunkPrerequisitePort({
      category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      maximumTransactionBytes: binding.cardanoProtocolParameters.maxTxSize,
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
      proofCborForAction: ({ action, artifact }) =>
        action.input.stage === "step_01"
          ? (admitDistinctAssetWorkflowArtifact(artifact).accepted?.txInclusion
              .txMembershipProofCbor ?? null)
          : null,
    }),
    base: createCursorFamilyWorkflowAdapter({
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
      l1,
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
      transactions: {
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
                  lucid: config.lucid,
                  address: config.signer.address,
                  proofCbor:
                    artifact.accepted.txInclusion.txMembershipProofCbor,
                })
              : undefined;
          return await actuator.capture({
            action: selected,
            artifact,
            publishedProofChunks,
          });
        },
      },
    }),
  });
  return Object.freeze({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    decisionDigest: config.decisionDigest,
    l1,
    actuator,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
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

export const runOrResumeManifestBoundDistinctAssetAccumulationWorkflow =
  async (input: {
    workflow: ManifestBoundDistinctAssetAccumulationWorkflow;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStore;
  }) => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow")
      throw new Error(
        "distinctAssetAccumulationLimit runner rejects caller-authored evidence",
      );
    return await executeManifestBoundDistinctAssetAccumulationWorkflow(input);
  };

export type LoadedDistinctAssetAccumulationWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundDistinctAssetAccumulationWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadDistinctAssetAccumulationWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedDistinctAssetAccumulationWorkflow>;

/** Standard strict loader-based surface consumed by ProductionWorkflowAdapter. */
export const createDistinctAssetAccumulationWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadDistinctAssetAccumulationWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY)
        throw new Error(
          "distinctAssetAccumulationLimit runner category changed",
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
          category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
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
            "distinctAssetAccumulationLimit requires concrete public retained DA",
          );
        const workflow =
          await createManifestBoundDistinctAssetAccumulationWorkflow(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "distinctAssetAccumulationLimit runtime binding changed invocation",
          );
        return (await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            runOrResumeManifestBoundDistinctAssetAccumulationWorkflow({
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
