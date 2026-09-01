import {
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";
import {
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX_V1,
  MinAdaStep02DatumSchema,
  MinAdaStep03DatumSchema,
  MinAdaStep04DatumSchema,
  MinAdaStep05DatumSchema,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import {
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import { resolvePublishedProofChunksV1 } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  createMinAdaCompleteCanonicalReplayFromHistoricalCorpusV1,
  requireCompleteCanonicalReplayDecisionV1,
} from "../workflow/complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifierV1,
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "../workflow/family-l1-observation-v1.js";
import type { FraudProofWorkflowJournalStoreV1 } from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowRunResultV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runFraudProofWorkflowV1,
} from "../workflow/orchestrator-v1.js";
import {
  createProductionCursorFamilyWorkflowAdapterV1,
  PRODUCTION_CURSOR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionCursorFamilyTransactionPortV1,
} from "../workflow/production-cursor-family-adapter-v1.js";
import {
  captureProductionCursorRemovalV1,
  productionCursorFamilyActionInputV1,
  productionCursorStringFieldV1,
} from "../workflow/production-cursor-family-runtime-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePortV1,
  type ProductionFieldCarriageRequirementV1,
  withProductionFieldCarriagePrerequisiteV1,
} from "../workflow/production-field-carriage-prerequisite-v1.js";
import {
  type ProductionHistoricalNativeScriptCheckpointStoreV1,
  type ProductionHistoricalNativeScriptCorpusV1,
  type ProductionHistoricalNativeScriptHistorySourceV1,
  requireProductionHistoricalNativeScriptHistoryAuthorityV1,
  resolveProductionHistoricalNativeScriptCorpusV1,
} from "../workflow/production-historical-native-script-corpus-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePortV1,
  withProductionProofChunkPrerequisiteV1,
} from "../workflow/production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "../workflow/release-finality-policy-v1.js";
import { captureLocallyEvaluatedTransactionV1 } from "../workflow/transaction-boundary-v1.js";
import type { MinAdaContractsV1 } from "./contracts-v1.js";
import type { PreparedMinAdaTxV1 } from "./prepare-v1.js";
import {
  admitProductionMinAdaArtifactV1,
  type AdmittedProductionMinAdaArtifactV1,
  prepareProductionMinAdaArtifactV1,
  type ProductionMinAdaTxArtifactV1,
} from "./production-artifact-v1.js";
import { submitMinAdaInit } from "./submit-init-v1.js";
import {
  submitMinAdaTxStep01,
  submitMinAdaUtxoStep01,
} from "./submit-step-01-v1.js";
import {
  submitMinAdaTxStep02,
  submitMinAdaUtxoStep02,
} from "./submit-step-02-v1.js";
import { submitMinAdaUtxoStep03 } from "./submit-step-03-v1.js";
import { submitMinAdaUtxoStep04 } from "./submit-step-04-v1.js";
import { submitMinAdaStep05 } from "./submit-step-05-v1.js";
import { MIN_ADA_CURSOR_SPEC_V1 } from "./workflow-spec-v1.js";

export type MinAdaWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfigV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"minAda">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MinAdaContractsV1;
  references: MinAdaWorkflowReferenceScriptsV1;
  historicalCorpus(): ProductionHistoricalNativeScriptCorpusV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

type AdmittedTxV1 = Readonly<{
  artifact: ProductionMinAdaTxArtifactV1;
  prepared: PreparedMinAdaTxV1;
}>;
const isTx = (
  admitted: AdmittedProductionMinAdaArtifactV1,
): admitted is AdmittedTxV1 => admitted.artifact.kind === "min-ada-tx";

const witnessSet = (admitted: AdmittedTxV1) => {
  const compact = deriveMidgardNativeTxWitnessSetCompactV1(
    decodeMidgardNativeTxFullV1FromCanonicalCbor(
      Buffer.from(admitted.prepared.nativeTxCanonicalCbor, "hex"),
    ).witnessSet,
  );
  return {
    addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
      "hex",
    ),
  };
};

const txFieldPlan = (admitted: AdmittedTxV1, owner: string) =>
  planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.outputs,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: admitted.prepared.outputItemCbors.map((item) =>
      Buffer.from(item, "hex"),
    ),
    owner,
    publish: true,
    witnessSet: witnessSet(admitted),
    label: "min-ada transaction field 2",
  });

const resolveField = async ({
  config,
  admitted,
}: {
  readonly config: BoundConfigV1;
  readonly admitted: AdmittedTxV1;
}) => {
  const planned = txFieldPlan(admitted, config.signer.paymentKeyHash);
  const publications = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("min-ada field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
    lucid: config.lucid,
    network: config.binding.network,
    planned,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("min-ada field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const transactionPort = (
  config: BoundConfigV1,
): ProductionCursorFamilyTransactionPortV1<"minAda"> => ({
  portVersion: PRODUCTION_CURSOR_FAMILY_TRANSACTION_PORT_V1,
  category: "minAda",
  prepare: async ({ evidence, classification }) =>
    await prepareProductionMinAdaArtifactV1({
      evidence,
      historicalNativeScriptCorpus: config.historicalCorpus(),
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitProductionMinAdaArtifactV1(artifact);
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash) {
      throw new Error("min-ada artifact changed the bound header");
    }
    const input = productionCursorFamilyActionInputV1({
      category: "minAda",
      action,
    });
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const threadOutRef = () =>
      productionCursorStringFieldV1(input, "threadOutRef");
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMinAdaInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudulentBlockOutRef: productionCursorStringFieldV1(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: admitted.artifact.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      const chunks = isTx(admitted)
        ? await resolvePublishedProofChunksV1({
            lucid: config.lucid,
            address: config.signer.address,
            proofCbor: admitted.prepared.txInclusion.txMembershipProofCbor,
          })
        : [];
      if (chunks === undefined) {
        throw new Error("min-ada transaction proof disappeared");
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            const shared = {
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              stateQueueBlockOutRef: productionCursorStringFieldV1(
                input,
                "stateQueueBlockOutRef",
              ),
              prepared: admitted.prepared,
              referenceScriptUtxo: config.references.steps[0],
              preSubmitBoundary,
              awaitConfirmation: false,
            } as const;
            if (isTx(admitted)) {
              await submitMinAdaTxStep01({
                ...shared,
                blueprint: config.binding.blueprint,
                prepared: admitted.prepared,
                publishedProofChunks: chunks,
                witnessReferenceScripts: config.references.witnesses,
              });
            } else {
              await submitMinAdaUtxoStep01({
                ...shared,
                prepared: admitted.prepared,
              });
            }
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      if (isTx(admitted)) {
        const carriage = await resolveField({ config, admitted });
        return Object.freeze({
          transaction: await captureLocallyEvaluatedTransactionV1(
            async (preSubmitBoundary) => {
              await submitMinAdaTxStep02({
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId,
                signer: config.signer,
                threadOutRef: threadOutRef(),
                prepared: admitted.prepared,
                publishedCarriageUtxos: carriage.publications,
                ...(carriage.certificate === undefined
                  ? {}
                  : { certificateUtxo: carriage.certificate }),
                referenceScriptUtxo: config.references.steps[1],
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            },
          ),
        });
      }
      const chunks = await resolvePublishedProofChunksV1({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.postMembershipProofCbor,
      });
      if (chunks === undefined) {
        throw new Error("min-ada post-membership proof disappeared");
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMinAdaUtxoStep02({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              prepared: admitted.prepared,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.references.steps[1],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03" && !isTx(admitted)) {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMinAdaUtxoStep03({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              coinsPerUtxoByte: BigInt(
                config.binding.cardanoProtocolParameters.coinsPerUtxoByte,
              ),
              referenceScriptUtxo: config.references.steps[2],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04" && !isTx(admitted)) {
      const chunks = await resolvePublishedProofChunksV1({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.predecessorNonMembershipProofCbor,
      });
      if (chunks === undefined) {
        throw new Error("min-ada predecessor proof disappeared");
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMinAdaUtxoStep04({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              predecessorNonMembershipProofCbor:
                admitted.prepared.predecessorNonMembershipProofCbor,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.references.steps[3],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_05") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMinAdaStep05({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              referenceScriptUtxo: config.references.steps[4],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureProductionCursorRemovalV1({
        category: "minAda",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: admitted.artifact.headerHash,
        input,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    }
    throw new Error(
      `min-ada ${admitted.artifact.kind} cannot execute ${input.stage}`,
    );
  },
});

export type ManifestBoundMinAdaWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MinAdaWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  historicalNativeScriptCheckpointStore: ProductionHistoricalNativeScriptCheckpointStoreV1;
  historicalNativeScriptHistorySource: ProductionHistoricalNativeScriptHistorySourceV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMinAdaWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"minAda">;
  l1: FraudProofFamilyL1ObservationPortV1<"minAda">;
  transactions: ProductionCursorFamilyTransactionPortV1<"minAda">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
  historicalNativeScriptCheckpointStore: ProductionHistoricalNativeScriptCheckpointStoreV1;
  historicalNativeScriptHistorySource: ProductionHistoricalNativeScriptHistorySourceV1;
}>;

type HistoricalCorpusCellV1 = {
  value?: ProductionHistoricalNativeScriptCorpusV1;
};

const historicalCorpusCells = new WeakMap<object, HistoricalCorpusCellV1>();

export const createManifestBoundMinAdaWorkflowV1 = async (
  config: ManifestBoundMinAdaWorkflowConfigV1,
): Promise<ManifestBoundMinAdaWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "minAda",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      MinAdaStep02DatumSchema,
      MinAdaStep03DatumSchema,
      MinAdaStep04DatumSchema,
      MinAdaStep05DatumSchema,
    ],
  });
  requireProductionHistoricalNativeScriptHistoryAuthorityV1({
    deploymentFingerprint: binding.deploymentFingerprint,
    checkpointStore: config.historicalNativeScriptCheckpointStore,
    historySource: config.historicalNativeScriptHistorySource,
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.minAda;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    certificate === null
  ) {
    throw new Error("min-ada manifest omitted required contracts");
  }
  const stepNames = [
    "fraudProofMinAda",
    "fraudProofMinAdaStep02",
    "fraudProofMinAdaStep03",
    "fraudProofMinAdaStep04",
    "fraudProofMinAdaStep05",
  ] as const;
  const steps = stepNames.map((contractName, index) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as MinAdaWorkflowReferenceScriptsV1["steps"];
  const witness = <Name extends keyof FaultProofWitnessReferenceScriptsV1>(
    name: Name,
    contractName: string,
  ) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo: config.referenceScripts.witnesses[name],
    });
  const references: MinAdaWorkflowReferenceScriptsV1 = Object.freeze({
    steps: Object.freeze(steps),
    witnesses: Object.freeze({
      computationThreadMint: witness(
        "computationThreadMint",
        "computationThreadMint",
      ),
      fraudProofMint: witness("fraudProofMint", "fraudProofMint"),
      phasMembershipWithdraw: witness(
        "phasMembershipWithdraw",
        "phasMembershipWithdraw",
      ),
      chunkedVerifyWithdraw: witness(
        "chunkedVerifyWithdraw",
        "chunkedVerifyWithdraw",
      ),
      pexcludesWithdraw: witness("pexcludesWithdraw", "pexcludesWithdraw"),
    }),
    fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName: "fieldPreimageCertificateMint",
      utxo: config.referenceScripts.fieldPreimageCertificateMint,
    }),
  });
  const contracts: MinAdaContractsV1 = Object.freeze({
    steps: chain.steps,
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
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.rawL1 === undefined)
    throw new Error("min-ada raw L1 authority is unavailable");
  const corpusCell: HistoricalCorpusCellV1 = {};
  const bound: BoundConfigV1 = {
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    historicalCorpus: () => {
      const corpus = corpusCell.value;
      if (corpus === undefined) {
        throw new Error(
          "min-ada history was not derived from this workflow's public authority",
        );
      }
      return corpus;
    },
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  };
  const transactions = transactionPort(bound);
  let adapter = createProductionCursorFamilyWorkflowAdapterV1({
    spec: MIN_ADA_CURSOR_SPEC_V1,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePortV1({
    category: "minAda",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      if (action.input.stage !== "step_02") return null;
      const admitted = admitProductionMinAdaArtifactV1(artifact);
      if (!isTx(admitted)) return null;
      return {
        planned: txFieldPlan(admitted, config.signer.paymentKeyHash),
        compactCbor: admitted.prepared.nativeTxCompactCbor,
        certificate: {
          policyId: certificate.policyId,
          mintingScript: certificate.mintingScript,
          referenceScriptUtxo: references.fieldPreimageCertificateMint,
        },
      } satisfies ProductionFieldCarriageRequirementV1;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProductionFieldCarriagePrerequisiteV1({
    category: "minAda",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePortV1({
    category: "minAda",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) => {
      const admitted = admitProductionMinAdaArtifactV1(artifact);
      return action.input.stage === "step_01" && isTx(admitted)
        ? admitted.prepared.txInclusion.txMembershipProofCbor
        : action.input.stage === "step_02" && !isTx(admitted)
          ? admitted.prepared.postMembershipProofCbor
          : action.input.stage === "step_04" && !isTx(admitted)
            ? admitted.prepared.predecessorNonMembershipProofCbor
            : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProductionProofChunkPrerequisiteV1({
    category: "minAda",
    base: adapter,
    prerequisite: proofPrerequisite,
  });
  const workflow = Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier:
      createFraudProofFamilyAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
    historicalNativeScriptCheckpointStore:
      config.historicalNativeScriptCheckpointStore,
    historicalNativeScriptHistorySource:
      config.historicalNativeScriptHistorySource,
  });
  historicalCorpusCells.set(workflow, corpusCell);
  return workflow;
};

export const runOrResumeManifestBoundMinAdaWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMinAdaWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  const evidence = await fetchCanonicalBlockEvidenceV1({
    observation,
    sources,
    minimumConfirmationDepth:
      workflow.binding.releaseFinality.policy.confirmationDepth,
  });
  const corpus = await resolveProductionHistoricalNativeScriptCorpusV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    checkpointStore: workflow.historicalNativeScriptCheckpointStore,
    historySource: workflow.historicalNativeScriptHistorySource,
    currentEvidence: evidence,
    sources,
  });
  const cell = historicalCorpusCells.get(workflow);
  if (cell === undefined) {
    throw new Error(
      "min-ada workflow was not created by its manifest-bound constructor",
    );
  }
  if (
    cell.value !== undefined &&
    cell.value.corpusDigest !== corpus.corpusDigest
  ) {
    throw new Error("min-ada authenticated history changed across resume");
  }
  cell.value = corpus;
  const replayer =
    createMinAdaCompleteCanonicalReplayFromHistoricalCorpusV1(corpus);
  const decision = await replayer.replay(evidence);
  const detections = requireCompleteCanonicalReplayDecisionV1({
    evidence,
    replayer,
    decision,
  });
  return await runFraudProofWorkflowV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    evidence,
    detections,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["minAda"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
