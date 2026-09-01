import {
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";
import {
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX_V1,
  type MidgardTxInput,
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT_V1,
  MissingNativeScriptTxStep07Datum,
  MissingNativeScriptTxStep08Datum,
  MissingNativeScriptUtxoStep02DatumSchema,
  MissingNativeScriptUtxoStep03DatumSchema,
  MissingNativeScriptUtxoStep04DatumSchema,
  MissingNativeScriptUtxoStep05DatumSchema,
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
import { parseSubmitStep01TxInclusion } from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  createMissingNativeScriptUtxoCompleteCanonicalReplayV1,
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
import type { MissingNativeScriptUtxoContractsV1 } from "./contracts-v1.js";
import {
  admitProductionMissingNativeScriptUtxoArtifactV1,
  prepareProductionMissingNativeScriptUtxoArtifactV1,
} from "./production-artifact-v1.js";
import { submitMissingNativeScriptUtxoInit } from "./submit-init-v1.js";
import { submitMissingNativeScriptUtxoStep01 } from "./submit-step-01-v1.js";
import { submitMissingNativeScriptUtxoStep02 } from "./submit-step-02-v1.js";
import { submitMissingNativeScriptUtxoStep03 } from "./submit-step-03-v1.js";
import { submitMissingNativeScriptUtxoStep04 } from "./submit-step-04-v1.js";
import { submitMissingNativeScriptUtxoStep05 } from "./submit-step-05-v1.js";
import {
  submitMissingNativeScriptUtxoStep05StartGrammarV1,
  submitMissingNativeScriptUtxoStep06V1,
} from "./submit-step-06-v1.js";
import { submitMissingNativeScriptUtxoStep07V1 } from "./submit-step-07-v1.js";
import { MISSING_NATIVE_SCRIPT_UTXO_CURSOR_SPEC_V1 } from "./workflow-spec-v1.js";

export type MissingNativeScriptUtxoWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfigV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"missingNativeScriptUtxo">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MissingNativeScriptUtxoContractsV1;
  references: MissingNativeScriptUtxoWorkflowReferenceScriptsV1;
  historicalCorpus(): ProductionHistoricalNativeScriptCorpusV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

type Admitted = ReturnType<
  typeof admitProductionMissingNativeScriptUtxoArtifactV1
>;

const bytes = (values: readonly string[]): readonly Uint8Array[] =>
  values.map((value) => Buffer.from(value, "hex"));

const witnessSet = (admitted: Admitted) => {
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

const spendInputs = (admitted: Admitted): readonly MidgardTxInput[] =>
  admitted.artifact.spendInputs.map((input) => ({
    tx_id: input.tx_id,
    output_index: BigInt(input.output_index),
  }));

const spendFieldPlan = (admitted: Admitted, owner: string) =>
  planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.spendInputs,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: bytes(admitted.prepared.spendInputItemCbors),
    owner,
    publish: true,
    label: "missing-native-script-utxo field 0",
  });

const scriptFieldPlan = (admitted: Admitted, owner: string) =>
  planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.scriptWitnesses,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: bytes(admitted.prepared.scriptWitnessItemCbors),
    owner,
    publish: true,
    witnessSet: witnessSet(admitted),
    anchorWitnessSetHash:
      admitted.prepared.txInclusion.nativeTx.witness_set_hash,
    label: "missing-native-script-utxo field 6",
  });

const direct = (admitted: Admitted): boolean =>
  admitted.prepared.scriptWitnessItemCbors.length <=
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT_V1;

const resolveField = async ({
  config,
  planned,
}: {
  readonly config: BoundConfigV1;
  readonly planned: ReturnType<typeof spendFieldPlan>;
}) => {
  const publications = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error(
      "missing-native-script-utxo field publications disappeared",
    );
  }
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
    lucid: config.lucid,
    network: config.binding.network,
    planned,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("missing-native-script-utxo certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const transactionPort = (
  config: BoundConfigV1,
): ProductionCursorFamilyTransactionPortV1<"missingNativeScriptUtxo"> => ({
  portVersion: PRODUCTION_CURSOR_FAMILY_TRANSACTION_PORT_V1,
  category: "missingNativeScriptUtxo",
  prepare: async ({ evidence, classification }) =>
    await prepareProductionMissingNativeScriptUtxoArtifactV1({
      evidence,
      historicalNativeScriptCorpus: config.historicalCorpus(),
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitProductionMissingNativeScriptUtxoArtifactV1(artifact);
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash) {
      throw new Error(
        "missing-native-script-utxo artifact changed the bound header",
      );
    }
    const input = productionCursorFamilyActionInputV1({
      category: "missingNativeScriptUtxo",
      action,
    });
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const threadOutRef = () =>
      productionCursorStringFieldV1(input, "threadOutRef");
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoInit({
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
      const chunks = await resolvePublishedProofChunksV1({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.txInclusion.txMembershipProofCbor,
      });
      if (chunks === undefined)
        throw new Error("missing-native-script-utxo tx proof disappeared");
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep01({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              stateQueueBlockOutRef: productionCursorStringFieldV1(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: parseSubmitStep01TxInclusion(
                admitted.prepared.txInclusion,
              ),
              prevUtxosRoot: admitted.prepared.prevUtxosRoot,
              referenceScriptUtxo: config.references.steps[0],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      const carriage = await resolveField({
        config,
        planned: spendFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
              spendInputs: spendInputs(admitted),
              badInputIndex: admitted.prepared.badInputIndex,
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
    if (input.stage === "step_03") {
      const chunks = await resolvePublishedProofChunksV1({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.membershipProofCbor,
      });
      if (chunks === undefined)
        throw new Error(
          "missing-native-script-utxo membership proof disappeared",
        );
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep03({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              prepared: admitted.prepared,
              referenceScriptUtxo: config.references.steps[2],
              publishedProofChunks: chunks,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep04({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              missingNativeScriptBytes:
                admitted.prepared.missingNativeScriptBytes,
              referenceScriptUtxo: config.references.steps[3],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_05") {
      const carriage = await resolveField({
        config,
        planned: scriptFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            const shared = {
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
              witnessSet: witnessSet(admitted),
              scriptTxWitsItems: bytes(
                admitted.prepared.scriptWitnessItemCbors,
              ),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[4],
              preSubmitBoundary,
              awaitConfirmation: false,
            } as const;
            if (direct(admitted)) {
              await submitMissingNativeScriptUtxoStep05({
                ...shared,
                scriptWitnessItems: shared.scriptTxWitsItems,
                witnessReferenceScripts: config.references.witnesses,
              });
            } else {
              await submitMissingNativeScriptUtxoStep05StartGrammarV1(shared);
            }
          },
        ),
      });
    }
    if (input.stage === "step_06") {
      const carriage = await resolveField({
        config,
        planned: scriptFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep06V1({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
              witnessSet: witnessSet(admitted),
              scriptTxWitsItems: bytes(
                admitted.prepared.scriptWitnessItemCbors,
              ),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[5],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_07") {
      const carriage = await resolveField({
        config,
        planned: scriptFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep07V1({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
              witnessSet: witnessSet(admitted),
              scriptTxWitsItems: bytes(
                admitted.prepared.scriptWitnessItemCbors,
              ),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[6],
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
        category: "missingNativeScriptUtxo",
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
      `missing-native-script-utxo unsupported stage ${input.stage}`,
    );
  },
});

export type ManifestBoundMissingNativeScriptUtxoWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MissingNativeScriptUtxoWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  historicalNativeScriptCheckpointStore: ProductionHistoricalNativeScriptCheckpointStoreV1;
  historicalNativeScriptHistorySource: ProductionHistoricalNativeScriptHistorySourceV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMissingNativeScriptUtxoWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"missingNativeScriptUtxo">;
  l1: FraudProofFamilyL1ObservationPortV1<"missingNativeScriptUtxo">;
  transactions: ProductionCursorFamilyTransactionPortV1<"missingNativeScriptUtxo">;
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

export const createManifestBoundMissingNativeScriptUtxoWorkflowV1 = async (
  config: ManifestBoundMissingNativeScriptUtxoWorkflowConfigV1,
): Promise<ManifestBoundMissingNativeScriptUtxoWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "missingNativeScriptUtxo",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      MissingNativeScriptUtxoStep02DatumSchema,
      MissingNativeScriptUtxoStep03DatumSchema,
      MissingNativeScriptUtxoStep04DatumSchema,
      MissingNativeScriptUtxoStep05DatumSchema,
      MissingNativeScriptTxStep07Datum,
      MissingNativeScriptTxStep08Datum,
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
  const chain = binding.resolvedContracts.contracts.missingNativeScriptUtxo;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    certificate === null
  ) {
    throw new Error(
      "missing-native-script-utxo manifest omitted required contracts",
    );
  }
  const stepNames = [
    "fraudProofMissingNativeScriptUtxo",
    "fraudProofMissingNativeScriptUtxoStep02",
    "fraudProofMissingNativeScriptUtxoStep03",
    "fraudProofMissingNativeScriptUtxoStep04",
    "fraudProofMissingNativeScriptUtxoStep05",
    "fraudProofMissingNativeScriptUtxoStep06",
    "fraudProofMissingNativeScriptUtxoStep07",
  ] as const;
  const steps = stepNames.map((contractName, index) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as MissingNativeScriptUtxoWorkflowReferenceScriptsV1["steps"];
  const witness = <Name extends keyof FaultProofWitnessReferenceScriptsV1>(
    name: Name,
    contractName: string,
  ) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo: config.referenceScripts.witnesses[name],
    });
  const references: MissingNativeScriptUtxoWorkflowReferenceScriptsV1 =
    Object.freeze({
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
  const contracts: MissingNativeScriptUtxoContractsV1 = Object.freeze({
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
    throw new Error(
      "missing-native-script-utxo raw L1 authority is unavailable",
    );
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
          "missing-native-script-utxo history was not derived from this workflow's public DA sources",
        );
      }
      return corpus;
    },
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  };
  const transactions = transactionPort(bound);
  let adapter = createProductionCursorFamilyWorkflowAdapterV1({
    spec: MISSING_NATIVE_SCRIPT_UTXO_CURSOR_SPEC_V1,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePortV1({
    category: "missingNativeScriptUtxo",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      const admitted =
        admitProductionMissingNativeScriptUtxoArtifactV1(artifact);
      const planned =
        action.input.stage === "step_02"
          ? spendFieldPlan(admitted, config.signer.paymentKeyHash)
          : typeof action.input.stage === "string" &&
              ["step_05", "step_06", "step_07"].includes(action.input.stage)
            ? scriptFieldPlan(admitted, config.signer.paymentKeyHash)
            : null;
      if (planned === null) return null;
      return {
        planned,
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
    category: "missingNativeScriptUtxo",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePortV1({
    category: "missingNativeScriptUtxo",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) => {
      const admitted =
        admitProductionMissingNativeScriptUtxoArtifactV1(artifact);
      return action.input.stage === "step_01"
        ? admitted.prepared.txInclusion.txMembershipProofCbor
        : action.input.stage === "step_03"
          ? admitted.prepared.membershipProofCbor
          : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProductionProofChunkPrerequisiteV1({
    category: "missingNativeScriptUtxo",
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

export const runOrResumeManifestBoundMissingNativeScriptUtxoWorkflowV1 =
  async ({
    workflow,
    sources,
    journal,
  }: {
    readonly workflow: ManifestBoundMissingNativeScriptUtxoWorkflowV1;
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
        "missing-native-script-utxo workflow was not created by the manifest-bound constructor",
      );
    }
    if (
      cell.value !== undefined &&
      cell.value.corpusDigest !== corpus.corpusDigest
    ) {
      throw new Error(
        "missing-native-script-utxo authenticated history changed across resume",
      );
    }
    cell.value = corpus;
    const replayer =
      createMissingNativeScriptUtxoCompleteCanonicalReplayV1(corpus);
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
        launchScope: ["missingNativeScriptUtxo"],
      }),
      journal,
      terminalVerifier: workflow.terminalVerifier,
      releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    });
  };
