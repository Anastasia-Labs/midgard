import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX,
  type MidgardTxInput,
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT,
  MissingNativeScriptTxStep07Datum,
  MissingNativeScriptTxStep08Datum,
  MissingNativeScriptUtxoStep02DatumSchema,
  MissingNativeScriptUtxoStep03DatumSchema,
  MissingNativeScriptUtxoStep04DatumSchema,
  MissingNativeScriptUtxoStep05DatumSchema,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import {
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening-v1.js";
import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { parseSubmitStep01TxInclusion } from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import {
  createMissingNativeScriptUtxoCompleteCanonicalReplay,
  requireCompleteCanonicalReplayDecision,
} from "../workflow/complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation-v1.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflow,
} from "../workflow/orchestrator-v1.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/production-cursor-family-adapter-v1.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/production-cursor-family-runtime-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
  withFieldCarriagePrerequisite,
} from "../workflow/production-field-carriage-prerequisite-v1.js";
import {
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptCorpus,
  type HistoricalNativeScriptHistorySource,
  requireHistoricalNativeScriptHistoryAuthority,
  resolveHistoricalNativeScriptCorpus,
} from "../workflow/production-historical-native-script-corpus-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  withProofChunkPrerequisite,
} from "../workflow/production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy-v1.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary-v1.js";
import type { MissingNativeScriptUtxoContracts } from "./contracts-v1.js";
import {
  admitMissingNativeScriptUtxoArtifact,
  prepareMissingNativeScriptUtxoArtifact,
} from "./production-artifact-v1.js";
import { submitMissingNativeScriptUtxoInit } from "./submit-init-v1.js";
import { submitMissingNativeScriptUtxoStep01 } from "./submit-step-01-v1.js";
import { submitMissingNativeScriptUtxoStep02 } from "./submit-step-02-v1.js";
import { submitMissingNativeScriptUtxoStep03 } from "./submit-step-03-v1.js";
import { submitMissingNativeScriptUtxoStep04 } from "./submit-step-04-v1.js";
import { submitMissingNativeScriptUtxoStep05 } from "./submit-step-05-v1.js";
import {
  submitMissingNativeScriptUtxoStep05StartGrammar,
  submitMissingNativeScriptUtxoStep06,
} from "./submit-step-06-v1.js";
import { submitMissingNativeScriptUtxoStep07 } from "./submit-step-07-v1.js";
import { MISSING_NATIVE_SCRIPT_UTXO_CURSOR_SPEC } from "./workflow-spec-v1.js";

export type MissingNativeScriptUtxoWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"missingNativeScriptUtxo">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MissingNativeScriptUtxoContracts;
  references: MissingNativeScriptUtxoWorkflowReferenceScripts;
  historicalCorpus(): HistoricalNativeScriptCorpus;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

type Admitted = ReturnType<typeof admitMissingNativeScriptUtxoArtifact>;

const bytes = (values: readonly string[]): readonly Uint8Array[] =>
  values.map((value) => Buffer.from(value, "hex"));

const witnessSet = (admitted: Admitted) => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    decodeMidgardNativeTxFullFromCanonicalCbor(
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
  planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: bytes(admitted.prepared.spendInputItemCbors),
    owner,
    publish: true,
    label: "missing-native-script-utxo field 0",
  });

const scriptFieldPlan = (admitted: Admitted, owner: string) =>
  planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
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
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT;

const resolveField = async ({
  config,
  planned,
}: {
  readonly config: BoundConfig;
  readonly planned: ReturnType<typeof spendFieldPlan>;
}) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error(
      "missing-native-script-utxo field publications disappeared",
    );
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
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
  config: BoundConfig,
): CursorFamilyTransactionPort<"missingNativeScriptUtxo"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "missingNativeScriptUtxo",
  prepare: async ({ evidence, classification }) =>
    await prepareMissingNativeScriptUtxoArtifact({
      evidence,
      historicalNativeScriptCorpus: config.historicalCorpus(),
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitMissingNativeScriptUtxoArtifact(artifact);
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash) {
      throw new Error(
        "missing-native-script-utxo artifact changed the bound header",
      );
    }
    const input = cursorFamilyActionInput({
      category: "missingNativeScriptUtxo",
      action,
    });
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const threadOutRef = () => cursorStringField(input, "threadOutRef");
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudulentBlockOutRef: cursorStringField(
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
      const chunks = await resolvePublishedProofChunks({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.txInclusion.txMembershipProofCbor,
      });
      if (chunks === undefined)
        throw new Error("missing-native-script-utxo tx proof disappeared");
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep01({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              stateQueueBlockOutRef: cursorStringField(
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
        transaction: await captureLocallyEvaluatedTransaction(
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
      const chunks = await resolvePublishedProofChunks({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.membershipProofCbor,
      });
      if (chunks === undefined)
        throw new Error(
          "missing-native-script-utxo membership proof disappeared",
        );
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
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
        transaction: await captureLocallyEvaluatedTransaction(
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
        transaction: await captureLocallyEvaluatedTransaction(
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
              await submitMissingNativeScriptUtxoStep05StartGrammar(shared);
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
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep06({
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
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep07({
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
      return await captureCursorRemoval({
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

export type ManifestBoundMissingNativeScriptUtxoWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MissingNativeScriptUtxoWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMissingNativeScriptUtxoWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"missingNativeScriptUtxo">;
  l1: FraudProofFamilyL1ObservationPort<"missingNativeScriptUtxo">;
  transactions: CursorFamilyTransactionPort<"missingNativeScriptUtxo">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
}>;

type HistoricalCorpusCell = {
  value?: HistoricalNativeScriptCorpus;
};

const historicalCorpusCells = new WeakMap<object, HistoricalCorpusCell>();

export const createManifestBoundMissingNativeScriptUtxoWorkflow = async (
  config: ManifestBoundMissingNativeScriptUtxoWorkflowConfig,
): Promise<ManifestBoundMissingNativeScriptUtxoWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
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
  requireHistoricalNativeScriptHistoryAuthority({
    deploymentFingerprint: binding.deploymentFingerprint,
    checkpointStore: config.historicalNativeScriptCheckpointStore,
    historySource: config.historicalNativeScriptHistorySource,
  });
  assertManifestBoundWorkflowSigner({
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
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as MissingNativeScriptUtxoWorkflowReferenceScripts["steps"];
  const witness = <Name extends keyof FaultProofWitnessReferenceScripts>(
    name: Name,
    contractName: string,
  ) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.witnesses[name],
    });
  const references: MissingNativeScriptUtxoWorkflowReferenceScripts =
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
      fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fieldPreimageCertificateMint",
        utxo: config.referenceScripts.fieldPreimageCertificateMint,
      }),
    });
  const contracts: MissingNativeScriptUtxoContracts = Object.freeze({
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
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.rawL1 === undefined)
    throw new Error(
      "missing-native-script-utxo raw L1 authority is unavailable",
    );
  const corpusCell: HistoricalCorpusCell = {};
  const bound: BoundConfig = {
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
  let adapter = createCursorFamilyWorkflowAdapter({
    spec: MISSING_NATIVE_SCRIPT_UTXO_CURSOR_SPEC,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePort({
    category: "missingNativeScriptUtxo",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      const admitted = admitMissingNativeScriptUtxoArtifact(artifact);
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
      } satisfies FieldCarriageRequirement;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withFieldCarriagePrerequisite({
    category: "missingNativeScriptUtxo",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePort({
    category: "missingNativeScriptUtxo",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) => {
      const admitted = admitMissingNativeScriptUtxoArtifact(artifact);
      return action.input.stage === "step_01"
        ? admitted.prepared.txInclusion.txMembershipProofCbor
        : action.input.stage === "step_03"
          ? admitted.prepared.membershipProofCbor
          : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProofChunkPrerequisite({
    category: "missingNativeScriptUtxo",
    base: adapter,
    prerequisite: proofPrerequisite,
  });
  const workflow = Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
    historicalNativeScriptCheckpointStore:
      config.historicalNativeScriptCheckpointStore,
    historicalNativeScriptHistorySource:
      config.historicalNativeScriptHistorySource,
  });
  historicalCorpusCells.set(workflow, corpusCell);
  return workflow;
};

export const runOrResumeManifestBoundMissingNativeScriptUtxoWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMissingNativeScriptUtxoWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  const evidence = await fetchCanonicalBlockEvidence({
    observation,
    sources,
    minimumConfirmationDepth:
      workflow.binding.releaseFinality.policy.confirmationDepth,
  });
  const corpus = await resolveHistoricalNativeScriptCorpus({
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
  const replayer = createMissingNativeScriptUtxoCompleteCanonicalReplay(corpus);
  const decision = await replayer.replay(evidence);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer,
    decision,
  });
  return await runFraudProofWorkflow({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    evidence,
    detections,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["missingNativeScriptUtxo"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
