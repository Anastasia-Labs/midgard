import {
  adjudicateMidgardNativeTxFullV1Validity,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  encodeMidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core";
import {
  type ForcedInclusionTxV1,
  type FraudProofCatalogueCategoryName,
  FraudProofComputationThreadStepDatum,
  type HeaderV1,
  type OutputReference,
  OutputReferenceSchema,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED_V1,
  PROOF_THREAD_SOURCE_KIND_FORCED_V1,
  RejectionReasonV1Schema,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { submitCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import {
  type CanonicalBlockEvidenceV1,
  fetchCanonicalBlockEvidenceV1,
} from "../evidence/canonical-block-evidence-v1.js";
import { requireLinearFaultThreadUtxoV1 } from "../linear-fault-family-v1.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItemV1,
} from "../prepare-double-spend.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { submitRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  parseSubmitStep01TxInclusion,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "../workflow/family-l1-observation-v1.js";
import {
  DirectoryFraudProofWorkflowJournalStoreV1,
  type FraudProofWorkflowJournalStoreV1,
} from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  assertProductionWorkflowJournalActuationV1,
  bindProductionWorkflowActuationJournalV1,
} from "../workflow/production-actuation-permit-v1.js";
import {
  PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
  type ProductionWorkflowAdapterReadinessInputV1,
  type ProductionWorkflowAdapterRunnerV1,
} from "../workflow/production-adapters-v1.js";
import { bindProductionWorkflowFundingReservationJournalV1 } from "../workflow/production-funding-reservation-permit-v1.js";
import {
  type ProductionHistoricalNativeScriptCheckpointStoreV1,
  type ProductionHistoricalNativeScriptHistorySourceV1,
  resolveProductionHistoricalNativeScriptCorpusV1,
} from "../workflow/production-historical-native-script-corpus-v1.js";
import { createResolvedOutputNonCanonicalCentralJournalAdapterV1 } from "./central-journal-v1.js";
import type { ResolvedOutputNonCanonicalContractsV1 } from "./contracts-v1.js";
import {
  deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpusV1,
  detectResolvedOutputNonCanonicalCompleteReplayV1,
  resolvedOutputEvidenceIdentityV1,
  type ResolvedOutputEvidenceV1,
} from "./resolved-output-non-canonical-v1.js";
import {
  ResolvedOutputStep02DatumV1Schema,
  ResolvedOutputStep03DatumV1Schema,
  ResolvedOutputStep04DatumV1Schema,
  ResolvedOutputStep05DatumV1Schema,
} from "./schemas-v1.js";
import { submitResolvedOutputNonCanonicalCancelV1 } from "./submit-cancel-v1.js";
import { submitResolvedOutputNonCanonicalStep01AcceptedV1 } from "./submit-step-01-accepted-v1.js";
import { submitResolvedOutputNonCanonicalStep01ForcedV1 } from "./submit-step-01-forced-v1.js";
import { submitResolvedOutputNonCanonicalStep02V1 } from "./submit-step-02-v1.js";
import { submitResolvedOutputNonCanonicalStep03V1 } from "./submit-step-03-v1.js";
import { submitResolvedOutputNonCanonicalStep04V1 } from "./submit-step-04-v1.js";
import { submitResolvedOutputNonCanonicalStep05V1 } from "./submit-step-05-v1.js";
import {
  nextResolvedOutputActionV1,
  type ResolvedOutputJournalV1,
  type ResolvedOutputStageV1,
} from "./workflow-v1.js";

export const RESOLVED_OUTPUT_NON_CANONICAL_PRODUCTION_WORKFLOW_V1 =
  "midgard-resolved-output-non-canonical-production-workflow-v1" as const;
export const RESOLVED_OUTPUT_NON_CANONICAL_VIOLATION_ID_V1 =
  "resolved-output-non-canonical" as const;

export const RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS_V1 =
  Object.freeze({
    step01: "fraudProofResolvedOutputNonCanonical",
    step02: "fraudProofResolvedOutputNonCanonicalStep02",
    step03: "fraudProofResolvedOutputNonCanonicalStep03",
    step04: "fraudProofResolvedOutputNonCanonicalStep04",
    step05: "fraudProofResolvedOutputNonCanonicalStep05",
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
  } as const);

export type ResolvedOutputNonCanonicalProductionReferenceScriptsV1 = Readonly<{
  step01: UTxO;
  step02: UTxO;
  step03: UTxO;
  step04: UTxO;
  step05: UTxO;
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

export type ManifestBoundResolvedOutputNonCanonicalConfigV1 = Readonly<{
  schemaVersion: typeof RESOLVED_OUTPUT_NON_CANONICAL_PRODUCTION_WORKFLOW_V1;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBindingV1<"resolvedOutputNonCanonical">;
  contracts: ResolvedOutputNonCanonicalContractsV1;
  referenceScripts: ResolvedOutputNonCanonicalProductionReferenceScriptsV1;
}>;

export type LoadManifestBoundResolvedOutputNonCanonicalConfigV1 = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: ResolvedOutputNonCanonicalProductionReferenceScriptsV1;
}>;

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: FraudProofWorkflowDeploymentBindingV1<"resolvedOutputNonCanonical">;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxoV1({ binding, contractName, utxo });

export const bindResolvedOutputNonCanonicalReferenceScriptsV1 = ({
  binding,
  referenceScripts,
}: {
  readonly binding: FraudProofWorkflowDeploymentBindingV1<"resolvedOutputNonCanonical">;
  readonly referenceScripts: ResolvedOutputNonCanonicalProductionReferenceScriptsV1;
}): ResolvedOutputNonCanonicalProductionReferenceScriptsV1 => {
  const names = RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS_V1;
  return Object.freeze({
    step01: bindReference({
      binding,
      contractName: names.step01,
      utxo: referenceScripts.step01,
    }),
    step02: bindReference({
      binding,
      contractName: names.step02,
      utxo: referenceScripts.step02,
    }),
    step03: bindReference({
      binding,
      contractName: names.step03,
      utxo: referenceScripts.step03,
    }),
    step04: bindReference({
      binding,
      contractName: names.step04,
      utxo: referenceScripts.step04,
    }),
    step05: bindReference({
      binding,
      contractName: names.step05,
      utxo: referenceScripts.step05,
    }),
    fieldPreimageCertificateMint: bindReference({
      binding,
      contractName: names.fieldPreimageCertificateMint,
      utxo: referenceScripts.fieldPreimageCertificateMint,
    }),
    witnesses: Object.freeze({
      ...referenceScripts.witnesses,
      computationThreadMint: bindReference({
        binding,
        contractName: names.computationThreadMint,
        utxo: referenceScripts.witnesses.computationThreadMint,
      }),
      fraudProofMint: bindReference({
        binding,
        contractName: names.fraudProofMint,
        utxo: referenceScripts.witnesses.fraudProofMint,
      }),
      phasMembershipWithdraw: bindReference({
        binding,
        contractName: names.phasMembershipWithdraw,
        utxo: referenceScripts.witnesses.phasMembershipWithdraw,
      }),
    }),
  });
};

export const loadManifestBoundResolvedOutputNonCanonicalConfigV1 = async (
  input: LoadManifestBoundResolvedOutputNonCanonicalConfigV1,
): Promise<ManifestBoundResolvedOutputNonCanonicalConfigV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "resolvedOutputNonCanonical",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      ResolvedOutputStep02DatumV1Schema,
      ResolvedOutputStep03DatumV1Schema,
      ResolvedOutputStep04DatumV1Schema,
      ResolvedOutputStep05DatumV1Schema,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });
  const localContracts = binding.resolvedContracts.contracts as unknown as {
    readonly resolvedOutputNonCanonical?: ResolvedOutputNonCanonicalContractsV1;
  };
  const chain = localContracts.resolvedOutputNonCanonical;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined || chain.steps.length !== 5) {
    throw new Error(
      "resolvedOutputNonCanonical deployment changed its five-step topology",
    );
  }
  if (certificate === null) {
    throw new Error(
      "resolvedOutputNonCanonical deployment omitted field-preimage certificate",
    );
  }
  const referenceScripts = bindResolvedOutputNonCanonicalReferenceScriptsV1({
    binding,
    referenceScripts: input.referenceScripts,
  });
  return Object.freeze({
    schemaVersion: RESOLVED_OUTPUT_NON_CANONICAL_PRODUCTION_WORKFLOW_V1,
    lucid: input.lucid,
    signer: input.signer,
    binding,
    contracts: {
      steps: chain.steps.map((step, index) => ({
        ...step,
        blueprintTitle: [
          "fraud_proofs/resolved_output_non_canonical/step_01.main.spend",
          "fraud_proofs/resolved_output_non_canonical/step_02.main.spend",
          "fraud_proofs/resolved_output_non_canonical/step_03.main.spend",
          "fraud_proofs/resolved_output_non_canonical/step_04.main.spend",
          "fraud_proofs/resolved_output_non_canonical/step_05.main.spend",
        ][index]!,
        referenceOutRef: [
          referenceScripts.step01,
          referenceScripts.step02,
          referenceScripts.step03,
          referenceScripts.step04,
          referenceScripts.step05,
        ][index]!.txHash.concat(
          "#",
          [
            referenceScripts.step01,
            referenceScripts.step02,
            referenceScripts.step03,
            referenceScripts.step04,
            referenceScripts.step05,
          ][index]!.outputIndex.toString(),
        ),
      })) as unknown as ResolvedOutputNonCanonicalContractsV1["steps"],
      computationThread: binding.resolvedContracts.contracts.computationThread,
      fraudProof: binding.resolvedContracts.contracts.fraudProof,
      hubOraclePolicyId: binding.deploymentInfo.hubOracleMint!.scriptHash,
      stateQueuePolicyId: binding.definition.stateQueue.policyId,
      fieldPreimageCertificatePolicyId: certificate.policyId,
      fieldPreimageCertificateMintingScript: certificate.mintingScript,
    },
    referenceScripts,
  });
};

export type ResolvedOutputNonCanonicalProductionStageV1 = Readonly<{
  fraudulentBlockOutRef: string;
  threadOutRef?: string;
  threadUtxo?: UTxO;
  threadToken?: Readonly<{ unit: string; fraudulentHeaderHash: string }>;
  stateQueueBlockOutRef?: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: HeaderV1;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
  nativeTxCompactCbor?: string;
  witnessSetCompactCbor?: string;
  publishedCarriageUtxos?: readonly UTxO[];
  certificateUtxo?: UTxO;
  validFrom?: bigint;
  validTo?: bigint;
}>;

/** Derives the only admissible family evidence from L1-bound public retained DA. */
export type ResolvedOutputNonCanonicalAuthenticatedSourceV1 = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: HeaderV1;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
}>;

/** Rebuilds all accepted/forced submitter material from the authenticated block. */
export const deriveResolvedOutputNonCanonicalAuthenticatedSourceV1 = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidenceV1;
  readonly evidence: ResolvedOutputEvidenceV1;
}): Promise<ResolvedOutputNonCanonicalAuthenticatedSourceV1> => {
  if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED_V1) {
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.subject.transaction_id,
    );
    if (selected === undefined) {
      throw new Error(
        "resolvedOutputNonCanonical accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItemV1));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "resolvedOutputNonCanonical accepted source trie differs from authenticated reconstruction",
      );
    }
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      Buffer.from(selected.txCbor, "hex"),
    );
    return Object.freeze({
      nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
      witnessSetCompactCbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
      acceptedInclusion: parseSubmitStep01TxInclusion({
        nativeTxId: selected.nodeTxId,
        nativeTx: selected.nativeTxCompact,
        nativeTxCompactCbor: selected.nativeCompactCbor,
        l2TransactionSourceCbor: selected.l2TransactionSourceCbor,
        transactionsPhasRoot: trie.root,
        txMembershipProofCbor: requireProof(
          trie,
          Buffer.from(selected.nodeTxId, "hex"),
          "resolvedOutputNonCanonical accepted transaction",
        ),
      }),
    });
  }
  const forced = block.reconstruction.forcedTransactions.find(
    ({ key, value }) =>
      value.tx_id === evidence.subject.transaction_id &&
      Data.to(key as never, OutputReferenceSchema as never) ===
        Data.to(
          evidence.subject.source_key as never,
          OutputReferenceSchema as never,
        ),
  );
  if (forced === undefined || forced.value.verdict === "ForcedTxValid") {
    throw new Error(
      "resolvedOutputNonCanonical forced subject disappeared from retained DA",
    );
  }
  const reason = forced.value.verdict.ForcedTxInvalid.reason;
  if (
    evidence.subject.rejection_reason === null ||
    Data.to(reason as never, RejectionReasonV1Schema as never) !==
      Data.to(
        evidence.subject.rejection_reason as never,
        RejectionReasonV1Schema as never,
      )
  ) {
    throw new Error(
      "resolvedOutputNonCanonical forced reason differs from authenticated source",
    );
  }
  const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
    encodeMidgardNativeTxCanonicalV1(
      adjudicateMidgardNativeTxFullV1Validity(
        decodeMidgardNativeTxFullV1FromCanonicalCbor(
          forced.fullTransactionCbor,
        ),
        "TxIsInvalid",
      ),
    ),
  );
  if (
    material.proofSource.compactCbor.toString("hex") !==
      forced.value.source.compact_cbor ||
    material.proofSource.witnessSetCompactCbor.toString("hex") !==
      forced.value.source.witness_set_compact_cbor ||
    material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
      forced.value.source.field_preimage_lengths_cbor
  ) {
    throw new Error(
      "resolvedOutputNonCanonical forced source material differs from authenticated leaf",
    );
  }
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: forced.key },
  } as const;
  return Object.freeze({
    nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
    witnessSetCompactCbor:
      material.proofSource.witnessSetCompactCbor.toString("hex"),
    forcedHeader: block.header,
    forcedMembership: await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey,
    }),
    forcedDirection: 1n,
  });
};

/** Complete replay member: scans every accepted field-2 output and exact forced reason. */
type ResolvedOutputNonCanonicalProductionRuntimeLoaderV1 = Readonly<{
  config: LoadManifestBoundResolvedOutputNonCanonicalConfigV1;
  journal: ResolvedOutputJournalV1;
  observe: (identity: string) => Promise<ResolvedOutputStageV1>;
  resolveStage: (input: {
    readonly action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "submitReconstruction"
      | "submitStep05"
      | "removeDescendants"
      | "cancel";
    readonly evidence: ResolvedOutputEvidenceV1;
  }) => Promise<ResolvedOutputNonCanonicalProductionStageV1>;
}>;

export const createResolvedOutputNonCanonicalRawL1StageResolverV1 =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundResolvedOutputNonCanonicalConfigV1;
    readonly l1: FraudProofFamilyL1ObservationPortV1<FraudProofCatalogueCategoryName>;
    readonly source: ResolvedOutputNonCanonicalAuthenticatedSourceV1;
  }): ResolvedOutputNonCanonicalProductionRuntimeLoaderV1["resolveStage"] =>
  async ({ action, evidence }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "resolvedOutputNonCanonical init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "resolvedOutputNonCanonical removal requires raw-L1 proof token",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    const expectedStep =
      action === "submitStep01"
        ? 1
        : action === "submitStep02"
          ? 2
          : action === "submitStep03"
            ? 3
            : action === "submitReconstruction"
              ? 4
              : 5;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `resolvedOutputNonCanonical ${action} differs from authenticated raw-L1 stage`,
      );
    }
    const common = {
      fraudulentBlockOutRef: stage.stateQueueBlockOutRef,
      threadOutRef: stage.threadOutRef,
      nativeTxCompactCbor: source.nativeTxCompactCbor,
      witnessSetCompactCbor: source.witnessSetCompactCbor,
    };
    if (action !== "submitStep01") return common;
    if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_FORCED_V1) {
      return {
        ...common,
        forcedHeader: required(
          source.forcedHeader,
          "authenticated forced header",
        ),
        forcedMembership: required(
          source.forcedMembership,
          "authenticated forced membership",
        ),
        forcedDirection: required(
          source.forcedDirection,
          "authenticated forced direction",
        ),
      };
    }
    const thread = await requireLinearFaultThreadUtxoV1({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      family: "resolved-output-non-canonical",
      stepIndex: 0,
      threadOutRef: stage.threadOutRef,
    });
    return {
      ...common,
      threadUtxo: thread.threadUtxo,
      threadToken: thread.threadToken,
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      acceptedInclusion: required(
        source.acceptedInclusion,
        "authenticated accepted inclusion",
      ),
    };
  };

const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`resolvedOutputNonCanonical missing ${label}`);
  return value;
};

const createManifestBoundResolvedOutputNonCanonicalSubmissionV1 = ({
  config,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundResolvedOutputNonCanonicalConfigV1;
  readonly observe: (identity: string) => Promise<ResolvedOutputStageV1>;
  readonly resolveStage: ResolvedOutputNonCanonicalProductionRuntimeLoaderV1["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createResolvedOutputNonCanonicalCentralJournalAdapterV1
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => ({
  observe,
  submit: async (
    action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "submitReconstruction"
      | "submitStep05"
      | "removeDescendants",
    evidence: ResolvedOutputEvidenceV1,
  ) => {
    if (evidence.subject.transaction_id.length !== 64)
      throw new Error(
        "resolvedOutputNonCanonical evidence transaction id is not canonical",
      );
    const familyIdentity = resolvedOutputEvidenceIdentityV1(evidence);
    const transition =
      action === "submitInit"
        ? (["none", "step01"] as const)
        : action === "submitStep01"
          ? (["step01", "step02"] as const)
          : action === "submitStep02"
            ? (["step02", "step03"] as const)
            : action === "submitStep03"
              ? (["step03", "reconstructing"] as const)
              : action === "submitReconstruction"
                ? (["reconstructing", "step05"] as const)
                : action === "submitStep05"
                  ? (["step05", "proven"] as const)
                  : (["proven", "removed"] as const);
    await centralJournal?.begin(
      action,
      familyIdentity,
      transition[0],
      transition[1],
    );
    const stage = await resolveStage({ action, evidence });
    if (action === "submitInit") {
      const result = await submitCommittedFieldShapeInit({
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        network: config.binding.network,
        contracts: config.contracts as never,
        category: config.binding.resolvedContracts.category,
        catalogue: config.binding.catalogue,
        signer: config.signer,
        fraudulentBlockOutRef: stage.fraudulentBlockOutRef,
        fraudulentHeaderHash: config.binding.definition.headerHash,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: "step01" as const,
        txHash: result.txHash,
        outputReference: `${result.txHash}#${result.firstStepOutputIndex.toString()}`,
      };
    }
    if (action === "submitStep01") {
      if (
        evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED_V1
      ) {
        const result = await submitResolvedOutputNonCanonicalStep01AcceptedV1({
          lucid: config.lucid,
          blueprint: config.binding.blueprint,
          network: config.binding.network,
          contracts: config.contracts,
          signer: config.signer,
          finding: evidence,
          threadUtxo: required(stage.threadUtxo, "step01 thread UTxO"),
          threadToken: required(stage.threadToken, "step01 thread token"),
          stateQueueBlockOutRef: required(
            stage.stateQueueBlockOutRef,
            "state-queue block out-ref",
          ),
          txInclusion: required(stage.acceptedInclusion, "accepted inclusion"),
          referenceScriptUtxo: config.referenceScripts.step01,
          witnessReferenceScripts: config.referenceScripts.witnesses,
          preSubmitBoundary: centralJournal?.boundary(
            action,
            familyIdentity,
            transition[0],
            transition[1],
          ),
        });
        return {
          stage: "step02" as const,
          txHash: result.txHash,
          outputReference: result.nextThreadOutRef,
        };
      }
      if (evidence.subject.source_kind !== PROOF_THREAD_SOURCE_KIND_FORCED_V1)
        throw new Error(
          "resolvedOutputNonCanonical evidence source kind is invalid",
        );
      const result = await submitResolvedOutputNonCanonicalStep01ForcedV1({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step01 thread out-ref"),
        finding: evidence,
        forcedSource: {
          header: required(stage.forcedHeader, "forced header"),
          membership: required(stage.forcedMembership, "forced membership"),
          direction: required(stage.forcedDirection, "forced direction"),
        },
        referenceScriptUtxo: config.referenceScripts.step01,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: "step02" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep02") {
      const auxiliaryHashes: string[] = [];
      const result = await submitResolvedOutputNonCanonicalStep02V1({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step02 thread out-ref"),
        evidence,
        nativeTxCompactCbor: required(
          stage.nativeTxCompactCbor,
          "native transaction compact CBOR",
        ),
        witnessSetCompactCbor: required(
          stage.witnessSetCompactCbor,
          "witness-set compact CBOR",
        ),
        publishCarriage: evidence.carriage === "RawUtxo",
        publishedCarriageUtxos: stage.publishedCarriageUtxos,
        certificateUtxo: stage.certificateUtxo,
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        publicationPreSubmitBoundary: centralJournal?.auxiliaryBoundary(
          "publication",
          familyIdentity,
          "step02",
          auxiliaryHashes,
        ),
        certificatePreSubmitBoundary: centralJournal?.auxiliaryBoundary(
          "certificate",
          familyIdentity,
          "step02",
          auxiliaryHashes,
        ),
        onCarriageReady:
          centralJournal === undefined
            ? undefined
            : async () => {
                for (const txHash of auxiliaryHashes) {
                  await centralJournal.confirmAuxiliary(txHash);
                }
              },
        referenceScriptUtxo: config.referenceScripts.step02,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: "step03" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep03") {
      const result = await submitResolvedOutputNonCanonicalStep03V1({
        lucid: config.lucid,
        network: config.binding.network,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step03 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step03,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: "reconstructing" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitReconstruction") {
      const result = await submitResolvedOutputNonCanonicalStep04V1({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(
          stage.threadOutRef,
          "reconstruction thread out-ref",
        ),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step04,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: result.terminal
          ? ("step05" as const)
          : ("reconstructing" as const),
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep05") {
      const result = await submitResolvedOutputNonCanonicalStep05V1({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step05 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step05,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: "proven" as const,
        txHash: result.txHash,
        outputReference: null,
      };
    }
    const result = await submitRemoveFraudulentBlock({
      lucid: config.lucid,
      blueprint: config.binding.blueprint,
      deploymentInfo: config.binding.deploymentInfo,
      network: config.binding.network,
      signer: config.signer,
      fraudCategory:
        "resolvedOutputNonCanonical" as FraudProofCatalogueCategoryName,
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "resolvedOutputNonCanonical production removal requires a state-queue mutation lease coordinator",
          );
        })(),
      awaitConfirmation: true,
      validFrom: stage.validFrom,
      validTo: stage.validTo,
      preSubmitBoundary: centralJournal?.boundary(
        action,
        familyIdentity,
        transition[0],
        transition[1],
      ),
    });
    return {
      stage: "removed" as const,
      txHash: result.txHash,
      outputReference: null,
    };
  },
  cancel: async (
    current: "step01" | "step02" | "step03" | "reconstructing",
    evidence: ResolvedOutputEvidenceV1,
  ) => {
    const stage = await resolveStage({ action: "cancel", evidence });
    const index =
      current === "step01"
        ? 0
        : current === "step02"
          ? 1
          : current === "step03"
            ? 2
            : 3;
    const result = await submitResolvedOutputNonCanonicalCancelV1({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      signer: config.signer,
      threadOutRef: required(stage.threadOutRef, "cancel thread out-ref"),
      referenceScriptUtxo: [
        config.referenceScripts.step01,
        config.referenceScripts.step02,
        config.referenceScripts.step03,
        config.referenceScripts.step04,
      ][index]!,
      witnessReferenceScripts: config.referenceScripts.witnesses,
    });
    return {
      stage: "cancelled" as const,
      txHash: result.txHash,
      outputReference: null,
    };
  },
});

const createManifestBoundResolvedOutputNonCanonicalProductionRuntimeV1 = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundResolvedOutputNonCanonicalConfigV1;
  readonly journal: ResolvedOutputJournalV1;
  readonly observe: ResolvedOutputNonCanonicalProductionRuntimeLoaderV1["observe"];
  readonly resolveStage: ResolvedOutputNonCanonicalProductionRuntimeLoaderV1["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createResolvedOutputNonCanonicalCentralJournalAdapterV1
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundResolvedOutputNonCanonicalSubmissionV1({
    config,
    observe: async (identity) => {
      const observed = await observe(identity);
      await centralJournal?.reconcile(observed);
      return observed;
    },
    resolveStage,
    centralJournal,
    stateQueueMutationLeaseCoordinator,
  });
  return Object.freeze({
    runtimeVersion: RESOLVED_OUTPUT_NON_CANONICAL_PRODUCTION_WORKFLOW_V1,
    config,
    runOrResume: async (evidence: ResolvedOutputEvidenceV1) => {
      const identity = resolvedOutputEvidenceIdentityV1(evidence);
      for (;;) {
        const stage = await submission.observe(identity);
        const action = nextResolvedOutputActionV1(stage);
        if (action === "done") return stage;
        const result = await submission.submit(action, evidence);
        await journal.append({
          sequence: (await journal.load(identity)).length,
          identity,
          stage: result.stage,
          action,
          phase: "submitted",
          txHash: result.txHash,
          outputReference: result.outputReference,
        });
      }
    },
  });
};

export type ManifestBoundResolvedOutputNonCanonicalWorkflowConfigV1 =
  LoadManifestBoundResolvedOutputNonCanonicalConfigV1 &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
      historicalCheckpointStore: ProductionHistoricalNativeScriptCheckpointStoreV1;
      historicalSource: ProductionHistoricalNativeScriptHistorySourceV1;
    }>;

export type ManifestBoundResolvedOutputNonCanonicalWorkflowV1 = Readonly<{
  workflowVersion: typeof RESOLVED_OUTPUT_NON_CANONICAL_PRODUCTION_WORKFLOW_V1;
  config: ManifestBoundResolvedOutputNonCanonicalConfigV1;
  binding: FraudProofWorkflowDeploymentBindingV1<"resolvedOutputNonCanonical">;
  l1: FraudProofFamilyL1ObservationPortV1<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
  historicalCheckpointStore: ProductionHistoricalNativeScriptCheckpointStoreV1;
  historicalSource: ProductionHistoricalNativeScriptHistorySourceV1;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundResolvedOutputNonCanonicalWorkflowV1 = async (
  input: ManifestBoundResolvedOutputNonCanonicalWorkflowConfigV1,
): Promise<ManifestBoundResolvedOutputNonCanonicalWorkflowV1> => {
  const config =
    await loadManifestBoundResolvedOutputNonCanonicalConfigV1(input);
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: input.source,
    releaseFinality: config.binding.releaseFinality,
    releaseEconomics: config.binding.releaseEconomics,
    definition: config.binding.definition,
  });
  return Object.freeze({
    workflowVersion: RESOLVED_OUTPUT_NON_CANONICAL_PRODUCTION_WORKFLOW_V1,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
    historicalCheckpointStore: input.historicalCheckpointStore,
    historicalSource: input.historicalSource,
  });
};

const resolvedOutputStageFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPortV1<FraudProofCatalogueCategoryName>["observe"]
    >
  >["stage"],
): ResolvedOutputStageV1 => {
  switch (stage.kind) {
    case "not_started":
      return "none";
    case "step":
      if (stage.step === 1) return "step01";
      if (stage.step === 2) return "step02";
      if (stage.step === 3) return "step03";
      if (stage.step === 4) return "reconstructing";
      if (stage.step === 5) return "step05";
      throw new Error(
        "resolvedOutputNonCanonical L1 stage exceeds five-step topology",
      );
    case "proof_token":
      return "proven";
    case "removed":
      return "removed";
  }
};

/**
 * Watcher-facing runner. Evidence is always reconstructed from authenticated
 * L1 plus public retained DA; unknown/caller-authored evidence fields fail.
 */
export const runOrResumeManifestBoundResolvedOutputNonCanonicalWorkflowV1 =
  async (input: {
    readonly workflow: ManifestBoundResolvedOutputNonCanonicalWorkflowV1;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: ResolvedOutputJournalV1;
  }): Promise<ResolvedOutputStageV1> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "resolvedOutputNonCanonical runner rejects caller-authored evidence inputs",
      );
    }
    const headerHash = input.workflow.binding.definition.headerHash;
    const observation = await input.workflow.l1.observeHeader({ headerHash });
    const canonical = await fetchCanonicalBlockEvidenceV1({
      observation,
      sources: input.sources,
    });
    const corpus = await resolveProductionHistoricalNativeScriptCorpusV1({
      deploymentFingerprint: input.workflow.binding.deploymentFingerprint,
      checkpointStore: input.workflow.historicalCheckpointStore,
      historySource: input.workflow.historicalSource,
      currentEvidence: canonical,
      sources: input.sources,
    });
    const priorLedger =
      await deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpusV1({
        block: canonical,
        corpus,
      });
    const findings = detectResolvedOutputNonCanonicalCompleteReplayV1({
      block: canonical,
      priorLedger,
    });
    if (findings.length !== 1)
      throw new Error(
        `resolvedOutputNonCanonical public replay yielded ${findings.length.toString()} exact findings`,
      );
    const evidence = findings[0]!;
    const source = await deriveResolvedOutputNonCanonicalAuthenticatedSourceV1({
      block: canonical,
      evidence,
    });
    const runtime =
      createManifestBoundResolvedOutputNonCanonicalProductionRuntimeV1({
        config: input.workflow.config,
        journal: input.journal,
        observe: async () =>
          resolvedOutputStageFromL1(
            (await input.workflow.l1.observe({ headerHash })).stage,
          ),
        resolveStage: createResolvedOutputNonCanonicalRawL1StageResolverV1({
          config: input.workflow.config,
          l1: input.workflow.l1,
          source,
        }),
      });
    return await runtime.runOrResume(evidence);
  };

export const executeManifestBoundResolvedOutputNonCanonicalWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundResolvedOutputNonCanonicalWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<ResolvedOutputStageV1> => {
  const headerHash = workflow.binding.definition.headerHash;
  const canonical = await fetchCanonicalBlockEvidenceV1({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const corpus = await resolveProductionHistoricalNativeScriptCorpusV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    checkpointStore: workflow.historicalCheckpointStore,
    historySource: workflow.historicalSource,
    currentEvidence: canonical,
    sources,
  });
  const priorLedger =
    await deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpusV1({
      block: canonical,
      corpus,
    });
  const findings = detectResolvedOutputNonCanonicalCompleteReplayV1({
    block: canonical,
    priorLedger,
  });
  if (findings.length !== 1)
    throw new Error(
      `resolvedOutputNonCanonical public replay yielded ${findings.length.toString()} exact findings`,
    );
  const evidence = findings[0]!;
  const source = await deriveResolvedOutputNonCanonicalAuthenticatedSourceV1({
    block: canonical,
    evidence,
  });
  const centralJournal =
    createResolvedOutputNonCanonicalCentralJournalAdapterV1({
      store: journal,
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      headerHash,
      decisionDigest: workflow.decisionDigest,
      transactionConfirmed: async (txHash) =>
        await workflow.l1.transactionConfirmed({ headerHash, txHash }),
    });
  const runtime =
    createManifestBoundResolvedOutputNonCanonicalProductionRuntimeV1({
      config: workflow.config,
      journal: centralJournal.familyJournal,
      observe: async () =>
        resolvedOutputStageFromL1(
          (await workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createResolvedOutputNonCanonicalRawL1StageResolverV1({
        config: workflow.config,
        l1: workflow.l1,
        source,
      }),
      centralJournal,
      stateQueueMutationLeaseCoordinator:
        workflow.stateQueueMutationLeaseCoordinator,
    });
  return await runtime.runOrResume(evidence);
};

export type LoadedResolvedOutputNonCanonicalProductionWorkflowV1 = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundResolvedOutputNonCanonicalWorkflowConfigV1;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadResolvedOutputNonCanonicalProductionWorkflowV1 = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: ProductionWorkflowAdapterReadinessInputV1;
}) => Promise<LoadedResolvedOutputNonCanonicalProductionWorkflowV1>;

/**
 * Family-local runner surface for central admission. It consumes only a
 * manifest/runtime path and concrete public-DA transports; neither evidence
 * nor a watcher-owned journal implementation can enter this boundary.
 */
export const createResolvedOutputNonCanonicalProductionWorkflowRunnerSurfaceV1 =
  ({
    loadRuntimeConfig,
  }: {
    readonly loadRuntimeConfig: LoadResolvedOutputNonCanonicalProductionWorkflowV1;
  }): ProductionWorkflowAdapterRunnerV1 =>
    Object.freeze({
      runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
      runOrResume: async (invocation) => {
        if (String(invocation.category) !== "resolvedOutputNonCanonical") {
          throw new Error(
            `resolvedOutputNonCanonical production runner category mismatch: ${invocation.category}`,
          );
        }
        const journal = bindProductionWorkflowFundingReservationJournalV1({
          permit: invocation.fundingReservationPermit,
          journal: bindProductionWorkflowActuationJournalV1({
            journal: new DirectoryFraudProofWorkflowJournalStoreV1(
              invocation.journalDirectory,
            ),
            permit: invocation.actuationPermit,
            decisionDigest: invocation.decisionDigest,
            deploymentFingerprint: invocation.deploymentFingerprint,
            category:
              "resolvedOutputNonCanonical" as FraudProofCatalogueCategoryName,
            headerHash: invocation.headerHash,
          }),
        });
        assertProductionWorkflowJournalActuationV1({
          journal,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category:
            "resolvedOutputNonCanonical" as FraudProofCatalogueCategoryName,
          headerHash: invocation.headerHash,
          checkpoint: "runner_start",
        });
        const loaded = await loadRuntimeConfig({
          runtimeConfigPath: invocation.runtimeConfigPath,
          invocation,
        });
        if (typeof loaded.close !== "function") {
          throw new Error(
            "resolvedOutputNonCanonical runtime omitted its transport disposer",
          );
        }
        try {
          if (
            loaded.schemaVersion !==
            "midgard-production-fraud-proof-runtime-config-v1"
          ) {
            throw new Error(
              "resolvedOutputNonCanonical runtime config has an unsupported schema",
            );
          }
          if (
            loaded.retainedDaSources.length === 0 ||
            loaded.retainedDaSources.some(
              (source) => !(source instanceof DaLibp2pRetainedDaSource),
            )
          ) {
            throw new Error(
              "resolvedOutputNonCanonical production runner requires concrete public retained-DA sources",
            );
          }
          const workflow =
            await createManifestBoundResolvedOutputNonCanonicalWorkflowV1(
              loaded.config,
            );
          if (
            workflow.binding.deploymentFingerprint !==
              invocation.deploymentFingerprint ||
            String(workflow.binding.definition.category) !==
              "resolvedOutputNonCanonical" ||
            workflow.binding.definition.headerHash !== invocation.headerHash ||
            workflow.decisionDigest !== invocation.decisionDigest
          ) {
            throw new Error(
              "resolvedOutputNonCanonical manifest-bound workflow identity differs from invocation",
            );
          }
          return await executeManifestBoundResolvedOutputNonCanonicalWorkflowV1(
            {
              workflow,
              sources: loaded.retainedDaSources,
              journal,
            },
          );
        } finally {
          await loaded.close();
        }
      },
    });
