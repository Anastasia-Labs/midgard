import {
  adjudicateMidgardNativeTxFullV1Validity,
  decodeMidgardFieldPreimageV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  encodeMidgardNativeTxCanonicalV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  type ForcedInclusionTxV1,
  forcedVerdictSubjectV1,
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
import type { CanonicalViolationDetectionV1 } from "../workflow/classification-v1.js";
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
import { createFieldItemWidthIllegalCentralJournalAdapterV1 } from "./central-journal-v1.js";
import type { FieldItemWidthIllegalContractsV1 } from "./contracts-v1.js";
import {
  fieldItemWidthCoordinateIsSupportedV1,
  fieldItemWidthEvidenceClosesV1,
  fieldItemWidthEvidenceIdentityV1,
  type FieldItemWidthEvidenceV1,
  fieldItemWidthIsIllegalV1,
  type FieldItemWidthJournalV1,
  type FieldItemWidthStageV1,
  prepareFieldItemWidthEvidenceV1,
  runFieldItemWidthProofV1,
} from "./field-item-width-illegal-v1.js";
import {
  FieldItemWidthStep02DatumV1Schema,
  FieldItemWidthStep03DatumV1Schema,
} from "./schemas-v1.js";
import { submitFieldItemWidthIllegalCancelV1 } from "./submit-cancel-v1.js";
import { submitFieldItemWidthIllegalStep01AcceptedV1 } from "./submit-step-01-accepted-v1.js";
import { submitFieldItemWidthIllegalStep01ForcedV1 } from "./submit-step-01-forced-v1.js";
import { submitFieldItemWidthIllegalStep02V1 } from "./submit-step-02-v1.js";
import { submitFieldItemWidthIllegalStep03V1 } from "./submit-step-03-v1.js";

export const FIELD_ITEM_WIDTH_ILLEGAL_PRODUCTION_WORKFLOW_V1 =
  "midgard-field-item-width-illegal-production-workflow-v1" as const;
export const FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID_V1 =
  "field-item-width-illegal" as const;

export const FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS_V1 = Object.freeze({
  step01: "fraudProofFieldItemWidthIllegal",
  step02: "fraudProofFieldItemWidthIllegalStep02",
  step03: "fraudProofFieldItemWidthIllegalStep03",
  computationThreadMint: "computationThreadMint",
  fraudProofMint: "fraudProofMint",
  phasMembershipWithdraw: "phasMembershipWithdraw",
  fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
} as const);

export type FieldItemWidthIllegalProductionReferenceScriptsV1 = Readonly<{
  step01: UTxO;
  step02: UTxO;
  step03: UTxO;
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

export type ManifestBoundFieldItemWidthIllegalConfigV1 = Readonly<{
  schemaVersion: typeof FIELD_ITEM_WIDTH_ILLEGAL_PRODUCTION_WORKFLOW_V1;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBindingV1<"fieldItemWidthIllegal">;
  contracts: FieldItemWidthIllegalContractsV1;
  referenceScripts: FieldItemWidthIllegalProductionReferenceScriptsV1;
}>;

export type LoadManifestBoundFieldItemWidthIllegalConfigV1 = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: FieldItemWidthIllegalProductionReferenceScriptsV1;
}>;

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: FraudProofWorkflowDeploymentBindingV1<"fieldItemWidthIllegal">;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxoV1({ binding, contractName, utxo });

export const bindFieldItemWidthIllegalReferenceScriptsV1 = ({
  binding,
  referenceScripts,
}: {
  readonly binding: FraudProofWorkflowDeploymentBindingV1<"fieldItemWidthIllegal">;
  readonly referenceScripts: FieldItemWidthIllegalProductionReferenceScriptsV1;
}): FieldItemWidthIllegalProductionReferenceScriptsV1 => {
  const names = FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS_V1;
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

export const loadManifestBoundFieldItemWidthIllegalConfigV1 = async (
  input: LoadManifestBoundFieldItemWidthIllegalConfigV1,
): Promise<ManifestBoundFieldItemWidthIllegalConfigV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "fieldItemWidthIllegal",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      FieldItemWidthStep02DatumV1Schema,
      FieldItemWidthStep03DatumV1Schema,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.fieldItemWidthIllegal;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined || chain.steps.length !== 3) {
    throw new Error(
      "fieldItemWidthIllegal deployment changed its three-step topology",
    );
  }
  if (certificate === null) {
    throw new Error(
      "fieldItemWidthIllegal deployment omitted field-preimage certificate",
    );
  }
  const referenceScripts = bindFieldItemWidthIllegalReferenceScriptsV1({
    binding,
    referenceScripts: input.referenceScripts,
  });
  return Object.freeze({
    schemaVersion: FIELD_ITEM_WIDTH_ILLEGAL_PRODUCTION_WORKFLOW_V1,
    lucid: input.lucid,
    signer: input.signer,
    binding,
    contracts: {
      steps: chain.steps.map((step, index) => ({
        ...step,
        blueprintTitle: [
          "fraud_proofs/field_item_width_illegal/step_01.main.spend",
          "fraud_proofs/field_item_width_illegal/step_02.main.spend",
          "fraud_proofs/field_item_width_illegal/step_03.main.spend",
        ][index]!,
        referenceOutRef: [
          referenceScripts.step01,
          referenceScripts.step02,
          referenceScripts.step03,
        ][index]!.txHash.concat(
          "#",
          [
            referenceScripts.step01,
            referenceScripts.step02,
            referenceScripts.step03,
          ][index]!.outputIndex.toString(),
        ),
      })) as unknown as FieldItemWidthIllegalContractsV1["steps"],
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

export type FieldItemWidthIllegalProductionStageV1 = Readonly<{
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
export const deriveFieldItemWidthIllegalEvidenceFromCanonicalBlockV1 = (
  block: CanonicalBlockEvidenceV1,
): FieldItemWidthEvidenceV1 => {
  const findings: FieldItemWidthEvidenceV1[] = [];
  const inspect = ({
    canonicalCbor,
    subject,
    forcedCoordinate,
  }: {
    readonly canonicalCbor: Uint8Array;
    readonly subject: ReturnType<typeof acceptedVerdictSubjectV1>;
    readonly forcedCoordinate?: {
      readonly fieldIndex: number;
      readonly itemIndex: number;
    };
  }) => {
    const material =
      deriveMidgardNativeTxFaultEvidenceMaterialV1(canonicalCbor);
    if (material.transactionId.toString("hex") !== subject.transaction_id) {
      throw new Error(
        "fieldItemWidthIllegal retained-DA transaction identity changed",
      );
    }
    const coordinates =
      forcedCoordinate === undefined
        ? ([2, 5] as const).flatMap((fieldIndex) =>
            decodeMidgardFieldPreimageV1(
              material.fieldPreimages[fieldIndex]!,
            ).map((_, itemIndex) => ({ fieldIndex, itemIndex })),
          )
        : [forcedCoordinate];
    for (const coordinate of coordinates) {
      const fieldPreimage = material.fieldPreimages[coordinate.fieldIndex]!;
      const item =
        decodeMidgardFieldPreimageV1(fieldPreimage)[coordinate.itemIndex];
      if (item === undefined) {
        throw new Error(
          "fieldItemWidthIllegal retained-DA reason coordinate is absent",
        );
      }
      const illegal = fieldItemWidthIsIllegalV1(
        coordinate.fieldIndex,
        item.length,
      );
      if (forcedCoordinate === undefined && !illegal) continue;
      const prepared = prepareFieldItemWidthEvidenceV1({
        finding: { subject, ...coordinate },
        fieldPreimage,
        committedFieldHashHex:
          midgardFieldCommitmentV1(fieldPreimage).toString("hex"),
      });
      if (fieldItemWidthEvidenceClosesV1(prepared)) findings.push(prepared);
    }
  };
  for (const transaction of block.transactions) {
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      Buffer.from(transaction.txCbor, "hex"),
    );
    inspect({
      canonicalCbor: Buffer.from(transaction.txCbor, "hex"),
      subject: acceptedVerdictSubjectV1(material.transactionId.toString("hex")),
    });
  }
  for (const forced of block.reconstruction.forcedTransactions) {
    if (forced.value.verdict === "ForcedTxValid") continue;
    const reason = forced.value.verdict.ForcedTxInvalid.reason;
    if (typeof reason === "string" || !("FieldItemWidthIllegal" in reason))
      continue;
    const coordinate = reason.FieldItemWidthIllegal;
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      forced.fullTransactionCbor,
    );
    if (material.transactionId.toString("hex") !== forced.value.tx_id) {
      throw new Error(
        "fieldItemWidthIllegal forced retained-DA identity changed",
      );
    }
    inspect({
      canonicalCbor: forced.fullTransactionCbor,
      subject: forcedVerdictSubjectV1({
        transactionId: forced.value.tx_id,
        sourceKey: forced.key,
        rejectionReason: reason,
      }),
      forcedCoordinate: {
        fieldIndex: Number(coordinate.field_index),
        itemIndex: Number(coordinate.item_index),
      },
    });
  }
  if (findings.length !== 1) {
    throw new Error(
      `fieldItemWidthIllegal public retained DA yielded ${findings.length.toString()} exact findings`,
    );
  }
  return findings[0]!;
};

export type FieldItemWidthIllegalAuthenticatedSourceV1 = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: HeaderV1;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
}>;

/** Rebuilds all accepted/forced submitter material from the authenticated block. */
export const deriveFieldItemWidthIllegalAuthenticatedSourceV1 = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidenceV1;
  readonly evidence: FieldItemWidthEvidenceV1;
}): Promise<FieldItemWidthIllegalAuthenticatedSourceV1> => {
  if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED_V1) {
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.subject.transaction_id,
    );
    if (selected === undefined) {
      throw new Error(
        "fieldItemWidthIllegal accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItemV1));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "fieldItemWidthIllegal accepted source trie differs from authenticated reconstruction",
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
          "fieldItemWidthIllegal accepted transaction",
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
      "fieldItemWidthIllegal forced subject disappeared from retained DA",
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
      "fieldItemWidthIllegal forced reason differs from authenticated source",
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
      "fieldItemWidthIllegal forced source material differs from authenticated leaf",
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

/** Complete accepted-block replay member: scans every field-2/field-5 item. */
export const detectFieldItemWidthIllegalCompleteReplayV1 = (
  evidence: CanonicalBlockEvidenceV1,
): readonly CanonicalViolationDetectionV1[] => {
  const accepted = evidence.transactions.flatMap(
    (transaction, transactionIndex) => {
      const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
        Buffer.from(transaction.txCbor, "hex"),
      );
      const transactionId = material.transactionId.toString("hex");
      if (transaction.nodeTxId !== transactionId) {
        throw new Error(
          "fieldItemWidthIllegal complete replay transaction identity changed",
        );
      }
      return ([2, 5] as const).flatMap((fieldIndex) =>
        decodeMidgardFieldPreimageV1(
          material.fieldPreimages[fieldIndex]!,
        ).flatMap((item, itemIndex) =>
          fieldItemWidthIsIllegalV1(fieldIndex, item.length)
            ? [
                {
                  detectionId: `${FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID_V1}:${transactionIndex.toString()}:${transactionId}:${fieldIndex.toString()}:${itemIndex.toString()}:${item.length.toString()}`,
                  headerHash: evidence.headerHash,
                  violationId: FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID_V1,
                  position: BigInt(transactionIndex),
                  diagnostic: `transaction ${transactionId} field ${fieldIndex.toString()} item ${itemIndex.toString()} has illegal width ${item.length.toString()}`,
                },
              ]
            : [],
        ),
      );
    },
  );
  const forced = evidence.reconstruction.forcedTransactions.flatMap(
    (transaction, forcedIndex) => {
      if (transaction.value.verdict === "ForcedTxValid") return [];
      const reason = transaction.value.verdict.ForcedTxInvalid.reason;
      if (typeof reason === "string" || !("FieldItemWidthIllegal" in reason)) {
        return [];
      }
      const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
        encodeMidgardNativeTxCanonicalV1(
          adjudicateMidgardNativeTxFullV1Validity(
            decodeMidgardNativeTxFullV1FromCanonicalCbor(
              transaction.fullTransactionCbor,
            ),
            "TxIsInvalid",
          ),
        ),
      );
      if (
        material.transactionId.toString("hex") !== transaction.value.tx_id ||
        material.proofSource.compactCbor.toString("hex") !==
          transaction.value.source.compact_cbor ||
        material.proofSource.witnessSetCompactCbor.toString("hex") !==
          transaction.value.source.witness_set_compact_cbor ||
        material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
          transaction.value.source.field_preimage_lengths_cbor
      ) {
        throw new Error(
          "fieldItemWidthIllegal forced transaction differs from its authenticated leaf",
        );
      }
      const coordinate = reason.FieldItemWidthIllegal;
      const fieldIndex = Number(coordinate.field_index);
      const itemIndex = Number(coordinate.item_index);
      if (!fieldItemWidthCoordinateIsSupportedV1(fieldIndex, itemIndex)) {
        return [];
      }
      const preimage = material.fieldPreimages[fieldIndex];
      const item =
        preimage === undefined
          ? undefined
          : decodeMidgardFieldPreimageV1(preimage)[itemIndex];
      if (
        item === undefined ||
        fieldItemWidthIsIllegalV1(fieldIndex, item.length)
      ) {
        return [];
      }
      return [
        {
          detectionId: `${FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID_V1}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${fieldIndex.toString()}:${itemIndex.toString()}:${item.length.toString()}`,
          headerHash: evidence.headerHash,
          violationId: FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID_V1,
          position: BigInt(forcedIndex),
          diagnostic: `forced transaction ${transaction.value.tx_id} was rejected for legal field ${fieldIndex.toString()} item ${itemIndex.toString()} width ${item.length.toString()}`,
        },
      ];
    },
  );
  return [...accepted, ...forced];
};

export type FieldItemWidthIllegalProductionRuntimeLoaderV1 = Readonly<{
  config: LoadManifestBoundFieldItemWidthIllegalConfigV1;
  journal: FieldItemWidthJournalV1;
  observe: (identity: string) => Promise<FieldItemWidthStageV1>;
  resolveStage: (input: {
    readonly action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "removeDescendants"
      | "cancel";
    readonly evidence: FieldItemWidthEvidenceV1;
  }) => Promise<FieldItemWidthIllegalProductionStageV1>;
}>;

export const createFieldItemWidthIllegalRawL1StageResolverV1 =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundFieldItemWidthIllegalConfigV1;
    readonly l1: FraudProofFamilyL1ObservationPortV1<"fieldItemWidthIllegal">;
    readonly source: FieldItemWidthIllegalAuthenticatedSourceV1;
  }): FieldItemWidthIllegalProductionRuntimeLoaderV1["resolveStage"] =>
  async ({ action, evidence }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "fieldItemWidthIllegal init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "fieldItemWidthIllegal removal requires raw-L1 proof token",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    const expectedStep =
      action === "submitStep01" ? 1 : action === "submitStep02" ? 2 : 3;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `fieldItemWidthIllegal ${action} differs from authenticated raw-L1 stage`,
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
      family: "field-item-width-illegal",
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
    throw new Error(`fieldItemWidthIllegal missing ${label}`);
  return value;
};

export const createManifestBoundFieldItemWidthIllegalSubmissionV1 = ({
  config,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundFieldItemWidthIllegalConfigV1;
  readonly observe: (identity: string) => Promise<FieldItemWidthStageV1>;
  readonly resolveStage: FieldItemWidthIllegalProductionRuntimeLoaderV1["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createFieldItemWidthIllegalCentralJournalAdapterV1
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
      | "removeDescendants",
    evidence: FieldItemWidthEvidenceV1,
  ) => {
    if (evidence.subject.transaction_id.length !== 64)
      throw new Error(
        "fieldItemWidthIllegal evidence transaction id is not canonical",
      );
    const familyIdentity = fieldItemWidthEvidenceIdentityV1(evidence);
    const transition =
      action === "submitInit"
        ? (["none", "step01"] as const)
        : action === "submitStep01"
          ? (["step01", "step02"] as const)
          : action === "submitStep02"
            ? (["step02", "step03"] as const)
            : action === "submitStep03"
              ? (["step03", "proven"] as const)
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
        const result = await submitFieldItemWidthIllegalStep01AcceptedV1({
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
          "fieldItemWidthIllegal evidence source kind is invalid",
        );
      const result = await submitFieldItemWidthIllegalStep01ForcedV1({
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
      const result = await submitFieldItemWidthIllegalStep02V1({
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
      const result = await submitFieldItemWidthIllegalStep03V1({
        lucid: config.lucid,
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
      fraudCategory: "fieldItemWidthIllegal",
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "fieldItemWidthIllegal production removal requires a state-queue mutation lease coordinator",
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
    current: "step01" | "step02" | "step03",
    evidence: FieldItemWidthEvidenceV1,
  ) => {
    const stage = await resolveStage({ action: "cancel", evidence });
    const index = current === "step01" ? 0 : current === "step02" ? 1 : 2;
    const result = await submitFieldItemWidthIllegalCancelV1({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      signer: config.signer,
      threadOutRef: required(stage.threadOutRef, "cancel thread out-ref"),
      referenceScriptUtxo: [
        config.referenceScripts.step01,
        config.referenceScripts.step02,
        config.referenceScripts.step03,
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

export const loadFieldItemWidthIllegalProductionRuntimeV1 = async (
  input: FieldItemWidthIllegalProductionRuntimeLoaderV1,
) => {
  const config = await loadManifestBoundFieldItemWidthIllegalConfigV1(
    input.config,
  );
  return createManifestBoundFieldItemWidthIllegalProductionRuntimeV1({
    config,
    journal: input.journal,
    observe: input.observe,
    resolveStage: input.resolveStage,
  });
};

export const createManifestBoundFieldItemWidthIllegalProductionRuntimeV1 = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundFieldItemWidthIllegalConfigV1;
  readonly journal: FieldItemWidthJournalV1;
  readonly observe: FieldItemWidthIllegalProductionRuntimeLoaderV1["observe"];
  readonly resolveStage: FieldItemWidthIllegalProductionRuntimeLoaderV1["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createFieldItemWidthIllegalCentralJournalAdapterV1
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundFieldItemWidthIllegalSubmissionV1({
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
    runtimeVersion: FIELD_ITEM_WIDTH_ILLEGAL_PRODUCTION_WORKFLOW_V1,
    config,
    runOrResume: async (evidence: FieldItemWidthEvidenceV1) =>
      await runFieldItemWidthProofV1({
        evidence,
        journal,
        submission,
      }),
  });
};

export type ManifestBoundFieldItemWidthIllegalWorkflowConfigV1 =
  LoadManifestBoundFieldItemWidthIllegalConfigV1 &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundFieldItemWidthIllegalWorkflowV1 = Readonly<{
  workflowVersion: typeof FIELD_ITEM_WIDTH_ILLEGAL_PRODUCTION_WORKFLOW_V1;
  config: ManifestBoundFieldItemWidthIllegalConfigV1;
  binding: FraudProofWorkflowDeploymentBindingV1<"fieldItemWidthIllegal">;
  l1: FraudProofFamilyL1ObservationPortV1<"fieldItemWidthIllegal">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundFieldItemWidthIllegalWorkflowV1 = async (
  input: ManifestBoundFieldItemWidthIllegalWorkflowConfigV1,
): Promise<ManifestBoundFieldItemWidthIllegalWorkflowV1> => {
  const config = await loadManifestBoundFieldItemWidthIllegalConfigV1(input);
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: input.source,
    releaseFinality: config.binding.releaseFinality,
    releaseEconomics: config.binding.releaseEconomics,
    definition: config.binding.definition,
  });
  return Object.freeze({
    workflowVersion: FIELD_ITEM_WIDTH_ILLEGAL_PRODUCTION_WORKFLOW_V1,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

const fieldItemWidthStageFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPortV1<"fieldItemWidthIllegal">["observe"]
    >
  >["stage"],
): FieldItemWidthStageV1 => {
  switch (stage.kind) {
    case "not_started":
      return "none";
    case "step":
      if (stage.step === 1) return "step01";
      if (stage.step === 2) return "step02";
      if (stage.step === 3) return "step03";
      throw new Error(
        "fieldItemWidthIllegal L1 stage exceeds three-step topology",
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
export const runOrResumeManifestBoundFieldItemWidthIllegalWorkflowV1 =
  async (input: {
    readonly workflow: ManifestBoundFieldItemWidthIllegalWorkflowV1;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FieldItemWidthJournalV1;
  }): Promise<FieldItemWidthStageV1> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "fieldItemWidthIllegal runner rejects caller-authored evidence inputs",
      );
    }
    const headerHash = input.workflow.binding.definition.headerHash;
    const observation = await input.workflow.l1.observeHeader({ headerHash });
    const canonical = await fetchCanonicalBlockEvidenceV1({
      observation,
      sources: input.sources,
    });
    const evidence =
      deriveFieldItemWidthIllegalEvidenceFromCanonicalBlockV1(canonical);
    const source = await deriveFieldItemWidthIllegalAuthenticatedSourceV1({
      block: canonical,
      evidence,
    });
    const runtime = createManifestBoundFieldItemWidthIllegalProductionRuntimeV1(
      {
        config: input.workflow.config,
        journal: input.journal,
        observe: async () =>
          fieldItemWidthStageFromL1(
            (await input.workflow.l1.observe({ headerHash })).stage,
          ),
        resolveStage: createFieldItemWidthIllegalRawL1StageResolverV1({
          config: input.workflow.config,
          l1: input.workflow.l1,
          source,
        }),
      },
    );
    return await runtime.runOrResume(evidence);
  };

export const executeManifestBoundFieldItemWidthIllegalWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundFieldItemWidthIllegalWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FieldItemWidthStageV1> => {
  const headerHash = workflow.binding.definition.headerHash;
  const canonical = await fetchCanonicalBlockEvidenceV1({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const evidence =
    deriveFieldItemWidthIllegalEvidenceFromCanonicalBlockV1(canonical);
  const source = await deriveFieldItemWidthIllegalAuthenticatedSourceV1({
    block: canonical,
    evidence,
  });
  const centralJournal = createFieldItemWidthIllegalCentralJournalAdapterV1({
    store: journal,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    headerHash,
    decisionDigest: workflow.decisionDigest,
    transactionConfirmed: async (txHash) =>
      await workflow.l1.transactionConfirmed({ headerHash, txHash }),
  });
  const runtime = createManifestBoundFieldItemWidthIllegalProductionRuntimeV1({
    config: workflow.config,
    journal: centralJournal.familyJournal,
    observe: async () =>
      fieldItemWidthStageFromL1(
        (await workflow.l1.observe({ headerHash })).stage,
      ),
    resolveStage: createFieldItemWidthIllegalRawL1StageResolverV1({
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

export type LoadedFieldItemWidthIllegalProductionWorkflowV1 = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundFieldItemWidthIllegalWorkflowConfigV1;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadFieldItemWidthIllegalProductionWorkflowV1 = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: ProductionWorkflowAdapterReadinessInputV1;
}) => Promise<LoadedFieldItemWidthIllegalProductionWorkflowV1>;

/**
 * Family-local runner surface for central admission. It consumes only a
 * manifest/runtime path and concrete public-DA transports; neither evidence
 * nor a watcher-owned journal implementation can enter this boundary.
 */
export const createFieldItemWidthIllegalProductionWorkflowRunnerSurfaceV1 = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadFieldItemWidthIllegalProductionWorkflowV1;
}): ProductionWorkflowAdapterRunnerV1 =>
  Object.freeze({
    runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
    runOrResume: async (invocation) => {
      if (invocation.category !== "fieldItemWidthIllegal") {
        throw new Error(
          `fieldItemWidthIllegal production runner category mismatch: ${invocation.category}`,
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
          category: "fieldItemWidthIllegal",
          headerHash: invocation.headerHash,
        }),
      });
      assertProductionWorkflowJournalActuationV1({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: "fieldItemWidthIllegal",
        headerHash: invocation.headerHash,
        checkpoint: "runner_start",
      });
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      if (typeof loaded.close !== "function") {
        throw new Error(
          "fieldItemWidthIllegal runtime omitted its transport disposer",
        );
      }
      try {
        if (
          loaded.schemaVersion !==
          "midgard-production-fraud-proof-runtime-config-v1"
        ) {
          throw new Error(
            "fieldItemWidthIllegal runtime config has an unsupported schema",
          );
        }
        if (
          loaded.retainedDaSources.length === 0 ||
          loaded.retainedDaSources.some(
            (source) => !(source instanceof DaLibp2pRetainedDaSource),
          )
        ) {
          throw new Error(
            "fieldItemWidthIllegal production runner requires concrete public retained-DA sources",
          );
        }
        const workflow =
          await createManifestBoundFieldItemWidthIllegalWorkflowV1(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.category !== "fieldItemWidthIllegal" ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        ) {
          throw new Error(
            "fieldItemWidthIllegal manifest-bound workflow identity differs from invocation",
          );
        }
        return await executeManifestBoundFieldItemWidthIllegalWorkflowV1({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });
