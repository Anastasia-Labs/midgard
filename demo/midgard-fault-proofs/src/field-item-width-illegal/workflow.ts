import {
  adjudicateMidgardNativeTxFullValidity,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  type ForcedInclusionTxV1,
  forcedVerdictSubject,
  FraudProofComputationThreadStepDatum,
  type Header,
  type OutputReference,
  OutputReferenceSchema,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
  RejectionReasonSchema,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { submitCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import {
  type CanonicalBlockEvidence,
  fetchCanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItem,
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
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
} from "../workflow/actuation-permit.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/adapters.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { createFieldItemWidthIllegalCentralJournalAdapter } from "./central-journal.js";
import type { FieldItemWidthIllegalContracts } from "./contracts.js";
import {
  fieldItemWidthCoordinateIsSupported,
  type FieldItemWidthEvidence,
  fieldItemWidthEvidenceCloses,
  fieldItemWidthEvidenceIdentity,
  fieldItemWidthIsIllegal,
  type FieldItemWidthJournal,
  type FieldItemWidthStage,
  prepareFieldItemWidthEvidence,
  runFieldItemWidthProof,
} from "./field-item-width-illegal.js";
import {
  FieldItemWidthStep02DatumSchema,
  FieldItemWidthStep03DatumSchema,
} from "./schemas.js";
import { submitFieldItemWidthIllegalCancel } from "./submit-cancel.js";
import { submitFieldItemWidthIllegalStep01Accepted } from "./submit-step-01-accepted.js";
import { submitFieldItemWidthIllegalStep01Forced } from "./submit-step-01-forced.js";
import { submitFieldItemWidthIllegalStep02 } from "./submit-step-02.js";
import { submitFieldItemWidthIllegalStep03 } from "./submit-step-03.js";

export const FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW =
  "midgard-field-item-width-illegal-production-workflow-v1" as const;
export const FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID =
  "field-item-width-illegal" as const;

export const FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS = Object.freeze({
  step01: "fraudProofFieldItemWidthIllegal",
  step02: "fraudProofFieldItemWidthIllegalStep02",
  step03: "fraudProofFieldItemWidthIllegalStep03",
  computationThreadMint: "computationThreadMint",
  fraudProofMint: "fraudProofMint",
  phasMembershipWithdraw: "phasMembershipWithdraw",
  fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
} as const);

export type FieldItemWidthIllegalReferenceScripts = Readonly<{
  step01: UTxO;
  step02: UTxO;
  step03: UTxO;
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

export type ManifestBoundFieldItemWidthIllegalConfig = Readonly<{
  schemaVersion: typeof FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBinding<"fieldItemWidthIllegal">;
  contracts: FieldItemWidthIllegalContracts;
  referenceScripts: FieldItemWidthIllegalReferenceScripts;
}>;

export type LoadManifestBoundFieldItemWidthIllegalConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: FieldItemWidthIllegalReferenceScripts;
}>;

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"fieldItemWidthIllegal">;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxo({ binding, contractName, utxo });

export const bindFieldItemWidthIllegalReferenceScripts = ({
  binding,
  referenceScripts,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"fieldItemWidthIllegal">;
  readonly referenceScripts: FieldItemWidthIllegalReferenceScripts;
}): FieldItemWidthIllegalReferenceScripts => {
  const names = FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS;
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

export const loadManifestBoundFieldItemWidthIllegalConfig = async (
  input: LoadManifestBoundFieldItemWidthIllegalConfig,
): Promise<ManifestBoundFieldItemWidthIllegalConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "fieldItemWidthIllegal",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      FieldItemWidthStep02DatumSchema,
      FieldItemWidthStep03DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
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
  const referenceScripts = bindFieldItemWidthIllegalReferenceScripts({
    binding,
    referenceScripts: input.referenceScripts,
  });
  return Object.freeze({
    schemaVersion: FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW,
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
      })) as unknown as FieldItemWidthIllegalContracts["steps"],
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

export type FieldItemWidthIllegalStage = Readonly<{
  fraudulentBlockOutRef: string;
  threadOutRef?: string;
  threadUtxo?: UTxO;
  threadToken?: Readonly<{ unit: string; fraudulentHeaderHash: string }>;
  stateQueueBlockOutRef?: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
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
export const deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock = (
  block: CanonicalBlockEvidence,
): FieldItemWidthEvidence => {
  const findings: FieldItemWidthEvidence[] = [];
  const inspect = ({
    canonicalCbor,
    subject,
    forcedCoordinate,
  }: {
    readonly canonicalCbor: Uint8Array;
    readonly subject: ReturnType<typeof acceptedVerdictSubject>;
    readonly forcedCoordinate?: {
      readonly fieldIndex: number;
      readonly itemIndex: number;
    };
  }) => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(canonicalCbor);
    if (material.transactionId.toString("hex") !== subject.transaction_id) {
      throw new Error(
        "fieldItemWidthIllegal retained-DA transaction identity changed",
      );
    }
    const coordinates =
      forcedCoordinate === undefined
        ? ([2, 5] as const).flatMap((fieldIndex) =>
            decodeMidgardFieldPreimage(
              material.fieldPreimages[fieldIndex]!,
            ).map((_, itemIndex) => ({ fieldIndex, itemIndex })),
          )
        : [forcedCoordinate];
    for (const coordinate of coordinates) {
      const fieldPreimage = material.fieldPreimages[coordinate.fieldIndex]!;
      const item =
        decodeMidgardFieldPreimage(fieldPreimage)[coordinate.itemIndex];
      if (item === undefined) {
        throw new Error(
          "fieldItemWidthIllegal retained-DA reason coordinate is absent",
        );
      }
      const illegal = fieldItemWidthIsIllegal(
        coordinate.fieldIndex,
        item.length,
      );
      if (forcedCoordinate === undefined && !illegal) continue;
      const prepared = prepareFieldItemWidthEvidence({
        finding: { subject, ...coordinate },
        fieldPreimage,
        committedFieldHashHex:
          midgardFieldCommitment(fieldPreimage).toString("hex"),
      });
      if (fieldItemWidthEvidenceCloses(prepared)) findings.push(prepared);
    }
  };
  for (const transaction of block.transactions) {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(transaction.txCbor, "hex"),
    );
    inspect({
      canonicalCbor: Buffer.from(transaction.txCbor, "hex"),
      subject: acceptedVerdictSubject(material.transactionId.toString("hex")),
    });
  }
  for (const forced of block.reconstruction.forcedTransactions) {
    if (forced.value.verdict === "ForcedTxValid") continue;
    const reason = forced.value.verdict.ForcedTxInvalid.reason;
    if (typeof reason === "string" || !("FieldItemWidthIllegal" in reason))
      continue;
    const coordinate = reason.FieldItemWidthIllegal;
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      forced.fullTransactionCbor,
    );
    if (material.transactionId.toString("hex") !== forced.value.tx_id) {
      throw new Error(
        "fieldItemWidthIllegal forced retained-DA identity changed",
      );
    }
    inspect({
      canonicalCbor: forced.fullTransactionCbor,
      subject: forcedVerdictSubject({
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

export type FieldItemWidthIllegalAuthenticatedSource = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
}>;

/** Rebuilds all accepted/forced submitter material from the authenticated block. */
export const deriveFieldItemWidthIllegalAuthenticatedSource = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly evidence: FieldItemWidthEvidence;
}): Promise<FieldItemWidthIllegalAuthenticatedSource> => {
  if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
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
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "fieldItemWidthIllegal accepted source trie differs from authenticated reconstruction",
      );
    }
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
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
    Data.to(reason as never, RejectionReasonSchema as never) !==
      Data.to(
        evidence.subject.rejection_reason as never,
        RejectionReasonSchema as never,
      )
  ) {
    throw new Error(
      "fieldItemWidthIllegal forced reason differs from authenticated source",
    );
  }
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
    encodeMidgardNativeTxCanonical(
      adjudicateMidgardNativeTxFullValidity(
        decodeMidgardNativeTxFullFromCanonicalCbor(forced.fullTransactionCbor),
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
export const detectFieldItemWidthIllegalCompleteReplay = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] => {
  const accepted = evidence.transactions.flatMap(
    (transaction, transactionIndex) => {
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
        Buffer.from(transaction.txCbor, "hex"),
      );
      const transactionId = material.transactionId.toString("hex");
      if (transaction.nodeTxId !== transactionId) {
        throw new Error(
          "fieldItemWidthIllegal complete replay transaction identity changed",
        );
      }
      return ([2, 5] as const).flatMap((fieldIndex) =>
        decodeMidgardFieldPreimage(
          material.fieldPreimages[fieldIndex]!,
        ).flatMap((item, itemIndex) =>
          fieldItemWidthIsIllegal(fieldIndex, item.length)
            ? [
                {
                  detectionId: `${FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID}:${transactionIndex.toString()}:${transactionId}:${fieldIndex.toString()}:${itemIndex.toString()}:${item.length.toString()}`,
                  headerHash: evidence.headerHash,
                  violationId: FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID,
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
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
        encodeMidgardNativeTxCanonical(
          adjudicateMidgardNativeTxFullValidity(
            decodeMidgardNativeTxFullFromCanonicalCbor(
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
      if (!fieldItemWidthCoordinateIsSupported(fieldIndex, itemIndex)) {
        return [];
      }
      const preimage = material.fieldPreimages[fieldIndex];
      const item =
        preimage === undefined
          ? undefined
          : decodeMidgardFieldPreimage(preimage)[itemIndex];
      if (
        item === undefined ||
        fieldItemWidthIsIllegal(fieldIndex, item.length)
      ) {
        return [];
      }
      return [
        {
          detectionId: `${FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${fieldIndex.toString()}:${itemIndex.toString()}:${item.length.toString()}`,
          headerHash: evidence.headerHash,
          violationId: FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID,
          position: BigInt(forcedIndex),
          diagnostic: `forced transaction ${transaction.value.tx_id} was rejected for legal field ${fieldIndex.toString()} item ${itemIndex.toString()} width ${item.length.toString()}`,
        },
      ];
    },
  );
  return [...accepted, ...forced];
};

export type FieldItemWidthIllegalRuntimeLoader = Readonly<{
  config: LoadManifestBoundFieldItemWidthIllegalConfig;
  journal: FieldItemWidthJournal;
  observe: (identity: string) => Promise<FieldItemWidthStage>;
  resolveStage: (input: {
    readonly action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "removeDescendants"
      | "cancel";
    readonly evidence: FieldItemWidthEvidence;
  }) => Promise<FieldItemWidthIllegalStage>;
}>;

export const createFieldItemWidthIllegalRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundFieldItemWidthIllegalConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<"fieldItemWidthIllegal">;
    readonly source: FieldItemWidthIllegalAuthenticatedSource;
  }): FieldItemWidthIllegalRuntimeLoader["resolveStage"] =>
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
    if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_FORCED) {
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
    const thread = await requireLinearFaultThreadUtxo({
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

export const createManifestBoundFieldItemWidthIllegalSubmission = ({
  config,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundFieldItemWidthIllegalConfig;
  readonly observe: (identity: string) => Promise<FieldItemWidthStage>;
  readonly resolveStage: FieldItemWidthIllegalRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createFieldItemWidthIllegalCentralJournalAdapter
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
    evidence: FieldItemWidthEvidence,
  ) => {
    if (evidence.subject.transaction_id.length !== 64)
      throw new Error(
        "fieldItemWidthIllegal evidence transaction id is not canonical",
      );
    const familyIdentity = fieldItemWidthEvidenceIdentity(evidence);
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
      if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
        const result = await submitFieldItemWidthIllegalStep01Accepted({
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
      if (evidence.subject.source_kind !== PROOF_THREAD_SOURCE_KIND_FORCED)
        throw new Error(
          "fieldItemWidthIllegal evidence source kind is invalid",
        );
      const result = await submitFieldItemWidthIllegalStep01Forced({
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
      const result = await submitFieldItemWidthIllegalStep02({
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
      const result = await submitFieldItemWidthIllegalStep03({
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
    evidence: FieldItemWidthEvidence,
  ) => {
    const stage = await resolveStage({ action: "cancel", evidence });
    const index = current === "step01" ? 0 : current === "step02" ? 1 : 2;
    const result = await submitFieldItemWidthIllegalCancel({
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

export const loadFieldItemWidthIllegalRuntime = async (
  input: FieldItemWidthIllegalRuntimeLoader,
) => {
  const config = await loadManifestBoundFieldItemWidthIllegalConfig(
    input.config,
  );
  return createManifestBoundFieldItemWidthIllegalRuntime({
    config,
    journal: input.journal,
    observe: input.observe,
    resolveStage: input.resolveStage,
  });
};

export const createManifestBoundFieldItemWidthIllegalRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundFieldItemWidthIllegalConfig;
  readonly journal: FieldItemWidthJournal;
  readonly observe: FieldItemWidthIllegalRuntimeLoader["observe"];
  readonly resolveStage: FieldItemWidthIllegalRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createFieldItemWidthIllegalCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundFieldItemWidthIllegalSubmission({
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
    runtimeVersion: FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW,
    config,
    runOrResume: async (evidence: FieldItemWidthEvidence) =>
      await runFieldItemWidthProof({
        evidence,
        journal,
        submission,
      }),
  });
};

export type ManifestBoundFieldItemWidthIllegalWorkflowConfig =
  LoadManifestBoundFieldItemWidthIllegalConfig &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundFieldItemWidthIllegalWorkflow = Readonly<{
  workflowVersion: typeof FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW;
  config: ManifestBoundFieldItemWidthIllegalConfig;
  binding: FraudProofWorkflowDeploymentBinding<"fieldItemWidthIllegal">;
  l1: FraudProofFamilyL1ObservationPort<"fieldItemWidthIllegal">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundFieldItemWidthIllegalWorkflow = async (
  input: ManifestBoundFieldItemWidthIllegalWorkflowConfig,
): Promise<ManifestBoundFieldItemWidthIllegalWorkflow> => {
  const config = await loadManifestBoundFieldItemWidthIllegalConfig(input);
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: input.source,
    releaseFinality: config.binding.releaseFinality,
    releaseEconomics: config.binding.releaseEconomics,
    definition: config.binding.definition,
  });
  return Object.freeze({
    workflowVersion: FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW,
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
      FraudProofFamilyL1ObservationPort<"fieldItemWidthIllegal">["observe"]
    >
  >["stage"],
): FieldItemWidthStage => {
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
export const runOrResumeManifestBoundFieldItemWidthIllegalWorkflow =
  async (input: {
    readonly workflow: ManifestBoundFieldItemWidthIllegalWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FieldItemWidthJournal;
  }): Promise<FieldItemWidthStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "fieldItemWidthIllegal runner rejects caller-authored evidence inputs",
      );
    }
    const headerHash = input.workflow.binding.definition.headerHash;
    const observation = await input.workflow.l1.observeHeader({ headerHash });
    const canonical = await fetchCanonicalBlockEvidence({
      observation,
      sources: input.sources,
    });
    const evidence =
      deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock(canonical);
    const source = await deriveFieldItemWidthIllegalAuthenticatedSource({
      block: canonical,
      evidence,
    });
    const runtime = createManifestBoundFieldItemWidthIllegalRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        fieldItemWidthStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createFieldItemWidthIllegalRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

export const executeManifestBoundFieldItemWidthIllegalWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundFieldItemWidthIllegalWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FieldItemWidthStage> => {
  const headerHash = workflow.binding.definition.headerHash;
  const canonical = await fetchCanonicalBlockEvidence({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const evidence =
    deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock(canonical);
  const source = await deriveFieldItemWidthIllegalAuthenticatedSource({
    block: canonical,
    evidence,
  });
  const centralJournal = createFieldItemWidthIllegalCentralJournalAdapter({
    store: journal,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    headerHash,
    decisionDigest: workflow.decisionDigest,
    transactionConfirmed: async (txHash) =>
      await workflow.l1.transactionConfirmed({ headerHash, txHash }),
  });
  const runtime = createManifestBoundFieldItemWidthIllegalRuntime({
    config: workflow.config,
    journal: centralJournal.familyJournal,
    observe: async () =>
      fieldItemWidthStageFromL1(
        (await workflow.l1.observe({ headerHash })).stage,
      ),
    resolveStage: createFieldItemWidthIllegalRawL1StageResolver({
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

export type LoadedFieldItemWidthIllegalWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundFieldItemWidthIllegalWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadFieldItemWidthIllegalWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedFieldItemWidthIllegalWorkflow>;

/**
 * Family-local runner surface for central admission. It consumes only a
 * manifest/runtime path and concrete public-DA transports; neither evidence
 * nor a watcher-owned journal implementation can enter this boundary.
 */
export const createFieldItemWidthIllegalWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadFieldItemWidthIllegalWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== "fieldItemWidthIllegal") {
        throw new Error(
          `fieldItemWidthIllegal production runner category mismatch: ${invocation.category}`,
        );
      }
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "fieldItemWidthIllegal",
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
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
        const workflow = await createManifestBoundFieldItemWidthIllegalWorkflow(
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
        return await executeManifestBoundFieldItemWidthIllegalWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });
