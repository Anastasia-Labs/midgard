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
  type FraudProofCatalogueCategoryName,
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
import { createTransactionOutputNonCanonicalCentralJournalAdapter } from "./central-journal.js";
import type { TransactionOutputNonCanonicalContracts } from "./contracts.js";
import {
  TransactionOutputStep02DatumSchema,
  TransactionOutputStep03DatumSchema,
  TransactionOutputStep04DatumSchema,
} from "./schemas.js";
import { submitTransactionOutputNonCanonicalCancel } from "./submit-cancel.js";
import { submitTransactionOutputNonCanonicalStep01Accepted } from "./submit-step-01-accepted.js";
import { submitTransactionOutputNonCanonicalStep01Forced } from "./submit-step-01-forced.js";
import { submitTransactionOutputNonCanonicalStep02 } from "./submit-step-02.js";
import { submitTransactionOutputNonCanonicalStep03 } from "./submit-step-03.js";
import { submitTransactionOutputNonCanonicalStep04 } from "./submit-step-04.js";
import {
  prepareTransactionOutputEvidence,
  runTransactionOutputProof,
  type TransactionOutputEvidence,
  transactionOutputEvidenceCloses,
  transactionOutputEvidenceIdentity,
  type TransactionOutputJournal,
  type TransactionOutputStage,
} from "./transaction-output-non-canonical.js";

export const TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW =
  "midgard-transaction-output-non-canonical-production-workflow-v1" as const;
export const TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID =
  "transaction-output-non-canonical" as const;

export const TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS =
  Object.freeze({
    step01: "fraudProofTransactionOutputNonCanonical",
    step02: "fraudProofTransactionOutputNonCanonicalStep02",
    step03: "fraudProofTransactionOutputNonCanonicalStep03",
    step04: "fraudProofTransactionOutputNonCanonicalStep04",
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
  } as const);

export type TransactionOutputNonCanonicalReferenceScripts = Readonly<{
  step01: UTxO;
  step02: UTxO;
  step03: UTxO;
  step04: UTxO;
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

export type ManifestBoundTransactionOutputNonCanonicalConfig = Readonly<{
  schemaVersion: typeof TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBinding<"transactionOutputNonCanonical">;
  contracts: TransactionOutputNonCanonicalContracts;
  referenceScripts: TransactionOutputNonCanonicalReferenceScripts;
}>;

export type LoadManifestBoundTransactionOutputNonCanonicalConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: TransactionOutputNonCanonicalReferenceScripts;
}>;

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"transactionOutputNonCanonical">;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxo({ binding, contractName, utxo });

export const bindTransactionOutputNonCanonicalReferenceScripts = ({
  binding,
  referenceScripts,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"transactionOutputNonCanonical">;
  readonly referenceScripts: TransactionOutputNonCanonicalReferenceScripts;
}): TransactionOutputNonCanonicalReferenceScripts => {
  const names = TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS;
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

export const loadManifestBoundTransactionOutputNonCanonicalConfig = async (
  input: LoadManifestBoundTransactionOutputNonCanonicalConfig,
): Promise<ManifestBoundTransactionOutputNonCanonicalConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "transactionOutputNonCanonical",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      TransactionOutputStep02DatumSchema,
      TransactionOutputStep03DatumSchema,
      TransactionOutputStep04DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });
  const localContracts = binding.resolvedContracts.contracts as unknown as {
    readonly transactionOutputNonCanonical?: TransactionOutputNonCanonicalContracts;
  };
  const chain = localContracts.transactionOutputNonCanonical;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined || chain.steps.length !== 4) {
    throw new Error(
      "transactionOutputNonCanonical deployment changed its four-step topology",
    );
  }
  if (certificate === null) {
    throw new Error(
      "transactionOutputNonCanonical deployment omitted field-preimage certificate",
    );
  }
  const referenceScripts = bindTransactionOutputNonCanonicalReferenceScripts({
    binding,
    referenceScripts: input.referenceScripts,
  });
  return Object.freeze({
    schemaVersion: TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW,
    lucid: input.lucid,
    signer: input.signer,
    binding,
    contracts: {
      steps: chain.steps.map((step, index) => ({
        ...step,
        blueprintTitle: [
          "fraud_proofs/transaction_output_non_canonical/step_01.main.spend",
          "fraud_proofs/transaction_output_non_canonical/step_02.main.spend",
          "fraud_proofs/transaction_output_non_canonical/step_03.main.spend",
          "fraud_proofs/transaction_output_non_canonical/step_04.main.spend",
        ][index]!,
        referenceOutRef: [
          referenceScripts.step01,
          referenceScripts.step02,
          referenceScripts.step03,
          referenceScripts.step04,
        ][index]!.txHash.concat(
          "#",
          [
            referenceScripts.step01,
            referenceScripts.step02,
            referenceScripts.step03,
            referenceScripts.step04,
          ][index]!.outputIndex.toString(),
        ),
      })) as unknown as TransactionOutputNonCanonicalContracts["steps"],
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

export type TransactionOutputNonCanonicalStage = Readonly<{
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
export const deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock = (
  block: CanonicalBlockEvidence,
): TransactionOutputEvidence => {
  const findings: TransactionOutputEvidence[] = [];
  const inspect = ({
    canonicalCbor,
    subject,
    forcedCoordinate,
  }: {
    readonly canonicalCbor: Uint8Array;
    readonly subject: ReturnType<typeof acceptedVerdictSubject>;
    readonly forcedCoordinate?: { readonly itemIndex: number };
  }) => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(canonicalCbor);
    if (material.transactionId.toString("hex") !== subject.transaction_id) {
      throw new Error(
        "transactionOutputNonCanonical retained-DA transaction identity changed",
      );
    }
    const coordinates =
      forcedCoordinate === undefined
        ? decodeMidgardFieldPreimage(material.fieldPreimages[2]!).map(
            (_, itemIndex) => ({ itemIndex }),
          )
        : [forcedCoordinate];
    for (const coordinate of coordinates) {
      const fieldPreimage = material.fieldPreimages[2]!;
      const item =
        decodeMidgardFieldPreimage(fieldPreimage)[coordinate.itemIndex];
      if (item === undefined) {
        throw new Error(
          "transactionOutputNonCanonical retained-DA reason coordinate is absent",
        );
      }
      if (item.length > 16_384) continue;
      const prepared = prepareTransactionOutputEvidence({
        finding: { subject, fieldIndex: 2, itemIndex: coordinate.itemIndex },
        fieldPreimage,
        committedFieldHashHex:
          midgardFieldCommitment(fieldPreimage).toString("hex"),
      });
      if (transactionOutputEvidenceCloses(prepared)) findings.push(prepared);
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
    if (typeof reason === "string" || !("OutputNonCanonical" in reason))
      continue;
    const coordinate = reason.OutputNonCanonical;
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      forced.fullTransactionCbor,
    );
    if (material.transactionId.toString("hex") !== forced.value.tx_id) {
      throw new Error(
        "transactionOutputNonCanonical forced retained-DA identity changed",
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
        itemIndex: Number(coordinate.output_index),
      },
    });
  }
  if (findings.length !== 1) {
    throw new Error(
      `transactionOutputNonCanonical public retained DA yielded ${findings.length.toString()} exact findings`,
    );
  }
  return findings[0]!;
};

export type TransactionOutputNonCanonicalAuthenticatedSource = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
}>;

/** Rebuilds all accepted/forced submitter material from the authenticated block. */
export const deriveTransactionOutputNonCanonicalAuthenticatedSource = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly evidence: TransactionOutputEvidence;
}): Promise<TransactionOutputNonCanonicalAuthenticatedSource> => {
  if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.subject.transaction_id,
    );
    if (selected === undefined) {
      throw new Error(
        "transactionOutputNonCanonical accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "transactionOutputNonCanonical accepted source trie differs from authenticated reconstruction",
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
          "transactionOutputNonCanonical accepted transaction",
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
      "transactionOutputNonCanonical forced subject disappeared from retained DA",
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
      "transactionOutputNonCanonical forced reason differs from authenticated source",
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
      "transactionOutputNonCanonical forced source material differs from authenticated leaf",
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
export const detectTransactionOutputNonCanonicalCompleteReplay = (
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
          "transactionOutputNonCanonical complete replay transaction identity changed",
        );
      }
      const fieldIndex = 2 as const;
      const fieldPreimage = material.fieldPreimages[fieldIndex]!;
      return decodeMidgardFieldPreimage(fieldPreimage).flatMap(
        (item, itemIndex) => {
          if (item.length > 16_384) return [];
          const prepared = prepareTransactionOutputEvidence({
            finding: {
              subject: acceptedVerdictSubject(transactionId),
              fieldIndex,
              itemIndex,
            },
            fieldPreimage,
            committedFieldHashHex:
              midgardFieldCommitment(fieldPreimage).toString("hex"),
          });
          return prepared.decisiveFaultHolds
            ? [
                {
                  detectionId: `${TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID}:${transactionIndex.toString()}:${transactionId}:${fieldIndex.toString()}:${itemIndex.toString()}:${item.length.toString()}`,
                  headerHash: evidence.headerHash,
                  violationId: TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID,
                  position: BigInt(transactionIndex),
                  diagnostic: `transaction ${transactionId} field ${fieldIndex.toString()} item ${itemIndex.toString()} has illegal width ${item.length.toString()}`,
                },
              ]
            : [];
        },
      );
    },
  );
  const forced = evidence.reconstruction.forcedTransactions.flatMap(
    (transaction, forcedIndex) => {
      if (transaction.value.verdict === "ForcedTxValid") return [];
      const reason = transaction.value.verdict.ForcedTxInvalid.reason;
      if (typeof reason === "string" || !("OutputNonCanonical" in reason)) {
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
          "transactionOutputNonCanonical forced transaction differs from its authenticated leaf",
        );
      }
      const coordinate = reason.OutputNonCanonical;
      const fieldIndex = 2 as const;
      const itemIndex = Number(coordinate.output_index);
      const preimage = material.fieldPreimages[fieldIndex];
      const item =
        preimage === undefined
          ? undefined
          : decodeMidgardFieldPreimage(preimage)[itemIndex];
      if (item === undefined || item.length > 16_384) {
        return [];
      }
      const fieldPreimage = material.fieldPreimages[fieldIndex]!;
      const prepared = prepareTransactionOutputEvidence({
        finding: {
          subject: forcedVerdictSubject({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: reason,
          }),
          fieldIndex,
          itemIndex,
        },
        fieldPreimage,
        committedFieldHashHex:
          midgardFieldCommitment(fieldPreimage).toString("hex"),
      });
      if (!transactionOutputEvidenceCloses(prepared)) return [];
      return [
        {
          detectionId: `${TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${fieldIndex.toString()}:${itemIndex.toString()}:${item.length.toString()}`,
          headerHash: evidence.headerHash,
          violationId: TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID,
          position: BigInt(forcedIndex),
          diagnostic: `forced transaction ${transaction.value.tx_id} was rejected for legal field ${fieldIndex.toString()} item ${itemIndex.toString()} width ${item.length.toString()}`,
        },
      ];
    },
  );
  return [...accepted, ...forced];
};

export type TransactionOutputNonCanonicalRuntimeLoader = Readonly<{
  config: LoadManifestBoundTransactionOutputNonCanonicalConfig;
  journal: TransactionOutputJournal;
  observe: (identity: string) => Promise<TransactionOutputStage>;
  resolveStage: (input: {
    readonly action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "submitStep04"
      | "removeDescendants"
      | "cancel";
    readonly evidence: TransactionOutputEvidence;
  }) => Promise<TransactionOutputNonCanonicalStage>;
}>;

export const createTransactionOutputNonCanonicalRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundTransactionOutputNonCanonicalConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
    readonly source: TransactionOutputNonCanonicalAuthenticatedSource;
  }): TransactionOutputNonCanonicalRuntimeLoader["resolveStage"] =>
  async ({ action, evidence }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "transactionOutputNonCanonical init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "transactionOutputNonCanonical removal requires raw-L1 proof token",
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
            : 4;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `transactionOutputNonCanonical ${action} differs from authenticated raw-L1 stage`,
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
      family: "transaction-output-non-canonical",
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
    throw new Error(`transactionOutputNonCanonical missing ${label}`);
  return value;
};

export const createManifestBoundTransactionOutputNonCanonicalSubmission = ({
  config,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundTransactionOutputNonCanonicalConfig;
  readonly observe: (identity: string) => Promise<TransactionOutputStage>;
  readonly resolveStage: TransactionOutputNonCanonicalRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createTransactionOutputNonCanonicalCentralJournalAdapter
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
      | "submitStep04"
      | "removeDescendants",
    evidence: TransactionOutputEvidence,
  ) => {
    if (evidence.subject.transaction_id.length !== 64)
      throw new Error(
        "transactionOutputNonCanonical evidence transaction id is not canonical",
      );
    const familyIdentity = transactionOutputEvidenceIdentity(evidence);
    const transition =
      action === "submitInit"
        ? (["none", "step01"] as const)
        : action === "submitStep01"
          ? (["step01", "step02"] as const)
          : action === "submitStep02"
            ? (["step02", "step03"] as const)
            : action === "submitStep03"
              ? (["step03", "step04"] as const)
              : action === "submitStep04"
                ? (["step04", "proven"] as const)
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
        const result = await submitTransactionOutputNonCanonicalStep01Accepted({
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
          "transactionOutputNonCanonical evidence source kind is invalid",
        );
      const result = await submitTransactionOutputNonCanonicalStep01Forced({
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
      const result = await submitTransactionOutputNonCanonicalStep02({
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
      const auxiliaryHashes: string[] = [];
      const result = await submitTransactionOutputNonCanonicalStep03({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step03 thread out-ref"),
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
          "step03",
          auxiliaryHashes,
        ),
        certificatePreSubmitBoundary: centralJournal?.auxiliaryBoundary(
          "certificate",
          familyIdentity,
          "step03",
          auxiliaryHashes,
        ),
        onCarriageReady:
          centralJournal === undefined
            ? undefined
            : async () => {
                for (const txHash of auxiliaryHashes)
                  await centralJournal.confirmAuxiliary(txHash);
              },
        referenceScriptUtxo: config.referenceScripts.step03,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: result.terminal ? ("step04" as const) : ("step03" as const),
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep04") {
      const result = await submitTransactionOutputNonCanonicalStep04({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step04 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step04,
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
        "transactionOutputNonCanonical" as FraudProofCatalogueCategoryName,
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "transactionOutputNonCanonical production removal requires a state-queue mutation lease coordinator",
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
    current: "step01" | "step02" | "step03" | "step04",
    evidence: TransactionOutputEvidence,
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
    const result = await submitTransactionOutputNonCanonicalCancel({
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

export const loadTransactionOutputNonCanonicalRuntime = async (
  input: TransactionOutputNonCanonicalRuntimeLoader,
) => {
  const config = await loadManifestBoundTransactionOutputNonCanonicalConfig(
    input.config,
  );
  return createManifestBoundTransactionOutputNonCanonicalRuntime({
    config,
    journal: input.journal,
    observe: input.observe,
    resolveStage: input.resolveStage,
  });
};

export const createManifestBoundTransactionOutputNonCanonicalRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundTransactionOutputNonCanonicalConfig;
  readonly journal: TransactionOutputJournal;
  readonly observe: TransactionOutputNonCanonicalRuntimeLoader["observe"];
  readonly resolveStage: TransactionOutputNonCanonicalRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createTransactionOutputNonCanonicalCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundTransactionOutputNonCanonicalSubmission(
    {
      config,
      observe: async (identity) => {
        const observed = await observe(identity);
        await centralJournal?.reconcile(observed);
        return observed;
      },
      resolveStage,
      centralJournal,
      stateQueueMutationLeaseCoordinator,
    },
  );
  return Object.freeze({
    runtimeVersion: TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW,
    config,
    runOrResume: async (evidence: TransactionOutputEvidence) =>
      await runTransactionOutputProof({
        evidence,
        journal,
        submission,
      }),
  });
};

export type ManifestBoundTransactionOutputNonCanonicalWorkflowConfig =
  LoadManifestBoundTransactionOutputNonCanonicalConfig &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundTransactionOutputNonCanonicalWorkflow = Readonly<{
  workflowVersion: typeof TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW;
  config: ManifestBoundTransactionOutputNonCanonicalConfig;
  binding: FraudProofWorkflowDeploymentBinding<"transactionOutputNonCanonical">;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundTransactionOutputNonCanonicalWorkflow = async (
  input: ManifestBoundTransactionOutputNonCanonicalWorkflowConfig,
): Promise<ManifestBoundTransactionOutputNonCanonicalWorkflow> => {
  const config =
    await loadManifestBoundTransactionOutputNonCanonicalConfig(input);
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: input.source,
    releaseFinality: config.binding.releaseFinality,
    releaseEconomics: config.binding.releaseEconomics,
    definition: config.binding.definition,
  });
  return Object.freeze({
    workflowVersion: TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

const transactionOutputStageFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>["observe"]
    >
  >["stage"],
): TransactionOutputStage => {
  switch (stage.kind) {
    case "not_started":
      return "none";
    case "step":
      if (stage.step === 1) return "step01";
      if (stage.step === 2) return "step02";
      if (stage.step === 3) return "step03";
      if (stage.step === 4) return "step04";
      throw new Error(
        "transactionOutputNonCanonical L1 stage exceeds four-step topology",
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
export const runOrResumeManifestBoundTransactionOutputNonCanonicalWorkflow =
  async (input: {
    readonly workflow: ManifestBoundTransactionOutputNonCanonicalWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: TransactionOutputJournal;
  }): Promise<TransactionOutputStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "transactionOutputNonCanonical runner rejects caller-authored evidence inputs",
      );
    }
    const headerHash = input.workflow.binding.definition.headerHash;
    const observation = await input.workflow.l1.observeHeader({ headerHash });
    const canonical = await fetchCanonicalBlockEvidence({
      observation,
      sources: input.sources,
    });
    const evidence =
      deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock(canonical);
    const source = await deriveTransactionOutputNonCanonicalAuthenticatedSource(
      {
        block: canonical,
        evidence,
      },
    );
    const runtime = createManifestBoundTransactionOutputNonCanonicalRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        transactionOutputStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createTransactionOutputNonCanonicalRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

export const executeManifestBoundTransactionOutputNonCanonicalWorkflow =
  async ({
    workflow,
    sources,
    journal,
  }: {
    readonly workflow: ManifestBoundTransactionOutputNonCanonicalWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FraudProofWorkflowJournalStore;
  }): Promise<TransactionOutputStage> => {
    const headerHash = workflow.binding.definition.headerHash;
    const canonical = await fetchCanonicalBlockEvidence({
      observation: await workflow.l1.observeHeader({ headerHash }),
      sources,
    });
    const evidence =
      deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock(canonical);
    const source = await deriveTransactionOutputNonCanonicalAuthenticatedSource(
      {
        block: canonical,
        evidence,
      },
    );
    const centralJournal =
      createTransactionOutputNonCanonicalCentralJournalAdapter({
        store: journal,
        deploymentFingerprint: workflow.binding.deploymentFingerprint,
        headerHash,
        decisionDigest: workflow.decisionDigest,
        transactionConfirmed: async (txHash) =>
          await workflow.l1.transactionConfirmed({ headerHash, txHash }),
      });
    const runtime = createManifestBoundTransactionOutputNonCanonicalRuntime({
      config: workflow.config,
      journal: centralJournal.familyJournal,
      observe: async () =>
        transactionOutputStageFromL1(
          (await workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createTransactionOutputNonCanonicalRawL1StageResolver({
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

export type LoadedTransactionOutputNonCanonicalWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundTransactionOutputNonCanonicalWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadTransactionOutputNonCanonicalWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedTransactionOutputNonCanonicalWorkflow>;

/**
 * Family-local runner surface for central admission. It consumes only a
 * manifest/runtime path and concrete public-DA transports; neither evidence
 * nor a watcher-owned journal implementation can enter this boundary.
 */
export const createTransactionOutputNonCanonicalWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadTransactionOutputNonCanonicalWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "transactionOutputNonCanonical") {
        throw new Error(
          `transactionOutputNonCanonical production runner category mismatch: ${invocation.category}`,
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
          category:
            "transactionOutputNonCanonical" as FraudProofCatalogueCategoryName,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category:
          "transactionOutputNonCanonical" as FraudProofCatalogueCategoryName,
        headerHash: invocation.headerHash,
        checkpoint: "runner_start",
      });
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      if (typeof loaded.close !== "function") {
        throw new Error(
          "transactionOutputNonCanonical runtime omitted its transport disposer",
        );
      }
      try {
        if (
          loaded.schemaVersion !==
          "midgard-production-fraud-proof-runtime-config-v1"
        ) {
          throw new Error(
            "transactionOutputNonCanonical runtime config has an unsupported schema",
          );
        }
        if (
          loaded.retainedDaSources.length === 0 ||
          loaded.retainedDaSources.some(
            (source) => !(source instanceof DaLibp2pRetainedDaSource),
          )
        ) {
          throw new Error(
            "transactionOutputNonCanonical production runner requires concrete public retained-DA sources",
          );
        }
        const workflow =
          await createManifestBoundTransactionOutputNonCanonicalWorkflow(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          String(workflow.binding.definition.category) !==
            "transactionOutputNonCanonical" ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        ) {
          throw new Error(
            "transactionOutputNonCanonical manifest-bound workflow identity differs from invocation",
          );
        }
        return await executeManifestBoundTransactionOutputNonCanonicalWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });
