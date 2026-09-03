import {
  adjudicateMidgardNativeTxFullValidity,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardNativeTxWitnessSetCompact,
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
  WitnessScriptDecodingStep02DatumSchema,
  WitnessScriptDecodingStep03DatumSchema,
  WitnessScriptDecodingStep04DatumSchema,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

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
import { createWitnessScriptDecodingCentralJournalAdapter } from "./central-journal.js";
import type { WitnessScriptDecodingContracts } from "./contracts.js";
import { submitWitnessScriptDecodingCancel } from "./submit-cancel.js";
import { submitWitnessScriptDecodingInit } from "./submit-init.js";
import { submitWitnessScriptDecodingStep01Accepted } from "./submit-step-01.js";
import { submitWitnessScriptDecodingStep01Forced } from "./submit-step-01.js";
import { submitWitnessScriptDecodingStep02 } from "./submit-step-02.js";
import { submitWitnessScriptDecodingStep03 } from "./submit-step-03.js";
import { submitWitnessScriptDecodingStep04 } from "./submit-step-04.js";
import {
  prepareWitnessScriptDecodingEvidence,
  runWitnessScriptDecodingProof,
  type WitnessScriptDecodingAction,
  type WitnessScriptDecodingEvidence,
  witnessScriptDecodingEvidenceCloses,
  witnessScriptDecodingEvidenceIdentity,
  type WitnessScriptDecodingJournalEntry,
  WitnessScriptDecodingResultClasses,
  type WitnessScriptDecodingStage,
} from "./witness-script-decoding.js";

type WitnessScriptDecodingJournal = Readonly<{
  load: (
    identity: string,
  ) => Promise<readonly WitnessScriptDecodingJournalEntry[]>;
  append: (entry: WitnessScriptDecodingJournalEntry) => Promise<void>;
}>;

export const WITNESS_SCRIPT_DECODING_WORKFLOW =
  "midgard-witness-script-decoding-production-workflow-v1" as const;
export const WITNESS_SCRIPT_DECODING_VIOLATION_IDS = Object.freeze({
  HeaderMalformed: "witness-script-header-malformed",
  NativeMalformed: "witness-native-script-malformed",
  NodeLimit: "witness-native-script-node-limit",
  DepthLimit: "witness-native-script-depth-limit",
} as const);

export const witnessScriptDecodingViolationId = (
  resultClass: number,
): (typeof WITNESS_SCRIPT_DECODING_VIOLATION_IDS)[keyof typeof WITNESS_SCRIPT_DECODING_VIOLATION_IDS] => {
  if (resultClass === WitnessScriptDecodingResultClasses.HeaderMalformed)
    return WITNESS_SCRIPT_DECODING_VIOLATION_IDS.HeaderMalformed;
  if (resultClass === WitnessScriptDecodingResultClasses.NativeMalformed)
    return WITNESS_SCRIPT_DECODING_VIOLATION_IDS.NativeMalformed;
  if (resultClass === WitnessScriptDecodingResultClasses.NodeLimit)
    return WITNESS_SCRIPT_DECODING_VIOLATION_IDS.NodeLimit;
  if (resultClass === WitnessScriptDecodingResultClasses.DepthLimit)
    return WITNESS_SCRIPT_DECODING_VIOLATION_IDS.DepthLimit;
  throw new Error(
    "witnessScriptDecoding result has no exact classifier violation ID",
  );
};

export const WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS = Object.freeze({
  step01: "fraudProofWitnessScriptDecoding",
  step02: "fraudProofWitnessScriptDecodingStep02",
  step03: "fraudProofWitnessScriptDecodingStep03",
  step04: "fraudProofWitnessScriptDecodingStep04",
  computationThreadMint: "computationThreadMint",
  fraudProofMint: "fraudProofMint",
  phasMembershipWithdraw: "phasMembershipWithdraw",
  fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
} as const);

export type WitnessScriptDecodingReferenceScripts = Readonly<{
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

export type ManifestBoundWitnessScriptDecodingConfig = Readonly<{
  schemaVersion: typeof WITNESS_SCRIPT_DECODING_WORKFLOW;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBinding<"witnessScriptDecoding">;
  contracts: WitnessScriptDecodingContracts;
  referenceScripts: WitnessScriptDecodingReferenceScripts;
}>;

export type LoadManifestBoundWitnessScriptDecodingConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: WitnessScriptDecodingReferenceScripts;
}>;

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"witnessScriptDecoding">;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxo({ binding, contractName, utxo });

export const bindWitnessScriptDecodingReferenceScripts = ({
  binding,
  referenceScripts,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"witnessScriptDecoding">;
  readonly referenceScripts: WitnessScriptDecodingReferenceScripts;
}): WitnessScriptDecodingReferenceScripts => {
  const names = WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS;
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

export const loadManifestBoundWitnessScriptDecodingConfig = async (
  input: LoadManifestBoundWitnessScriptDecodingConfig,
): Promise<ManifestBoundWitnessScriptDecodingConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "witnessScriptDecoding",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      WitnessScriptDecodingStep02DatumSchema,
      WitnessScriptDecodingStep03DatumSchema,
      WitnessScriptDecodingStep04DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.witnessScriptDecoding;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined || chain.steps.length !== 4) {
    throw new Error(
      "witnessScriptDecoding deployment changed its four-step topology",
    );
  }
  if (certificate === null) {
    throw new Error(
      "witnessScriptDecoding deployment omitted field-preimage certificate",
    );
  }
  const referenceScripts = bindWitnessScriptDecodingReferenceScripts({
    binding,
    referenceScripts: input.referenceScripts,
  });
  return Object.freeze({
    schemaVersion: WITNESS_SCRIPT_DECODING_WORKFLOW,
    lucid: input.lucid,
    signer: input.signer,
    binding,
    contracts: {
      steps: chain.steps.map((step, index) => ({
        ...step,
        blueprintTitle: [
          "fraud_proofs/witness_script_decoding/step_01.main.spend",
          "fraud_proofs/witness_script_decoding/step_02.main.spend",
          "fraud_proofs/witness_script_decoding/step_03.main.spend",
          "fraud_proofs/witness_script_decoding/step_04.main.spend",
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
      })) as unknown as WitnessScriptDecodingContracts["steps"],
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

export type WitnessScriptDecodingAuthenticatedStage = Readonly<{
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
export const deriveWitnessScriptDecodingEvidenceFromCanonicalBlock = (
  block: CanonicalBlockEvidence,
): WitnessScriptDecodingEvidence => {
  const findings: WitnessScriptDecodingEvidence[] = [];
  const inspect = ({
    canonicalCbor,
    subject,
    forcedScriptIndex,
  }: {
    readonly canonicalCbor: Uint8Array;
    readonly subject: ReturnType<typeof acceptedVerdictSubject>;
    readonly forcedScriptIndex?: number;
  }) => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(canonicalCbor);
    if (material.transactionId.toString("hex") !== subject.transaction_id)
      throw new Error(
        "witnessScriptDecoding retained-DA transaction identity changed",
      );
    const fieldPreimage = material.fieldPreimages[6]!;
    const items = decodeMidgardFieldPreimage(fieldPreimage);
    const coordinates =
      forcedScriptIndex === undefined
        ? items.map((_, scriptIndex) => scriptIndex)
        : [forcedScriptIndex];
    for (const scriptIndex of coordinates) {
      if (items[scriptIndex] === undefined)
        throw new Error(
          "witnessScriptDecoding retained-DA reason coordinate is absent",
        );
      const prepared = prepareWitnessScriptDecodingEvidence({
        finding: {
          subject,
          witnessSetHash: decodeMidgardNativeTxCompact(
            material.proofSource.compactCbor,
          ).transactionWitnessSetHash.toString("hex"),
          scriptIndex,
        },
        fieldPreimage,
        committedFieldHashHex:
          midgardFieldCommitment(fieldPreimage).toString("hex"),
      });
      if (witnessScriptDecodingEvidenceCloses(prepared))
        findings.push(prepared);
    }
  };
  for (const transaction of block.transactions) {
    const cbor = Buffer.from(transaction.txCbor, "hex");
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(cbor);
    inspect({
      canonicalCbor: cbor,
      subject: acceptedVerdictSubject(material.transactionId.toString("hex")),
    });
  }
  for (const forced of block.reconstruction.forcedTransactions) {
    if (forced.value.verdict === "ForcedTxValid") continue;
    const reason = forced.value.verdict.ForcedTxInvalid.reason;
    if (typeof reason === "string") continue;
    const payload =
      "WitnessScriptHeaderMalformed" in reason
        ? reason.WitnessScriptHeaderMalformed
        : "WitnessNativeScriptMalformed" in reason
          ? reason.WitnessNativeScriptMalformed
          : "WitnessNativeScriptNodeLimit" in reason
            ? reason.WitnessNativeScriptNodeLimit
            : "WitnessNativeScriptDepthLimit" in reason
              ? reason.WitnessNativeScriptDepthLimit
              : undefined;
    if (payload === undefined) continue;
    inspect({
      canonicalCbor: encodeMidgardNativeTxCanonical(
        adjudicateMidgardNativeTxFullValidity(
          decodeMidgardNativeTxFullFromCanonicalCbor(
            forced.fullTransactionCbor,
          ),
          "TxIsInvalid",
        ),
      ),
      subject: forcedVerdictSubject({
        transactionId: forced.value.tx_id,
        sourceKey: forced.key,
        rejectionReason: reason,
      }) as ReturnType<typeof acceptedVerdictSubject>,
      forcedScriptIndex: Number(payload.script_index),
    });
  }
  if (findings.length !== 1)
    throw new Error(
      `witnessScriptDecoding public retained DA yielded ${findings.length.toString()} exact findings`,
    );
  return findings[0]!;
};

export type WitnessScriptDecodingAuthenticatedSource = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
}>;

/** Rebuilds all accepted/forced submitter material from the authenticated block. */
export const deriveWitnessScriptDecodingAuthenticatedSource = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly evidence: WitnessScriptDecodingEvidence;
}): Promise<WitnessScriptDecodingAuthenticatedSource> => {
  if (
    evidence.finding.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED
  ) {
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.finding.subject.transaction_id,
    );
    if (selected === undefined) {
      throw new Error(
        "witnessScriptDecoding accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "witnessScriptDecoding accepted source trie differs from authenticated reconstruction",
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
          "witnessScriptDecoding accepted transaction",
        ),
      }),
    });
  }
  const forced = block.reconstruction.forcedTransactions.find(
    ({ key, value }) =>
      value.tx_id === evidence.finding.subject.transaction_id &&
      Data.to(key as never, OutputReferenceSchema as never) ===
        Data.to(
          evidence.finding.subject.source_key as never,
          OutputReferenceSchema as never,
        ),
  );
  if (forced === undefined || forced.value.verdict === "ForcedTxValid") {
    throw new Error(
      "witnessScriptDecoding forced subject disappeared from retained DA",
    );
  }
  const reason = forced.value.verdict.ForcedTxInvalid.reason;
  if (
    evidence.finding.subject.rejection_reason === null ||
    Data.to(reason as never, RejectionReasonSchema as never) !==
      Data.to(
        evidence.finding.subject.rejection_reason as never,
        RejectionReasonSchema as never,
      )
  ) {
    throw new Error(
      "witnessScriptDecoding forced reason differs from authenticated source",
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
      "witnessScriptDecoding forced source material differs from authenticated leaf",
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

/** Complete replay member: scans every accepted field-6 item and exact forced coordinate. */
export const detectWitnessScriptDecodingCompleteReplay = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] => {
  const detections: CanonicalViolationDetection[] = [];
  for (const [
    transactionIndex,
    transaction,
  ] of evidence.transactions.entries()) {
    const cbor = Buffer.from(transaction.txCbor, "hex");
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(cbor);
    const transactionId = material.transactionId.toString("hex");
    if (transaction.nodeTxId !== transactionId)
      throw new Error(
        "witnessScriptDecoding complete replay transaction identity changed",
      );
    const field = material.fieldPreimages[6]!;
    const witnessSetHash = decodeMidgardNativeTxCompact(
      material.proofSource.compactCbor,
    ).transactionWitnessSetHash.toString("hex");
    for (const [scriptIndex] of decodeMidgardFieldPreimage(field).entries()) {
      const prepared = prepareWitnessScriptDecodingEvidence({
        finding: {
          subject: acceptedVerdictSubject(transactionId),
          witnessSetHash,
          scriptIndex,
        },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      });
      if (!witnessScriptDecodingEvidenceCloses(prepared)) continue;
      const violationId = witnessScriptDecodingViolationId(
        prepared.resultClass,
      );
      detections.push({
        detectionId: `${violationId}:${transactionIndex.toString()}:${transactionId}:${scriptIndex.toString()}:${prepared.resultClass.toString()}`,
        headerHash: evidence.headerHash,
        violationId,
        position: BigInt(transactionIndex),
        diagnostic: `accepted transaction ${transactionId} has undecodable field-6 script ${scriptIndex.toString()}`,
      });
    }
  }
  for (const [
    forcedIndex,
    transaction,
  ] of evidence.reconstruction.forcedTransactions.entries()) {
    if (transaction.value.verdict === "ForcedTxValid") continue;
    const reason = transaction.value.verdict.ForcedTxInvalid.reason;
    if (typeof reason === "string") continue;
    const payload =
      "WitnessScriptHeaderMalformed" in reason
        ? reason.WitnessScriptHeaderMalformed
        : "WitnessNativeScriptMalformed" in reason
          ? reason.WitnessNativeScriptMalformed
          : "WitnessNativeScriptNodeLimit" in reason
            ? reason.WitnessNativeScriptNodeLimit
            : "WitnessNativeScriptDepthLimit" in reason
              ? reason.WitnessNativeScriptDepthLimit
              : undefined;
    if (payload === undefined) continue;
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
    )
      throw new Error(
        "witnessScriptDecoding forced transaction differs from its authenticated leaf",
      );
    const field = material.fieldPreimages[6]!;
    const scriptIndex = Number(payload.script_index);
    if (decodeMidgardFieldPreimage(field)[scriptIndex] === undefined) continue;
    const prepared = prepareWitnessScriptDecodingEvidence({
      finding: {
        subject: forcedVerdictSubject({
          transactionId: transaction.value.tx_id,
          sourceKey: transaction.key,
          rejectionReason: reason,
        }),
        witnessSetHash: decodeMidgardNativeTxCompact(
          material.proofSource.compactCbor,
        ).transactionWitnessSetHash.toString("hex"),
        scriptIndex,
      },
      fieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
    });
    if (!witnessScriptDecodingEvidenceCloses(prepared)) continue;
    const violationId = witnessScriptDecodingViolationId(
      prepared.finding.accusedClass,
    );
    detections.push({
      detectionId: `${violationId}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${scriptIndex.toString()}:${prepared.resultClass.toString()}`,
      headerHash: evidence.headerHash,
      violationId,
      position: BigInt(forcedIndex),
      diagnostic: `forced transaction ${transaction.value.tx_id} was rejected for a decodable field-6 script ${scriptIndex.toString()}`,
    });
  }
  return detections;
};

export type WitnessScriptDecodingRuntimeLoader = Readonly<{
  config: LoadManifestBoundWitnessScriptDecodingConfig;
  journal: WitnessScriptDecodingJournal;
  observe: (
    identity: string,
  ) => Promise<
    Pick<
      WitnessScriptDecodingJournalEntry,
      "stage" | "transactionId" | "outputReference" | "checkpointHash"
    >
  >;
  resolveStage: (input: {
    readonly action: Exclude<WitnessScriptDecodingAction, "done"> | "cancel";
    readonly evidence: WitnessScriptDecodingEvidence;
  }) => Promise<WitnessScriptDecodingAuthenticatedStage>;
}>;

export const createWitnessScriptDecodingRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundWitnessScriptDecodingConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<"witnessScriptDecoding">;
    readonly source: WitnessScriptDecodingAuthenticatedSource;
  }): WitnessScriptDecodingRuntimeLoader["resolveStage"] =>
  async ({ action, evidence }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "witnessScriptDecoding init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "witnessScriptDecoding removal requires raw-L1 proof token",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    const expectedStep =
      action === "submitStep01"
        ? 1
        : action === "submitStep02"
          ? 2
          : action === "submitScanOrResume"
            ? 3
            : 4;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `witnessScriptDecoding ${action} differs from authenticated raw-L1 stage`,
      );
    }
    const common = {
      fraudulentBlockOutRef: stage.stateQueueBlockOutRef,
      threadOutRef: stage.threadOutRef,
      nativeTxCompactCbor: source.nativeTxCompactCbor,
      witnessSetCompactCbor: source.witnessSetCompactCbor,
    };
    if (action !== "submitStep01") return common;
    if (
      evidence.finding.subject.source_kind === PROOF_THREAD_SOURCE_KIND_FORCED
    ) {
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
      family: "witness-script-decoding",
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
    throw new Error(`witnessScriptDecoding missing ${label}`);
  return value;
};

export const createManifestBoundWitnessScriptDecodingSubmission = ({
  config,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundWitnessScriptDecodingConfig;
  readonly observe: (
    identity: string,
  ) => Promise<
    Pick<
      WitnessScriptDecodingJournalEntry,
      "stage" | "transactionId" | "outputReference" | "checkpointHash"
    >
  >;
  readonly resolveStage: WitnessScriptDecodingRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createWitnessScriptDecodingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => ({
  observe,
  submit: async (
    action: Exclude<WitnessScriptDecodingAction, "done">,
    evidence: WitnessScriptDecodingEvidence,
  ) => {
    const familyIdentity = witnessScriptDecodingEvidenceIdentity(evidence);
    const fixedTransition =
      action === "submitInit"
        ? (["none", "step01"] as const)
        : action === "submitStep01"
          ? (["step01", "step02"] as const)
          : action === "submitStep02"
            ? (["step02", "scan"] as const)
            : action === "submitStep04"
              ? (["step04", "proven"] as const)
              : action === "removeDescendants"
                ? (["proven", "removed"] as const)
                : (["scan", "scan"] as const);
    if (action !== "submitScanOrResume")
      await centralJournal?.begin(
        action,
        familyIdentity,
        fixedTransition[0],
        fixedTransition[1],
      );
    const stage = await resolveStage({ action, evidence });
    const boundary = centralJournal?.boundary(
      action,
      familyIdentity,
      fixedTransition[0],
      fixedTransition[1],
    );
    if (action === "submitInit") {
      const result = await submitWitnessScriptDecodingInit({
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        network: config.binding.network,
        contracts: config.contracts,
        category: config.binding.resolvedContracts.category,
        catalogue: config.binding.catalogue,
        signer: config.signer,
        fraudulentBlockOutRef: stage.fraudulentBlockOutRef,
        fraudulentHeaderHash: config.binding.definition.headerHash,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary: boundary,
      });
      return {
        stage: "step01" as const,
        transactionId: result.txHash,
        outputReference: result.nextThreadOutRef,
        checkpointHash: null,
      };
    }
    if (action === "submitStep01") {
      const subject = evidence.finding.subject;
      if (subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
        const result = await submitWitnessScriptDecodingStep01Accepted({
          lucid: config.lucid,
          blueprint: config.binding.blueprint,
          network: config.binding.network,
          contracts: config.contracts,
          categoryId: config.binding.resolvedContracts.category.categoryId,
          signer: config.signer,
          threadOutRef: required(stage.threadOutRef, "step01 thread out-ref"),
          stateQueueBlockOutRef: required(
            stage.stateQueueBlockOutRef,
            "state-queue block out-ref",
          ),
          txInclusion: required(stage.acceptedInclusion, "accepted inclusion"),
          scriptIndex: BigInt(evidence.finding.scriptIndex),
          referenceScriptUtxo: config.referenceScripts.step01,
          witnessReferenceScripts: config.referenceScripts.witnesses,
          preSubmitBoundary: boundary,
        });
        return {
          stage: "step02" as const,
          transactionId: result.txHash,
          outputReference: result.nextThreadOutRef,
          checkpointHash: null,
        };
      }
      const result = await submitWitnessScriptDecodingStep01Forced({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step01 thread out-ref"),
        header: required(stage.forcedHeader, "forced header"),
        membership: required(stage.forcedMembership, "forced membership"),
        direction: required(stage.forcedDirection, "forced direction"),
        witnessSetHash: evidence.finding.witnessSetHash,
        scriptIndex: BigInt(evidence.finding.scriptIndex),
        referenceScriptUtxo: config.referenceScripts.step01,
        preSubmitBoundary: boundary,
      });
      return {
        stage: "step02" as const,
        transactionId: result.txHash,
        outputReference: result.nextThreadOutRef,
        checkpointHash: null,
      };
    }
    if (action === "submitStep02") {
      const auxiliaryHashes: string[] = [];
      const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
        Buffer.from(
          required(stage.witnessSetCompactCbor, "witness-set compact CBOR"),
          "hex",
        ),
      );
      const result = await submitWitnessScriptDecodingStep02({
        lucid: config.lucid,
        network: config.binding.network,
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
        witnessSet: {
          addr_tx_wits_hash: witnessSet.addrTxWitsHash.toString("hex"),
          script_tx_wits_hash: witnessSet.scriptTxWitsHash.toString("hex"),
          redeemer_tx_wits_hash: witnessSet.redeemerTxWitsHash.toString("hex"),
        },
        scriptWitnessItems: decodeMidgardFieldPreimage(
          Buffer.from(evidence.fieldPreimageHex, "hex"),
        ),
        publishCarriage: evidence.carriage !== "Inline",
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
                for (const txHash of auxiliaryHashes)
                  await centralJournal.confirmAuxiliary(txHash);
              },
        referenceScriptUtxo: config.referenceScripts.step02,
        preSubmitBoundary: boundary,
      });
      return {
        stage: "scan" as const,
        transactionId: result.txHash,
        outputReference: result.nextThreadOutRef,
        checkpointHash: result.scanState.checkpoint_hash,
      };
    }
    if (action === "submitScanOrResume") {
      const result = await submitWitnessScriptDecodingStep03({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "scan thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step03,
        preSubmitBoundaryForResult:
          centralJournal === undefined
            ? undefined
            : async (closed) => {
                const target = closed ? "step04" : "scan";
                await centralJournal.begin(
                  action,
                  familyIdentity,
                  "scan",
                  target,
                );
                return centralJournal.boundary(
                  action,
                  familyIdentity,
                  "scan",
                  target,
                );
              },
      });
      return {
        stage: result.closed ? ("step04" as const) : ("scan" as const),
        transactionId: result.txHash,
        outputReference: result.nextThreadOutRef,
        checkpointHash: result.scanState.checkpoint_hash,
      };
    }
    if (action === "submitStep04") {
      const result = await submitWitnessScriptDecodingStep04({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step04 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step04,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary: boundary,
      });
      return {
        stage: "proven" as const,
        transactionId: result.txHash,
        outputReference: null,
        checkpointHash: null,
      };
    }
    const result = await submitRemoveFraudulentBlock({
      lucid: config.lucid,
      blueprint: config.binding.blueprint,
      deploymentInfo: config.binding.deploymentInfo,
      network: config.binding.network,
      signer: config.signer,
      fraudCategory: "witnessScriptDecoding",
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "witnessScriptDecoding production removal requires a state-queue mutation lease coordinator",
          );
        })(),
      awaitConfirmation: true,
      validFrom: stage.validFrom,
      validTo: stage.validTo,
      preSubmitBoundary: boundary,
    });
    return {
      stage: "removed" as const,
      transactionId: result.txHash,
      outputReference: null,
      checkpointHash: null,
    };
  },
  cancel: async (
    current: "step01" | "step02" | "scan" | "step04",
    evidence: WitnessScriptDecodingEvidence,
  ) => {
    const stage = await resolveStage({ action: "cancel", evidence });
    const index =
      current === "step01"
        ? 0
        : current === "step02"
          ? 1
          : current === "scan"
            ? 2
            : 3;
    const result = await submitWitnessScriptDecodingCancel({
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
      transactionId: result.txHash,
      outputReference: null,
      checkpointHash: null,
    };
  },
});

export const loadWitnessScriptDecodingRuntime = async (
  input: WitnessScriptDecodingRuntimeLoader,
) => {
  const config = await loadManifestBoundWitnessScriptDecodingConfig(
    input.config,
  );
  return createManifestBoundWitnessScriptDecodingRuntime({
    config,
    journal: input.journal,
    observe: input.observe,
    resolveStage: input.resolveStage,
  });
};

export const createManifestBoundWitnessScriptDecodingRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundWitnessScriptDecodingConfig;
  readonly journal: WitnessScriptDecodingJournal;
  readonly observe: WitnessScriptDecodingRuntimeLoader["observe"];
  readonly resolveStage: WitnessScriptDecodingRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createWitnessScriptDecodingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundWitnessScriptDecodingSubmission({
    config,
    observe: async (identity) => {
      const observed = await observe(identity);
      await centralJournal?.reconcile(observed.stage);
      return observed;
    },
    resolveStage,
    centralJournal,
    stateQueueMutationLeaseCoordinator,
  });
  return Object.freeze({
    runtimeVersion: WITNESS_SCRIPT_DECODING_WORKFLOW,
    config,
    runOrResume: async (evidence: WitnessScriptDecodingEvidence) =>
      await runWitnessScriptDecodingProof({
        evidence,
        load: journal.load,
        append: journal.append,
        submission,
      }),
  });
};

export type ManifestBoundWitnessScriptDecodingWorkflowConfig =
  LoadManifestBoundWitnessScriptDecodingConfig &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundWitnessScriptDecodingWorkflow = Readonly<{
  workflowVersion: typeof WITNESS_SCRIPT_DECODING_WORKFLOW;
  config: ManifestBoundWitnessScriptDecodingConfig;
  binding: FraudProofWorkflowDeploymentBinding<"witnessScriptDecoding">;
  l1: FraudProofFamilyL1ObservationPort<"witnessScriptDecoding">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundWitnessScriptDecodingWorkflow = async (
  input: ManifestBoundWitnessScriptDecodingWorkflowConfig,
): Promise<ManifestBoundWitnessScriptDecodingWorkflow> => {
  const config = await loadManifestBoundWitnessScriptDecodingConfig(input);
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: input.source,
    releaseFinality: config.binding.releaseFinality,
    releaseEconomics: config.binding.releaseEconomics,
    definition: config.binding.definition,
  });
  return Object.freeze({
    workflowVersion: WITNESS_SCRIPT_DECODING_WORKFLOW,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

const witnessScriptDecodingObservationFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPort<"witnessScriptDecoding">["observe"]
    >
  >["stage"],
): Pick<
  WitnessScriptDecodingJournalEntry,
  "stage" | "transactionId" | "outputReference" | "checkpointHash"
> => {
  switch (stage.kind) {
    case "not_started":
      return {
        stage: "none",
        transactionId: "0".repeat(64),
        outputReference: null,
        checkpointHash: null,
      };
    case "step":
      if (stage.step > 4)
        throw new Error(
          "witnessScriptDecoding L1 stage exceeds four-step topology",
        );
      return {
        stage:
          stage.step === 1
            ? "step01"
            : stage.step === 2
              ? "step02"
              : stage.step === 3
                ? "scan"
                : "step04",
        transactionId: stage.threadOutRef.split("#")[0]!,
        outputReference: stage.threadOutRef,
        checkpointHash: null,
      };
    case "proof_token":
      return {
        stage: "proven",
        transactionId: stage.fraudProofOutRef.split("#")[0]!,
        outputReference: stage.fraudProofOutRef,
        checkpointHash: null,
      };
    case "removed":
      return {
        stage: "removed",
        transactionId: stage.terminal.correction.removalTxHash,
        outputReference: null,
        checkpointHash: null,
      };
  }
};

/**
 * Watcher-facing runner. Evidence is always reconstructed from authenticated
 * L1 plus public retained DA; unknown/caller-authored evidence fields fail.
 */
export const runOrResumeManifestBoundWitnessScriptDecodingWorkflow =
  async (input: {
    readonly workflow: ManifestBoundWitnessScriptDecodingWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: WitnessScriptDecodingJournal;
  }): Promise<WitnessScriptDecodingStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "witnessScriptDecoding runner rejects caller-authored evidence inputs",
      );
    }
    const headerHash = input.workflow.binding.definition.headerHash;
    const observation = await input.workflow.l1.observeHeader({ headerHash });
    const canonical = await fetchCanonicalBlockEvidence({
      observation,
      sources: input.sources,
    });
    const evidence =
      deriveWitnessScriptDecodingEvidenceFromCanonicalBlock(canonical);
    const source = await deriveWitnessScriptDecodingAuthenticatedSource({
      block: canonical,
      evidence,
    });
    const runtime = createManifestBoundWitnessScriptDecodingRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        witnessScriptDecodingObservationFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createWitnessScriptDecodingRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

export const executeManifestBoundWitnessScriptDecodingWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundWitnessScriptDecodingWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<WitnessScriptDecodingStage> => {
  const headerHash = workflow.binding.definition.headerHash;
  const canonical = await fetchCanonicalBlockEvidence({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const evidence =
    deriveWitnessScriptDecodingEvidenceFromCanonicalBlock(canonical);
  const source = await deriveWitnessScriptDecodingAuthenticatedSource({
    block: canonical,
    evidence,
  });
  const centralJournal = createWitnessScriptDecodingCentralJournalAdapter({
    store: journal,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    headerHash,
    decisionDigest: workflow.decisionDigest,
    transactionConfirmed: async (txHash) =>
      await workflow.l1.transactionConfirmed({ headerHash, txHash }),
  });
  const runtime = createManifestBoundWitnessScriptDecodingRuntime({
    config: workflow.config,
    journal: centralJournal.familyJournal,
    observe: async () =>
      witnessScriptDecodingObservationFromL1(
        (await workflow.l1.observe({ headerHash })).stage,
      ),
    resolveStage: createWitnessScriptDecodingRawL1StageResolver({
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

export type LoadedWitnessScriptDecodingWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundWitnessScriptDecodingWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadWitnessScriptDecodingWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedWitnessScriptDecodingWorkflow>;

/**
 * Family-local runner surface for central admission. It consumes only a
 * manifest/runtime path and concrete public-DA transports; neither evidence
 * nor a watcher-owned journal implementation can enter this boundary.
 */
export const createWitnessScriptDecodingWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadWitnessScriptDecodingWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== "witnessScriptDecoding") {
        throw new Error(
          `witnessScriptDecoding production runner category mismatch: ${invocation.category}`,
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
          category: "witnessScriptDecoding",
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: "witnessScriptDecoding",
        headerHash: invocation.headerHash,
        checkpoint: "runner_start",
      });
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      if (typeof loaded.close !== "function") {
        throw new Error(
          "witnessScriptDecoding runtime omitted its transport disposer",
        );
      }
      try {
        if (
          loaded.schemaVersion !==
          "midgard-production-fraud-proof-runtime-config-v1"
        ) {
          throw new Error(
            "witnessScriptDecoding runtime config has an unsupported schema",
          );
        }
        if (
          loaded.retainedDaSources.length === 0 ||
          loaded.retainedDaSources.some(
            (source) => !(source instanceof DaLibp2pRetainedDaSource),
          )
        ) {
          throw new Error(
            "witnessScriptDecoding production runner requires concrete public retained-DA sources",
          );
        }
        const workflow = await createManifestBoundWitnessScriptDecodingWorkflow(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.category !== "witnessScriptDecoding" ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        ) {
          throw new Error(
            "witnessScriptDecoding manifest-bound workflow identity differs from invocation",
          );
        }
        return await executeManifestBoundWitnessScriptDecodingWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });
