import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxCompact,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  encodeProofThreadForcedSourceKey,
  type ForcedInclusionTxV1,
  forcedVerdictSubject,
  type Header,
  type OutputReference,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  RejectionReasonSchema,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import {
  parseSubmitStep01TxInclusion,
  type SubmitStep01TxInclusion,
} from "../step-support.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import {
  prepareWitnessScriptDecodingEvidence,
  type WitnessScriptDecodingEvidence,
  witnessScriptDecodingEvidenceCloses,
} from "./witness-script-decoding.js";

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
    const material = (
      subject.source_kind === 1n
        ? deriveMidgardForcedTxFaultEvidenceMaterial
        : deriveMidgardNativeTxFaultEvidenceMaterial
    )(canonicalCbor);
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
          witnessSetHash: (subject.source_kind === 1n
            ? decodeMidgardForcedTxCompact
            : decodeMidgardNativeTxCompact)(
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
      canonicalCbor: forced.fullTransactionCbor,
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
      encodeProofThreadForcedSourceKey(key).toString("hex") ===
        evidence.finding.subject.source_key,
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
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(
    forced.fullTransactionCbor,
  );
  if (
    material.proofSource.compactCbor.toString("hex") !==
      forced.value.submitted_source.compact_cbor ||
    material.proofSource.witnessSetCompactCbor.toString("hex") !==
      forced.value.submitted_source.witness_set_compact_cbor ||
    material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
      forced.value.submitted_source.field_preimage_lengths_cbor
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
