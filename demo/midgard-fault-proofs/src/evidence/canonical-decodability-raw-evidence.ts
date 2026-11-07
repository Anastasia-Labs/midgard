import {
  deriveMidgardNativeTxFaultEvidenceMaterial,
  type MidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core/codec";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type DaHashPreimageBlockEvidence,
  daHashPreimageBlockEvidenceFromVerifiedPayload,
} from "../prepare-da-hash-preimage.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  commitCountedRoot,
  keyValuePhasRootWithCount,
} from "../transition-trace/phas.js";

export const CANONICAL_DECODABILITY_RAW_EVIDENCE =
  "midgard-canonical-decodability-raw-evidence-v1" as const;
export const CANONICAL_DECODABILITY_ARTIFACT =
  "midgard-production-canonical-decodability-artifact-v1" as const;

export type CanonicalDecodabilityRawArtifact = Readonly<{
  schemaVersion: typeof CANONICAL_DECODABILITY_ARTIFACT;
  headerHash: string;
  committedTransactionsRoot: string;
  l2TransactionCount: number;
  transactionsPhasRoot: string;
  selectedTransactionIndex: number;
  selectedFieldIndex: number;
  selectedVerdict: number;
  txMembershipProofCbor: string;
  transactions: readonly Readonly<{
    nodeTxId: string;
    txCbor: string;
    l2TransactionSourceCbor: string;
  }>[];
}>;

export type AuthenticatedCanonicalDecodabilityTransaction = Readonly<{
  index: number;
  nodeTxId: string;
  l2TransactionSourceCbor: string;
  fullTransactionCbor: string;
  material: MidgardNativeTxFaultEvidenceMaterial;
}>;

export type CanonicalDecodabilityRawBlockEvidence = Readonly<{
  schemaVersion: typeof CANONICAL_DECODABILITY_RAW_EVIDENCE;
  grade: SDK.EvidenceGrade;
  provenance: DaHashPreimageBlockEvidence["provenance"];
  headerHash: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  l1ChainPoint: DaHashPreimageBlockEvidence["l1ChainPoint"];
  committedTransactionsRoot: string;
  l2TransactionCount: bigint;
  transactionsPhasRoot: string;
  transactions: readonly AuthenticatedCanonicalDecodabilityTransaction[];
  selected: Readonly<{
    transactionIndex: number;
    nodeTxId: string;
    fieldIndex: number;
    verdict: number;
    committedPreimage: Buffer;
    txMembershipProofCbor: string;
  }>;
}>;

const decodeSource = (cbor: string, index: number): SDK.L2TransactionSource => {
  let decoded: SDK.L2TransactionSource;
  try {
    decoded = Data.from(cbor, SDK.L2TransactionSource);
  } catch (cause) {
    throw new Error(
      `canonical-decodability transactions[${index.toString()}] source does not decode: ${String(cause)}`,
    );
  }
  if (Data.to(decoded, SDK.L2TransactionSource) !== cbor.toLowerCase()) {
    throw new Error(
      `canonical-decodability transactions[${index.toString()}] source is not canonical Data`,
    );
  }
  return decoded;
};

const sourceFromMaterial = (
  material: MidgardNativeTxFaultEvidenceMaterial,
): SDK.L2TransactionSource => ({
  tx_id: material.transactionId.toString("hex"),
  source: {
    compact_cbor: material.proofSource.compactCbor.toString("hex"),
    witness_set_compact_cbor:
      material.proofSource.witnessSetCompactCbor.toString("hex"),
    field_preimage_lengths_cbor:
      material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
  },
});

/**
 * Reopens an L1-authenticated transactions root and total-decodes only the
 * outer transaction envelope. A result exists iff one exact committed field
 * has a non-grammatical §5.1 envelope; arbitrary malformed payloads still fail.
 */
export const canonicalDecodabilityRawBlockEvidenceFromVerifiedPayload = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: SDK.EvidenceProvenance;
  readonly minimumConfirmationDepth?: number;
}): Promise<CanonicalDecodabilityRawBlockEvidence> => {
  const raw = await daHashPreimageBlockEvidenceFromVerifiedPayload({
    observation,
    payloadEnvelopeCbor,
    daProvenance,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  const payloadCbor = Buffer.from(
    (
      await unwrapDaPayload(payloadEnvelopeCbor, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      })
    ).innerBytes,
  );
  const payload = SDK.decodeDaPayload(payloadCbor);
  if (!SDK.encodeDaPayload(payload).equals(payloadCbor)) {
    throw new Error("canonical-decodability DA payload is not canonical");
  }

  const entries = raw.entries.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));
  const phas = await keyValuePhasRootWithCount(entries);
  const countedRoot = await commitCountedRoot({
    domain: SDK.ROOT_DOMAINS.transactionsV1,
    phasRoot: phas.root,
    count: phas.count,
  });
  if (
    countedRoot !== raw.committedTransactionsRoot ||
    phas.count !== raw.l2TransactionCount
  ) {
    throw new Error(
      "canonical-decodability raw transactions do not open the L1 root/count",
    );
  }

  const preimages = new Map(
    payload.block_body.transaction_preimages.map(([key, value], index) => {
      if (!/^[0-9a-f]{64}$/u.test(key) || !/^(?:[0-9a-f]{2})+$/u.test(value)) {
        throw new Error(
          `canonical-decodability transaction_preimages[${index.toString()}] is malformed`,
        );
      }
      return [key, value] as const;
    }),
  );
  if (preimages.size !== payload.block_body.transaction_preimages.length) {
    throw new Error(
      "canonical-decodability transaction preimages contain duplicate keys",
    );
  }

  const transactions: AuthenticatedCanonicalDecodabilityTransaction[] = [];
  let selected:
    | Omit<
        CanonicalDecodabilityRawBlockEvidence["selected"],
        "txMembershipProofCbor"
      >
    | undefined;
  for (const [index, [key, sourceCbor]] of raw.entries.entries()) {
    const fullCbor = preimages.get(key);
    if (fullCbor === undefined) {
      throw new Error(
        `canonical-decodability transaction preimage is missing ${key}`,
      );
    }
    const source = decodeSource(sourceCbor, index);
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(fullCbor, "hex"),
    );
    const expected = sourceFromMaterial(material);
    if (
      expected.tx_id !== key ||
      Data.to(source, SDK.L2TransactionSource) !==
        Data.to(expected, SDK.L2TransactionSource)
    ) {
      throw new Error(
        `canonical-decodability transaction ${key} preimage does not match its committed source`,
      );
    }
    transactions.push(
      Object.freeze({
        index,
        nodeTxId: key,
        l2TransactionSourceCbor: sourceCbor,
        fullTransactionCbor: fullCbor,
        material,
      }),
    );
    if (selected === undefined) {
      for (const [
        fieldIndex,
        committedPreimage,
      ] of material.fieldPreimages.entries()) {
        const field = SDK.canonicalDecodabilityEvidenceFromCommittedField({
          badTxId: key,
          fieldIndex,
          committedPreimage,
        });
        if (field.isViolation) {
          selected = Object.freeze({
            transactionIndex: index,
            nodeTxId: key,
            fieldIndex,
            verdict: field.verdict,
            committedPreimage,
          });
          break;
        }
      }
    }
  }
  if (preimages.size !== transactions.length) {
    throw new Error(
      "canonical-decodability transaction preimages contain uncommitted keys",
    );
  }
  if (selected === undefined) {
    throw new Error(
      "canonical-decodability raw route found no committed field-envelope violation",
    );
  }
  const trie = await buildTrieView(entries);
  return Object.freeze({
    schemaVersion: CANONICAL_DECODABILITY_RAW_EVIDENCE,
    grade: raw.grade,
    provenance: raw.provenance,
    headerHash: raw.headerHash,
    payloadEnvelopeSha256: raw.payloadEnvelopeSha256,
    payloadSha256: raw.payloadSha256,
    l1ChainPoint: raw.l1ChainPoint,
    committedTransactionsRoot: countedRoot,
    l2TransactionCount: raw.l2TransactionCount,
    transactionsPhasRoot: phas.root,
    transactions: Object.freeze(transactions),
    selected: Object.freeze({
      ...selected,
      txMembershipProofCbor: requireProof(
        trie,
        Buffer.from(selected.nodeTxId, "hex"),
        "canonical-decodability transaction",
      ),
    }),
  });
};

export const canonicalDecodabilityArtifactFromRawEvidence = (
  evidence: CanonicalDecodabilityRawBlockEvidence,
): CanonicalDecodabilityRawArtifact =>
  Object.freeze({
    schemaVersion: CANONICAL_DECODABILITY_ARTIFACT,
    headerHash: evidence.headerHash,
    committedTransactionsRoot: evidence.committedTransactionsRoot,
    l2TransactionCount: Number(evidence.l2TransactionCount),
    transactionsPhasRoot: evidence.transactionsPhasRoot,
    selectedTransactionIndex: evidence.selected.transactionIndex,
    selectedFieldIndex: evidence.selected.fieldIndex,
    selectedVerdict: evidence.selected.verdict,
    txMembershipProofCbor: evidence.selected.txMembershipProofCbor,
    transactions: Object.freeze(
      evidence.transactions.map((transaction) =>
        Object.freeze({
          nodeTxId: transaction.nodeTxId,
          txCbor: transaction.fullTransactionCbor,
          l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
        }),
      ),
    ),
  });
