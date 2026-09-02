import {
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  type MidgardNativeTxFaultEvidenceMaterialV1,
} from "@al-ft/midgard-core";
import { unwrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { daHashPreimageBlockEvidenceFromVerifiedPayloadV1 } from "../prepare-da-hash-preimage.js";
import {
  buildTrieView,
  requireProof,
  requireTransactionsRootMatchV1,
} from "../prepare-double-spend.js";

export type AuthenticatedObserversForbiddenRawTransactionV1 = Readonly<{
  index: number;
  nodeTxId: string;
  l2TransactionSourceCbor: string;
  fullTransactionCbor: string;
  material: MidgardNativeTxFaultEvidenceMaterialV1;
}>;

export type ObserversForbiddenRawBlockEvidenceV1 = Readonly<{
  schemaVersion: "midgard-observers-forbidden-raw-evidence-v1";
  headerHash: string;
  committedTransactionsRoot: string;
  l2TransactionCount: bigint;
  transactionsPhasRoot: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  transactions: readonly AuthenticatedObserversForbiddenRawTransactionV1[];
}>;

/** Authenticated retained-DA seam that intentionally precedes block parsing. */
export const observersForbiddenRawBlockEvidenceFromVerifiedPayloadV1 = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservationV1;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: SDK.EvidenceProvenanceV1;
  readonly minimumConfirmationDepth?: number;
}): Promise<ObserversForbiddenRawBlockEvidenceV1> => {
  const raw = await daHashPreimageBlockEvidenceFromVerifiedPayloadV1({
    observation,
    payloadEnvelopeCbor,
    daProvenance,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  const payloadCbor = Buffer.from(
    (
      await unwrapDaPayloadV1(payloadEnvelopeCbor, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
      })
    ).innerBytes,
  );
  const payload = SDK.decodeDaPayloadV1(payloadCbor);
  if (!SDK.encodeDaPayloadV1(payload).equals(payloadCbor))
    throw new Error("observersForbidden DA payload is not canonical");
  const entries = raw.entries.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));
  const trie = await buildTrieView(entries);
  await requireTransactionsRootMatchV1({
    sourceRoot: trie.root,
    expectedTransactionsRoot: raw.committedTransactionsRoot,
    count: raw.l2TransactionCount,
  });
  const preimages = new Map(payload.block_body.transaction_preimages);
  if (preimages.size !== payload.block_body.transaction_preimages.length)
    throw new Error("observersForbidden transaction preimages are duplicated");
  const transactions = raw.entries.map(([key, sourceCbor], index) => {
    const fullTransactionCbor = preimages.get(key);
    if (fullTransactionCbor === undefined)
      throw new Error(`observersForbidden transaction preimage omitted ${key}`);
    const source = Data.from(sourceCbor, SDK.L2TransactionSourceV1);
    if (Data.to(source, SDK.L2TransactionSourceV1) !== sourceCbor)
      throw new Error("observersForbidden transaction source is not canonical");
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      Buffer.from(fullTransactionCbor, "hex"),
    );
    if (
      source.tx_id !== key ||
      material.transactionId.toString("hex") !== key ||
      source.source.compact_cbor !==
        material.proofSource.compactCbor.toString("hex") ||
      source.source.witness_set_compact_cbor !==
        material.proofSource.witnessSetCompactCbor.toString("hex") ||
      source.source.field_preimage_lengths_cbor !==
        material.proofSource.fieldPreimageLengthsCbor.toString("hex")
    )
      throw new Error(
        `observersForbidden transaction ${key} differs from committed source`,
      );
    return Object.freeze({
      index,
      nodeTxId: key,
      l2TransactionSourceCbor: sourceCbor,
      fullTransactionCbor,
      material,
    });
  });
  if (preimages.size !== transactions.length)
    throw new Error("observersForbidden has uncommitted transaction preimages");
  return Object.freeze({
    schemaVersion: "midgard-observers-forbidden-raw-evidence-v1",
    headerHash: raw.headerHash,
    committedTransactionsRoot: raw.committedTransactionsRoot,
    l2TransactionCount: raw.l2TransactionCount,
    transactionsPhasRoot: trie.root,
    payloadEnvelopeSha256: raw.payloadEnvelopeSha256,
    payloadSha256: raw.payloadSha256,
    transactions: Object.freeze(transactions),
  });
};

export const observersForbiddenAcceptedMembershipV1 = async ({
  block,
  transactionId,
}: {
  readonly block: ObserversForbiddenRawBlockEvidenceV1;
  readonly transactionId: string;
}) =>
  requireProof(
    await buildTrieView(
      block.transactions.map((transaction) => ({
        key: Buffer.from(transaction.nodeTxId, "hex"),
        value: Buffer.from(transaction.l2TransactionSourceCbor, "hex"),
      })),
    ),
    Buffer.from(transactionId, "hex"),
    "observersForbidden accepted transaction",
  );
