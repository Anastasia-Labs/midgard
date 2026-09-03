import {
  deriveMidgardNativeTxFaultEvidenceMaterial,
  type MidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { daHashPreimageBlockEvidenceFromVerifiedPayload } from "../prepare-da-hash-preimage.js";
import {
  buildTrieView,
  requireProof,
  requireTransactionsRootMatch,
} from "../prepare-double-spend.js";

export type AuthenticatedObserversForbiddenRawTransaction = Readonly<{
  index: number;
  nodeTxId: string;
  l2TransactionSourceCbor: string;
  fullTransactionCbor: string;
  material: MidgardNativeTxFaultEvidenceMaterial;
}>;

export type ObserversForbiddenRawBlockEvidence = Readonly<{
  schemaVersion: "midgard-observers-forbidden-raw-evidence-v1";
  headerHash: string;
  committedTransactionsRoot: string;
  l2TransactionCount: bigint;
  transactionsPhasRoot: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  transactions: readonly AuthenticatedObserversForbiddenRawTransaction[];
}>;

/** Authenticated retained-DA seam that intentionally precedes block parsing. */
export const observersForbiddenRawBlockEvidenceFromVerifiedPayload = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: SDK.EvidenceProvenance;
  readonly minimumConfirmationDepth?: number;
}): Promise<ObserversForbiddenRawBlockEvidence> => {
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
  if (!SDK.encodeDaPayload(payload).equals(payloadCbor))
    throw new Error("observersForbidden DA payload is not canonical");
  const entries = raw.entries.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));
  const trie = await buildTrieView(entries);
  await requireTransactionsRootMatch({
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
    const source = Data.from(sourceCbor, SDK.L2TransactionSource);
    if (Data.to(source, SDK.L2TransactionSource) !== sourceCbor)
      throw new Error("observersForbidden transaction source is not canonical");
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
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

export const observersForbiddenAcceptedMembership = async ({
  block,
  transactionId,
}: {
  readonly block: ObserversForbiddenRawBlockEvidence;
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
