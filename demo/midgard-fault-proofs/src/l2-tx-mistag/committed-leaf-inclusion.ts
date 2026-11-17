/** Inclusion material for a code-1 normal leaf, read from the committed
 * leaves alone. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { isCommittedInvalidNormalLeaf } from "../evidence/committed-normal-leaf-validity.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";

export type L2TxMistagCommittedLeafInclusion = Readonly<{
  transactionIndex: number;
  transactionCount: number;
  committedTransactionsRoot: string;
  transactionsPhasRoot: string;
  nativeTxId: string;
  nativeTxCompactCbor: string;
  l2TransactionSourceCbor: string;
  txMembershipProofCbor: string;
}>;

/**
 * Opens the committed `transactions_root` over exactly `entries` (committed
 * key and source-leaf CBOR, in committed order) and proves the leaf at
 * `transactionIndex`. Only the leaf's committed compact is read, never its
 * inputs, outputs or fields: the `l2-tx-mistag` step reads nothing else, so a
 * leaf whose body does not decode is still proven.
 */
export const l2TxMistagInclusionFromCommittedLeaves = async ({
  entries,
  transactionIndex,
  committedTransactionsRoot,
  l2TransactionCount,
}: {
  readonly entries: readonly (readonly [string, string])[];
  readonly transactionIndex: number;
  readonly committedTransactionsRoot: string;
  readonly l2TransactionCount: bigint;
}): Promise<L2TxMistagCommittedLeafInclusion> => {
  const selected = entries[transactionIndex];
  if (selected === undefined)
    throw new Error("l2-tx-mistag selected transaction is absent");
  const [nativeTxId, l2TransactionSourceCbor] = selected;
  const source = Data.from(l2TransactionSourceCbor, SDK.L2TransactionSource);
  if (
    Data.to(source, SDK.L2TransactionSource) !==
      l2TransactionSourceCbor.toLowerCase() ||
    source.tx_id !== nativeTxId
  )
    throw new Error("l2-tx-mistag selected leaf is not its canonical source");
  if (!isCommittedInvalidNormalLeaf(source))
    throw new Error("l2-tx-mistag selected leaf is not a code-1 normal leaf");
  const trie = await buildTrieView(
    entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const countedRoot = await Effect.runPromise(
    SDK.commitCountedRootProgram({
      domain: SDK.ROOT_DOMAINS.transactionsV1,
      phasRoot: trie.root,
      count: BigInt(entries.length),
    }),
  );
  if (
    countedRoot !== committedTransactionsRoot ||
    BigInt(entries.length) !== l2TransactionCount
  )
    throw new Error(
      "l2-tx-mistag committed leaves do not open the L1 transactions root/count",
    );
  return Object.freeze({
    transactionIndex,
    transactionCount: entries.length,
    committedTransactionsRoot,
    transactionsPhasRoot: trie.root,
    nativeTxId,
    nativeTxCompactCbor: source.source.compact_cbor,
    l2TransactionSourceCbor,
    txMembershipProofCbor: requireProof(
      trie,
      Buffer.from(nativeTxId, "hex"),
      "l2-tx-mistag tx",
    ),
  });
};
