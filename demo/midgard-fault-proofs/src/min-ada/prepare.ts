/** Authenticated retained-DA preparation for Q27 MIN-ADA-TX. */
import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { type MinAdaFault } from "@al-ft/midgard-sdk";
import {
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";

import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalEvidenceBuilderInput,
} from "../evidence/prepare-from-evidence.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type PreparedTxInclusionJson,
  requireProof,
  requireTransactionsRootMatch,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import { minAdaOutputFacts } from "./scan.js";

export type PreparedMinAdaTx = {
  readonly kind: "min-ada-tx";
  readonly headerHash: string;
  readonly badTxId: string;
  readonly badOutputIndex: bigint;
  readonly nativeTxCanonicalCbor: string;
  readonly nativeTxCompactCbor: string;
  readonly outputItemCbors: readonly string[];
  readonly txInclusion: PreparedTxInclusionJson;
  readonly fault: MinAdaFault;
};

export const prepareMinAdaTxFromCanonicalEvidence = async ({
  evidence,
  badTxId,
  badOutputIndex,
}: CanonicalEvidenceBuilderInput & {
  readonly badTxId?: string;
  readonly badOutputIndex?: bigint;
}): Promise<PreparedMinAdaTx> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  const decoded = await Promise.all(
    admitted.transactions.map(decodeTransactionMaterial),
  );
  const requested = badTxId?.toLowerCase();
  if (requested !== undefined && !/^[0-9a-f]{64}$/u.test(requested)) {
    throw new Error("min-ada badTxId must be 32-byte lowercase hex");
  }
  if (badOutputIndex !== undefined && badOutputIndex < 0n) {
    throw new Error("min-ada badOutputIndex must be non-negative");
  }
  const selected = decoded.flatMap((tx) => {
    if (
      tx.nativeTx.validity !== "TxIsValid" ||
      (requested !== undefined && tx.nodeTxId !== requested)
    ) {
      return [];
    }
    const outputs = decodeMidgardFieldPreimage(
      tx.nativeTx.body.outputsPreimageCbor,
    );
    return outputs.flatMap((outputCbor, outputIndex) => {
      if (
        badOutputIndex !== undefined &&
        BigInt(outputIndex) !== badOutputIndex
      ) {
        return [];
      }
      const facts = minAdaOutputFacts(outputCbor);
      return facts !== null &&
        !outputMeetsMinAda(
          MIDGARD_COINS_PER_UTXO_BYTE,
          BigInt(facts.totalLength),
          facts.lovelace,
        )
        ? [{ tx, outputs, outputIndex }]
        : [];
    });
  })[0];
  if (selected === undefined) {
    throw new Error(
      "authenticated retained DA contains no accepted transaction output below the compiled min-Ada floor",
    );
  }
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  await requireTransactionsRootMatch({
    sourceRoot: trie.root,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    count: BigInt(decoded.length),
  });
  const fault: MinAdaFault = { output_index: BigInt(selected.outputIndex) };
  return {
    kind: "min-ada-tx",
    headerHash: admitted.headerHash,
    badTxId: selected.tx.nodeTxId,
    badOutputIndex: BigInt(selected.outputIndex),
    nativeTxCanonicalCbor: selected.tx.txCbor,
    nativeTxCompactCbor: selected.tx.nativeCompactCbor,
    outputItemCbors: selected.outputs.map((item) =>
      Buffer.from(item).toString("hex"),
    ),
    txInclusion: {
      nativeTxId: selected.tx.nodeTxId,
      nativeTx: selected.tx.nativeTxCompact,
      nativeTxCompactCbor: selected.tx.nativeCompactCbor,
      l2TransactionSourceCbor: selected.tx.l2TransactionSourceCbor,
      transactionsPhasRoot: trie.root,
      txMembershipProofCbor: requireProof(
        trie,
        transactionSourceTrieItem(selected.tx).key,
        "min-ada transaction",
      ),
    },
    fault,
  };
};
