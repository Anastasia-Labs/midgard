import {
  decodeMidgardFieldPreimage,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  buildMissingNativeScriptTxEvidence,
  buildTrieView,
  type CanonicalBlockEvidence,
  type DecodedTransactionMaterial,
  decodeTransactionMaterial,
  requireProof,
  requireTransactionsRootMatch,
  transactionSourceTrieItem,
} from "@al-ft/midgard-fault-proofs";
import { Proof } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

/** Exact on-chain fields. Live admission also requires canonical L1 preimage corroboration. */
export const prepareJourneyMissingNativeScriptTxEvidence = async (
  evidence: CanonicalBlockEvidence,
) => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const consumer = transactions.find(
    (tx) =>
      decodeMidgardFieldPreimage(
        tx.nativeTx.witnessSet.scriptTxWitsPreimageCbor,
      ).length === 0,
  );
  const producer =
    consumer === undefined
      ? undefined
      : transactions.find(
          (tx) => tx.nodeTxId === consumer.inputs[0]?.transactionId,
        );
  if (consumer === undefined || producer === undefined)
    throw new Error("Native script history omitted producer/consumer binding");
  const trie = await buildTrieView(transactions.map(transactionSourceTrieItem));
  await requireTransactionsRootMatch({
    sourceRoot: trie.root,
    expectedTransactionsRoot: evidence.header.transactionsRoot,
    count: BigInt(transactions.length),
  });
  const inclusion = (tx: DecodedTransactionMaterial) => ({
    nativeTxId: tx.nodeTxId,
    nativeTx: tx.nativeTxCompact,
    nativeTxCompactCbor: tx.nativeCompactCbor,
    l2TransactionSourceCbor: tx.l2TransactionSourceCbor,
    transactionsPhasRoot: trie.root,
    txMembershipProof: Data.from(
      requireProof(
        trie,
        Buffer.from(tx.nodeTxId, "hex"),
        "script journey inclusion",
      ),
      Proof,
    ),
    txMembershipProofCbor: requireProof(
      trie,
      Buffer.from(tx.nodeTxId, "hex"),
      "script journey inclusion",
    ),
  });
  const producerScript = decodeMidgardFieldPreimage(
    producer.nativeTx.witnessSet.scriptTxWitsPreimageCbor,
  )[0];
  if (producerScript === undefined)
    throw new Error("Native script producer omitted its preimage");
  const witnessSet = deriveMidgardNativeTxWitnessSetCompact(
    consumer.nativeTx.witnessSet,
  );
  return buildMissingNativeScriptTxEvidence({
    badTxInclusion: inclusion(consumer),
    badTxSpendInputs: consumer.inputs.map((ref) => ({
      tx_id: ref.transactionId,
      output_index: ref.outputIndex,
    })),
    badInputIndex: 0n,
    producingTxInclusion: inclusion(producer),
    producingOutputItemCbors: decodeMidgardFieldPreimage(
      producer.nativeTx.body.outputsPreimageCbor,
    ),
    missingNativeScriptBytes:
      decodeMidgardVersionedScript(producerScript).scriptBytes,
    badTxWitnessSet: {
      addr_tx_wits_hash: Buffer.from(witnessSet.addrTxWitsHash).toString("hex"),
      script_tx_wits_hash: Buffer.from(witnessSet.scriptTxWitsHash).toString(
        "hex",
      ),
      redeemer_tx_wits_hash: Buffer.from(
        witnessSet.redeemerTxWitsHash,
      ).toString("hex"),
    },
    badTxScriptWitnessItemCbors: decodeMidgardFieldPreimage(
      consumer.nativeTx.witnessSet.scriptTxWitsPreimageCbor,
    ),
    owner: evidence.header.operatorVkey,
  });
};
