/** Authenticated retained-DA preparation for Q27 MIN-ADA-TX/UTXO. */
import {
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  type MinAdaFault,
  type OutputReference,
  Proof,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildCanonicalMidgardLedgerOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
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
import {
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../transition-trace/phas.js";
import {
  type HistoricalNativeScriptCorpus,
  requireHistoricalNativeScriptCorpus,
} from "../workflow/historical-native-script-corpus.js";

export type PreparedMinAdaTx = {
  readonly kind: "min-ada-tx";
  readonly headerHash: string;
  readonly badTxId: string;
  readonly badOutputIndex: bigint;
  readonly nativeTxCanonicalCbor: string;
  readonly nativeTxCompactCbor: string;
  readonly outputItemCbors: readonly string[];
  readonly descriptorCbor: string;
  readonly txInclusion: PreparedTxInclusionJson;
  readonly fault: MinAdaFault;
};

export type PreparedMinAdaUtxo = {
  readonly kind: "min-ada-utxo";
  readonly headerHash: string;
  readonly outRef: OutputReference;
  readonly outRefKeyCbor: string;
  readonly descriptorCbor: string;
  readonly postUtxosRoot: string;
  readonly prevUtxosRoot: string;
  readonly postMembershipProof: Proof;
  readonly postMembershipProofCbor: string;
  readonly predecessorNonMembershipProof: Proof;
  readonly predecessorNonMembershipProofCbor: string;
  readonly fault: MinAdaFault;
};

export type PreparedMinAda = PreparedMinAdaTx | PreparedMinAdaUtxo;

const descriptorViolatesMinAda = (descriptorCbor: Uint8Array): boolean => {
  const descriptor = decodeMidgardLedgerOutputCommitment(descriptorCbor);
  return !outputMeetsMinAda(
    MIDGARD_COINS_PER_UTXO_BYTE,
    BigInt(descriptor.totalLength),
    descriptor.lovelace,
  );
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
      const material = buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex,
        outputCbor,
      });
      return descriptorViolatesMinAda(material.descriptorCbor)
        ? [{ tx, outputs, outputIndex, material }]
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
  const fault = {
    MinAdaTx: { output_index: BigInt(selected.outputIndex) },
  } as MinAdaFault;
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
    descriptorCbor: selected.material.descriptorCbor.toString("hex"),
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

const sameOutRef = (left: OutputReference, right: OutputReference): boolean =>
  left.transactionId === right.transactionId &&
  left.outputIndex === right.outputIndex;

export const prepareMinAdaUtxoFromCanonicalEvidence = async ({
  evidence,
  historicalNativeScriptCorpus,
  outRef,
}: CanonicalEvidenceBuilderInput & {
  readonly historicalNativeScriptCorpus: HistoricalNativeScriptCorpus;
  readonly outRef?: OutputReference;
}): Promise<PreparedMinAdaUtxo> => {
  const history = requireHistoricalNativeScriptCorpus(
    historicalNativeScriptCorpus,
  );
  if (history.currentEvidence !== evidence) {
    throw new Error(
      "min-ada UTxO history is not bound to the challenged evidence",
    );
  }
  const predecessor = history.reconstructions.at(-2);
  if (
    predecessor !== undefined &&
    (predecessor.headerHash !== evidence.header.prevHeaderHash ||
      predecessor.header.utxosRoot !== evidence.header.prevUtxosRoot)
  ) {
    throw new Error(
      "min-ada predecessor history does not authenticate the challenged prev_utxos_root",
    );
  }
  const members = (
    entries: CanonicalBlockEvidence["reconstruction"]["utxos"],
  ) =>
    entries.map((entry) => {
      const decoded = decodeMidgardSpendInputItem(entry.key);
      const material = buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: entry.key,
        outputCbor: entry.value,
      });
      return {
        key: Buffer.from(entry.key),
        value: Buffer.from(material.descriptorCbor),
        outRef: {
          transactionId: Buffer.from(decoded.txId).toString("hex"),
          outputIndex: BigInt(decoded.outputIndex),
        } satisfies OutputReference,
      };
    });
  const postMembers = members(evidence.reconstruction.utxos);
  const previousMembers = members(predecessor?.utxos ?? []);
  const postTrie = await keyValuePhasRootWithCount(postMembers);
  const previousTrie = await keyValuePhasRootWithCount(previousMembers);
  if (
    postTrie.root !== evidence.header.utxosRoot ||
    previousTrie.root !== evidence.header.prevUtxosRoot
  ) {
    throw new Error(
      "min-ada reconstructed UTxO roots do not match the challenged header",
    );
  }
  const selected = postMembers.find(
    (member) =>
      (outRef === undefined || sameOutRef(member.outRef, outRef)) &&
      descriptorViolatesMinAda(member.value) &&
      !previousMembers.some((previous) => previous.key.equals(member.key)),
  );
  if (selected === undefined) {
    throw new Error(
      outRef === undefined
        ? "authenticated post-block ledger contains no newly introduced underfunded UTxO"
        : `post-block UTxO ${outRef.transactionId}#${outRef.outputIndex.toString()} is absent, sufficiently funded, or inherited`,
    );
  }
  const postMembershipProof = await keyValuePhasProof(
    postTrie,
    selected.key,
    selected.value,
  );
  const predecessorNonMembershipProof = await keyValuePhasNonMembershipProof(
    previousTrie,
    selected.key,
  );
  return {
    kind: "min-ada-utxo",
    headerHash: evidence.headerHash,
    outRef: selected.outRef,
    outRefKeyCbor: selected.key.toString("hex"),
    descriptorCbor: selected.value.toString("hex"),
    postUtxosRoot: postTrie.root,
    prevUtxosRoot: previousTrie.root,
    postMembershipProof,
    postMembershipProofCbor: Data.to(postMembershipProof, Proof),
    predecessorNonMembershipProof,
    predecessorNonMembershipProofCbor: Data.to(
      predecessorNonMembershipProof,
      Proof,
    ),
    fault: "MinAdaUtxo" as MinAdaFault,
  };
};
