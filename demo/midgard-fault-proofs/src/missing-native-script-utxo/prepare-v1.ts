/** DA-first Q33 preparation against an authenticated predecessor ledger. */
import {
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  missingNativeScriptIsAbsent,
  type OutputReference,
  Proof,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalEvidenceBuilderInput,
} from "../evidence/prepare-from-evidence-v1.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type PreparedTxInclusionJson,
  requireProof,
  requireTransactionsRootMatch,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import {
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../transition-trace/phas.js";
import {
  type HistoricalNativeScriptCorpus,
  requireHistoricalNativeScriptCorpus,
} from "../workflow/production-historical-native-script-corpus-v1.js";

export type PreparedMissingNativeScriptUtxo = {
  readonly headerHash: string;
  readonly badTxId: string;
  readonly nativeTxCanonicalCbor: string;
  readonly nativeTxCompactCbor: string;
  readonly txInclusion: PreparedTxInclusionJson;
  readonly badInputIndex: bigint;
  readonly spendInputItemCbors: readonly string[];
  readonly outRef: OutputReference;
  readonly descriptorCbor: string;
  readonly prevUtxosRoot: string;
  readonly membershipProof: Proof;
  readonly membershipProofCbor: string;
  readonly missingNativeScriptBytes: string;
  readonly expectedMissingScriptHash: string;
  readonly scriptWitnessItemCbors: readonly string[];
};

const outRefKey = (entry: { readonly key: Uint8Array }) => {
  const decoded = decodeMidgardSpendInputItem(entry.key);
  return `${Buffer.from(decoded.txId).toString("hex")}#${decoded.outputIndex.toString()}`;
};

/**
 * The native preimage corpus is itself authenticated retained DA. It may span
 * older blocks because a predecessor UTxO's credential does not reveal the
 * language or payload, only the versioned script hash.
 */
export const prepareMissingNativeScriptUtxoFromCanonicalEvidence = async ({
  evidence,
  historicalNativeScriptCorpus,
  badTxId,
}: CanonicalEvidenceBuilderInput & {
  readonly historicalNativeScriptCorpus: HistoricalNativeScriptCorpus;
  readonly badTxId?: string;
}): Promise<PreparedMissingNativeScriptUtxo> => {
  const history = requireHistoricalNativeScriptCorpus(
    historicalNativeScriptCorpus,
  );
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  if (
    history.currentEvidence.headerHash !== evidence.headerHash ||
    history.currentEvidence.payloadEnvelopeSha256 !==
      evidence.payloadEnvelopeSha256
  ) {
    throw new Error(
      "missing-native-script-utxo historical corpus is not bound to the challenged evidence",
    );
  }
  const previousReconstruction =
    history.reconstructions[history.reconstructions.length - 2];
  if (previousReconstruction === undefined) {
    throw new Error(
      "missing-native-script-utxo cannot exist against the empty genesis predecessor",
    );
  }
  if (
    previousReconstruction.headerHash !== evidence.header.prevHeaderHash ||
    previousReconstruction.header.utxosRoot !== evidence.header.prevUtxosRoot
  ) {
    throw new Error(
      "missing-native-script-utxo predecessor evidence does not match the challenged header",
    );
  }
  const previousMembers = previousReconstruction.utxos.map((entry) => {
    const material = buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: entry.key,
      outputCbor: entry.value,
    });
    return {
      key: Buffer.from(entry.key),
      value: Buffer.from(material.descriptorCbor),
      outRefKey: outRefKey(entry),
    };
  });
  const previousTrie = await keyValuePhasRootWithCount(previousMembers);
  if (previousTrie.root !== evidence.header.prevUtxosRoot) {
    throw new Error(
      "missing-native-script-utxo predecessor material does not reproduce prev_utxos_root",
    );
  }
  const nativeScripts = historicalNativeScriptCorpus.entries.map((entry) => ({
    hash: entry.scriptHash,
    bytes: Buffer.from(entry.scriptBytesHex, "hex"),
  }));
  const decoded = await Promise.all(
    admitted.transactions.map(decodeTransactionMaterial),
  );
  const requested = badTxId?.toLowerCase();
  const selected = decoded.flatMap((tx) => {
    if (
      tx.nativeTx.validity !== "TxIsValid" ||
      (requested !== undefined && tx.nodeTxId !== requested)
    ) {
      return [];
    }
    const inputItems = decodeMidgardFieldPreimage(
      tx.nativeTx.body.spendInputsPreimageCbor,
    );
    const scriptItems = decodeMidgardFieldPreimage(
      tx.nativeTx.witnessSet.scriptTxWitsPreimageCbor,
    );
    return inputItems.flatMap((item, inputIndex) => {
      const input = decodeMidgardSpendInputItem(item);
      const key = `${Buffer.from(input.txId).toString("hex")}#${input.outputIndex.toString()}`;
      const member = previousMembers.find(
        (candidate) => candidate.outRefKey === key,
      );
      if (member === undefined) return [];
      const descriptor = decodeMidgardLedgerOutputCommitment(member.value);
      const credential = decodeMidgardAddressBytes(
        descriptor.address,
      ).paymentCredential;
      if (credential.kind !== "Script") return [];
      const expectedHash = credential.hash.toString("hex");
      const preimage = nativeScripts.find(
        (candidate) => candidate.hash === expectedHash,
      );
      if (
        preimage === undefined ||
        !missingNativeScriptIsAbsent({
          scriptTxWitsItems: scriptItems,
          expectedMissingScriptHash: expectedHash,
        })
      ) {
        return [];
      }
      return [
        {
          tx,
          inputItems,
          scriptItems,
          inputIndex,
          input,
          member,
          preimage,
          expectedHash,
        },
      ];
    });
  })[0];
  if (selected === undefined) {
    throw new Error(
      "authenticated retained evidence contains no missing-native-script-utxo violation with a known native preimage",
    );
  }
  const txTrie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  await requireTransactionsRootMatch({
    sourceRoot: txTrie.root,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    count: BigInt(decoded.length),
  });
  const membershipProof = await keyValuePhasProof(
    previousTrie,
    selected.member.key,
    selected.member.value,
  );
  const outRef: OutputReference = {
    transactionId: Buffer.from(selected.input.txId).toString("hex"),
    outputIndex: BigInt(selected.input.outputIndex),
  };
  return {
    headerHash: admitted.headerHash,
    badTxId: selected.tx.nodeTxId,
    nativeTxCanonicalCbor: selected.tx.txCbor,
    nativeTxCompactCbor: selected.tx.nativeCompactCbor,
    txInclusion: {
      nativeTxId: selected.tx.nodeTxId,
      nativeTx: selected.tx.nativeTxCompact,
      nativeTxCompactCbor: selected.tx.nativeCompactCbor,
      l2TransactionSourceCbor: selected.tx.l2TransactionSourceCbor,
      transactionsPhasRoot: txTrie.root,
      txMembershipProofCbor: requireProof(
        txTrie,
        transactionSourceTrieItem(selected.tx).key,
        "missing-native-script-utxo transaction",
      ),
    },
    badInputIndex: BigInt(selected.inputIndex),
    spendInputItemCbors: selected.inputItems.map((item) =>
      Buffer.from(item).toString("hex"),
    ),
    outRef,
    descriptorCbor: selected.member.value.toString("hex"),
    prevUtxosRoot: previousTrie.root,
    membershipProof,
    membershipProofCbor: Data.to(membershipProof, Proof),
    missingNativeScriptBytes: Buffer.from(selected.preimage.bytes).toString(
      "hex",
    ),
    expectedMissingScriptHash: selected.expectedHash,
    scriptWitnessItemCbors: selected.scriptItems.map((item) =>
      Buffer.from(item).toString("hex"),
    ),
  };
};
