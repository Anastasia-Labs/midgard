/** DA-first Q34 preparation from authenticated L1 + retained block material. */
import {
  decodeMidgardAddressWitnessFieldPreimage,
  decodeMidgardFieldPreimage,
  decodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  MIDGARD_POSIX_TIME_NONE,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import { missingSignatureVkeyHash } from "@al-ft/midgard-sdk";

import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence.js";
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

export type PreparedNativeScriptInvalid = {
  readonly headerHash: string;
  readonly badTxId: string;
  readonly nativeTxCanonicalCbor: string;
  readonly nativeTxCompactCbor: string;
  readonly txInclusion: PreparedTxInclusionJson;
  readonly scriptIndex: bigint;
  readonly scriptItemCbor: string;
  readonly scriptHash: string;
  readonly addrWitnessItemCbors: readonly string[];
  readonly scriptWitnessItemCbors: readonly string[];
};

const signerHashes = (preimage: Uint8Array): ReadonlySet<string> =>
  new Set(
    decodeMidgardAddressWitnessFieldPreimage(preimage).map((witness) =>
      missingSignatureVkeyHash(
        Buffer.from(witness.verificationKey).toString("hex"),
      ),
    ),
  );

/** Selects the first accepted transaction's first well-formed false native witness. */
export const prepareNativeScriptInvalidFromCanonicalEvidence = async ({
  evidence,
  badTxId,
}: CanonicalEvidenceBuilderInput & {
  readonly badTxId?: string;
}): Promise<PreparedNativeScriptInvalid> => {
  blockTransactionsFromCanonicalEvidence(evidence as CanonicalBlockEvidence);
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  const decoded = await Promise.all(
    admitted.transactions.map(decodeTransactionMaterial),
  );
  const requested = badTxId?.toLowerCase();
  if (requested !== undefined && !/^[0-9a-f]{64}$/u.test(requested)) {
    throw new Error(
      "native-script-invalid badTxId must be 32-byte lowercase hex",
    );
  }
  const selected = decoded.flatMap((tx) => {
    if (
      tx.nativeTx.validity !== "TxIsValid" ||
      (requested !== undefined && tx.nodeTxId !== requested)
    ) {
      return [];
    }
    const scriptItems = decodeMidgardFieldPreimage(
      tx.nativeTx.witnessSet.scriptTxWitsPreimageCbor,
    );
    const signers = signerHashes(tx.nativeTx.witnessSet.addrTxWitsPreimageCbor);
    const start = tx.nativeTx.body.validityIntervalStart;
    const end = tx.nativeTx.body.validityIntervalEnd;
    return scriptItems.flatMap((item, index) => {
      const script = decodeMidgardVersionedScript(item);
      if (
        script.language !== "NativeCardano" ||
        verifyMidgardNativeScript(script.nativeScript, {
          validityIntervalStart:
            start === MIDGARD_POSIX_TIME_NONE ? undefined : start,
          validityIntervalEnd:
            end === MIDGARD_POSIX_TIME_NONE ? undefined : end,
          witnessSigners: signers,
        })
      ) {
        return [];
      }
      return [{ tx, scriptItems, item: Buffer.from(item), index, script }];
    });
  })[0];
  if (selected === undefined) {
    throw new Error(
      requested === undefined
        ? "authenticated retained DA contains no accepted false native witness"
        : `transaction ${requested} does not contain an accepted false native witness`,
    );
  }
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  await requireTransactionsRootMatch({
    sourceRoot: trie.root,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    count: BigInt(decoded.length),
  });
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
      transactionsPhasRoot: trie.root,
      txMembershipProofCbor: requireProof(
        trie,
        transactionSourceTrieItem(selected.tx).key,
        "native-script-invalid transaction",
      ),
    },
    scriptIndex: BigInt(selected.index),
    scriptItemCbor: selected.item.toString("hex"),
    scriptHash: hashMidgardVersionedScript(selected.script),
    addrWitnessItemCbors: decodeMidgardFieldPreimage(
      selected.tx.nativeTx.witnessSet.addrTxWitsPreimageCbor,
    ).map((item) => Buffer.from(item).toString("hex")),
    scriptWitnessItemCbors: selected.scriptItems.map((item) =>
      Buffer.from(item).toString("hex"),
    ),
  };
};
