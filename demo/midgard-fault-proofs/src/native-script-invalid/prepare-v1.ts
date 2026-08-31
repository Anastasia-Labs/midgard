/** DA-first Q34 preparation from authenticated L1 + retained block material. */
import {
  decodeMidgardAddressWitnessFieldPreimageV1,
  decodeMidgardFieldPreimageV1,
  decodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  MIDGARD_POSIX_TIME_NONE,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import { missingSignatureVkeyHashV1 } from "@al-ft/midgard-sdk";

import {
  blockTransactionsFromCanonicalEvidenceV1,
  type CanonicalBlockEvidenceV1,
} from "../evidence/canonical-block-evidence-v1.js";
import {
  admitCanonicalEvidenceForProofBuildV1,
  type CanonicalEvidenceBuilderInputV1,
} from "../evidence/prepare-from-evidence-v1.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type PreparedTxInclusionJson,
  requireProof,
  requireTransactionsRootMatchV1,
  transactionSourceTrieItemV1,
} from "../prepare-double-spend.js";

export type PreparedNativeScriptInvalidV1 = {
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
    decodeMidgardAddressWitnessFieldPreimageV1(preimage).map((witness) =>
      missingSignatureVkeyHashV1(
        Buffer.from(witness.verificationKey).toString("hex"),
      ),
    ),
  );

/** Selects the first accepted transaction's first well-formed false native witness. */
export const prepareNativeScriptInvalidFromCanonicalEvidenceV1 = async ({
  evidence,
  badTxId,
}: CanonicalEvidenceBuilderInputV1 & {
  readonly badTxId?: string;
}): Promise<PreparedNativeScriptInvalidV1> => {
  blockTransactionsFromCanonicalEvidenceV1(
    evidence as CanonicalBlockEvidenceV1,
  );
  const admitted = admitCanonicalEvidenceForProofBuildV1(evidence);
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
    const scriptItems = decodeMidgardFieldPreimageV1(
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
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItemV1));
  await requireTransactionsRootMatchV1({
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
        transactionSourceTrieItemV1(selected.tx).key,
        "native-script-invalid transaction",
      ),
    },
    scriptIndex: BigInt(selected.index),
    scriptItemCbor: selected.item.toString("hex"),
    scriptHash: hashMidgardVersionedScript(selected.script),
    addrWitnessItemCbors: decodeMidgardFieldPreimageV1(
      selected.tx.nativeTx.witnessSet.addrTxWitsPreimageCbor,
    ).map((item) => Buffer.from(item).toString("hex")),
    scriptWitnessItemCbors: selected.scriptItems.map((item) =>
      Buffer.from(item).toString("hex"),
    ),
  };
};
