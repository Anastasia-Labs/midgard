import { createHash } from "node:crypto";

import {
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  MIN_ADA_VIOLATION_ID,
  MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID,
  missingNativeScriptIsAbsent,
  missingNativeScriptTxVersionedScriptHash,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalViolationDetection } from "./classification.js";
import {
  HISTORICAL_NATIVE_SCRIPT_CORPUS_PREIMAGE,
  type HistoricalNativeScriptCorpus,
} from "./historical-native-script-corpus.create-historical-native-script-provider-roster.js";
import {
  admittedCorpusPreimages,
  type HistoricalNativeScriptCorpusPreimage,
  requireHistoricalNativeScriptCorpus,
} from "./historical-native-script-corpus.resolve-historical-native-script-corpus.js";

const corpusPreimageWithoutDigest = (
  preimage: Omit<HistoricalNativeScriptCorpusPreimage, "preimageDigest">,
) => ({
  schemaVersion: preimage.schemaVersion,
  throughHeaderHash: preimage.throughHeaderHash,
  scriptHash: preimage.scriptHash,
  scriptBytesHex: preimage.scriptBytesHex,
  occurrences: preimage.occurrences,
  providerRosterDigest: preimage.providerRosterDigest,
  corpusDigest: preimage.corpusDigest,
  checkpointDigest: preimage.checkpointDigest,
});

/** Opaque occurrence-bearing preimage derived only from an admitted corpus. */
export const historicalNativeScriptPreimageFromCorpus = ({
  corpus,
  scriptHash,
}: {
  readonly corpus: HistoricalNativeScriptCorpus;
  readonly scriptHash: string;
}): HistoricalNativeScriptCorpusPreimage | null => {
  requireHistoricalNativeScriptCorpus(corpus);
  if (!/^[0-9a-f]{56}$/u.test(scriptHash)) {
    throw new Error("historical native-script preimage hash is invalid");
  }
  const matches = corpus.entries.filter(
    (candidate) => candidate.scriptHash === scriptHash,
  );
  if (matches.length === 0) return null;
  if (matches.length !== 1) {
    throw new Error(
      "historical native-script corpus contains duplicate preimages",
    );
  }
  const entry = matches[0]!;
  if (
    entry.occurrences.length === 0 ||
    missingNativeScriptTxVersionedScriptHash(
      Buffer.from(entry.scriptBytesHex, "hex"),
    ) !== scriptHash
  ) {
    throw new Error(
      "historical native-script corpus preimage/hash binding changed",
    );
  }
  const withoutDigest = Object.freeze({
    schemaVersion: HISTORICAL_NATIVE_SCRIPT_CORPUS_PREIMAGE,
    throughHeaderHash: corpus.throughHeaderHash,
    scriptHash,
    scriptBytesHex: entry.scriptBytesHex,
    occurrences: Object.freeze(
      entry.occurrences.map((occurrence) => Object.freeze({ ...occurrence })),
    ),
    providerRosterDigest: corpus.providerRosterDigest,
    corpusDigest: corpus.corpusDigest,
    checkpointDigest: corpus.checkpointDigest,
  });
  const preimage = Object.freeze({
    ...withoutDigest,
    preimageDigest: createHash("sha256")
      .update(JSON.stringify(withoutDigest))
      .digest("hex"),
  });
  admittedCorpusPreimages.add(preimage);
  return preimage;
};

export const requireHistoricalNativeScriptCorpusPreimage = (
  preimage: HistoricalNativeScriptCorpusPreimage,
): HistoricalNativeScriptCorpusPreimage => {
  if (
    !admittedCorpusPreimages.has(preimage) ||
    preimage.preimageDigest !==
      createHash("sha256")
        .update(JSON.stringify(corpusPreimageWithoutDigest(preimage)))
        .digest("hex")
  ) {
    throw new Error(
      "historical native-script corpus preimage lacks authenticated corpus authority",
    );
  }
  return preimage;
};

export const historicalNativeScriptBytesFromCorpus = ({
  corpus,
  scriptHash,
}: {
  readonly corpus: HistoricalNativeScriptCorpus;
  readonly scriptHash: string;
}): Uint8Array | null => {
  requireHistoricalNativeScriptCorpus(corpus);
  const entry = corpus.entries.find(
    (candidate) => candidate.scriptHash === scriptHash,
  );
  return entry === undefined ? null : Buffer.from(entry.scriptBytesHex, "hex");
};

const ledgerOutRefKey = (bytes: Uint8Array): string => {
  const outRef = decodeMidgardSpendInputItem(bytes);
  return `${Buffer.from(outRef.txId).toString("hex")}#${outRef.outputIndex.toString()}`;
};

/**
 * Complete Q33 detector over every accepted spend in the challenged block.
 * The predecessor ledger and the native-script hash-to-preimage authority are
 * both retained behind the admitted, contiguous history capability.
 */
export const detectMissingNativeScriptUtxoFromHistoricalCorpus = async ({
  evidence,
  corpus,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly corpus: HistoricalNativeScriptCorpus;
}): Promise<readonly CanonicalViolationDetection[]> => {
  const history = requireHistoricalNativeScriptCorpus(corpus);
  if (
    history.currentEvidence !== evidence ||
    corpus.throughHeaderHash !== evidence.headerHash
  ) {
    throw new Error(
      "missing-native-script-utxo detector requires the exact admitted current history",
    );
  }
  const predecessor = history.reconstructions.at(-2);
  if (predecessor === undefined) return Object.freeze([]);
  if (
    predecessor.headerHash !== evidence.header.prevHeaderHash ||
    predecessor.header.utxosRoot !== evidence.header.prevUtxosRoot
  ) {
    throw new Error(
      "missing-native-script-utxo detector predecessor changed after history admission",
    );
  }
  const predecessorOutputs = new Map(
    predecessor.utxos.map((entry) => {
      const material = buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: entry.key,
        outputCbor: entry.value,
      });
      return [
        ledgerOutRefKey(entry.key),
        decodeMidgardLedgerOutputCommitment(material.descriptorCbor),
      ] as const;
    }),
  );
  const knownNativeScripts = new Set(
    corpus.entries.map((entry) => entry.scriptHash),
  );
  const detections: CanonicalViolationDetection[] = [];
  evidence.transactions.forEach((transaction, transactionIndex) => {
    const native = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(transaction.txCbor, "hex"),
    );
    if (native.validity !== "TxIsValid") return;
    const scriptWitnessItems = decodeMidgardFieldPreimage(
      native.witnessSet.scriptTxWitsPreimageCbor,
    );
    decodeMidgardFieldPreimage(native.body.spendInputsPreimageCbor).forEach(
      (inputBytes, inputIndex) => {
        const descriptor = predecessorOutputs.get(ledgerOutRefKey(inputBytes));
        if (descriptor === undefined) return;
        const credential = decodeMidgardAddressBytes(
          descriptor.address,
        ).paymentCredential;
        if (credential.kind !== "Script") return;
        const scriptHash = credential.hash.toString("hex");
        if (
          !knownNativeScripts.has(scriptHash) ||
          !missingNativeScriptIsAbsent({
            scriptTxWitsItems: scriptWitnessItems,
            expectedMissingScriptHash: scriptHash,
          })
        ) {
          return;
        }
        detections.push(
          Object.freeze({
            detectionId: `${MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID}:${transaction.nodeTxId}:${inputIndex.toString()}`,
            headerHash: evidence.headerHash,
            violationId: MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID,
            position: BigInt(transactionIndex),
            diagnostic: `accepted transaction ${transaction.nodeTxId} spends predecessor native-script output ${ledgerOutRefKey(inputBytes)} without witness ${scriptHash}`,
          }),
        );
      },
    );
  });
  return Object.freeze(detections);
};

/** Complete MIN-ADA-UTXO introduction scan against the authenticated predecessor. */
export const detectMinAdaUtxoFromHistoricalCorpus = ({
  evidence,
  corpus,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly corpus: HistoricalNativeScriptCorpus;
}): readonly CanonicalViolationDetection[] => {
  const history = requireHistoricalNativeScriptCorpus(corpus);
  if (history.currentEvidence !== evidence) {
    throw new Error(
      "min-ada detector requires the exact admitted current history",
    );
  }
  const predecessor = history.reconstructions.at(-2);
  if (
    predecessor !== undefined &&
    (predecessor.headerHash !== evidence.header.prevHeaderHash ||
      predecessor.header.utxosRoot !== evidence.header.prevUtxosRoot)
  ) {
    throw new Error("min-ada detector predecessor changed after admission");
  }
  const predecessorKeys = new Set(
    (predecessor?.utxos ?? []).map((entry) =>
      Buffer.from(entry.key).toString("hex"),
    ),
  );
  return Object.freeze(
    evidence.reconstruction.utxos.flatMap((entry, position) => {
      const keyHex = Buffer.from(entry.key).toString("hex");
      if (predecessorKeys.has(keyHex)) return [];
      const descriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: entry.key,
        outputCbor: entry.value,
      }).descriptorCbor;
      const decoded = decodeMidgardLedgerOutputCommitment(descriptor);
      if (
        outputMeetsMinAda(
          MIDGARD_COINS_PER_UTXO_BYTE,
          BigInt(decoded.totalLength),
          decoded.lovelace,
        )
      ) {
        return [];
      }
      const outRef = decodeMidgardSpendInputItem(entry.key);
      const transactionId = Buffer.from(outRef.txId).toString("hex");
      return [
        Object.freeze({
          detectionId: `${MIN_ADA_VIOLATION_ID}:utxo:${transactionId}:${outRef.outputIndex.toString()}`,
          headerHash: evidence.headerHash,
          violationId: MIN_ADA_VIOLATION_ID,
          position: BigInt(position),
          diagnostic: `post-state UTxO ${transactionId}#${outRef.outputIndex.toString()} was introduced below the exact min-Ada floor`,
        }),
      ];
    }),
  );
};
