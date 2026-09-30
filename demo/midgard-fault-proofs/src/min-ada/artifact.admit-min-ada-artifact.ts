import {
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";

import {
  admitNativeInclusionArtifact,
  canonicalHex,
  canonicalNaturalString,
  EVEN_HEX,
  exactJournalRecord,
  HEX_32,
} from "../workflow/native-index-artifact.js";
import {
  type AdmittedMinAdaArtifact,
  canonicalHexList,
  common,
  decodeProof,
  MIN_ADA_ARTIFACT,
  minAdaTxDetectionId,
  minAdaUtxoDetectionId,
  replayRoot,
} from "./artifact.prepare-min-ada-artifact.js";

export const admitMinAdaArtifact = (value: unknown): AdmittedMinAdaArtifact => {
  const candidate = value as Readonly<Record<string, unknown>>;
  if (candidate.kind === "min-ada-tx") {
    const parsed = exactJournalRecord(
      value,
      [
        "schemaVersion",
        "kind",
        "headerHash",
        "detectionId",
        "position",
        "tx",
        "nativeTxCanonicalCbor",
        "badOutputIndex",
        "outputItemCbors",
        "descriptorCbor",
      ],
      "min-ada transaction artifact",
    );
    const identity = common(parsed);
    const tx = admitNativeInclusionArtifact(parsed.tx, "min-ada transaction");
    const badOutputIndexString = canonicalNaturalString(
      parsed.badOutputIndex,
      "min-ada output index",
    );
    const badOutputIndex = BigInt(badOutputIndexString);
    const outputItemCbors = canonicalHexList(
      parsed.outputItemCbors,
      "min-ada output items",
    );
    const item = outputItemCbors[Number(badOutputIndex)];
    if (item === undefined)
      throw new Error("min-ada output index is out of bounds");
    const material = buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: Number(badOutputIndex),
      outputCbor: Buffer.from(item, "hex"),
    });
    const descriptorCbor = canonicalHex(
      parsed.descriptorCbor,
      EVEN_HEX,
      "min-ada descriptor",
    );
    if (
      material.descriptorCbor.toString("hex") !== descriptorCbor ||
      outputMeetsMinAda(
        MIDGARD_COINS_PER_UTXO_BYTE,
        BigInt(material.descriptor.totalLength),
        material.descriptor.lovelace,
      )
    ) {
      throw new Error(
        "min-ada transaction descriptor does not violate the floor",
      );
    }
    const detectionId = minAdaTxDetectionId({
      txId: tx.artifact.nativeTxId,
      outputIndex: badOutputIndex,
    });
    if (identity.detectionId !== detectionId) {
      throw new Error("min-ada transaction detection identity changed");
    }
    const nativeTxCanonicalCbor = canonicalHex(
      parsed.nativeTxCanonicalCbor,
      EVEN_HEX,
      "min-ada canonical transaction",
    );
    const artifact = Object.freeze({
      schemaVersion: MIN_ADA_ARTIFACT,
      kind: "min-ada-tx" as const,
      ...identity,
      tx: tx.artifact,
      nativeTxCanonicalCbor,
      badOutputIndex: badOutputIndexString,
      outputItemCbors,
      descriptorCbor,
    });
    return Object.freeze({
      artifact,
      prepared: {
        kind: "min-ada-tx",
        headerHash: identity.headerHash,
        badTxId: tx.artifact.nativeTxId,
        badOutputIndex,
        nativeTxCanonicalCbor,
        nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
        outputItemCbors,
        descriptorCbor,
        txInclusion: {
          nativeTxId: tx.artifact.nativeTxId,
          nativeTx: tx.inclusion.nativeTx,
          nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
          l2TransactionSourceCbor: tx.artifact.l2TransactionSourceCbor,
          transactionsPhasRoot: tx.artifact.transactionsPhasRoot,
          txMembershipProofCbor: tx.artifact.txMembershipProofCbor,
        },
        fault: { MinAdaTx: { output_index: badOutputIndex } },
      },
    });
  }
  const parsed = exactJournalRecord(
    value,
    [
      "schemaVersion",
      "kind",
      "headerHash",
      "detectionId",
      "position",
      "outRef",
      "outRefKeyCbor",
      "descriptorCbor",
      "postUtxosRoot",
      "prevUtxosRoot",
      "postMembershipProofCbor",
      "predecessorNonMembershipProofCbor",
    ],
    "min-ada UTxO artifact",
  );
  if (parsed.kind !== "min-ada-utxo") {
    throw new Error("min-ada artifact has an unknown shape");
  }
  const identity = common(parsed);
  const outRefRecord = exactJournalRecord(
    parsed.outRef,
    ["transactionId", "outputIndex"],
    "min-ada UTxO outRef",
  );
  const transactionId = canonicalHex(
    outRefRecord.transactionId,
    HEX_32,
    "min-ada UTxO transaction id",
  );
  const outputIndexString = canonicalNaturalString(
    outRefRecord.outputIndex,
    "min-ada UTxO output index",
  );
  const outputIndex = BigInt(outputIndexString);
  const outRefKeyCbor = canonicalHex(
    parsed.outRefKeyCbor,
    EVEN_HEX,
    "min-ada UTxO key",
  );
  const decodedKey = decodeMidgardSpendInputItem(
    Buffer.from(outRefKeyCbor, "hex"),
  );
  if (
    Buffer.from(decodedKey.txId).toString("hex") !== transactionId ||
    BigInt(decodedKey.outputIndex) !== outputIndex
  ) {
    throw new Error("min-ada UTxO key and outRef disagree");
  }
  const descriptorCbor = canonicalHex(
    parsed.descriptorCbor,
    EVEN_HEX,
    "min-ada UTxO descriptor",
  );
  const descriptor = decodeMidgardLedgerOutputCommitment(
    Buffer.from(descriptorCbor, "hex"),
  );
  if (
    outputMeetsMinAda(
      MIDGARD_COINS_PER_UTXO_BYTE,
      BigInt(descriptor.totalLength),
      descriptor.lovelace,
    )
  ) {
    throw new Error("min-ada UTxO descriptor meets the floor");
  }
  const postUtxosRoot = canonicalHex(
    parsed.postUtxosRoot,
    HEX_32,
    "min-ada post UTxO root",
  );
  const prevUtxosRoot = canonicalHex(
    parsed.prevUtxosRoot,
    HEX_32,
    "min-ada predecessor UTxO root",
  );
  const postMembershipProofCbor = canonicalHex(
    parsed.postMembershipProofCbor,
    EVEN_HEX,
    "min-ada post membership proof",
  );
  const predecessorNonMembershipProofCbor = canonicalHex(
    parsed.predecessorNonMembershipProofCbor,
    EVEN_HEX,
    "min-ada predecessor nonmembership proof",
  );
  const postMembershipProof = decodeProof(
    postMembershipProofCbor,
    "min-ada post membership proof",
  );
  const predecessorNonMembershipProof = decodeProof(
    predecessorNonMembershipProofCbor,
    "min-ada predecessor nonmembership proof",
  );
  const key = Buffer.from(outRefKeyCbor, "hex");
  if (
    replayRoot({
      key,
      value: Buffer.from(descriptorCbor, "hex"),
      proof: postMembershipProof,
      membership: true,
      label: "min-ada post membership proof",
    }) !== postUtxosRoot ||
    replayRoot({
      key,
      proof: predecessorNonMembershipProof,
      membership: false,
      label: "min-ada predecessor nonmembership proof",
    }) !== prevUtxosRoot
  ) {
    throw new Error(
      "min-ada UTxO proofs do not open their authenticated roots",
    );
  }
  const detectionId = minAdaUtxoDetectionId({
    transactionId,
    outputIndex,
  });
  if (identity.detectionId !== detectionId) {
    throw new Error("min-ada UTxO detection identity changed");
  }
  const outRef = Object.freeze({
    transactionId,
    outputIndex: outputIndexString,
  });
  const artifact = Object.freeze({
    schemaVersion: MIN_ADA_ARTIFACT,
    kind: "min-ada-utxo" as const,
    ...identity,
    outRef,
    outRefKeyCbor,
    descriptorCbor,
    postUtxosRoot,
    prevUtxosRoot,
    postMembershipProofCbor,
    predecessorNonMembershipProofCbor,
  });
  return Object.freeze({
    artifact,
    prepared: {
      kind: "min-ada-utxo",
      headerHash: identity.headerHash,
      outRef: { transactionId, outputIndex },
      outRefKeyCbor,
      descriptorCbor,
      postUtxosRoot,
      prevUtxosRoot,
      postMembershipProof,
      postMembershipProofCbor,
      predecessorNonMembershipProof,
      predecessorNonMembershipProofCbor,
      fault: "MinAdaUtxo",
    },
  });
};
