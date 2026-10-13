import {
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";

import {
  admitNativeInclusionArtifact,
  canonicalHex,
  canonicalNaturalString,
  EVEN_HEX,
  exactJournalRecord,
} from "../workflow/native-index-artifact.js";
import {
  type AdmittedMinAdaArtifact,
  canonicalHexList,
  common,
  MIN_ADA_ARTIFACT,
  minAdaTxDetectionId,
} from "./artifact.prepare-min-ada-artifact.js";
import { minAdaOutputFacts } from "./scan.js";

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
    const facts = minAdaOutputFacts(Buffer.from(item, "hex"));
    if (
      facts === null ||
      outputMeetsMinAda(
        MIDGARD_COINS_PER_UTXO_BYTE,
        BigInt(facts.totalLength),
        facts.lovelace,
      )
    ) {
      throw new Error("min-ada transaction output does not violate the floor");
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
        txInclusion: {
          nativeTxId: tx.artifact.nativeTxId,
          nativeTx: tx.inclusion.nativeTx,
          nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
          l2TransactionSourceCbor: tx.artifact.l2TransactionSourceCbor,
          transactionsPhasRoot: tx.artifact.transactionsPhasRoot,
          txMembershipProofCbor: tx.artifact.txMembershipProofCbor,
        },
        fault: { output_index: badOutputIndex },
      },
    });
  }
  throw new Error("min-ada artifact has an unknown shape");
};
