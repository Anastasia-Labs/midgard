import { MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT } from "@al-ft/midgard-sdk";

import type { HistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import {
  admitNativeInclusionArtifact,
  canonicalHex,
  canonicalNaturalString,
  EVEN_HEX,
  exactJournalRecord,
  HEX_28,
  HEX_32,
} from "../workflow/native-index-artifact.js";
import type { FraudProofRawL1Point } from "../workflow/raw-l1-snapshot.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "../workflow/release-finality-policy.js";
import {
  admitPreparedArtifact,
  type AdmittedMissingNativeScriptTxArtifact,
  detectionCoordinates,
  inputList,
  MISSING_NATIVE_SCRIPT_TX_ARTIFACT,
  type MissingNativeScriptTxArtifact,
  type WitnessSetJson,
} from "./artifact.prepare-missing-native-script-tx-artifact.js";
import { admitHistoricalNativeScriptPreimage } from "./historical-preimage.js";
import type { HistoricalNativeScriptSourceRoster } from "./historical-script.js";

const hexList = (value: unknown, label: string): readonly string[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return Object.freeze(
    value.map((item, index) =>
      canonicalHex(item, EVEN_HEX, `${label}[${index.toString()}]`),
    ),
  );
};

/**
 * Strict persisted-shape route selector used only by the claim prerequisite.
 * The transaction port still performs full live corpus/L1 admission before it
 * captures any body; this function never grants artifact authority.
 */
export const missingNativeScriptTxArtifactUsesDirectRoute = (
  value: unknown,
): boolean => {
  const parsed = exactJournalRecord(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "badTx",
      "badTxSpendInputs",
      "badInputIndex",
      "producingTx",
      "producingOutputItemCbors",
      "historicalPreimage",
      "badTxWitnessSet",
      "badTxScriptWitnessItemCbors",
      "expectedMissingScriptHash",
    ],
    "production missing-native-script-tx route artifact",
  );
  if (parsed.schemaVersion !== MISSING_NATIVE_SCRIPT_TX_ARTIFACT) {
    throw new Error(
      "production missing-native-script-tx route artifact schema changed",
    );
  }
  return (
    hexList(
      parsed.badTxScriptWitnessItemCbors,
      "production missing-native-script-tx route witnesses",
    ).length <= MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT
  );
};

const admitWitnessSet = (value: unknown): WitnessSetJson => {
  const parsed = exactJournalRecord(
    value,
    ["addr_tx_wits_hash", "script_tx_wits_hash", "redeemer_tx_wits_hash"],
    "missing-native-script witness set",
  );
  return Object.freeze({
    addr_tx_wits_hash: canonicalHex(
      parsed.addr_tx_wits_hash,
      HEX_32,
      "missing-native-script address witness hash",
    ),
    script_tx_wits_hash: canonicalHex(
      parsed.script_tx_wits_hash,
      HEX_32,
      "missing-native-script script witness hash",
    ),
    redeemer_tx_wits_hash: canonicalHex(
      parsed.redeemer_tx_wits_hash,
      HEX_32,
      "missing-native-script redeemer witness hash",
    ),
  });
};

export const admitMissingNativeScriptTxArtifact = async ({
  value,
  historicalNativeScriptCorpus,
  historicalSourceRoster,
  historicalThroughPoint,
  releaseFinality,
}: {
  readonly value: unknown;
  readonly historicalNativeScriptCorpus: HistoricalNativeScriptCorpus;
  readonly historicalSourceRoster: HistoricalNativeScriptSourceRoster;
  readonly historicalThroughPoint: FraudProofRawL1Point;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): Promise<AdmittedMissingNativeScriptTxArtifact> => {
  const parsed = exactJournalRecord(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "badTx",
      "badTxSpendInputs",
      "badInputIndex",
      "producingTx",
      "producingOutputItemCbors",
      "historicalPreimage",
      "badTxWitnessSet",
      "badTxScriptWitnessItemCbors",
      "expectedMissingScriptHash",
    ],
    "production missing-native-script-tx artifact",
  );
  if (parsed.schemaVersion !== MISSING_NATIVE_SCRIPT_TX_ARTIFACT) {
    throw new Error(
      "production missing-native-script-tx artifact schema changed",
    );
  }
  if (
    !Number.isSafeInteger(parsed.position) ||
    (parsed.position as number) < 0
  ) {
    throw new Error("production missing-native-script-tx position is invalid");
  }
  const headerHash = canonicalHex(
    parsed.headerHash,
    HEX_28,
    "production missing-native-script-tx header hash",
  );
  if (typeof parsed.detectionId !== "string") {
    throw new Error("production missing-native-script-tx detection is invalid");
  }
  const coordinates = detectionCoordinates({
    detectionId: parsed.detectionId,
    label: "production missing-native-script-tx detection",
  });
  if (coordinates.transactionIndex !== parsed.position) {
    throw new Error("production missing-native-script-tx position changed");
  }
  const historicalPreimage = await admitHistoricalNativeScriptPreimage({
    value: parsed.historicalPreimage,
    corpus: historicalNativeScriptCorpus,
    expectedHeaderHash: headerHash,
    expectedScriptHash: coordinates.expectedScriptHash,
    roster: historicalSourceRoster,
    throughPoint: historicalThroughPoint,
    releaseFinality,
  });
  const artifact = Object.freeze({
    schemaVersion: MISSING_NATIVE_SCRIPT_TX_ARTIFACT,
    headerHash,
    detectionId: parsed.detectionId,
    position: parsed.position as number,
    badTx: admitNativeInclusionArtifact(
      parsed.badTx,
      "production missing-native-script bad transaction",
    ).artifact,
    badTxSpendInputs: inputList(
      parsed.badTxSpendInputs,
      "production missing-native-script spend inputs",
    ),
    badInputIndex: canonicalNaturalString(
      parsed.badInputIndex,
      "production missing-native-script bad input index",
    ),
    producingTx: admitNativeInclusionArtifact(
      parsed.producingTx,
      "production missing-native-script producer transaction",
    ).artifact,
    producingOutputItemCbors: hexList(
      parsed.producingOutputItemCbors,
      "production missing-native-script producer outputs",
    ),
    historicalPreimage: historicalPreimage.artifact,
    badTxWitnessSet: admitWitnessSet(parsed.badTxWitnessSet),
    badTxScriptWitnessItemCbors: hexList(
      parsed.badTxScriptWitnessItemCbors,
      "production missing-native-script witnesses",
    ),
    expectedMissingScriptHash: canonicalHex(
      parsed.expectedMissingScriptHash,
      HEX_28,
      "production missing-native-script expected hash",
    ),
  }) satisfies MissingNativeScriptTxArtifact;
  if (
    artifact.headerHash !== historicalNativeScriptCorpus.throughHeaderHash ||
    artifact.expectedMissingScriptHash !== coordinates.expectedScriptHash ||
    artifact.badTx.nativeTxId !== coordinates.badTxId ||
    artifact.producingTx.nativeTxId !== coordinates.producerTxId ||
    artifact.badInputIndex !== coordinates.inputIndex.toString()
  ) {
    throw new Error(
      "production missing-native-script artifact changed its detection/history identity",
    );
  }
  const evidence = admitPreparedArtifact(
    artifact,
    historicalPreimage.artifact.scriptBytesHex,
  );
  return Object.freeze({ artifact, evidence });
};
