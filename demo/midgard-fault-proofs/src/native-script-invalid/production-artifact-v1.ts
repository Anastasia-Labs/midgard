import {
  decodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { NATIVE_SCRIPT_INVALID_VIOLATION_ID } from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import type { CanonicalBlockClassification } from "../workflow/classification-v1.js";
import type { JournalJsonObject } from "../workflow/journal-v1.js";
import {
  admitNativeInclusionArtifact,
  canonicalHex,
  canonicalNaturalString,
  EVEN_HEX,
  exactJournalRecord,
  HEX_28,
  HEX_32,
  type NativeInclusionArtifact,
  safeNaturalNumber,
} from "../workflow/production-native-index-artifact-v1.js";
import {
  type PreparedNativeScriptInvalid,
  prepareNativeScriptInvalidFromCanonicalEvidence,
} from "./prepare-v1.js";

export const NATIVE_SCRIPT_INVALID_ARTIFACT =
  "midgard-production-native-script-invalid-artifact-v1" as const;

export type NativeScriptInvalidArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof NATIVE_SCRIPT_INVALID_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: NativeInclusionArtifact;
    nativeTxCanonicalCbor: string;
    scriptIndex: string;
    scriptItemCbor: string;
    scriptHash: string;
    addrWitnessItemCbors: readonly string[];
    scriptWitnessItemCbors: readonly string[];
  }>;

export type AdmittedNativeScriptInvalidArtifact = Readonly<{
  artifact: NativeScriptInvalidArtifact;
  prepared: PreparedNativeScriptInvalid;
}>;

const canonicalHexList = (value: unknown, label: string): readonly string[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return Object.freeze(
    value.map((item, index) =>
      canonicalHex(item, EVEN_HEX, `${label}[${index.toString()}]`),
    ),
  );
};

export const nativeScriptInvalidDetectionId = ({
  txId,
  scriptIndex,
}: {
  readonly txId: string;
  readonly scriptIndex: bigint;
}): string =>
  `${NATIVE_SCRIPT_INVALID_VIOLATION_ID}:${txId}:${scriptIndex.toString()}`;

export const prepareNativeScriptInvalidArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "nativeScriptInvalid" };
}): Promise<NativeScriptInvalidArtifact> => {
  const prepared = await prepareNativeScriptInvalidFromCanonicalEvidence({
    evidence,
  });
  const detectionId = nativeScriptInvalidDetectionId({
    txId: prepared.badTxId,
    scriptIndex: prepared.scriptIndex,
  });
  const selected = classification.selected;
  if (
    classification.headerHash !== prepared.headerHash ||
    selected.violationId !== NATIVE_SCRIPT_INVALID_VIOLATION_ID ||
    selected.detectionId !== detectionId ||
    selected.position < 0n ||
    selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "native-script-invalid classification does not identify the authenticated prepared witness",
    );
  }
  return Object.freeze({
    schemaVersion: NATIVE_SCRIPT_INVALID_ARTIFACT,
    headerHash: prepared.headerHash,
    detectionId,
    position: Number(selected.position),
    tx: Object.freeze({
      nativeTxId: prepared.txInclusion.nativeTxId,
      nativeTxCompactCbor: prepared.txInclusion.nativeTxCompactCbor,
      l2TransactionSourceCbor: prepared.txInclusion.l2TransactionSourceCbor,
      transactionsPhasRoot: prepared.txInclusion.transactionsPhasRoot,
      txMembershipProofCbor: prepared.txInclusion.txMembershipProofCbor,
    }),
    nativeTxCanonicalCbor: prepared.nativeTxCanonicalCbor,
    scriptIndex: prepared.scriptIndex.toString(),
    scriptItemCbor: prepared.scriptItemCbor,
    scriptHash: prepared.scriptHash,
    addrWitnessItemCbors: Object.freeze([...prepared.addrWitnessItemCbors]),
    scriptWitnessItemCbors: Object.freeze([...prepared.scriptWitnessItemCbors]),
  });
};

export const admitNativeScriptInvalidArtifact = (
  value: unknown,
): AdmittedNativeScriptInvalidArtifact => {
  const parsed = exactJournalRecord(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "tx",
      "nativeTxCanonicalCbor",
      "scriptIndex",
      "scriptItemCbor",
      "scriptHash",
      "addrWitnessItemCbors",
      "scriptWitnessItemCbors",
    ],
    "native-script-invalid artifact",
  );
  const tx = admitNativeInclusionArtifact(
    parsed.tx,
    "native-script-invalid transaction",
  );
  const headerHash = canonicalHex(
    parsed.headerHash,
    HEX_28,
    "native-script-invalid header hash",
  );
  const position = safeNaturalNumber(
    parsed.position,
    "native-script-invalid position",
  );
  const scriptIndexString = canonicalNaturalString(
    parsed.scriptIndex,
    "native-script-invalid script index",
  );
  const scriptIndex = BigInt(scriptIndexString);
  const scriptItemCbor = canonicalHex(
    parsed.scriptItemCbor,
    EVEN_HEX,
    "native-script-invalid script item",
  );
  const scriptHash = canonicalHex(
    parsed.scriptHash,
    HEX_28,
    "native-script-invalid script hash",
  );
  const scriptWitnessItemCbors = canonicalHexList(
    parsed.scriptWitnessItemCbors,
    "native-script-invalid script witnesses",
  );
  const addrWitnessItemCbors = canonicalHexList(
    parsed.addrWitnessItemCbors,
    "native-script-invalid address witnesses",
  );
  if (
    scriptIndex >= BigInt(scriptWitnessItemCbors.length) ||
    scriptWitnessItemCbors[Number(scriptIndex)] !== scriptItemCbor
  ) {
    throw new Error(
      "native-script-invalid selected script is absent from the authenticated field",
    );
  }
  let derivedHash: string;
  try {
    derivedHash = hashMidgardVersionedScript(
      decodeMidgardVersionedScript(Buffer.from(scriptItemCbor, "hex")),
    );
  } catch (cause) {
    throw new Error(
      `native-script-invalid selected witness is not a canonical versioned script: ${String(cause)}`,
    );
  }
  if (derivedHash !== scriptHash) {
    throw new Error(
      "native-script-invalid script bytes and committed script hash disagree",
    );
  }
  const nativeTxCanonicalCbor = canonicalHex(
    parsed.nativeTxCanonicalCbor,
    EVEN_HEX,
    "native-script-invalid canonical transaction",
  );
  const detectionId = nativeScriptInvalidDetectionId({
    txId: tx.artifact.nativeTxId,
    scriptIndex,
  });
  if (
    parsed.schemaVersion !== NATIVE_SCRIPT_INVALID_ARTIFACT ||
    parsed.detectionId !== detectionId ||
    !HEX_32.test(tx.artifact.nativeTxId)
  ) {
    throw new Error("native-script-invalid artifact identity changed");
  }
  const artifact = Object.freeze({
    schemaVersion: NATIVE_SCRIPT_INVALID_ARTIFACT,
    headerHash,
    detectionId,
    position,
    tx: tx.artifact,
    nativeTxCanonicalCbor,
    scriptIndex: scriptIndexString,
    scriptItemCbor,
    scriptHash,
    addrWitnessItemCbors,
    scriptWitnessItemCbors,
  });
  return Object.freeze({
    artifact,
    prepared: {
      headerHash,
      badTxId: tx.artifact.nativeTxId,
      nativeTxCanonicalCbor,
      nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
      txInclusion: {
        nativeTxId: tx.artifact.nativeTxId,
        nativeTx: tx.inclusion.nativeTx,
        nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
        l2TransactionSourceCbor: tx.artifact.l2TransactionSourceCbor,
        transactionsPhasRoot: tx.artifact.transactionsPhasRoot,
        txMembershipProofCbor: tx.artifact.txMembershipProofCbor,
      },
      scriptIndex,
      scriptItemCbor,
      scriptHash,
      addrWitnessItemCbors,
      scriptWitnessItemCbors,
    },
  });
};
