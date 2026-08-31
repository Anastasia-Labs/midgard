import {
  decodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1 } from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import type { CanonicalBlockClassificationV1 } from "../workflow/classification-v1.js";
import type { JournalJsonObjectV1 } from "../workflow/journal-v1.js";
import {
  admitProductionNativeInclusionArtifactV1,
  canonicalHexV1,
  canonicalNaturalStringV1,
  EVEN_HEX_V1,
  exactJournalRecordV1,
  HEX_28_V1,
  HEX_32_V1,
  type ProductionNativeInclusionArtifactV1,
  safeNaturalNumberV1,
} from "../workflow/production-native-index-artifact-v1.js";
import {
  type PreparedNativeScriptInvalidV1,
  prepareNativeScriptInvalidFromCanonicalEvidenceV1,
} from "./prepare-v1.js";

export const PRODUCTION_NATIVE_SCRIPT_INVALID_ARTIFACT_V1 =
  "midgard-production-native-script-invalid-artifact-v1" as const;

export type ProductionNativeScriptInvalidArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_NATIVE_SCRIPT_INVALID_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: ProductionNativeInclusionArtifactV1;
    nativeTxCanonicalCbor: string;
    scriptIndex: string;
    scriptItemCbor: string;
    scriptHash: string;
    addrWitnessItemCbors: readonly string[];
    scriptWitnessItemCbors: readonly string[];
  }>;

export type AdmittedProductionNativeScriptInvalidArtifactV1 = Readonly<{
  artifact: ProductionNativeScriptInvalidArtifactV1;
  prepared: PreparedNativeScriptInvalidV1;
}>;

const canonicalHexList = (value: unknown, label: string): readonly string[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return Object.freeze(
    value.map((item, index) =>
      canonicalHexV1(item, EVEN_HEX_V1, `${label}[${index.toString()}]`),
    ),
  );
};

export const nativeScriptInvalidDetectionIdV1 = ({
  txId,
  scriptIndex,
}: {
  readonly txId: string;
  readonly scriptIndex: bigint;
}): string =>
  `${NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1}:${txId}:${scriptIndex.toString()}`;

export const prepareProductionNativeScriptInvalidArtifactV1 = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  > & { readonly category: "nativeScriptInvalid" };
}): Promise<ProductionNativeScriptInvalidArtifactV1> => {
  const prepared = await prepareNativeScriptInvalidFromCanonicalEvidenceV1({
    evidence,
  });
  const detectionId = nativeScriptInvalidDetectionIdV1({
    txId: prepared.badTxId,
    scriptIndex: prepared.scriptIndex,
  });
  const selected = classification.selected;
  if (
    classification.headerHash !== prepared.headerHash ||
    selected.violationId !== NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1 ||
    selected.detectionId !== detectionId ||
    selected.position < 0n ||
    selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "native-script-invalid classification does not identify the authenticated prepared witness",
    );
  }
  return Object.freeze({
    schemaVersion: PRODUCTION_NATIVE_SCRIPT_INVALID_ARTIFACT_V1,
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

export const admitProductionNativeScriptInvalidArtifactV1 = (
  value: unknown,
): AdmittedProductionNativeScriptInvalidArtifactV1 => {
  const parsed = exactJournalRecordV1(
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
  const tx = admitProductionNativeInclusionArtifactV1(
    parsed.tx,
    "native-script-invalid transaction",
  );
  const headerHash = canonicalHexV1(
    parsed.headerHash,
    HEX_28_V1,
    "native-script-invalid header hash",
  );
  const position = safeNaturalNumberV1(
    parsed.position,
    "native-script-invalid position",
  );
  const scriptIndexString = canonicalNaturalStringV1(
    parsed.scriptIndex,
    "native-script-invalid script index",
  );
  const scriptIndex = BigInt(scriptIndexString);
  const scriptItemCbor = canonicalHexV1(
    parsed.scriptItemCbor,
    EVEN_HEX_V1,
    "native-script-invalid script item",
  );
  const scriptHash = canonicalHexV1(
    parsed.scriptHash,
    HEX_28_V1,
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
  const nativeTxCanonicalCbor = canonicalHexV1(
    parsed.nativeTxCanonicalCbor,
    EVEN_HEX_V1,
    "native-script-invalid canonical transaction",
  );
  const detectionId = nativeScriptInvalidDetectionIdV1({
    txId: tx.artifact.nativeTxId,
    scriptIndex,
  });
  if (
    parsed.schemaVersion !== PRODUCTION_NATIVE_SCRIPT_INVALID_ARTIFACT_V1 ||
    parsed.detectionId !== detectionId ||
    !HEX_32_V1.test(tx.artifact.nativeTxId)
  ) {
    throw new Error("native-script-invalid artifact identity changed");
  }
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_NATIVE_SCRIPT_INVALID_ARTIFACT_V1,
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
