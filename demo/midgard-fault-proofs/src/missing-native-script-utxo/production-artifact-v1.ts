import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  decodeMidgardNativeScript,
  decodeMidgardSpendInputItemV1,
  encodeMidgardSpendInputItemV1,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID_V1,
  Proof,
  type Proof as ProofV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import type { CanonicalBlockClassificationV1 } from "../workflow/classification-v1.js";
import type { JournalJsonObjectV1 } from "../workflow/journal-v1.js";
import type { ProductionHistoricalNativeScriptCorpusV1 } from "../workflow/production-historical-native-script-corpus-v1.js";
import {
  admitProductionNativeInclusionArtifactV1,
  admitProductionTxInputListV1,
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
  type PreparedMissingNativeScriptUtxoV1,
  prepareMissingNativeScriptUtxoFromCanonicalEvidenceV1,
} from "./prepare-v1.js";

export const PRODUCTION_MISSING_NATIVE_SCRIPT_UTXO_ARTIFACT_V1 =
  "midgard-production-missing-native-script-utxo-artifact-v1" as const;

type InputJsonV1 = Readonly<{ tx_id: string; output_index: string }>;

export type ProductionMissingNativeScriptUtxoArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_MISSING_NATIVE_SCRIPT_UTXO_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: ProductionNativeInclusionArtifactV1;
    nativeTxCanonicalCbor: string;
    badInputIndex: string;
    spendInputs: readonly InputJsonV1[];
    descriptorCbor: string;
    prevUtxosRoot: string;
    membershipProofCbor: string;
    missingNativeScriptBytes: string;
    expectedMissingScriptHash: string;
    scriptWitnessItemCbors: readonly string[];
  }>;

export type AdmittedProductionMissingNativeScriptUtxoArtifactV1 = Readonly<{
  artifact: ProductionMissingNativeScriptUtxoArtifactV1;
  prepared: PreparedMissingNativeScriptUtxoV1;
}>;

const proofSteps = (proof: ProofV1) =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        type: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    }
    if ("Fork" in step) {
      return {
        type: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: step.Fork.neighbor.prefix,
          root: step.Fork.neighbor.root,
        },
      };
    }
    return {
      type: "leaf" as const,
      skip: Number(step.Leaf.skip),
      neighbor: { key: step.Leaf.key, value: step.Leaf.value },
    };
  });

const canonicalHexList = (value: unknown, label: string): readonly string[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return Object.freeze(
    value.map((item, index) =>
      canonicalHexV1(item, EVEN_HEX_V1, `${label}[${index.toString()}]`),
    ),
  );
};

export const missingNativeScriptUtxoDetectionIdV1 = ({
  txId,
  inputIndex,
}: {
  readonly txId: string;
  readonly inputIndex: bigint;
}): string =>
  `${MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID_V1}:${txId}:${inputIndex.toString()}`;

export const prepareProductionMissingNativeScriptUtxoArtifactV1 = async ({
  evidence,
  historicalNativeScriptCorpus,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly historicalNativeScriptCorpus: ProductionHistoricalNativeScriptCorpusV1;
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  > & { readonly category: "missingNativeScriptUtxo" };
}): Promise<ProductionMissingNativeScriptUtxoArtifactV1> => {
  const prepared = await prepareMissingNativeScriptUtxoFromCanonicalEvidenceV1({
    evidence,
    historicalNativeScriptCorpus,
  });
  const selected = classification.selected;
  const detectionId = missingNativeScriptUtxoDetectionIdV1({
    txId: prepared.badTxId,
    inputIndex: prepared.badInputIndex,
  });
  if (
    classification.headerHash !== prepared.headerHash ||
    selected.violationId !== MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID_V1 ||
    selected.detectionId !== detectionId ||
    selected.position < 0n ||
    selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "missing-native-script-utxo classification does not identify the authenticated prepared input",
    );
  }
  const spendInputs = prepared.spendInputItemCbors.map((item) => {
    const decoded = decodeMidgardSpendInputItemV1(Buffer.from(item, "hex"));
    return Object.freeze({
      tx_id: Buffer.from(decoded.txId).toString("hex"),
      output_index: decoded.outputIndex.toString(),
    });
  });
  return Object.freeze({
    schemaVersion: PRODUCTION_MISSING_NATIVE_SCRIPT_UTXO_ARTIFACT_V1,
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
    badInputIndex: prepared.badInputIndex.toString(),
    spendInputs: Object.freeze(spendInputs),
    descriptorCbor: prepared.descriptorCbor,
    prevUtxosRoot: prepared.prevUtxosRoot,
    membershipProofCbor: prepared.membershipProofCbor,
    missingNativeScriptBytes: prepared.missingNativeScriptBytes,
    expectedMissingScriptHash: prepared.expectedMissingScriptHash,
    scriptWitnessItemCbors: Object.freeze([...prepared.scriptWitnessItemCbors]),
  });
};

export const admitProductionMissingNativeScriptUtxoArtifactV1 = (
  value: unknown,
): AdmittedProductionMissingNativeScriptUtxoArtifactV1 => {
  const parsed = exactJournalRecordV1(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "tx",
      "nativeTxCanonicalCbor",
      "badInputIndex",
      "spendInputs",
      "descriptorCbor",
      "prevUtxosRoot",
      "membershipProofCbor",
      "missingNativeScriptBytes",
      "expectedMissingScriptHash",
      "scriptWitnessItemCbors",
    ],
    "missing-native-script-utxo artifact",
  );
  const tx = admitProductionNativeInclusionArtifactV1(
    parsed.tx,
    "missing-native-script-utxo transaction",
  );
  const headerHash = canonicalHexV1(
    parsed.headerHash,
    HEX_28_V1,
    "missing-native-script-utxo header hash",
  );
  const position = safeNaturalNumberV1(
    parsed.position,
    "missing-native-script-utxo position",
  );
  const badInputIndexString = canonicalNaturalStringV1(
    parsed.badInputIndex,
    "missing-native-script-utxo input index",
  );
  const badInputIndex = BigInt(badInputIndexString);
  const spend = admitProductionTxInputListV1(
    parsed.spendInputs,
    "missing-native-script-utxo spend inputs",
  );
  if (badInputIndex >= BigInt(spend.inputs.length)) {
    throw new Error("missing-native-script-utxo input index is out of bounds");
  }
  const selectedInputJson = spend.json[Number(badInputIndex)]!;
  const descriptorCbor = canonicalHexV1(
    parsed.descriptorCbor,
    EVEN_HEX_V1,
    "missing-native-script-utxo descriptor",
  );
  const prevUtxosRoot = canonicalHexV1(
    parsed.prevUtxosRoot,
    HEX_32_V1,
    "missing-native-script-utxo predecessor root",
  );
  const membershipProofCbor = canonicalHexV1(
    parsed.membershipProofCbor,
    EVEN_HEX_V1,
    "missing-native-script-utxo predecessor membership proof",
  );
  let membershipProof: ProofV1;
  try {
    membershipProof = Data.from(membershipProofCbor, Proof);
  } catch {
    throw new Error(
      "missing-native-script-utxo predecessor membership proof is malformed",
    );
  }
  const selectedInputCbor = encodeMidgardSpendInputItemV1({
    txId: Buffer.from(selectedInputJson.tx_id, "hex"),
    outputIndex: Number(selectedInputJson.output_index),
  }).toString("hex");
  let opened: Buffer | null;
  try {
    opened = MpfProof.fromJSON(
      Buffer.from(selectedInputCbor, "hex"),
      Buffer.from(descriptorCbor, "hex"),
      proofSteps(membershipProof),
    ).verify(true);
  } catch {
    throw new Error(
      "missing-native-script-utxo predecessor membership proof cannot be replayed",
    );
  }
  if (opened === null || opened.toString("hex") !== prevUtxosRoot) {
    throw new Error(
      "missing-native-script-utxo predecessor membership proof does not open prev_utxos_root",
    );
  }
  const missingNativeScriptBytes = canonicalHexV1(
    parsed.missingNativeScriptBytes,
    EVEN_HEX_V1,
    "missing-native-script-utxo native preimage",
  );
  const expectedMissingScriptHash = canonicalHexV1(
    parsed.expectedMissingScriptHash,
    HEX_28_V1,
    "missing-native-script-utxo expected script hash",
  );
  let derivedHash: string;
  try {
    const native = decodeMidgardNativeScript(
      Buffer.from(missingNativeScriptBytes, "hex"),
    );
    derivedHash = hashMidgardVersionedScript({
      language: "NativeCardano",
      scriptBytes: native.cbor,
      nativeScript: native.script,
    });
  } catch {
    throw new Error(
      "missing-native-script-utxo native preimage does not decode canonically",
    );
  }
  if (derivedHash !== expectedMissingScriptHash) {
    throw new Error(
      "missing-native-script-utxo native preimage and expected hash disagree",
    );
  }
  const scriptWitnessItemCbors = canonicalHexList(
    parsed.scriptWitnessItemCbors,
    "missing-native-script-utxo script witnesses",
  );
  const nativeTxCanonicalCbor = canonicalHexV1(
    parsed.nativeTxCanonicalCbor,
    EVEN_HEX_V1,
    "missing-native-script-utxo canonical transaction",
  );
  const detectionId = missingNativeScriptUtxoDetectionIdV1({
    txId: tx.artifact.nativeTxId,
    inputIndex: badInputIndex,
  });
  if (
    parsed.schemaVersion !==
      PRODUCTION_MISSING_NATIVE_SCRIPT_UTXO_ARTIFACT_V1 ||
    parsed.detectionId !== detectionId
  ) {
    throw new Error("missing-native-script-utxo artifact identity changed");
  }
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_MISSING_NATIVE_SCRIPT_UTXO_ARTIFACT_V1,
    headerHash,
    detectionId,
    position,
    tx: tx.artifact,
    nativeTxCanonicalCbor,
    badInputIndex: badInputIndexString,
    spendInputs: spend.json,
    descriptorCbor,
    prevUtxosRoot,
    membershipProofCbor,
    missingNativeScriptBytes,
    expectedMissingScriptHash,
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
      badInputIndex,
      spendInputItemCbors: spend.json.map((input) =>
        encodeMidgardSpendInputItemV1({
          txId: Buffer.from(input.tx_id, "hex"),
          outputIndex: Number(input.output_index),
        }).toString("hex"),
      ),
      outRef: {
        transactionId: selectedInputJson.tx_id,
        outputIndex: BigInt(selectedInputJson.output_index),
      },
      descriptorCbor,
      prevUtxosRoot,
      membershipProof,
      membershipProofCbor,
      missingNativeScriptBytes,
      expectedMissingScriptHash,
      scriptWitnessItemCbors,
    },
  });
};
