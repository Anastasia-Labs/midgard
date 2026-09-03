import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  decodeMidgardNativeScript,
  decodeMidgardSpendInputItem,
  encodeMidgardSpendInputItem,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID,
  Proof,
  type Proof as ProofV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import type { CanonicalBlockClassification } from "../workflow/classification-v1.js";
import type { JournalJsonObject } from "../workflow/journal-v1.js";
import type { HistoricalNativeScriptCorpus } from "../workflow/production-historical-native-script-corpus-v1.js";
import {
  admitNativeInclusionArtifact,
  admitTxInputList,
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
  type PreparedMissingNativeScriptUtxo,
  prepareMissingNativeScriptUtxoFromCanonicalEvidence,
} from "./prepare-v1.js";

export const MISSING_NATIVE_SCRIPT_UTXO_ARTIFACT =
  "midgard-production-missing-native-script-utxo-artifact-v1" as const;

type InputJson = Readonly<{ tx_id: string; output_index: string }>;

export type MissingNativeScriptUtxoArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof MISSING_NATIVE_SCRIPT_UTXO_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: NativeInclusionArtifact;
    nativeTxCanonicalCbor: string;
    badInputIndex: string;
    spendInputs: readonly InputJson[];
    descriptorCbor: string;
    prevUtxosRoot: string;
    membershipProofCbor: string;
    missingNativeScriptBytes: string;
    expectedMissingScriptHash: string;
    scriptWitnessItemCbors: readonly string[];
  }>;

export type AdmittedMissingNativeScriptUtxoArtifact = Readonly<{
  artifact: MissingNativeScriptUtxoArtifact;
  prepared: PreparedMissingNativeScriptUtxo;
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
      canonicalHex(item, EVEN_HEX, `${label}[${index.toString()}]`),
    ),
  );
};

export const missingNativeScriptUtxoDetectionId = ({
  txId,
  inputIndex,
}: {
  readonly txId: string;
  readonly inputIndex: bigint;
}): string =>
  `${MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID}:${txId}:${inputIndex.toString()}`;

export const prepareMissingNativeScriptUtxoArtifact = async ({
  evidence,
  historicalNativeScriptCorpus,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly historicalNativeScriptCorpus: HistoricalNativeScriptCorpus;
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "missingNativeScriptUtxo" };
}): Promise<MissingNativeScriptUtxoArtifact> => {
  const prepared = await prepareMissingNativeScriptUtxoFromCanonicalEvidence({
    evidence,
    historicalNativeScriptCorpus,
  });
  const selected = classification.selected;
  const detectionId = missingNativeScriptUtxoDetectionId({
    txId: prepared.badTxId,
    inputIndex: prepared.badInputIndex,
  });
  if (
    classification.headerHash !== prepared.headerHash ||
    selected.violationId !== MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID ||
    selected.detectionId !== detectionId ||
    selected.position < 0n ||
    selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "missing-native-script-utxo classification does not identify the authenticated prepared input",
    );
  }
  const spendInputs = prepared.spendInputItemCbors.map((item) => {
    const decoded = decodeMidgardSpendInputItem(Buffer.from(item, "hex"));
    return Object.freeze({
      tx_id: Buffer.from(decoded.txId).toString("hex"),
      output_index: decoded.outputIndex.toString(),
    });
  });
  return Object.freeze({
    schemaVersion: MISSING_NATIVE_SCRIPT_UTXO_ARTIFACT,
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

export const admitMissingNativeScriptUtxoArtifact = (
  value: unknown,
): AdmittedMissingNativeScriptUtxoArtifact => {
  const parsed = exactJournalRecord(
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
  const tx = admitNativeInclusionArtifact(
    parsed.tx,
    "missing-native-script-utxo transaction",
  );
  const headerHash = canonicalHex(
    parsed.headerHash,
    HEX_28,
    "missing-native-script-utxo header hash",
  );
  const position = safeNaturalNumber(
    parsed.position,
    "missing-native-script-utxo position",
  );
  const badInputIndexString = canonicalNaturalString(
    parsed.badInputIndex,
    "missing-native-script-utxo input index",
  );
  const badInputIndex = BigInt(badInputIndexString);
  const spend = admitTxInputList(
    parsed.spendInputs,
    "missing-native-script-utxo spend inputs",
  );
  if (badInputIndex >= BigInt(spend.inputs.length)) {
    throw new Error("missing-native-script-utxo input index is out of bounds");
  }
  const selectedInputJson = spend.json[Number(badInputIndex)]!;
  const descriptorCbor = canonicalHex(
    parsed.descriptorCbor,
    EVEN_HEX,
    "missing-native-script-utxo descriptor",
  );
  const prevUtxosRoot = canonicalHex(
    parsed.prevUtxosRoot,
    HEX_32,
    "missing-native-script-utxo predecessor root",
  );
  const membershipProofCbor = canonicalHex(
    parsed.membershipProofCbor,
    EVEN_HEX,
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
  const selectedInputCbor = encodeMidgardSpendInputItem({
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
  const missingNativeScriptBytes = canonicalHex(
    parsed.missingNativeScriptBytes,
    EVEN_HEX,
    "missing-native-script-utxo native preimage",
  );
  const expectedMissingScriptHash = canonicalHex(
    parsed.expectedMissingScriptHash,
    HEX_28,
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
  const nativeTxCanonicalCbor = canonicalHex(
    parsed.nativeTxCanonicalCbor,
    EVEN_HEX,
    "missing-native-script-utxo canonical transaction",
  );
  const detectionId = missingNativeScriptUtxoDetectionId({
    txId: tx.artifact.nativeTxId,
    inputIndex: badInputIndex,
  });
  if (
    parsed.schemaVersion !== MISSING_NATIVE_SCRIPT_UTXO_ARTIFACT ||
    parsed.detectionId !== detectionId
  ) {
    throw new Error("missing-native-script-utxo artifact identity changed");
  }
  const artifact = Object.freeze({
    schemaVersion: MISSING_NATIVE_SCRIPT_UTXO_ARTIFACT,
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
        encodeMidgardSpendInputItem({
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
