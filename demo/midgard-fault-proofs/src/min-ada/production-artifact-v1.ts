import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  decodeMidgardLedgerOutputCommitmentV1,
  decodeMidgardSpendInputItemV1,
} from "@al-ft/midgard-core";
import {
  EMPTY_MERKLE_TREE_ROOT,
  MIN_ADA_VIOLATION_ID_V1,
  Proof,
  type Proof as ProofV1,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterialV1,
  MIDGARD_COINS_PER_UTXO_BYTE_V1,
  outputMeetsMinAdaV1,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import type { CanonicalBlockClassificationV1 } from "../workflow/classification-v1.js";
import type { JournalJsonObjectV1 } from "../workflow/journal-v1.js";
import type { ProductionHistoricalNativeScriptCorpusV1 } from "../workflow/production-historical-native-script-corpus-v1.js";
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
  type PreparedMinAdaTxV1,
  type PreparedMinAdaUtxoV1,
  prepareMinAdaTxFromCanonicalEvidenceV1,
  prepareMinAdaUtxoFromCanonicalEvidenceV1,
} from "./prepare-v1.js";

export const PRODUCTION_MIN_ADA_ARTIFACT_V1 =
  "midgard-production-min-ada-artifact-v1" as const;

type CommonArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_MIN_ADA_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
  }>;

export type ProductionMinAdaTxArtifactV1 = CommonArtifactV1 &
  Readonly<{
    kind: "min-ada-tx";
    tx: ProductionNativeInclusionArtifactV1;
    nativeTxCanonicalCbor: string;
    badOutputIndex: string;
    outputItemCbors: readonly string[];
    descriptorCbor: string;
  }>;

export type ProductionMinAdaUtxoArtifactV1 = CommonArtifactV1 &
  Readonly<{
    kind: "min-ada-utxo";
    outRef: Readonly<{ transactionId: string; outputIndex: string }>;
    outRefKeyCbor: string;
    descriptorCbor: string;
    postUtxosRoot: string;
    prevUtxosRoot: string;
    postMembershipProofCbor: string;
    predecessorNonMembershipProofCbor: string;
  }>;

export type ProductionMinAdaArtifactV1 =
  | ProductionMinAdaTxArtifactV1
  | ProductionMinAdaUtxoArtifactV1;

export type AdmittedProductionMinAdaArtifactV1 =
  | Readonly<{
      artifact: ProductionMinAdaTxArtifactV1;
      prepared: PreparedMinAdaTxV1;
    }>
  | Readonly<{
      artifact: ProductionMinAdaUtxoArtifactV1;
      prepared: PreparedMinAdaUtxoV1;
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

const replayRoot = ({
  key,
  value,
  proof,
  membership,
  label,
}: {
  readonly key: Buffer;
  readonly value?: Buffer;
  readonly proof: ProofV1;
  readonly membership: boolean;
  readonly label: string;
}): string => {
  let root: Buffer | null;
  try {
    root = MpfProof.fromJSON(key, value, proofSteps(proof)).verify(membership);
  } catch {
    throw new Error(`${label} cannot be replayed`);
  }
  return root === null ? EMPTY_MERKLE_TREE_ROOT : root.toString("hex");
};

const decodeProof = (cbor: string, label: string): ProofV1 => {
  try {
    const proof = Data.from(cbor, Proof);
    if (Data.to(proof, Proof) !== cbor) throw new Error("noncanonical");
    return proof;
  } catch {
    throw new Error(`${label} is not canonical proof CBOR`);
  }
};

export const minAdaTxDetectionIdV1 = ({
  txId,
  outputIndex,
}: {
  readonly txId: string;
  readonly outputIndex: bigint;
}): string => `${MIN_ADA_VIOLATION_ID_V1}:tx:${txId}:${outputIndex.toString()}`;

export const minAdaUtxoDetectionIdV1 = ({
  transactionId,
  outputIndex,
}: {
  readonly transactionId: string;
  readonly outputIndex: bigint;
}): string =>
  `${MIN_ADA_VIOLATION_ID_V1}:utxo:${transactionId}:${outputIndex.toString()}`;

type Classification = Extract<
  CanonicalBlockClassificationV1,
  { readonly decision: "fault_detected" }
> & { readonly category: "minAda" };

const requireClassification = ({
  classification,
  headerHash,
  detectionId,
}: {
  readonly classification: Classification;
  readonly headerHash: string;
  readonly detectionId: string;
}): number => {
  const selected = classification.selected;
  if (
    classification.headerHash !== headerHash ||
    selected.violationId !== MIN_ADA_VIOLATION_ID_V1 ||
    selected.detectionId !== detectionId ||
    selected.position < 0n ||
    selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "min-ada classification does not identify the authenticated prepared output",
    );
  }
  return Number(selected.position);
};

export const prepareProductionMinAdaArtifactV1 = async ({
  evidence,
  historicalNativeScriptCorpus,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly historicalNativeScriptCorpus: ProductionHistoricalNativeScriptCorpusV1;
  readonly classification: Classification;
}): Promise<ProductionMinAdaArtifactV1> => {
  if (
    classification.selected.detectionId.startsWith(
      `${MIN_ADA_VIOLATION_ID_V1}:utxo:`,
    )
  ) {
    const prepared = await prepareMinAdaUtxoFromCanonicalEvidenceV1({
      evidence,
      historicalNativeScriptCorpus,
    });
    const detectionId = minAdaUtxoDetectionIdV1(prepared.outRef);
    return Object.freeze({
      schemaVersion: PRODUCTION_MIN_ADA_ARTIFACT_V1,
      kind: "min-ada-utxo",
      headerHash: prepared.headerHash,
      detectionId,
      position: requireClassification({
        classification,
        headerHash: prepared.headerHash,
        detectionId,
      }),
      outRef: Object.freeze({
        transactionId: prepared.outRef.transactionId,
        outputIndex: prepared.outRef.outputIndex.toString(),
      }),
      outRefKeyCbor: prepared.outRefKeyCbor,
      descriptorCbor: prepared.descriptorCbor,
      postUtxosRoot: prepared.postUtxosRoot,
      prevUtxosRoot: prepared.prevUtxosRoot,
      postMembershipProofCbor: prepared.postMembershipProofCbor,
      predecessorNonMembershipProofCbor:
        prepared.predecessorNonMembershipProofCbor,
    });
  }
  const prepared = await prepareMinAdaTxFromCanonicalEvidenceV1({ evidence });
  const detectionId = minAdaTxDetectionIdV1({
    txId: prepared.badTxId,
    outputIndex: prepared.badOutputIndex,
  });
  return Object.freeze({
    schemaVersion: PRODUCTION_MIN_ADA_ARTIFACT_V1,
    kind: "min-ada-tx",
    headerHash: prepared.headerHash,
    detectionId,
    position: requireClassification({
      classification,
      headerHash: prepared.headerHash,
      detectionId,
    }),
    tx: Object.freeze({
      nativeTxId: prepared.txInclusion.nativeTxId,
      nativeTxCompactCbor: prepared.txInclusion.nativeTxCompactCbor,
      l2TransactionSourceCbor: prepared.txInclusion.l2TransactionSourceCbor,
      transactionsPhasRoot: prepared.txInclusion.transactionsPhasRoot,
      txMembershipProofCbor: prepared.txInclusion.txMembershipProofCbor,
    }),
    nativeTxCanonicalCbor: prepared.nativeTxCanonicalCbor,
    badOutputIndex: prepared.badOutputIndex.toString(),
    outputItemCbors: Object.freeze([...prepared.outputItemCbors]),
    descriptorCbor: prepared.descriptorCbor,
  });
};

const common = (value: unknown) => {
  const record = value as Readonly<Record<string, unknown>>;
  const headerHash = canonicalHexV1(
    record.headerHash,
    HEX_28_V1,
    "min-ada header hash",
  );
  const position = safeNaturalNumberV1(record.position, "min-ada position");
  if (
    record.schemaVersion !== PRODUCTION_MIN_ADA_ARTIFACT_V1 ||
    typeof record.detectionId !== "string"
  ) {
    throw new Error("min-ada artifact identity changed");
  }
  return { headerHash, position, detectionId: record.detectionId };
};

export const admitProductionMinAdaArtifactV1 = (
  value: unknown,
): AdmittedProductionMinAdaArtifactV1 => {
  const candidate = value as Readonly<Record<string, unknown>>;
  if (candidate.kind === "min-ada-tx") {
    const parsed = exactJournalRecordV1(
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
    const tx = admitProductionNativeInclusionArtifactV1(
      parsed.tx,
      "min-ada transaction",
    );
    const badOutputIndexString = canonicalNaturalStringV1(
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
    const material = buildCanonicalMidgardLedgerOutputMaterialV1({
      outputIndex: Number(badOutputIndex),
      outputCbor: Buffer.from(item, "hex"),
    });
    const descriptorCbor = canonicalHexV1(
      parsed.descriptorCbor,
      EVEN_HEX_V1,
      "min-ada descriptor",
    );
    if (
      material.descriptorCbor.toString("hex") !== descriptorCbor ||
      outputMeetsMinAdaV1(
        MIDGARD_COINS_PER_UTXO_BYTE_V1,
        BigInt(material.descriptor.totalLength),
        material.descriptor.lovelace,
      )
    ) {
      throw new Error(
        "min-ada transaction descriptor does not violate the floor",
      );
    }
    const detectionId = minAdaTxDetectionIdV1({
      txId: tx.artifact.nativeTxId,
      outputIndex: badOutputIndex,
    });
    if (identity.detectionId !== detectionId) {
      throw new Error("min-ada transaction detection identity changed");
    }
    const nativeTxCanonicalCbor = canonicalHexV1(
      parsed.nativeTxCanonicalCbor,
      EVEN_HEX_V1,
      "min-ada canonical transaction",
    );
    const artifact = Object.freeze({
      schemaVersion: PRODUCTION_MIN_ADA_ARTIFACT_V1,
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
  const parsed = exactJournalRecordV1(
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
  const outRefRecord = exactJournalRecordV1(
    parsed.outRef,
    ["transactionId", "outputIndex"],
    "min-ada UTxO outRef",
  );
  const transactionId = canonicalHexV1(
    outRefRecord.transactionId,
    HEX_32_V1,
    "min-ada UTxO transaction id",
  );
  const outputIndexString = canonicalNaturalStringV1(
    outRefRecord.outputIndex,
    "min-ada UTxO output index",
  );
  const outputIndex = BigInt(outputIndexString);
  const outRefKeyCbor = canonicalHexV1(
    parsed.outRefKeyCbor,
    EVEN_HEX_V1,
    "min-ada UTxO key",
  );
  const decodedKey = decodeMidgardSpendInputItemV1(
    Buffer.from(outRefKeyCbor, "hex"),
  );
  if (
    Buffer.from(decodedKey.txId).toString("hex") !== transactionId ||
    BigInt(decodedKey.outputIndex) !== outputIndex
  ) {
    throw new Error("min-ada UTxO key and outRef disagree");
  }
  const descriptorCbor = canonicalHexV1(
    parsed.descriptorCbor,
    EVEN_HEX_V1,
    "min-ada UTxO descriptor",
  );
  const descriptor = decodeMidgardLedgerOutputCommitmentV1(
    Buffer.from(descriptorCbor, "hex"),
  );
  if (
    outputMeetsMinAdaV1(
      MIDGARD_COINS_PER_UTXO_BYTE_V1,
      BigInt(descriptor.totalLength),
      descriptor.lovelace,
    )
  ) {
    throw new Error("min-ada UTxO descriptor meets the floor");
  }
  const postUtxosRoot = canonicalHexV1(
    parsed.postUtxosRoot,
    HEX_32_V1,
    "min-ada post UTxO root",
  );
  const prevUtxosRoot = canonicalHexV1(
    parsed.prevUtxosRoot,
    HEX_32_V1,
    "min-ada predecessor UTxO root",
  );
  const postMembershipProofCbor = canonicalHexV1(
    parsed.postMembershipProofCbor,
    EVEN_HEX_V1,
    "min-ada post membership proof",
  );
  const predecessorNonMembershipProofCbor = canonicalHexV1(
    parsed.predecessorNonMembershipProofCbor,
    EVEN_HEX_V1,
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
  const detectionId = minAdaUtxoDetectionIdV1({
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
    schemaVersion: PRODUCTION_MIN_ADA_ARTIFACT_V1,
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
