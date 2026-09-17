import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  EMPTY_MERKLE_TREE_ROOT,
  MIN_ADA_VIOLATION_ID,
  Proof,
  type Proof as ProofV1,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import type { HistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import type { JournalJsonObject } from "../workflow/journal.js";
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
} from "../workflow/native-index-artifact.js";
import {
  type PreparedMinAdaTx,
  type PreparedMinAdaUtxo,
  prepareMinAdaTxFromCanonicalEvidence,
  prepareMinAdaUtxoFromCanonicalEvidence,
} from "./prepare.js";

export const MIN_ADA_ARTIFACT =
  "midgard-production-min-ada-artifact-v1" as const;

type CommonArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof MIN_ADA_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
  }>;

export type MinAdaTxArtifact = CommonArtifact &
  Readonly<{
    kind: "min-ada-tx";
    tx: NativeInclusionArtifact;
    nativeTxCanonicalCbor: string;
    badOutputIndex: string;
    outputItemCbors: readonly string[];
    descriptorCbor: string;
  }>;

export type MinAdaUtxoArtifact = CommonArtifact &
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

export type MinAdaArtifact = MinAdaTxArtifact | MinAdaUtxoArtifact;

export type AdmittedMinAdaArtifact =
  | Readonly<{
      artifact: MinAdaTxArtifact;
      prepared: PreparedMinAdaTx;
    }>
  | Readonly<{
      artifact: MinAdaUtxoArtifact;
      prepared: PreparedMinAdaUtxo;
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

export const minAdaTxDetectionId = ({
  txId,
  outputIndex,
}: {
  readonly txId: string;
  readonly outputIndex: bigint;
}): string => `${MIN_ADA_VIOLATION_ID}:tx:${txId}:${outputIndex.toString()}`;

export const minAdaUtxoDetectionId = ({
  transactionId,
  outputIndex,
}: {
  readonly transactionId: string;
  readonly outputIndex: bigint;
}): string =>
  `${MIN_ADA_VIOLATION_ID}:utxo:${transactionId}:${outputIndex.toString()}`;

type Classification = Extract<
  CanonicalBlockClassification,
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
    selected.violationId !== MIN_ADA_VIOLATION_ID ||
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

export const prepareMinAdaArtifact = async ({
  evidence,
  historicalNativeScriptCorpus,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly historicalNativeScriptCorpus: HistoricalNativeScriptCorpus;
  readonly classification: Classification;
}): Promise<MinAdaArtifact> => {
  if (
    classification.selected.detectionId.startsWith(
      `${MIN_ADA_VIOLATION_ID}:utxo:`,
    )
  ) {
    const prepared = await prepareMinAdaUtxoFromCanonicalEvidence({
      evidence,
      historicalNativeScriptCorpus,
    });
    const detectionId = minAdaUtxoDetectionId(prepared.outRef);
    return Object.freeze({
      schemaVersion: MIN_ADA_ARTIFACT,
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
  const prepared = await prepareMinAdaTxFromCanonicalEvidence({ evidence });
  const detectionId = minAdaTxDetectionId({
    txId: prepared.badTxId,
    outputIndex: prepared.badOutputIndex,
  });
  return Object.freeze({
    schemaVersion: MIN_ADA_ARTIFACT,
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
  const headerHash = canonicalHex(
    record.headerHash,
    HEX_28,
    "min-ada header hash",
  );
  const position = safeNaturalNumber(record.position, "min-ada position");
  if (
    record.schemaVersion !== MIN_ADA_ARTIFACT ||
    typeof record.detectionId !== "string"
  ) {
    throw new Error("min-ada artifact identity changed");
  }
  return { headerHash, position, detectionId: record.detectionId };
};

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
