import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  EMPTY_MERKLE_TREE_ROOT,
  MIN_ADA_VIOLATION_ID,
  Proof,
  type Proof as ProofV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import type { HistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import {
  canonicalHex,
  EVEN_HEX,
  HEX_28,
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

export const canonicalHexList = (
  value: unknown,
  label: string,
): readonly string[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return Object.freeze(
    value.map((item, index) =>
      canonicalHex(item, EVEN_HEX, `${label}[${index.toString()}]`),
    ),
  );
};

export const replayRoot = ({
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

export const decodeProof = (cbor: string, label: string): ProofV1 => {
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

export const common = (value: unknown) => {
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
