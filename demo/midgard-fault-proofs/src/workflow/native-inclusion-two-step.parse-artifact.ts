import { parseSubmitStep01TxInclusion } from "../step-support.js";
import { type JournalJsonObject } from "./journal.js";

export const NATIVE_INCLUSION_TWO_STEP_ARTIFACT =
  "midgard-production-native-inclusion-two-step-artifact-v1" as const;

export type NativeInclusionTwoStepCategory = "invalidRange" | "zeroInput";

export type NativeInclusionTwoStepArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof NATIVE_INCLUSION_TWO_STEP_ARTIFACT;
    category: NativeInclusionTwoStepCategory;
    headerHash: string;
    detectionId: string;
    position: number;
    blockSlot: string | null;
    violationReason: string | null;
    nativeTxId: string;
    nativeTxCompactCbor: string;
    l2TransactionSourceCbor: string;
    transactionsPhasRoot: string;
    txMembershipProofCbor: string;
    sourceKind: "accepted" | "forced";
    subjectCbor: string;
    inputFieldPreimageCbor: string;
    inputFieldCommitment: string;
    forcedSourceCbor: string;
  }>;

const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

const OPTIONAL_HEX = /^(?:[0-9a-f]{2})*$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
};

const canonicalHex = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

const naturalNumber = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

export const proofSteps = (
  proof: ReturnType<typeof parseSubmitStep01TxInclusion>["txMembershipProof"],
) =>
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

export const parseArtifact = (
  value: unknown,
): NativeInclusionTwoStepArtifact => {
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "category",
      "headerHash",
      "detectionId",
      "position",
      "blockSlot",
      "violationReason",
      "nativeTxId",
      "nativeTxCompactCbor",
      "l2TransactionSourceCbor",
      "transactionsPhasRoot",
      "txMembershipProofCbor",
      "sourceKind",
      "subjectCbor",
      "inputFieldPreimageCbor",
      "inputFieldCommitment",
      "forcedSourceCbor",
    ],
    "native-inclusion two-step artifact",
  );
  if (
    parsed.schemaVersion !== NATIVE_INCLUSION_TWO_STEP_ARTIFACT ||
    (parsed.category !== "invalidRange" && parsed.category !== "zeroInput") ||
    (parsed.sourceKind !== "accepted" && parsed.sourceKind !== "forced") ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("native-inclusion two-step artifact identity changed");
  }
  let blockSlot: string | null;
  let violationReason: string | null;
  if (parsed.category === "invalidRange") {
    if (
      typeof parsed.blockSlot !== "string" ||
      !NATURAL.test(parsed.blockSlot) ||
      typeof parsed.violationReason !== "string"
    ) {
      throw new Error(
        "native-inclusion two-step artifact family fields changed",
      );
    }
    blockSlot = parsed.blockSlot;
    violationReason = parsed.violationReason;
  } else {
    if (parsed.blockSlot !== null || parsed.violationReason !== null) {
      throw new Error(
        "native-inclusion two-step artifact family fields changed",
      );
    }
    blockSlot = null;
    violationReason = null;
  }
  return Object.freeze({
    schemaVersion: NATIVE_INCLUSION_TWO_STEP_ARTIFACT,
    category: parsed.category,
    headerHash: canonicalHex(parsed.headerHash, HEX_28, "artifact header"),
    detectionId: parsed.detectionId,
    position: naturalNumber(parsed.position, "artifact position"),
    blockSlot,
    violationReason,
    nativeTxId: canonicalHex(parsed.nativeTxId, HEX_32, "artifact tx id"),
    nativeTxCompactCbor: canonicalHex(
      parsed.nativeTxCompactCbor,
      EVEN_HEX,
      "artifact compact tx",
    ),
    l2TransactionSourceCbor: canonicalHex(
      parsed.l2TransactionSourceCbor,
      EVEN_HEX,
      "artifact transaction source",
    ),
    transactionsPhasRoot: canonicalHex(
      parsed.transactionsPhasRoot,
      HEX_32,
      "artifact transaction PHAS root",
    ),
    txMembershipProofCbor: canonicalHex(
      parsed.txMembershipProofCbor,
      OPTIONAL_HEX,
      "artifact membership proof",
    ),
    sourceKind: parsed.sourceKind,
    subjectCbor: canonicalHex(
      parsed.subjectCbor,
      OPTIONAL_HEX,
      "artifact subject",
    ),
    inputFieldPreimageCbor: canonicalHex(
      parsed.inputFieldPreimageCbor,
      OPTIONAL_HEX,
      "artifact input field preimage",
    ),
    inputFieldCommitment: canonicalHex(
      parsed.inputFieldCommitment,
      HEX_32,
      "artifact input field commitment",
    ),
    forcedSourceCbor: canonicalHex(
      parsed.forcedSourceCbor,
      OPTIONAL_HEX,
      "artifact forced source",
    ),
  });
};
