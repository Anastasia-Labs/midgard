import {
  decodeMidgardNativeByteListPreimage,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  ForcedInclusionTxV1Schema,
  HeaderSchema,
  MIDGARD_FIELD_INDEX,
  OutputReferenceSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
} from "../field-opening.js";
import {
  INPUT_SET_UNIQUENESS_VIOLATION_ID,
  type InputSetUniquenessClaim,
  scanInputSetUniqueness,
} from "../input-set-uniqueness/scan.js";
import { type JournalJsonObject } from "./journal.js";
import {
  admitNativeInclusionArtifact,
  canonicalHex,
  EVEN_HEX,
  exactJournalRecord,
  HEX_28,
  type NativeInclusionArtifact,
  NATURAL_DECIMAL,
  safeNaturalNumber,
} from "./native-index-artifact.js";

export const INPUT_SET_UNIQUENESS_ARTIFACT =
  "midgard-production-input-set-uniqueness-artifact-v1" as const;

export const INPUT_SET_UNIQUENESS_FORCED_ARTIFACT =
  "midgard-production-input-set-uniqueness-forced-artifact-v1" as const;

export const InputSetUniquenessForcedSourceSchema = Data.Object({
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
});

type ClaimJson =
  | Readonly<{
      kind: "duplicateSpendInputs" | "duplicateReferenceInputs";
      firstIndex: string;
      secondIndex: string;
    }>
  | Readonly<{
      kind: "spendReferenceOverlap";
      spendIndex: string;
      referenceIndex: string;
    }>;

export type InputSetUniquenessArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof INPUT_SET_UNIQUENESS_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    tx: NativeInclusionArtifact;
    spendInputItemCbors: readonly string[];
    referenceInputItemCbors: readonly string[];
    claim: ClaimJson;
  }>;

export type InputSetUniquenessForcedArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof INPUT_SET_UNIQUENESS_FORCED_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    forcedIndex: number;
    transactionId: string;
    subjectCbor: string;
    nativeTxCompactCbor: string;
    spendInputItemCbors: readonly string[];
    referenceInputItemCbors: readonly string[];
    forcedSourceCbor: string;
  }>;

export type AdmittedAcceptedArtifact = Readonly<{
  sourceKind: "accepted";
  artifact: InputSetUniquenessArtifact;
  inclusion: ReturnType<typeof admitNativeInclusionArtifact>["inclusion"];
  claim: InputSetUniquenessClaim;
  spendPlan: FaultProofFieldOpeningPlan | null;
  referencePlan: FaultProofFieldOpeningPlan | null;
}>;

export type AdmittedForcedArtifact = Readonly<{
  sourceKind: "forced";
  artifact: InputSetUniquenessForcedArtifact;
  forcedSource: Data.Static<typeof InputSetUniquenessForcedSourceSchema>;
  spendPlan: FaultProofFieldOpeningPlan;
  referencePlan: FaultProofFieldOpeningPlan;
}>;

export type AdmittedArtifact =
  | AdmittedAcceptedArtifact
  | AdmittedForcedArtifact;

export const inputItems = (
  tx: MidgardNativeTxFull,
  field: "spend" | "reference",
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    field === "spend"
      ? tx.body.spendInputsPreimageCbor
      : tx.body.referenceInputsPreimageCbor,
    `${field} inputs`,
  ).map((item) => Buffer.from(item).toString("hex"));

export const claimJson = (claim: InputSetUniquenessClaim): ClaimJson =>
  claim.kind === "spendReferenceOverlap"
    ? Object.freeze({
        kind: claim.kind,
        spendIndex: claim.spendIndex.toString(),
        referenceIndex: claim.referenceIndex.toString(),
      })
    : Object.freeze({
        kind: claim.kind,
        firstIndex: claim.firstIndex.toString(),
        secondIndex: claim.secondIndex.toString(),
      });

export const claimIdentity = (claim: InputSetUniquenessClaim): string =>
  claim.kind === "spendReferenceOverlap"
    ? `${claim.kind}:${claim.spendIndex.toString()}:${claim.referenceIndex.toString()}`
    : `${claim.kind}:${claim.firstIndex.toString()}:${claim.secondIndex.toString()}`;

const parseClaim = (value: unknown): InputSetUniquenessClaim => {
  const record = exactJournalRecord(
    value,
    typeof value === "object" && value !== null && "kind" in value
      ? (value as { readonly kind?: unknown }).kind === "spendReferenceOverlap"
        ? ["kind", "spendIndex", "referenceIndex"]
        : ["kind", "firstIndex", "secondIndex"]
      : [],
    "input-set-uniqueness claim",
  );
  const natural = (field: string): bigint => {
    const item = record[field];
    if (typeof item !== "string" || !NATURAL_DECIMAL.test(item)) {
      throw new Error(`input-set-uniqueness claim ${field} is malformed`);
    }
    return BigInt(item);
  };
  if (record.kind === "duplicateSpendInputs") {
    return {
      kind: "duplicateSpendInputs",
      firstIndex: natural("firstIndex"),
      secondIndex: natural("secondIndex"),
    };
  }
  if (record.kind === "duplicateReferenceInputs") {
    return {
      kind: "duplicateReferenceInputs",
      firstIndex: natural("firstIndex"),
      secondIndex: natural("secondIndex"),
    };
  }
  if (record.kind === "spendReferenceOverlap") {
    return {
      kind: "spendReferenceOverlap",
      spendIndex: natural("spendIndex"),
      referenceIndex: natural("referenceIndex"),
    };
  }
  throw new Error("input-set-uniqueness claim kind is unsupported");
};

export const parseItemList = (
  value: unknown,
  label: string,
): readonly string[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return Object.freeze(
    value.map((item, index) => {
      const parsed = canonicalHex(
        item,
        EVEN_HEX,
        `${label}[${index.toString()}]`,
      );
      if (!/^825820[0-9a-f]{64}19[0-9a-f]{4}$/u.test(parsed)) {
        throw new Error(`${label}[${index.toString()}] is not an out-ref item`);
      }
      return parsed;
    }),
  );
};

export const admitAcceptedInputSetUniquenessArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedAcceptedArtifact => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("input-set-uniqueness carriage owner is malformed");
  }
  const parsed = exactJournalRecord(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "tx",
      "spendInputItemCbors",
      "referenceInputItemCbors",
      "claim",
    ],
    "input-set-uniqueness artifact",
  );
  if (
    parsed.schemaVersion !== INPUT_SET_UNIQUENESS_ARTIFACT ||
    typeof parsed.detectionId !== "string"
  ) {
    throw new Error("input-set-uniqueness artifact identity changed");
  }
  const headerHash = canonicalHex(
    parsed.headerHash,
    HEX_28,
    "input-set-uniqueness header hash",
  );
  const position = safeNaturalNumber(
    parsed.position,
    "input-set-uniqueness position",
  );
  const tx = admitNativeInclusionArtifact(
    parsed.tx,
    "input-set-uniqueness transaction",
  );
  if (tx.inclusion.nativeTx.validity_code !== 0n) {
    throw new Error("input-set-uniqueness transaction is not accepted");
  }
  const spends = parseItemList(
    parsed.spendInputItemCbors,
    "input-set-uniqueness spend inputs",
  );
  const references = parseItemList(
    parsed.referenceInputItemCbors,
    "input-set-uniqueness reference inputs",
  );
  const claim = parseClaim(parsed.claim);
  const rederived = scanInputSetUniqueness({
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
  });
  if (
    !rederived.some(
      (candidate) => claimIdentity(candidate) === claimIdentity(claim),
    )
  ) {
    throw new Error(
      "input-set-uniqueness artifact does not re-derive its claim",
    );
  }
  const expectedDetection = `${INPUT_SET_UNIQUENESS_VIOLATION_ID}:${position.toString()}:${tx.artifact.nativeTxId}:${claimIdentity(claim)}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("input-set-uniqueness detection identity changed");
  }
  const plan = (
    fieldIndex: number,
    items: readonly string[],
  ): FaultProofFieldOpeningPlan =>
    planFaultProofFieldOpening({
      anchorSourceKind: 0n,
      fieldIndex,
      anchorTxId: tx.artifact.nativeTxId,
      nativeTxCompactCbor: tx.artifact.nativeTxCompactCbor,
      itemCbors: items.map((item) => Buffer.from(item, "hex")),
      owner: carriageOwner,
      label: "input-set-uniqueness artifact",
    });
  const spendPlan =
    claim.kind === "duplicateReferenceInputs"
      ? null
      : plan(MIDGARD_FIELD_INDEX.spendInputs, spends);
  const referencePlan =
    claim.kind === "duplicateSpendInputs"
      ? null
      : plan(MIDGARD_FIELD_INDEX.referenceInputs, references);
  const artifact = Object.freeze({
    schemaVersion: INPUT_SET_UNIQUENESS_ARTIFACT,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    tx: tx.artifact,
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
    claim: claimJson(claim),
  }) satisfies InputSetUniquenessArtifact;
  return Object.freeze({
    sourceKind: "accepted" as const,
    artifact,
    inclusion: tx.inclusion,
    claim,
    spendPlan,
    referencePlan,
  });
};
