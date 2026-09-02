import { createHash } from "node:crypto";

import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_V1 =
  "distinctAssetAccumulationLimit" as const;
export const DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID_V1 =
  "00000035" as const;
export const MIDGARD_MAX_DISTINCT_ASSETS_V1 = 16_384 as const;

export type DistinctAssetAccumulationCoordinateV1 =
  | Readonly<{ kind: "input"; inputIndex: number; assetIndex: number }>
  | Readonly<{ kind: "output"; outputIndex: number; assetIndex: number }>
  | Readonly<{ kind: "mint"; mintIndex: number }>;

export type DistinctAssetAccumulationFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  coordinate: DistinctAssetAccumulationCoordinateV1;
}>;

const fail = (message: string): never => {
  throw new Error(
    `${DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_V1}: ${message}`,
  );
};
const index = (value: number, label: string): number =>
  Number.isSafeInteger(value) && value >= 0
    ? value
    : fail(`${label} is not a non-negative safe integer`);

const reasonMatches = (
  reason: VerdictSubjectV1["rejection_reason"],
  coordinate: DistinctAssetAccumulationCoordinateV1,
): boolean => {
  if (reason === null || typeof reason === "string") return false;
  if (coordinate.kind === "input")
    return (
      "InputAssetAccumulationLimit" in reason &&
      reason.InputAssetAccumulationLimit.input_index ===
        BigInt(coordinate.inputIndex) &&
      reason.InputAssetAccumulationLimit.asset_index ===
        BigInt(coordinate.assetIndex)
    );
  if (coordinate.kind === "output")
    return (
      "OutputAssetAccumulationLimit" in reason &&
      reason.OutputAssetAccumulationLimit.output_index ===
        BigInt(coordinate.outputIndex) &&
      reason.OutputAssetAccumulationLimit.asset_index ===
        BigInt(coordinate.assetIndex)
    );
  return (
    "MintAssetAccumulationLimit" in reason &&
    reason.MintAssetAccumulationLimit.mint_index ===
      BigInt(coordinate.mintIndex)
  );
};

export const classifyDistinctAssetAccumulationFindingV1 = (
  finding: DistinctAssetAccumulationFindingV1,
): DistinctAssetAccumulationFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    fail("verdict subject is not canonical");
  if (finding.coordinate.kind === "input") {
    index(finding.coordinate.inputIndex, "input index");
    index(finding.coordinate.assetIndex, "asset index");
  } else if (finding.coordinate.kind === "output") {
    index(finding.coordinate.outputIndex, "output index");
    index(finding.coordinate.assetIndex, "asset index");
  } else index(finding.coordinate.mintIndex, "mint index");
  if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    if (!reasonMatches(finding.subject.rejection_reason, finding.coordinate))
      fail("typed rejection reason or coordinate changed");
  } else if (
    finding.subject.direction !==
      PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    finding.subject.rejection_reason !== null
  )
    fail("direction/reason polarity changed");
  return Object.freeze(finding);
};

export type DistinctAssetAccumulatorCheckpointV1 = Readonly<{
  assetRootHex: string;
  seenAssetCount: number;
  nonzeroAssetCount: number;
  cursor: number;
}>;

export type DistinctAssetAccumulationEvidenceV1 = Readonly<{
  finding: DistinctAssetAccumulationFindingV1;
  traceStateHashHex: string;
  workRootHex: string;
  pre: DistinctAssetAccumulatorCheckpointV1;
  post: DistinctAssetAccumulatorCheckpointV1 | null;
  mutationWasPresent: boolean;
}>;

const h32 = (value: string, label: string): string =>
  /^[0-9a-f]{64}$/u.test(value) ? value : fail(`${label} is not canonical h32`);

export const prepareDistinctAssetAccumulationEvidenceV1 = (
  evidence: DistinctAssetAccumulationEvidenceV1,
): DistinctAssetAccumulationEvidenceV1 => {
  classifyDistinctAssetAccumulationFindingV1(evidence.finding);
  h32(evidence.traceStateHashHex, "trace state hash");
  h32(evidence.workRootHex, "work root");
  h32(evidence.pre.assetRootHex, "pre asset root");
  index(evidence.pre.seenAssetCount, "pre seen count");
  index(evidence.pre.nonzeroAssetCount, "pre nonzero count");
  index(evidence.pre.cursor, "pre cursor");
  if (evidence.pre.seenAssetCount > MIDGARD_MAX_DISTINCT_ASSETS_V1)
    fail("pre accumulator already exceeds consensus limit");
  if (evidence.pre.nonzeroAssetCount > evidence.pre.seenAssetCount)
    fail("pre nonzero count exceeds seen count");
  const crossing =
    !evidence.mutationWasPresent &&
    evidence.pre.seenAssetCount === MIDGARD_MAX_DISTINCT_ASSETS_V1;
  if (crossing !== (evidence.post === null))
    fail("mutation successor does not match exact first-crossing polarity");
  if (evidence.post !== null) {
    h32(evidence.post.assetRootHex, "post asset root");
    index(evidence.post.seenAssetCount, "post seen count");
    index(evidence.post.nonzeroAssetCount, "post nonzero count");
    if (
      evidence.post.seenAssetCount !==
      evidence.pre.seenAssetCount + (evidence.mutationWasPresent ? 0 : 1)
    )
      fail("post seen count is not the canonical mutation successor");
  }
  return Object.freeze(evidence);
};

export const distinctAssetAccumulationFaultHoldsV1 = (
  evidence: DistinctAssetAccumulationEvidenceV1,
): boolean =>
  prepareDistinctAssetAccumulationEvidenceV1(evidence).post === null;

export const distinctAssetAccumulationEvidenceClosesV1 = (
  evidence: DistinctAssetAccumulationEvidenceV1,
): boolean =>
  evidence.finding.subject.direction ===
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
    ? distinctAssetAccumulationFaultHoldsV1(evidence)
    : !distinctAssetAccumulationFaultHoldsV1(evidence);

export const distinctAssetAccumulationEvidenceIdentityV1 = (
  evidence: DistinctAssetAccumulationEvidenceV1,
): string =>
  createHash("sha256")
    .update(DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID_V1)
    .update(evidence.finding.subject.transaction_id)
    .update(JSON.stringify(evidence.finding.coordinate))
    .update(evidence.traceStateHashHex)
    .update(evidence.workRootHex)
    .digest("hex");
