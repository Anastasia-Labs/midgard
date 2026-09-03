import { createHash } from "node:crypto";

import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY =
  "distinctAssetAccumulationLimit" as const;
export const DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID =
  "00000035" as const;
export const MIDGARD_MAX_DISTINCT_ASSETS = 16_384 as const;

export type DistinctAssetAccumulationCoordinate =
  | Readonly<{ kind: "input"; inputIndex: number; assetIndex: number }>
  | Readonly<{ kind: "output"; outputIndex: number; assetIndex: number }>
  | Readonly<{ kind: "mint"; mintIndex: number }>;

export type DistinctAssetAccumulationFinding = Readonly<{
  subject: VerdictSubject;
  coordinate: DistinctAssetAccumulationCoordinate;
}>;

const fail = (message: string): never => {
  throw new Error(`${DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY}: ${message}`);
};
const index = (value: number, label: string): number =>
  Number.isSafeInteger(value) && value >= 0
    ? value
    : fail(`${label} is not a non-negative safe integer`);

const reasonMatches = (
  reason: VerdictSubject["rejection_reason"],
  coordinate: DistinctAssetAccumulationCoordinate,
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

export const classifyDistinctAssetAccumulationFinding = (
  finding: DistinctAssetAccumulationFinding,
): DistinctAssetAccumulationFinding => {
  if (!verdictSubjectIsCanonical(finding.subject))
    fail("verdict subject is not canonical");
  if (finding.coordinate.kind === "input") {
    index(finding.coordinate.inputIndex, "input index");
    index(finding.coordinate.assetIndex, "asset index");
  } else if (finding.coordinate.kind === "output") {
    index(finding.coordinate.outputIndex, "output index");
    index(finding.coordinate.assetIndex, "asset index");
  } else index(finding.coordinate.mintIndex, "mint index");
  if (finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    if (!reasonMatches(finding.subject.rejection_reason, finding.coordinate))
      fail("typed rejection reason or coordinate changed");
  } else if (
    finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    finding.subject.rejection_reason !== null
  )
    fail("direction/reason polarity changed");
  return Object.freeze(finding);
};

export type DistinctAssetAccumulatorCheckpoint = Readonly<{
  assetRootHex: string;
  seenAssetCount: number;
  nonzeroAssetCount: number;
  cursor: number;
}>;

export type DistinctAssetAccumulationEvidence = Readonly<{
  finding: DistinctAssetAccumulationFinding;
  traceStateHashHex: string;
  workRootHex: string;
  pre: DistinctAssetAccumulatorCheckpoint;
  post: DistinctAssetAccumulatorCheckpoint | null;
  mutationWasPresent: boolean;
}>;

const h32 = (value: string, label: string): string =>
  /^[0-9a-f]{64}$/u.test(value) ? value : fail(`${label} is not canonical h32`);

export const prepareDistinctAssetAccumulationEvidence = (
  evidence: DistinctAssetAccumulationEvidence,
): DistinctAssetAccumulationEvidence => {
  classifyDistinctAssetAccumulationFinding(evidence.finding);
  h32(evidence.traceStateHashHex, "trace state hash");
  h32(evidence.workRootHex, "work root");
  h32(evidence.pre.assetRootHex, "pre asset root");
  index(evidence.pre.seenAssetCount, "pre seen count");
  index(evidence.pre.nonzeroAssetCount, "pre nonzero count");
  index(evidence.pre.cursor, "pre cursor");
  if (evidence.pre.seenAssetCount > MIDGARD_MAX_DISTINCT_ASSETS)
    fail("pre accumulator already exceeds consensus limit");
  if (evidence.pre.nonzeroAssetCount > evidence.pre.seenAssetCount)
    fail("pre nonzero count exceeds seen count");
  const crossing =
    !evidence.mutationWasPresent &&
    evidence.pre.seenAssetCount === MIDGARD_MAX_DISTINCT_ASSETS;
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

export const distinctAssetAccumulationFaultHolds = (
  evidence: DistinctAssetAccumulationEvidence,
): boolean => prepareDistinctAssetAccumulationEvidence(evidence).post === null;

export const distinctAssetAccumulationEvidenceCloses = (
  evidence: DistinctAssetAccumulationEvidence,
): boolean =>
  evidence.finding.subject.direction ===
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
    ? distinctAssetAccumulationFaultHolds(evidence)
    : !distinctAssetAccumulationFaultHolds(evidence);

export const distinctAssetAccumulationEvidenceIdentity = (
  evidence: DistinctAssetAccumulationEvidence,
): string =>
  createHash("sha256")
    .update(DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID)
    .update(evidence.finding.subject.transaction_id)
    .update(JSON.stringify(evidence.finding.coordinate))
    .update(evidence.traceStateHashHex)
    .update(evidence.workRootHex)
    .digest("hex");
