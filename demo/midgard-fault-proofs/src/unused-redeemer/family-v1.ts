import { createHash } from "node:crypto";

import {
  buildMidgardBoundedItemV1,
  decodeMidgardRedeemerWitnessFieldPreimageV1,
  encodeMidgardRedeemerWitnessItemV1,
  hashMidgardRedeemerItemLeafV1,
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
  MIDGARD_REDEEMER_PURPOSE_TAGS_V1,
  type MidgardValidationMerkleMembershipV1,
  verifyMidgardValidationMerkleMembershipV1,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 as ACCEPTED,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1 as REJECTED,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";

export const UNUSED_REDEEMER_CATEGORY_V1 = "unusedRedeemer" as const;
export const UNUSED_REDEEMER_CATEGORY_ID_V1 = "00000030" as const;
export const UNUSED_REDEEMER_VIOLATION_ID_V1 = "unused-redeemer" as const;
export const UNUSED_REDEEMER_FIELD_INDEX_V1 = 8 as const;
const fail = (m: string): never => {
  throw new Error(`unusedRedeemer: ${m}`);
};
const index = (n: number, label: string): number =>
  Number.isSafeInteger(n) && n >= 0 ? n : fail(`${label} is invalid`);
const hex = (s: string, bytes: number, label: string): string =>
  new RegExp(`^[0-9a-f]{${String(bytes * 2)}}$`, "u").test(s)
    ? s
    : fail(`${label} is invalid`);

export type UnusedRedeemerFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  redeemerIndex: number;
}>;
export const classifyUnusedRedeemerFindingV1 = (
  finding: UnusedRedeemerFindingV1,
): UnusedRedeemerFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    fail("verdict subject is not canonical");
  index(finding.redeemerIndex, "redeemer index");
  if (finding.subject.direction === REJECTED) {
    const reason = finding.subject.rejection_reason;
    if (
      reason === null ||
      typeof reason === "string" ||
      !("UnusedRedeemer" in reason) ||
      reason.UnusedRedeemer.redeemer_index !== BigInt(finding.redeemerIndex)
    )
      fail("typed reason or coordinate changed");
  } else if (
    finding.subject.direction !== ACCEPTED ||
    finding.subject.rejection_reason !== null
  )
    fail("direction/reason polarity changed");
  return Object.freeze(finding);
};

export type UnusedRedeemerSelectionOpeningV1 = Readonly<{
  frontierIndex: number;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
  scriptHashHex: string;
  purposeSubjectHex: string;
  purposeMembership: MidgardValidationMerkleMembershipV1;
  languageTag: 0 | 3 | 128;
  sourceLeafHex: string;
  redeemerLeafHex: string;
  executionMembership: MidgardValidationMerkleMembershipV1;
}>;
// Compatibility carriage names consumed by the shared transaction builders;
// production replay replaces these with the exact retained stage-10/12 seam.
export type LegacyUnusedRedeemerSourceOpeningV1 = Readonly<{
  frontierIndex: number;
  originKind: 0 | 1;
  sourceIndex: number;
  sourceKeyHex: string;
  languageTag: 0 | 3 | 128;
  scriptHashHex: string;
  scriptTotalLength: number;
  itemCommitmentHex: string;
  membership: MidgardValidationMerkleMembershipV1;
}>;
export type LegacyUnusedRedeemerPurposeOpeningV1 = Readonly<{
  frontierIndex: number;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
  scriptHashHex: string;
  purposeSubjectHex: string;
  membership: MidgardValidationMerkleMembershipV1;
}>;
export type AuthenticatedCommittedRedeemerUniverseV1 =
  | Readonly<{
      schemaVersion: "midgard-committed-redeemer-universe-v1";
      transactionId: string;
      universeDigest: string;
      selections: readonly UnusedRedeemerSelectionOpeningV1[];
    }>
  | Readonly<{
      schemaVersion: "midgard-committed-script-universe-v1";
      transactionId: string;
      universeDigest: string;
      sources: readonly LegacyUnusedRedeemerSourceOpeningV1[];
      purposes: readonly LegacyUnusedRedeemerPurposeOpeningV1[];
    }>;
export type UnusedRedeemerEvidenceV1 = Readonly<{
  finding: UnusedRedeemerFindingV1;
  fieldPreimageHex: string;
  targetItemHex: string;
  targetPurposeTag: 0 | 1 | 3 | 6;
  targetPointerIndex: number;
  targetItemCommitmentHex: string;
  targetRedeemerLeafHex: string;
  selections: readonly UnusedRedeemerSelectionOpeningV1[];
  sources: readonly LegacyUnusedRedeemerSourceOpeningV1[];
  purposes: readonly LegacyUnusedRedeemerPurposeOpeningV1[];
  targetScriptHashHex: string;
  matchedSelectionIndex: number | null;
  unused: boolean;
  checkpointDigest: string;
}>;
const tagForKind = (kind: 0 | 1 | 2 | 3): 0 | 1 | 3 | 6 =>
  ([0, 1, 3, 6] as const)[kind];

const verifySelections = (
  items: readonly UnusedRedeemerSelectionOpeningV1[],
): void => {
  const count = items[0]?.purposeMembership.frontier.count ?? 0;
  if (count !== items.length) fail("purpose frontier is incomplete");
  items.forEach((item, frontierIndex) => {
    index(item.purposeIndex, "purpose index");
    hex(item.scriptHashHex, 28, "script hash");
    hex(item.sourceLeafHex, 32, "source leaf");
    if (item.redeemerLeafHex !== "")
      hex(item.redeemerLeafHex, 32, "redeemer leaf");
    const purposeLeaf = hashMidgardScriptPurposeLeafV1({
      purposeKind: item.purposeKind,
      purposeIndex: BigInt(item.purposeIndex),
      scriptHash: Buffer.from(item.scriptHashHex, "hex"),
      subject: Buffer.from(item.purposeSubjectHex, "hex"),
    });
    const executionLeaf = hashMidgardScriptExecutionLeafV1({
      languageTag: item.languageTag,
      purposeLeaf,
      sourceLeaf: Buffer.from(item.sourceLeafHex, "hex"),
      redeemerLeaf: Buffer.from(item.redeemerLeafHex, "hex"),
    });
    if (
      item.frontierIndex !== frontierIndex ||
      item.purposeMembership.leafIndex !== frontierIndex ||
      item.executionMembership.leafIndex !== frontierIndex ||
      item.purposeMembership.frontier.count !== count ||
      item.executionMembership.frontier.count !== count ||
      !Buffer.from(item.purposeMembership.leafHash).equals(purposeLeaf) ||
      !Buffer.from(item.executionMembership.leafHash).equals(executionLeaf) ||
      !verifyMidgardValidationMerkleMembershipV1(item.purposeMembership) ||
      !verifyMidgardValidationMerkleMembershipV1(item.executionMembership)
    )
      fail("purpose/execution frontier changed");
  });
};

export const prepareUnusedRedeemerEvidenceV1 = ({
  finding: raw,
  fieldPreimage,
  universe,
}: {
  finding: UnusedRedeemerFindingV1;
  fieldPreimage: Uint8Array;
  universe: AuthenticatedCommittedRedeemerUniverseV1;
}): UnusedRedeemerEvidenceV1 => {
  const finding = classifyUnusedRedeemerFindingV1(raw);
  if (
    universe.transactionId !== finding.subject.transaction_id ||
    !/^[0-9a-f]{64}$/u.test(universe.universeDigest)
  )
    fail("committed universe identity changed");
  const selections =
    "selections" in universe
      ? universe.selections
      : fail("retained replay omitted authenticated execution selections");
  verifySelections(selections);
  const target =
    decodeMidgardRedeemerWitnessFieldPreimageV1(fieldPreimage)[
      finding.redeemerIndex
    ];
  if (target === undefined) fail("redeemer coordinate is outside field 8");
  const decodedPurposeTag = MIDGARD_REDEEMER_PURPOSE_TAGS_V1[target.purpose];
  if (
    decodedPurposeTag !== 0 &&
    decodedPurposeTag !== 1 &&
    decodedPurposeTag !== 3 &&
    decodedPurposeTag !== 6
  )
    fail("unsupported redeemer purpose");
  const purposeTag = decodedPurposeTag as 0 | 1 | 3 | 6;
  const targetItem = encodeMidgardRedeemerWitnessItemV1(target);
  const bounded = buildMidgardBoundedItemV1({
    fieldIndex: 8,
    itemIndex: finding.redeemerIndex,
    bytes: targetItem,
  });
  const targetLeaf = hashMidgardRedeemerItemLeafV1({
    redeemerIndex: finding.redeemerIndex,
    itemCommitment: bounded.commitment,
  }).toString("hex");
  const pointer = index(Number(target.index), "pointer index");
  const matches = selections.filter(
    (s) =>
      tagForKind(s.purposeKind) === purposeTag &&
      s.purposeIndex === pointer &&
      s.redeemerLeafHex === targetLeaf,
  );
  if (matches.length > 1) fail("ambiguous execution selection");
  const unused = matches.length === 0;
  if (unused !== (finding.subject.direction === ACCEPTED))
    fail("selection frontier contradicts proof direction");
  const checkpointDigest = createHash("sha256")
    .update("MidgardUnusedRedeemerEvidenceV1\0")
    .update(finding.subject.transaction_id, "hex")
    .update(String(finding.redeemerIndex))
    .update(targetLeaf, "hex")
    .update(universe.universeDigest, "hex")
    .digest("hex");
  const purposes = selections.map((s) => ({
    frontierIndex: s.frontierIndex,
    purposeKind: s.purposeKind,
    purposeIndex: s.purposeIndex,
    scriptHashHex: s.scriptHashHex,
    purposeSubjectHex: s.purposeSubjectHex,
    membership: s.purposeMembership,
  }));
  return Object.freeze({
    finding,
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    targetItemHex: Buffer.from(targetItem).toString("hex"),
    targetPurposeTag: purposeTag,
    targetPointerIndex: pointer,
    targetItemCommitmentHex: bounded.commitment.toString("hex"),
    targetRedeemerLeafHex: targetLeaf,
    targetScriptHashHex: selections[0]?.scriptHashHex ?? "00".repeat(28),
    sources: Object.freeze([]),
    purposes: Object.freeze(purposes),
    selections: Object.freeze([...selections]),
    matchedSelectionIndex: matches[0]?.frontierIndex ?? null,
    unused,
    checkpointDigest,
  });
};
export const unusedRedeemerEvidenceClosesV1 = (
  e: UnusedRedeemerEvidenceV1,
): boolean => e.unused === (e.finding.subject.direction === ACCEPTED);
export const unusedRedeemerAccountabilityRouteV1 = ({
  committedFrontierIsCanonical,
  evidence,
}: {
  committedFrontierIsCanonical: boolean;
  evidence: UnusedRedeemerEvidenceV1;
}): "unusedRedeemer" | "validationTraceInvalid" =>
  committedFrontierIsCanonical && unusedRedeemerEvidenceClosesV1(evidence)
    ? "unusedRedeemer"
    : "validationTraceInvalid";
export const unusedRedeemerEvidenceIdentityV1 = (
  e: UnusedRedeemerEvidenceV1,
): string =>
  createHash("sha256")
    .update("MidgardUnusedRedeemerIdentityV1\0")
    .update(e.finding.subject.transaction_id, "hex")
    .update(String(e.finding.redeemerIndex))
    .update(e.targetRedeemerLeafHex, "hex")
    .update(e.checkpointDigest, "hex")
    .digest("hex");
