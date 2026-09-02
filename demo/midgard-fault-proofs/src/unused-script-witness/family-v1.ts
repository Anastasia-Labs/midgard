import { createHash } from "node:crypto";

import {
  buildMidgardBoundedItemV1,
  decodeMidgardScriptWitnessFieldPreimageV1,
  encodeMidgardVersionedScript,
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardReferenceScriptSourceLeafV1,
  hashMidgardScriptPurposeLeafV1,
  hashMidgardVersionedScript,
  type MidgardValidationMerkleMembershipV1,
  verifyMidgardValidationMerkleMembershipV1,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";

export const UNUSED_SCRIPT_WITNESS_CATEGORY_V1 = "unusedScriptWitness" as const;
export const UNUSED_SCRIPT_WITNESS_CATEGORY_ID_V1 = "0000002f" as const;
export const UNUSED_SCRIPT_WITNESS_VIOLATION_ID_V1 =
  "unused-script-witness" as const;
export const UNUSED_SCRIPT_WITNESS_FIELD_INDEX_V1 = 6 as const;

const fail = (message: string): never => {
  throw new Error(`${UNUSED_SCRIPT_WITNESS_CATEGORY_V1}: ${message}`);
};
const index = (value: number, label: string): number =>
  Number.isSafeInteger(value) && value >= 0
    ? value
    : fail(`${label} is not a non-negative safe integer`);
const hex = (value: string, bytes: number, label: string): string =>
  new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value)
    ? value
    : fail(`${label} is not canonical ${bytes.toString()}-byte hex`);

export type UnusedScriptWitnessFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  scriptIndex: number;
}>;

export const classifyUnusedScriptWitnessFindingV1 = (
  finding: UnusedScriptWitnessFindingV1,
): UnusedScriptWitnessFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    fail("verdict subject is not canonical");
  index(finding.scriptIndex, "script index");
  if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    const reason = finding.subject.rejection_reason;
    if (
      reason === null ||
      typeof reason === "string" ||
      !("UnusedScriptWitness" in reason) ||
      reason.UnusedScriptWitness.script_index !== BigInt(finding.scriptIndex)
    )
      fail("typed rejection reason or script coordinate changed");
  } else if (
    finding.subject.direction !==
      PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    finding.subject.rejection_reason !== null
  ) {
    fail("direction/reason polarity changed");
  }
  return Object.freeze(finding);
};

export type UnusedScriptSourceOpeningV1 = Readonly<{
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
export type UnusedScriptPurposeOpeningV1 = Readonly<{
  frontierIndex: number;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
  scriptHashHex: string;
  purposeSubjectHex: string;
  membership: MidgardValidationMerkleMembershipV1;
}>;
/**
 * Consumer-side shape for a fully header/trace/work-root-bound terminal ScriptSources
 * universe. Production values must come from retained-DA authentication; the
 * pure family rule never admits a free-standing frontier.
 */
export type AuthenticatedCommittedScriptUniverseV1 = Readonly<{
  schemaVersion: "midgard-committed-script-universe-v1";
  transactionId: string;
  universeDigest: string;
  sources: readonly UnusedScriptSourceOpeningV1[];
  purposes: readonly UnusedScriptPurposeOpeningV1[];
}>;
export type UnusedScriptWitnessEvidenceV1 = Readonly<{
  finding: UnusedScriptWitnessFindingV1;
  fieldPreimageHex: string;
  targetItemHex: string;
  targetScriptHashHex: string;
  targetItemCommitmentHex: string;
  targetSourceLeafHex: string;
  sources: readonly UnusedScriptSourceOpeningV1[];
  purposes: readonly UnusedScriptPurposeOpeningV1[];
  firstMatchingSourceIndex: number;
  matchedPurposeIndex: number | null;
  unused: boolean;
  checkpointDigest: string;
}>;

const verifyCompleteFrontier = <
  T extends {
    readonly frontierIndex: number;
    readonly membership: MidgardValidationMerkleMembershipV1;
  },
>(
  items: readonly T[],
  label: string,
  complete = true,
): void => {
  const frontierCount = items[0]?.membership.frontier.count ?? 0;
  if (
    (complete && frontierCount !== items.length) ||
    (!complete && frontierCount < items.length)
  )
    fail(`${label} frontier cardinality is inconsistent`);
  items.forEach((item, frontierIndex) => {
    if (
      item.frontierIndex !== frontierIndex ||
      item.membership.leafIndex !== frontierIndex ||
      item.membership.frontier.count !== frontierCount ||
      !verifyMidgardValidationMerkleMembershipV1(item.membership)
    )
      fail(`${label} frontier is incomplete, reordered, or unauthenticated`);
  });
};

export const prepareUnusedScriptWitnessEvidenceV1 = ({
  finding: rawFinding,
  fieldPreimage,
  universe,
}: {
  readonly finding: UnusedScriptWitnessFindingV1;
  readonly fieldPreimage: Uint8Array;
  readonly universe: AuthenticatedCommittedScriptUniverseV1;
}): UnusedScriptWitnessEvidenceV1 => {
  const finding = classifyUnusedScriptWitnessFindingV1(rawFinding);
  if (
    universe.schemaVersion !== "midgard-committed-script-universe-v1" ||
    universe.transactionId !== finding.subject.transaction_id ||
    !/^[0-9a-f]{64}$/u.test(universe.universeDigest)
  )
    fail("authenticated committed script universe identity changed");
  const { sources, purposes } = universe;
  const scripts = decodeMidgardScriptWitnessFieldPreimageV1(fieldPreimage);
  const target = scripts[finding.scriptIndex];
  if (target === undefined) fail("script coordinate is outside field 6");
  verifyCompleteFrontier(sources, "source", false);
  verifyCompleteFrontier(purposes, "purpose");
  const sourceFrontierCount = sources[0]?.membership.frontier.count ?? 0;
  if (
    sources.length !==
    (finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
      ? finding.scriptIndex + 1
      : sourceFrontierCount)
  )
    fail("source frontier prefix does not match the direction seam");
  const item = encodeMidgardVersionedScript(target);
  const targetHash = hashMidgardVersionedScript(target);
  const bounded = buildMidgardBoundedItemV1({
    fieldIndex: UNUSED_SCRIPT_WITNESS_FIELD_INDEX_V1,
    itemIndex: finding.scriptIndex,
    bytes: item,
  });
  const languageTag =
    target.language === "NativeCardano"
      ? 0
      : target.language === "PlutusV3"
        ? 3
        : 128;
  const targetLeaf = hashMidgardInlineScriptSourceLeafV1({
    sourceIndex: BigInt(finding.scriptIndex),
    scriptLanguageTag: languageTag,
    scriptHash: Buffer.from(targetHash, "hex"),
    scriptTotalLength: item.length,
    itemCommitment: bounded.commitment,
  });
  const accused = sources.find(
    (source) =>
      source.originKind === 0 && source.sourceIndex === finding.scriptIndex,
  );
  if (
    accused === undefined ||
    accused.scriptHashHex !== targetHash ||
    !Buffer.from(accused.membership.leafHash).equals(targetLeaf)
  )
    fail("accused inline source item or membership was substituted");
  let inlineCount = 0;
  let priorReferenceKey = "";
  let referencesStarted = false;
  sources.forEach((source) => {
    index(source.sourceIndex, "source index");
    hex(source.scriptHashHex, 28, "source script hash");
    hex(source.itemCommitmentHex, 32, "source item commitment");
    index(source.scriptTotalLength, "source item length");
    if (source.scriptTotalLength === 0) fail("source item length is zero");
    const leaf =
      source.originKind === 0
        ? (() => {
            if (referencesStarted || source.sourceIndex !== inlineCount)
              return fail(
                "inline sources are not the complete canonical prefix",
              );
            inlineCount += 1;
            if (source.sourceKeyHex !== "")
              return fail("inline source carries a reference key");
            return hashMidgardInlineScriptSourceLeafV1({
              sourceIndex: BigInt(source.sourceIndex),
              scriptLanguageTag: source.languageTag,
              scriptHash: Buffer.from(source.scriptHashHex, "hex"),
              scriptTotalLength: source.scriptTotalLength,
              itemCommitment: Buffer.from(source.itemCommitmentHex, "hex"),
            });
          })()
        : (() => {
            referencesStarted = true;
            if (
              !/^[0-9a-f]{76}$/u.test(source.sourceKeyHex) ||
              source.sourceKeyHex <= priorReferenceKey
            )
              return fail("reference sources are not canonical and ordered");
            priorReferenceKey = source.sourceKeyHex;
            return hashMidgardReferenceScriptSourceLeafV1({
              sourceKey: Buffer.from(source.sourceKeyHex, "hex"),
              scriptLanguageTag: source.languageTag,
              scriptHash: Buffer.from(source.scriptHashHex, "hex"),
              scriptTotalLength: source.scriptTotalLength,
              itemCommitment: Buffer.from(source.itemCommitmentHex, "hex"),
            });
          })();
    if (!Buffer.from(source.membership.leafHash).equals(leaf))
      fail("source descriptor differs from authenticated leaf");
  });
  purposes.forEach((purpose) => {
    index(purpose.purposeIndex, "purpose index");
    if (![0, 1, 2, 3].includes(purpose.purposeKind))
      fail("purpose kind is outside the complete Midgard frontier");
    hex(purpose.scriptHashHex, 28, "purpose script hash");
    const leaf = hashMidgardScriptPurposeLeafV1({
      purposeKind: purpose.purposeKind,
      purposeIndex: BigInt(purpose.purposeIndex),
      scriptHash: Buffer.from(purpose.scriptHashHex, "hex"),
      subject: Buffer.from(purpose.purposeSubjectHex, "hex"),
    });
    if (!Buffer.from(purpose.membership.leafHash).equals(leaf))
      fail("purpose descriptor differs from authenticated leaf");
  });
  const firstMatchingSource =
    sources.find((source) => source.scriptHashHex === targetHash) ??
    fail("authenticated source frontier omitted the accused script hash");
  const selected =
    firstMatchingSource.originKind === 0 &&
    firstMatchingSource.sourceIndex === finding.scriptIndex;
  const matchedPurpose = selected
    ? purposes.find((purpose) => purpose.scriptHashHex === targetHash)
    : undefined;
  const unused = !selected || matchedPurpose === undefined;
  const checkpointDigest = createHash("sha256")
    .update(UNUSED_SCRIPT_WITNESS_CATEGORY_ID_V1)
    .update(finding.subject.transaction_id)
    .update(finding.scriptIndex.toString())
    .update(targetHash)
    .update(sources.map(({ scriptHashHex }) => scriptHashHex).join(""))
    .update(
      purposes
        .map(
          ({ purposeKind, purposeIndex, scriptHashHex }) =>
            `${purposeKind.toString()}:${purposeIndex.toString()}:${scriptHashHex}`,
        )
        .join("|"),
    )
    .digest("hex");
  return Object.freeze({
    finding,
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    targetItemHex: item.toString("hex"),
    targetScriptHashHex: targetHash,
    targetItemCommitmentHex: bounded.commitment.toString("hex"),
    targetSourceLeafHex: targetLeaf.toString("hex"),
    sources: Object.freeze([...sources]),
    purposes: Object.freeze([...purposes]),
    firstMatchingSourceIndex: firstMatchingSource.frontierIndex,
    matchedPurposeIndex: matchedPurpose?.frontierIndex ?? null,
    unused,
    checkpointDigest,
  });
};

export const unusedScriptWitnessEvidenceClosesV1 = (
  evidence: UnusedScriptWitnessEvidenceV1,
): boolean =>
  evidence.finding.subject.direction ===
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
    ? evidence.unused
    : !evidence.unused;

/** Explicit optimistic-accountability disjunction used by replay tests. */
export const unusedScriptWitnessAccountabilityRouteV1 = ({
  evidence,
  committedUniverseDigest,
  canonicalUniverseDigest,
}: {
  readonly evidence: UnusedScriptWitnessEvidenceV1;
  readonly committedUniverseDigest: string;
  readonly canonicalUniverseDigest: string;
}): "direct" | "traceInvalid" | "none" => {
  hex(committedUniverseDigest, 32, "committed universe digest");
  hex(canonicalUniverseDigest, 32, "canonical universe digest");
  if (committedUniverseDigest !== canonicalUniverseDigest)
    return "traceInvalid";
  return unusedScriptWitnessEvidenceClosesV1(evidence) ? "direct" : "none";
};

export const unusedScriptWitnessEvidenceIdentityV1 = (
  evidence: UnusedScriptWitnessEvidenceV1,
): string =>
  createHash("sha256")
    .update(UNUSED_SCRIPT_WITNESS_CATEGORY_ID_V1)
    .update(evidence.checkpointDigest)
    .digest("hex");
