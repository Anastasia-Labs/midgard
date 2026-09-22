import {
  buildMidgardBoundedItem,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  type MidgardValidationMerkleMembership,
  verifyMidgardValidationMerkleMembership,
} from "@al-ft/midgard-core";
import {
  encodeVerdictSubject,
  hashHexWithBlake2b,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  type RejectionReason,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

export const MISSING_SCRIPT_SOURCE_CATEGORY = "missingScriptSource" as const;
export const MISSING_SCRIPT_SOURCE_ID = "0000002d" as const;
export const MissingScriptSourceViolationIds = Object.freeze({
  Missing: "script-source-missing",
} as const);

export const missingScriptSourceViolationId = (_resultClass?: number) =>
  MissingScriptSourceViolationIds.Missing;

export const MissingScriptSourceResultClasses = Object.freeze({
  Pending: -1,
  Present: -2,
  NoFault: -2,
  Missing: 0,
} as const);
export type MissingScriptSourceResultClass =
  (typeof MissingScriptSourceResultClasses)[keyof typeof MissingScriptSourceResultClasses];

const fail = (message: string): never => {
  throw new Error(`${MISSING_SCRIPT_SOURCE_CATEGORY}: ${message}`);
};
const exactHex = (value: string, bytes: number, label: string): string => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value))
    return fail(`${label} must be canonical ${bytes.toString()}-byte hex`);
  return value;
};
const exactIndex = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} must be a non-negative safe integer`);
  return value;
};

const accusedClassV1 = (
  reason: RejectionReason,
  purposeKind: number,
  purposeIndex: number,
): MissingScriptSourceResultClass => {
  if (typeof reason === "string") return fail("typed reason is outside family");
  if (!("ScriptSourceMissing" in reason))
    return fail("typed reason is outside missing-script-source");
  const payload = reason.ScriptSourceMissing;
  if (
    payload.purpose_kind !== BigInt(purposeKind) ||
    payload.purpose_index !== BigInt(purposeIndex)
  )
    return fail("typed reason purpose coordinate differs");
  return MissingScriptSourceResultClasses.Missing;
};

export type MissingScriptSourceFinding = Readonly<{
  subject: VerdictSubject;
  purposeKind?: 0 | 1 | 2 | 3;
  purposeIndex?: number;
  /** Absolute consensus purpose-frontier index. */
  executionIndex: number;
}>;

export const classifyMissingScriptSourceFinding = (
  finding: MissingScriptSourceFinding,
): MissingScriptSourceFinding & {
  readonly purposeKind: 0 | 1 | 2 | 3;
  readonly purposeIndex: number;
  readonly accusedClass: MissingScriptSourceResultClass;
} => {
  if (!verdictSubjectIsCanonical(finding.subject))
    return fail("verdict subject is not canonical");
  const reasonPayload =
    finding.subject.rejection_reason !== null &&
    typeof finding.subject.rejection_reason !== "string" &&
    "ScriptSourceMissing" in finding.subject.rejection_reason
      ? finding.subject.rejection_reason.ScriptSourceMissing
      : undefined;
  const purposeKind =
    finding.purposeKind ??
    (reasonPayload === undefined ? 0 : Number(reasonPayload.purpose_kind));
  const purposeIndex =
    finding.purposeIndex ??
    (reasonPayload === undefined
      ? finding.executionIndex
      : Number(reasonPayload.purpose_index));
  if (![0, 1, 2, 3].includes(purposeKind))
    return fail("purpose kind must be spend, mint, observe, or receive");
  exactIndex(purposeIndex, "purpose index");
  exactIndex(finding.executionIndex, "execution index");
  const accusedClass =
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION
      ? finding.subject.rejection_reason === null
        ? fail("wrongful rejection carries no typed reason")
        : accusedClassV1(
            finding.subject.rejection_reason,
            purposeKind,
            purposeIndex,
          )
      : finding.subject.direction ===
            PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE &&
          finding.subject.rejection_reason === null
        ? MissingScriptSourceResultClasses.Pending
        : fail("direction/reason polarity is invalid");
  return Object.freeze({
    ...finding,
    purposeKind: purposeKind as 0 | 1 | 2 | 3,
    purposeIndex,
    accusedClass,
  });
};

export type ExecutionSourceDescriptor = Readonly<{
  sourceIndex: number;
  originKind: 0 | 1;
  sourceKeyHex: string;
  languageTag: 0 | 3 | 128;
  scriptHashHex: string;
  scriptItemHex: string;
  scriptTotalLength?: number;
  scriptItemCommitmentHex?: string;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
  purposeSubjectHex: string;
  redeemerLeafHex: "";
  purposeMembership: MidgardValidationMerkleMembership;
  sourceMembership: MidgardValidationMerkleMembership;
  executionMembership: MidgardValidationMerkleMembership;
}>;

export type MissingScriptSourceEvidence = Readonly<{
  finding: ReturnType<typeof classifyMissingScriptSourceFinding>;
  descriptor: ExecutionSourceDescriptor;
  itemCommitmentHex: string;
  itemLength: number;
  sourceLeafHex: string;
  purposeLeafHex: string;
  executionLeafHex: string;
  resultClass: MissingScriptSourceResultClass;
  initialControlCbor: string;
  chunkProofCount: number;
  sourceCount: number;
  foundAtSourceIndex: number | null;
  /** Complete consensus-ordered witness, resolved-spend, resolved-reference frontier. */
  sources: readonly ExecutionSourceDescriptor[];
}>;

export const prepareMissingScriptSourceEvidence = ({
  finding: rawFinding,
  descriptor,
  sources = [descriptor],
}: {
  readonly finding: MissingScriptSourceFinding;
  readonly descriptor: ExecutionSourceDescriptor;
  readonly sources?: readonly ExecutionSourceDescriptor[];
}): MissingScriptSourceEvidence => {
  const finding = classifyMissingScriptSourceFinding(rawFinding);
  exactIndex(descriptor.sourceIndex, "source index");
  const item = Buffer.from(descriptor.scriptItemHex, "hex");
  if (item.toString("hex") !== descriptor.scriptItemHex)
    return fail("script item is not canonical hex");
  const scriptHash = Buffer.from(
    exactHex(descriptor.scriptHashHex, 28, "script hash"),
    "hex",
  );
  const sourceKey = Buffer.from(descriptor.sourceKeyHex, "hex");
  const itemIndex =
    descriptor.originKind === 0
      ? descriptor.sourceIndex
      : sourceKey.length === 38
        ? sourceKey.readUInt16BE(36)
        : fail("reference source key is not a canonical out-ref");
  const bounded =
    item.length === 0
      ? null
      : buildMidgardBoundedItem({
          fieldIndex: descriptor.originKind === 0 ? 6 : 2,
          itemIndex,
          bytes: item,
        });
  const itemLength = descriptor.scriptTotalLength ?? item.length;
  const itemCommitmentHex =
    descriptor.scriptItemCommitmentHex ??
    bounded?.commitment.toString("hex") ??
    fail("source descriptor omitted item commitment");
  exactIndex(itemLength, "script total length");
  exactHex(itemCommitmentHex, 32, "script item commitment");
  const purposeLeaf = hashMidgardScriptPurposeLeaf({
    purposeKind: finding.purposeKind,
    purposeIndex: BigInt(finding.purposeIndex),
    scriptHash,
    subject: Buffer.from(descriptor.purposeSubjectHex, "hex"),
  });
  const sourceLeaf =
    descriptor.originKind === 0
      ? hashMidgardInlineScriptSourceLeaf({
          sourceIndex: BigInt(descriptor.sourceIndex),
          scriptLanguageTag: 0,
          scriptHash,
          scriptTotalLength: itemLength,
          itemCommitment: Buffer.from(itemCommitmentHex, "hex"),
        })
      : hashMidgardReferenceScriptSourceLeaf({
          sourceKey,
          scriptLanguageTag: 0,
          scriptHash,
          scriptTotalLength: itemLength,
          itemCommitment: Buffer.from(itemCommitmentHex, "hex"),
        });
  const executionLeaf = hashMidgardScriptExecutionLeaf({
    languageTag: 0,
    purposeLeaf,
    sourceLeaf,
    redeemerLeaf: Buffer.alloc(0),
  });
  const exactMemberships = [
    [
      descriptor.purposeMembership,
      finding.executionIndex,
      purposeLeaf,
      "purpose",
    ],
  ] as const;
  for (const [membership, index, leaf, label] of exactMemberships) {
    if (
      membership.leafIndex !== index ||
      !Buffer.from(membership.leafHash).equals(leaf) ||
      !verifyMidgardValidationMerkleMembership(membership)
    )
      return fail(`${label} frontier membership was substituted`);
  }
  if (
    descriptor.purposeKind !== finding.purposeKind ||
    descriptor.purposeIndex !== finding.purposeIndex
  )
    return fail("authenticated purpose coordinate was substituted");
  if (sources.length === 0 && descriptor.sourceMembership.leafIndex !== 0)
    return fail(
      "empty source frontier carries a nonzero membership coordinate",
    );
  let foundAtSourceIndex: number | null = null;
  for (const [sourceIndex, source] of sources.entries()) {
    exactIndex(source.sourceIndex, "source frontier index");
    if (source.sourceIndex !== sourceIndex)
      return fail("source frontier is not complete and consensus ordered");
    if (![0, 1].includes(source.originKind))
      return fail("source location is outside witness/resolved frontiers");
    exactHex(source.scriptHashHex, 28, "source script hash");
    const sourceItem = Buffer.from(source.scriptItemHex, "hex");
    if (sourceItem.toString("hex") !== source.scriptItemHex)
      return fail("source item is not canonical hex");
    const sourceBounded =
      sourceItem.length === 0
        ? null
        : buildMidgardBoundedItem({
            fieldIndex: source.originKind === 0 ? 6 : 2,
            itemIndex:
              source.originKind === 0
                ? source.sourceIndex
                : Buffer.from(source.sourceKeyHex, "hex").readUInt16BE(36),
            bytes: sourceItem,
          });
    const sourceLength = source.scriptTotalLength ?? sourceItem.length;
    const sourceCommitmentHex =
      source.scriptItemCommitmentHex ??
      sourceBounded?.commitment.toString("hex") ??
      fail("source descriptor omitted item commitment");
    const sourceHash = Buffer.from(source.scriptHashHex, "hex");
    const authenticatedLeaf =
      source.originKind === 0
        ? hashMidgardInlineScriptSourceLeaf({
            sourceIndex: BigInt(source.sourceIndex),
            scriptLanguageTag: source.languageTag,
            scriptHash: sourceHash,
            scriptTotalLength: sourceLength,
            itemCommitment: Buffer.from(sourceCommitmentHex, "hex"),
          })
        : hashMidgardReferenceScriptSourceLeaf({
            sourceKey: Buffer.from(source.sourceKeyHex, "hex"),
            scriptLanguageTag: source.languageTag,
            scriptHash: sourceHash,
            scriptTotalLength: sourceLength,
            itemCommitment: Buffer.from(sourceCommitmentHex, "hex"),
          });
    if (
      source.sourceMembership.leafIndex !== sourceIndex ||
      !Buffer.from(source.sourceMembership.leafHash).equals(
        authenticatedLeaf,
      ) ||
      !verifyMidgardValidationMerkleMembership(source.sourceMembership)
    )
      return fail("source frontier membership was substituted");
    if (
      source.scriptHashHex === descriptor.scriptHashHex &&
      foundAtSourceIndex === null
    )
      foundAtSourceIndex = sourceIndex;
  }
  const resultClass =
    foundAtSourceIndex === null
      ? MissingScriptSourceResultClasses.Missing
      : MissingScriptSourceResultClasses.Present;
  return Object.freeze({
    finding,
    descriptor,
    itemCommitmentHex,
    itemLength,
    sourceLeafHex: sourceLeaf.toString("hex"),
    purposeLeafHex: purposeLeaf.toString("hex"),
    executionLeafHex: executionLeaf.toString("hex"),
    resultClass,
    initialControlCbor: "",
    chunkProofCount: bounded?.chunkHashes.length ?? 0,
    sourceCount: sources.length,
    foundAtSourceIndex,
    sources: Object.freeze(
      sources.map((source) => Object.freeze({ ...source })),
    ),
  });
};

export const missingScriptSourceEvidenceCloses = (
  evidence: MissingScriptSourceEvidence,
): boolean =>
  evidence.finding.subject.direction ===
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
    ? evidence.resultClass === MissingScriptSourceResultClasses.Missing
    : evidence.resultClass === MissingScriptSourceResultClasses.Present;

const cborHead = (major: number, value: number): Buffer => {
  if (value < 24) return Buffer.from([(major << 5) | value]);
  if (value <= 0xff) return Buffer.from([(major << 5) | 24, value]);
  if (value <= 0xffff) {
    const result = Buffer.alloc(3);
    result[0] = (major << 5) | 25;
    result.writeUInt16BE(value, 1);
    return result;
  }
  const result = Buffer.alloc(5);
  result[0] = (major << 5) | 26;
  result.writeUInt32BE(value, 1);
  return result;
};
const cborBytes = (value: Buffer): Buffer =>
  Buffer.concat([cborHead(2, value.length), value]);

export const missingScriptSourceCheckpoint = ({
  evidence,
  controlCbor,
  nextExpectedScriptHash,
}: {
  readonly evidence: MissingScriptSourceEvidence;
  readonly controlCbor: string;
  readonly nextExpectedScriptHash: string;
}): string => {
  const bytes = Buffer.concat([
    Buffer.from(
      "midgard/fraud-proofs/missing-script-source/checkpoint-v1",
      "ascii",
    ),
    encodeVerdictSubject(evidence.finding.subject),
    cborHead(0, evidence.finding.executionIndex),
    cborHead(0, evidence.descriptor.sourceIndex),
    cborHead(0, evidence.descriptor.originKind),
    cborBytes(Buffer.from(evidence.descriptor.sourceKeyHex, "hex")),
    cborHead(0, evidence.itemLength),
    cborBytes(Buffer.from(evidence.itemCommitmentHex, "hex")),
    cborBytes(Buffer.from(controlCbor, "hex")),
    cborBytes(
      Buffer.from(
        exactHex(nextExpectedScriptHash, 28, "next script hash"),
        "hex",
      ),
    ),
  ]);
  return Effect.runSync(hashHexWithBlake2b(bytes.toString("hex"), 32));
};

export const MISSING_SCRIPT_SOURCE_STAGES = [
  "none",
  "step01",
  "step02",
  "step03",
  "scan",
  "proven",
  "removed",
  "cancelled",
] as const;
export type MissingScriptSourceStage =
  (typeof MISSING_SCRIPT_SOURCE_STAGES)[number];

export const nextMissingScriptSourceAction = (
  stage: MissingScriptSourceStage,
) =>
  (
    ({
      none: "submitInit",
      step01: "submitStep01",
      step02: "submitStep02",
      step03: "submitStep03",
      scan: "submitScanOrResume",
      proven: "removeDescendants",
      removed: "done",
      cancelled: "done",
    }) as const
  )[stage];

export const missingScriptSourceEvidenceIdentity = (
  evidence: MissingScriptSourceEvidence,
): string =>
  [
    evidence.finding.subject.transaction_id,
    evidence.finding.subject.direction.toString(),
    evidence.finding.executionIndex.toString(),
    evidence.finding.purposeKind.toString(),
    evidence.finding.purposeIndex.toString(),
    evidence.executionLeafHex,
    evidence.itemCommitmentHex,
  ].join(":");
