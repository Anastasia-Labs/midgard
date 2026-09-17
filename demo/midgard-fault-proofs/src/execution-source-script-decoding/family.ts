import {
  buildMidgardBoundedItem,
  buildMidgardNativeScriptDecodingTrace,
  encodeMidgardNativeScriptStructureControl,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  MidgardNativeScriptDecodingBindKinds,
  MidgardNativeScriptDecodingRefusalClasses,
  MidgardNativeScriptDecodingTraceOutcomeKinds,
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

export const EXECUTION_SOURCE_SCRIPT_DECODING_CATEGORY =
  "executionSourceScriptDecoding" as const;
export const EXECUTION_SOURCE_SCRIPT_DECODING_ID = "00000031" as const;
export const ExecutionSourceScriptDecodingViolationIds = Object.freeze({
  Malformed: "execution-native-script-malformed",
  NodeLimit: "execution-native-script-node-limit",
  DepthLimit: "execution-native-script-depth-limit",
} as const);

export const executionSourceScriptDecodingViolationId = (
  resultClass: 0 | 1 | 2,
) =>
  [
    ExecutionSourceScriptDecodingViolationIds.Malformed,
    ExecutionSourceScriptDecodingViolationIds.NodeLimit,
    ExecutionSourceScriptDecodingViolationIds.DepthLimit,
  ][resultClass];

export const ExecutionSourceScriptDecodingResultClasses = Object.freeze({
  Pending: -1,
  NoFault: -2,
  Malformed: 0,
  NodeLimit: 1,
  DepthLimit: 2,
} as const);
export type ExecutionSourceScriptDecodingResultClass =
  (typeof ExecutionSourceScriptDecodingResultClasses)[keyof typeof ExecutionSourceScriptDecodingResultClasses];

const fail = (message: string): never => {
  throw new Error(`${EXECUTION_SOURCE_SCRIPT_DECODING_CATEGORY}: ${message}`);
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
  executionIndex: number,
): ExecutionSourceScriptDecodingResultClass => {
  if (typeof reason === "string") return fail("typed reason is outside family");
  const classes = [
    ["ExecutionNativeScriptMalformed", 0],
    ["ExecutionNativeScriptNodeLimit", 1],
    ["ExecutionNativeScriptDepthLimit", 2],
  ] as const;
  for (const [constructor, resultClass] of classes) {
    if (constructor in reason) {
      const payload = (
        reason as unknown as Record<string, { execution_index: bigint }>
      )[constructor];
      if (
        payload === undefined ||
        payload.execution_index !== BigInt(executionIndex)
      )
        return fail("typed reason execution coordinate differs");
      return resultClass;
    }
  }
  return fail("typed reason is outside execution-source-script-decoding");
};

export type ExecutionSourceScriptDecodingFinding = Readonly<{
  subject: VerdictSubject;
  executionIndex: number;
}>;

export const classifyExecutionSourceScriptDecodingFinding = (
  finding: ExecutionSourceScriptDecodingFinding,
): ExecutionSourceScriptDecodingFinding & {
  readonly accusedClass: ExecutionSourceScriptDecodingResultClass;
} => {
  if (!verdictSubjectIsCanonical(finding.subject))
    return fail("verdict subject is not canonical");
  exactIndex(finding.executionIndex, "execution index");
  const accusedClass =
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION
      ? finding.subject.rejection_reason === null
        ? fail("wrongful rejection carries no typed reason")
        : accusedClassV1(
            finding.subject.rejection_reason,
            finding.executionIndex,
          )
      : finding.subject.direction ===
            PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE &&
          finding.subject.rejection_reason === null
        ? ExecutionSourceScriptDecodingResultClasses.Pending
        : fail("direction/reason polarity is invalid");
  return Object.freeze({ ...finding, accusedClass });
};

export type ExecutionSourceDescriptor = Readonly<{
  sourceIndex: number;
  originKind: 0 | 1;
  sourceKeyHex: string;
  languageTag: 0;
  scriptHashHex: string;
  scriptItemHex: string;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: number;
  purposeSubjectHex: string;
  redeemerLeafHex: "";
  purposeMembership: MidgardValidationMerkleMembership;
  sourceMembership: MidgardValidationMerkleMembership;
  executionMembership: MidgardValidationMerkleMembership;
}>;

export type ExecutionSourceScriptDecodingEvidence = Readonly<{
  finding: ReturnType<typeof classifyExecutionSourceScriptDecodingFinding>;
  descriptor: ExecutionSourceDescriptor;
  itemCommitmentHex: string;
  itemLength: number;
  sourceLeafHex: string;
  purposeLeafHex: string;
  executionLeafHex: string;
  resultClass: ExecutionSourceScriptDecodingResultClass;
  initialControlCbor: string;
  chunkProofCount: number;
}>;

const scanResult = (item: Uint8Array) => {
  const trace = buildMidgardNativeScriptDecodingTrace(item);
  if (trace.bind.kind === MidgardNativeScriptDecodingBindKinds.Malformed)
    return { resultClass: 0 as const, initialControlCbor: "" };
  if (trace.bind.kind === MidgardNativeScriptDecodingBindKinds.NonNative)
    return { resultClass: -2 as const, initialControlCbor: "" };
  if (trace.outcome === null) return fail("native trace has no outcome");
  const resultClass =
    trace.outcome.kind === MidgardNativeScriptDecodingTraceOutcomeKinds.Terminal
      ? -2
      : trace.outcome.refusalClass ===
          MidgardNativeScriptDecodingRefusalClasses.Malformed
        ? 0
        : trace.outcome.refusalClass ===
            MidgardNativeScriptDecodingRefusalClasses.NodeLimit
          ? 1
          : 2;
  return {
    resultClass: resultClass as ExecutionSourceScriptDecodingResultClass,
    initialControlCbor: encodeMidgardNativeScriptStructureControl(
      trace.bind.control,
    ).toString("hex"),
  };
};

export const prepareExecutionSourceScriptDecodingEvidence = ({
  finding: rawFinding,
  descriptor,
}: {
  readonly finding: ExecutionSourceScriptDecodingFinding;
  readonly descriptor: ExecutionSourceDescriptor;
}): ExecutionSourceScriptDecodingEvidence => {
  const finding = classifyExecutionSourceScriptDecodingFinding(rawFinding);
  exactIndex(descriptor.sourceIndex, "source index");
  const item = Buffer.from(descriptor.scriptItemHex, "hex");
  if (item.toString("hex") !== descriptor.scriptItemHex || item.length === 0)
    return fail("script item is not canonical non-empty hex");
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
  const bounded = buildMidgardBoundedItem({
    fieldIndex: descriptor.originKind === 0 ? 6 : 2,
    itemIndex,
    bytes: item,
  });
  const purposeLeaf = hashMidgardScriptPurposeLeaf({
    purposeKind: descriptor.purposeKind,
    purposeIndex: BigInt(descriptor.purposeIndex),
    scriptHash,
    subject: Buffer.from(descriptor.purposeSubjectHex, "hex"),
  });
  const sourceLeaf =
    descriptor.originKind === 0
      ? hashMidgardInlineScriptSourceLeaf({
          sourceIndex: BigInt(descriptor.sourceIndex),
          scriptLanguageTag: 0,
          scriptHash,
          scriptTotalLength: item.length,
          itemCommitment: bounded.commitment,
        })
      : hashMidgardReferenceScriptSourceLeaf({
          sourceKey,
          scriptLanguageTag: 0,
          scriptHash,
          scriptTotalLength: item.length,
          itemCommitment: bounded.commitment,
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
    [descriptor.sourceMembership, descriptor.sourceIndex, sourceLeaf, "source"],
    [
      descriptor.executionMembership,
      finding.executionIndex,
      executionLeaf,
      "execution",
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
  const scan = scanResult(item);
  return Object.freeze({
    finding,
    descriptor,
    itemCommitmentHex: bounded.commitment.toString("hex"),
    itemLength: item.length,
    sourceLeafHex: sourceLeaf.toString("hex"),
    purposeLeafHex: purposeLeaf.toString("hex"),
    executionLeafHex: executionLeaf.toString("hex"),
    resultClass: scan.resultClass,
    initialControlCbor: scan.initialControlCbor,
    chunkProofCount: bounded.chunkHashes.length,
  });
};

export const executionSourceScriptDecodingEvidenceCloses = (
  evidence: ExecutionSourceScriptDecodingEvidence,
): boolean =>
  evidence.finding.subject.direction ===
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
    ? evidence.resultClass >= 0
    : evidence.resultClass !== evidence.finding.accusedClass;

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

export const executionSourceScriptDecodingCheckpoint = ({
  evidence,
  controlCbor,
  nextExpectedScriptHash,
}: {
  readonly evidence: ExecutionSourceScriptDecodingEvidence;
  readonly controlCbor: string;
  readonly nextExpectedScriptHash: string;
}): string => {
  const bytes = Buffer.concat([
    Buffer.from(
      "midgard/fraud-proofs/execution-source-script-decoding/checkpoint-v1",
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

export const EXECUTION_SOURCE_SCRIPT_DECODING_STAGES = [
  "none",
  "step01",
  "step02",
  "step03",
  "scan",
  "proven",
  "removed",
  "cancelled",
] as const;
export type ExecutionSourceScriptDecodingStage =
  (typeof EXECUTION_SOURCE_SCRIPT_DECODING_STAGES)[number];

export const nextExecutionSourceScriptDecodingAction = (
  stage: ExecutionSourceScriptDecodingStage,
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

export const executionSourceScriptDecodingEvidenceIdentity = (
  evidence: ExecutionSourceScriptDecodingEvidence,
): string =>
  [
    evidence.finding.subject.transaction_id,
    evidence.finding.subject.direction.toString(),
    evidence.finding.executionIndex.toString(),
    evidence.executionLeafHex,
    evidence.itemCommitmentHex,
  ].join(":");
