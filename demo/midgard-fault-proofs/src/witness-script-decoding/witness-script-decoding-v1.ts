import {
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  buildMidgardNativeScriptDecodingTraceV1,
  decodeMidgardFieldPreimageV1,
  encodeMidgardNativeScriptStructureControlV1,
  midgardBoundedItemChunkCountV1,
  midgardFieldCommitmentV1,
  MidgardNativeScriptDecodingBindKindsV1,
  MidgardNativeScriptDecodingRefusalClassesV1,
  MidgardNativeScriptDecodingTraceOutcomeKindsV1,
  parseMidgardVersionedScriptHeaderV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";
import {
  encodeVerdictSubjectV1,
  hashHexWithBlake2b,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  type RejectionReasonV1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

export const WITNESS_SCRIPT_DECODING_CATEGORY_V1 =
  "witnessScriptDecoding" as const;
export const WITNESS_SCRIPT_DECODING_PROPOSED_ID_V1 = "00000022" as const;
export const WITNESS_SCRIPT_DECODING_FIELD_INDEX_V1 = 6 as const;

export const WitnessScriptDecodingResultClassesV1 = Object.freeze({
  Pending: -1,
  NoFault: -2,
  HeaderMalformed: 0,
  NativeMalformed: 1,
  NodeLimit: 2,
  DepthLimit: 3,
} as const);
export type WitnessScriptDecodingResultClassV1 =
  (typeof WitnessScriptDecodingResultClassesV1)[keyof typeof WitnessScriptDecodingResultClassesV1];

const fail = (message: string): never => {
  throw new Error(`${WITNESS_SCRIPT_DECODING_CATEGORY_V1}: ${message}`);
};

const exactHex = (value: string, bytes: number, label: string): string => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value)) {
    return fail(`${label} must be canonical ${bytes.toString()}-byte hex`);
  }
  return value;
};

const exactIndex = (value: number): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    return fail("script index must be a non-negative safe integer");
  }
  return value;
};

const accusedClassV1 = (
  reason: RejectionReasonV1,
  scriptIndex: number,
): WitnessScriptDecodingResultClassV1 => {
  if (typeof reason === "string") return fail("typed reason is outside family");
  const entries: readonly [string, WitnessScriptDecodingResultClassV1][] = [
    [
      "WitnessScriptHeaderMalformed",
      WitnessScriptDecodingResultClassesV1.HeaderMalformed,
    ],
    [
      "WitnessNativeScriptMalformed",
      WitnessScriptDecodingResultClassesV1.NativeMalformed,
    ],
    [
      "WitnessNativeScriptNodeLimit",
      WitnessScriptDecodingResultClassesV1.NodeLimit,
    ],
    [
      "WitnessNativeScriptDepthLimit",
      WitnessScriptDecodingResultClassesV1.DepthLimit,
    ],
  ];
  for (const [constructor, resultClass] of entries) {
    if (constructor in reason) {
      const payload = (
        reason as unknown as Record<string, { script_index: bigint }>
      )[constructor];
      if (
        payload === undefined ||
        payload.script_index !== BigInt(scriptIndex)
      ) {
        return fail("typed reason script coordinate differs");
      }
      return resultClass;
    }
  }
  return fail("typed reason is outside witness-script-decoding");
};

export type WitnessScriptDecodingFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  witnessSetHash: string;
  scriptIndex: number;
}>;

export const classifyWitnessScriptDecodingFindingV1 = (
  finding: WitnessScriptDecodingFindingV1,
): WitnessScriptDecodingFindingV1 & {
  readonly accusedClass: WitnessScriptDecodingResultClassV1;
} => {
  if (!verdictSubjectIsCanonicalV1(finding.subject)) {
    return fail("verdict subject is not canonical");
  }
  exactIndex(finding.scriptIndex);
  exactHex(finding.witnessSetHash, 32, "witness set hash");
  const accusedClass =
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
      ? finding.subject.rejection_reason === null
        ? fail("wrongful rejection carries no typed reason")
        : accusedClassV1(finding.subject.rejection_reason, finding.scriptIndex)
      : finding.subject.direction ===
            PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 &&
          finding.subject.rejection_reason === null
        ? WitnessScriptDecodingResultClassesV1.Pending
        : fail("direction/reason polarity is invalid");
  return Object.freeze({ ...finding, accusedClass });
};

const resultClassOfItemV1 = (
  item: Uint8Array,
): {
  readonly resultClass: WitnessScriptDecodingResultClassV1;
  readonly initialControlCbor: string;
} => {
  const header = parseMidgardVersionedScriptHeaderV1(item, item.length);
  if (header === null) {
    return {
      resultClass: WitnessScriptDecodingResultClassesV1.HeaderMalformed,
      initialControlCbor: "",
    };
  }
  if (header.languageTag !== 0) {
    return {
      resultClass: WitnessScriptDecodingResultClassesV1.NoFault,
      initialControlCbor: "",
    };
  }
  const trace = buildMidgardNativeScriptDecodingTraceV1(item);
  if (trace.bind.kind === MidgardNativeScriptDecodingBindKindsV1.Malformed) {
    // A parsed tag-0 wrapper can only land here for the empty payload. That is
    // a payload structural failure, not a header failure.
    return {
      resultClass: WitnessScriptDecodingResultClassesV1.NativeMalformed,
      initialControlCbor: "",
    };
  }
  if (trace.bind.kind !== MidgardNativeScriptDecodingBindKindsV1.Bound) {
    return fail("native header produced an impossible non-native trace");
  }
  const initialControlCbor = encodeMidgardNativeScriptStructureControlV1(
    trace.bind.control,
  ).toString("hex");
  if (trace.outcome === null)
    return fail("native scan has no terminal outcome");
  if (
    trace.outcome.kind ===
    MidgardNativeScriptDecodingTraceOutcomeKindsV1.Terminal
  ) {
    return {
      resultClass: WitnessScriptDecodingResultClassesV1.NoFault,
      initialControlCbor,
    };
  }
  const resultClass =
    trace.outcome.refusalClass ===
    MidgardNativeScriptDecodingRefusalClassesV1.Malformed
      ? WitnessScriptDecodingResultClassesV1.NativeMalformed
      : trace.outcome.refusalClass ===
          MidgardNativeScriptDecodingRefusalClassesV1.NodeLimit
        ? WitnessScriptDecodingResultClassesV1.NodeLimit
        : WitnessScriptDecodingResultClassesV1.DepthLimit;
  return { resultClass, initialControlCbor };
};

export type WitnessScriptDecodingEvidenceV1 = Readonly<{
  finding: ReturnType<typeof classifyWitnessScriptDecodingFindingV1>;
  fieldPreimageHex: string;
  fieldCommitmentHex: string;
  itemHex: string;
  itemCommitmentHex: string;
  itemLength: number;
  /** Class step-02 can authenticate from the wrapper alone. */
  initialResultClass: WitnessScriptDecodingResultClassV1;
  /** Whole-item prediction the resumable scan must eventually establish. */
  resultClass: WitnessScriptDecodingResultClassV1;
  initialControlCbor: string;
  carriage: "Inline" | "RawUtxo" | "Certified";
  chunkProofCount: number;
}>;

export const prepareWitnessScriptDecodingEvidenceV1 = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: WitnessScriptDecodingFindingV1;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): WitnessScriptDecodingEvidenceV1 => {
  const finding = classifyWitnessScriptDecodingFindingV1(rawFinding);
  const commitment = midgardFieldCommitmentV1(fieldPreimage).toString("hex");
  if (
    commitment !== exactHex(committedFieldHashHex, 32, "committed field hash")
  ) {
    return fail("retained field preimage differs from committed field hash");
  }
  const item = decodeMidgardFieldPreimageV1(fieldPreimage)[finding.scriptIndex];
  if (item === undefined) return fail("script coordinate is outside field 6");
  const bounded = buildMidgardBoundedItemV1({
    fieldIndex: WITNESS_SCRIPT_DECODING_FIELD_INDEX_V1,
    itemIndex: finding.scriptIndex,
    bytes: item,
  });
  const decoded = resultClassOfItemV1(item);
  const header = parseMidgardVersionedScriptHeaderV1(item, item.length);
  const initialResultClass =
    header === null
      ? WitnessScriptDecodingResultClassesV1.HeaderMalformed
      : header.languageTag === 0
        ? WitnessScriptDecodingResultClassesV1.Pending
        : WitnessScriptDecodingResultClassesV1.NoFault;
  // Materialize all proofs here from retained bytes; submission selects only
  // the cursor and optional adjacent proof needed by each resume transaction.
  const chunkProofCount = midgardBoundedItemChunkCountV1(item.length);
  for (let index = 0; index < chunkProofCount; index += 1) {
    buildMidgardBoundedItemChunkProofV1(bounded, index);
  }
  return Object.freeze({
    finding,
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: commitment,
    itemHex: Buffer.from(item).toString("hex"),
    itemCommitmentHex: bounded.commitment.toString("hex"),
    itemLength: item.length,
    initialResultClass,
    resultClass: decoded.resultClass,
    initialControlCbor: decoded.initialControlCbor,
    carriage: selectMidgardFieldCarriageTierV1(fieldPreimage.length),
    chunkProofCount,
  });
};

export const witnessScriptDecodingEvidenceClosesV1 = (
  evidence: WitnessScriptDecodingEvidenceV1,
): boolean => {
  const { subject, accusedClass } = evidence.finding;
  const faultHolds = evidence.resultClass >= 0;
  return subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
    ? faultHolds
    : evidence.resultClass !== accusedClass;
};

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

/** Exact twin of Aiken `checkpoint_v1`. */
export const witnessScriptDecodingCheckpointV1 = ({
  evidence,
  controlCbor,
  nextExpectedScriptHash,
}: {
  readonly evidence: WitnessScriptDecodingEvidenceV1;
  readonly controlCbor: string;
  readonly nextExpectedScriptHash: string;
}): string => {
  const nextHash = exactHex(nextExpectedScriptHash, 28, "next script hash");
  const bytes = Buffer.concat([
    Buffer.from(
      "midgard/fraud-proofs/witness-script-decoding/checkpoint-v1",
      "ascii",
    ),
    encodeVerdictSubjectV1(evidence.finding.subject),
    cborHead(0, evidence.finding.scriptIndex),
    cborHead(0, evidence.itemLength),
    cborBytes(Buffer.from(evidence.itemCommitmentHex, "hex")),
    cborBytes(Buffer.from(controlCbor, "hex")),
    cborBytes(Buffer.from(nextHash, "hex")),
  ]);
  return Effect.runSync(hashHexWithBlake2b(bytes.toString("hex"), 32));
};

export const WITNESS_SCRIPT_DECODING_STAGES_V1 = [
  "none",
  "step01",
  "step02",
  "scan",
  "step04",
  "proven",
  "removed",
  "cancelled",
] as const;
export type WitnessScriptDecodingStageV1 =
  (typeof WITNESS_SCRIPT_DECODING_STAGES_V1)[number];
export type WitnessScriptDecodingActionV1 =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitScanOrResume"
  | "submitStep04"
  | "removeDescendants"
  | "done";

export const nextWitnessScriptDecodingActionV1 = (
  stage: WitnessScriptDecodingStageV1,
): WitnessScriptDecodingActionV1 => {
  switch (stage) {
    case "none":
      return "submitInit";
    case "step01":
      return "submitStep01";
    case "step02":
      return "submitStep02";
    case "scan":
      return "submitScanOrResume";
    case "step04":
      return "submitStep04";
    case "proven":
      return "removeDescendants";
    case "removed":
    case "cancelled":
      return "done";
  }
};

export type WitnessScriptDecodingJournalEntryV1 = Readonly<{
  sequence: number;
  identity: string;
  stage: WitnessScriptDecodingStageV1;
  transactionId: string;
  outputReference: string | null;
  checkpointHash: string | null;
}>;

export const witnessScriptDecodingEvidenceIdentityV1 = (
  evidence: WitnessScriptDecodingEvidenceV1,
): string =>
  [
    evidence.finding.subject.transaction_id,
    evidence.finding.subject.direction.toString(),
    evidence.finding.scriptIndex.toString(),
    evidence.fieldCommitmentHex,
    evidence.itemCommitmentHex,
  ].join(":");

export const reconcileWitnessScriptDecodingJournalV1 = ({
  identity,
  entries,
  observed,
}: {
  readonly identity: string;
  readonly entries: readonly WitnessScriptDecodingJournalEntryV1[];
  readonly observed: Pick<
    WitnessScriptDecodingJournalEntryV1,
    "stage" | "transactionId" | "outputReference" | "checkpointHash"
  >;
}): WitnessScriptDecodingStageV1 => {
  let previous = -1;
  for (const entry of entries) {
    if (entry.identity !== identity) return fail("journal identity mismatch");
    if (entry.sequence !== previous + 1)
      return fail("journal sequence is not contiguous");
    exactHex(entry.transactionId, 32, "journal transaction id");
    previous = entry.sequence;
  }
  const last = entries.at(-1);
  if (last !== undefined && last.stage === observed.stage) {
    if (
      last.transactionId !== observed.transactionId ||
      last.outputReference !== observed.outputReference ||
      last.checkpointHash !== observed.checkpointHash
    ) {
      return fail("authenticated transaction identity changed across restart");
    }
  }
  return observed.stage;
};

export type WitnessScriptDecodingSubmissionV1 = Readonly<{
  observe: (
    identity: string,
  ) => Promise<
    Pick<
      WitnessScriptDecodingJournalEntryV1,
      "stage" | "transactionId" | "outputReference" | "checkpointHash"
    >
  >;
  submit: (
    action: Exclude<WitnessScriptDecodingActionV1, "done">,
    evidence: WitnessScriptDecodingEvidenceV1,
  ) => Promise<
    Pick<
      WitnessScriptDecodingJournalEntryV1,
      "stage" | "transactionId" | "outputReference" | "checkpointHash"
    >
  >;
  cancel: (
    stage: "step01" | "step02" | "scan" | "step04",
    evidence: WitnessScriptDecodingEvidenceV1,
  ) => Promise<
    Pick<
      WitnessScriptDecodingJournalEntryV1,
      "stage" | "transactionId" | "outputReference" | "checkpointHash"
    >
  >;
}>;

export const runWitnessScriptDecodingProofV1 = async ({
  evidence,
  load,
  append,
  submission,
}: {
  readonly evidence: WitnessScriptDecodingEvidenceV1;
  readonly load: (
    identity: string,
  ) => Promise<readonly WitnessScriptDecodingJournalEntryV1[]>;
  readonly append: (
    entry: WitnessScriptDecodingJournalEntryV1,
  ) => Promise<void>;
  readonly submission: WitnessScriptDecodingSubmissionV1;
}): Promise<WitnessScriptDecodingStageV1> => {
  if (!witnessScriptDecodingEvidenceClosesV1(evidence)) {
    return fail("honest verdict cannot start a proof thread");
  }
  const identity = witnessScriptDecodingEvidenceIdentityV1(evidence);
  for (;;) {
    const entries = await load(identity);
    const observed = await submission.observe(identity);
    const stage = reconcileWitnessScriptDecodingJournalV1({
      identity,
      entries,
      observed,
    });
    const action = nextWitnessScriptDecodingActionV1(stage);
    if (action === "done") return stage;
    const result = await submission.submit(action, evidence);
    await append({ sequence: entries.length, identity, ...result });
  }
};
