import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  buildMidgardNativeScriptDecodingTrace,
  decodeMidgardFieldPreimage,
  encodeMidgardNativeScriptStructureControl,
  midgardBoundedItemChunkCount,
  midgardFieldCommitment,
  MidgardNativeScriptDecodingBindKinds,
  MidgardNativeScriptDecodingRefusalClasses,
  MidgardNativeScriptDecodingTraceOutcomeKinds,
  parseMidgardVersionedScriptHeader,
  selectMidgardFieldCarriageTier,
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

export const WITNESS_SCRIPT_DECODING_CATEGORY =
  "witnessScriptDecoding" as const;
export const WITNESS_SCRIPT_DECODING_PROPOSED_ID = "00000022" as const;
export const WITNESS_SCRIPT_DECODING_FIELD_INDEX = 6 as const;

export const WitnessScriptDecodingResultClasses = Object.freeze({
  Pending: -1,
  NoFault: -2,
  HeaderMalformed: 0,
  NativeMalformed: 1,
  NodeLimit: 2,
  DepthLimit: 3,
} as const);
export type WitnessScriptDecodingResultClass =
  (typeof WitnessScriptDecodingResultClasses)[keyof typeof WitnessScriptDecodingResultClasses];

const fail = (message: string): never => {
  throw new Error(`${WITNESS_SCRIPT_DECODING_CATEGORY}: ${message}`);
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
  reason: RejectionReason,
  scriptIndex: number,
): WitnessScriptDecodingResultClass => {
  if (typeof reason === "string") return fail("typed reason is outside family");
  const entries: readonly [string, WitnessScriptDecodingResultClass][] = [
    [
      "WitnessScriptHeaderMalformed",
      WitnessScriptDecodingResultClasses.HeaderMalformed,
    ],
    [
      "WitnessNativeScriptMalformed",
      WitnessScriptDecodingResultClasses.NativeMalformed,
    ],
    [
      "WitnessNativeScriptNodeLimit",
      WitnessScriptDecodingResultClasses.NodeLimit,
    ],
    [
      "WitnessNativeScriptDepthLimit",
      WitnessScriptDecodingResultClasses.DepthLimit,
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

export type WitnessScriptDecodingFinding = Readonly<{
  subject: VerdictSubject;
  witnessSetHash: string;
  scriptIndex: number;
}>;

export const classifyWitnessScriptDecodingFinding = (
  finding: WitnessScriptDecodingFinding,
): WitnessScriptDecodingFinding & {
  readonly accusedClass: WitnessScriptDecodingResultClass;
} => {
  if (!verdictSubjectIsCanonical(finding.subject)) {
    return fail("verdict subject is not canonical");
  }
  exactIndex(finding.scriptIndex);
  exactHex(finding.witnessSetHash, 32, "witness set hash");
  const accusedClass =
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION
      ? finding.subject.rejection_reason === null
        ? fail("wrongful rejection carries no typed reason")
        : accusedClassV1(finding.subject.rejection_reason, finding.scriptIndex)
      : finding.subject.direction ===
            PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE &&
          finding.subject.rejection_reason === null
        ? WitnessScriptDecodingResultClasses.Pending
        : fail("direction/reason polarity is invalid");
  return Object.freeze({ ...finding, accusedClass });
};

const resultClassOfItem = (
  item: Uint8Array,
): {
  readonly resultClass: WitnessScriptDecodingResultClass;
  readonly initialControlCbor: string;
} => {
  const header = parseMidgardVersionedScriptHeader(item, item.length);
  if (header === null) {
    return {
      resultClass: WitnessScriptDecodingResultClasses.HeaderMalformed,
      initialControlCbor: "",
    };
  }
  if (header.languageTag !== 0) {
    return {
      resultClass: WitnessScriptDecodingResultClasses.NoFault,
      initialControlCbor: "",
    };
  }
  const trace = buildMidgardNativeScriptDecodingTrace(item);
  if (trace.bind.kind === MidgardNativeScriptDecodingBindKinds.Malformed) {
    // A parsed tag-0 wrapper can only land here for the empty payload. That is
    // a payload structural failure, not a header failure.
    return {
      resultClass: WitnessScriptDecodingResultClasses.NativeMalformed,
      initialControlCbor: "",
    };
  }
  if (trace.bind.kind !== MidgardNativeScriptDecodingBindKinds.Bound) {
    return fail("native header produced an impossible non-native trace");
  }
  const initialControlCbor = encodeMidgardNativeScriptStructureControl(
    trace.bind.control,
  ).toString("hex");
  if (trace.outcome === null)
    return fail("native scan has no terminal outcome");
  if (
    trace.outcome.kind === MidgardNativeScriptDecodingTraceOutcomeKinds.Terminal
  ) {
    return {
      resultClass: WitnessScriptDecodingResultClasses.NoFault,
      initialControlCbor,
    };
  }
  const resultClass =
    trace.outcome.refusalClass ===
    MidgardNativeScriptDecodingRefusalClasses.Malformed
      ? WitnessScriptDecodingResultClasses.NativeMalformed
      : trace.outcome.refusalClass ===
          MidgardNativeScriptDecodingRefusalClasses.NodeLimit
        ? WitnessScriptDecodingResultClasses.NodeLimit
        : WitnessScriptDecodingResultClasses.DepthLimit;
  return { resultClass, initialControlCbor };
};

export type WitnessScriptDecodingEvidence = Readonly<{
  finding: ReturnType<typeof classifyWitnessScriptDecodingFinding>;
  fieldPreimageHex: string;
  fieldCommitmentHex: string;
  itemHex: string;
  itemCommitmentHex: string;
  itemLength: number;
  /** Class step-02 can authenticate from the wrapper alone. */
  initialResultClass: WitnessScriptDecodingResultClass;
  /** Whole-item prediction the resumable scan must eventually establish. */
  resultClass: WitnessScriptDecodingResultClass;
  initialControlCbor: string;
  carriage: "Inline" | "RawUtxo" | "Certified";
  chunkProofCount: number;
}>;

export const prepareWitnessScriptDecodingEvidence = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: WitnessScriptDecodingFinding;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): WitnessScriptDecodingEvidence => {
  const finding = classifyWitnessScriptDecodingFinding(rawFinding);
  const commitment = midgardFieldCommitment(fieldPreimage).toString("hex");
  if (
    commitment !== exactHex(committedFieldHashHex, 32, "committed field hash")
  ) {
    return fail("retained field preimage differs from committed field hash");
  }
  const item = decodeMidgardFieldPreimage(fieldPreimage)[finding.scriptIndex];
  if (item === undefined) return fail("script coordinate is outside field 6");
  const bounded = buildMidgardBoundedItem({
    fieldIndex: WITNESS_SCRIPT_DECODING_FIELD_INDEX,
    itemIndex: finding.scriptIndex,
    bytes: item,
  });
  const decoded = resultClassOfItem(item);
  const header = parseMidgardVersionedScriptHeader(item, item.length);
  const initialResultClass =
    header === null
      ? WitnessScriptDecodingResultClasses.HeaderMalformed
      : header.languageTag === 0
        ? WitnessScriptDecodingResultClasses.Pending
        : WitnessScriptDecodingResultClasses.NoFault;
  // Materialize all proofs here from retained bytes; submission selects only
  // the cursor and optional adjacent proof needed by each resume transaction.
  const chunkProofCount = midgardBoundedItemChunkCount(item.length);
  for (let index = 0; index < chunkProofCount; index += 1) {
    buildMidgardBoundedItemChunkProof(bounded, index);
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
    carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
    chunkProofCount,
  });
};

export const witnessScriptDecodingEvidenceCloses = (
  evidence: WitnessScriptDecodingEvidence,
): boolean => {
  const { subject, accusedClass } = evidence.finding;
  const faultHolds = evidence.resultClass >= 0;
  return subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
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
export const witnessScriptDecodingCheckpoint = ({
  evidence,
  controlCbor,
  nextExpectedScriptHash,
}: {
  readonly evidence: WitnessScriptDecodingEvidence;
  readonly controlCbor: string;
  readonly nextExpectedScriptHash: string;
}): string => {
  const nextHash = exactHex(nextExpectedScriptHash, 28, "next script hash");
  const bytes = Buffer.concat([
    Buffer.from(
      "midgard/fraud-proofs/witness-script-decoding/checkpoint-v1",
      "ascii",
    ),
    encodeVerdictSubject(evidence.finding.subject),
    cborHead(0, evidence.finding.scriptIndex),
    cborHead(0, evidence.itemLength),
    cborBytes(Buffer.from(evidence.itemCommitmentHex, "hex")),
    cborBytes(Buffer.from(controlCbor, "hex")),
    cborBytes(Buffer.from(nextHash, "hex")),
  ]);
  return Effect.runSync(hashHexWithBlake2b(bytes.toString("hex"), 32));
};

export const WITNESS_SCRIPT_DECODING_STAGES = [
  "none",
  "step01",
  "step02",
  "scan",
  "step04",
  "proven",
  "removed",
  "cancelled",
] as const;
export type WitnessScriptDecodingStage =
  (typeof WITNESS_SCRIPT_DECODING_STAGES)[number];
export type WitnessScriptDecodingAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitScanOrResume"
  | "submitStep04"
  | "removeDescendants"
  | "done";

export const nextWitnessScriptDecodingAction = (
  stage: WitnessScriptDecodingStage,
): WitnessScriptDecodingAction => {
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

export type WitnessScriptDecodingJournalEntry = Readonly<{
  sequence: number;
  identity: string;
  stage: WitnessScriptDecodingStage;
  transactionId: string;
  outputReference: string | null;
  checkpointHash: string | null;
}>;

export const witnessScriptDecodingEvidenceIdentity = (
  evidence: WitnessScriptDecodingEvidence,
): string =>
  [
    evidence.finding.subject.transaction_id,
    evidence.finding.subject.direction.toString(),
    evidence.finding.scriptIndex.toString(),
    evidence.fieldCommitmentHex,
    evidence.itemCommitmentHex,
  ].join(":");

export const reconcileWitnessScriptDecodingJournal = ({
  identity,
  entries,
  observed,
}: {
  readonly identity: string;
  readonly entries: readonly WitnessScriptDecodingJournalEntry[];
  readonly observed: Pick<
    WitnessScriptDecodingJournalEntry,
    "stage" | "transactionId" | "outputReference" | "checkpointHash"
  >;
}): WitnessScriptDecodingStage => {
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

export type WitnessScriptDecodingSubmission = Readonly<{
  observe: (
    identity: string,
  ) => Promise<
    Pick<
      WitnessScriptDecodingJournalEntry,
      "stage" | "transactionId" | "outputReference" | "checkpointHash"
    >
  >;
  submit: (
    action: Exclude<WitnessScriptDecodingAction, "done">,
    evidence: WitnessScriptDecodingEvidence,
  ) => Promise<
    Pick<
      WitnessScriptDecodingJournalEntry,
      "stage" | "transactionId" | "outputReference" | "checkpointHash"
    >
  >;
  cancel: (
    stage: "step01" | "step02" | "scan" | "step04",
    evidence: WitnessScriptDecodingEvidence,
  ) => Promise<
    Pick<
      WitnessScriptDecodingJournalEntry,
      "stage" | "transactionId" | "outputReference" | "checkpointHash"
    >
  >;
}>;

export const runWitnessScriptDecodingProof = async ({
  evidence,
  load,
  append,
  submission,
}: {
  readonly evidence: WitnessScriptDecodingEvidence;
  readonly load: (
    identity: string,
  ) => Promise<readonly WitnessScriptDecodingJournalEntry[]>;
  readonly append: (entry: WitnessScriptDecodingJournalEntry) => Promise<void>;
  readonly submission: WitnessScriptDecodingSubmission;
}): Promise<WitnessScriptDecodingStage> => {
  if (!witnessScriptDecodingEvidenceCloses(evidence)) {
    return fail("honest verdict cannot start a proof thread");
  }
  const identity = witnessScriptDecodingEvidenceIdentity(evidence);
  for (;;) {
    const entries = await load(identity);
    const observed = await submission.observe(identity);
    const stage = reconcileWitnessScriptDecodingJournal({
      identity,
      entries,
      observed,
    });
    const action = nextWitnessScriptDecodingAction(stage);
    if (action === "done") return stage;
    const result = await submission.submit(action, evidence);
    await append({ sequence: entries.length, identity, ...result });
  }
};
