import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  classifyMidgardWitnessScriptItem,
  decodeMidgardFieldPreimage,
  midgardBoundedItemChunkCount,
  midgardFieldCommitment,
  type MidgardWitnessScriptItemClass,
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

import { claimedItem } from "../evidence/claimed-coordinate.js";
import {
  PLUTUS_SCRIPT_ITEM_KIND_WITNESS,
  plutusItemScriptDecode,
  plutusRecordResultClass,
  type PlutusScriptRefusedResultClass,
  plutusUndecodableAccusedClass,
} from "../script-decode/item-record.js";

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
  | (typeof WitnessScriptDecodingResultClasses)[keyof typeof WitnessScriptDecodingResultClasses]
  | PlutusScriptRefusedResultClass;

const fail = (message: string): never => {
  throw new Error(`${WITNESS_SCRIPT_DECODING_CATEGORY}: ${message}`);
};

const exactHex = (value: string, bytes: number, label: string): string => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value)) {
    return fail(`${label} must be canonical ${bytes.toString()}-byte hex`);
  }
  return value;
};

const exactIndex = (value: bigint): bigint => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    return fail("script index must be a non-negative safe integer");
  }
  return value;
};

const accusedClassV1 = (
  reason: RejectionReason,
  scriptIndex: bigint,
): WitnessScriptDecodingResultClass => {
  if (typeof reason === "string") return fail("typed reason is outside family");
  if ("PlutusScriptUndecodable" in reason) {
    try {
      return (
        plutusUndecodableAccusedClass(
          reason,
          PLUTUS_SCRIPT_ITEM_KIND_WITNESS,
          scriptIndex,
        ) ?? fail("typed reason is outside witness-script-decoding")
      );
    } catch {
      return fail("typed reason script coordinate differs");
    }
  }
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
      if (payload === undefined || payload.script_index !== scriptIndex) {
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
  /** Any integer the rejection codec admits; a rejection may name one field 6 lacks. */
  scriptIndex: bigint;
}>;

export const classifyWitnessScriptDecodingFinding = (
  finding: WitnessScriptDecodingFinding,
): WitnessScriptDecodingFinding & {
  readonly accusedClass: WitnessScriptDecodingResultClass;
} => {
  if (!verdictSubjectIsCanonical(finding.subject)) {
    return fail("verdict subject is not canonical");
  }
  // A rejection may cite any position, so a negative one is a coordinate
  // field 6 lacks; an acceptance binds an existing script.
  if (typeof finding.scriptIndex !== "bigint") {
    return fail("script index must be an integer");
  }
  if (finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    exactIndex(finding.scriptIndex);
  }
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

const RESULT_CLASS_OF_ITEM_CLASS: Readonly<
  Record<MidgardWitnessScriptItemClass, WitnessScriptDecodingResultClass>
> = Object.freeze({
  noFault: WitnessScriptDecodingResultClasses.NoFault,
  headerMalformed: WitnessScriptDecodingResultClasses.HeaderMalformed,
  nativeMalformed: WitnessScriptDecodingResultClasses.NativeMalformed,
  nodeLimit: WitnessScriptDecodingResultClasses.NodeLimit,
  depthLimit: WitnessScriptDecodingResultClasses.DepthLimit,
});

const resultClassOfItem = (
  item: Uint8Array,
): {
  readonly resultClass: WitnessScriptDecodingResultClass;
  readonly initialControlCbor: string;
} => {
  const { itemClass, initialControlCbor } =
    classifyMidgardWitnessScriptItem(item);
  return {
    resultClass: RESULT_CLASS_OF_ITEM_CLASS[itemClass],
    initialControlCbor,
  };
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
  /**
   * The rejection names a script field 6 does not have. Step 02 then reads
   * the field's count instead of an item and closes the scan with no fault.
   */
  absent: boolean;
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
  const item = claimedItem(
    decodeMidgardFieldPreimage(fieldPreimage),
    finding.scriptIndex,
  );
  if (item === undefined) {
    // Canonical validation decodes no script at a position field 6 lacks, so
    // only a rejection citing one is wrong.
    if (finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION)
      return fail("script coordinate is outside field 6");
    return Object.freeze({
      finding,
      fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
      fieldCommitmentHex: commitment,
      itemHex: "",
      // Twin of `absent_item_commitment`: an empty item's, at item 0.
      itemCommitmentHex: buildMidgardBoundedItem({
        fieldIndex: WITNESS_SCRIPT_DECODING_FIELD_INDEX,
        itemIndex: 0,
        bytes: Buffer.alloc(0),
      }).commitment.toString("hex"),
      itemLength: 0,
      initialResultClass: WitnessScriptDecodingResultClasses.NoFault,
      resultClass: WitnessScriptDecodingResultClasses.NoFault,
      initialControlCbor: "",
      carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
      chunkProofCount: 0,
      absent: true,
    });
  }
  const bounded = buildMidgardBoundedItem({
    fieldIndex: WITNESS_SCRIPT_DECODING_FIELD_INDEX,
    itemIndex: Number(finding.scriptIndex),
    bytes: item,
  });
  const decoded = resultClassOfItem(item);
  const header = parseMidgardVersionedScriptHeader(item, item.length);
  // A Plutus item's class is its decode record's, which step 02 reads.
  const plutus = plutusItemScriptDecode(item);
  const plutusClass =
    plutus === undefined
      ? undefined
      : (plutusRecordResultClass(plutus.record) ??
        WitnessScriptDecodingResultClasses.NoFault);
  const resultClass = plutusClass ?? decoded.resultClass;
  const initialResultClass =
    header === null
      ? WitnessScriptDecodingResultClasses.HeaderMalformed
      : plutusClass !== undefined
        ? plutusClass
        : header.payloadLength === 0
          ? // Twin of `authenticate_item_v1`: a decodable tag-0 wrapper over an
            // empty payload is the structural class, fixed at step 02 because
            // the frozen scan control cannot open an empty region.
            WitnessScriptDecodingResultClasses.NativeMalformed
          : WitnessScriptDecodingResultClasses.Pending;
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
    resultClass,
    initialControlCbor: decoded.initialControlCbor,
    carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
    chunkProofCount,
    absent: false,
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

const cborHead = (major: number, input: bigint | number): Buffer => {
  const value = BigInt(input);
  if (value < 24n) return Buffer.from([(major << 5) | Number(value)]);
  if (value >= 1n << 64n) {
    return fail("CBOR head argument is wider than 8 bytes");
  }
  const [width, additional] =
    value <= 0xffn
      ? [1, 24]
      : value <= 0xffffn
        ? [2, 25]
        : value <= 0xffffffffn
          ? [4, 26]
          : [8, 27];
  return Buffer.concat([
    Buffer.from([(major << 5) | additional]),
    Buffer.from(value.toString(16).padStart(width * 2, "0"), "hex"),
  ]);
};
/**
 * Twin of `cbor.serialise` on an integer the rejection codec admits, of
 * either sign.
 */
const cborInteger = (value: bigint): Buffer =>
  value < 0n ? cborHead(1, -1n - value) : cborHead(0, value);
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
    cborInteger(evidence.finding.scriptIndex),
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

export type WitnessScriptDecodingJournalEntry = Readonly<{
  sequence: number;
  identity: string;
  stage: WitnessScriptDecodingStage;
  transactionId: string;
  outputReference: string | null;
  checkpointHash: string | null;
}>;
