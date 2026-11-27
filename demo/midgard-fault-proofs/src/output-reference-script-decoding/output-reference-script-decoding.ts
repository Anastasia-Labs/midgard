import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  buildMidgardLedgerOutputScanTrace,
  buildMidgardNativeScriptDecodingTrace,
  computeHash32,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeScriptStructureControl,
  type MidgardLedgerOutputScanControl,
  MidgardNativeScriptDecodingBindKinds,
  MidgardNativeScriptDecodingRefusalClasses,
  MidgardNativeScriptDecodingTraceOutcomeKinds,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk/fraud-proof";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { claimedItem } from "../evidence/claimed-coordinate.js";
import {
  PLUTUS_SCRIPT_ITEM_KIND_REFERENCE_SCRIPT,
  PLUTUS_SCRIPT_REFUSED_RESULT_BASE,
  plutusItemScriptDecode,
  plutusRecordResultClass,
  type PlutusScriptRefusedResultClass,
  plutusUndecodableAccusedClass,
} from "../script-decode/item-record.js";

export {
  outputReferenceScriptCheckpoint,
  outputReferenceScriptControlData,
} from "./output-reference-script-decoding.step-state.js";

export const OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY =
  "outputReferenceScriptDecoding" as const;
export const OUTPUT_REFERENCE_SCRIPT_DECODING_MAX_OUTPUT_BYTES = 16_384;
export const OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID =
  "output-reference-script-malformed" as const;
export const OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID =
  "output-reference-script-node-limit" as const;
export const OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID =
  "output-reference-script-depth-limit" as const;
export const OUTPUT_REFERENCE_PLUTUS_SCRIPT_UNDECODABLE_VIOLATION_ID =
  "output-reference-plutus-script-undecodable" as const;
export type OutputReferenceScriptDecodingViolationId =
  | typeof OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID
  | typeof OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID
  | typeof OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID
  | typeof OUTPUT_REFERENCE_PLUTUS_SCRIPT_UNDECODABLE_VIOLATION_ID;

export const OutputReferenceScriptResultClasses = Object.freeze({
  Pending: -1,
  NoFault: -2,
  Malformed: 0,
  NodeLimit: 1,
  DepthLimit: 2,
} as const);
export type OutputReferenceScriptResultClass =
  | (typeof OutputReferenceScriptResultClasses)[keyof typeof OutputReferenceScriptResultClasses]
  | PlutusScriptRefusedResultClass;

/**
 * The class a rejection accuses, as the on-chain binding computes it: an
 * integer of any width, since a rejection may name any decode class.
 */
export type OutputReferenceScriptAccusedClass = bigint;

const fail = (message: string): never => {
  throw new Error(`${OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY}: ${message}`);
};

const exactOutputIndex = (subject: VerdictSubject): bigint => {
  const reason = subject.rejection_reason;
  if (reason === null || typeof reason === "string")
    return fail("forced subject has the wrong typed reason");
  if (
    "PlutusScriptUndecodable" in reason &&
    reason.PlutusScriptUndecodable.item_kind ===
      PLUTUS_SCRIPT_ITEM_KIND_REFERENCE_SCRIPT
  )
    return reason.PlutusScriptUndecodable.item_index;
  const entries = [
    [
      "OutputReferenceScriptMalformed",
      OutputReferenceScriptResultClasses.Malformed,
    ],
    [
      "OutputReferenceScriptNodeLimit",
      OutputReferenceScriptResultClasses.NodeLimit,
    ],
    [
      "OutputReferenceScriptDepthLimit",
      OutputReferenceScriptResultClasses.DepthLimit,
    ],
  ] as const;
  for (const [kind] of entries) {
    if (!(kind in reason)) continue;
    const payload = (
      reason as unknown as Record<string, { output_index: bigint }>
    )[kind];
    if (payload === undefined) return fail("typed reason payload is absent");
    return payload.output_index;
  }
  return fail("forced subject has the wrong typed reason");
};

const accusedClass = (
  subject: VerdictSubject,
): OutputReferenceScriptAccusedClass => {
  const reason = subject.rejection_reason;
  if (reason === null || typeof reason === "string")
    return fail("typed reason is absent");
  if ("OutputReferenceScriptMalformed" in reason)
    return BigInt(OutputReferenceScriptResultClasses.Malformed);
  if ("OutputReferenceScriptNodeLimit" in reason)
    return BigInt(OutputReferenceScriptResultClasses.NodeLimit);
  if ("OutputReferenceScriptDepthLimit" in reason)
    return BigInt(OutputReferenceScriptResultClasses.DepthLimit);
  if ("PlutusScriptUndecodable" in reason)
    return (
      plutusUndecodableAccusedClass(
        reason,
        PLUTUS_SCRIPT_ITEM_KIND_REFERENCE_SCRIPT,
        reason.PlutusScriptUndecodable.item_index,
      ) ?? fail("typed reason is outside family")
    );
  return fail("typed reason is outside family");
};

export type OutputReferenceScriptDecodingFinding = Readonly<{
  subject: VerdictSubject;
  /** Any integer the rejection codec admits; a rejection may name one field 2 lacks. */
  outputIndex: bigint;
}>;

export const classifyOutputReferenceScriptDecodingFinding = (
  finding: OutputReferenceScriptDecodingFinding,
): void => {
  if (!verdictSubjectIsCanonical(finding.subject))
    return fail("subject is not canonical");
  // A rejection may name any output coordinate, including one field 2 lacks;
  // an acceptance names an output the field holds.
  if (
    typeof finding.outputIndex !== "bigint" ||
    (finding.outputIndex < 0n &&
      finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION)
  )
    return fail("output coordinate is invalid");
  if (finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    if (exactOutputIndex(finding.subject) !== finding.outputIndex)
      return fail("typed reason output coordinate differs");
  } else if (
    finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    finding.subject.rejection_reason !== null
  )
    return fail("subject polarity is invalid");
};

const resultClassOf = (item: Uint8Array): OutputReferenceScriptResultClass => {
  const trace = buildMidgardNativeScriptDecodingTrace(item);
  if (trace.bind.kind === MidgardNativeScriptDecodingBindKinds.Malformed)
    return OutputReferenceScriptResultClasses.Malformed;
  if (trace.bind.kind === MidgardNativeScriptDecodingBindKinds.NonNative) {
    const decoded = plutusItemScriptDecode(item);
    if (decoded === undefined) return fail("non-native item has no script");
    return (
      plutusRecordResultClass(decoded.record) ??
      OutputReferenceScriptResultClasses.NoFault
    );
  }
  if (trace.outcome === null)
    return fail("native scan has no terminal outcome");
  if (
    trace.outcome.kind === MidgardNativeScriptDecodingTraceOutcomeKinds.Terminal
  )
    return OutputReferenceScriptResultClasses.NoFault;
  return trace.outcome.refusalClass ===
    MidgardNativeScriptDecodingRefusalClasses.Malformed
    ? OutputReferenceScriptResultClasses.Malformed
    : trace.outcome.refusalClass ===
        MidgardNativeScriptDecodingRefusalClasses.NodeLimit
      ? OutputReferenceScriptResultClasses.NodeLimit
      : OutputReferenceScriptResultClasses.DepthLimit;
};

export type OutputReferenceScriptDecodingEvidence =
  OutputReferenceScriptDecodingFinding &
    Readonly<{
      canonicalTransactionCborHex: string;
      outputFieldPreimageHex: string;
      outputCborHex: string;
      outputLength: number;
      outputHashHex: string;
      outputChunkHashes: readonly string[];
      outputScanControls: readonly MidgardLedgerOutputScanControl[];
      referenceScriptItemHex: string;
      referenceScriptItemCommitmentHex: string;
      resultClass: OutputReferenceScriptResultClass;
      accusedClass: OutputReferenceScriptAccusedClass;
      carriage: "Inline" | "RawUtxo" | "Certified";
      chunkProofCount: number;
      initialControlCbor: string;
      /**
       * The rejection names an output field 2 does not have: a negative
       * coordinate, or one past the field's last output. Step 02 closes the
       * thread at step 06 with no fault (`rule.absent_output_state_v1`),
       * reading the field's count for a coordinate past it.
       */
      absent: boolean;
    }>;

export const prepareOutputReferenceScriptDecodingEvidence = ({
  subject,
  outputIndex,
  canonicalTransactionCbor,
  admitHonestVerdict = false,
}: OutputReferenceScriptDecodingFinding & {
  readonly canonicalTransactionCbor: Uint8Array;
  /**
   * Diagnostic only: keep the authenticated evidence of a verdict the decoder
   * agrees with, so a lifecycle suite can drive the honest refusal through
   * the real chain. The submission builders still refuse to close on it.
   */
  readonly admitHonestVerdict?: boolean;
}): OutputReferenceScriptDecodingEvidence => {
  classifyOutputReferenceScriptDecodingFinding({ subject, outputIndex });
  const material = (
    subject.source_kind === 1n
      ? deriveMidgardForcedTxFaultEvidenceMaterial
      : deriveMidgardNativeTxFaultEvidenceMaterial
  )(canonicalTransactionCbor);
  if (material.transactionId.toString("hex") !== subject.transaction_id)
    return fail("transaction identity was substituted");
  const field = material.fieldPreimages[2]!;
  const output = claimedItem(decodeMidgardFieldPreimage(field), outputIndex);
  if (output === undefined) {
    // Canonical validation decodes no reference script at a position field 2
    // lacks, so only a rejection naming one is wrong.
    if (subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION)
      return fail("output coordinate is out of range");
    return Object.freeze({
      subject,
      outputIndex,
      canonicalTransactionCborHex: Buffer.from(
        canonicalTransactionCbor,
      ).toString("hex"),
      outputFieldPreimageHex: field.toString("hex"),
      outputCborHex: "",
      outputLength: 0,
      outputHashHex: "",
      outputChunkHashes: Object.freeze([]),
      outputScanControls: Object.freeze([]),
      referenceScriptItemHex: "",
      // The closed state commits an empty item at position 0, as
      // `rule.absent_output_state_v1` does.
      referenceScriptItemCommitmentHex: buildMidgardBoundedItem({
        fieldIndex: 2,
        itemIndex: 0,
        bytes: Buffer.alloc(0),
      }).commitment.toString("hex"),
      resultClass: OutputReferenceScriptResultClasses.NoFault,
      accusedClass: accusedClass(subject),
      carriage: selectMidgardFieldCarriageTier(field.length),
      chunkProofCount: 0,
      initialControlCbor: "",
      absent: true,
    });
  }
  if (output.length > OUTPUT_REFERENCE_SCRIPT_DECODING_MAX_OUTPUT_BYTES)
    return fail("output exceeds canonical size bound");
  let trace;
  try {
    trace = buildMidgardLedgerOutputScanTrace(output);
  } catch {
    return fail("selected output descriptor is not canonical");
  }
  // An output without a reference script binds the empty item and closes
  // no-fault at step 04 (the twin of `bind_reference_script_v1`'s language
  // -1 arm), so a rejection naming its reference script still closes.
  const carriesScript = trace.terminal.referenceScriptLanguage !== -1;
  const offset = trace.terminal.referenceScriptItemOffset;
  if (carriesScript && offset < 0)
    return fail("selected output carries no reference script");
  const item = carriesScript ? output.subarray(offset) : Buffer.alloc(0);
  const bounded = buildMidgardBoundedItem({
    fieldIndex: 2,
    itemIndex: Number(outputIndex),
    bytes: item,
  });
  for (let index = 0; index < bounded.frontier.count; index += 1)
    buildMidgardBoundedItemChunkProof(bounded, index);
  const resultClass = carriesScript
    ? resultClassOf(item)
    : OutputReferenceScriptResultClasses.NoFault;
  const nativeTrace = carriesScript
    ? buildMidgardNativeScriptDecodingTrace(item)
    : undefined;
  const evidence = Object.freeze({
    subject,
    outputIndex,
    canonicalTransactionCborHex: Buffer.from(canonicalTransactionCbor).toString(
      "hex",
    ),
    outputFieldPreimageHex: field.toString("hex"),
    outputCborHex: output.toString("hex"),
    outputLength: output.length,
    outputHashHex: computeHash32(output).toString("hex"),
    outputChunkHashes: Object.freeze(
      Array.from({ length: Math.ceil(output.length / 4_095) }, (_, index) =>
        computeHash32(
          output.subarray(index * 4_095, (index + 1) * 4_095),
        ).toString("hex"),
      ),
    ),
    outputScanControls: Object.freeze([
      trace.initial,
      ...trace.steps.map((step) => step.next),
    ]),
    referenceScriptItemHex: item.toString("hex"),
    referenceScriptItemCommitmentHex: bounded.commitment.toString("hex"),
    resultClass,
    accusedClass:
      subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION
        ? accusedClass(subject)
        : BigInt(OutputReferenceScriptResultClasses.Pending),
    carriage: selectMidgardFieldCarriageTier(field.length),
    chunkProofCount: bounded.frontier.count,
    initialControlCbor:
      nativeTrace?.bind.kind === MidgardNativeScriptDecodingBindKinds.Bound
        ? encodeMidgardNativeScriptStructureControl(
            nativeTrace.bind.control,
          ).toString("hex")
        : "",
    absent: false,
  });
  if (!admitHonestVerdict && !outputReferenceScriptEvidenceCloses(evidence))
    return fail("authenticated decoder agrees with operator verdict");
  return evidence;
};

export const outputReferenceScriptEvidenceCloses = (
  evidence: OutputReferenceScriptDecodingEvidence,
): boolean =>
  evidence.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION
    ? BigInt(evidence.resultClass) !== evidence.accusedClass
    : terminalVerdictContradiction(evidence.subject, evidence.resultClass >= 0);

/** Exhaustive replay over every accepted output and exact forced coordinate. */
export const detectOutputReferenceScriptDecodingCompleteReplay = (
  block: CanonicalBlockEvidence,
): readonly OutputReferenceScriptDecodingEvidence[] => {
  const results: OutputReferenceScriptDecodingEvidence[] = [];
  const inspect = (
    subject: VerdictSubject,
    bytes: Uint8Array,
    outputIndex: bigint,
  ): void => {
    try {
      results.push(
        prepareOutputReferenceScriptDecodingEvidence({
          subject,
          outputIndex,
          canonicalTransactionCbor: bytes,
        }),
      );
    } catch (cause) {
      if (
        cause instanceof Error &&
        [
          "selected output descriptor is not canonical",
          "selected output carries no reference script",
          "authenticated decoder agrees with operator verdict",
          "output coordinate is out of range",
        ].some((suffix) => cause.message.endsWith(suffix))
      )
        return;
      throw cause;
    }
  };
  block.transactions.forEach((transaction) => {
    const bytes = Buffer.from(transaction.txCbor, "hex");
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(bytes);
    const subject = acceptedVerdictSubject(
      material.transactionId.toString("hex"),
    );
    decodeMidgardFieldPreimage(material.fieldPreimages[2]!).forEach(
      (_item, outputIndex) => inspect(subject, bytes, BigInt(outputIndex)),
    );
  });
  block.reconstruction.forcedTransactions.forEach((forced) => {
    if (forced.value.verdict === "ForcedTxValid") return;
    const reason = forced.value.verdict.ForcedTxInvalid.reason;
    if (
      typeof reason === "string" ||
      !(
        "OutputReferenceScriptMalformed" in reason ||
        "OutputReferenceScriptNodeLimit" in reason ||
        "OutputReferenceScriptDepthLimit" in reason ||
        ("PlutusScriptUndecodable" in reason &&
          reason.PlutusScriptUndecodable.item_kind ===
            PLUTUS_SCRIPT_ITEM_KIND_REFERENCE_SCRIPT)
      )
    )
      return;
    const outputIndex =
      "PlutusScriptUndecodable" in reason
        ? reason.PlutusScriptUndecodable.item_index
        : "OutputReferenceScriptMalformed" in reason
          ? reason.OutputReferenceScriptMalformed.output_index
          : "OutputReferenceScriptNodeLimit" in reason
            ? reason.OutputReferenceScriptNodeLimit.output_index
            : reason.OutputReferenceScriptDepthLimit.output_index;
    inspect(
      forcedVerdictSubject({
        transactionId: forced.value.tx_id,
        sourceKey: forced.key,
        rejectionReason: reason,
      }),
      forced.fullTransactionCbor,
      outputIndex,
    );
  });
  return Object.freeze(results);
};

/**
 * The violation id of the class a finding closes or accuses. Every class from
 * `PLUTUS_SCRIPT_REFUSED_RESULT_BASE` up is a `PlutusScriptUndecodable` one,
 * the unreachable class a rejection may accuse included.
 */
export const outputReferenceScriptDecodingViolationId = (
  input: bigint | number,
): OutputReferenceScriptDecodingViolationId => {
  const resultClass = BigInt(input);
  if (resultClass >= BigInt(PLUTUS_SCRIPT_REFUSED_RESULT_BASE))
    return OUTPUT_REFERENCE_PLUTUS_SCRIPT_UNDECODABLE_VIOLATION_ID;
  switch (Number(resultClass)) {
    case OutputReferenceScriptResultClasses.Malformed:
      return OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID;
    case OutputReferenceScriptResultClasses.NodeLimit:
      return OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID;
    case OutputReferenceScriptResultClasses.DepthLimit:
      return OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID;
    default:
      return fail("terminal result has no output-reference violation id");
  }
};
