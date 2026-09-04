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
import {
  acceptedVerdictSubject,
  encodeVerdictSubject,
  forcedVerdictSubject,
  hashHexWithBlake2b,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";

export const OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY =
  "outputReferenceScriptDecoding" as const;
export const OUTPUT_REFERENCE_SCRIPT_DECODING_ID = "0000002a" as const;
export const OUTPUT_REFERENCE_SCRIPT_DECODING_FIELD_INDEX = 2 as const;
export const OUTPUT_REFERENCE_SCRIPT_DECODING_MAX_OUTPUT_BYTES = 16_384;
export const OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID =
  "output-reference-script-malformed" as const;
export const OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID =
  "output-reference-script-node-limit" as const;
export const OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID =
  "output-reference-script-depth-limit" as const;
export type OutputReferenceScriptDecodingViolationId =
  | typeof OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID
  | typeof OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID
  | typeof OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID;

export const OutputReferenceScriptResultClasses = Object.freeze({
  Pending: -1,
  NoFault: -2,
  Malformed: 0,
  NodeLimit: 1,
  DepthLimit: 2,
} as const);
export type OutputReferenceScriptResultClass =
  (typeof OutputReferenceScriptResultClasses)[keyof typeof OutputReferenceScriptResultClasses];

const fail = (message: string): never => {
  throw new Error(`${OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY}: ${message}`);
};

const exactOutputIndex = (subject: VerdictSubject): number => {
  const reason = subject.rejection_reason;
  if (reason === null || typeof reason === "string")
    return fail("forced subject has the wrong typed reason");
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
    const outputIndex = Number(payload.output_index);
    if (!Number.isSafeInteger(outputIndex) || outputIndex < 0)
      return fail("typed output coordinate is invalid");
    return outputIndex;
  }
  return fail("forced subject has the wrong typed reason");
};

const accusedClass = (
  subject: VerdictSubject,
): OutputReferenceScriptResultClass => {
  const reason = subject.rejection_reason;
  if (reason === null || typeof reason === "string")
    return fail("typed reason is absent");
  if ("OutputReferenceScriptMalformed" in reason)
    return OutputReferenceScriptResultClasses.Malformed;
  if ("OutputReferenceScriptNodeLimit" in reason)
    return OutputReferenceScriptResultClasses.NodeLimit;
  if ("OutputReferenceScriptDepthLimit" in reason)
    return OutputReferenceScriptResultClasses.DepthLimit;
  return fail("typed reason is outside family");
};

export type OutputReferenceScriptDecodingFinding = Readonly<{
  subject: VerdictSubject;
  outputIndex: number;
}>;

export const classifyOutputReferenceScriptDecodingFinding = (
  finding: OutputReferenceScriptDecodingFinding,
): void => {
  if (!verdictSubjectIsCanonical(finding.subject))
    return fail("subject is not canonical");
  if (!Number.isSafeInteger(finding.outputIndex) || finding.outputIndex < 0)
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
  if (trace.bind.kind === MidgardNativeScriptDecodingBindKinds.NonNative)
    return OutputReferenceScriptResultClasses.NoFault;
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
      accusedClass: OutputReferenceScriptResultClass;
      carriage: "Inline" | "RawUtxo" | "Certified";
      chunkProofCount: number;
      initialControlCbor: string;
    }>;

export const prepareOutputReferenceScriptDecodingEvidence = ({
  subject,
  outputIndex,
  canonicalTransactionCbor,
}: OutputReferenceScriptDecodingFinding & {
  readonly canonicalTransactionCbor: Uint8Array;
}): OutputReferenceScriptDecodingEvidence => {
  classifyOutputReferenceScriptDecodingFinding({ subject, outputIndex });
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
    canonicalTransactionCbor,
  );
  if (material.transactionId.toString("hex") !== subject.transaction_id)
    return fail("transaction identity was substituted");
  const field = material.fieldPreimages[2]!;
  const output = decodeMidgardFieldPreimage(field)[outputIndex];
  if (output === undefined) return fail("output coordinate is out of range");
  if (output.length > OUTPUT_REFERENCE_SCRIPT_DECODING_MAX_OUTPUT_BYTES)
    return fail("output exceeds canonical size bound");
  let trace;
  try {
    trace = buildMidgardLedgerOutputScanTrace(output);
  } catch {
    return fail("selected output descriptor is not canonical");
  }
  const offset = trace.terminal.referenceScriptItemOffset;
  if (trace.terminal.referenceScriptLanguage === -1 || offset < 0)
    return fail("selected output carries no reference script");
  const item = output.subarray(offset);
  const bounded = buildMidgardBoundedItem({
    fieldIndex: 2,
    itemIndex: outputIndex,
    bytes: item,
  });
  for (let index = 0; index < bounded.frontier.count; index += 1)
    buildMidgardBoundedItemChunkProof(bounded, index);
  const resultClass = resultClassOf(item);
  const nativeTrace = buildMidgardNativeScriptDecodingTrace(item);
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
        : OutputReferenceScriptResultClasses.Pending,
    carriage: selectMidgardFieldCarriageTier(field.length),
    chunkProofCount: bounded.frontier.count,
    initialControlCbor:
      nativeTrace.bind.kind === MidgardNativeScriptDecodingBindKinds.Bound
        ? encodeMidgardNativeScriptStructureControl(
            nativeTrace.bind.control,
          ).toString("hex")
        : "",
  });
  if (!outputReferenceScriptEvidenceCloses(evidence))
    return fail("authenticated decoder agrees with operator verdict");
  return evidence;
};

export const outputReferenceScriptEvidenceCloses = (
  evidence: OutputReferenceScriptDecodingEvidence,
): boolean =>
  evidence.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION
    ? evidence.resultClass !== evidence.accusedClass
    : terminalVerdictContradiction(evidence.subject, evidence.resultClass >= 0);

export const outputReferenceScriptControlData = (
  control: MidgardLedgerOutputScanControl,
) => ({
  version: BigInt(control.version),
  stage: BigInt(control.stage),
  cursor: BigInt(control.cursor),
  map_entry_count: BigInt(control.mapEntryCount),
  optional_field_count: BigInt(control.optionalFieldCount),
  address: control.address.toString("hex"),
  lovelace: control.lovelace,
  cardano_value_size: BigInt(control.cardanoValueSize),
  policy_remaining: BigInt(control.policyRemaining),
  asset_remaining: BigInt(control.assetRemaining),
  policy_asset_cursor: BigInt(control.policyAssetCursor),
  previous_policy: control.previousPolicy.toString("hex"),
  current_policy: control.currentPolicy.toString("hex"),
  previous_asset_name: control.previousAssetName.toString("hex"),
  asset_count: BigInt(control.assetFrontier.count),
  asset_peaks: control.assetFrontier.peaks.map(({ height, hash }) => ({
    height: BigInt(height),
    hash: hash.toString("hex"),
  })),
  datum_offset: BigInt(control.datumOffset),
  datum_length: BigInt(control.datumLength),
  payload_remaining: BigInt(control.payloadRemaining),
  reference_script_language: BigInt(control.referenceScriptLanguage),
  reference_script_item_offset: BigInt(control.referenceScriptItemOffset),
  reference_script_offset: BigInt(control.referenceScriptOffset),
  reference_script_length: BigInt(control.referenceScriptLength),
});

const cborInteger = (value: number): Buffer => {
  const major = value < 0 ? 1 : 0;
  const normalized = value < 0 ? -1 - value : value;
  if (normalized < 24) return Buffer.from([(major << 5) | normalized]);
  if (normalized < 256) return Buffer.from([(major << 5) | 24, normalized]);
  const result = Buffer.alloc(3);
  result[0] = (major << 5) | 25;
  result.writeUInt16BE(normalized, 1);
  return result;
};
const cborBytes = (value: Buffer): Buffer =>
  value.length < 24
    ? Buffer.concat([Buffer.from([0x40 | value.length]), value])
    : Buffer.concat([Buffer.from([0x58, value.length]), value]);

/** Exact off-chain twin of `rule.checkpoint_v1`. */
export const outputReferenceScriptCheckpoint = ({
  evidence,
  controlCbor,
  nextExpectedScriptHash,
}: {
  readonly evidence: OutputReferenceScriptDecodingEvidence;
  readonly controlCbor: string;
  readonly nextExpectedScriptHash: string;
}): string =>
  Effect.runSync(
    hashHexWithBlake2b(
      Buffer.concat([
        Buffer.from(
          "midgard/fraud-proofs/output-reference-script-decoding/checkpoint-v1",
          "ascii",
        ),
        encodeVerdictSubject(evidence.subject),
        cborInteger(evidence.outputIndex),
        cborInteger(evidence.accusedClass),
        cborInteger(Buffer.from(evidence.referenceScriptItemHex, "hex").length),
        cborBytes(
          Buffer.from(evidence.referenceScriptItemCommitmentHex, "hex"),
        ),
        cborBytes(Buffer.from(controlCbor, "hex")),
        cborBytes(Buffer.from(nextExpectedScriptHash, "hex")),
      ]).toString("hex"),
      32,
    ),
  );

/** Exhaustive replay over every accepted output and exact forced coordinate. */
export const detectOutputReferenceScriptDecodingCompleteReplay = (
  block: CanonicalBlockEvidence,
): readonly OutputReferenceScriptDecodingEvidence[] => {
  const results: OutputReferenceScriptDecodingEvidence[] = [];
  const inspect = (
    subject: VerdictSubject,
    bytes: Uint8Array,
    outputIndex: number,
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
      (_item, outputIndex) => inspect(subject, bytes, outputIndex),
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
        "OutputReferenceScriptDepthLimit" in reason
      )
    )
      return;
    const subject = forcedVerdictSubject({
      transactionId: forced.value.tx_id,
      sourceKey: forced.key,
      rejectionReason: reason,
    });
    inspect(subject, forced.fullTransactionCbor, exactOutputIndex(subject));
  });
  return Object.freeze(results);
};

export const outputReferenceScriptDecodingViolationId = (
  resultClass: OutputReferenceScriptResultClass,
): OutputReferenceScriptDecodingViolationId => {
  switch (resultClass) {
    case OutputReferenceScriptResultClasses.Malformed:
      return OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID;
    case OutputReferenceScriptResultClasses.NodeLimit:
      return OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID;
    case OutputReferenceScriptResultClasses.DepthLimit:
      return OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID;
    case OutputReferenceScriptResultClasses.Pending:
    case OutputReferenceScriptResultClasses.NoFault:
      return fail("terminal result has no output-reference violation id");
  }
};

/** Central replay route with one exact stable violation id per rejection arm. */
export const detectOutputReferenceScriptDecodingCanonicalViolations = (
  block: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] =>
  detectOutputReferenceScriptDecodingCompleteReplay(block).map((evidence) => {
    const forced =
      evidence.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION;
    const violationId = outputReferenceScriptDecodingViolationId(
      forced ? evidence.accusedClass : evidence.resultClass,
    );
    const position = forced
      ? block.reconstruction.forcedTransactions.findIndex(
          ({ value }) => value.tx_id === evidence.subject.transaction_id,
        )
      : block.transactions.findIndex(
          ({ nodeTxId }) => nodeTxId === evidence.subject.transaction_id,
        );
    if (position < 0)
      return fail("detected subject is absent from authenticated block");
    return Object.freeze({
      detectionId: `${violationId}:${forced ? "forced" : "accepted"}:${position.toString()}:${evidence.subject.transaction_id}:${evidence.outputIndex.toString()}:${evidence.resultClass.toString()}`,
      headerHash: block.headerHash,
      violationId,
      position: BigInt(position),
      diagnostic: forced
        ? `forced transaction ${evidence.subject.transaction_id} was rejected for a decodable output reference script at ${evidence.outputIndex.toString()}`
        : `accepted transaction ${evidence.subject.transaction_id} has an undecodable output reference script at ${evidence.outputIndex.toString()}`,
    });
  });
