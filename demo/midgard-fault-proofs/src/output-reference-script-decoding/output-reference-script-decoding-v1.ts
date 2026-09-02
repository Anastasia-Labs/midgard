import {
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  buildMidgardLedgerOutputScanTraceV1,
  buildMidgardNativeScriptDecodingTraceV1,
  computeHash32,
  decodeMidgardFieldPreimageV1,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  encodeMidgardNativeScriptStructureControlV1,
  type MidgardLedgerOutputScanControlV1,
  MidgardNativeScriptDecodingBindKindsV1,
  MidgardNativeScriptDecodingRefusalClassesV1,
  MidgardNativeScriptDecodingTraceOutcomeKindsV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  encodeVerdictSubjectV1,
  forcedVerdictSubjectV1,
  hashHexWithBlake2b,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  terminalVerdictContradictionV1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import type { CanonicalViolationDetectionV1 } from "../workflow/classification-v1.js";

export const OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_V1 =
  "outputReferenceScriptDecoding" as const;
export const OUTPUT_REFERENCE_SCRIPT_DECODING_ID_V1 = "0000002a" as const;
export const OUTPUT_REFERENCE_SCRIPT_DECODING_FIELD_INDEX_V1 = 2 as const;
export const OUTPUT_REFERENCE_SCRIPT_DECODING_MAX_OUTPUT_BYTES_V1 = 16_384;
export const OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID_V1 =
  "output-reference-script-malformed" as const;
export const OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID_V1 =
  "output-reference-script-node-limit" as const;
export const OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID_V1 =
  "output-reference-script-depth-limit" as const;
export type OutputReferenceScriptDecodingViolationIdV1 =
  | typeof OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID_V1
  | typeof OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID_V1
  | typeof OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID_V1;

export const OutputReferenceScriptResultClassesV1 = Object.freeze({
  Pending: -1,
  NoFault: -2,
  Malformed: 0,
  NodeLimit: 1,
  DepthLimit: 2,
} as const);
export type OutputReferenceScriptResultClassV1 =
  (typeof OutputReferenceScriptResultClassesV1)[keyof typeof OutputReferenceScriptResultClassesV1];

const fail = (message: string): never => {
  throw new Error(
    `${OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_V1}: ${message}`,
  );
};

const exactOutputIndex = (subject: VerdictSubjectV1): number => {
  const reason = subject.rejection_reason;
  if (reason === null || typeof reason === "string")
    return fail("forced subject has the wrong typed reason");
  const entries = [
    [
      "OutputReferenceScriptMalformed",
      OutputReferenceScriptResultClassesV1.Malformed,
    ],
    [
      "OutputReferenceScriptNodeLimit",
      OutputReferenceScriptResultClassesV1.NodeLimit,
    ],
    [
      "OutputReferenceScriptDepthLimit",
      OutputReferenceScriptResultClassesV1.DepthLimit,
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
  subject: VerdictSubjectV1,
): OutputReferenceScriptResultClassV1 => {
  const reason = subject.rejection_reason;
  if (reason === null || typeof reason === "string")
    return fail("typed reason is absent");
  if ("OutputReferenceScriptMalformed" in reason)
    return OutputReferenceScriptResultClassesV1.Malformed;
  if ("OutputReferenceScriptNodeLimit" in reason)
    return OutputReferenceScriptResultClassesV1.NodeLimit;
  if ("OutputReferenceScriptDepthLimit" in reason)
    return OutputReferenceScriptResultClassesV1.DepthLimit;
  return fail("typed reason is outside family");
};

export type OutputReferenceScriptDecodingFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  outputIndex: number;
}>;

export const classifyOutputReferenceScriptDecodingFindingV1 = (
  finding: OutputReferenceScriptDecodingFindingV1,
): void => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    return fail("subject is not canonical");
  if (!Number.isSafeInteger(finding.outputIndex) || finding.outputIndex < 0)
    return fail("output coordinate is invalid");
  if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    if (exactOutputIndex(finding.subject) !== finding.outputIndex)
      return fail("typed reason output coordinate differs");
  } else if (
    finding.subject.direction !==
      PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    finding.subject.rejection_reason !== null
  )
    return fail("subject polarity is invalid");
};

const resultClassOf = (
  item: Uint8Array,
): OutputReferenceScriptResultClassV1 => {
  const trace = buildMidgardNativeScriptDecodingTraceV1(item);
  if (trace.bind.kind === MidgardNativeScriptDecodingBindKindsV1.Malformed)
    return OutputReferenceScriptResultClassesV1.Malformed;
  if (trace.bind.kind === MidgardNativeScriptDecodingBindKindsV1.NonNative)
    return OutputReferenceScriptResultClassesV1.NoFault;
  if (trace.outcome === null)
    return fail("native scan has no terminal outcome");
  if (
    trace.outcome.kind ===
    MidgardNativeScriptDecodingTraceOutcomeKindsV1.Terminal
  )
    return OutputReferenceScriptResultClassesV1.NoFault;
  return trace.outcome.refusalClass ===
    MidgardNativeScriptDecodingRefusalClassesV1.Malformed
    ? OutputReferenceScriptResultClassesV1.Malformed
    : trace.outcome.refusalClass ===
        MidgardNativeScriptDecodingRefusalClassesV1.NodeLimit
      ? OutputReferenceScriptResultClassesV1.NodeLimit
      : OutputReferenceScriptResultClassesV1.DepthLimit;
};

export type OutputReferenceScriptDecodingEvidenceV1 =
  OutputReferenceScriptDecodingFindingV1 &
    Readonly<{
      canonicalTransactionCborHex: string;
      outputFieldPreimageHex: string;
      outputCborHex: string;
      outputLength: number;
      outputHashHex: string;
      outputChunkHashes: readonly string[];
      outputScanControls: readonly MidgardLedgerOutputScanControlV1[];
      referenceScriptItemHex: string;
      referenceScriptItemCommitmentHex: string;
      resultClass: OutputReferenceScriptResultClassV1;
      accusedClass: OutputReferenceScriptResultClassV1;
      carriage: "Inline" | "RawUtxo" | "Certified";
      chunkProofCount: number;
      initialControlCbor: string;
    }>;

export const prepareOutputReferenceScriptDecodingEvidenceV1 = ({
  subject,
  outputIndex,
  canonicalTransactionCbor,
}: OutputReferenceScriptDecodingFindingV1 & {
  readonly canonicalTransactionCbor: Uint8Array;
}): OutputReferenceScriptDecodingEvidenceV1 => {
  classifyOutputReferenceScriptDecodingFindingV1({ subject, outputIndex });
  const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
    canonicalTransactionCbor,
  );
  if (material.transactionId.toString("hex") !== subject.transaction_id)
    return fail("transaction identity was substituted");
  const field = material.fieldPreimages[2]!;
  const output = decodeMidgardFieldPreimageV1(field)[outputIndex];
  if (output === undefined) return fail("output coordinate is out of range");
  if (output.length > OUTPUT_REFERENCE_SCRIPT_DECODING_MAX_OUTPUT_BYTES_V1)
    return fail("output exceeds canonical size bound");
  let trace;
  try {
    trace = buildMidgardLedgerOutputScanTraceV1(output);
  } catch {
    return fail("selected output descriptor is not canonical");
  }
  const offset = trace.terminal.referenceScriptItemOffset;
  if (trace.terminal.referenceScriptLanguage === -1 || offset < 0)
    return fail("selected output carries no reference script");
  const item = output.subarray(offset);
  const bounded = buildMidgardBoundedItemV1({
    fieldIndex: 2,
    itemIndex: outputIndex,
    bytes: item,
  });
  for (let index = 0; index < bounded.frontier.count; index += 1)
    buildMidgardBoundedItemChunkProofV1(bounded, index);
  const resultClass = resultClassOf(item);
  const nativeTrace = buildMidgardNativeScriptDecodingTraceV1(item);
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
      subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
        ? accusedClass(subject)
        : OutputReferenceScriptResultClassesV1.Pending,
    carriage: selectMidgardFieldCarriageTierV1(field.length),
    chunkProofCount: bounded.frontier.count,
    initialControlCbor:
      nativeTrace.bind.kind === MidgardNativeScriptDecodingBindKindsV1.Bound
        ? encodeMidgardNativeScriptStructureControlV1(
            nativeTrace.bind.control,
          ).toString("hex")
        : "",
  });
  if (!outputReferenceScriptEvidenceClosesV1(evidence))
    return fail("authenticated decoder agrees with operator verdict");
  return evidence;
};

export const outputReferenceScriptEvidenceClosesV1 = (
  evidence: OutputReferenceScriptDecodingEvidenceV1,
): boolean =>
  evidence.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
    ? evidence.resultClass !== evidence.accusedClass
    : terminalVerdictContradictionV1(
        evidence.subject,
        evidence.resultClass >= 0,
      );

export const outputReferenceScriptControlDataV1 = (
  control: MidgardLedgerOutputScanControlV1,
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
export const outputReferenceScriptCheckpointV1 = ({
  evidence,
  controlCbor,
  nextExpectedScriptHash,
}: {
  readonly evidence: OutputReferenceScriptDecodingEvidenceV1;
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
        encodeVerdictSubjectV1(evidence.subject),
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
export const detectOutputReferenceScriptDecodingCompleteReplayV1 = (
  block: CanonicalBlockEvidenceV1,
): readonly OutputReferenceScriptDecodingEvidenceV1[] => {
  const results: OutputReferenceScriptDecodingEvidenceV1[] = [];
  const inspect = (
    subject: VerdictSubjectV1,
    bytes: Uint8Array,
    outputIndex: number,
  ): void => {
    try {
      results.push(
        prepareOutputReferenceScriptDecodingEvidenceV1({
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
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(bytes);
    const subject = acceptedVerdictSubjectV1(
      material.transactionId.toString("hex"),
    );
    decodeMidgardFieldPreimageV1(material.fieldPreimages[2]!).forEach(
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
    const subject = forcedVerdictSubjectV1({
      transactionId: forced.value.tx_id,
      sourceKey: forced.key,
      rejectionReason: reason,
    });
    inspect(subject, forced.fullTransactionCbor, exactOutputIndex(subject));
  });
  return Object.freeze(results);
};

export const outputReferenceScriptDecodingViolationIdV1 = (
  resultClass: OutputReferenceScriptResultClassV1,
): OutputReferenceScriptDecodingViolationIdV1 => {
  switch (resultClass) {
    case OutputReferenceScriptResultClassesV1.Malformed:
      return OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID_V1;
    case OutputReferenceScriptResultClassesV1.NodeLimit:
      return OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID_V1;
    case OutputReferenceScriptResultClassesV1.DepthLimit:
      return OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID_V1;
    case OutputReferenceScriptResultClassesV1.Pending:
    case OutputReferenceScriptResultClassesV1.NoFault:
      return fail("terminal result has no output-reference violation id");
  }
};

/** Central replay route with one exact stable violation id per rejection arm. */
export const detectOutputReferenceScriptDecodingCanonicalViolationsV1 = (
  block: CanonicalBlockEvidenceV1,
): readonly CanonicalViolationDetectionV1[] =>
  detectOutputReferenceScriptDecodingCompleteReplayV1(block).map((evidence) => {
    const forced =
      evidence.subject.direction ===
      PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1;
    const violationId = outputReferenceScriptDecodingViolationIdV1(
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
