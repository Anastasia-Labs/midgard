import {
  advanceMidgardLedgerOutputScan,
  buildMidgardBoundedItem,
  buildMidgardLedgerOutputProofTrace,
  decodeMidgardInputFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  finishMidgardLedgerOutputScan,
  initialMidgardLedgerOutputScanControl,
  type MidgardLedgerOutputProofTrace,
  type MidgardLedgerOutputScanControl,
  MidgardLedgerOutputScanStages,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  type Proof,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";

export const RESOLVED_OUTPUT_NON_CANONICAL_CATEGORY =
  "resolvedOutputNonCanonical" as const;

export const RESOLVED_OUTPUT_NON_CANONICAL_ID = "00000026" as const;

export const fail = (message: string): never => {
  throw new Error(`${RESOLVED_OUTPUT_NON_CANONICAL_CATEGORY}: ${message}`);
};

const hex = (value: string, bytes: number, label: string): string => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value))
    return fail(`${label} must be ${bytes.toString()} bytes of lowercase hex`);
  return value;
};

export const index = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} is invalid`);
  return value;
};

export type ResolvedOutputCoordinate = Readonly<{
  sourceKind: 0 | 1;
  inputIndex: number;
}>;

export type AuthenticatedPriorLedgerOutput = Readonly<{
  priorRoot: string;
  transactionId: string;
  outputIndex: number;
  descriptorCborHex: string;
  outputCborHex: string;
  /** Exact MPF proof bytes reconstructed from the retained predecessor DA. */
  membershipProofCborHex: string;
  membershipProof?: Proof;
}>;

export type ResolvedOutputNonCanonicalEvidence = Readonly<{
  subject: VerdictSubject;
  coordinate: ResolvedOutputCoordinate;
  resolved: AuthenticatedPriorLedgerOutput;
  outputIsNonCanonical: boolean;
  canonicalTrace: MidgardLedgerOutputProofTrace | null;
  canonicalTransactionCborHex: string;
  inputFieldPreimageHex: string;
  carriage: "Inline" | "RawUtxo" | "Certified";
  scanControls: readonly MidgardLedgerOutputScanControl[];
}>;

export type ResolvedOutputEvidence = ResolvedOutputNonCanonicalEvidence;

export type ResolvedOutputFinding = Readonly<{
  subject: VerdictSubject;
  coordinate: ResolvedOutputCoordinate;
}>;

export const classifyResolvedOutputFinding = (
  finding: ResolvedOutputFinding,
): ResolvedOutputFinding => {
  classifyResolvedOutputNonCanonicalFinding(finding);
  return Object.freeze(finding);
};

const deriveScanControls = (
  item: Buffer,
): readonly MidgardLedgerOutputScanControl[] => {
  let control = initialMidgardLedgerOutputScanControl();
  const controls = [control];
  for (let step = 0; step <= item.length + 32; step += 1) {
    const finished = finishMidgardLedgerOutputScan({
      control,
      totalLength: item.length,
    });
    if (finished !== null) return Object.freeze([...controls, finished]);
    const chunkStart = Math.floor(control.cursor / 4_095) * 4_095;
    const next = advanceMidgardLedgerOutputScan({
      control,
      totalLength: item.length,
      window: item.subarray(chunkStart, chunkStart + 8_190),
      windowOffset: control.cursor - chunkStart,
    });
    if (next === null) return Object.freeze(controls);
    controls.push(next);
    control = next;
    if (control.stage === MidgardLedgerOutputScanStages.Terminal)
      return Object.freeze(controls);
  }
  return fail("output scan exceeded its strict progress bound");
};

export const resolvedOutputScanControlData = (
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

const exactForcedCoordinate = (
  subject: VerdictSubject,
): ResolvedOutputCoordinate => {
  const reason = subject.rejection_reason;
  if (
    reason === null ||
    typeof reason === "string" ||
    !("InputSpentOutputNonCanonical" in reason)
  ) {
    return fail("forced subject has the wrong typed rejection reason");
  }
  const value = reason.InputSpentOutputNonCanonical;
  const sourceKind = Number(value.source_kind);
  if (sourceKind !== 0 && sourceKind !== 1)
    return fail("reason source kind is invalid");
  return {
    sourceKind,
    inputIndex: index(Number(value.input_index), "reason input index"),
  };
};

export const classifyResolvedOutputNonCanonicalFinding = ({
  subject,
  coordinate,
}: {
  readonly subject: VerdictSubject;
  readonly coordinate: ResolvedOutputCoordinate;
}): void => {
  if (!verdictSubjectIsCanonical(subject))
    return fail("subject is not canonical");
  if (coordinate.sourceKind !== 0 && coordinate.sourceKind !== 1)
    return fail("source kind must select spend or reference inputs");
  index(coordinate.inputIndex, "input index");
  if (subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    const exact = exactForcedCoordinate(subject);
    if (
      exact.sourceKind !== coordinate.sourceKind ||
      exact.inputIndex !== coordinate.inputIndex
    )
      return fail("reason coordinate was substituted");
  } else if (
    subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    subject.rejection_reason !== null
  ) {
    return fail("subject polarity is invalid");
  }
};

/**
 * Authenticates the selected input/out-ref and descriptor commitment before
 * interpreting the retained output. The MPF proof is retained verbatim for
 * step 03; its root/key/value verification is repeated by that validator.
 */
export const prepareResolvedOutputNonCanonicalEvidence = ({
  subject,
  coordinate,
  canonicalTransactionCbor,
  resolved,
}: {
  readonly subject: VerdictSubject;
  readonly coordinate: ResolvedOutputCoordinate;
  readonly canonicalTransactionCbor: Uint8Array;
  readonly resolved: AuthenticatedPriorLedgerOutput;
}): ResolvedOutputNonCanonicalEvidence => {
  classifyResolvedOutputNonCanonicalFinding({ subject, coordinate });
  hex(resolved.priorRoot, 32, "prior root");
  hex(resolved.transactionId, 32, "resolved transaction id");
  index(resolved.outputIndex, "resolved output index");
  const material = (
    subject.source_kind === 1n
      ? deriveMidgardForcedTxFaultEvidenceMaterial
      : deriveMidgardNativeTxFaultEvidenceMaterial
  )(canonicalTransactionCbor);
  if (material.transactionId.toString("hex") !== subject.transaction_id)
    return fail("transaction identity was substituted");
  const field = material.fieldPreimages[coordinate.sourceKind]!;
  const selected =
    decodeMidgardInputFieldPreimage(field)[coordinate.inputIndex];
  if (selected === undefined) return fail("input coordinate is absent");
  if (
    Buffer.from(selected.txId).toString("hex") !== resolved.transactionId ||
    selected.outputIndex !== resolved.outputIndex
  )
    return fail("resolved out-ref differs from the authenticated input item");
  const descriptorBytes = Buffer.from(resolved.descriptorCborHex, "hex");
  const outputBytes = Buffer.from(resolved.outputCborHex, "hex");
  const descriptor = decodeMidgardLedgerOutputCommitment(descriptorBytes);
  const item = buildMidgardBoundedItem({
    fieldIndex: 2,
    itemIndex: resolved.outputIndex,
    bytes: outputBytes,
  });
  if (
    descriptor.outputIndex !== resolved.outputIndex ||
    descriptor.totalLength !== outputBytes.length ||
    !descriptor.itemCommitment.equals(item.commitment)
  )
    return fail("descriptor does not bind the exact resolved output");
  let canonicalTrace: MidgardLedgerOutputProofTrace | null = null;
  let outputIsNonCanonical = false;
  try {
    const rebuilt = buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: resolved.outputIndex,
      outputCbor: outputBytes,
    });
    if (!rebuilt.descriptorCbor.equals(descriptorBytes))
      return fail(
        "canonical output does not reconstruct the retained descriptor",
      );
    canonicalTrace = buildMidgardLedgerOutputProofTrace({
      outputIndex: resolved.outputIndex,
      outputCbor: outputBytes,
    });
  } catch (cause) {
    if (
      cause instanceof Error &&
      cause.message.startsWith(`${RESOLVED_OUTPUT_NON_CANONICAL_CATEGORY}:`)
    )
      throw cause;
    outputIsNonCanonical = true;
  }
  const evidence = Object.freeze({
    subject,
    coordinate,
    resolved,
    outputIsNonCanonical,
    canonicalTrace,
    canonicalTransactionCborHex: Buffer.from(canonicalTransactionCbor).toString(
      "hex",
    ),
    inputFieldPreimageHex:
      material.fieldPreimages[coordinate.sourceKind]!.toString("hex"),
    carriage: selectMidgardFieldCarriageTier(
      material.fieldPreimages[coordinate.sourceKind]!.length,
    ),
    scanControls: deriveScanControls(outputBytes),
  });
  if (!terminalVerdictContradiction(subject, outputIsNonCanonical))
    return fail("authenticated output agrees with the operator verdict");
  return evidence;
};

export const resolvedOutputEvidenceIdentity = (
  evidence: ResolvedOutputNonCanonicalEvidence,
): string =>
  [
    evidence.subject.transaction_id,
    evidence.subject.direction.toString(),
    evidence.coordinate.sourceKind.toString(),
    evidence.coordinate.inputIndex.toString(),
    evidence.resolved.priorRoot,
    evidence.resolved.transactionId,
    evidence.resolved.outputIndex.toString(),
    decodeMidgardLedgerOutputCommitment(
      Buffer.from(evidence.resolved.descriptorCborHex, "hex"),
    ).itemCommitment.toString("hex"),
  ].join(":");

export const resolvedOutputEvidenceCloses = (
  evidence: ResolvedOutputNonCanonicalEvidence,
): boolean =>
  terminalVerdictContradiction(evidence.subject, evidence.outputIsNonCanonical);

export type ResolvedOutputPriorLedgerReplay = Readonly<{
  /** L1-authenticated predecessor root these entries reconstruct. */
  priorRoot: string;
  /** Every resolved input needed by the challenged block, keyed `txid#index`. */
  outputs: ReadonlyMap<
    string,
    Omit<AuthenticatedPriorLedgerOutput, "priorRoot">
  >;
}>;

export const outRefKey = (transactionId: string, outputIndex: number): string =>
  `${transactionId}#${outputIndex.toString()}`;
