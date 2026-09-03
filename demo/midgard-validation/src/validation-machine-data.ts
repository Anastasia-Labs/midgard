import type {
  MidgardBlake2b256TraceControlV1,
  MidgardBoundedItemChunkProofV1,
  MidgardCekDataBytesControlV1,
  MidgardCekDataFrameV1,
  MidgardCekDataIntegerControlV1,
  MidgardCekDataTraverseControlV1,
  MidgardCekSourceBlobControlV1,
  MidgardLedgerOutputProofWitnessV1,
  MidgardMpfProofDescriptorV1,
  MidgardMpfProofFrameV1,
  MidgardMpfProofStepV1,
  MidgardRedeemerItemProofControlV1,
  MidgardRedeemerItemProofWitnessV1,
  MidgardValidationMachineStateV1,
  MidgardValidationMerkleFrontierV1,
  MidgardValidationMerkleMembershipV1,
  MidgardValidationPhaseName,
} from "@al-ft/midgard-core";
import {
  advanceMidgardRedeemerItemProofV1,
  decodeMidgardCekProgramEnvelopeV1,
  decodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardCekProgramEnvelopeV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  hashMidgardCekProgramEnvelopeV1,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  midgardRedeemerItemDescriptorV1,
  verifyMidgardCekProgramMaterialBundleV1,
} from "@al-ft/midgard-core";
import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
  readCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import {
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  type MidgardFieldCarriageV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import { Constr, Data } from "@lucid-evolution/lucid";

import type {
  MidgardCekContextPartsControlV1,
  MidgardCekFinalContextControlV1,
  MidgardCekRedeemerContextControlV1,
  MidgardCekTxInfoAssemblyControlV1,
} from "./cek-context.js";
import { midgardCekCoreStepDataV1 } from "./cek-data.js";
import type {
  MidgardCekDataSequenceSummaryV1,
  MidgardCekDataSummaryV1,
} from "./script-context-proof.js";
import {
  type DeterministicValidationMachineTrace,
  emptyMidgardInputResolutionScheduleV1,
  type ValidationMachineFieldCarriagePlanInputV1,
  type ValidationMachineSignerSetProof,
  type ValidationMachineWorkWitness,
} from "./validation-machine/index.js";

type PlutusData = unknown;
type ConstructorData = Constr<PlutusData>;

const bytes = (value: Uint8Array): string => Buffer.from(value).toString("hex");
const int = (value: number | bigint): bigint => BigInt(value);
const record = (fields: readonly PlutusData[]): ConstructorData =>
  new Constr(0, [...fields]);
const bool = (value: boolean): ConstructorData => new Constr(value ? 1 : 0, []);
const option = <T>(
  value: T | null,
  encode: (exact: T) => PlutusData,
): ConstructorData =>
  value === null ? new Constr(1, []) : new Constr(0, [encode(value)]);
const byteList = (values: readonly Uint8Array[]): readonly string[] =>
  values.map(bytes);

const proofData = (proofCbor: Uint8Array): PlutusData =>
  Data.from(bytes(proofCbor)) as PlutusData;

const frontierPeaksData = (
  frontier: MidgardValidationMerkleFrontierV1,
): readonly ConstructorData[] =>
  frontier.peaks.map((peak) => record([int(peak.height), bytes(peak.hash)]));

const mpfProofStepData = (step: MidgardMpfProofStepV1): ConstructorData => {
  if (step.kind === "branch") {
    return new Constr(0, [int(step.skip), bytes(step.neighbors)]);
  }
  if (step.kind === "fork") {
    return new Constr(1, [
      int(step.skip),
      record([
        int(step.neighbor.nibble),
        bytes(step.neighbor.prefix),
        bytes(step.neighbor.root),
      ]),
    ]);
  }
  return new Constr(2, [int(step.skip), bytes(step.key), bytes(step.value)]);
};

const mpfProofFrameData = (frame: MidgardMpfProofFrameV1): ConstructorData =>
  record([
    int(frame.version),
    int(frame.frameIndex),
    int(frame.cursor),
    int(frame.nextCursor),
    mpfProofStepData(frame.step),
  ]);

const mpfProofDescriptorData = (
  descriptor: MidgardMpfProofDescriptorV1,
): ConstructorData =>
  record([
    int(descriptor.version),
    int(descriptor.frameCount),
    int(descriptor.terminalCursor),
    frontierPeaksData(descriptor.frontier),
  ]);

const ledgerDeltaOperationProofData = (
  descriptor: MidgardMpfProofDescriptorV1,
  membership: MidgardValidationMerkleMembershipV1,
): ConstructorData =>
  record([
    mpfProofDescriptorData(descriptor),
    int(membership.frontier.count),
    frontierPeaksData(membership.frontier),
    int(membership.leafIndex),
    byteList(membership.siblings),
  ]);

/**
 * §8.8 `FieldCarriageV1` — how a field's preimage bytes reach the consuming
 * transaction. Constructor order is frozen consensus wire format and mirrors
 * `onchain/aiken/lib/midgard/native-tx-field-access-v1.ak:168`: `Inline` 0,
 * `RawUtxo` 1, `Certified` 2.
 */
const fieldCarriageData = (carriage: MidgardFieldCarriageV1): PlutusData => {
  switch (carriage.carriage) {
    case "Inline":
      return new Constr(0, [bytes(carriage.preimage)]);
    case "RawUtxo":
      return new Constr(1, [int(carriage.refInputIndex)]);
    case "Certified":
      return new Constr(2, [
        int(carriage.certRefInputIndex),
        carriage.chunkRefInputIndices.map((index) => int(index)),
      ]);
  }
};

/**
 * Turns one step's carriage plan input into the §8 carriage §8.4 admits for it
 * (#600).
 *
 * This is the seam. A trace records *which field a step read*; a carriage says
 * *how those bytes reach the consuming transaction*, and tiers 2–3 answer that
 * with positional reference-input indices §8.7 requires to be resolved by
 * content against a concrete transaction. The committed `evidence_hash` is
 * transition-only (#619), so the tier named here is a delivery decision the
 * observe-stage field door verifies by content — it is never part of what
 * `prepare_semantic_resolution` commits.
 *
 * A resolver is supplied by the dispute submitter, which holds the reference
 * inputs; `resolveMidgardFieldCarriageAgainstReferenceInputsV1` in
 * `@al-ft/midgard-sdk` is the one this repository builds against.
 */
export type ValidationMachineFieldCarriageResolverV1 = (
  planInput: ValidationMachineFieldCarriagePlanInputV1,
) => MidgardFieldCarriageV1;

/**
 * Raised when an auxiliary is encoded without a carriage resolver and §8.4 does
 * not admit tier 1 for the preimage's length.
 *
 * **This is not the retired trace-time refusal.** Nothing refuses while a trace
 * is built — the block-build path depends on that (#600). What refuses is
 * *encoding an auxiliary without the context its carriage needs*: above §8.3's
 * tier-1 cap the carriage is reference-input indices, and a caller that
 * supplied no reference inputs has nothing for them to point at. Emitting tier-1
 * `Inline` anyway would name a carriage §8.4 does not admit for that length, and
 * inventing indices would name references no transaction can satisfy.
 */
export class ValidationMachineCarriageResolutionRequiredErrorV1 extends Error {
  override readonly name = "ValidationMachineCarriageResolutionRequiredErrorV1";
  readonly fieldIndex: number;
  readonly preimageLength: number;
  readonly selectedTier: "RawUtxo" | "Certified";
  readonly maxTier1PreimageBytes: number;

  constructor({
    fieldIndex,
    preimageLength,
    selectedTier,
  }: {
    readonly fieldIndex: number;
    readonly preimageLength: number;
    readonly selectedTier: "RawUtxo" | "Certified";
  }) {
    super(
      `V1 field ${fieldIndex.toString()} has a ${preimageLength.toString()}-byte §5.1 preimage, ` +
        `which §8.4's partition carries as tier-${selectedTier === "RawUtxo" ? "2" : "3"} ` +
        `\`${selectedTier}\` rather than tier-1 \`Inline\` (cap ` +
        `${MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1.toString()} bytes), so its carriage is ` +
        "positional reference-input indices that §8.7 resolves by content against a concrete " +
        "transaction. Encoding this auxiliary requires a carriage resolver built from that " +
        "transaction's complete reference-input set.",
    );
    this.fieldIndex = fieldIndex;
    this.preimageLength = preimageLength;
    this.selectedTier = selectedTier;
    this.maxTier1PreimageBytes = MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1;
  }
}

/**
 * The resolver used when a caller supplies none: tier-1 `Inline` wherever §8.4
 * admits it, and a refusal above, because there is nothing honest to emit.
 *
 * Most callers are inside the tier-1 domain and should not have to think about
 * reference inputs; the ones above it must, and this is what makes that
 * non-optional rather than silently wrong.
 */
export const inlineFieldCarriageResolverV1: ValidationMachineFieldCarriageResolverV1 =
  ({ fieldIndex, fieldPreimage }) => {
    const tier = selectMidgardFieldCarriageTierV1(fieldPreimage.length);
    if (tier !== "Inline") {
      throw new ValidationMachineCarriageResolutionRequiredErrorV1({
        fieldIndex,
        preimageLength: fieldPreimage.length,
        selectedTier: tier,
      });
    }
    return { carriage: "Inline", preimage: Buffer.from(fieldPreimage) };
  };

/**
 * Raised when a caller-supplied resolver returns a carriage whose tier is not
 * the one §8.4's partition admits for the preimage it was asked about.
 *
 * The resolver is the dispute submitter's, and the submitter is not trusted to
 * pick a tier: §8.4 is a *partition*, so the preimage's own length names
 * exactly one admissible carriage and there is no choice to delegate (#597
 * Ruling 1). Without this check the seam would encode whatever came back — a
 * tier-1 `Inline` above §8.3's cap, or an index tier below it — a carriage the
 * observe-stage door's §8.4 partition refuses on-chain, discovered only at
 * submission.
 */
export class ValidationMachineCarriageTierMismatchErrorV1 extends Error {
  override readonly name = "ValidationMachineCarriageTierMismatchErrorV1";
  readonly fieldIndex: number;
  readonly preimageLength: number;
  readonly expectedTier: MidgardFieldCarriageV1["carriage"];
  readonly returnedTier: MidgardFieldCarriageV1["carriage"];

  constructor({
    fieldIndex,
    preimageLength,
    expectedTier,
    returnedTier,
  }: {
    readonly fieldIndex: number;
    readonly preimageLength: number;
    readonly expectedTier: MidgardFieldCarriageV1["carriage"];
    readonly returnedTier: MidgardFieldCarriageV1["carriage"];
  }) {
    super(
      `V1 field ${fieldIndex.toString()} has a ${preimageLength.toString()}-byte §5.1 preimage, ` +
        `which §8.4's partition carries as \`${expectedTier}\`, but the supplied carriage ` +
        `resolver returned \`${returnedTier}\`. §8.4 admits exactly one tier per length, so a ` +
        "resolver names indices for the tier the length selects and never chooses the tier " +
        "itself; encoding this carriage would build a step the observe-stage door's §8.4 " +
        "partition refuses.",
    );
    this.fieldIndex = fieldIndex;
    this.preimageLength = preimageLength;
    this.expectedTier = expectedTier;
    this.returnedTier = returnedTier;
  }
}

/**
 * Raised when a resolver returns tier-1 `Inline` carrying bytes that are not the
 * preimage the step actually read.
 *
 * The tier check above says the resolver picked the right *shape*; this says it
 * carried the right *bytes*. Only tier 1 can get this wrong, because only tier 1
 * carries bytes at all — tiers 2 and 3 carry positional indices, and what is
 * behind those indices is §8.7's content-addressed problem rather than this
 * seam's. An `Inline` preimage that diverges from the trace's own carries bytes
 * the observe-stage door's field-commitment hash refuses: true by a caller's
 * convention rather than by construction, which is the same class as the tier
 * substitution.
 */
export class ValidationMachineCarriagePreimageSubstitutedErrorV1 extends Error {
  override readonly name =
    "ValidationMachineCarriagePreimageSubstitutedErrorV1";
  readonly fieldIndex: number;
  readonly preimageLength: number;
  readonly returnedPreimageLength: number;

  constructor({
    fieldIndex,
    preimageLength,
    returnedPreimageLength,
  }: {
    readonly fieldIndex: number;
    readonly preimageLength: number;
    readonly returnedPreimageLength: number;
  }) {
    super(
      `V1 field ${fieldIndex.toString()} read a ${preimageLength.toString()}-byte §5.1 preimage, ` +
        `but the supplied carriage resolver returned tier-1 \`Inline\` carrying ` +
        `${returnedPreimageLength.toString()} substituted bytes. A resolver decides how a step's ` +
        "preimage travels, never which bytes they are; carrying these bytes would build a step " +
        "the observe-stage door's field-commitment hash refuses.",
    );
    this.fieldIndex = fieldIndex;
    this.preimageLength = preimageLength;
    this.returnedPreimageLength = returnedPreimageLength;
  }
}

/**
 * The seam itself: resolve one step's carriage, then check the answer against
 * §8.4's partition — and, at tier 1, against the step's own bytes — before it
 * can reach `evidence_hash`.
 *
 * Every field-reading auxiliary arm goes through here rather than calling the
 * resolver directly, so the invariant is structural — a misbehaving or merely
 * stale resolver cannot put an inadmissible tier on the wire at any one of the
 * arms while the others are checked.
 *
 * The byte comparison applies to tier 1 and to nothing else: tiers 2 and 3 carry
 * reference-input indices rather than bytes, so there is no preimage here to
 * disagree with. What is behind those indices is §8.7 content addressing, which
 * the door resolves and `assertMidgardFieldCarriageResolvesAtDoorV1` guards.
 */
const resolvedFieldCarriageData = (
  resolveFieldCarriage: ValidationMachineFieldCarriageResolverV1,
  planInput: ValidationMachineFieldCarriagePlanInputV1,
): PlutusData => {
  const carriage = resolveFieldCarriage(planInput);
  const expectedTier = selectMidgardFieldCarriageTierV1(
    planInput.fieldPreimage.length,
  );
  if (carriage.carriage !== expectedTier) {
    throw new ValidationMachineCarriageTierMismatchErrorV1({
      fieldIndex: planInput.fieldIndex,
      preimageLength: planInput.fieldPreimage.length,
      expectedTier,
      returnedTier: carriage.carriage,
    });
  }
  if (
    carriage.carriage === "Inline" &&
    !carriage.preimage.equals(planInput.fieldPreimage)
  ) {
    throw new ValidationMachineCarriagePreimageSubstitutedErrorV1({
      fieldIndex: planInput.fieldIndex,
      preimageLength: planInput.fieldPreimage.length,
      returnedPreimageLength: carriage.preimage.length,
    });
  }
  return fieldCarriageData(carriage);
};

const chunkProofData = (
  proof: MidgardBoundedItemChunkProofV1,
): ConstructorData =>
  record([
    int(proof.version),
    int(proof.fieldIndex),
    int(proof.itemIndex),
    int(proof.totalLength),
    int(proof.chunkIndex),
    bytes(proof.chunk),
    frontierPeaksData(proof.frontier),
    byteList(proof.siblings),
  ]);

const signerProofData = (
  proof: ValidationMachineSignerSetProof,
): ConstructorData => {
  switch (proof.kind) {
    case "none":
      return new Constr(0, []);
    case "membership":
      return new Constr(1, [
        frontierPeaksData(proof.frontier),
        int(proof.signerIndex),
        byteList(proof.siblings),
      ]);
    case "empty":
      return new Constr(2, [frontierPeaksData(proof.frontier)]);
    case "belowFirst":
      return new Constr(3, [
        frontierPeaksData(proof.frontier),
        bytes(proof.firstSignerHash),
        byteList(proof.siblings),
      ]);
    case "aboveLast":
      return new Constr(4, [
        frontierPeaksData(proof.frontier),
        bytes(proof.lastSignerHash),
        byteList(proof.siblings),
      ]);
    case "between":
      return new Constr(5, [
        frontierPeaksData(proof.frontier),
        int(proof.lowerIndex),
        bytes(proof.lowerSignerHash),
        byteList(proof.lowerSiblings),
        bytes(proof.upperSignerHash),
        byteList(proof.upperSiblings),
      ]);
  }
};

const summaryData = (summary: MidgardCekDataSummaryV1): ConstructorData =>
  record([bytes(summary.root), summary.cborLength, summary.memory]);

const sequenceSummaryData = (
  summary: MidgardCekDataSequenceSummaryV1,
): ConstructorData =>
  record([
    bytes(summary.root),
    summary.length,
    summary.payloadCborLength,
    summary.memory,
  ]);

const dataTraverseFrameData = (
  frame: MidgardCekDataFrameV1,
): ConstructorData => {
  const kind =
    frame.kind === "constrSmall"
      ? 0
      : frame.kind === "constrLarge"
        ? 1
        : frame.kind === "list"
          ? 2
          : 3;
  const constructor = frame.kind === "constrSmall" ? frame.constructor : 0n;
  const constructorCborRoot =
    frame.kind === "constrLarge" ? frame.constructorCborRoot : Buffer.alloc(0);
  const constructorCborLength =
    frame.kind === "constrLarge" ? frame.constructorCborLength : 0n;
  const constructorMemory =
    frame.kind === "constrLarge" ? frame.constructorMemory : 0n;
  return record([
    int(kind),
    constructor,
    bytes(constructorCborRoot),
    constructorCborLength,
    constructorMemory,
    bytes(frame.tail),
    int(frame.expectedChildren),
    int(frame.childCount),
    frontierPeaksData(frame.childFrontier),
    int(frame.foldCursor),
    sequenceSummaryData(frame.sequence),
  ]);
};

const dataTraverseActionData = (
  action: Extract<
    MidgardLedgerOutputProofWitnessV1,
    { readonly kind: "datum" }
  >["action"],
): ConstructorData => {
  if (action === null) return new Constr(0, []);
  switch (action.kind) {
    case "headScalar":
      return new Constr(1, [int(action.itemLength)]);
    case "headSequence":
      return new Constr(2, [int(action.expectedChildren)]);
    case "headMap":
      return new Constr(3, []);
    case "headLargeConstructor":
      return new Constr(4, [
        int(action.constructorCborLength),
        int(action.expectedChildren),
      ]);
    case "attachScalar":
      return new Constr(5, [option(action.parent, dataTraverseFrameData)]);
    case "foldList":
      return new Constr(6, [
        dataTraverseFrameData(action.frame),
        int(action.childIndex),
        summaryData(action.child),
        byteList(action.siblings),
      ]);
    case "foldMap":
      return new Constr(7, [
        dataTraverseFrameData(action.frame),
        int(action.pairIndex),
        summaryData(action.key),
        summaryData(action.value),
        byteList(action.keySiblings),
        byteList(action.valueSiblings),
      ]);
    case "finalizeFrame":
      return new Constr(8, [
        dataTraverseFrameData(action.frame),
        option(action.parent, dataTraverseFrameData),
      ]);
  }
};

const ledgerOutputProofWitnessData = (
  witness: MidgardLedgerOutputProofWitnessV1,
): ConstructorData => {
  if (witness === null) return new Constr(0, []);
  switch (witness.kind) {
    case "chunks":
      return new Constr(1, [
        chunkProofData(witness.chunkProof),
        option(witness.nextChunkProof, chunkProofData),
      ]);
    case "value":
      return new Constr(2, [
        bytes(witness.policyId),
        bytes(witness.assetName),
        witness.quantity,
        byteList(witness.siblings),
      ]);
    case "datum":
      return new Constr(3, [
        dataTraverseActionData(witness.action),
        option(witness.chunkProof, chunkProofData),
        option(witness.nextChunkProof, chunkProofData),
      ]);
    case "nativeFrame":
      return new Constr(4, [
        record([
          bytes(witness.frame.tail),
          int(witness.frame.kind),
          int(witness.frame.childCount),
          int(witness.frame.remaining),
          int(witness.frame.validCount),
          witness.frame.required,
        ]),
      ]);
  }
};

const redeemerControlData = (
  control: MidgardCekRedeemerContextControlV1,
): ConstructorData =>
  record([
    int(control.cursor),
    sequenceSummaryData(control.mapItems),
    bytes(control.activeScanHash),
    bytes(control.activeRedeemerLeaf),
    summaryData(control.activePurpose),
    summaryData(control.currentRedeemer),
  ]);

const finalContextControlData = (
  control: MidgardCekFinalContextControlV1,
): ConstructorData =>
  record([
    summaryData(control.txInfo),
    summaryData(control.redeemer),
    summaryData(control.scriptInfo),
  ]);

const contextPartsControlData = (
  control: MidgardCekContextPartsControlV1,
): ConstructorData =>
  record([
    sequenceSummaryData(control.redeemerItems),
    summaryData(control.redeemer),
    summaryData(control.scriptInfo),
  ]);

const txInfoAssemblyControlData = (
  control: MidgardCekTxInfoAssemblyControlV1,
): ConstructorData =>
  record([
    sequenceSummaryData(control.tailFields),
    summaryData(control.redeemer),
    summaryData(control.scriptInfo),
  ]);

const redeemerItemControlData = (
  control: MidgardRedeemerItemProofControlV1,
): ConstructorData => {
  const blake2b256ControlData = (
    hash: MidgardBlake2b256TraceControlV1,
  ): ConstructorData =>
    record([
      int(hash.version),
      int(hash.stage),
      int(hash.cursor),
      int(hash.totalLength),
      bytes(hash.chainingValue),
      bytes(hash.activeBlock),
      int(hash.activeBlockLength),
      bytes(hash.workingValue),
      int(hash.round),
    ]);
  const sourceBlobData = (
    blob: MidgardCekSourceBlobControlV1,
  ): ConstructorData =>
    record([
      int(blob.version),
      int(blob.stage),
      int(blob.sourceStart),
      int(blob.sourceLength),
      record([
        int(blob.frontier.count),
        blob.frontier.byteLength,
        blob.frontier.peaks.map((peak) =>
          record([int(peak.height), bytes(peak.root), peak.byteLength]),
        ),
      ]),
      option(blob.activeHash, blake2b256ControlData),
    ]);
  const integerControlData = (
    integer: MidgardCekDataIntegerControlV1,
  ): ConstructorData =>
    record([
      int(integer.version),
      int(integer.stage),
      int(integer.sourceStart),
      int(integer.sourceLength),
      integer.memory,
      option(integer.blob, sourceBlobData),
    ]);
  const bytesControlData = (
    byteControl: MidgardCekDataBytesControlV1,
  ): ConstructorData =>
    record([
      int(byteControl.version),
      int(byteControl.stage),
      int(byteControl.sourceStart),
      int(byteControl.sourceLength),
      int(byteControl.bytesLength),
      option(byteControl.blob, sourceBlobData),
    ]);
  const traversalData = (
    traversal: MidgardCekDataTraverseControlV1,
  ): ConstructorData =>
    record([
      int(traversal.version),
      int(traversal.stage),
      int(traversal.sourceStart),
      int(traversal.sourceLength),
      int(traversal.offset),
      bytes(traversal.frameRoot),
      option(traversal.pendingLargeExpectedChildren, int),
      option(traversal.integer, integerControlData),
      option(traversal.bytes, bytesControlData),
      option(traversal.result, summaryData),
    ]);
  return record([
    int(control.version),
    int(control.mode),
    int(control.stage),
    int(control.itemIndex),
    int(control.itemCount),
    int(control.totalLength),
    bytes(control.itemCommitment),
    int(control.expectedPurposeTag),
    int(control.expectedPointerIndex),
    int(control.purposeTag),
    int(control.pointerIndex),
    int(control.dataOffset),
    int(control.dataLength),
    control.executionMemory,
    control.executionSteps,
    option(control.traversal, traversalData),
  ]);
};

const redeemerItemProofWitnessData = (
  witness: MidgardRedeemerItemProofWitnessV1,
): ConstructorData => {
  const action =
    witness.action.kind === "openHeader"
      ? new Constr(0, [])
      : witness.action.kind === "openTail"
        ? new Constr(1, [])
        : witness.action.kind === "traverseData"
          ? new Constr(2, [dataTraverseActionData(witness.action.action)])
          : new Constr(3, []);
  return record([
    action,
    option(witness.chunkProof, chunkProofData),
    option(witness.nextChunkProof, chunkProofData),
  ]);
};

const valueMutationData = (
  mutation: Extract<
    NonNullable<ValidationMachineWorkWitness["auxiliary"]>,
    {
      readonly kind: "valueInputAsset" | "valueOutputAsset" | "valueMintAsset";
    }
  >["mutationStep"],
): ConstructorData =>
  record([
    bool(mutation.oldDelta !== null),
    mutation.oldDelta ?? 0n,
    proofData(mutation.proofCbor),
  ]);

const sourceKind = (kind: "spend" | "reference"): bigint =>
  kind === "spend" ? 0n : 1n;
const originKind = (kind: "inline" | "reference"): bigint =>
  kind === "inline" ? 0n : 1n;

const resolverPhaseIndex = (phase: MidgardValidationPhaseName): number => {
  const index = {
    canonicalDecode: 0,
    compactBinding: 1,
    staticLedgerRules: 2,
    inputSets: 3,
    signatures: 4,
    phaseANativeScripts: 5,
    phaseAScriptPreconditions: 6,
    resolveInputs: 7,
    scriptSources: 8,
    nativeScripts: 9,
    scriptIntegrity: 10,
    cek: 11,
    valueAndMint: 12,
    ledgerDelta: 13,
    terminal: -1,
  }[phase];
  if (index < 0) {
    throw new Error(`validation phase ${phase} has no resolver`);
  }
  return index;
};

const scanStage = (
  witness: ValidationMachineWorkWitness,
  label: string,
): number => {
  const outer = readCborArrayHeader(witness.cbor, 0, label);
  if (outer.length < 6) {
    throw new Error(`${label} control has too few fields`);
  }
  let offset = outer.nextOffset;
  for (let index = 0; index < 5; index += 1) {
    offset = readCborBytes(
      witness.cbor,
      offset,
      `${label}.binding_${index.toString()}`,
    ).nextOffset;
  }
  const stage = readCborInteger(witness.cbor, offset, `${label}.stage`).value;
  const exact = Number(stage);
  if (!Number.isSafeInteger(exact) || exact < 0) {
    throw new Error(`${label} stage is invalid`);
  }
  return exact;
};

const scriptSourcesControlStatus = (
  witness: ValidationMachineWorkWitness,
): {
  readonly stage: number;
  readonly pendingHashStage: number | null;
} => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "script_sources_control",
  );
  if (control.length !== 30 && control.length !== 31) {
    throw new Error("script_sources_control has an invalid field count");
  }
  const stage = Number(asBigInt(control[9], "script_sources_control.stage"));
  if (!Number.isSafeInteger(stage) || stage < 0) {
    throw new Error("script_sources_control stage is invalid");
  }
  if (stage !== 0 || control.length === 30) {
    return { stage, pendingHashStage: null };
  }
  const pendingCbor = asBytes(
    control[30],
    "script_sources_control.pending_source",
  );
  if (pendingCbor.length === 0) {
    throw new Error("script_sources_control pending source is empty");
  }
  const pending = asArray(
    decodeSingleCbor(pendingCbor),
    "script_sources_pending_source",
  );
  if (
    pending.length !== 9 ||
    asBigInt(pending[0], "script_sources_pending_source.version") !== 1n
  ) {
    throw new Error("script_sources_control pending source is invalid");
  }
  const hashControlCbor = asBytes(
    pending[8],
    "script_sources_pending_source.hash_control",
  );
  const hashControl = readCborArrayHeader(
    hashControlCbor,
    0,
    "script_sources_pending_source.hash_control",
  );
  const hashVersion = readCborInteger(
    hashControlCbor,
    hashControl.nextOffset,
    "script_sources_pending_source.hash_control.version",
  );
  const hashStage = readCborInteger(
    hashControlCbor,
    hashVersion.nextOffset,
    "script_sources_pending_source.hash_control.stage",
  );
  if (hashControl.length !== 9 || hashVersion.value !== 1n) {
    throw new Error("script_sources pending hash control is invalid");
  }
  const pendingHashStage = Number(hashStage.value);
  if (
    !Number.isSafeInteger(pendingHashStage) ||
    pendingHashStage < 0 ||
    pendingHashStage > 3
  ) {
    throw new Error("script_sources pending hash stage is invalid");
  }
  return { stage, pendingHashStage };
};

const scriptSourcesDiscoveryCurrentScriptHash = (
  witness: ValidationMachineWorkWitness,
): Buffer => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "script_sources_control",
  );
  if (
    control.length !== 31 ||
    asBigInt(control[9], "script_sources_control.stage") !== 9n
  ) {
    throw new Error("script_sources_control is not at discovery stage 9");
  }
  const discovery = asArray(
    decodeSingleCbor(asBytes(control[30], "script_sources_control.discovery")),
    "script_sources_discovery",
  );
  if (discovery.length !== 15) {
    throw new Error("script_sources discovery has an invalid field count");
  }
  const scriptHash = Buffer.from(
    asBytes(discovery[5], "script_sources_discovery.current_script_hash"),
  );
  if (scriptHash.length !== 28) {
    throw new Error(
      "script_sources discovery current script hash has an invalid length",
    );
  }
  return scriptHash;
};

const scriptSourcesDiscoveryCurrentPurpose = (
  witness: ValidationMachineWorkWitness,
): {
  readonly purposeKind: 0 | 1 | 2 | 3;
  readonly purposeIndex: bigint;
} => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "script_sources_control",
  );
  if (
    control.length !== 31 ||
    asBigInt(control[9], "script_sources_control.stage") !== 10n
  ) {
    throw new Error("script_sources_control is not at discovery stage 10");
  }
  const discovery = asArray(
    decodeSingleCbor(asBytes(control[30], "script_sources_control.discovery")),
    "script_sources_discovery",
  );
  if (discovery.length !== 15) {
    throw new Error("script_sources discovery has an invalid field count");
  }
  const purposeKind = Number(
    asBigInt(discovery[3], "script_sources_discovery.current_purpose_kind"),
  );
  if (
    purposeKind !== 0 &&
    purposeKind !== 1 &&
    purposeKind !== 2 &&
    purposeKind !== 3
  ) {
    throw new Error("script_sources discovery current purpose kind is invalid");
  }
  return {
    purposeKind,
    purposeIndex: asBigInt(
      discovery[4],
      "script_sources_discovery.current_purpose_index",
    ),
  };
};

const scriptIntegrityStage = (
  witness: ValidationMachineWorkWitness,
): number => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "script_integrity_control",
  );
  if (control.length !== 2 && control.length !== 4) {
    throw new Error("script_integrity_control has an invalid field count");
  }
  const stage = Number(asBigInt(control[1], "script_integrity_control.stage"));
  if (
    !Number.isSafeInteger(stage) ||
    stage < 0 ||
    stage > 3 ||
    (stage < 2 && control.length !== 2) ||
    (stage >= 2 && control.length !== 4)
  ) {
    throw new Error("script_integrity_control stage is invalid");
  }
  return stage;
};

const resolveInputsCursor = (witness: ValidationMachineWorkWitness): number => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "resolve_inputs_control",
  );
  if (control.length !== 11) {
    throw new Error("resolve_inputs_control has an invalid field count");
  }
  const cursor = Number(asBigInt(control[4], "resolve_inputs_control.cursor"));
  if (!Number.isSafeInteger(cursor) || cursor < 0) {
    throw new Error("resolve_inputs_control cursor is invalid");
  }
  return cursor;
};

const ledgerDeltaControlStatus = (
  witness: ValidationMachineWorkWitness,
): {
  readonly stage: number;
  readonly pendingStage: number | null;
} => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "ledger_delta_control",
  );
  if (control.length !== 14) {
    throw new Error("ledger_delta_control has an invalid field count");
  }
  const stage = Number(asBigInt(control[4], "ledger_delta_control.stage"));
  if (!Number.isSafeInteger(stage) || stage < 0 || stage > 2) {
    throw new Error("ledger_delta_control stage is invalid");
  }
  const pendingCbor = asBytes(
    control[12],
    "ledger_delta_control.pending_mutation",
  );
  if (pendingCbor.length === 0) {
    return { stage, pendingStage: null };
  }
  const pending = asArray(
    decodeSingleCbor(pendingCbor),
    "ledger_delta_pending_mutation",
  );
  if (
    pending.length !== 10 ||
    asBigInt(pending[0], "ledger_delta_pending_mutation.version") !== 1n
  ) {
    throw new Error("ledger_delta pending mutation is invalid");
  }
  const pendingStage = Number(
    asBigInt(pending[1], "ledger_delta_pending_mutation.stage"),
  );
  if (pendingStage !== 0 && pendingStage !== 1) {
    throw new Error("ledger_delta pending mutation stage is invalid");
  }
  return { stage, pendingStage };
};

/**
 * The four cek semantic resolvers, in their `semantic_resolver_script_hashes`
 * order under the `cek_v1` prepare validator (lib `verify_cek`): the
 * ValueAndMint hand-off (`cek_finish_semantic_v1`), the execution selection
 * (`cek_execution_selection_semantic_v1`), the context step
 * (`cek_context_step_semantic_v1`) and the core step
 * (`cek_core_step_semantic_v1`).
 */
export type CekStepKindV1 = "finish" | "selection" | "context" | "core";

/**
 * The cek work witness is the nine-field list
 * `[native_control, context_control, execution_cursor, completed_cpu,
 * completed_memory, active_state_hash, program_envelope_hash,
 * execution_cpu_limit, execution_memory_limit]` that
 * `encodeMidgardCekValidationWitnessV1` writes and the on-chain
 * `cek_witness_control_v1` decodes. The four cek semantic resolvers partition
 * the step space on the control alone, in the order the on-chain
 * discriminators are consulted (`cek_control_is_core_step_v1`,
 * `cek_control_is_context_step_v1`, `cek_control_is_finish_v1`, else the
 * execution selection): a core step carries an active state, a context step
 * carries a context control and no active state, and a step with neither is
 * the ValueAndMint hand-off when no execution is left to select (cursor at
 * the execution count) and an execution selection otherwise. The language
 * bitmap takes no part in the discrimination: a transaction whose executions
 * are all native carries `language_bitmap == 0` and still has one selection
 * step per execution, exactly as the machine emits them (#629). The
 * auxiliary is not consulted: each on-chain semantic
 * resolver `expect`s the auxiliary shape of its own kind (none, a
 * `NativeExecutionScanWitness`, a cek context witness, a
 * `CekCoreStepWitness`), so a witness whose auxiliary does not match the kind
 * its control names is refused at the submission encoder, exactly as the
 * resolver would refuse it. Note that a Plutus trace never emits a `finish`
 * step: the hand-off to ValueAndMint is claimed by the last core step (or the
 * last native selection) itself, and `finish` is the stand-alone hand-off of
 * a trace with nothing left to select.
 */
export const cekKindV1 = (
  witness: ValidationMachineWorkWitness,
): CekStepKindV1 => {
  const control = asArray(decodeSingleCbor(witness.cbor), "cek_witness");
  if (control.length !== 9) {
    throw new Error("cek_witness has an invalid field count");
  }
  const nativeControl = asArray(
    decodeSingleCbor(asBytes(control[0], "cek_witness.native_control")),
    "cek_witness.native_control",
  );
  if (nativeControl.length !== 26) {
    throw new Error("cek_witness native control has an invalid field count");
  }
  const executionCount = asBigInt(
    nativeControl[21],
    "cek_witness.native_control.execution_count",
  );
  const executionCursor = asBigInt(control[2], "cek_witness.execution_cursor");
  const hasContextControl =
    asBytes(control[1], "cek_witness.context_control").length > 0;
  const hasActiveState =
    asBytes(control[5], "cek_witness.active_state_hash").length > 0;
  const selectionExhausted = executionCursor === executionCount;
  if (hasActiveState) {
    return "core";
  }
  if (hasContextControl) {
    return "context";
  }
  return selectionExhausted ? "finish" : "selection";
};

/**
 * The eleven ValueAndMint semantic resolvers, in their
 * `semantic_resolver_script_hashes` order under the `value_and_mint_v1`
 * prepare validator (lib `verify_value_and_mint`), one per reachable
 * `(stage, auxiliary)` pair of the stage bodies.
 */
export type ValueAndMintStepKindV1 =
  | "begin"
  | "replayBegin"
  | "replayInput"
  | "replayAsset"
  | "replayFinish"
  | "outputDescriptor"
  | "outputAsset"
  | "outputFinish"
  | "mintAsset"
  | "mintFinish"
  | "finalize";

/**
 * `ValueAndMintControlV1` is the twelve-field list the machine writes for
 * every ValueAndMint step; its stage (field 1) and the cursor facts the stage
 * bodies branch on select the semantic resolver, exactly as each on-chain
 * `verify_value_and_mint_<kind>_semantics_v1` pins them (stage, then the
 * remaining replay schedule / replay-asset cursor for stage 2, the output and
 * output-asset cursors for stage 3 and the mint cursor for stage 4). The
 * auxiliary is not consulted: every kind's resolver reconstructs the
 * auxiliary of its own shape from its action fields, so a witness whose
 * auxiliary does not match the kind its control names is refused at the
 * submission encoder, exactly as the resolver would refuse it.
 */
export const valueAndMintKindV1 = (
  witness: ValidationMachineWorkWitness,
): ValueAndMintStepKindV1 => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "value_and_mint_control",
  );
  if (control.length !== 12) {
    throw new Error("value_and_mint_control has an invalid field count");
  }
  const nativeControl = asArray(
    decodeSingleCbor(
      asBytes(control[0], "value_and_mint_control.native_control"),
    ),
    "value_and_mint_control.native_control",
  );
  if (nativeControl.length !== 26) {
    throw new Error(
      "value_and_mint_control native control has an invalid field count",
    );
  }
  const stage = Number(asBigInt(control[1], "value_and_mint_control.stage"));
  if (!Number.isSafeInteger(stage) || stage < 0 || stage > 5) {
    throw new Error("value_and_mint_control stage is invalid");
  }
  switch (stage) {
    case 0:
      return "begin";
    case 1:
      return "replayBegin";
    case 2: {
      const remainingScheduleEmpty = asBytes(
        control[7],
        "value_and_mint_control.replay_remaining_schedule_hash",
      ).equals(emptyMidgardInputResolutionScheduleV1());
      if (remainingScheduleEmpty) {
        return "replayFinish";
      }
      const replayAssetCursor = asBigInt(
        control[4],
        "value_and_mint_control.replay_asset_cursor",
      );
      return replayAssetCursor === 0n ? "replayInput" : "replayAsset";
    }
    case 3: {
      const outputCursor = asBigInt(
        control[8],
        "value_and_mint_control.output_cursor",
      );
      const outputCount = asBigInt(
        nativeControl[16],
        "value_and_mint_control.native_control.output_count",
      );
      if (outputCursor === outputCount) {
        return "outputFinish";
      }
      const outputAssetCursor = asBigInt(
        control[9],
        "value_and_mint_control.output_asset_cursor",
      );
      return outputAssetCursor === 0n ? "outputDescriptor" : "outputAsset";
    }
    case 4: {
      const mintCursor = asBigInt(
        control[10],
        "value_and_mint_control.mint_cursor",
      );
      const mintCount = asBigInt(
        nativeControl[19],
        "value_and_mint_control.native_control.mint_count",
      );
      return mintCursor === mintCount ? "mintFinish" : "mintAsset";
    }
    default:
      return "finalize";
  }
};

const nativeScanCursor = (
  witness: ValidationMachineWorkWitness,
): {
  readonly stage: number;
  readonly cursor: number;
} => {
  const control = asArray(
    Data.from(Buffer.from(witness.cbor).toString("hex")),
    "phase_a_native_control",
  );
  if (control.length !== 18) {
    throw new Error("phase-A native control has an invalid field count");
  }
  const exactStage = Number(
    asBigInt(control[5], "phase_a_native_control.stage"),
  );
  const exactCursor = Number(
    asBigInt(control[11], "phase_a_native_control.cursor"),
  );
  if (
    !Number.isSafeInteger(exactStage) ||
    exactStage < 0 ||
    !Number.isSafeInteger(exactCursor) ||
    exactCursor < 0
  ) {
    throw new Error("phase-A native control stage or cursor is invalid");
  }
  return { stage: exactStage, cursor: exactCursor };
};

const nativePayloadChildCount = ({
  witness,
  cursor,
  stage,
}: {
  readonly witness: Extract<
    NonNullable<ValidationMachineWorkWitness["auxiliary"]>,
    { readonly kind: "nativeScriptToken" }
  >;
  readonly cursor: number;
  readonly stage: number;
}): number => {
  const expectedChunkIndex = Math.floor(
    cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  );
  if (witness.chunkProof.chunkIndex !== expectedChunkIndex) {
    throw new Error(
      "phase-A native token proof does not cover the committed cursor",
    );
  }
  const window = Buffer.concat([
    witness.chunkProof.chunk,
    witness.nextChunkProof?.chunk ?? Buffer.alloc(0),
  ]);
  let offset =
    cursor - expectedChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1;
  if (stage === 6) {
    offset = readCborUnsigned(
      window,
      offset,
      "phase_a_native_payload.required",
    ).nextOffset;
  }
  const children = readCborArrayHeader(
    window,
    offset,
    "phase_a_native_payload.children",
  );
  return children.length;
};

export const validationSemanticResolverIndexV1 = (
  witness: ValidationMachineWorkWitness,
): number => {
  const auxiliary = witness.auxiliary;
  switch (witness.phase) {
    case "canonicalDecode":
      if (auxiliary === null) return 0;
      if (
        auxiliary.kind === "transactionFieldChunk" ||
        auxiliary.kind === "transactionFieldItem"
      ) {
        return 1;
      }
      break;
    case "compactBinding":
    case "staticLedgerRules":
      if (auxiliary === null) return 0;
      break;
    case "inputSets":
      if (auxiliary === null) return 0;
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      break;
    case "signatures":
      if (auxiliary === null) {
        return scanStage(witness, "signatures_control") === 2 ? 3 : 0;
      }
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      if (auxiliary.kind === "requiredSignerItem") return 2;
      break;
    case "phaseANativeScripts": {
      if (auxiliary === null) return 0;
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      if (auxiliary.kind === "nativeScriptFrame") return 13;
      if (auxiliary.kind !== "nativeScriptToken") break;
      const { stage, cursor } = nativeScanCursor(witness);
      if (stage === 1) return 2;
      if (stage === 3) {
        return {
          none: -1,
          membership: 8,
          empty: 9,
          belowFirst: 10,
          aboveLast: 11,
          between: 12,
        }[auxiliary.signerProof.kind];
      }
      if (stage === 4 || stage === 5) {
        return nativePayloadChildCount({
          witness: auxiliary,
          cursor,
          stage,
        }) > 0
          ? 3
          : 4;
      }
      if (stage === 6) {
        return nativePayloadChildCount({
          witness: auxiliary,
          cursor,
          stage,
        }) > 0
          ? 5
          : 6;
      }
      if (stage === 7 || stage === 8) return 7;
      break;
    }
    case "phaseAScriptPreconditions":
      if (auxiliary === null) return 0;
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      break;
    case "resolveInputs":
      if (auxiliary === null) {
        return resolveInputsCursor(witness) === 0 ? 0 : 1;
      }
      if (auxiliary.kind === "scheduledLedgerLookup") {
        return auxiliary.value === null ? 5 : 2;
      }
      if (auxiliary.kind === "ledgerOutputProofStep") return 3;
      if (auxiliary.kind === "ledgerOutputProofFinalize") return 4;
      break;
    case "scriptSources": {
      const { stage, pendingHashStage } = scriptSourcesControlStatus(witness);
      if (stage === 0) {
        if (pendingHashStage === null) {
          if (auxiliary?.kind === "transactionFieldChunk") return 5;
          if (auxiliary === null) return 6;
          break;
        }
        if (
          pendingHashStage === 0 &&
          auxiliary?.kind === "scriptSourceHashBlock"
        ) {
          return 7;
        }
        if (
          (pendingHashStage === 1 || pendingHashStage === 2) &&
          auxiliary === null
        ) {
          return 8;
        }
        if (pendingHashStage === 3 && auxiliary === null) return 9;
        break;
      }
      if (stage === 9) {
        if (auxiliary?.kind === "scriptSourceScan") {
          const currentScriptHash =
            scriptSourcesDiscoveryCurrentScriptHash(witness);
          if (!auxiliary.scriptHash.equals(currentScriptHash)) return 10;
          if (auxiliary.scriptLanguageTag === 0) return 11;
          if (
            auxiliary.scriptLanguageTag === 3 ||
            auxiliary.scriptLanguageTag === 128
          ) {
            return 12;
          }
          break;
        }
        if (auxiliary === null) return 13;
        break;
      }
      if (stage === 1) {
        if (auxiliary === null) return 14;
        if (
          auxiliary.kind === "transactionRedeemerItemBegin" ||
          (auxiliary.kind === "redeemerItemStep" &&
            auxiliary.redeemerControl === null)
        ) {
          return 15;
        }
        break;
      }
      if (stage === 11) {
        if (auxiliary === null) return 16;
        if (auxiliary.kind === "scriptSourceScan") return 17;
        break;
      }
      if (stage === 12) {
        if (auxiliary === null) return 18;
        if (
          auxiliary.kind === "redeemerScanBegin" ||
          (auxiliary.kind === "redeemerItemStep" &&
            auxiliary.redeemerControl === null)
        ) {
          return 19;
        }
        break;
      }
      if (stage === 10) {
        if (auxiliary === null) return 20;
        if (auxiliary.kind === "redeemerScanBegin") return 21;
        if (
          auxiliary.kind === "redeemerItemStep" &&
          auxiliary.redeemerControl === null
        ) {
          const next = advanceMidgardRedeemerItemProofV1({
            control: auxiliary.control,
            witness: auxiliary.witness,
          });
          const descriptor =
            next === null ? null : midgardRedeemerItemDescriptorV1(next);
          if (descriptor === null) return 21;
          const purpose = scriptSourcesDiscoveryCurrentPurpose(witness);
          return descriptor.purposeTag === [0, 1, 3, 6][purpose.purposeKind] &&
            BigInt(descriptor.pointerIndex) === purpose.purposeIndex
            ? 22
            : 21;
        }
        break;
      }
      if (stage === 8) {
        if (auxiliary === null) return 23;
        if (auxiliary.kind === "scriptPurposeScan") return 24;
        break;
      }
      if (stage === 7) {
        if (auxiliary?.kind === "transactionFieldChunk") return 25;
        if (auxiliary?.kind === "scriptPurposeScan") return 26;
        if (auxiliary === null) return 27;
        break;
      }
      if (stage !== 5) return 0;
      if (auxiliary?.kind === "ledgerOutputProofBegin") return 1;
      if (auxiliary?.kind === "ledgerOutputProofStep") return 2;
      if (auxiliary?.kind === "ledgerOutputProofFinalize") return 3;
      if (auxiliary === null) return 4;
      break;
    }
    case "ledgerDelta": {
      const control = ledgerDeltaControlStatus(witness);
      if (auxiliary === null) {
        if (control.pendingStage === 1) return 6;
        if (control.pendingStage === 0) break;
        if (control.stage === 0) return 2;
        if (control.stage === 1) return 4;
        return 7;
      }
      if (auxiliary.kind === "ledgerDeltaOperation") return 0;
      if (auxiliary.kind === "ledgerDeltaReplay") return 1;
      if (auxiliary.kind === "ledgerDeltaOutput") return 3;
      if (auxiliary.kind === "ledgerDeltaProofFrame") return 5;
      break;
    }
    case "scriptIntegrity":
      if (auxiliary === null) return scriptIntegrityStage(witness);
      break;
    case "nativeScripts":
      if (auxiliary === null) return 0;
      if (auxiliary.kind === "nativeExecutionDescriptor") {
        return auxiliary.languageTag === 0 ? 1 : 2;
      }
      break;
    // Both kind switches below are exhaustive over their kind unions and
    // return on every arm, so neither needs (nor may carry) a trailing break.
    case "cek":
      switch (cekKindV1(witness)) {
        case "core":
          return 3;
        case "context":
          return 2;
        case "selection":
          return 1;
        case "finish":
          return 0;
      }
    // eslint-disable-next-line no-fallthrough
    case "valueAndMint":
      switch (valueAndMintKindV1(witness)) {
        case "begin":
          return 0;
        case "replayBegin":
          return 1;
        case "replayInput":
          return 2;
        case "replayAsset":
          return 3;
        case "replayFinish":
          return 4;
        case "outputDescriptor":
          return 5;
        case "outputAsset":
          return 6;
        case "outputFinish":
          return 7;
        case "mintAsset":
          return 8;
        case "mintFinish":
          return 9;
        case "finalize":
          return 10;
      }
    // eslint-disable-next-line no-fallthrough
    case "terminal":
      break;
  }
  throw new Error(
    `validation evidence ${witness.phase}/${auxiliary?.kind ?? "none"} has no semantic resolver`,
  );
};

export type ValidationOneStepArgumentV1 = {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly transitionCbor: Buffer;
  readonly auxiliaryCbor: Buffer;
  readonly evidenceCbor: Buffer;
  readonly cekRouteMaterial?: CekRouteMaterialV1;
};

export type CekRouteMaterialV1 = {
  readonly envelopeCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
  readonly programEnvelopeHash: Buffer;
};

const CEK_ROUTE_MATERIAL_KEYS_V1 = Object.freeze([
  "envelopeCbor",
  "programMaterialSidecarCbor",
  "programEnvelopeHash",
] as const);

const exactCekRouteMaterialObjectV1 = (
  value: unknown,
): Record<(typeof CEK_ROUTE_MATERIAL_KEYS_V1)[number], unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("CEK route material must be an object");
  }
  const actual = Object.keys(value).sort();
  const expected = [...CEK_ROUTE_MATERIAL_KEYS_V1].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(
      `CEK route material must contain exactly ${CEK_ROUTE_MATERIAL_KEYS_V1.join(", ")}`,
    );
  }
  return value as Record<(typeof CEK_ROUTE_MATERIAL_KEYS_V1)[number], unknown>;
};

const exactBytesV1 = (value: unknown, label: string): Buffer => {
  if (!(value instanceof Uint8Array)) {
    throw new Error(`${label} must be bytes`);
  }
  return Buffer.from(value);
};

export const extractCekProgramEnvelopeFromFirstSourceChunkV1 = ({
  chunk,
  languageTag,
}: {
  readonly chunk: Uint8Array;
  readonly languageTag: 3 | 128;
}): Buffer => {
  const source = Buffer.from(chunk);
  const outer = readCborArrayHeader(source, 0, "CEK selected versioned script");
  if (outer.length !== 2) {
    throw new Error("CEK selected versioned script must contain two fields");
  }
  const language = readCborUnsigned(
    source,
    outer.nextOffset,
    "CEK selected versioned script language",
  );
  const payload = readCborBytes(
    source,
    language.nextOffset,
    "CEK selected versioned script payload",
  );
  if (
    language.value !== BigInt(languageTag) ||
    payload.nextOffset !== source.length
  ) {
    throw new Error(
      "CEK selected versioned script language or payload length is invalid",
    );
  }
  return Buffer.from(payload.value);
};

/**
 * Validates and defensively copies the complete C28 route material. The
 * envelope must be the exact selected script payload, both retained forms
 * must be canonical, and the sidecar must be exactly the complete graph for
 * that one envelope.
 */
export const validateCekRouteMaterialV1 = ({
  value,
  firstSourceChunk,
  languageTag,
}: {
  readonly value: unknown;
  readonly firstSourceChunk: Uint8Array;
  readonly languageTag: 3 | 128;
}): CekRouteMaterialV1 => {
  const routeMaterial = exactCekRouteMaterialObjectV1(value);
  const envelopeCbor = exactBytesV1(
    routeMaterial.envelopeCbor,
    "CEK route envelope CBOR",
  );
  const selectedEnvelopeCbor = extractCekProgramEnvelopeFromFirstSourceChunkV1({
    chunk: firstSourceChunk,
    languageTag,
  });
  if (!envelopeCbor.equals(selectedEnvelopeCbor)) {
    throw new Error(
      "CEK route envelope must equal the selected first-source-chunk payload",
    );
  }
  const envelope = decodeMidgardCekProgramEnvelopeV1(envelopeCbor);
  if (!encodeMidgardCekProgramEnvelopeV1(envelope).equals(envelopeCbor)) {
    throw new Error("CEK route envelope CBOR is not canonical");
  }
  const programMaterialSidecarCbor = exactBytesV1(
    routeMaterial.programMaterialSidecarCbor,
    "CEK route program-material sidecar CBOR",
  );
  const entries = decodeMidgardCekProgramMaterialSidecarV1(
    programMaterialSidecarCbor,
  );
  if (
    !encodeMidgardCekProgramMaterialSidecarV1(entries).equals(
      programMaterialSidecarCbor,
    )
  ) {
    throw new Error("CEK route program-material sidecar CBOR is not canonical");
  }
  verifyMidgardCekProgramMaterialBundleV1([envelope], entries);
  const programEnvelopeHash = exactBytesV1(
    routeMaterial.programEnvelopeHash,
    "CEK route program-envelope hash",
  );
  const canonicalEnvelopeHash = Buffer.from(
    hashMidgardCekProgramEnvelopeV1(envelope),
  );
  if (
    programEnvelopeHash.length !== 32 ||
    !programEnvelopeHash.equals(canonicalEnvelopeHash)
  ) {
    throw new Error("CEK route program-envelope hash is invalid");
  }
  return Object.freeze({
    envelopeCbor: Buffer.from(envelopeCbor),
    programMaterialSidecarCbor: Buffer.from(programMaterialSidecarCbor),
    programEnvelopeHash: Buffer.from(programEnvelopeHash),
  });
};

const buildCekRouteMaterialV1 = ({
  trace,
  witness,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly witness: ValidationMachineWorkWitness;
}): CekRouteMaterialV1 | undefined => {
  if (
    witness.phase !== "cek" ||
    witness.auxiliary?.kind !== "nativeExecutionScan" ||
    witness.auxiliary.languageTag === 0
  ) {
    return undefined;
  }
  const envelopeCbor = extractCekProgramEnvelopeFromFirstSourceChunkV1({
    chunk: witness.auxiliary.firstChunkProof.chunk,
    languageTag: witness.auxiliary.languageTag,
  });
  const envelope = decodeMidgardCekProgramEnvelopeV1(envelopeCbor);
  return validateCekRouteMaterialV1({
    value: {
      envelopeCbor,
      programMaterialSidecarCbor: trace.programMaterialSidecarCbor,
      programEnvelopeHash: hashMidgardCekProgramEnvelopeV1(envelope),
    },
    firstSourceChunk: witness.auxiliary.firstChunkProof.chunk,
    languageTag: witness.auxiliary.languageTag,
  });
};

export const validationMachineStateDataV1 = (
  state: MidgardValidationMachineStateV1,
): ConstructorData =>
  record([
    int(state.machineVersion),
    bytes(state.eventKeyHash),
    bytes(state.transactionId),
    bytes(state.transactionCommitment),
    bytes(state.validationContextHash),
    new Constr(state.sourceKind === "normal" ? 0 : 1, []),
    bytes(state.priorLedgerRoot),
    new Constr(
      {
        canonicalDecode: 0,
        compactBinding: 1,
        staticLedgerRules: 2,
        inputSets: 3,
        signatures: 4,
        phaseANativeScripts: 5,
        phaseAScriptPreconditions: 6,
        resolveInputs: 7,
        scriptSources: 8,
        nativeScripts: 9,
        scriptIntegrity: 10,
        cek: 11,
        valueAndMint: 12,
        ledgerDelta: 13,
        terminal: 14,
      }[state.phase],
      [],
    ),
    int(state.programCounter),
    bytes(state.workRoot),
    state.executionCpu,
    state.executionMemory,
    new Constr(
      state.verdict === "pending" ? 0 : state.verdict === "accepted" ? 1 : 2,
      [],
    ),
    bytes(state.rejectionCodeHash),
    bytes(state.ledgerDeltaRoot),
  ]);

export const validationOneStepWitnessDataV1 = ({
  witness,
  claimedSuccessor,
}: {
  readonly witness: ValidationMachineWorkWitness;
  readonly claimedSuccessor: MidgardValidationMachineStateV1;
}): ConstructorData =>
  record([bytes(witness.cbor), validationMachineStateDataV1(claimedSuccessor)]);

export const validationAuxiliaryWitnessDataV1 = (
  auxiliary: ValidationMachineWorkWitness["auxiliary"],
  resolveFieldCarriage: ValidationMachineFieldCarriageResolverV1 = inlineFieldCarriageResolverV1,
): PlutusData => {
  if (auxiliary === null) return new Constr(0, []);
  switch (auxiliary.kind) {
    case "transactionFieldChunk":
      return new Constr(1, [
        int(auxiliary.fieldIndex),
        int(auxiliary.itemIndex),
        resolvedFieldCarriageData(resolveFieldCarriage, auxiliary),
      ]);
    case "requiredSignerItem":
      return new Constr(2, [
        resolvedFieldCarriageData(resolveFieldCarriage, auxiliary),
        signerProofData(auxiliary.signerProof),
      ]);
    case "nativeScriptToken":
      return new Constr(3, [
        chunkProofData(auxiliary.chunkProof),
        option(auxiliary.nextChunkProof, chunkProofData),
        signerProofData(auxiliary.signerProof),
      ]);
    case "nativeScriptFrame":
      return new Constr(4, [
        record([
          bytes(auxiliary.frame.tail),
          int(auxiliary.frame.kind),
          int(auxiliary.frame.childCount),
          int(auxiliary.frame.remaining),
          int(auxiliary.frame.validCount),
          auxiliary.frame.required,
        ]),
      ]);
    case "scheduledLedgerLookup": {
      const fields = [
        sourceKind(auxiliary.sourceKind),
        bytes(auxiliary.key),
        bytes(auxiliary.nextScheduleHash),
      ];
      return auxiliary.value === null
        ? new Constr(6, [...fields, proofData(auxiliary.proofCbor)])
        : new Constr(5, [
            ...fields,
            bytes(auxiliary.value),
            proofData(auxiliary.proofCbor),
            signerProofData(auxiliary.signerProof),
          ]);
    }
    case "resolvedInputReplay":
      return new Constr(7, [
        sourceKind(auxiliary.sourceKind),
        bytes(auxiliary.key),
        bytes(auxiliary.nextScheduleHash),
        bytes(auxiliary.value),
      ]);
    case "scriptPurposeScan":
      return new Constr(8, [
        int(auxiliary.purposeKind),
        auxiliary.purposeIndex,
        bytes(auxiliary.scriptHash),
        bytes(auxiliary.subject),
        byteList(auxiliary.siblings),
      ]);
    case "scriptSourceScan":
      return new Constr(9, [
        int(auxiliary.sourceIndex),
        originKind(auxiliary.originKind),
        bytes(auxiliary.sourceKey),
        int(auxiliary.scriptLanguageTag),
        bytes(auxiliary.scriptHash),
        int(auxiliary.scriptTotalLength),
        bytes(auxiliary.scriptItemCommitment),
        byteList(auxiliary.siblings),
      ]);
    case "redeemerScanBegin":
      return new Constr(10, [
        int(auxiliary.itemIndex),
        int(auxiliary.itemCount),
        int(auxiliary.totalLength),
        bytes(auxiliary.itemCommitment),
        byteList(auxiliary.siblings),
      ]);
    case "nativeExecutionScan":
      return new Constr(11, [
        int(auxiliary.executionIndex),
        int(auxiliary.languageTag),
        int(auxiliary.purpose.purposeKind),
        auxiliary.purpose.purposeIndex,
        bytes(auxiliary.purpose.scriptHash),
        bytes(auxiliary.purpose.subject),
        byteList(auxiliary.purpose.siblings),
        int(auxiliary.source.sourceIndex),
        originKind(auxiliary.source.originKind),
        bytes(auxiliary.source.sourceKey),
        int(auxiliary.source.scriptTotalLength),
        bytes(auxiliary.source.scriptItemCommitment),
        byteList(auxiliary.source.siblings),
        bytes(auxiliary.redeemerLeaf),
        byteList(auxiliary.executionSiblings),
        chunkProofData(auxiliary.firstChunkProof),
      ]);
    case "cekCoreStep":
      return new Constr(12, [midgardCekCoreStepDataV1(auxiliary.step)]);
    case "cekResolvedContextItem":
      return new Constr(13, [
        sourceKind(auxiliary.sourceKind),
        int(auxiliary.itemIndex),
        bytes(auxiliary.key),
        bytes(auxiliary.descriptorCbor),
        byteList(auxiliary.siblings),
      ]);
    case "cekOutputContextItem":
      return new Constr(14, [
        int(auxiliary.outputIndex),
        bytes(auxiliary.descriptorCbor),
        byteList(auxiliary.siblings),
      ]);
    case "cekSignerContextItem":
      return new Constr(15, [
        frontierPeaksData(auxiliary.frontier),
        int(auxiliary.signerIndex),
        bytes(auxiliary.signerHash),
        byteList(auxiliary.siblings),
      ]);
    case "cekMintContextItem":
      return new Constr(16, [
        int(auxiliary.mintIndex),
        bytes(auxiliary.policyId),
        bytes(auxiliary.assetName),
        auxiliary.quantity,
        byteList(auxiliary.siblings),
      ]);
    case "cekRedeemerContextSelect":
      return new Constr(17, [
        redeemerControlData(auxiliary.control),
        int(auxiliary.itemIndex),
        int(auxiliary.itemCount),
        int(auxiliary.totalLength),
        bytes(auxiliary.itemCommitment),
        byteList(auxiliary.redeemerSiblings),
        int(auxiliary.purposeFrontierIndex),
        int(auxiliary.purpose.purposeKind),
        auxiliary.purpose.purposeIndex,
        bytes(auxiliary.purpose.scriptHash),
        bytes(auxiliary.purpose.subject),
        byteList(auxiliary.purpose.siblings),
      ]);
    case "redeemerItemStep":
      return new Constr(18, [
        option(auxiliary.redeemerControl, redeemerControlData),
        redeemerItemControlData(auxiliary.control),
        redeemerItemProofWitnessData(auxiliary.witness),
      ]);
    case "cekContextFinalize":
      return new Constr(19, [redeemerControlData(auxiliary.redeemerControl)]);
    case "cekContextFinalizeSpend":
      return new Constr(20, [
        redeemerControlData(auxiliary.redeemerControl),
        int(auxiliary.itemIndex),
        bytes(auxiliary.key),
        bytes(auxiliary.descriptorCbor),
        byteList(auxiliary.siblings),
      ]);
    case "cekContextAssemble":
      return new Constr(21, [contextPartsControlData(auxiliary.control)]);
    case "cekTxInfoFinalize":
      return new Constr(22, [txInfoAssemblyControlData(auxiliary.control)]);
    case "cekContextSeed":
      return new Constr(23, [finalContextControlData(auxiliary.control)]);
    case "valueInputAsset":
      return new Constr(24, [
        sourceKind(auxiliary.sourceKind),
        bytes(auxiliary.key),
        bytes(auxiliary.nextScheduleHash),
        bytes(auxiliary.descriptorCbor),
        int(auxiliary.assetIndex),
        bytes(auxiliary.policyId),
        bytes(auxiliary.assetName),
        auxiliary.quantity,
        frontierPeaksData(auxiliary.assetFrontier),
        byteList(auxiliary.assetSiblings),
        valueMutationData(auxiliary.mutationStep),
      ]);
    case "valueOutputDescriptor":
      return new Constr(38, [
        int(auxiliary.outputIndex),
        bytes(auxiliary.descriptorCbor),
        byteList(auxiliary.siblings),
      ]);
    case "valueOutputAsset":
      return new Constr(25, [
        int(auxiliary.outputIndex),
        bytes(auxiliary.descriptorCbor),
        int(auxiliary.assetIndex),
        bytes(auxiliary.policyId),
        bytes(auxiliary.assetName),
        auxiliary.quantity,
        frontierPeaksData(auxiliary.assetFrontier),
        byteList(auxiliary.assetSiblings),
        valueMutationData(auxiliary.mutationStep),
      ]);
    case "valueMintAsset":
      return new Constr(26, [
        int(auxiliary.mintIndex),
        bytes(auxiliary.policyId),
        bytes(auxiliary.assetName),
        auxiliary.quantity,
        byteList(auxiliary.siblings),
        valueMutationData(auxiliary.mutationStep),
      ]);
    case "ledgerDeltaOperation":
      return new Constr(35, [
        int(auxiliary.operationKind === "delete" ? 0 : 1),
        bytes(auxiliary.key),
        bytes(auxiliary.value),
        ledgerDeltaOperationProofData(
          auxiliary.mutationStep.proofFoldTrace.descriptor,
          auxiliary.operationMembership,
        ),
      ]);
    case "ledgerDeltaReplay":
      return new Constr(27, [
        sourceKind(auxiliary.sourceKind),
        bytes(auxiliary.key),
        bytes(auxiliary.nextScheduleHash),
        bytes(auxiliary.value),
      ]);
    case "ledgerDeltaOutput":
      return new Constr(28, [
        int(auxiliary.outputIndex),
        bytes(auxiliary.descriptorCbor),
        byteList(auxiliary.siblings),
      ]);
    case "ledgerDeltaProofFrame":
      return new Constr(34, [
        mpfProofFrameData(auxiliary.frame),
        byteList(auxiliary.siblings),
      ]);
    case "transactionRedeemerItemBegin":
      return new Constr(29, [
        resolvedFieldCarriageData(resolveFieldCarriage, auxiliary),
      ]);
    case "transactionFieldItem":
      return new Constr(30, [
        resolvedFieldCarriageData(resolveFieldCarriage, auxiliary),
      ]);
    case "ledgerOutputProofBegin":
      return new Constr(31, [
        int(auxiliary.outputIndex),
        int(auxiliary.totalLength),
        bytes(auxiliary.itemCommitment),
        byteList(auxiliary.siblings),
      ]);
    case "ledgerOutputProofStep":
      return new Constr(32, [ledgerOutputProofWitnessData(auxiliary.witness)]);
    case "ledgerOutputProofFinalize":
      return new Constr(33, [
        bytes(auxiliary.descriptorCbor),
        signerProofData(auxiliary.signerProof),
      ]);
    case "scriptSourceHashBlock":
      return new Constr(36, [
        chunkProofData(auxiliary.chunkProof),
        option(auxiliary.nextChunkProof, chunkProofData),
      ]);
    case "mintFoldAsset":
      return new Constr(39, [
        chunkProofData(auxiliary.chunkProof),
        option(auxiliary.nextChunkProof, chunkProofData),
      ]);
    case "nativeExecutionDescriptor":
      return new Constr(37, [
        int(auxiliary.executionIndex),
        int(auxiliary.languageTag),
        int(auxiliary.purpose.purposeKind),
        auxiliary.purpose.purposeIndex,
        bytes(auxiliary.purpose.scriptHash),
        bytes(auxiliary.purpose.subject),
        byteList(auxiliary.purpose.siblings),
        int(auxiliary.source.sourceIndex),
        originKind(auxiliary.source.originKind),
        bytes(auxiliary.source.sourceKey),
        int(auxiliary.source.scriptTotalLength),
        bytes(auxiliary.source.scriptItemCommitment),
        byteList(auxiliary.source.siblings),
        bytes(auxiliary.redeemerLeaf),
        byteList(auxiliary.executionSiblings),
        option(auxiliary.firstChunkProof, chunkProofData),
        frontierPeaksData(auxiliary.signerFrontier),
      ]);
  }
};

export const encodeValidationOneStepWitnessCborV1 = (input: {
  readonly witness: ValidationMachineWorkWitness;
  readonly claimedSuccessor: MidgardValidationMachineStateV1;
}): Buffer =>
  Buffer.from(Data.to(validationOneStepWitnessDataV1(input) as never), "hex");

export const encodeValidationAuxiliaryWitnessCborV1 = (
  auxiliary: ValidationMachineWorkWitness["auxiliary"],
  resolveFieldCarriage?: ValidationMachineFieldCarriageResolverV1,
): Buffer =>
  Buffer.from(
    Data.to(
      validationAuxiliaryWitnessDataV1(
        auxiliary,
        resolveFieldCarriage,
      ) as never,
    ),
    "hex",
  );

/**
 * `resolveFieldCarriage` is #600's seam: the tier every field-reading step's
 * evidence names is chosen **here**, because this is where the auxiliary first
 * becomes committed evidence and the earliest point at which a transaction — and
 * therefore a reference-input set — exists. Omit it inside §8.3's tier-1 domain;
 * above the cap it is required, and its absence is a refusal rather than a
 * fabricated index.
 */
export const buildValidationOneStepArgumentV1 = ({
  trace,
  stateIndex,
  resolveFieldCarriage,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly stateIndex: number;
  readonly resolveFieldCarriage?: ValidationMachineFieldCarriageResolverV1;
}): ValidationOneStepArgumentV1 => {
  if (!Number.isSafeInteger(stateIndex) || stateIndex < 0) {
    throw new Error(
      "validation one-step state index must be a non-negative safe integer",
    );
  }
  const pre = trace.states[stateIndex];
  const claimedSuccessor = trace.states[stateIndex + 1];
  const witness = trace.witnesses[stateIndex];
  if (
    pre === undefined ||
    claimedSuccessor === undefined ||
    witness === undefined
  ) {
    throw new Error(
      `validation trace does not contain transition ${stateIndex.toString()}`,
    );
  }
  if (
    witness.phase !== pre.phase ||
    witness.programCounter !== pre.programCounter ||
    claimedSuccessor.programCounter !== pre.programCounter + 1
  ) {
    throw new Error(
      "validation one-step witness is not aligned with its trace states",
    );
  }
  const transitionData = validationOneStepWitnessDataV1({
    witness,
    claimedSuccessor,
  });
  const auxiliaryData = validationAuxiliaryWitnessDataV1(
    witness.auxiliary,
    resolveFieldCarriage,
  );
  const transitionCbor = Buffer.from(Data.to(transitionData as never), "hex");
  const auxiliaryCbor = Buffer.from(Data.to(auxiliaryData as never), "hex");
  const evidenceCbor = Buffer.from(
    Data.to(record([transitionData, auxiliaryData]) as never),
    "hex",
  );
  const maximum = MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes;
  if (
    transitionCbor.length >= maximum ||
    auxiliaryCbor.length >= maximum ||
    evidenceCbor.length >= maximum
  ) {
    throw new Error(
      `validation transition ${stateIndex.toString()} exceeds the strict L1 preimage envelope`,
    );
  }
  const cekRouteMaterial = buildCekRouteMaterialV1({ trace, witness });
  return {
    resolverIndex: resolverPhaseIndex(pre.phase),
    semanticResolverIndex: validationSemanticResolverIndexV1(witness),
    transitionCbor,
    auxiliaryCbor,
    evidenceCbor,
    ...(cekRouteMaterial === undefined ? {} : { cekRouteMaterial }),
  };
};
