import type {
  MidgardMpfProofDescriptor,
  MidgardMpfProofFrame,
  MidgardMpfProofStep,
  MidgardValidationMerkleFrontier,
  MidgardValidationMerkleMembership,
} from "@al-ft/midgard-core";
import {
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  type MidgardFieldCarriage,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { lucidDataFromCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";
import { Constr } from "@lucid-evolution/lucid";

import { type ValidationMachineFieldCarriagePlanInput } from "./validation-machine/index.js";

export type PlutusData = unknown;

export type ConstructorData = Constr<PlutusData>;

export const bytes = (value: Uint8Array): string =>
  Buffer.from(value).toString("hex");

export const int = (value: number | bigint): bigint => BigInt(value);

export const record = (fields: readonly PlutusData[]): ConstructorData =>
  new Constr(0, [...fields]);

export const bool = (value: boolean): ConstructorData =>
  new Constr(value ? 1 : 0, []);

export const option = <T>(
  value: T | null,
  encode: (exact: T) => PlutusData,
): ConstructorData =>
  value === null ? new Constr(1, []) : new Constr(0, [encode(value)]);

export const byteList = (values: readonly Uint8Array[]): readonly string[] =>
  values.map(bytes);

export const proofData = (proofCbor: Uint8Array): PlutusData =>
  lucidDataFromCborIterative(proofCbor) as PlutusData;

export const frontierPeaksData = (
  frontier: MidgardValidationMerkleFrontier,
): readonly ConstructorData[] =>
  frontier.peaks.map((peak) => record([int(peak.height), bytes(peak.hash)]));

const mpfProofStepData = (step: MidgardMpfProofStep): ConstructorData => {
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

export const mpfProofFrameData = (
  frame: MidgardMpfProofFrame,
): ConstructorData =>
  record([
    int(frame.version),
    int(frame.frameIndex),
    int(frame.cursor),
    int(frame.nextCursor),
    mpfProofStepData(frame.step),
  ]);

const mpfProofDescriptorData = (
  descriptor: MidgardMpfProofDescriptor,
): ConstructorData =>
  record([
    int(descriptor.version),
    int(descriptor.frameCount),
    int(descriptor.terminalCursor),
    frontierPeaksData(descriptor.frontier),
  ]);

export const ledgerDeltaOperationProofData = (
  descriptor: MidgardMpfProofDescriptor,
  membership: MidgardValidationMerkleMembership,
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
export const fieldCarriageData = (
  carriage: MidgardFieldCarriage,
): PlutusData => {
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
export type ValidationMachineFieldCarriageResolver = (
  planInput: ValidationMachineFieldCarriagePlanInput,
) => MidgardFieldCarriage;

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
export class ValidationMachineCarriageResolutionRequiredError extends Error {
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
        `${MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES.toString()} bytes), so its carriage is ` +
        "positional reference-input indices that §8.7 resolves by content against a concrete " +
        "transaction. Encoding this auxiliary requires a carriage resolver built from that " +
        "transaction's complete reference-input set.",
    );
    this.fieldIndex = fieldIndex;
    this.preimageLength = preimageLength;
    this.selectedTier = selectedTier;
    this.maxTier1PreimageBytes = MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES;
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
export const inlineFieldCarriageResolver: ValidationMachineFieldCarriageResolver =
  ({ fieldIndex, fieldPreimage }) => {
    const tier = selectMidgardFieldCarriageTier(fieldPreimage.length);
    if (tier !== "Inline") {
      throw new ValidationMachineCarriageResolutionRequiredError({
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
export class ValidationMachineCarriageTierMismatchError extends Error {
  override readonly name = "ValidationMachineCarriageTierMismatchErrorV1";
  readonly fieldIndex: number;
  readonly preimageLength: number;
  readonly expectedTier: MidgardFieldCarriage["carriage"];
  readonly returnedTier: MidgardFieldCarriage["carriage"];

  constructor({
    fieldIndex,
    preimageLength,
    expectedTier,
    returnedTier,
  }: {
    readonly fieldIndex: number;
    readonly preimageLength: number;
    readonly expectedTier: MidgardFieldCarriage["carriage"];
    readonly returnedTier: MidgardFieldCarriage["carriage"];
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
export class ValidationMachineCarriagePreimageSubstitutedError extends Error {
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
