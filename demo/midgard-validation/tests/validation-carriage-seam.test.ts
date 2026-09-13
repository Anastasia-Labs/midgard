import {
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  type MidgardFieldCarriage,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { describe, expect, it } from "vitest";

import {
  encodeValidationAuxiliaryWitnessCbor,
  ValidationMachineCarriagePreimageSubstitutedError,
  ValidationMachineCarriageResolutionRequiredError,
  ValidationMachineCarriageTierMismatchError,
  type ValidationMachineFieldCarriageResolver,
  type ValidationMachineWorkWitness,
} from "../src/index.js";

/**
 * #600's carriage seam, as refusals (#600 review findings 2 and 3).
 *
 * The seam is `validationAuxiliaryWitnessDataV1`: the point where a step's
 * carriage plan input becomes the auxiliary CBOR that `evidence_hash` commits.
 * Two things must be impossible there, and neither is visible from a green
 * trace suite, because the happy path never reaches either:
 *
 * 1. Encoding an above-cap preimage with **no resolver**. §8.4 carries it as
 *    reference-input indices and a caller with no reference inputs has none to
 *    give, so `inlineFieldCarriageResolverV1` refuses rather than emitting a
 *    tier-1 `Inline` that §8.4 does not admit for that length.
 * 2. Encoding whatever a **caller-supplied** resolver returns. The submitter
 *    owns the resolver but not the tier: §8.4 is a partition over the preimage's
 *    length, so the seam checks the returned tier against it (#597 Ruling 1).
 *
 * Every arm that carries a carriage is exercised, because the invariant is only
 * structural if it holds at all of them.
 */

const ABOVE_CAP_PREIMAGE_BYTES = MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES + 1;
const BELOW_CAP_PREIMAGE_BYTES = 1_024;

type CarriageAuxiliary = NonNullable<ValidationMachineWorkWitness["auxiliary"]>;

/**
 * One auxiliary per carriage-carrying constructor, over a preimage of the given
 * size. The signer and item indices are the ones those steps carry; only the
 * plan input matters here.
 */
const carriageAuxiliaries = (
  preimageBytes: number,
): readonly {
  readonly kind: string;
  readonly auxiliary: CarriageAuxiliary;
}[] =>
  (
    [
      {
        kind: "transactionFieldChunk",
        fieldIndex: 2,
        itemIndex: 0,
        fieldPreimage: Buffer.alloc(preimageBytes, 0xa5),
      },
      {
        kind: "transactionFieldItem",
        fieldIndex: 2,
        fieldPreimage: Buffer.alloc(preimageBytes, 0xa5),
      },
      {
        kind: "transactionRedeemerItemBegin",
        fieldIndex: 8,
        fieldPreimage: Buffer.alloc(preimageBytes, 0xa5),
      },
      {
        kind: "requiredSignerItem",
        fieldIndex: 4,
        fieldPreimage: Buffer.alloc(preimageBytes, 0xa5),
        signerProof: { kind: "none" },
      },
    ] satisfies readonly CarriageAuxiliary[]
  ).map((auxiliary) => ({ kind: auxiliary.kind, auxiliary }));

/** A resolver that hands back exactly the carriage it was told to. */
const fixedResolver =
  (carriage: MidgardFieldCarriage): ValidationMachineFieldCarriageResolver =>
  () =>
    carriage;

describe("§8 carriage seam refusals V1", () => {
  it("refuses to commit an above-cap preimage with no carriage resolver", () => {
    expect(selectMidgardFieldCarriageTier(ABOVE_CAP_PREIMAGE_BYTES)).toBe(
      "RawUtxo",
    );
    for (const { kind, auxiliary } of carriageAuxiliaries(
      ABOVE_CAP_PREIMAGE_BYTES,
    )) {
      let thrown: unknown = null;
      try {
        encodeValidationAuxiliaryWitnessCbor(auxiliary);
      } catch (error) {
        thrown = error;
      }
      expect(thrown, kind).toBeInstanceOf(
        ValidationMachineCarriageResolutionRequiredError,
      );
      const error = thrown as ValidationMachineCarriageResolutionRequiredError;
      expect(error.name).toBe(
        "ValidationMachineCarriageResolutionRequiredErrorV1",
      );
      expect(error.fieldIndex).toBe(
        (auxiliary as { readonly fieldIndex: number }).fieldIndex,
      );
      expect(error.preimageLength).toBe(ABOVE_CAP_PREIMAGE_BYTES);
      expect(error.selectedTier).toBe("RawUtxo");
      expect(error.maxTier1PreimageBytes).toBe(
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
      );
    }
  });

  it("commits the tier §8.4 admits when no resolver is supplied and the preimage fits tier 1", () => {
    // The default resolver is not a refusal machine — inside the cap it is the
    // ordinary path, and the block-build caller depends on that (#600).
    for (const { kind, auxiliary } of carriageAuxiliaries(
      BELOW_CAP_PREIMAGE_BYTES,
    )) {
      expect(
        encodeValidationAuxiliaryWitnessCbor(auxiliary).length,
        kind,
      ).toBeGreaterThan(BELOW_CAP_PREIMAGE_BYTES);
    }
  });

  it("refuses a resolver that returns tier-1 Inline above §8.3's cap", () => {
    for (const { kind, auxiliary } of carriageAuxiliaries(
      ABOVE_CAP_PREIMAGE_BYTES,
    )) {
      let thrown: unknown = null;
      try {
        encodeValidationAuxiliaryWitnessCbor(
          auxiliary,
          fixedResolver({
            carriage: "Inline",
            preimage: Buffer.alloc(ABOVE_CAP_PREIMAGE_BYTES, 0xa5),
          }),
        );
      } catch (error) {
        thrown = error;
      }
      expect(thrown, kind).toBeInstanceOf(
        ValidationMachineCarriageTierMismatchError,
      );
      const error = thrown as ValidationMachineCarriageTierMismatchError;
      expect(error.name).toBe("ValidationMachineCarriageTierMismatchErrorV1");
      expect(error.fieldIndex).toBe(
        (auxiliary as { readonly fieldIndex: number }).fieldIndex,
      );
      expect(error.preimageLength).toBe(ABOVE_CAP_PREIMAGE_BYTES);
      expect(error.expectedTier).toBe("RawUtxo");
      expect(error.returnedTier).toBe("Inline");
    }
  });

  it("refuses a resolver that substitutes the tier-1 preimage's bytes", () => {
    // The tier is right and the length is right, so nothing about the shape of
    // the carriage gives this away — and the auxiliary **is** the committed
    // evidence, so encoding it would hash an `evidence_hash` over bytes the
    // trace never read. Only tier 1 can do this: tiers 2 and 3 carry indices.
    for (const { kind, auxiliary } of carriageAuxiliaries(
      BELOW_CAP_PREIMAGE_BYTES,
    )) {
      const substituted = Buffer.alloc(BELOW_CAP_PREIMAGE_BYTES, 0xa5);
      substituted[0] = 0x5a;
      let thrown: unknown = null;
      try {
        encodeValidationAuxiliaryWitnessCbor(
          auxiliary,
          fixedResolver({ carriage: "Inline", preimage: substituted }),
        );
      } catch (error) {
        thrown = error;
      }
      expect(thrown, kind).toBeInstanceOf(
        ValidationMachineCarriagePreimageSubstitutedError,
      );
      const error = thrown as ValidationMachineCarriagePreimageSubstitutedError;
      expect(error.name).toBe(
        "ValidationMachineCarriagePreimageSubstitutedErrorV1",
      );
      expect(error.fieldIndex).toBe(
        (auxiliary as { readonly fieldIndex: number }).fieldIndex,
      );
      expect(error.preimageLength).toBe(BELOW_CAP_PREIMAGE_BYTES);
      expect(error.returnedPreimageLength).toBe(BELOW_CAP_PREIMAGE_BYTES);
    }
  });

  it("refuses a resolver that returns an indexed tier below §8.3's cap", () => {
    expect(selectMidgardFieldCarriageTier(BELOW_CAP_PREIMAGE_BYTES)).toBe(
      "Inline",
    );
    for (const returned of [
      { carriage: "RawUtxo", refInputIndex: 1 },
      {
        carriage: "Certified",
        certRefInputIndex: 3,
        chunkRefInputIndices: [1, 2],
      },
    ] satisfies readonly MidgardFieldCarriage[]) {
      for (const { kind, auxiliary } of carriageAuxiliaries(
        BELOW_CAP_PREIMAGE_BYTES,
      )) {
        let thrown: unknown = null;
        try {
          encodeValidationAuxiliaryWitnessCbor(
            auxiliary,
            fixedResolver(returned),
          );
        } catch (error) {
          thrown = error;
        }
        expect(thrown, `${kind}/${returned.carriage}`).toBeInstanceOf(
          ValidationMachineCarriageTierMismatchError,
        );
        const error = thrown as ValidationMachineCarriageTierMismatchError;
        expect(error.preimageLength).toBe(BELOW_CAP_PREIMAGE_BYTES);
        expect(error.expectedTier).toBe("Inline");
        expect(error.returnedTier).toBe(returned.carriage);
      }
    }
  });

  it("passes a partition-admissible resolver at every tier", () => {
    // The tier-equality check must not narrow what an honest resolver may
    // return: the representative-index helper the cross-language vectors use
    // produces exactly `selectMidgardFieldCarriageTier`'s tier, so a
    // tier-equality check is transparent to it.
    for (const { preimageBytes, carriage } of [
      {
        preimageBytes: BELOW_CAP_PREIMAGE_BYTES,
        carriage: {
          carriage: "Inline",
          preimage: Buffer.alloc(BELOW_CAP_PREIMAGE_BYTES, 0xa5),
        },
      },
      {
        preimageBytes: ABOVE_CAP_PREIMAGE_BYTES,
        carriage: { carriage: "RawUtxo", refInputIndex: 1 },
      },
      {
        preimageBytes: 16_384,
        carriage: {
          carriage: "Certified",
          certRefInputIndex: 3,
          chunkRefInputIndices: [1, 2],
        },
      },
    ] satisfies readonly {
      readonly preimageBytes: number;
      readonly carriage: MidgardFieldCarriage;
    }[]) {
      expect(selectMidgardFieldCarriageTier(preimageBytes)).toBe(
        carriage.carriage,
      );
      for (const { auxiliary } of carriageAuxiliaries(preimageBytes)) {
        expect(() =>
          encodeValidationAuxiliaryWitnessCbor(
            auxiliary,
            fixedResolver(carriage),
          ),
        ).not.toThrow();
      }
    }
  });
});
