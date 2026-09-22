/**
 * #621 route freedom, the pure half: the build-time delivery route for the
 * CanonicalDecode complete item is resolved from the caller's request, the
 * presence of a publication out-ref, the staged §8.4 tier, and the measured
 * cost heuristic — in that precedence order — and every refusal arm fires
 * bare here, before any emulator is involved (the journeys in
 * `submit-init-emulator-route-freedom*.test.ts` drive the reachable arms
 * through real staged transactions).
 *
 * The 13,522-byte pin is `maxReliableDirectCompleteItemBytes`, an owner-signed
 * consensus measurement: since Option B it steers cost, not soundness. #622
 * re-measured it and the owner approved the lane-level rebind 12,810 → 13,522
 * at the #617 wave sign-off (ruling (b), 2026-08-22) — these tests pin that
 * this builder reads it and never overrides it.
 */
import { MIDGARD_ENVELOPE_MEASUREMENTS } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  resolveValidationProofItemDeliveryRoute,
  ValidationInlineDeliveryEnvelopeRefusedError,
} from "../src/index.js";

const RELIABLE_DIRECT_PIN = 13_522;

describe("resolveValidationProofItemDeliveryRouteV1 (#621)", () => {
  it("pins the heuristic threshold to the owner-signed 13,522-byte measurement", () => {
    expect(
      MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes,
    ).toBe(RELIABLE_DIRECT_PIN);
  });

  it("refuses a delivery request off the complete-item path", () => {
    for (const requestedDelivery of ["inline", "reference"] as const) {
      expect(() =>
        resolveValidationProofItemDeliveryRoute({
          requestedDelivery,
          hasProofItemReferenceOutRef: false,
          committedCarriage: undefined,
        }),
      ).toThrow(/exists only on the CanonicalDecode complete-item path/u);
    }
  });

  it("resolves no route off the complete-item path when nothing is requested", () => {
    expect(
      resolveValidationProofItemDeliveryRoute({
        requestedDelivery: undefined,
        hasProofItemReferenceOutRef: false,
        committedCarriage: undefined,
      }),
    ).toBeUndefined();
  });

  it("refuses a delivery request on §8.4 tiers 2-3, naming the tier", () => {
    for (const requestedDelivery of ["inline", "reference"] as const) {
      expect(() =>
        resolveValidationProofItemDeliveryRoute({
          requestedDelivery,
          hasProofItemReferenceOutRef: false,
          committedCarriage: "RawUtxo",
        }),
      ).toThrow(/tier-2 `RawUtxo` already names reference inputs/u);
      expect(() =>
        resolveValidationProofItemDeliveryRoute({
          requestedDelivery,
          hasProofItemReferenceOutRef: false,
          committedCarriage: "Certified",
        }),
      ).toThrow(/tier-3 `Certified` already names reference inputs/u);
    }
  });

  it("resolves no tier-1 route on tiers 2-3 when nothing is requested", () => {
    for (const committedCarriage of ["RawUtxo", "Certified"] as const) {
      expect(
        resolveValidationProofItemDeliveryRoute({
          requestedDelivery: undefined,
          hasProofItemReferenceOutRef: false,
          committedCarriage,
        }),
      ).toBeUndefined();
    }
  });

  it("refuses the inline request that contradicts a supplied publication out-ref", () => {
    expect(() =>
      resolveValidationProofItemDeliveryRoute({
        requestedDelivery: "inline",
        hasProofItemReferenceOutRef: true,
        committedCarriage: "Inline",
        preimageByteLength: 100,
      }),
    ).toThrow(/"inline" contradicts `proofItemReferenceOutRef`/u);
  });

  it("honors an explicit inline request at any tier-1 size — the envelope, not the heuristic, is the inline gate", () => {
    for (const preimageByteLength of [0, RELIABLE_DIRECT_PIN, 14_332]) {
      expect(
        resolveValidationProofItemDeliveryRoute({
          requestedDelivery: "inline",
          hasProofItemReferenceOutRef: false,
          committedCarriage: "Inline",
          preimageByteLength,
        }),
      ).toBe("inline");
    }
  });

  it("honors an explicit reference request at any tier-1 size, out-ref or not", () => {
    for (const hasProofItemReferenceOutRef of [false, true]) {
      for (const preimageByteLength of [0, 100, 14_332]) {
        expect(
          resolveValidationProofItemDeliveryRoute({
            requestedDelivery: "reference",
            hasProofItemReferenceOutRef,
            committedCarriage: "Inline",
            preimageByteLength,
          }),
        ).toBe("reference");
      }
    }
  });

  it("implies the reference route from a supplied out-ref when nothing is requested", () => {
    expect(
      resolveValidationProofItemDeliveryRoute({
        requestedDelivery: undefined,
        hasProofItemReferenceOutRef: true,
        committedCarriage: "Inline",
        preimageByteLength: 100,
      }),
    ).toBe("reference");
  });

  it("routes by the measured heuristic when nothing is requested, splitting exactly at the pin", () => {
    expect(
      resolveValidationProofItemDeliveryRoute({
        requestedDelivery: undefined,
        hasProofItemReferenceOutRef: false,
        committedCarriage: "Inline",
        preimageByteLength: RELIABLE_DIRECT_PIN,
      }),
    ).toBe("inline");
    expect(
      resolveValidationProofItemDeliveryRoute({
        requestedDelivery: undefined,
        hasProofItemReferenceOutRef: false,
        committedCarriage: "Inline",
        preimageByteLength: RELIABLE_DIRECT_PIN + 1,
      }),
    ).toBe("reference");
  });

  it("refuses the heuristic without a preimage length rather than guessing", () => {
    expect(() =>
      resolveValidationProofItemDeliveryRoute({
        requestedDelivery: undefined,
        hasProofItemReferenceOutRef: false,
        committedCarriage: "Inline",
      }),
    ).toThrow(/needs the tier-1 preimage length/u);
  });

  it("propagates the single-publication envelope refusal for impossible sizes", () => {
    expect(() =>
      resolveValidationProofItemDeliveryRoute({
        requestedDelivery: undefined,
        hasProofItemReferenceOutRef: false,
        committedCarriage: "Inline",
        preimageByteLength: 14_397,
      }),
    ).toThrow(/exceeds the measured single-publication envelope/u);
  });
});

describe("ValidationInlineDeliveryEnvelopeRefusedErrorV1 (#621)", () => {
  it("carries the projection and the envelope it refused against", () => {
    const error = new ValidationInlineDeliveryEnvelopeRefusedError({
      label: "Validation canonical item observation",
      projectedSignedBytes: 16_500,
      maxTransactionBytes: 16_384,
    });
    expect(error).toBeInstanceOf(Error);
    expect(error.projectedSignedBytes).toBe(16_500);
    expect(error.maxTransactionBytes).toBe(16_384);
    expect(error.message).toMatch(
      /would sign at 16500 bytes, over the 16384-byte L1 proof envelope; refusing pre-sign/u,
    );
    expect(error.message).toMatch(/rides the §8 publication route instead/u);
  });
});
