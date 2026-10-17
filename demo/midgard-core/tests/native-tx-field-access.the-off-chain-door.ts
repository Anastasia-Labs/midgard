import "./native-tx-field-access.8-4-tier-3-view-straddle-aware-lazy-chunk-verify.js";

import { describe, expect, it } from "vitest";

import { MidgardTxCodecError } from "../src/codec/errors.js";
import {
  authenticatedMidgardFieldView,
  deriveMidgardFieldPreimageCertificate,
  encodeMidgardFieldPreimage,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  midgardFieldCommitment,
  midgardFieldItemAt,
  midgardFieldItemCount,
  splitMidgardFieldPreimageIntoChunks,
} from "../src/codec/native-tx-field-access.js";
import {
  filler,
  hex,
} from "./native-tx-field-access.7-access-invariants-over-a-whole-view.js";

describe("the off-chain door", () => {
  const fieldIndex = 3;
  const items = [filler(28, 1), filler(28, 2)];
  const preimage = encodeMidgardFieldPreimage(items);
  const commitment = midgardFieldCommitment(preimage);
  const txId = filler(32, 21);

  it("serves tier 1 from the redeemer's own bytes", () => {
    const view = authenticatedMidgardFieldView({
      fieldIndex,
      txId,
      expectedCommitment: commitment,
      carriage: { carriage: "Inline", preimage },
    });
    expect(midgardFieldItemCount(view)).toBe(2);
  });

  it("serves tier 2 from a positional reference input", () => {
    const view = authenticatedMidgardFieldView({
      fieldIndex,
      txId,
      expectedCommitment: commitment,
      carriage: { carriage: "RawUtxo", refInputIndex: 1 },
      referenceInputs: [{}, { inlineDatumBytes: preimage }],
    });
    expect(midgardFieldItemCount(view)).toBe(2);
  });

  it("fails closed when a named reference input is absent or carries nothing", () => {
    expect(() =>
      authenticatedMidgardFieldView({
        fieldIndex,
        txId,
        expectedCommitment: commitment,
        carriage: { carriage: "RawUtxo", refInputIndex: 3 },
        referenceInputs: [{ inlineDatumBytes: preimage }],
      }),
    ).toThrow(/not present/u);
    expect(() =>
      authenticatedMidgardFieldView({
        fieldIndex,
        txId,
        expectedCommitment: commitment,
        carriage: { carriage: "RawUtxo", refInputIndex: 0 },
        referenceInputs: [{}],
      }),
    ).toThrow(/no nothing-but-bytes inline datum/u);
  });

  // One tier-3 carriage, reused by the three assertions below. The preimage is
  // 600 items — larger than one chunk, so §8.4 admits exactly this tier.
  const bigItems = Array.from({ length: 600 }, (_, index) =>
    filler(28, index + 1),
  );
  const bigPreimage = encodeMidgardFieldPreimage(bigItems);
  const bigCommitment = midgardFieldCommitment(bigPreimage);
  const bigCertificate = deriveMidgardFieldPreimageCertificate({
    owner: filler(28, 5),
    txId,
    fieldIndex,
    preimage: bigPreimage,
  });
  const bigChunks = splitMidgardFieldPreimageIntoChunks(bigPreimage);
  const tier3ReferenceInputs = [
    {
      certificate: bigCertificate,
      certificateAssetName: MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
    },
    { inlineDatumBytes: bigChunks[0] },
    { inlineDatumBytes: bigChunks[1] },
  ];
  const tier3Carriage = {
    carriage: "Certified",
    certRefInputIndex: 0,
    chunkRefInputIndices: [1, 2],
  } as const;

  it("serves tier 3 from a certificate and its positional chunks", () => {
    const view = authenticatedMidgardFieldView({
      fieldIndex,
      txId,
      expectedCommitment: bigCommitment,
      carriage: tier3Carriage,
      referenceInputs: tier3ReferenceInputs,
    });
    expect(midgardFieldItemCount(view)).toBe(bigItems.length);
    expect(hex(midgardFieldItemAt(view, 599))).toBe(hex(bigItems[599]));
  });

  it("refuses a tier-3 carriage over bytes the field never committed to", () => {
    // The counterexample the tier-3 door has to answer. `commitment` here is
    // the honest commitment of the two-item preimage this block opens with;
    // `bigChunks` carry a 600-item preimage. Nothing may come back — not a
    // short read, not a count. The honest manifest wears its own (foreign)
    // commitment and fails the welded-hash equality (#606) — the same check
    // the on-chain door runs.
    expect(() =>
      authenticatedMidgardFieldView({
        fieldIndex,
        txId,
        expectedCommitment: commitment,
        carriage: tier3Carriage,
        referenceInputs: tier3ReferenceInputs,
      }),
    ).toThrow(/field_hash does not match the anchored commitment/u);
    // And a manifest that lies about its own weld is caught one check later,
    // by the §4 reconstruction the off-chain door runs for itself (on-chain
    // this is the certificate policy's mint-time proof).
    expect(() =>
      authenticatedMidgardFieldView({
        fieldIndex,
        txId,
        expectedCommitment: commitment,
        carriage: tier3Carriage,
        referenceInputs: [
          {
            ...tier3ReferenceInputs[0],
            certificate: { ...bigCertificate, fieldHash: commitment },
          },
          tier3ReferenceInputs[1],
          tier3ReferenceInputs[2],
        ],
      }),
    ).toThrow(/does not match the committed field hash/u);
    expect(() =>
      authenticatedMidgardFieldView({
        fieldIndex,
        txId,
        expectedCommitment: commitment,
        carriage: tier3Carriage,
        referenceInputs: tier3ReferenceInputs,
      }),
    ).toThrow(MidgardTxCodecError);
  });

  it("rejects the same commitment mismatch at every tier", () => {
    // Tiers 1 and 2 already refused this; tier 3 refusing it too is what makes
    // carriage an encoding detail rather than a choice of how hard to check.
    // (Under tier 3 the refusal happens one check earlier since #606 — at the
    // welded-hash equality — hence the two-message alternation.)
    for (const carriage of [
      { carriage: "Inline", preimage: bigPreimage } as const,
      { carriage: "RawUtxo", refInputIndex: 3 } as const,
      tier3Carriage,
    ]) {
      expect(() =>
        authenticatedMidgardFieldView({
          fieldIndex,
          txId,
          expectedCommitment: commitment,
          carriage,
          referenceInputs: [
            ...tier3ReferenceInputs,
            { inlineDatumBytes: bigPreimage },
          ],
        }),
      ).toThrow(
        /does not match the (committed field hash|anchored commitment)/u,
      );
    }
  });

  it("requires the §8.6 constant token name at the tier-3 manifest input", () => {
    expect(() =>
      authenticatedMidgardFieldView({
        fieldIndex,
        txId,
        expectedCommitment: bigCommitment,
        carriage: tier3Carriage,
        referenceInputs: [
          {
            ...tier3ReferenceInputs[0],
            certificateAssetName: Buffer.alloc(32),
          },
          tier3ReferenceInputs[1],
          tier3ReferenceInputs[2],
        ],
      }),
    ).toThrow(/§8.6 constant name/u);
  });
});
