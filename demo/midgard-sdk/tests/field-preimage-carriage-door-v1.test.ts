import {
  type MidgardFieldCarriagePlan,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core/codec/native-tx-carriage-v1";
import type { UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertMidgardFieldCarriageResolvesAtDoor,
  deriveFieldPreimageCertification,
  fieldPreimagePublicationDatumCbor,
  resolveMidgardFieldCarriageAgainstReferenceInputs,
} from "@/fraud-proof/field-preimage-carriage-v1.js";

/**
 * Ruling D3-A's door guard, exercised directly (#600).
 *
 * The guard's production wiring point is the observe-stage dispute builder and
 * is deferred to #579, so these rows are the only thing that can make its
 * refusal fire. They are the point of the guard rather than a sample of it: a
 * committed carriage is positional indices into the *door* transaction's
 * canonically-sorted reference-input list, and the two ways that list moves
 * between commitment and the door — an entry dropped, an entry inserted ahead
 * of the carriage — are exactly the two refusals below.
 */

const CARRIAGE_OWNER = Buffer.alloc(28, 0x11);
const CARRIAGE_TX_ID = Buffer.alloc(32, 0x22);
const CERTIFICATE_POLICY_ID = "ab".repeat(28);
const PROVER_KEY_ADDRESS = "addr_test1_prover_key";
const CERTIFICATE_ADDRESS = "addr_test1_field_preimage_certificate";
const FIELD_INDEX = 2;

const carriageUtxo = ({
  txHash,
  outputIndex,
  address,
  datum,
  assets,
}: {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address: string;
  readonly datum: string;
  readonly assets?: Record<string, bigint>;
}): UTxO => ({
  txHash,
  outputIndex,
  address,
  assets: { lovelace: 5_000_000n, ...(assets ?? {}) },
  datum,
});

/**
 * The published spending validator a real dispute step reads through
 * `readFrom`. It is not carriage, it sorts ahead of every carriage UTxO, and it
 * is what makes the committed indices non-zero — dropping it is the cheapest
 * realistic way for the door's list to disagree with the committed one.
 */
const scriptReference = carriageUtxo({
  txHash: "00".repeat(32),
  outputIndex: 0,
  address: "addr_test1_published_validator",
  datum: "d87980",
});

/** Some other reference input the door transaction acquired later. */
const lateReferenceInput = carriageUtxo({
  txHash: "01".repeat(32),
  outputIndex: 0,
  address: "addr_test1_some_other_reference",
  datum: "d87980",
});

const planFor = (preimageBytes: number): MidgardFieldCarriagePlan =>
  planMidgardFieldCarriage({
    owner: CARRIAGE_OWNER,
    txId: CARRIAGE_TX_ID,
    fieldIndex: FIELD_INDEX,
    preimage: Buffer.alloc(preimageBytes, 0xa5),
  });

/** The carriage UTxOs a plan requires, in no particular order. */
const carriageInputsFor = (plan: MidgardFieldCarriagePlan): readonly UTxO[] => [
  ...plan.publications.map((publication, offset) =>
    carriageUtxo({
      txHash: (offset + 3).toString(16).padStart(2, "0").repeat(32),
      outputIndex: offset,
      address: PROVER_KEY_ADDRESS,
      datum: fieldPreimagePublicationDatumCbor(publication.bytes),
    }),
  ),
  ...(plan.tier === "Certified"
    ? [
        ((): UTxO => {
          const certification = deriveFieldPreimageCertification(plan);
          return carriageUtxo({
            txHash: "f1".repeat(32),
            outputIndex: 0,
            address: CERTIFICATE_ADDRESS,
            datum: certification.datumCbor,
            assets: {
              [`${CERTIFICATE_POLICY_ID}${certification.assetNameHex}`]: 1n,
            },
          });
        })(),
      ]
    : []),
];

const resolveAgainst = (
  plan: MidgardFieldCarriagePlan,
  referenceInputs: readonly UTxO[],
) =>
  resolveMidgardFieldCarriageAgainstReferenceInputs({
    plan,
    referenceInputs,
    certificatePolicyId: CERTIFICATE_POLICY_ID,
  });

const assertAtDoor = (
  plan: MidgardFieldCarriagePlan,
  committedAgainst: readonly UTxO[],
  doorReferenceInputs: readonly UTxO[],
): void =>
  assertMidgardFieldCarriageResolvesAtDoor({
    carriage: resolveAgainst(plan, committedAgainst),
    plan,
    doorReferenceInputs,
    certificatePolicyId: CERTIFICATE_POLICY_ID,
    label: "field 2",
  });

// §8.4's partition: 14,336 is the tier-1 cap and 15,148 is `chunk_bytes_k`.
const TIER2_PREIMAGE_BYTES = 14_778;
const TIER3_PREIMAGE_BYTES = 16_384;

describe("§8 carriage door-resolution guard V1", () => {
  it("passes when the door's reference-input set is the one the indices were resolved against", () => {
    for (const preimageBytes of [TIER2_PREIMAGE_BYTES, TIER3_PREIMAGE_BYTES]) {
      const plan = planFor(preimageBytes);
      const referenceInputs = [scriptReference, ...carriageInputsFor(plan)];
      // The committed indices are not `0`: the script reference sorts first, so
      // a guard that agreed with itself on a trivially-zero index would prove
      // nothing.
      const carriage = resolveAgainst(plan, referenceInputs);
      expect(
        carriage.carriage === "RawUtxo"
          ? carriage.refInputIndex
          : carriage.carriage === "Certified"
            ? carriage.chunkRefInputIndices[0]
            : -1,
      ).toBe(1);
      expect(() =>
        assertAtDoor(plan, referenceInputs, referenceInputs),
      ).not.toThrow();
      // Order of the *argument* is irrelevant — both sides sort canonically —
      // so a re-ordered but identical set still passes.
      expect(() =>
        assertAtDoor(plan, referenceInputs, [...referenceInputs].reverse()),
      ).not.toThrow();
    }
  });

  it("refuses when the door transaction dropped a reference input the indices counted", () => {
    const tier2Plan = planFor(TIER2_PREIMAGE_BYTES);
    const tier2Inputs = [scriptReference, ...carriageInputsFor(tier2Plan)];
    expect(() =>
      assertAtDoor(tier2Plan, tier2Inputs, tier2Inputs.slice(1)),
    ).toThrowError(
      /^field 2 §8 carriage does not resolve at the door-running transaction: reference-input index 1 committed, 0 at the door\./,
    );

    const tier3Plan = planFor(TIER3_PREIMAGE_BYTES);
    const tier3Inputs = [scriptReference, ...carriageInputsFor(tier3Plan)];
    // Tier 3 disagrees on the certificate index first, because the guard checks
    // the manifest before the chunk vector.
    expect(() =>
      assertAtDoor(tier3Plan, tier3Inputs, tier3Inputs.slice(1)),
    ).toThrowError(
      /^field 2 §8 carriage does not resolve at the door-running transaction: certificate index 3 committed, 2 at the door\./,
    );
  });

  it("refuses when the door transaction acquired a reference input ahead of the carriage", () => {
    const tier2Plan = planFor(TIER2_PREIMAGE_BYTES);
    const tier2Inputs = [scriptReference, ...carriageInputsFor(tier2Plan)];
    expect(() =>
      assertAtDoor(tier2Plan, tier2Inputs, [
        ...tier2Inputs,
        lateReferenceInput,
      ]),
    ).toThrowError(
      /^field 2 §8 carriage does not resolve at the door-running transaction: reference-input index 1 committed, 2 at the door\./,
    );

    const tier3Plan = planFor(TIER3_PREIMAGE_BYTES);
    const tier3Inputs = [scriptReference, ...carriageInputsFor(tier3Plan)];
    expect(() =>
      assertAtDoor(tier3Plan, tier3Inputs, [
        ...tier3Inputs,
        lateReferenceInput,
      ]),
    ).toThrowError(
      /^field 2 §8 carriage does not resolve at the door-running transaction: certificate index 3 committed, 4 at the door\./,
    );
  });

  it("refuses a chunk-vector disagreement the certificate index alone would hide", () => {
    // §8.7 healing: chunk 0 is republished at a different out-ref carrying the
    // same bytes. The set keeps its size, so the manifest's index does not
    // move — only the chunk *vector* reorders, which is the disagreement a
    // guard that stopped at the first scalar comparison would pass.
    const plan = planFor(TIER3_PREIMAGE_BYTES);
    const carriageInputs = carriageInputsFor(plan);
    const committed = [scriptReference, ...carriageInputs];
    const healedChunkZero = carriageUtxo({
      txHash: "05".repeat(32),
      outputIndex: 0,
      address: PROVER_KEY_ADDRESS,
      datum: carriageInputs[0]!.datum!,
    });
    const committedCarriage = resolveAgainst(plan, committed);
    expect(
      committedCarriage.carriage === "Certified"
        ? committedCarriage.chunkRefInputIndices
        : null,
    ).toStrictEqual([1, 2]);
    expect(() =>
      assertMidgardFieldCarriageResolvesAtDoor({
        carriage: committedCarriage,
        plan,
        doorReferenceInputs: [
          scriptReference,
          healedChunkZero,
          ...carriageInputs.slice(1),
        ],
        certificatePolicyId: CERTIFICATE_POLICY_ID,
        label: "field 2",
      }),
    ).toThrowError(
      /^field 2 §8 carriage does not resolve at the door-running transaction: chunk indices \[1,2\] committed, \[2,1\] at the door\./,
    );
  });

  it("has nothing to check for tier-1 carriage, which indexes nothing", () => {
    const plan = planFor(1_024);
    expect(plan.tier).toBe("Inline");
    expect(() =>
      assertMidgardFieldCarriageResolvesAtDoor({
        carriage: resolveMidgardFieldCarriageAgainstReferenceInputs({
          plan,
          referenceInputs: [],
        }),
        plan,
        // A set that shares nothing with the committed one, which any
        // index-comparing branch would refuse.
        doorReferenceInputs: [lateReferenceInput],
        label: "field 2",
      }),
    ).not.toThrow();
  });
});
