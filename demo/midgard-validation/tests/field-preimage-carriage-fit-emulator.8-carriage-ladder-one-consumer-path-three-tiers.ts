import {
  layOutMidgardFieldCarriage,
  MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
  midgardCarriagePublicationBytes,
  type MidgardFieldCarriagePlan,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  authenticatedMidgardFieldView,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  midgardFieldCommitment,
  midgardFieldItemAt,
  midgardFieldItemCount,
  type MidgardFieldView,
  type ResolvedCarriageReferenceInput,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  fieldPreimagePublicationBytes,
  fieldPreimagePublicationOutputs,
} from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  FIELD_INDEX,
  fieldPreimage,
  type Harness,
  itemCountForPreimageBytes,
  itemPayloadAt,
  keyHash,
  MAX_L1_TX_BYTES,
  publicationOutputFor,
  publish,
  PUBLISHER_ADDRESS,
  PUBLISHER_KEY,
  RELIABILITY_RESERVE_BYTES,
  setupEmulator,
  STRIDE,
  TX_ID,
} from "./field-preimage-carriage-fit-emulator.publish.js";

/**
 * The consuming step, and the whole of the "tier is invisible" claim.
 *
 * It takes a carriage layout and the reference inputs that layout indexes, and
 * reads an item. There is no tier parameter, no branch on one, and no way to
 * tell from this function which rung of the ladder it is standing on — which is
 * §8's simplest-fitting-first mandate as a property of the code rather than a
 * convention.
 */
export const readItemThroughTheDoor = ({
  plan,
  referenceInputs,
  index,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly referenceInputs: readonly ResolvedCarriageReferenceInput[];
  readonly index: number;
}): { readonly view: MidgardFieldView; readonly item: Buffer } => {
  const layout = layOutMidgardFieldCarriage({ plan });
  const view = authenticatedMidgardFieldView({
    fieldIndex: plan.fieldIndex,
    txId: plan.txId,
    expectedCommitment: plan.commitment,
    carriage: layout.carriage,
    referenceInputs,
  });
  return { view, item: midgardFieldItemAt(view, index) };
};

/**
 * Resolves a plan's carriage out of the ledger the way a step builder does:
 * the manifest first, then the chunks in §8.4 order, each located by the
 * content of its datum rather than by UTxO identity (§8.7).
 */
export const resolveReferenceInputs = ({
  plan,
  utxos,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly utxos: readonly UTxO[];
}): readonly ResolvedCarriageReferenceInput[] => {
  const layout = layOutMidgardFieldCarriage({ plan });
  if (layout.carriage.carriage === "Inline") {
    return [];
  }
  const chunkInputs = plan.publications.map((publication) => {
    const expected = publicationOutputFor(
      plan,
      publication.chunkIndex,
    ).datumCbor;
    const utxo = utxos.find((candidate) => candidate.datum === expected);
    if (utxo === undefined) {
      throw new Error(
        `carriage for chunk ${publication.chunkIndex.toString()} is not on the ledger`,
      );
    }
    const datum = utxo.datum;
    if (datum === undefined || datum === null) {
      throw new Error("resolved carriage UTxO carries no inline datum");
    }
    return { inlineDatumBytes: fieldPreimagePublicationBytes(datum) };
  });
  if (layout.carriage.carriage === "RawUtxo") {
    return chunkInputs;
  }
  const certificate = plan.certificate;
  const certificateAssetName = plan.certificateAssetName;
  if (certificate === null || certificateAssetName === null) {
    throw new Error("tier-3 plan carries no certificate");
  }
  return [{ certificate, certificateAssetName }, ...chunkInputs];
};

/**
 * A tier-3 plan's reference inputs with one chunk's bytes replaced — what a
 * builder that resolved carriage by *anything other than content* would hand
 * the door.
 *
 * It bypasses {@link resolveReferenceInputs} deliberately. That helper refuses
 * first, by content, which is correct and is asserted on its own; but a
 * refusal that never reaches the door is not evidence about the door, and the
 * door is what §8.6's digest vector is for.
 */
export const referenceInputsWithSubstitutedChunk = ({
  plan,
  chunkIndex,
  bytes,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly chunkIndex: number;
  readonly bytes: Buffer;
}): readonly ResolvedCarriageReferenceInput[] => {
  const certificate = plan.certificate;
  const certificateAssetName = plan.certificateAssetName;
  if (certificate === null || certificateAssetName === null) {
    throw new Error("tier-3 plan carries no certificate");
  }
  return [
    { certificate, certificateAssetName },
    ...plan.publications.map((publication) => ({
      inlineDatumBytes:
        publication.chunkIndex === chunkIndex ? bytes : publication.bytes,
    })),
  ];
};

describe("§8 carriage ladder — tier selection is a partition", () => {
  it("assigns exactly one tier to every length up to the §5.4 cap", () => {
    const planFor = (bytes: number): MidgardFieldCarriagePlan =>
      planMidgardFieldCarriage({
        owner: keyHash(PUBLISHER_KEY),
        txId: TX_ID,
        fieldIndex: FIELD_INDEX,
        preimage: fieldPreimage(itemCountForPreimageBytes(bytes)),
      });

    // The boundaries are the interesting inputs: one byte either side of each
    // rung, so a tier chosen with the wrong comparison operator is visible.
    expect(planFor(1).tier).toBe("Inline");
    expect(planFor(MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES).tier).toBe(
      "Inline",
    );
    expect(planFor(MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES + 40).tier).toBe(
      "RawUtxo",
    );
    expect(planFor(MIDGARD_CHUNK_BYTES_K).tier).toBe("RawUtxo");
    expect(planFor(MIDGARD_CHUNK_BYTES_K + 40).tier).toBe("Certified");
    const corner = planFor(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES);
    expect(corner.tier).toBe("Certified");
    expect(corner.publications.length).toBe(3);
    expect(corner.publications.map((entry) => entry.bytes.length)).toEqual([
      MIDGARD_CHUNK_BYTES_K,
      MIDGARD_CHUNK_BYTES_K,
      corner.totalLength - 2 * MIDGARD_CHUNK_BYTES_K,
    ]);
  });

  it("refuses a preimage above the §5.4 aggregate cap", () => {
    expect(() =>
      planMidgardFieldCarriage({
        owner: keyHash(PUBLISHER_KEY),
        txId: TX_ID,
        fieldIndex: FIELD_INDEX,
        preimage: Buffer.alloc(
          MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES + 1,
        ),
      }),
    ).toThrow();
  });
});

describe("§8 carriage ladder — one consumer path, three tiers", () => {
  let harness: Harness;

  beforeAll(async () => {
    // Inflated, and only because of §8.3 erratum E1: the tier-3 case publishes
    // full-`K` chunks, and at the currently-pinned `K` those measure 16,648
    // bytes and the real ledger refuses them. The block asserts that overrun
    // explicitly below rather than letting the inflated limit hide it.
    harness = await setupEmulator();
  });

  it("carries a preimage of every tier to a dispute read with no tier branch", async () => {
    const cases = [
      { label: "tier 1 — redeemer", bytes: 4_000, tier: "Inline" as const },
      { label: "tier 2 — raw UTxO", bytes: 15_000, tier: "RawUtxo" as const },
      {
        label: "tier 3 — certified chunks",
        bytes: MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
        tier: "Certified" as const,
      },
    ];

    for (const testCase of cases) {
      const itemCount = itemCountForPreimageBytes(testCase.bytes);
      const preimage = fieldPreimage(itemCount);
      const plan = planMidgardFieldCarriage({
        owner: keyHash(PUBLISHER_KEY),
        txId: TX_ID,
        fieldIndex: FIELD_INDEX,
        preimage,
      });
      expect(plan.tier).toBe(testCase.tier);
      expect(plan.commitment).toEqual(midgardFieldCommitment(preimage));

      for (const publication of fieldPreimagePublicationOutputs(plan)) {
        const result = await publish({
          harness,
          publication,
          signerKey: PUBLISHER_KEY,
          address: PUBLISHER_ADDRESS,
        });
        // The E1 gate, stated per publication rather than assumed for the
        // block: a chunk at or under the frontier really does fit the real
        // limit, and a full-`K` chunk really does not. The second half is the
        // live defect, and it is asserted as an exact overrun so that the day
        // `K` is re-pinned this line turns red and has to be revisited rather
        // than silently continuing to pass.
        if (publication.byteLength <= MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES) {
          expect(result.signedBytes).toBeLessThanOrEqual(
            MAX_L1_TX_BYTES - RELIABILITY_RESERVE_BYTES,
          );
        } else {
          expect(publication.byteLength).toBe(MIDGARD_CHUNK_BYTES_K);
          expect(result.signedBytes).toBe(16_648);
          expect(result.signedBytes - MAX_L1_TX_BYTES).toBe(264);
        }
        // Whatever the size, the cost model the guard is built on reproduces
        // the real signed transaction to the byte.
        expect(midgardCarriagePublicationBytes(publication.byteLength)).toBe(
          result.signedBytes,
        );
      }
      const utxos = await harness.lucid.utxosAt(PUBLISHER_ADDRESS);
      const referenceInputs = resolveReferenceInputs({ plan, utxos });

      // The same three lines for every tier, and the last item of each — the
      // read most likely to be off by one, and under tier 3 the one that lands
      // in the ragged tail.
      const lastIndex = itemCount - 1;
      const { view, item } = readItemThroughTheDoor({
        plan,
        referenceInputs,
        index: lastIndex,
      });
      expect(midgardFieldItemCount(view)).toBe(itemCount);
      expect(item).toEqual(itemPayloadAt(preimage, lastIndex));

      // And an item that straddles a chunk boundary wherever there is one.
      if (plan.publications.length > 1) {
        const straddling = Math.floor((MIDGARD_CHUNK_BYTES_K - 3) / STRIDE);
        const straddled = readItemThroughTheDoor({
          plan,
          referenceInputs,
          index: straddling,
        });
        expect(straddled.item).toEqual(itemPayloadAt(preimage, straddling));
      }
    }
  });
});
