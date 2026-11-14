import {
  healMidgardFieldCarriage,
  type MidgardFieldCarriagePlan,
  midgardFieldCarriagePlansAreInterchangeable,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  midgardFieldCommitment,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  fieldPreimagePublicationDatumCbor,
  fieldPreimagePublicationOutputs,
} from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { beforeEach, describe, expect, it } from "vitest";

import {
  readItemThroughTheDoor,
  referenceInputsWithSubstitutedChunk,
  resolveReferenceInputs,
} from "./field-preimage-carriage-fit-emulator.8-carriage-ladder-one-consumer-path-three-tiers.js";
import {
  FIELD_INDEX,
  fieldPreimage,
  type Harness,
  HEALER_ADDRESS,
  HEALER_KEY,
  itemCountForPreimageBytes,
  itemPayloadAt,
  keyHash,
  publicationOutputFor,
  publish,
  PUBLISHER_ADDRESS,
  PUBLISHER_KEY,
  setupEmulator,
  STRIDE,
  TX_ID,
} from "./field-preimage-carriage-fit-emulator.publish.js";

describe("§8.7 healing — carriage lost or corrupted is re-published by a second identity", () => {
  let harness: Harness;

  // A **fresh ledger per test**, not per block. Carriage is addressed by
  // content, so two tests that publish the same corner leave two UTxOs carrying
  // byte-identical datums — and a yank in the second test would then be healed,
  // silently and wrongly, by the first test's leftover. That is exactly the
  // class of false green a content-addressed resolver invites, and a shared
  // `beforeAll` walks into it.
  beforeEach(async () => {
    // Inflated for the same reason as the ladder block, and for no other: a
    // tier-3 corner cannot be published at the currently-pinned `K` (§8.3 E1).
    // The tier-2 heal at the end of this file runs at the real limit.
    harness = await setupEmulator();
  });

  const publishCorner = async (): Promise<{
    readonly preimage: Buffer;
    readonly itemCount: number;
    readonly plan: MidgardFieldCarriagePlan;
    readonly published: readonly UTxO[];
  }> => {
    const itemCount = itemCountForPreimageBytes(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    const preimage = fieldPreimage(itemCount);
    const plan = planMidgardFieldCarriage({
      owner: keyHash(PUBLISHER_KEY),
      txId: TX_ID,
      fieldIndex: FIELD_INDEX,
      preimage,
    });
    expect(plan.tier).toBe("Certified");
    expect(plan.publications.length).toBe(3);
    const published: UTxO[] = [];
    for (const publication of fieldPreimagePublicationOutputs(plan)) {
      const result = await publish({
        harness,
        publication,
        signerKey: PUBLISHER_KEY,
        address: PUBLISHER_ADDRESS,
      });
      published.push(result.utxo);
    }
    return { preimage, itemCount, plan, published };
  };

  /**
   * The item index that lands inside chunk `chunkIndex`, so a yank of that
   * chunk is a yank of the bytes the read actually needs. Derived rather than
   * hard-coded: an off-by-one here would make a refusal attributable to the
   * wrong chunk.
   */
  const itemIndexInsideChunk = (chunkIndex: number): number =>
    Math.floor((MIDGARD_CHUNK_BYTES_K * chunkIndex + 200) / STRIDE);

  // §8.7 names two ways carriage stops being usable, and #565 claims healing
  // for both. They are different failures — one removes the UTxO, the other
  // leaves a UTxO carrying the wrong bytes — so they are exercised separately.
  //
  // The full-`K` chunk 0 is deliberately first. It is the largest publication
  // the ladder produces and the one that sits exactly on erratum E1's reliable
  // frontier; healing only the 2,467-byte ragged tail, as an earlier revision of
  // this file did, exercised the one chunk whose publication was never in
  // question.
  for (const chunkIndex of [0, 2]) {
    it(`survives a yank of chunk ${chunkIndex.toString()}: the original certificate still describes healed bytes`, async () => {
      const { preimage, plan, published } = await publishCorner();
      const readIndex = itemIndexInsideChunk(chunkIndex);
      const chunkBytes = plan.publications[chunkIndex]?.bytes.length;
      // The corner's ragged tail: 32,763 − 2·15,148 = 2,467 bytes at E1's
      // repaired `K`. It was 963 at the superseded 15,900.
      expect(chunkBytes).toBe(chunkIndex === 2 ? 2_467 : MIDGARD_CHUNK_BYTES_K);

      const beforeYank = readItemThroughTheDoor({
        plan,
        referenceInputs: resolveReferenceInputs({
          plan,
          utxos: await harness.lucid.utxosAt(PUBLISHER_ADDRESS),
        }),
        index: readIndex,
      });
      expect(beforeYank.item).toEqual(itemPayloadAt(preimage, readIndex));

      // The yank. §8.7's "mid-game yank" is the publisher spending their own
      // carriage out from under a dispute — an ordinary key spend, because raw
      // carriage is unauthenticated data at the publisher's own address and
      // nothing stops them.
      const yanked = published[chunkIndex];
      if (yanked === undefined) {
        throw new Error("expected three published chunks");
      }
      harness.lucid.selectWallet.fromPrivateKey(PUBLISHER_KEY.to_bech32());
      const yankTx = await harness.lucid
        .newTx()
        .collectFrom([yanked])
        .complete({ localUPLCEval: true });
      const yankHash = await (
        await yankTx.sign.withWallet().complete()
      ).submit();
      await harness.lucid.awaitTx(yankHash);

      // The dispute is now stuck, and stuck is what it must be: fail-closed,
      // never a wrong answer. Two refusals are asserted, and they are asserted
      // *separately* because they belong to different components — an earlier
      // revision asserted one `toThrow()` that the resolver's own error
      // satisfied, so the door was never reached and never tested.
      //
      // One: the step builder cannot assemble the reference inputs, because the
      // chunk is not on the ledger. That is this file's helper refusing, and it
      // is checked by message so it cannot be confused with the next one.
      const survivors = await harness.lucid.utxosAt(PUBLISHER_ADDRESS);
      expect(() => resolveReferenceInputs({ plan, utxos: survivors })).toThrow(
        `carriage for chunk ${chunkIndex.toString()} is not on the ledger`,
      );

      // Two, and this is the one that matters: suppose a builder *does* hand
      // the door something in that slot — the obvious mistake, some other
      // chunk's bytes. The door itself refuses, on the certificate's digest
      // vector, and the refusal comes from the SDK/codec rather than from this
      // file. Substituting the *neighbouring* chunk's real bytes makes it a
      // genuine wrong-chunk substitution and not merely malformed input.
      const substitute = plan.publications[1];
      if (substitute === undefined) {
        throw new Error("expected a neighbouring chunk to substitute");
      }
      expect(() =>
        readItemThroughTheDoor({
          plan,
          referenceInputs: referenceInputsWithSubstitutedChunk({
            plan,
            chunkIndex,
            bytes: substitute.bytes,
          }),
          index: readIndex,
        }),
      ).toThrow();

      // The heal. A second identity — different key, different UTxOs, different
      // min-Ada reclaim authority, no relationship to the original publisher —
      // re-derives the carriage from the preimage bytes alone.
      const healedPlan = healMidgardFieldCarriage({
        healer: keyHash(HEALER_KEY),
        txId: TX_ID,
        fieldIndex: FIELD_INDEX,
        preimage,
      });
      expect(
        midgardFieldCarriagePlansAreInterchangeable(plan, healedPlan),
      ).toBe(true);
      expect(healedPlan.certificate?.owner).not.toEqual(
        plan.certificate?.owner,
      );

      const healedPublication = publicationOutputFor(healedPlan, chunkIndex);
      // Byte-identical to what was yanked, which is the whole mechanism: the
      // certificate that already exists describes these bytes without being
      // re-minted.
      expect(healedPublication.datumCbor).toBe(
        publicationOutputFor(plan, chunkIndex).datumCbor,
      );
      expect(healedPublication.digestHex).toBe(
        publicationOutputFor(plan, chunkIndex).digestHex,
      );

      await publish({
        harness,
        publication: healedPublication,
        signerKey: HEALER_KEY,
        address: HEALER_ADDRESS,
      });

      // The dispute proceeds. Note the carriage now spans two addresses under
      // two owners, and the read neither knows nor cares — it resolves by
      // content.
      const healedUtxos = [
        ...(await harness.lucid.utxosAt(PUBLISHER_ADDRESS)),
        ...(await harness.lucid.utxosAt(HEALER_ADDRESS)),
      ];
      const afterHeal = readItemThroughTheDoor({
        plan,
        referenceInputs: resolveReferenceInputs({ plan, utxos: healedUtxos }),
        index: readIndex,
      });
      expect(afterHeal.item).toEqual(itemPayloadAt(preimage, readIndex));
      expect(afterHeal.item).toEqual(beforeYank.item);

      // And the *healer's* certificate is equally good over the *publisher's*
      // surviving chunks — interchangeable in both directions, which is what
      // "anyone's republication heals anyone's certificate" means.
      const healedCertificateRead = readItemThroughTheDoor({
        plan: healedPlan,
        referenceInputs: resolveReferenceInputs({
          plan: healedPlan,
          utxos: healedUtxos,
        }),
        index: readIndex,
      });
      expect(healedCertificateRead.item).toEqual(
        itemPayloadAt(preimage, readIndex),
      );
    }, 120_000);
  }

  it("survives a malicious publication: wrong bytes on the ledger are ignored, not consumed", async () => {
    // #565's other healing claim, and the one no test reached: the attack is
    // not always removal. A hostile party can publish *wrong* bytes for a chunk
    // and leave them sitting on the ledger next to the right ones. §8.7 says
    // content addressing makes that a non-event; this is that claim, exercised.
    const { preimage, plan, published } = await publishCorner();
    const readIndex = itemIndexInsideChunk(0);
    const victim = published[0];
    const genuine = plan.publications[0];
    if (victim === undefined || genuine === undefined) {
      throw new Error("expected a published chunk 0");
    }

    // The impostor: chunk 0's length, one byte different, published by the
    // *other* identity at their own address. Nothing forbids this; carriage is
    // unauthenticated data (§8.5).
    const corrupted = Buffer.from(genuine.bytes);
    corrupted[7] = (corrupted[7] ?? 0) ^ 0xff;
    await publish({
      harness,
      publication: {
        chunkIndex: 0,
        datumCbor: fieldPreimagePublicationDatumCbor(corrupted),
        byteLength: corrupted.length,
        digestHex: midgardFieldCommitment(corrupted).toString("hex"),
      },
      signerKey: HEALER_KEY,
      address: HEALER_ADDRESS,
    });

    // With both on the ledger, resolution by content picks the genuine one and
    // the read is unaffected. The impostor is not rejected — it is not seen.
    const both = [
      ...(await harness.lucid.utxosAt(PUBLISHER_ADDRESS)),
      ...(await harness.lucid.utxosAt(HEALER_ADDRESS)),
    ];
    const withImpostor = readItemThroughTheDoor({
      plan,
      referenceInputs: resolveReferenceInputs({ plan, utxos: both }),
      index: readIndex,
    });
    expect(withImpostor.item).toEqual(itemPayloadAt(preimage, readIndex));

    // And now yank the genuine one, leaving *only* the corrupted bytes. This is
    // the composite attack — remove the truth, leave a plausible lie — and the
    // door must still refuse rather than serve the lie.
    harness.lucid.selectWallet.fromPrivateKey(PUBLISHER_KEY.to_bech32());
    const yankTx = await harness.lucid
      .newTx()
      .collectFrom([victim])
      .complete({ localUPLCEval: true });
    await harness.lucid.awaitTx(
      await (await yankTx.sign.withWallet().complete()).submit(),
    );
    const survivors = [
      ...(await harness.lucid.utxosAt(PUBLISHER_ADDRESS)),
      ...(await harness.lucid.utxosAt(HEALER_ADDRESS)),
    ];
    expect(() => resolveReferenceInputs({ plan, utxos: survivors })).toThrow(
      "carriage for chunk 0 is not on the ledger",
    );
    // Forced past the resolver — a builder that matched on length instead of
    // content would produce exactly this — the door refuses on the digest.
    expect(() =>
      readItemThroughTheDoor({
        plan,
        referenceInputs: referenceInputsWithSubstitutedChunk({
          plan,
          chunkIndex: 0,
          bytes: corrupted,
        }),
        index: readIndex,
      }),
    ).toThrow();
  }, 120_000);
});
