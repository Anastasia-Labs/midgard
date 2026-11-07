import "./field-preimage-carriage-fit-emulator.8-3-phase-4-exit-measurement-the-tier-2-raw-utx-o-bound.js";

import {
  healMidgardFieldCarriage,
  MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
  midgardFieldCarriagePlansAreInterchangeable,
  midgardFieldCarriagePublishability,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { certifyFieldPreimageRedeemer } from "@al-ft/midgard-sdk";
import { beforeAll, describe, expect, it } from "vitest";

import {
  readItemThroughTheDoor,
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
  MAX_L1_TX_BYTES,
  publicationOutputFor,
  publish,
  PUBLISHER_ADDRESS,
  PUBLISHER_KEY,
  rawPublicationOutput,
  RELIABILITY_RESERVE_BYTES,
  setupEmulator,
  TX_ID,
} from "./field-preimage-carriage-fit-emulator.publish.js";

describe("§8.3 erratum E1 — the publishable frontier is enforced, at the real maxTxSize", () => {
  let harness: Harness;

  beforeAll(async () => {
    // No inflation. Every publication in this block is judged by the ledger.
    harness = await setupEmulator();
  });

  it("submits a frontier-sized publication and refuses the byte past it", async () => {
    const atFrontier = await publish({
      harness,
      publication: rawPublicationOutput(
        Buffer.alloc(MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES, 0x5c),
      ),
      signerKey: PUBLISHER_KEY,
      address: PUBLISHER_ADDRESS,
    });
    // Submitted, on a ledger with maxTxSize = 16,384. This is the assertion
    // that could not have been written before: the previous revision ran every
    // block at 65,536 and nothing anywhere checked a publication against the
    // real limit.
    expect(atFrontier.utxo.txHash).not.toBe("");
    expect(atFrontier.signedBytes).toBe(
      MAX_L1_TX_BYTES - RELIABILITY_RESERVE_BYTES,
    );

    // One byte more and the guard refuses — before the ledger is asked, and
    // naming the erratum rather than surfacing a submission error.
    await expect(
      publish({
        harness,
        publication: rawPublicationOutput(
          Buffer.alloc(MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES + 1, 0x5c),
        ),
        signerKey: PUBLISHER_KEY,
        address: PUBLISHER_ADDRESS,
      }),
    ).rejects.toThrow("§8.3 erratum E1");
  }, 120_000);

  it("passes every chunk of the largest tier-3 plan, and still names one over a lowered budget", () => {
    const plan = planMidgardFieldCarriage({
      owner: keyHash(PUBLISHER_KEY),
      txId: TX_ID,
      fieldIndex: FIELD_INDEX,
      preimage: fieldPreimage(
        itemCountForPreimageBytes(
          MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
        ),
      ),
    });
    // This is E1's repair as an end-to-end property, at the largest plan the
    // format admits: three chunks, two of them full-`K`, and *every* one of them
    // publishable inside the reserve. Before the repair this row asserted the
    // opposite — chunks 0 and 1 unpublishable at 16,648 signed bytes apiece —
    // and that was the whole shape of the outage: every tier-3 plan there was
    // had a chunk the ledger would refuse.
    expect(plan.tier).toBe("Certified");
    expect(
      plan.publications.map((publication) => publication.bytes.length),
    ).toEqual([MIDGARD_CHUNK_BYTES_K, MIDGARD_CHUNK_BYTES_K, 2_467]);
    const report = midgardFieldCarriagePublishability({ plan });
    expect(report.publishable).toBe(true);
    expect(report.unpublishableChunks).toEqual([]);
    // The guard is not vacuous now that honest plans pass it: judged against a
    // budget one byte under the full-`K` publication it names exactly the two
    // full chunks and the overrun to the byte. Without this half the row would
    // be a gate that cannot fail.
    const tightened = midgardFieldCarriagePublishability({
      plan,
      budgetBytes: MAX_L1_TX_BYTES - RELIABILITY_RESERVE_BYTES - 1,
    });
    expect(tightened.publishable).toBe(false);
    expect(
      tightened.unpublishableChunks.map((chunk) => chunk.chunkIndex),
    ).toEqual([0, 1]);
    for (const chunk of tightened.unpublishableChunks) {
      expect(chunk.byteLength).toBe(MIDGARD_CHUNK_BYTES_K);
      expect(chunk.publicationBytes).toBe(
        MAX_L1_TX_BYTES - RELIABILITY_RESERVE_BYTES,
      );
      expect(chunk.overrunBytes).toBe(1);
    }
  });

  it("heals a frontier-sized tier-2 publication under real protocol parameters", async () => {
    // §8.7 healing at the size that matters. The tier-3 healing block above
    // runs inflated because tier 3 cannot be published at all right now; this
    // one runs at the real limit over the largest publication the ladder can
    // actually make, so "a yanked publication is healed" is demonstrated as a
    // property of the deployed parameters rather than of an emulator setting.
    const itemCount = itemCountForPreimageBytes(
      MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
    );
    const preimage = fieldPreimage(itemCount);
    const plan = planMidgardFieldCarriage({
      owner: keyHash(PUBLISHER_KEY),
      txId: TX_ID,
      fieldIndex: FIELD_INDEX,
      preimage,
    });
    expect(plan.tier).toBe("RawUtxo");
    expect(midgardFieldCarriagePublishability({ plan }).publishable).toBe(true);

    const original = await publish({
      harness,
      publication: publicationOutputFor(plan, 0),
      signerKey: PUBLISHER_KEY,
      address: PUBLISHER_ADDRESS,
    });
    expect(original.signedBytes).toBeLessThanOrEqual(
      MAX_L1_TX_BYTES - RELIABILITY_RESERVE_BYTES,
    );

    const lastIndex = itemCount - 1;
    const before = readItemThroughTheDoor({
      plan,
      referenceInputs: resolveReferenceInputs({
        plan,
        utxos: await harness.lucid.utxosAt(PUBLISHER_ADDRESS),
      }),
      index: lastIndex,
    });
    expect(before.item).toEqual(itemPayloadAt(preimage, lastIndex));

    harness.lucid.selectWallet.fromPrivateKey(PUBLISHER_KEY.to_bech32());
    const yankTx = await harness.lucid
      .newTx()
      .collectFrom([original.utxo])
      .complete({ localUPLCEval: true });
    await harness.lucid.awaitTx(
      await (await yankTx.sign.withWallet().complete()).submit(),
    );
    expect(() =>
      resolveReferenceInputs({
        plan,
        utxos: [],
      }),
    ).toThrow("carriage for chunk 0 is not on the ledger");

    const healedPlan = healMidgardFieldCarriage({
      healer: keyHash(HEALER_KEY),
      txId: TX_ID,
      fieldIndex: FIELD_INDEX,
      preimage,
    });
    expect(midgardFieldCarriagePlansAreInterchangeable(plan, healedPlan)).toBe(
      true,
    );
    const healed = await publish({
      harness,
      publication: publicationOutputFor(healedPlan, 0),
      signerKey: HEALER_KEY,
      address: HEALER_ADDRESS,
    });
    expect(healed.utxo.txHash).not.toBe("");
    expect(healed.signedBytes).toBe(original.signedBytes);

    const after = readItemThroughTheDoor({
      plan,
      referenceInputs: resolveReferenceInputs({
        plan,
        utxos: await harness.lucid.utxosAt(HEALER_ADDRESS),
      }),
      index: lastIndex,
    });
    expect(after.item).toEqual(before.item);
  }, 120_000);
});

/**
 * The §8.6 redeemer a certification carries, sized once.
 *
 * It re-carries no chunk bytes — only the committed structures and the
 * positional indices — which is why §8.3 says certification never constrains
 * `K`. The 400-byte compact structure and 100-byte witness set are the
 * stand-in shapes both cost claims below are quoted against, and they are
 * written once so the two cannot drift apart.
 */
export const CERTIFY_REDEEMER_BYTES =
  certifyFieldPreimageRedeemer({
    sourceKind: 0n,
    compactCbor: "ab".repeat(400),
    witnessSetCompactCbor: "cd".repeat(100),
    chunkRefInputIndices: [0, 1, 2],
    outputIndex: 0,
  }).length / 2;
