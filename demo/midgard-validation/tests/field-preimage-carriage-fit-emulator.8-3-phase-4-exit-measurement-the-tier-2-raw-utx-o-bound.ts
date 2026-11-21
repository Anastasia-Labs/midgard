import "./field-preimage-carriage-fit-emulator.8-7-healing-carriage-lost-or-corrupted-is-re-published-by-a-second-identity.js";

import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core";
import {
  MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES,
  MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
  midgardCarriageDataByteStringBytes,
  midgardCarriagePublicationBytes,
  midgardCarriagePublicationFramingBytes,
} from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { beforeAll, describe, expect, it } from "vitest";

import {
  type Harness,
  MAX_L1_TX_BYTES,
  MEASUREMENT_MAX_TX_BYTES,
  MEASUREMENT_PUBLICATION_BUDGET_BYTES,
  publish,
  PUBLISHER_ADDRESS,
  PUBLISHER_KEY,
  rawPublicationOutput,
  RELIABILITY_RESERVE_BYTES,
  setupEmulator,
} from "./field-preimage-carriage-fit-emulator.publish.js";

describe("§8.3 Phase-4 exit measurement — the tier-2 raw-UTxO bound", () => {
  let harness: Harness;

  beforeAll(async () => {
    // The only block that legitimately builds past the limit, because that is
    // what finding a frontier means.
    harness = await setupEmulator({ maxTxSize: MEASUREMENT_MAX_TX_BYTES });
  });

  /**
   * The signed size of a real §8.5 publication of `bytes` payload bytes.
   *
   * Measured over raw payload bytes rather than over a plan, because a plan
   * below the tier-1 bound publishes nothing at all: the question is what a
   * nothing-but-bytes publication transaction costs at a given payload size,
   * and that is the same transaction whether the payload is a whole tier-2
   * preimage or a tier-3 chunk.
   *
   * **At one-byte resolution.** An earlier revision measured at the field-1
   * stride of 40 bytes and published the largest stride-quantised preimage
   * under each frontier as the frontier — 15,643 and 15,123, which are one and
   * twenty-five bytes short. A frontier measured on a lattice is not a
   * frontier, and the exact row's own stated property ("lands on `maxTxSize`")
   * was false of the transaction it reported.
   */
  const measure = async (bytes: number): Promise<number> => {
    const payload = Buffer.from(
      Array.from({ length: bytes }, (_unused, index) => (index * 7 + 3) & 0xff),
    );
    const result = await publish({
      harness,
      publication: rawPublicationOutput(payload),
      signerKey: PUBLISHER_KEY,
      address: PUBLISHER_ADDRESS,
      submit: false,
      maxPublicationBytes: MEASUREMENT_PUBLICATION_BUDGET_BYTES,
    });
    return result.signedBytes;
  };

  it("reproduces the real signed transaction size from the three-term cost model", async () => {
    // The frontiers below are *derived* from `midgardCarriagePublicationBytes`
    // rather than searched for, which is only sound if that function is the
    // truth. This is where that is established: across four orders of magnitude
    // of payload, the model and the real signed emulator transaction agree to
    // the byte.
    //
    // **The samples straddle the boundaries the claim needs, not just the sizes
    // the ladder uses.** An earlier revision sampled only inside the band where
    // the non-payload framing happens to be a flat 248 bytes, and then published
    // "two terms and no third" — a claim its own sample set was structurally
    // incapable of falsifying. The third term is the CBOR head of the inline
    // datum's byte-string wrapper, which steps at datum lengths 24, 256 and
    // 65,536; the payloads below sit on both sides of the first two. The third
    // step is at a 63,548-byte payload, which is above the §5.4 aggregate cap
    // and so cannot be a real publication at any `maxTxSize` — it is pinned
    // analytically in midgard-core's unit suite instead, and it is pinned
    // precisely because it is the step the collapsed model got *optimistically*
    // wrong.
    for (const payloadBytes of [
      22, 23, 63, 64, 65, 245, 246, 548, 1_000, 8_000, 14_336, 15_148, 15_644,
      15_900,
    ]) {
      const signedBytes = await measure(payloadBytes);
      expect({ payloadBytes, signedBytes }).toEqual({
        payloadBytes,
        signedBytes: midgardCarriagePublicationBytes(payloadBytes),
      });
      // And the decomposition §8.3 E1 publishes: whatever is left after the
      // payload's own Plutus Data encoding is the fixed 245 bytes plus that
      // datum's own head. This is the assertion that would catch the claim E1
      // corrected — that the 740 bytes measured at the frontier were a floor
      // for the family rather than a figure that moves with the payload.
      const datumBytes = midgardCarriageDataByteStringBytes(payloadBytes);
      expect(signedBytes - datumBytes).toBe(
        midgardCarriagePublicationFramingBytes(datumBytes),
      );
    }

    // The step is real and measured, not merely modelled: the same family of
    // transaction costs 246, 247 and 248 bytes of non-payload framing at these
    // three payloads. A model that collapsed them to a constant would have to
    // be wrong at two of the three.
    expect(await measure(22)).toBe(269);
    expect(await measure(23)).toBe(271);
    expect(await measure(246)).toBe(504);
  }, 180_000);

  it("measures the largest publishable preimage and re-pins K against it", async () => {
    // A frontier is a pair of adjacent measurements, not a single one: the
    // largest payload that fits, and the smallest that does not. Both are real
    // signed transactions.
    const exactSigned = await measure(MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES);
    const exactOverSigned = await measure(
      MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES + 1,
    );
    const reliableSigned = await measure(
      MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
    );
    const reliableOverSigned = await measure(
      MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES + 1,
    );
    const atKSigned = await measure(MIDGARD_CHUNK_BYTES_K);

    // Reported so a re-take reads the numbers off the report rather than
    // reconstructing them. This is the Phase-4 measurement §8.3 declared
    // itself provisional pending.
    console.log(
      JSON.stringify({
        measurement: "tier2-raw-utxo-bound-v1",
        maxL1TxBytes: MAX_L1_TX_BYTES,
        reliabilityReserveBytes: RELIABILITY_RESERVE_BYTES,
        supersededChunkBytesK: MIDGARD_CHUNK_BYTES_K,
        exactFrontierPreimageBytes: MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES,
        exactFrontierTransactionBytes: exactSigned,
        firstUnpublishablePreimageBytes:
          MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES + 1,
        firstUnpublishableTransactionBytes: exactOverSigned,
        reliableFrontierPreimageBytes: MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
        reliableFrontierTransactionBytes: reliableSigned,
        transactionBytesAtSupersededK: atKSigned,
        supersededKOverrunBytes: atKSigned - MAX_L1_TX_BYTES,
        framingBytesAtExactFrontier:
          exactSigned - MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES,
        // Non-datum framing *at this datum size* — 245 fixed plus a 3-byte
        // head. Reported under a name that says so, because an earlier
        // revision called the same subtraction "fixed" and that is what let
        // the head hide inside it.
        nonDatumFramingBytesAtExactFrontier:
          exactSigned -
          midgardCarriageDataByteStringBytes(
            MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES,
          ),
        countedEraExactFrontier:
          MIDGARD_ENVELOPE_MEASUREMENTS.maxExactCompleteItemPublicationBytes,
        countedEraReliableFrontier:
          MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableCompleteItemPublicationBytes,
        countedEraAppliedCap:
          MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes,
      }),
    );

    // The frontiers are pinned to the byte, the way the counted era pinned
    // `maxExactCompleteItemPublicationBytes` and
    // `maxReliableCompleteItemPublicationBytes`: spec §8.10 and §8.3's erratum
    // quote these numbers, so an environment or builder change that moves them
    // has to move the document too rather than drifting away from it.
    expect(MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES).toBe(15_644);
    expect(MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES).toBe(15_148);
    // "Lands on `maxTxSize`" is asserted as an equality, because that is the
    // claim §8.10's row makes and the previous revision's 16,383 did not meet.
    expect(exactSigned).toBe(MAX_L1_TX_BYTES);
    expect(exactOverSigned).toBe(MAX_L1_TX_BYTES + 1);
    expect(reliableSigned).toBe(MAX_L1_TX_BYTES - RELIABILITY_RESERVE_BYTES);
    expect(reliableOverSigned).toBe(
      MAX_L1_TX_BYTES - RELIABILITY_RESERVE_BYTES + 1,
    );

    // The claim #574 owes as the flat successor to the counted-era bound —
    // stated against the counted **frontiers**, which are the like-for-like
    // quantities. `maxSinglePublicationCompleteItemBytes` = 14,396 is an
    // applied policy cap with 1,128 bytes of unused headroom, and comparing a
    // frontier to a cap measures the cap's safety margin rather than the
    // format's gain; the previous revision did that and reported +1,247 B,
    // about 17× the real figure.
    //
    // **CORRECTED 2026-08-14 (owner ruling): the gain was +74 / +75 B, not
    // +155 / +155.** The two counted-era frontiers this subtracts from were
    // themselves ~80 bytes low — 14,993/15,489 where the counted publisher
    // actually reaches 15,073/15,570, as the three sibling measurements in the
    // same `MIDGARD_ENVELOPE_MEASUREMENTS` block (datum bytes, min-Ada, fee)
    // had said all along. The flat format's real gain over the counted era is
    // therefore about half what was claimed. The overstated figure is not
    // preserved anywhere as if it still held; the measured one replaces it here
    // and in §8.10 of `docs/spec/midgard-tx.md`.
    expect(
      MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES -
        MIDGARD_ENVELOPE_MEASUREMENTS.maxExactCompleteItemPublicationBytes,
    ).toBe(74);
    expect(
      MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES -
        MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableCompleteItemPublicationBytes,
    ).toBe(75);
    // The counted reliable publication and the flat reliable frontier land in
    // the same size of transaction. That is what makes the two gains comparable
    // at all. They differ by one byte rather than being equal, and that byte is
    // accounted for: across the 512-byte reserve the counted shape's non-payload
    // framing steps by 15 (814 B at 15,570 -> 799 B at 15,073) while the flat
    // shape's steps by 16 (740 B at 15,644 -> 724 B at 15,148), so 16 - 15 = 1.
    // Two ends of one gain, not two different gains.
    expect(reliableSigned).toBe(
      MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableCompleteItemPublicationTransactionBytes,
    );

    // §8.3's provisional `K = 15,900` was **falsified** by this measurement: a
    // 15,900-byte publication measured 16,648 signed bytes, 264 over
    // `maxTxSize`, reserve or no reserve. Erratum E1 recorded that and re-pinned
    // `K` to the reliable frontier, and the re-pin has now landed — so what this
    // block measures today is the repaired ladder rather than the outage.
    //
    // A full-`K` publication is exactly the reliable frontier's transaction: it
    // clears `maxTxSize` with the 512-byte reserve intact, which is the property
    // that makes every chunk of every tier-3 plan publishable.
    expect(atKSigned).toBe(MAX_L1_TX_BYTES - RELIABILITY_RESERVE_BYTES);
    expect(atKSigned).toBe(15_872);
    expect(MAX_L1_TX_BYTES - atKSigned).toBe(RELIABILITY_RESERVE_BYTES);

    // E1's repair, as an exact equality rather than an inequality: `K` **is**
    // the reliable frontier. This is the one line that says the erratum is
    // closed, and it is written as `=== 0` on purpose — any future re-pin of
    // either half that does not move the other turns it red instead of leaving a
    // silent gap, which is what the superseded 752-byte gap was.
    expect(MIDGARD_CHUNK_BYTES_K - MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES).toBe(
      0,
    );
    // And the superseded value, kept as a measurement rather than as prose: it
    // is still over the limit, which is why it is no longer `K`.
    expect(midgardCarriagePublicationBytes(15_900)).toBe(16_648);
    expect(midgardCarriagePublicationBytes(15_900) - MAX_L1_TX_BYTES).toBe(264);

    // §8.3 E1's tier-1 note: the same Plutus Data chunking cost applies to
    // redeemer carriage, and it is 450 bytes of the 2,048-byte allowance before
    // any step machinery exists.
    expect(
      midgardCarriageDataByteStringBytes(
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
      ),
    ).toBe(14_786);
  }, 300_000);
});
