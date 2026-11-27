import "./field-preimage-carriage-fit-emulator.8-7-healing-carriage-lost-or-corrupted-is-re-published-by-a-second-identity.js";

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

describe("§8.3 the tier-2 raw-UTxO bound", () => {
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
   * preimage or a tier-3 chunk. It measures at one-byte resolution, because a
   * frontier measured on a stride lattice is not a frontier.
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
    // the ladder uses.** The third term is the CBOR head of the inline datum's
    // byte-string wrapper, which steps at datum lengths 24, 256 and 65,536; the
    // payloads below sit on both sides of the first two. The third step is at a
    // 63,548-byte payload, which is above the §5.4 aggregate cap and so cannot
    // be a real publication at any `maxTxSize` — midgard-core's unit suite pins
    // it analytically instead.
    for (const payloadBytes of [
      22, 23, 63, 64, 65, 245, 246, 548, 1_000, 8_000, 14_336, 15_148, 15_644,
    ]) {
      const signedBytes = await measure(payloadBytes);
      expect({ payloadBytes, signedBytes }).toEqual({
        payloadBytes,
        signedBytes: midgardCarriagePublicationBytes(payloadBytes),
      });
      // Whatever is left after the payload's own Plutus Data encoding is the
      // fixed 245 bytes plus that datum's own head.
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

  it("measures the largest publishable preimage and K against it", async () => {
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

    expect(MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES).toBe(15_644);
    expect(MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES).toBe(15_148);
    // "Lands on `maxTxSize`" is asserted as an equality, because that is the
    // claim §8.10's row makes.
    expect(exactSigned).toBe(MAX_L1_TX_BYTES);
    expect(exactOverSigned).toBe(MAX_L1_TX_BYTES + 1);
    expect(reliableSigned).toBe(MAX_L1_TX_BYTES - RELIABILITY_RESERVE_BYTES);
    expect(reliableOverSigned).toBe(
      MAX_L1_TX_BYTES - RELIABILITY_RESERVE_BYTES + 1,
    );

    // `K` is the reliable frontier, so a full-`K` publication clears
    // `maxTxSize` with the 512-byte reserve intact, which is the property that
    // makes every chunk of every tier-3 plan publishable.
    expect(MIDGARD_CHUNK_BYTES_K).toBe(MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES);
    expect(atKSigned).toBe(15_872);

    // The same Plutus Data chunking cost applies to redeemer carriage, and it
    // is 450 bytes of the 2,048-byte allowance before any step machinery
    // exists.
    expect(
      midgardCarriageDataByteStringBytes(
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
      ),
    ).toBe(14_786);
  }, 300_000);
});
