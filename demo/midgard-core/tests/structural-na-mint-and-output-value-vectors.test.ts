import { describe, expect, it } from "vitest";

import {
  decodeMidgardNativeMint,
  decodeMidgardTxOutput,
  decodeMidgardValue,
} from "../src/codec/index.js";

/**
 * TypeScript twin vectors for the GOAL_SPEC §9.3 structural-N/A closures of
 * rows Q24 (`ada-minted`) and Q25 (`negative-output-value`).
 *
 * Every hex string below is byte-identical to the constant of the same name in
 * the Aiken modules
 *   onchain/aiken/lib/midgard/fraud-proofs/native-tx/structural-na-q24-ada-minted.test.ak
 *   onchain/aiken/lib/midgard/fraud-proofs/native-tx/structural-na-q25-negative-output-value.test.ak
 * so the same bytes are proven unrepresentable on both sides of the boundary.
 */

const bytes = (hex: string): Uint8Array => Buffer.from(hex, "hex");

// --- Q24 vectors -------------------------------------------------------------

/**
 * Field-5 preimages under §5.6's enveloped per-policy grammar:
 * `81` array(1) then `bytes(enc_5)`, where `enc_5` is
 * `82 ‖ bytes(policy_id) ‖ map(k) ‖ asset entries`.
 *
 * Byte-identical to `ada_mint_field_preimage_cbor` and
 * `real_mint_field_preimage_cbor` in the Aiken module, verified by deriving them
 * through `encodeMidgardFieldPreimageV1` and comparing.
 */
const ADA_MINT_FIELD_PREIMAGE_CBOR = "81458240a14005";

/** The adjacent well-formed 28-byte-policy control. */
const REAL_MINT_FIELD_PREIMAGE_CBOR =
  "81582382581c01010101010101010101010101010101010101010101010101010101a1410105";

/** The same control with a 27-byte policy id (one byte short). */
const SHORT_MINT_FIELD_PREIMAGE_CBOR =
  "81582282581b0101010101010101010101010101010101010101010101010101ffa1410105";

/**
 * The retired raw-map form of the positive control, kept as a negative: §5.6
 * replaced it, and §6.1 admits one byte form per value. The Aiken twin proves the
 * same refusal in `q24_retired_raw_map_mint_preimage_is_refused`.
 */
const RETIRED_RAW_MAP_MINT_PREIMAGE_CBOR =
  "a1581c01010101010101010101010101010101010101010101010101010101a1410105";

// --- Q25 vectors -------------------------------------------------------------

const NEGATIVE_LOVELACE_OUTPUT =
  "a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018220a0";
const ZERO_LOVELACE_OUTPUT =
  "a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0";
const ZERO_QUANTITY_OUTPUT =
  "a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c01010101010101010101010101010101010101010101010101010101a1410100";
const NEGATIVE_QUANTITY_OUTPUT =
  "a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c01010101010101010101010101010101010101010101010101010101a1410120";
const POSITIVE_QUANTITY_OUTPUT =
  "a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c01010101010101010101010101010101010101010101010101010101a1410101";

const POLICY_ID_HEX =
  "01010101010101010101010101010101010101010101010101010101";

describe("Q24 ada-minted is unrepresentable in canonical V1", () => {
  it("rejects a mint entry whose policy id is the ADA asset class", () => {
    expect(() =>
      decodeMidgardNativeMint(bytes(ADA_MINT_FIELD_PREIMAGE_CBOR)),
    ).toThrow(/mint policy id must be 28 bytes/i);
  });

  it("rejects a mint entry whose policy id is 27 bytes", () => {
    expect(() =>
      decodeMidgardNativeMint(bytes(SHORT_MINT_FIELD_PREIMAGE_CBOR)),
    ).toThrow(/mint policy id must be 28 bytes/i);
  });

  it("rejects the retired raw-map mint preimage form", () => {
    expect(() =>
      decodeMidgardNativeMint(bytes(RETIRED_RAW_MAP_MINT_PREIMAGE_CBOR)),
    ).toThrow(/not a §5\.1 definite array header/u);
  });

  it("accepts the adjacent 28-byte-policy control", () => {
    const decoded = decodeMidgardNativeMint(
      bytes(REAL_MINT_FIELD_PREIMAGE_CBOR),
    );
    expect(decoded?.policyIds).toEqual([POLICY_ID_HEX]);
  });

  it("accepts the empty mint preimage", () => {
    expect(decodeMidgardNativeMint(bytes("80"))).toBeUndefined();
  });
});

describe("Q25 negative-output-value is unrepresentable in canonical V1", () => {
  it("rejects an output value carrying negative lovelace", () => {
    expect(() =>
      decodeMidgardTxOutput(bytes(NEGATIVE_LOVELACE_OUTPUT)),
    ).toThrow();
    expect(() => decodeMidgardValue(bytes("8220a0"))).toThrow();
  });

  it("rejects an output value carrying a zero asset quantity", () => {
    expect(() => decodeMidgardTxOutput(bytes(ZERO_QUANTITY_OUTPUT))).toThrow(
      /Value asset quantity cannot be zero/,
    );
  });

  it("rejects an output value carrying a negative asset quantity", () => {
    expect(() =>
      decodeMidgardTxOutput(bytes(NEGATIVE_QUANTITY_OUTPUT)),
    ).toThrow();
  });

  it("accepts the adjacent zero-lovelace control", () => {
    const output = decodeMidgardTxOutput(bytes(ZERO_LOVELACE_OUTPUT));
    expect(output.value.lovelace).toBe(0n);
    expect(output.value.assets.size).toBe(0);
  });

  it("accepts the adjacent positive-quantity control", () => {
    const output = decodeMidgardTxOutput(bytes(POSITIVE_QUANTITY_OUTPUT));
    expect(output.value.lovelace).toBe(0n);
    expect([...(output.value.assets.get(POLICY_ID_HEX) ?? new Map())]).toEqual([
      ["01", 1n],
    ]);
  });
});
