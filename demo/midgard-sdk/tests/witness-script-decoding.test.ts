import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { acceptedVerdictSubject } from "../src/fraud-proof/proof-thread-substrate.js";
import {
  WITNESS_SCRIPT_DECODING_PHYSICAL_SCRIPTS,
  WitnessScriptDecodingBoundSchema,
  WitnessScriptDecodingScanStateSchema,
} from "../src/fraud-proof/witness-script-decoding.js";

const txId = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";

describe("witnessScriptDecoding V1 ABI", () => {
  it("pins the Aiken BoundWitnessScriptV1 Plutus-Data vector", () => {
    const encoded = Data.to(
      {
        subject: acceptedVerdictSubject(txId),
        witness_set_hash: "11".repeat(32),
        script_index: 0n,
        accused_class: -1n,
      } as never,
      WitnessScriptDecodingBoundSchema as never,
    );
    expect(encoded).toBe(
      "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff582011111111111111111111111111111111111111111111111111111111111111110020ff",
    );
  });

  it("round-trips the resumable scan state without field reordering", () => {
    const value = {
      bound: {
        subject: acceptedVerdictSubject(txId),
        witness_set_hash: "11".repeat(32),
        script_index: 0n,
        accused_class: -1n,
      },
      total_length: 36n,
      item_commitment: "22".repeat(32),
      control_cbor: "8801030018241824400000",
      next_expected_script_hash: "33".repeat(28),
      checkpoint_hash: "44".repeat(32),
      result_class: -1n,
    };
    expect(
      Data.from(
        Data.to(value as never, WitnessScriptDecodingScanStateSchema as never),
        WitnessScriptDecodingScanStateSchema as never,
      ),
    ).toEqual(value);
  });

  it("pins the physical titles and parameter order", () => {
    expect(
      WITNESS_SCRIPT_DECODING_PHYSICAL_SCRIPTS.map((script) => script.role),
    ).toEqual(["firstStep", "itemAuthenticator", "resumableScan", "terminal"]);
  });
});
