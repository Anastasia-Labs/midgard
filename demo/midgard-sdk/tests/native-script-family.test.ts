import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  MissingNativeScriptUtxoStep03SpendRedeemerSchema,
  MissingNativeScriptUtxoStep04DatumSchema,
} from "../src/fraud-proof/missing-native-script-utxo.js";
import {
  NativeScriptInvalidStep02SpendRedeemerSchema,
  NativeScriptInvalidStep03DatumSchema,
  NativeScriptInvalidStep03SpendRedeemerSchema,
} from "../src/fraud-proof/native-script-invalid.js";

const h28 = "11".repeat(28);
const h32 = "22".repeat(32);
const inlineOpening = {
  WitnessFieldOpening: {
    native_tx_compact_cbor: "80",
    witness_set: {
      script_tx_wits_hash: h32,
      addr_tx_wits_hash: h32,
      redeemer_tx_wits_hash: h32,
    },
    carriage: { Inline: { preimage: "80" } },
  },
} as const;

describe("Q33/Q34 native-script family wire codecs", () => {
  it("round-trips Q33 predecessor membership and native credential state", () => {
    const membership = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          out_ref: { transactionId: h32, outputIndex: 2n },
          descriptor_cbor: "80",
          membership: {
            RedeemerCarriedMembership: {
              membership_proof: [],
              membership_proof_script_redeemer_index: 1n,
            },
          },
        },
      ],
    } as const;
    expect(
      Data.from(
        Data.to(
          membership as never,
          MissingNativeScriptUtxoStep03SpendRedeemerSchema,
        ),
        MissingNativeScriptUtxoStep03SpendRedeemerSchema,
      ),
    ).toEqual(membership);

    const state = {
      fraud_prover: h28,
      data: {
        out_ref: { transactionId: h32, outputIndex: 3n },
        descriptor_cbor: "820040",
        bad_tx_id: h32,
        bad_tx_witness_set_hash: h32,
      },
    } as const;
    expect(
      Data.from(
        Data.to(state as never, MissingNativeScriptUtxoStep04DatumSchema),
        MissingNativeScriptUtxoStep04DatumSchema,
      ),
    ).toEqual(state);
  });

  it("round-trips Q34 authenticated field-6 selection", () => {
    const redeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          script_index: 3n,
          script_tx_wits_opening: inlineOpening,
        },
      ],
    } as const;
    expect(
      Data.from(
        Data.to(
          redeemer as never,
          NativeScriptInvalidStep02SpendRedeemerSchema,
        ),
        NativeScriptInvalidStep02SpendRedeemerSchema,
      ),
    ).toEqual(redeemer);
  });

  it("round-trips Q34 committed item, signer opening, and terminal state", () => {
    const datum = {
      fraud_prover: h28,
      data: {
        bad_tx_id: h32,
        bad_tx_witness_set_hash: h32,
        script_item_hash: h32,
        validity_interval_start: 4n,
        validity_interval_end: 9n,
      },
    } as const;
    expect(
      Data.from(
        Data.to(datum as never, NativeScriptInvalidStep03DatumSchema),
        NativeScriptInvalidStep03DatumSchema,
      ),
    ).toEqual(datum);

    const redeemer = {
      Continue: [
        {
          DirectFinalize: {
            input_index: 0n,
            output_index: 0n,
            fraud_proof_mint_redeemer_index: 0n,
            script_item_cbor: "820040",
            addr_tx_wits_opening: inlineOpening,
          },
        },
      ],
    } as const;
    expect(
      Data.from(
        Data.to(
          redeemer as never,
          NativeScriptInvalidStep03SpendRedeemerSchema,
        ),
        NativeScriptInvalidStep03SpendRedeemerSchema,
      ),
    ).toEqual(redeemer);
  });
});
