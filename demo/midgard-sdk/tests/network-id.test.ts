import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  isExplicitTransactionNetworkMismatch,
  NetworkIdFaultSchema,
  NetworkIdStep01SpendRedeemerSchema,
  NetworkIdStep02DatumSchema,
  NetworkIdStep02SpendRedeemerSchema,
} from "../src/fraud-proof/network-id.js";

const h28 = "11".repeat(28);
const h32 = "22".repeat(32);

describe("Q35 network-id wire codec", () => {
  it("pins all claim constructor indices", () => {
    expect(Data.to("TransactionNetwork" as never, NetworkIdFaultSchema)).toBe(
      "d87980",
    );
    expect(
      Data.to(
        { OutputNetwork: { output_index: 11n } } as never,
        NetworkIdFaultSchema,
      ),
    ).toBe("d87a9f0bff");
    expect(
      Data.to(
        { OutputNetworkUtxo: { observed_network_id: 7n } } as never,
        NetworkIdFaultSchema,
      ),
    ).toBe("d87b9f07ff");
  });

  it("round-trips direct post-UTxO descriptor membership evidence", () => {
    const redeemer = {
      Continue: [
        {
          tx_inclusion: null,
          post_utxo_membership: {
            input_index: 0n,
            output_index: 0n,
            hub_ref_input_index: 0n,
            state_queue_node_ref_input_index: 1n,
            out_ref: { transactionId: h32, outputIndex: 11n },
            descriptor_cbor: "80",
            membership: {
              RedeemerCarriedMembership: {
                membership_proof: [],
                membership_proof_script_redeemer_index: 0n,
              },
            },
            predecessor: "Introduced",
          },
          fault: { OutputNetworkUtxo: { observed_network_id: 7n } },
        },
      ],
    } as const;
    const encoded = Data.to(
      redeemer as never,
      NetworkIdStep01SpendRedeemerSchema,
    );
    expect(Data.from(encoded, NetworkIdStep01SpendRedeemerSchema)).toEqual(
      redeemer,
    );
  });

  it("round-trips the authenticated step-02 state", () => {
    const datum = {
      fraud_prover: h28,
      data: {
        bad_tx_id: h32,
        committed_tx_network_id: 1n,
        expected_network_id: 0n,
        fault: { OutputNetwork: { output_index: 11n } },
        post_utxo: null,
      },
    } as const;
    const encoded = Data.to(datum as never, NetworkIdStep02DatumSchema);
    expect(Data.from(encoded, NetworkIdStep02DatumSchema)).toEqual(datum);
  });

  it("round-trips a complete inline field-2 opening", () => {
    const redeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 0n,
          outputs_opening: {
            BodyFieldOpening: {
              native_tx_compact_cbor: "80",
              carriage: { Inline: { preimage: "81" } },
            },
          },
          predecessor_carriage: null,
        },
      ],
    } as const;
    const encoded = Data.to(
      redeemer as never,
      NetworkIdStep02SpendRedeemerSchema,
    );
    expect(Data.from(encoded, NetworkIdStep02SpendRedeemerSchema)).toEqual(
      redeemer,
    );
  });

  it("does not misclassify Cardano's absent transaction network", () => {
    expect(
      isExplicitTransactionNetworkMismatch({
        committedNetworkId: 255n,
        expectedNetworkId: 0n,
      }),
    ).toBe(false);
    expect(
      isExplicitTransactionNetworkMismatch({
        committedNetworkId: 1n,
        expectedNetworkId: 0n,
      }),
    ).toBe(true);
  });
});
