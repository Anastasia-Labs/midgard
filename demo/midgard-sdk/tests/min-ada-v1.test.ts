import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  MinAdaFaultV1Schema,
  MinAdaStep01SpendRedeemerSchema,
  MinAdaStep02DatumSchema,
  MinAdaStep02SpendRedeemerSchema,
  MinAdaStep04SpendRedeemerSchema,
} from "../src/fraud-proof/min-ada-v1.js";

const h28 = "11".repeat(28);
const h32 = "22".repeat(32);

describe("Q27 min-Ada wire codec", () => {
  it("pins the transaction and post-UTxO constructor indices", () => {
    expect(
      Data.to({ MinAdaTx: { output_index: 3n } } as never, MinAdaFaultV1Schema),
    ).toBe("d8799f03ff");
    expect(Data.to("MinAdaUtxo" as never, MinAdaFaultV1Schema)).toBe("d87a80");
  });

  it("round-trips direct post-root membership evidence", () => {
    const redeemer = {
      Continue: [
        {
          tx_inclusion: null,
          post_utxo_membership: {
            input_index: 0n,
            output_index: 0n,
            hub_ref_input_index: 0n,
            state_queue_node_ref_input_index: 1n,
            out_ref: { transactionId: h32, outputIndex: 2n },
            descriptor_cbor: "80",
          },
          fault: "MinAdaUtxo",
        },
      ],
    } as const;
    const encoded = Data.to(redeemer as never, MinAdaStep01SpendRedeemerSchema);
    expect(Data.from(encoded, MinAdaStep01SpendRedeemerSchema)).toEqual(
      redeemer,
    );
  });

  it("round-trips the culpability-bound post-root state", () => {
    const datum = {
      fraud_prover: h28,
      data: {
        bad_tx_id: h32,
        fault: "MinAdaUtxo",
        post_utxo: {
          out_ref: { transactionId: h32, outputIndex: 2n },
          descriptor_cbor: "80",
          post_utxos_root: h32,
          prev_utxos_root: h32,
        },
      },
    } as const;
    const encoded = Data.to(datum as never, MinAdaStep02DatumSchema);
    expect(Data.from(encoded, MinAdaStep02DatumSchema)).toEqual(datum);
  });

  it("round-trips published post-root membership", () => {
    const redeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          outputs_opening: null,
          post_membership: {
            PublishedChunkMembership: [
              { ordered_chunk_reference_input_indices: [0n, 1n, 2n, 3n] },
            ],
          },
        },
      ],
    } as const;
    const encoded = Data.to(redeemer as never, MinAdaStep02SpendRedeemerSchema);
    expect(Data.from(encoded, MinAdaStep02SpendRedeemerSchema)).toEqual(
      redeemer,
    );
  });

  it("round-trips published predecessor non-membership", () => {
    const redeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          predecessor_non_membership: {
            PublishedChunkNonMembership: [
              { ordered_chunk_reference_input_indices: [0n, 1n, 2n, 3n] },
            ],
          },
        },
      ],
    } as const;
    const encoded = Data.to(redeemer as never, MinAdaStep04SpendRedeemerSchema);
    expect(Data.from(encoded, MinAdaStep04SpendRedeemerSchema)).toEqual(
      redeemer,
    );
  });
});
