import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  InputNoIdxStep01SpendRedeemer,
  InputNoIdxStep03SpendRedeemer,
  InvalidSignatureStep01SpendRedeemer,
  MinFeeStep01SpendRedeemer,
  NoReferenceInputStep01SpendRedeemer,
  ReferenceInputNoIdxStep01SpendRedeemer,
  ReferenceInputNoIdxStep03SpendRedeemer,
  WithdrawnInputStep01SpendRedeemer,
} from "../src/index.js";

const h32 = "11".repeat(32);
const schemas = [
  ["input-no-idx step-01", InputNoIdxStep01SpendRedeemer],
  ["input-no-idx step-03", InputNoIdxStep03SpendRedeemer],
  ["no-reference-input step-01", NoReferenceInputStep01SpendRedeemer],
  ["reference-input-no-idx step-01", ReferenceInputNoIdxStep01SpendRedeemer],
  ["reference-input-no-idx step-03", ReferenceInputNoIdxStep03SpendRedeemer],
  ["invalid-signature step-01", InvalidSignatureStep01SpendRedeemer],
  ["min-fee step-01", MinFeeStep01SpendRedeemer],
  ["withdrawn-input step-01", WithdrawnInputStep01SpendRedeemer],
] as const;

const common = {
  input_index: 0n,
  output_index: 1n,
  hub_ref_input_index: 2n,
  state_queue_node_ref_input_index: 3n,
  native_tx_id: h32,
  l2_transaction_source_cbor: "80",
  transactions_phas_root: h32,
};

describe("native transaction inclusion carriage ABI", () => {
  for (const [label, schema] of schemas) {
    it(`${label} accepts both authenticated carriage constructors`, () => {
      const direct = {
        Continue: [
          {
            RedeemerCarriedInclusion: [
              {
                ...common,
                tx_membership_proof: [],
                inclusion_proof_script_withdraw_redeemer_index: 0n,
              },
            ],
          },
        ],
      } as const;
      const published = {
        Continue: [
          {
            PublishedChunkInclusion: [
              {
                ...common,
                ordered_chunk_reference_input_indices: [4n, 5n, 6n, 7n],
              },
            ],
          },
        ],
      } as const;

      expect(Data.from(Data.to(direct as never, schema), schema)).toEqual(
        direct,
      );
      expect(Data.from(Data.to(published as never, schema), schema)).toEqual(
        published,
      );
    });
  }
});
