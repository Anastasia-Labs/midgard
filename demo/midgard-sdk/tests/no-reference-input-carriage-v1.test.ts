import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  NoReferenceInputStep03SpendRedeemerSchema,
  NoReferenceInputStep04SpendRedeemerSchema,
} from "@/fraud-proof/no-reference-input.js";

const direct = {
  RedeemerCarriedNonMembership: {
    non_membership_proof: [],
    non_membership_proof_script_redeemer_index: 2n,
  },
} as const;

const published = {
  PublishedChunkNonMembership: [
    { ordered_chunk_reference_input_indices: [1n, 3n] },
  ],
} as const;

describe("no-reference-input non-membership carriage V1", () => {
  it.each([
    ["step-03 direct", NoReferenceInputStep03SpendRedeemerSchema, direct],
    ["step-03 published", NoReferenceInputStep03SpendRedeemerSchema, published],
    ["step-04 direct", NoReferenceInputStep04SpendRedeemerSchema, direct],
    ["step-04 published", NoReferenceInputStep04SpendRedeemerSchema, published],
  ] as const)("round-trips %s", (_label, schema, carriage) => {
    const redeemer =
      schema === NoReferenceInputStep03SpendRedeemerSchema
        ? {
            Continue: [
              {
                input_index: 0n,
                output_index: 1n,
                non_membership_in_ledger: carriage,
              },
            ],
          }
        : {
            Continue: [
              {
                input_index: 0n,
                output_index: 1n,
                fraud_proof_mint_redeemer_index: 3n,
                non_membership_in_txs: carriage,
              },
            ],
          };
    const encoded = Data.to(redeemer as never, schema);
    expect(Data.from(encoded, schema)).toEqual(redeemer);
  });
});
