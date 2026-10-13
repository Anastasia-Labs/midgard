import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  MIN_ADA_STEP_NAMES,
  MinAdaFaultSchema,
  MinAdaStep01SpendRedeemerSchema,
  MinAdaStep02DatumSchema,
  MinAdaStep02SpendRedeemerSchema,
  MinAdaStep03StateSchema,
  MinAdaStep04DatumSchema,
  MinAdaStep04SpendRedeemerSchema,
} from "../src/fraud-proof/min-ada.js";

const h28 = "11".repeat(28);
const h32 = "22".repeat(32);

describe("Q27 min-Ada wire codec", () => {
  it("pins the single transaction-output fault constructor", () => {
    // `MinAdaFaultV1.MinAdaTx { output_index: 3 }` is Constr 0 [3].
    expect(Data.to({ output_index: 3n } as never, MinAdaFaultSchema)).toBe(
      "d8799f03ff",
    );
    // No second constructor exists any more.
    expect(() => Data.from("d87a80", MinAdaFaultSchema)).toThrow();
  });

  it("round-trips a transaction-bound step-01 claim", () => {
    const redeemer = {
      Continue: [
        {
          forced_source: null,
          tx_inclusion: null,
          fault: { output_index: 2n },
        },
      ],
    } as const;
    const encoded = Data.to(redeemer as never, MinAdaStep01SpendRedeemerSchema);
    expect(Data.from(encoded, MinAdaStep01SpendRedeemerSchema)).toEqual(
      redeemer,
    );
  });

  it("round-trips the step-02 state without any post-UTxO field", () => {
    const datum = {
      fraud_prover: h28,
      data: {
        grammar_checkpoint_hash: "",
        grammar_complete: false,
        walk_checkpoint_hash: "",
        direction: 0n,
        source_kind: 0n,
        bad_tx_id: h32,
        fault: { output_index: 2n },
      },
    } as const;
    const encoded = Data.to(datum as never, MinAdaStep02DatumSchema);
    expect(Data.from(encoded, MinAdaStep02DatumSchema)).toEqual(datum);
  });

  it("requires the outputs opening on every step-02 continuation", () => {
    const opening = {
      BodyFieldOpening: {
        native_tx_compact_cbor: "80",
        carriage: { Inline: { preimage: "81" } },
      },
    };
    const redeemer = {
      Continue: [
        {
          grammar_checkpoint_bytes: "",
          walk_checkpoint_bytes: "",
          input_index: 0n,
          output_index: 0n,
          yield_to_ref_input_index: 0n,
          outputs_opening: opening,
        },
      ],
    } as const;
    const encoded = Data.to(redeemer as never, MinAdaStep02SpendRedeemerSchema);
    expect(Data.from(encoded, MinAdaStep02SpendRedeemerSchema)).toEqual(
      redeemer,
    );
    expect(() =>
      Data.to(
        {
          Continue: [{ ...redeemer.Continue[0], outputs_opening: null }],
        } as never,
        MinAdaStep02SpendRedeemerSchema,
      ),
    ).toThrow();
  });

  it("has only the transaction scan and descriptor step-03 states", () => {
    expect(() =>
      Data.from(
        // Constr 2: the deleted post-UTxO descriptor arm.
        "d87b9f4180418058202222222222222222222222222222222222222222222222222222222222222222ff",
        MinAdaStep03StateSchema,
      ),
    ).toThrow();
  });

  it("pins the four-step chain and its terminal step", () => {
    expect(MIN_ADA_STEP_NAMES).toStrictEqual([
      "step_01",
      "step_02",
      "step_03",
      "step_04",
    ]);
    const datum = { fraud_prover: h28, data: "PredicateAuthenticated" };
    const encoded = Data.to(datum as never, MinAdaStep04DatumSchema);
    expect(encoded).toBe(`d8799f581c${h28}d8799fd87980ffff`);
    const redeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 1n,
        },
      ],
    } as const;
    expect(
      Data.from(
        Data.to(redeemer as never, MinAdaStep04SpendRedeemerSchema),
        MinAdaStep04SpendRedeemerSchema,
      ),
    ).toEqual(redeemer);
  });
});
