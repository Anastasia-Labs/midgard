import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildNonExistentInputFaultProofContracts,
  NonExistentInputStep01SpendRedeemer,
  NonExistentInputStep02Datum,
  NonExistentInputStep02SpendRedeemer,
  NonExistentInputStep03Datum,
  NonExistentInputStep03SpendRedeemer,
  NonExistentInputStep04Datum,
  NonExistentInputStep04SpendRedeemer,
  parseFaultProofBlueprint,
  type Proof,
} from "../src/index.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const blueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");

const h32 = "00".repeat(32);
const h32b = "11".repeat(32);
const h32c = "22".repeat(32);
const h28 = "aa".repeat(28);
const h28b = "bb".repeat(28);
const h28c = "cc".repeat(28);

const proof: Proof = [];
const midgardInput = { tx_id: h32c, output_index: 0n };
const txInclusionArgs = {
  input_index: 0n,
  output_index: 0n,
  hub_ref_input_index: 1n,
  state_queue_node_ref_input_index: 2n,
  native_tx_id: h32,
  native_tx_compact_cbor: h32,
  tx_membership_proof: proof,
  inclusion_proof_script_withdraw_redeemer_index: 3n,
};

const roundTrip = <A>(value: A, schema: Parameters<typeof Data.to>[1]): A =>
  Data.from(Data.to(value, schema), schema) as A;

describe("non-existent-input ABI", () => {
  it("round-trips step datums and redeemers", () => {
    expect(
      roundTrip(
        { Continue: [txInclusionArgs] },
        NonExistentInputStep01SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ native_tx_id: h32 }] });

    const step02Datum = {
      fraud_prover: h28,
      data: {
        bad_tx_inputs_hash: h32,
        blocks_prev_utxos_root: h32b,
        blocks_transactions_root: h32c,
      },
    };
    expect(roundTrip(step02Datum, NonExistentInputStep02Datum)).toEqual(
      step02Datum,
    );
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              inputs_preimage: [midgardInput],
              bad_input_index: 0n,
            },
          ],
        },
        NonExistentInputStep02SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ bad_input_index: 0n }] });

    const step03Datum = {
      fraud_prover: h28,
      data: {
        missing_input: midgardInput,
        blocks_prev_utxos_root: h32,
        blocks_transactions_root: h32b,
      },
    };
    expect(roundTrip(step03Datum, NonExistentInputStep03Datum)).toEqual(
      step03Datum,
    );
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              non_membership_proof_in_ledger: proof,
              non_membership_proof_script_redeemer_index: 1n,
            },
          ],
        },
        NonExistentInputStep03SpendRedeemer,
      ),
    ).toMatchObject({
      Continue: [{ non_membership_proof_script_redeemer_index: 1n }],
    });

    const step04Datum = {
      fraud_prover: h28,
      data: { missing_input_tx_id: h32, blocks_transactions_root: h32b },
    };
    expect(roundTrip(step04Datum, NonExistentInputStep04Datum)).toEqual(
      step04Datum,
    );
    expect(
      roundTrip(
        {
          Continue: [
            {
              input_index: 0n,
              output_index: 0n,
              non_membership_proof_in_txs: proof,
              non_membership_proof_script_redeemer_index: 1n,
              fraud_proof_mint_redeemer_index: 2n,
            },
          ],
        },
        NonExistentInputStep04SpendRedeemer,
      ),
    ).toMatchObject({ Continue: [{ fraud_proof_mint_redeemer_index: 2n }] });
  });
});

describe("non-existent-input fault-proof contract builder", () => {
  it("builds four distinct ordered validators from the Aiken blueprint", async () => {
    const blueprint = parseFaultProofBlueprint(
      JSON.parse(readFileSync(blueprintPath, "utf8")) as unknown,
    );

    const contracts = await Effect.runPromise(
      buildNonExistentInputFaultProofContracts({
        blueprint,
        network: "Preprod",
        hubOraclePolicyId: h28b,
        fraudProofCataloguePolicyId: h28c,
      }),
    );

    expect(contracts.nonExistentInput.firstStep).toBe(
      contracts.nonExistentInput.steps[0],
    );
    expect(contracts.nonExistentInput.steps).toHaveLength(4);
    expect(
      new Set(
        contracts.nonExistentInput.steps.map((step) => step.spendingScriptHash),
      ).size,
    ).toBe(4);
  });
});
