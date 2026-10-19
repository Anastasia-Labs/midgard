import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  DirectPurposeSelectRedeemer,
  DirectSourceSelection,
  type DirectSourceSelection as SourceSelection,
  DirectSpendSelectRedeemer,
  DirectSpendWorkState,
} from "../src/fraud-proof/direct-purpose-workers.js";

const h32 = "44".repeat(32);
const h28 = "33".repeat(28);
const coordinate = { kind: 3n, index: 7n };
const receipt: SourceSelection = {
  selection: {
    bound: {
      subject: {
        version: 1n,
        direction: 0n,
        source_kind: 0n,
        transaction_id: h32,
        source_key: "",
        rejection_reason: null,
      },
      compact_cbor: "00",
      network_order_context: { body_network_mismatch: false },
      event_pre_root: "55".repeat(32),
    },
    source_index: { root: "66".repeat(32), count: 7n },
    coordinate,
    purpose: { coordinate, script_hash: h28, subject: h28 },
  },
  source: {
    field_index: 1n,
    item_index: 2n,
    source_key: "02",
    language_tag: 128n,
    script_hash: h28,
    total_length: 8n,
    item_commitment: h32,
  },
};
const record = (value: Data): Constr<Data> => {
  if (!(value instanceof Constr)) throw new Error("expected Aiken record");
  return value;
};

describe("direct purpose wire custody", () => {
  it("pins the completed source map and pointer pair in the source receipt", () => {
    const raw = record(Data.from(Data.to(receipt, DirectSourceSelection)));
    const selected = record(raw.fields[0]!);
    expect(selected.fields[1]).toEqual(new Constr(0, ["66".repeat(32), 7n]));
    expect(selected.fields[2]).toEqual(new Constr(0, [3n, 7n]));
    const source = record(record(raw.fields[1]!).fields[0]!);
    expect(source.fields[0]).toBe(1n);
    expect(source.fields[3]).toBe(128n);
    expect(source.fields[6]).toBe(h32);
  });

  it("keeps the raw spend item index separate from its derived sorted rank", () => {
    const spend = record(
      Data.from(
        Data.to(
          {
            source_index: receipt.selection.source_index,
            cursor: {
              bound: receipt.selection.bound,
              field_index: 0n,
              phase: "Walking",
              checkpoint_hash: h32,
            },
            raw_item_index: 2n,
            target: { script_hash: h28, subject: "00" },
            rank: 1n,
            complete: true,
          },
          DirectSpendWorkState,
        ),
      ),
    );
    expect(spend.fields[2]).toBe(2n);
    expect(spend.fields[4]).toBe(1n);
  });

  // The same encodings are pinned on-chain in
  // onchain/aiken/validators/fraud-proofs/direct-script-derivation/purpose-select.test.ak.
  it("encodes the selection door's spend hand-off and the spend door's arguments as on-chain", () => {
    expect(
      Data.to(
        { Continue: [{ Spend: { input_index: 5n, output_index: 6n } }] },
        DirectPurposeSelectRedeemer,
      ),
    ).toBe("d87a9fd8799f0506ffff");
    expect(
      Data.to(
        {
          Continue: [
            {
              input_index: 1n,
              output_index: 2n,
              raw_item_index: 3n,
              opening: {
                BodyFieldOpening: {
                  native_tx_compact_cbor: "a1",
                  carriage: { Inline: { preimage: "b2" } },
                },
              },
              descriptor_cbor: "c3",
              membership: {
                RedeemerCarriedMembership: {
                  membership_proof: [],
                  membership_proof_script_redeemer_index: 4n,
                },
              },
            },
          ],
        },
        DirectSpendSelectRedeemer,
      ),
    ).toBe("d87a9fd8799f010203d8799f41a1d8799f41b2ffff41c3d8799f8004ffffff");
  });
});
