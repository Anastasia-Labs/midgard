import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  DirectSourceSelection,
  type DirectSourceSelection as SourceSelection,
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
});
