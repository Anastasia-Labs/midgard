import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import { ValidationTraceProof } from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { parseExactAikenDataCbor } from "../src/aiken-blueprint-data.js";

const blueprint = JSON.parse(
  readFileSync(
    resolve(process.cwd(), "../../onchain/aiken/plutus.json"),
    "utf8",
  ),
) as unknown;

const DEFINITION = "midgard/validation_trace_v1/ValidationTraceProof";

describe("exact Aiken blueprint data validation", () => {
  it("accepts the generated validation-trace proof ABI", () => {
    const cbor = Data.to(
      {
        state_index: 3n,
        state_hash: "11".repeat(32),
        siblings: ["22".repeat(32), "33".repeat(32)],
      },
      ValidationTraceProof,
    );

    expect(
      parseExactAikenDataCbor({
        blueprint,
        definitionName: DEFINITION,
        cbor,
        maxBytes: 1_024,
      }),
    ).toBeInstanceOf(Constr);
  });

  it("rejects the wrong constructor shape, non-canonical hex, and overflow", () => {
    const wrongShape = Data.to(new Constr(0, [0n]));
    expect(() =>
      parseExactAikenDataCbor({
        blueprint,
        definitionName: DEFINITION,
        cbor: wrongShape,
        maxBytes: 1_024,
      }),
    ).toThrow(/requires 3 fields/);

    const valid = Data.to(
      {
        state_index: 0n,
        state_hash: "44".repeat(32),
        siblings: [],
      },
      ValidationTraceProof,
    );
    expect(() =>
      parseExactAikenDataCbor({
        blueprint,
        definitionName: DEFINITION,
        cbor: valid.toUpperCase(),
        maxBytes: 1_024,
      }),
    ).toThrow(/lowercase hex/);
    expect(() =>
      parseExactAikenDataCbor({
        blueprint,
        definitionName: DEFINITION,
        cbor: valid,
        maxBytes: 1,
      }),
    ).toThrow(/exceeding 1/);
  });
});
