import { readFile } from "node:fs/promises";

import { CML } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import {
  classifyWatcherNativeBlock,
  makeWatcherBlockRelevancePolicy,
} from "../../src/runtime/block-relevance.js";

type Reference = Readonly<{
  transactions: readonly Readonly<{
    txHash: string;
    transactionCbor: string;
  }>[];
}>;

const blockWith = (
  transactionCbors: readonly string[],
): WatcherNativeBlockAdmission =>
  Object.freeze({
    schemaVersion: "midgard-watcher-native-block-admission-v1",
    blockType: "7",
    protocolMajor: "10",
    blockHash: "aa".repeat(32),
    prevHash: "bb".repeat(32),
    slot: "100",
    blockNo: "10",
    rawBlockCbor: "80",
    rawHeaderCbor: "80",
    transactionIds: Object.freeze(transactionCbors.map(() => "cc".repeat(32))),
    transactionCbors: Object.freeze([...transactionCbors]),
  });

const unrelated = makeWatcherBlockRelevancePolicy(["d1".repeat(28)]);

let reference: Reference;
let scriptOutputHash: string | null = null;
let scriptOutputTx: string | null = null;
let anyInput: string | null = null;
let anyInputTx: string | null = null;
beforeAll(async () => {
  reference = JSON.parse(
    await readFile(
      new URL("../support/conway-reference-transactions.json", import.meta.url),
      "utf8",
    ),
  ) as Reference;
  for (const { transactionCbor } of reference.transactions) {
    const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
    const body = transaction.body();
    const outputs = body.outputs();
    for (let index = 0; index < outputs.len(); index += 1) {
      const hash = outputs
        .get(index)
        .address()
        .payment_cred()
        ?.as_script()
        ?.to_hex();
      if (hash !== undefined && scriptOutputHash === null) {
        scriptOutputHash = hash;
        scriptOutputTx = transactionCbor;
      }
    }
    if (anyInput === null) {
      const input = body.inputs().get(0);
      anyInput = `${input.transaction_id().to_hex()}#${input.index().toString()}`;
      anyInputTx = transactionCbor;
    }
    transaction.free();
  }
});

describe("native block relevance classification", () => {
  it("treats an empty block and untracked transactions as quiet", () => {
    expect(
      classifyWatcherNativeBlock({ block: blockWith([]), policy: unrelated }),
    ).toBe("quiet");
    expect(
      classifyWatcherNativeBlock({
        block: blockWith(reference.transactions.map((t) => t.transactionCbor)),
        policy: unrelated,
      }),
    ).toBe("quiet");
  });

  it("marks a block touched when an output pays to a tracked script", () => {
    const hash = scriptOutputHash;
    const transaction = scriptOutputTx;
    if (hash === null || transaction === null)
      throw new Error("reference fixture lacks a script output");
    expect(
      classifyWatcherNativeBlock({
        block: blockWith([transaction]),
        policy: makeWatcherBlockRelevancePolicy([hash]),
      }),
    ).toBe("touched");
  });

  it("marks a block touched when it mints under a tracked policy", () => {
    const transaction = anyInputTx;
    if (transaction === null) throw new Error("reference fixture lacks a tx");
    // The reference set carries no mint, so graft one onto a real body.
    const policy = "e5".repeat(28);
    const source = CML.Transaction.from_cbor_hex(transaction);
    const body = source.body();
    const assets = CML.MapAssetNameToNonZeroInt64.new();
    assets.insert(CML.AssetName.from_str("watcher"), 1n);
    const mint = CML.Mint.new();
    mint.insert_assets(CML.ScriptHash.from_hex(policy), assets);
    body.set_mint(mint);
    const minted = CML.Transaction.new(
      body,
      source.witness_set(),
      true,
    ).to_cbor_hex();
    expect(
      classifyWatcherNativeBlock({
        block: blockWith([minted]),
        policy: makeWatcherBlockRelevancePolicy([policy]),
      }),
    ).toBe("touched");
    expect(
      classifyWatcherNativeBlock({
        block: blockWith([minted]),
        policy: unrelated,
      }),
    ).toBe("quiet");
  });

  it("marks a block touched when it spends a tracked outref", () => {
    const outRef = anyInput;
    const transaction = anyInputTx;
    if (outRef === null || transaction === null)
      throw new Error("reference fixture lacks an input");
    expect(
      classifyWatcherNativeBlock({
        block: blockWith([transaction]),
        policy: unrelated,
        trackedOutRefs: [outRef],
      }),
    ).toBe("touched");
    expect(() =>
      classifyWatcherNativeBlock({
        block: blockWith([transaction]),
        policy: unrelated,
        trackedOutRefs: ["not-an-outref"],
      }),
    ).toThrow("malformed");
  });

  it("treats an undecodable transaction as touched and refuses a bad policy", () => {
    expect(
      classifyWatcherNativeBlock({
        block: blockWith(["ff"]),
        policy: unrelated,
      }),
    ).toBe("touched");
    expect(() => makeWatcherBlockRelevancePolicy([])).toThrow("tracks no");
    expect(() => makeWatcherBlockRelevancePolicy(["zz"])).toThrow("28-byte");
  });
});
