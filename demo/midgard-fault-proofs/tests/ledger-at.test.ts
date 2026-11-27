import {
  buildMidgardBoundedItem,
  buildMidgardLedgerOutputMaterial,
  computeHash28,
  encodeMidgardSpendInputItem,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
} from "@al-ft/midgard-core";
import {
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
} from "@al-ft/midgard-sdk/core";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation/ledger-output-descriptor";
import { describe, expect, it } from "vitest";

import { undecodableNativeOutputDescriptor } from "../src/evidence/retained-ledger-output.js";
import {
  ledgerEntryLeaf,
  LedgerRootMismatchError,
  predecessorLedger,
} from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
} from "../src/ledger-store/index.js";
import { keyValuePhasRootWithCount } from "../src/transition-trace/phas.js";
import { entriesOf, memoryLedger } from "./support/fixture-ledger.js";
import { oracleLedgerRoot } from "./support/ledger-root-oracle.js";

const outRef = encodeMidgardSpendInputItem({
  txId: Buffer.alloc(32, 0x55),
  outputIndex: 0,
});
const plainOutput = Buffer.from(
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  "hex",
);
// The same output with a native script_ref whose body does not decode.
const script = Buffer.from("8200428109", "hex");
const output = Buffer.concat([
  Buffer.from("a3", "hex"),
  plainOutput.subarray(1),
  Buffer.from("03", "hex"),
  script,
]);

const committedDescriptor = (scriptBytes: Buffer) => {
  const {
    version: _version,
    outputIndex: _outputIndex,
    totalLength: _totalLength,
    itemCommitment: _itemCommitment,
    ...facts
  } = buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef,
    outputCbor: plainOutput,
  }).descriptor;
  return buildMidgardLedgerOutputMaterial({
    outputIndex: 0,
    outputCbor: output,
    facts: {
      ...facts,
      referenceScriptLanguage: 0,
      referenceScriptHash: computeHash28(script),
      referenceScriptTotalLength: scriptBytes.length,
      referenceScriptItemCommitment: buildMidgardBoundedItem({
        fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
        itemIndex: 0,
        bytes: scriptBytes,
      }).commitment,
    },
  }).descriptorCbor;
};

describe("ledger root of an output with an undecodable native script_ref", () => {
  it("derives the descriptor from the output bytes, and a store admits it only at that root", async () => {
    const derived = undecodableNativeOutputDescriptor({ key: outRef, output });
    const { root } = await keyValuePhasRootWithCount([
      { key: outRef, value: derived },
    ]);
    expect(ledgerEntryLeaf({ outRef, output }, "test").leaf).toEqual(derived);
    // A source-supplied descriptor must be the output's own.
    const other = committedDescriptor(Buffer.from("820041", "hex"));
    expect(other.equals(derived)).toBe(false);
    expect(() =>
      ledgerEntryLeaf({ outRef, output, descriptor: other }, "test"),
    ).toThrow("not a canonical ledger entry");
    // The derived descriptor still has to open the committed root.
    const otherRoot = await keyValuePhasRootWithCount([
      { key: outRef, value: other },
    ]);
    const store = createLedgerStore(createMemoryLedgerStorage());
    expect((await store.admit([{ outRef, output }], root)).root).toBe(root);
    await expect(
      store.admit([{ outRef, output }], otherRoot.root),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
  });

  it("refuses an output that is neither canonical nor an undecodable native script", () => {
    expect(() =>
      ledgerEntryLeaf({ outRef, output: Buffer.from("ff", "hex") }, "test"),
    ).toThrow("not a canonical ledger entry");
    // Even with a supplied descriptor.
    expect(() =>
      ledgerEntryLeaf(
        {
          outRef,
          output: Buffer.from("ff", "hex"),
          descriptor: committedDescriptor(script),
        },
        "test",
      ),
    ).toThrow("not a canonical ledger entry");
  });
});

describe("predecessor ledger", () => {
  it("uses the known genesis ledger without fetching a predecessor payload", async () => {
    const source = async () => {
      throw new Error("the empty ledger was asked of the source");
    };
    const empty = await predecessorLedger({
      prevHeaderHash: GENESIS_HEADER_HASH,
      prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      ledgerAt: source,
      label: "test",
    });
    expect(empty.root).toBe(EMPTY_MERKLE_TREE_ROOT);
    expect(await entriesOf(empty)).toEqual([]);
    await expect(
      predecessorLedger({
        prevHeaderHash: "22".repeat(28),
        prevUtxosRoot: "11".repeat(32),
        ledgerAt: source,
        label: "test",
      }),
    ).rejects.toThrow("the empty ledger was asked of the source");
  });

  it("checks a supplied non-genesis ledger even when the committed root is empty", async () => {
    await expect(
      predecessorLedger({
        prevHeaderHash: "22".repeat(28),
        prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
        ledgerAt: async () =>
          await memoryLedger(
            [{ outRef, output: plainOutput }],
            await oracleLedgerRoot([{ outRef, output: plainOutput }]),
          ),
        label: "test",
      }),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
  });
});
