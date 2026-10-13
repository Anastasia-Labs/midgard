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
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  checkLedgerAgainstRoot,
  checkLedgerJson,
  ledgerJson,
  LedgerRootMismatchError,
  predecessorLedger,
} from "../src/ledger-at.js";
import { keyValuePhasRootWithCount } from "../src/transition-trace/phas.js";

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

describe("ledger root check for an output with an undecodable native script_ref", () => {
  it("admits the source's descriptor only when it binds the output and opens the root", async () => {
    const descriptor = committedDescriptor(script);
    const { root } = await keyValuePhasRootWithCount([
      { key: outRef, value: descriptor },
    ]);
    await expect(
      checkLedgerAgainstRoot({
        entries: [{ outRef, output }],
        utxosRoot: root,
        label: "test",
      }),
    ).rejects.toThrow("not a canonical ledger entry");
    const ledger = await checkLedgerAgainstRoot({
      entries: [{ outRef, output, descriptor }],
      utxosRoot: root,
      label: "test",
    });
    expect(ledger.entries[0]!.descriptor).toEqual(descriptor);
    const json = ledgerJson(ledger);
    expect(json[0]).toHaveLength(3);
    expect(
      (await checkLedgerJson({ value: json, utxosRoot: root, label: "test" }))
        .utxosRoot,
    ).toBe(root);
    // A descriptor that does not bind the output's script bytes is refused
    // even when it is what the root commits.
    const unbound = committedDescriptor(Buffer.from("820041", "hex"));
    const unboundRoot = await keyValuePhasRootWithCount([
      { key: outRef, value: unbound },
    ]);
    await expect(
      checkLedgerAgainstRoot({
        entries: [{ outRef, output, descriptor: unbound }],
        utxosRoot: unboundRoot.root,
        label: "test",
      }),
    ).rejects.toThrow("not a canonical ledger entry");
    // A bound descriptor still has to open the committed root.
    await expect(
      checkLedgerAgainstRoot({
        entries: [{ outRef, output, descriptor }],
        utxosRoot: unboundRoot.root,
        label: "test",
      }),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
  });

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
    expect(empty.entries).toEqual([]);
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
        ledgerAt: async () => [{ outRef, output: plainOutput }],
        label: "test",
      }),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
  });
});
