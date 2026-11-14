/**
 * A checked ledger proves input resolution from its own descriptor trie. Its
 * proofs must be byte for byte the ones a trace build makes from the same
 * ledger's entries, for keys it holds and keys it does not.
 */
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  ledgerEntriesPreState,
} from "@al-ft/midgard-validation";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it } from "vitest";

import {
  type CheckedLedger,
  checkedLedgerPreState,
  checkLedgerAgainstRoot,
  ledgerEntriesRoot,
  type LedgerEntry,
} from "../src/ledger-at.js";

const keyed = (byte: number, outputIndex = 0) =>
  encodeMidgardSpendInputItem({ txId: Buffer.alloc(32, byte), outputIndex });

const plainOutput = Buffer.from(
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  "hex",
);
// The same output with a native script_ref whose body does not decode: its
// leaf is the bytes-only descriptor, not a canonical one.
const undecodableScriptRefOutput = Buffer.concat([
  Buffer.from("a3", "hex"),
  plainOutput.subarray(1),
  Buffer.from("038200428109", "hex"),
]);

const checked = async (entries: readonly LedgerEntry[]) =>
  await checkLedgerAgainstRoot({
    entries,
    utxosRoot: await ledgerEntriesRoot(entries),
    label: "test",
  });

/** The descriptors a trace build's replay derives from the entries. */
const replayDescriptors = (ledger: CheckedLedger) =>
  new Map(
    ledger.entries.map((entry) => {
      let descriptor: Buffer;
      try {
        descriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
          outRef: entry.outRef,
          outputCbor: entry.output,
        }).descriptorCbor;
      } catch {
        descriptor = entry.descriptor!;
      }
      return [entry.outRef.toString("hex"), descriptor];
    }),
  );

const bothAdapters = async (
  ledger: CheckedLedger,
  keys: readonly Buffer[],
) => ({
  checked: await checkedLedgerPreState(ledger).proveResolution(keys),
  entries: await ledgerEntriesPreState(
    ledger.entries,
    replayDescriptors(ledger),
  ).proveResolution(keys),
});

describe("a checked ledger as a trace's pre-state ledger", () => {
  it("proves held and absent keys byte for byte as the entries do", async () => {
    const ledger = await checked([
      { outRef: keyed(0x56), output: makeOutput(2_000_000n) },
      { outRef: keyed(0x58), output: makeOutput(3_000_000n) },
      { outRef: keyed(0x5a), output: undecodableScriptRefOutput },
      { outRef: keyed(0x5a, 1), output: makeOutput(4_000_000n) },
    ]);
    expect(ledger.entries[2]!.descriptor).toBeDefined();
    // Held, between, below, above, a sibling index, and held again after
    // an absence proof, which must leave the shared trie as it was.
    const keys = [
      keyed(0x58),
      keyed(0x57),
      keyed(0x01),
      keyed(0xff),
      keyed(0x5a, 2),
      keyed(0x5a),
      keyed(0x56),
      keyed(0x5a, 1),
    ];
    const proofs = await bothAdapters(ledger, keys);
    expect(proofs.checked).toHaveLength(keys.length);
    expect(proofs.checked).toEqual(proofs.entries);
    expect(await checkedLedgerPreState(ledger).proveResolution(keys)).toEqual(
      proofs.checked,
    );
  });

  it("proves absence from the empty ledger as the entries do", async () => {
    const ledger = await checked([]);
    const proofs = await bothAdapters(ledger, [keyed(0x56), keyed(0x57)]);
    expect(proofs.checked).toHaveLength(2);
    expect(proofs.checked).toEqual(proofs.entries);
  });
});
