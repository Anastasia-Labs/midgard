import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  mpfProofFromWitness,
  normalizedMpfRoot,
} from "../src/transition-trace/detect.js";
import { createTransitionTraceLedgerReplay } from "../src/transition-trace/ledger-replay.js";
import { keyValuePhasRootWithCount } from "../src/transition-trace/phas.js";

const key = (index: number) =>
  encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x51),
    outputIndex: index,
  });
const output = encodeMidgardTxOutput({
  address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x22)]),
  value: { lovelace: 2_000_000n, assets: new Map() },
});

describe("transition trace retained descriptor mutations", () => {
  it("produces ordered canonical MPF witnesses against each evolving root", async () => {
    const entries = [0, 1].map((index) => ({
      key: key(index),
      value: Buffer.from(
        buildCanonicalMidgardLedgerEntryOutputMaterial({
          outRef: key(index),
          outputCbor: output,
        }).descriptorCbor,
      ),
    }));
    const initial = await keyValuePhasRootWithCount(entries);
    const ledger = await createTransitionTraceLedgerReplay({
      entries,
      expectedRoot: initial.root,
    });
    const removed = await ledger.delete(key(0));
    const deletion = mpfProofFromWitness({
      key: key(0),
      value: entries[0]!.value,
      proof: removed.delete_proof,
      label: "delete",
    });
    expect(normalizedMpfRoot(deletion.verify(true), "before delete")).toBe(
      initial.root,
    );
    expect(normalizedMpfRoot(deletion.verify(false), "after delete")).toBe(
      ledger.root(),
    );
    const before = ledger.root();
    const inserted = await ledger.insert(key(2), output);
    const insertion = mpfProofFromWitness({
      key: key(2),
      value: Buffer.from(inserted.value, "hex"),
      proof: inserted.insert_proof,
      label: "insert",
    });
    expect(normalizedMpfRoot(insertion.verify(false), "before insert")).toBe(
      before,
    );
    expect(normalizedMpfRoot(insertion.verify(true), "after insert")).toBe(
      ledger.root(),
    );
    expect(ledger.root()).toBe(
      (
        await keyValuePhasRootWithCount([
          entries[1]!,
          { key: key(2), value: Buffer.from(inserted.value, "hex") },
        ])
      ).root,
    );
    await expect(ledger.delete(key(0))).rejects.toThrow(/absent input/u);
    await expect(ledger.insert(key(2), output)).rejects.toThrow(
      /existing output/u,
    );
  });

  it("refuses a raw-output trie or duplicate ledger keys before replay", async () => {
    const descriptor = Buffer.from(
      buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: key(0),
        outputCbor: output,
      }).descriptorCbor,
    );
    const correct = await keyValuePhasRootWithCount([
      { key: key(0), value: descriptor },
    ]);
    await expect(
      createTransitionTraceLedgerReplay({
        entries: [{ key: key(0), value: output }],
        expectedRoot: correct.root,
      }),
    ).rejects.toThrow(/descriptor trie/u);
    await expect(
      createTransitionTraceLedgerReplay({
        entries: [
          { key: key(0), value: descriptor },
          { key: key(0), value: descriptor },
        ],
        expectedRoot: correct.root,
      }),
    ).rejects.toThrow(/duplicate/u);
    const empty = await createTransitionTraceLedgerReplay({
      entries: [],
      expectedRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    });
    expect(empty.root()).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
  });
});
