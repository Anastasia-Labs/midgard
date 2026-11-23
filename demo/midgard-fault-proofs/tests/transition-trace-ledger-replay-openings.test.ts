/**
 * The transition replay's ledger witnesses, every deletion's opening and
 * proof and every insertion's proof, are byte for byte those a trie of the
 * whole pre-state ledger gives for the same edits.
 */
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardMpfDeletionOpening,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  parseMidgardMpfProofJson,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation/ledger-output-descriptor";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { type LedgerEntry } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
} from "../src/ledger-store/index.js";
import { createTransitionTraceLedgerReplay } from "../src/transition-trace/ledger-replay.js";
import { oracleLedgerRoot } from "./support/ledger-root-oracle.js";

const outRef = (index: number) =>
  encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x61 + (index % 7)),
    outputIndex: index,
  });

const output = (index: number) =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x22)]),
    value: { lovelace: 2_000_000n + BigInt(index), assets: new Map() },
  });

const entry = (index: number): LedgerEntry => ({
  outRef: outRef(index),
  output: output(index),
});

const descriptor = ({ outRef, output }: LedgerEntry) =>
  Buffer.from(
    buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef,
      outputCbor: output,
    }).descriptorCbor,
  );

/** The transition replay over the pre-state ledger of `entries`. */
const replayOver = async (entries: readonly LedgerEntry[]) => {
  const root = await oracleLedgerRoot(entries);
  return await createTransitionTraceLedgerReplay({
    ledger: await createLedgerStore(createMemoryLedgerStorage()).admit(
      entries,
      root,
    ),
    expectedRoot: root,
  });
};

type Edit =
  | { readonly type: "delete"; readonly index: number }
  | { readonly type: "insert"; readonly index: number };

const hashOf = (trie: Trie) =>
  trie.hash === null || trie.hash === undefined
    ? SDK.EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(trie.hash).toString("hex");

/** The witnesses and root after each edit, from one trie of every entry. */
const wholeLedgerWitnesses = async (
  entries: readonly LedgerEntry[],
  edits: readonly Edit[],
) => {
  const trie = await Trie.fromList(
    entries.map((held) => ({ key: held.outRef, value: descriptor(held) })),
  );
  const witnesses: unknown[] = [];
  for (const edit of edits) {
    const key = outRef(edit.index);
    if (edit.type === "delete") {
      const value = descriptor(entry(edit.index));
      const proved = await trie.prove(key);
      const opening = await buildMidgardMpfDeletionOpening(
        trie,
        key,
        parseMidgardMpfProofJson(proved.toJSON()),
      );
      await trie.delete(key);
      witnesses.push({
        key: key.toString("hex"),
        value: value.toString("hex"),
        opening: opening.toString("hex"),
        delete_proof: Data.from(proved.toCBOR().toString("hex"), SDK.Proof),
        root: hashOf(trie),
      });
    } else {
      const value = descriptor(entry(edit.index));
      await trie.insert(key, value);
      witnesses.push({
        key: key.toString("hex"),
        value: value.toString("hex"),
        insert_proof: Data.from(
          (await trie.prove(key)).toCBOR().toString("hex"),
          SDK.Proof,
        ),
        root: hashOf(trie),
      });
    }
  }
  return witnesses;
};

const replayedWitnesses = async (
  entries: readonly LedgerEntry[],
  edits: readonly Edit[],
) => {
  const ledger = await replayOver(entries);
  const witnesses: unknown[] = [];
  for (const edit of edits) {
    const witness =
      edit.type === "delete"
        ? await ledger.delete(outRef(edit.index))
        : await ledger.insert(outRef(edit.index), output(edit.index));
    witnesses.push({ ...witness, root: ledger.root() });
  }
  return witnesses;
};

const MANY = Array.from({ length: 256 }, (_, index) => entry(index));

describe("transition replay ledger witnesses", () => {
  it("opens a deletion that leaves a Branch with one child", async () => {
    const replayed = await replayedWitnesses(
      MANY,
      MANY.map((_, index) => ({ type: "delete", index }) as const),
    );
    expect(
      replayed.filter(
        (witness) => (witness as { opening: string }).opening !== "",
      ).length,
    ).toBeGreaterThan(0);
  });

  it.each([
    [
      "deletes every entry of a 256-entry ledger",
      MANY,
      MANY.map((_, index) => ({ type: "delete", index }) as const),
    ],
    [
      "interleaves deletions and insertions",
      MANY.slice(0, 40),
      [
        { type: "delete", index: 3 },
        { type: "insert", index: 300 },
        { type: "delete", index: 17 },
        { type: "delete", index: 300 },
        { type: "insert", index: 301 },
        { type: "delete", index: 39 },
        { type: "delete", index: 0 },
      ] as const,
    ],
    [
      "empties a single-entry ledger and refills it",
      [entry(5)],
      [
        { type: "delete", index: 5 },
        { type: "insert", index: 6 },
      ] as const,
    ],
  ] as const)(
    "gives the whole ledger trie's witnesses when it %s",
    async (_, entries, edits) => {
      const replayed = await replayedWitnesses(entries, edits);
      expect(replayed).toStrictEqual(
        await wholeLedgerWitnesses(entries, edits),
      );
    },
  );
});
