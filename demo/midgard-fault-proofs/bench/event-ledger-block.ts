/** One honest committed block of `events` accepted normal transactions over
 * a predecessor ledger of `entries` key-owned outputs, each the chronology
 * seed deposit's: the `i`th transaction spends the `i`th entry, signed by its
 * owner, and pays its output on, so every event's input is present at its
 * pre-state and no family has a fault to find. The operator's step roots are
 * taken from one trie it edits as it applies each event, so building the
 * block rebuilds no trie per step. */
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";

import type { LedgerEntry } from "../src/ledger-at.js";
import { committedL2BlockFixture } from "../tests/helpers/committed-l2-block-fixture.js";
import {
  chronologyDeposit,
  chronologyDepositEntry,
  signedChronologyTransaction,
} from "../tests/helpers/input-existence-chronology-seed.js";

const OWNED_OUTPUT = chronologyDepositEntry(chronologyDeposit(81)).output;

const leaf = ({ outRef, output }: LedgerEntry) => ({
  key: outRef,
  value: buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef,
    outputCbor: output,
  }).descriptorCbor,
});

const rootOf = (trie: Trie): string => Buffer.from(trie.hash).toString("hex");

export const eventLedgerBlock = async ({
  events,
  entries,
}: {
  readonly events: number;
  readonly entries: number;
}) => {
  if (events > entries)
    throw new Error("each event spends its own predecessor entry");
  const predecessorEntries = Array.from({ length: entries }, (_, index) => {
    const txId = Buffer.alloc(32, 0x5a);
    txId.writeUInt32BE(index);
    return {
      outRef: encodeMidgardSpendInputItem({ txId, outputIndex: 0 }),
      output: OWNED_OUTPUT,
    };
  });
  const txs = predecessorEntries.slice(0, events).map(({ outRef, output }) =>
    signedChronologyTransaction({
      spendInputs: [outRef],
      outputs: [output],
      fee: 0n,
    }),
  );
  const trie = await Trie.fromList(predecessorEntries.map(leaf));
  const prevUtxosRoot = rootOf(trie);
  const roots = [prevUtxosRoot];
  for (const [index, tx] of txs.entries()) {
    const spent = predecessorEntries[index]!;
    await trie.delete(spent.outRef);
    const created = leaf({
      outRef: encodeMidgardSpendInputItem({
        txId: Buffer.from(tx.txId, "hex"),
        outputIndex: 0,
      }),
      output: spent.output,
    });
    await trie.insert(created.key, created.value);
    roots.push(rootOf(trie));
  }
  return {
    ...(await committedL2BlockFixture({
      predecessorEntries,
      txs,
      steps: txs.map((_, index) => ({
        pre: roots[index]!,
        post: roots[index + 1]!,
      })),
    })),
    txs,
    predecessorEntries,
  };
};
