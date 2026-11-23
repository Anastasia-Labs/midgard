/**
 * Ledger tries, roots and mutation steps for tests, built from complete
 * entries with the MPF library's own `Trie.fromList`
 * over each entry's canonical descriptor. Production code holds its ledger
 * in the store and hands a trie to `applyValidationLedgerMutations`; tests
 * that start from a list of entries build that trie here.
 */
import { Trie } from "#merkle-patricia-forestry";

import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "../src/ledger-output-descriptor.js";
import {
  applyValidationLedgerMutations,
  committedLedgerRoot,
  exactTrieRoot,
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerMutationStep,
  type ValidationMachineLedgerOp,
} from "../src/validation-machine/ledger-mutation.js";

/** A fresh library trie holding exactly `entries`, keyed by out-ref with
 * each output's canonical descriptor as its value. */
export const ledgerTrieOver = async (
  entries: readonly ValidationMachineLedgerEntry[],
): Promise<Trie> =>
  await Trie.fromList(
    entries.map(({ outRef, output }) => ({
      key: Buffer.from(outRef),
      value: buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef,
        outputCbor: output,
      }).descriptorCbor,
    })),
  );

/** The committed root of the ledger holding exactly `entries`. */
export const ledgerRootOver = async (
  entries: readonly ValidationMachineLedgerEntry[],
): Promise<Buffer> =>
  committedLedgerRoot(exactTrieRoot(await ledgerTrieOver(entries)));

/** The mutation steps `operations` make, in order, on the ledger holding
 * exactly `initialEntries`. */
export const mutationStepsOver = async ({
  initialEntries,
  operations,
}: {
  readonly initialEntries: readonly ValidationMachineLedgerEntry[];
  readonly operations: readonly ValidationMachineLedgerOp[];
}): Promise<readonly ValidationMachineLedgerMutationStep[]> =>
  await applyValidationLedgerMutations(
    await ledgerTrieOver(initialEntries),
    operations,
  );
