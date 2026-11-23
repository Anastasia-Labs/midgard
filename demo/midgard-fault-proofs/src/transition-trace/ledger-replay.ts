import {
  buildMidgardMpfDeletionOpening,
  decodeMidgardSpendInputItem,
  parseMidgardMpfProofJson,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation/ledger-output-descriptor";
import { Data } from "@lucid-evolution/lucid";

import { readingLedger } from "../event-ledger.ledger.js";
import { type Ledger, ledgerTrie } from "../ledger-store/index.js";

/** `run`, with a ledger no longer readable named this process's own fault. */
const reading = async <T>(run: () => Promise<T>): Promise<T> =>
  await readingLedger("transition replay ledger", run);

/** Mutation witness builder over `ledger`, whose root must be `expectedRoot`.
 * Its edits stay in its own trie, so `ledger` is unchanged by them, and a
 * ledger no longer readable is this process's own fault (`ReplayLocalFault`).
 * Authority comes from the caller's authenticated descriptor root; this
 * helper never classifies a block or admits a challenge. */
export const createTransitionTraceLedgerReplay = async ({
  ledger,
  expectedRoot,
}: {
  readonly ledger: Ledger;
  readonly expectedRoot: string;
}) => {
  if (ledger.root !== expectedRoot)
    throw new Error(
      "Transition replay ledger root differs from the authenticated predecessor root",
    );
  const trie = await reading(async () => await ledgerTrie(ledger));
  const root = () =>
    trie.hash === null || trie.hash === undefined
      ? SDK.EMPTY_MERKLE_TREE_ROOT
      : Buffer.from(trie.hash).toString("hex");
  return Object.freeze({
    root,
    has: async (key: Buffer): Promise<boolean> =>
      await reading(async () => (await trie.get(key)) !== undefined),
    delete: async (key: Buffer): Promise<SDKFraudProof.LedgerDeleteWitness> =>
      await reading(async () => {
        decodeMidgardSpendInputItem(key);
        const value = await trie.get(key);
        if (value === undefined)
          throw new Error("Transition replay cannot delete an absent input");
        const proved = await trie.prove(key);
        const opening = await buildMidgardMpfDeletionOpening(
          trie,
          key,
          parseMidgardMpfProofJson(proved.toJSON()),
        );
        await trie.delete(key);
        return {
          key: key.toString("hex"),
          value: value.toString("hex"),
          opening: opening.toString("hex"),
          delete_proof: Data.from(proved.toCBOR().toString("hex"), SDK.Proof),
        };
      }),
    insert: async (
      key: Buffer,
      output: Buffer,
    ): Promise<SDKFraudProof.LedgerInsertWitness> =>
      await reading(async () => {
        decodeMidgardSpendInputItem(key);
        if ((await trie.get(key)) !== undefined)
          throw new Error(
            "Transition replay cannot replace an existing output",
          );
        const value = Buffer.from(
          buildCanonicalMidgardLedgerEntryOutputMaterial({
            outRef: key,
            outputCbor: output,
          }).descriptorCbor,
        );
        await trie.insert(key, value);
        const proof = Data.from(
          (await trie.prove(key)).toCBOR().toString("hex"),
          SDK.Proof,
        );
        return {
          key: key.toString("hex"),
          value: value.toString("hex"),
          insert_proof: proof,
        };
      }),
  });
};
