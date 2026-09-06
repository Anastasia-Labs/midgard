import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { decodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import type { KeyValuePhasEntry } from "./phas.js";

/** Mutation witness builder. Authority comes from the caller's authenticated
 * descriptor root; this helper never classifies a block or admits a challenge. */
export const createTransitionTraceLedgerReplay = async ({
  entries,
  expectedRoot,
}: {
  readonly entries: readonly KeyValuePhasEntry[];
  readonly expectedRoot: string;
}) => {
  const values = new Map<string, Buffer>();
  for (const entry of entries) {
    decodeMidgardSpendInputItem(entry.key);
    const key = entry.key.toString("hex");
    if (values.has(key))
      throw new Error("Transition replay has a duplicate ledger key");
    values.set(key, Buffer.from(entry.value));
  }
  const trie = await Trie.fromList(
    entries.map(({ key, value }) => ({
      key: Buffer.from(key),
      value: Buffer.from(value),
    })),
  );
  const root = () =>
    trie.hash === null || trie.hash === undefined
      ? SDK.EMPTY_MERKLE_TREE_ROOT
      : Buffer.from(trie.hash).toString("hex");
  if (root() !== expectedRoot)
    throw new Error(
      "Transition replay descriptor trie differs from authenticated predecessor root",
    );
  return Object.freeze({
    root,
    async delete(key: Buffer): Promise<SDK.LedgerDeleteWitness> {
      decodeMidgardSpendInputItem(key);
      const value = values.get(key.toString("hex"));
      if (value === undefined)
        throw new Error("Transition replay cannot delete an absent input");
      const proof = Data.from(
        (await trie.prove(key)).toCBOR().toString("hex"),
        SDK.Proof,
      );
      await trie.delete(key);
      values.delete(key.toString("hex"));
      return {
        key: key.toString("hex"),
        value: value.toString("hex"),
        membership_proof: proof,
        delete_proof: proof,
      };
    },
    async insert(
      key: Buffer,
      output: Buffer,
    ): Promise<SDK.LedgerInsertWitness> {
      decodeMidgardSpendInputItem(key);
      if (values.has(key.toString("hex")))
        throw new Error("Transition replay cannot replace an existing output");
      const value = Buffer.from(
        buildCanonicalMidgardLedgerEntryOutputMaterial({
          outRef: key,
          outputCbor: output,
        }).descriptorCbor,
      );
      await trie.insert(key, value);
      values.set(key.toString("hex"), value);
      const proof = Data.from(
        (await trie.prove(key)).toCBOR().toString("hex"),
        SDK.Proof,
      );
      return {
        key: key.toString("hex"),
        value: value.toString("hex"),
        non_membership_proof: proof,
        insert_proof: proof,
      };
    },
  });
};
