import { type Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardMpfDeletionOpening,
  parseMidgardMpfProofJson,
} from "@al-ft/midgard-core";
import { Proof } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

/** The proof of `key` in the delta map `trie`, and, when the contribution
 * removes the unit, the terminal-Branch group opening the removal needs
 * (`terminal_branch_keeps_two_children`); otherwise an empty opening. */
export const proveDeltaUnit = async (
  trie: Trie,
  key: Buffer,
  removal: boolean,
): Promise<{ readonly proof: Proof; readonly opening: string }> => {
  const proved = await trie.prove(key);
  const opening = removal
    ? await buildMidgardMpfDeletionOpening(
        trie,
        key,
        parseMidgardMpfProofJson(proved.toJSON()),
      )
    : Buffer.alloc(0);
  return {
    proof: Data.from(proved.toCBOR().toString("hex"), Proof),
    opening: opening.toString("hex"),
  };
};
