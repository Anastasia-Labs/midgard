import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { directOutputAddress, directReceiveCredentials } from "./purposes.js";

const rootHex = (hash: Uint8Array | null | undefined): string =>
  hash === null ||
  hash === undefined ||
  Buffer.from(hash).equals(Buffer.alloc(32))
    ? SDK.EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(hash).toString("hex");

/** One prefix pass records exact earliest occurrences; one sorted pass builds
 * the same map. Every real output gets evidence, including plain outputs. */
export const buildDirectReceiveEvidence = async (
  outputs: readonly Buffer[],
) => {
  const trie = await Trie.fromList([]);
  const first = new Map<string, number>();
  const witnesses: SDK.DirectReceiveOutputWitness[] = [];
  const outputRoots: string[] = [SDK.EMPTY_MERKLE_TREE_ROOT];
  for (const [index, item] of outputs.entries()) {
    const address = directOutputAddress(item);
    if (!address.protected || address.paymentCredential.kind !== "Script") {
      witnesses.push("NoCredential");
    } else {
      const hash = address.paymentCredential.hash.toString("hex");
      const key = Buffer.from(hash, "hex");
      const oldRoot = rootHex(trie.hash);
      const previous = first.get(hash);
      if (previous === undefined) {
        await trie.insert(key, encodeCbor(BigInt(index)));
        const proof = await trie.prove(key);
        if (
          rootHex(proof.verify(false)) !== oldRoot ||
          rootHex(proof.verify(true)) !== rootHex(trie.hash)
        )
          throw new Error("direct receive evidence: insertion root differs");
        witnesses.push({
          FirstCredential: {
            proof: Data.from(
              Buffer.from(proof.toCBOR()).toString("hex"),
              SDK.Proof,
            ),
          },
        });
        first.set(hash, index);
      } else {
        const proof = await trie.prove(key);
        if (rootHex(proof.verify(true)) !== oldRoot)
          throw new Error(
            "direct receive evidence: repeated credential root differs",
          );
        witnesses.push({
          RepeatedCredential: {
            first_output_index: BigInt(previous),
            proof: Data.from(
              Buffer.from(proof.toCBOR()).toString("hex"),
              SDK.Proof,
            ),
          },
        });
      }
    }
    outputRoots.push(rootHex(trie.hash));
  }
  const declarationTrie = await Trie.fromList([]);
  const entries: SDK.DirectDeclaredCredential[] = [];
  const declarationRoots: string[] = [SDK.EMPTY_MERKLE_TREE_ROOT];
  for (const credential of directReceiveCredentials(outputs)) {
    const oldRoot = rootHex(declarationTrie.hash);
    const key = Buffer.from(credential.hash, "hex");
    await declarationTrie.insert(
      key,
      encodeCbor(BigInt(credential.firstOutputIndex)),
    );
    const proof = await declarationTrie.prove(key);
    if (rootHex(proof.verify(false)) !== oldRoot)
      throw new Error("direct receive evidence: declaration insertion differs");
    entries.push({
      script_hash: credential.hash,
      first_output_index: BigInt(credential.firstOutputIndex),
      proof: Data.from(Buffer.from(proof.toCBOR()).toString("hex"), SDK.Proof),
    });
    declarationRoots.push(rootHex(declarationTrie.hash));
  }
  const outputRoot = rootHex(trie.hash);
  if (
    rootHex(declarationTrie.hash) !== outputRoot ||
    entries.length !== first.size
  )
    throw new Error("direct receive evidence: sorted exact set differs");
  return { witnesses, entries, outputRoots, declarationRoots, outputRoot };
};
