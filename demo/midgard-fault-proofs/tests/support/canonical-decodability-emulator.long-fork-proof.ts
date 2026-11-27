import { Proof } from "@aiken-lang/merkle-patricia-forestry";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";

/** Worst admitted prefix-check work: next cursor i admits at most 63-i
 * nibble bytes. Forks at i=1..63 therefore check 62+...+0=1953 bytes;
 * opposite-half neighbors force7 sparse-Merkle hashes per Fork; the last
 * cursor64 is a Branch. The library verifies this selected opening,
 * but its opaque sibling roots are not a materialized full native-source Trie.
 */
export const verifiedMaximumForkPrefixes = (
  key: Buffer,
  value: Buffer,
  terminalKind: "branch" | "leaf" = "branch",
) => {
  const path = computeHash32(key).toString("hex");
  const forkSteps = Array.from({ length: 63 }, (_unused, index) => ({
    type: "fork",
    skip: 0,
    neighbor: {
      nibble: Number.parseInt(path[index]!, 16) ^ 8,
      prefix: "0f".repeat(62 - index),
      root: computeHash32(Buffer.from([0xf0, index])).toString("hex"),
    },
  }));
  const lastBranch = {
    type: "branch",
    skip: 0,
    neighbors: Buffer.concat(
      Array.from({ length: 4 }, (_unused, index) =>
        computeHash32(Buffer.from([0xb0, index])),
      ),
    ).toString("hex"),
  };
  // A cursor64 Fork is impossible; a Leaf at that cursor can still select
  // the opposite half and requires seven sparse-Merkle hashes. Preserve the
  // shared first63 path nibbles so the library also verifies the Leaf opening.
  const neighborPath = `${path.slice(0, 63)}${(
    Number.parseInt(path[63]!, 16) ^ 8
  ).toString(16)}`;
  const lastLeaf = {
    type: "leaf",
    skip: 0,
    neighbor: {
      key: neighborPath,
      value: computeHash32(Buffer.from([0xe0])).toString("hex"),
    },
  };
  const proof = Proof.fromJSON(key, value, [
    ...forkSteps,
    terminalKind === "leaf" ? lastLeaf : lastBranch,
  ]);
  const root = proof.verify(true);
  if (root === null || root.length !== 32)
    throw new Error("Bounded Fork opening did not reconstruct a 32-byte root");
  const cbor = proof.toCBOR();
  return {
    transactionsPhasRoot: root.toString("hex"),
    proofCbor: cbor.toString("hex"),
    proofCborBytes: cbor.length,
    forkSteps: 63,
    prefixBytes: 1953,
    terminalKind,
  };
};
