#!/usr/bin/env node

/**
 * The `mpf-node-encoding-v1` cross-language golden channel: the node encoding
 * of the Merkle Patricia Forestry every Midgard root commits to, taken from the
 * MPF library itself.
 *
 * A leaf node hashes `suffix(path, cursor) ‖ blake2b_256(value)`, where the
 * suffix opens with 0xff at an even cursor and with 0x10 and the cursor's
 * nibble at an odd one. A branch node hashes its prefix nibbles (each below 16)
 * followed by the merkle root of its children. Leaf and branch node preimages
 * are therefore disjoint.
 *
 * Midgard holds several encoders of that rule: the Aiken library and
 * `mpf_proof_v1`, the TypeScript proof fold and catalogue reconstruction in
 * this package, and the PHAS and native-owner encoders in `midgard-node`. This
 * generator computes every value from the MPF library's `Trie` alone, never
 * from a Midgard encoder, so each consumer is checked against an independent
 * producer:
 *
 *   * `tests/fixtures/mpf-node-encoding-v1.generated.json` is read by the
 *     encoding vitests in this package and in `midgard-node`; and
 *   * `onchain/aiken/lib/midgard/mpf-node-encoding-v1-golden.test.ak` asserts
 *     the Aiken library and `mpf_proof_v1` on the same values.
 *
 * The suffix bytes are spelled here from the rule above and then checked
 * against the library: for every cursor, their hash with the value digest must
 * equal the library's own leaf hash for that cursor's nibble suffix.
 *
 * usage: node scripts/generate-mpf-node-encoding-v1-goldens.mjs [--check]
 */

import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  aikenBytes,
  formatAikenSource,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "./golden-channel.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/mpf-node-encoding-v1.generated.json",
);
const aikenFileName = "mpf-node-encoding-v1-golden.test.ak";
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard",
  aikenFileName,
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-mpf-node-encoding-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const EVEN_LEAF_MARKER = 0xff;
const ODD_LEAF_MARKER = 0x10;
const PATH_NIBBLE_COUNT = 64;

const digest = (bytes) => Buffer.from(blake2b(bytes, { dkLen: 32 }));
const fail = (message) => {
  throw new Error(`mpf-node-encoding-v1: ${message}`);
};
const nibbleAt = (path, cursor) =>
  cursor % 2 === 0 ? path[cursor >> 1] >> 4 : path[cursor >> 1] & 15;
const nibbleString = (path, from) =>
  path.toString("hex").slice(from, PATH_NIBBLE_COUNT);

const suffixBytes = (path, cursor) =>
  cursor % 2 === 0
    ? Buffer.concat([
        Buffer.from([EVEN_LEAF_MARKER]),
        path.subarray(cursor / 2),
      ])
    : Buffer.concat([
        Buffer.from([ODD_LEAF_MARKER, nibbleAt(path, cursor)]),
        path.subarray((cursor + 1) / 2),
      ]);

const keyOf = (index) => Buffer.from(`mpf-node-encoding-v1/${index}`);
const valueOf = (index) => Buffer.from(`value-${index}`);

const trieOf = async (entries) => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const { key, value } of entries) {
    await trie.insert(key, value);
  }
  return trie;
};
const rootOf = (trie) =>
  trie.hash == null ? Buffer.alloc(32) : Buffer.from(trie.hash);

// ## Suffix and leaf hash at every cursor

// The library's own leaf hash function, read from a leaf node it builds.
const leafNode = await Trie.fromList([
  { key: keyOf("leaf"), value: valueOf("leaf") },
]);
const libraryLeafHash = leafNode.constructor.computeHash;
if (typeof libraryLeafHash !== "function" || leafNode.prefix === undefined) {
  fail("the library's single-entry trie is not a leaf node");
}

const suffixKey = keyOf("suffix");
const suffixPath = digest(suffixKey);
const suffixValue = valueOf("suffix");
const suffixValueDigest = digest(suffixValue);
const suffixRows = Array.from(
  { length: PATH_NIBBLE_COUNT + 1 },
  (_, cursor) => {
    const suffix = suffixBytes(suffixPath, cursor);
    const prefix = nibbleString(suffixPath, cursor);
    const leafHash = Buffer.from(libraryLeafHash(prefix, suffixValueDigest));
    if (!digest(Buffer.concat([suffix, suffixValueDigest])).equals(leafHash)) {
      fail(`suffix at cursor ${cursor} is not what the library's leaf commits`);
    }
    return { cursor, prefix, suffix: hex(suffix), leafHash: hex(leafHash) };
  },
);

// ## A trie with branch prefixes and leaves at odd and even cursors

// Hashed paths, root first: every key opens with nibble a, so the root branch
// has prefix "a". Under a6, 79 is a leaf at cursor 3 beside a branch with
// prefix "4" over 1171 and 3922 (leaves at cursor 5). Under a9, 405 is a leaf
// at cursor 3 beside a branch over 37 and 2148 (leaves at cursor 4). 4 is a
// leaf at cursor 2.
const MEMBER_INDICES = [4, 37, 79, 405, 1171, 2148, 3922];
// Absent keys under a6 (1277) and a9 (2272).
const ABSENT_INDICES = [1277, 2272];
// Inserted into, then deleted from, the trie without it.
const MUTATED_INDEX = 79;
// Updated in place.
const UPDATED_INDEX = 405;

const entryOf = (index) => ({
  index,
  key: keyOf(index),
  value: valueOf(index),
});
const members = MEMBER_INDICES.map(entryOf);
const trie = await trieOf(members);
const root = rootOf(trie);

const stepCursor = (steps) =>
  steps.reduce((cursor, step) => cursor + step.skip + 1, 0);
const proofJson = async (source, key, allowMissing) =>
  (await source.prove(key, allowMissing)).toJSON();

const membership = [];
for (const { index, key, value } of members) {
  const proof = await proofJson(trie, key, false);
  membership.push({
    index,
    key: hex(key),
    value: hex(value),
    leafCursor: stepCursor(proof),
    proof,
  });
}

const exclusion = [];
for (const { index, key } of members) {
  const without = await trieOf(
    members.filter((entry) => entry.index !== index),
  );
  exclusion.push({
    index,
    key: hex(key),
    root: hex(rootOf(without)),
    proof: await proofJson(without, key, true),
  });
}
for (const index of ABSENT_INDICES) {
  const { key } = entryOf(index);
  exclusion.push({
    index,
    key: hex(key),
    root: hex(root),
    proof: await proofJson(trie, key, true),
  });
}

const oddLeaves = membership.filter(({ leafCursor }) => leafCursor % 2 === 1);
const evenLeaves = membership.filter(({ leafCursor }) => leafCursor % 2 === 0);
const prefixedSteps = membership.flatMap(({ proof }) =>
  proof.filter((step) => step.skip > 0),
);
const forkPrefixes = membership.flatMap(({ proof }) =>
  proof.filter((step) => step.type === "fork" && step.neighbor.prefix !== ""),
);
if (
  oddLeaves.length < 2 ||
  evenLeaves.length < 2 ||
  prefixedSteps.length === 0 ||
  forkPrefixes.length === 0
) {
  fail("the trie no longer has the shape this channel pins");
}

// ## One insert/delete pair and one update

const mutated = entryOf(MUTATED_INDEX);
const mutation = exclusion.find(({ index }) => index === MUTATED_INDEX);
const mutationMembership = membership.find(
  ({ index }) => index === MUTATED_INDEX,
);
const insertDelete = {
  index: MUTATED_INDEX,
  key: hex(mutated.key),
  value: hex(mutated.value),
  rootWithout: mutation.root,
  rootWith: hex(root),
  insertProof: mutation.proof,
  deleteProof: mutationMembership.proof,
};

const updated = entryOf(UPDATED_INDEX);
const updatedValue = Buffer.from(`value-${UPDATED_INDEX}-updated`);
const updatedTrie = await trieOf(
  members.map((entry) =>
    entry.index === UPDATED_INDEX ? { ...entry, value: updatedValue } : entry,
  ),
);
const update = {
  index: UPDATED_INDEX,
  key: hex(updated.key),
  oldValue: hex(updated.value),
  newValue: hex(updatedValue),
  rootBefore: hex(root),
  rootAfter: hex(rootOf(updatedTrie)),
  proof: membership.find(({ index }) => index === UPDATED_INDEX).proof,
};

const golden = {
  schema: "midgard-mpf-node-encoding-v1-golden",
  version: 1,
  generator:
    "demo/midgard-core/scripts/generate-mpf-node-encoding-v1-goldens.mjs",
  aikenModule: `onchain/aiken/lib/midgard/${aikenFileName}`,
  evenLeafMarker: hex([EVEN_LEAF_MARKER]),
  oddLeafMarker: hex([ODD_LEAF_MARKER]),
  suffixes: {
    key: hex(suffixKey),
    path: hex(suffixPath),
    value: hex(suffixValue),
    valueDigest: hex(suffixValueDigest),
    rows: suffixRows,
  },
  trie: {
    root: hex(root),
    membership,
    exclusion,
  },
  insertDelete,
  update,
};

writeOrCheck(generatedJsonPath, `${JSON.stringify(golden, null, 2)}\n`);

// ## Aiken module

const aikenStep = (step) => {
  if (step.type === "branch") {
    return `mpf.Branch { skip: ${step.skip}, neighbors: ${aikenBytes(step.neighbors)} }`;
  }
  if (step.type === "fork") {
    const { nibble, prefix, root: neighborRoot } = step.neighbor;
    return `mpf.Fork { skip: ${step.skip}, neighbor: mpf.Neighbor { nibble: ${nibble}, prefix: ${aikenBytes(prefix)}, root: ${aikenBytes(neighborRoot)} } }`;
  }
  return `mpf.Leaf { skip: ${step.skip}, key: ${aikenBytes(step.neighbor.key)}, value: ${aikenBytes(step.neighbor.value)} }`;
};
const aikenProof = (steps) => `[${steps.map(aikenStep).join(", ")}]`;

const suffixTable = suffixRows
  .map(
    ({ cursor, suffix, leafHash }) =>
      `Pair(${cursor}, Pair(${aikenBytes(suffix)}, ${aikenBytes(leafHash)}))`,
  )
  .join(",\n");

const membershipTests = membership
  .map(
    ({ index, key, value, proof }) => `
test member_${index}() {
  let proof = ${aikenProof(proof)}
  and {
    mpf.has(mpf.from_root(trie_root), ${aikenBytes(key)}, ${aikenBytes(value)}, proof),
    mpf_proof_v1.has(trie_root, ${aikenBytes(key)}, ${aikenBytes(value)}, proof),
    mpf_proof_v1.has_value_hash(trie_root, ${aikenBytes(key)}, blake2b_256(${aikenBytes(value)}), proof),
  }
}`,
  )
  .join("\n");

const exclusionTests = exclusion
  .map(
    ({ index, key, root: excludedRoot, proof }) => `
test absent_${index}() {
  mpf_proof_v1.does_not_have(${aikenBytes(excludedRoot)}, ${aikenBytes(key)}, ${aikenProof(proof)})
}`,
  )
  .join("\n");

const aikenSource = `//// Generated by
//// demo/midgard-core/scripts/generate-mpf-node-encoding-v1-goldens.mjs. Do not
//// edit; regenerate. Every value comes from the MPF library's Trie.
////
//// The MPF node encoding, pinned against the library that defines it. A leaf
//// node hashes its suffix (0xff at an even cursor, 0x10 and the cursor's
//// nibble at an odd one) and its value digest; a branch node hashes its prefix
//// nibbles and its children's merkle root. Leaf and branch node preimages are
//// disjoint.

use aiken/builtin.{blake2b_256}
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, suffix}
use midgard/mpf_proof_v1

const suffix_path = ${aikenBytes(hex(suffixPath))}

const suffix_value_digest = ${aikenBytes(hex(suffixValueDigest))}

const trie_root = ${aikenBytes(hex(root))}

test suffix_and_leaf_hash_at_every_cursor() {
  let rows = [
${suffixTable}
  ]
  and {
    list.length(rows) == ${suffixRows.length},
    list.all(
      rows,
      fn(row) {
        let Pair(cursor, Pair(expected, leaf_hash)) = row
        and {
          suffix(suffix_path, cursor) == expected,
          combine(expected, suffix_value_digest) == leaf_hash,
        }
      },
    ),
  }
}
${membershipTests}
${exclusionTests}

test insert_then_delete_${insertDelete.index}() {
  let key = ${aikenBytes(insertDelete.key)}
  let value = ${aikenBytes(insertDelete.value)}
  let without = ${aikenBytes(insertDelete.rootWithout)}
  let with = ${aikenBytes(insertDelete.rootWith)}
  let insert_proof = ${aikenProof(insertDelete.insertProof)}
  let delete_proof = ${aikenProof(insertDelete.deleteProof)}
  and {
    mpf.root(mpf.insert(mpf.from_root(without), key, value, insert_proof)) == with,
    mpf_proof_v1.insert_root(without, key, value, insert_proof) == Some(with),
    mpf_proof_v1.insert_root_paired_fold(without, key, value, insert_proof) == Some(with),
    mpf.root(mpf.delete(mpf.from_root(with), key, value, delete_proof)) == without,
    mpf_proof_v1.delete_root_paired_fold(with, key, value, delete_proof, #"") == Some(without),
  }
}

test update_${update.index}() {
  let key = ${aikenBytes(update.key)}
  let old_value = ${aikenBytes(update.oldValue)}
  let new_value = ${aikenBytes(update.newValue)}
  let before = ${aikenBytes(update.rootBefore)}
  let after = ${aikenBytes(update.rootAfter)}
  let proof = ${aikenProof(update.proof)}
  and {
    mpf_proof_v1.update_root(before, key, old_value, new_value, proof) == Some(after),
    mpf_proof_v1.update_root_paired_fold(before, key, old_value, new_value, proof) == Some(after),
  }
}
`;

writeOrCheck(
  generatedAikenPath,
  formatAikenSource({
    source: aikenSource,
    fileName: aikenFileName,
    repositoryRoot,
    tmpPrefix: "midgard-mpf-node-encoding-v1-aiken-format-",
  }),
);
