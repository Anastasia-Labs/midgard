/**
 * The root of a ledger trie built from its leaves in path order, holding
 * only the open branches on the path of the last leaf: at most 64 frames
 * of 16 child hashes, however many leaves it takes. It hashes as the
 * Merkle Patricia Forestry trie the node builds (`@aiken-lang/merkle-patricia-forestry`,
 * `computeLedgerMpfRootFromLedgerEntries`) does: a key's path is the
 * blake2b-256 of the key, 64 nibbles; a leaf hashes the rest of its path
 * from where its parent branches, then its value's digest; a branch hashes
 * its prefix's nibbles, then the binary Merkle root of its 16 children
 * (absent ones as 32 zero bytes); the empty trie's root is Midgard's
 * empty root (`EMPTY_MERKLE_TREE_ROOT`).
 */
import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";

const DIGEST_BYTES = 32;
const PATH_NIBBLES = 2 * DIGEST_BYTES;
const NULL_HASH = new Uint8Array(DIGEST_BYTES);
const EMPTY_ROOT = Buffer.from(SDK.EMPTY_MERKLE_TREE_ROOT, "hex");

/** blake2b-256 of `parts`, concatenated. */
const digest = (...parts: readonly Uint8Array[]): Uint8Array => {
  const hash = blake2b.create({ dkLen: DIGEST_BYTES });
  for (const part of parts) hash.update(part);
  return hash.digest();
};

/** The path of `key` in the trie: its blake2b-256. */
export const mpfPath = (key: Uint8Array): Uint8Array => digest(key);

/** The digest a leaf holding `value` commits to. */
export const mpfValueDigest = (value: Uint8Array): Uint8Array => digest(value);

const nibble = (path: Uint8Array, index: number): number =>
  index % 2 === 0 ? path[index >> 1]! >> 4 : path[index >> 1]! & 0x0f;

/** The number of nibbles `left` and `right` share from the start. */
const commonNibbles = (left: Uint8Array, right: Uint8Array): number => {
  for (let index = 0; index < DIGEST_BYTES; index += 1)
    if (left[index] !== right[index])
      return (left[index]! ^ right[index]!) > 0x0f ? 2 * index : 2 * index + 1;
  return PATH_NIBBLES;
};

/** A leaf's hash, `start` nibbles into its path: the rest of the path (an
 * odd count as 0x10 and its first nibble, then the bytes after it; an even
 * count as 0xff, then its bytes), then the value's digest. */
const leafHash = (path: Uint8Array, start: number, valueDigest: Uint8Array) =>
  digest(
    start % 2 === 1
      ? Uint8Array.of(0x10, nibble(path, start))
      : Uint8Array.of(0xff),
    path.subarray((start + 1) >> 1),
    valueDigest,
  );

/** The binary Merkle root of a branch's 16 children. */
const childrenRoot = (children: readonly (Uint8Array | undefined)[]) => {
  let level = Array.from(
    { length: 16 },
    (_, index) => children[index] ?? NULL_HASH,
  );
  while (level.length > 1) {
    const next: Uint8Array[] = [];
    for (let index = 0; index < level.length; index += 2)
      next.push(digest(level[index]!, level[index + 1]!));
    level = next;
  }
  return level[0]!;
};

/** A branch's hash: its prefix, the nibbles of `path` from `start` to
 * `end`, one byte each, then its children's root. */
const branchHash = (
  path: Uint8Array,
  start: number,
  end: number,
  children: readonly (Uint8Array | undefined)[],
) =>
  digest(
    Uint8Array.from({ length: end - start }, (_, offset) =>
      nibble(path, start + offset),
    ),
    childrenRoot(children),
  );

/** An open branch: the nibble its children branch at, a path through it,
 * and its children's hashes so far. */
type Frame = {
  readonly at: number;
  readonly path: Uint8Array;
  readonly children: (Uint8Array | undefined)[];
};

/**
 * Builds a trie's root from its leaves, pushed in strictly increasing path
 * order. A leaf's place is fixed once the next one is known: its parent
 * branches at the longer of its common prefixes with its two neighbours.
 * So each push hashes the leaf before it into its parent, and every
 * branch deeper than the new leaf's common prefix is complete and hashed
 * into its own parent.
 */
export class SortedMpfRoot {
  readonly #open: Frame[] = [];
  #last: { path: Uint8Array; valueDigest: Uint8Array } | undefined;
  #lastShared = -1;
  #root: Uint8Array | undefined;

  /** Adds the leaf at `path` (32 bytes) committing to `valueDigest`. */
  push(path: Uint8Array, valueDigest: Uint8Array): void {
    if (path.length !== DIGEST_BYTES || valueDigest.length !== DIGEST_BYTES)
      throw new Error("a trie leaf is a 32-byte path and a 32-byte digest");
    if (this.#root !== undefined)
      throw new Error("the trie's root was already taken");
    let shared = -1;
    if (this.#last !== undefined) {
      shared = commonNibbles(this.#last.path, path);
      if (
        shared === PATH_NIBBLES ||
        nibble(this.#last.path, shared) > nibble(path, shared)
      )
        throw new Error(
          `trie leaves out of path order: ${Buffer.from(path).toString("hex")} after ${Buffer.from(this.#last.path).toString("hex")}`,
        );
      this.#close(shared);
    }
    this.#last = {
      path: Uint8Array.from(path),
      valueDigest: Uint8Array.from(valueDigest),
    };
    this.#lastShared = shared;
  }

  /** The trie's root, as 64 hex; no leaf may follow. */
  root(): string {
    if (this.#root === undefined)
      this.#root = this.#last === undefined ? EMPTY_ROOT : this.#close(-1);
    return Buffer.from(this.#root).toString("hex");
  }

  /** Hashes the last leaf into its parent and closes every branch deeper
   * than `shared`, the nibbles the next leaf shares with it (-1 at the
   * end): the root once nothing is left open. */
  #close(shared: number): Uint8Array {
    const last = this.#last!;
    const parent = Math.max(this.#lastShared, shared);
    if (parent < 0) return leafHash(last.path, 0, last.valueDigest);
    this.#attach(
      parent,
      last.path,
      leafHash(last.path, parent + 1, last.valueDigest),
    );
    for (;;) {
      const frame = this.#open.at(-1)!;
      if (frame.at <= shared) return NULL_HASH;
      this.#open.pop();
      const above = Math.max(this.#open.at(-1)?.at ?? -1, shared);
      const hash = branchHash(frame.path, above + 1, frame.at, frame.children);
      if (above < 0) return hash;
      this.#attach(above, frame.path, hash);
    }
  }

  /** Sets `hash` as the child of the branch at nibble `at` on `path`,
   * opening that branch if the innermost open one is above it. */
  #attach(at: number, path: Uint8Array, hash: Uint8Array): void {
    let frame = this.#open.at(-1);
    if (frame === undefined || frame.at < at) {
      frame = { at, path, children: [] };
      this.#open.push(frame);
    }
    frame.children[nibble(path, at)] = hash;
  }
}
