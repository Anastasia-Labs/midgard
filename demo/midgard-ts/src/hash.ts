/**
 * Hashing helpers for Midgard binary-codec structures.
 *
 * All Midgard "roots"/"hashes" over codec payloads are blake2b-256 of the
 * canonical binary encoding of the corresponding value.
 */

import { blake2b } from "@noble/hashes/blake2.js";

import type { Hash32 } from "./types/primitives";

/** blake2b-256 (32-byte) digest. */
export function blake2b256(bytes: Uint8Array): Hash32 {
  return blake2b(bytes, { dkLen: 32 });
}

/** Alias used throughout the codec for blake2b-256 over a codec payload. */
export const computeHash32 = blake2b256;

export function bytesEqual(a: Uint8Array, b: Uint8Array): boolean {
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++) if (a[i] !== b[i]) return false;
  return true;
}

export function ensureHashMatch(
  root: Uint8Array,
  preimage: Uint8Array,
  fieldName: string,
): void {
  if (!bytesEqual(root, computeHash32(preimage))) {
    throw new Error(`Hash mismatch for ${fieldName}`);
  }
}
