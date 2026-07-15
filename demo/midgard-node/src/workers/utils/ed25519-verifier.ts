import {
  createPublicKey,
  type KeyObject,
  verify as verifySignature,
} from "node:crypto";

import type { MidgardLedgerVKeyWitness } from "@al-ft/midgard-validation";

import type { ValidationCacheStats } from "./validation-pool.js";

const ED25519_PUBLIC_KEY_LENGTH = 32;
const ED25519_SIGNATURE_LENGTH = 64;
const ED25519_SPKI_PREFIX = Buffer.from("302a300506032b6570032100", "hex");
const RFC_8032_EMPTY_MESSAGE_PUBLIC_KEY = Buffer.from(
  "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a",
  "hex",
);
const RFC_8032_EMPTY_MESSAGE_SIGNATURE = Buffer.from(
  "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b",
  "hex",
);
export const DEFAULT_NODE_ED25519_CACHE_MAX_ENTRIES = 4_096;

/**
 * Worker-local Ed25519 verifier backed by Node/OpenSSL.
 *
 * Raw Cardano verification keys are wrapped in the exact RFC 8410 Subject
 * Public Key Info prefix before import. Only successfully imported keys enter
 * the bounded LRU; malformed inputs fail closed and cannot poison the cache.
 */
export class NodeEd25519Verifier {
  readonly #publicKeys = new Map<string, KeyObject>();
  readonly #maxEntries: number;
  #hits = 0;
  #misses = 0;
  #evictions = 0;

  constructor(maxEntries = DEFAULT_NODE_ED25519_CACHE_MAX_ENTRIES) {
    this.#maxEntries = Math.max(1, Math.floor(maxEntries));
  }

  readonly verify = (
    message: Buffer,
    witness: MidgardLedgerVKeyWitness,
  ): boolean => {
    if (
      witness.vkey.length !== ED25519_PUBLIC_KEY_LENGTH ||
      witness.signature.length !== ED25519_SIGNATURE_LENGTH
    ) {
      return false;
    }
    try {
      return verifySignature(
        null,
        message,
        this.#publicKey(witness.vkey),
        witness.signature,
      );
    } catch {
      return false;
    }
  };

  /** Fail worker startup if this Node/OpenSSL runtime cannot verify Ed25519. */
  assertReady(): void {
    const validWitness: MidgardLedgerVKeyWitness = {
      index: 0,
      keyHash: Buffer.alloc(28),
      vkey: RFC_8032_EMPTY_MESSAGE_PUBLIC_KEY,
      signature: RFC_8032_EMPTY_MESSAGE_SIGNATURE,
    };
    const invalidSignature = Buffer.from(RFC_8032_EMPTY_MESSAGE_SIGNATURE);
    invalidSignature[0] ^= 0x01;
    if (
      !this.verify(Buffer.alloc(0), validWitness) ||
      this.verify(Buffer.alloc(0), {
        ...validWitness,
        signature: invalidSignature,
      })
    ) {
      throw new Error("Node/OpenSSL Ed25519 readiness self-test failed");
    }
    this.clear();
  }

  stats(): ValidationCacheStats {
    return {
      size: this.#publicKeys.size,
      maxEntries: this.#maxEntries,
      hits: this.#hits,
      misses: this.#misses,
      evictions: this.#evictions,
    };
  }

  clear(): void {
    this.#publicKeys.clear();
    this.#hits = 0;
    this.#misses = 0;
    this.#evictions = 0;
  }

  #publicKey(rawKey: Buffer): KeyObject {
    const cacheKey = rawKey.toString("hex");
    const cached = this.#publicKeys.get(cacheKey);
    if (cached !== undefined) {
      this.#publicKeys.delete(cacheKey);
      this.#publicKeys.set(cacheKey, cached);
      this.#hits += 1;
      return cached;
    }

    const publicKey = createPublicKey({
      key: Buffer.concat([ED25519_SPKI_PREFIX, rawKey]),
      format: "der",
      type: "spki",
    });
    if (publicKey.asymmetricKeyType !== "ed25519") {
      throw new Error("raw verification key did not import as Ed25519");
    }
    this.#misses += 1;
    if (this.#publicKeys.size >= this.#maxEntries) {
      const oldestKey = this.#publicKeys.keys().next().value as
        | string
        | undefined;
      if (oldestKey !== undefined) {
        this.#publicKeys.delete(oldestKey);
        this.#evictions += 1;
      }
    }
    this.#publicKeys.set(cacheKey, publicKey);
    return publicKey;
  }
}
