import { createHash } from "node:crypto";

import type { MidgardLedgerVKeyWitness } from "@al-ft/midgard-validation";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  DEFAULT_NODE_ED25519_CACHE_MAX_ENTRIES,
  NodeEd25519Verifier,
} from "../src/workers/utils/ed25519-verifier.js";

const witness = (
  vkey: Buffer,
  signature: Buffer,
): MidgardLedgerVKeyWitness => ({
  index: 0,
  keyHash: Buffer.alloc(28),
  vkey,
  signature,
});

const cmlVerdict = (
  message: Buffer,
  value: MidgardLedgerVKeyWitness,
): boolean => {
  if (value.vkey.length !== 32 || value.signature.length !== 64) return false;
  try {
    const publicKey = CML.PublicKey.from_bytes(value.vkey);
    try {
      const signature = CML.Ed25519Signature.from_raw_bytes(value.signature);
      try {
        return publicKey.verify(message, signature);
      } finally {
        signature.free();
      }
    } finally {
      publicKey.free();
    }
  } catch {
    return false;
  }
};

describe("worker-local Node Ed25519 verifier", () => {
  it("passes its RFC 8032 valid/invalid startup self-test without retaining state", () => {
    const verifier = new NodeEd25519Verifier();
    expect(() => verifier.assertReady()).not.toThrow();
    expect(verifier.stats()).toStrictEqual({
      size: 0,
      maxEntries: DEFAULT_NODE_ED25519_CACHE_MAX_ENTRIES,
      hits: 0,
      misses: 0,
      evictions: 0,
    });
  });

  it("matches CML for randomized valid and invalid signatures", () => {
    const verifier = new NodeEd25519Verifier();
    const privateKeys = Array.from({ length: 32 }, () =>
      CML.PrivateKey.generate_ed25519(),
    );
    try {
      for (let sample = 0; sample < 256; sample += 1) {
        const privateKey = privateKeys[sample % privateKeys.length]!;
        const publicKey = privateKey.to_public();
        const message = createHash("sha256")
          .update(`node-ed25519-differential-${sample.toString()}`)
          .digest();
        const signature = privateKey.sign(message);
        try {
          const valid = witness(
            Buffer.from(publicKey.to_raw_bytes()),
            Buffer.from(signature.to_raw_bytes()),
          );
          expect(verifier.verify(message, valid)).toBe(
            cmlVerdict(message, valid),
          );
          const corruptedSignature = Buffer.from(valid.signature);
          corruptedSignature[sample % corruptedSignature.length] ^= 0x01;
          const invalid = witness(valid.vkey, corruptedSignature);
          expect(verifier.verify(message, invalid)).toBe(
            cmlVerdict(message, invalid),
          );
        } finally {
          signature.free();
          publicKey.free();
        }
      }
      expect(verifier.stats()).toMatchObject({
        size: 32,
        maxEntries: DEFAULT_NODE_ED25519_CACHE_MAX_ENTRIES,
        hits: 480,
        misses: 32,
        evictions: 0,
      });
    } finally {
      for (const privateKey of privateKeys) privateKey.free();
    }
  });

  it("fails malformed shapes closed without poisoning the cache", () => {
    const verifier = new NodeEd25519Verifier();
    expect(
      verifier.verify(
        Buffer.alloc(32),
        witness(Buffer.alloc(31), Buffer.alloc(64)),
      ),
    ).toBe(false);
    expect(
      verifier.verify(
        Buffer.alloc(32),
        witness(Buffer.alloc(32), Buffer.alloc(63)),
      ),
    ).toBe(false);
    expect(verifier.stats()).toStrictEqual({
      size: 0,
      maxEntries: DEFAULT_NODE_ED25519_CACHE_MAX_ENTRIES,
      hits: 0,
      misses: 0,
      evictions: 0,
    });
  });

  it("evicts least-recently-used keys at the configured bound", () => {
    const verifier = new NodeEd25519Verifier(2);
    const message = Buffer.alloc(32);
    const signature = Buffer.alloc(64);
    const keys = [0, 1, 2].map((index) =>
      createHash("sha256").update(`cache-key-${index.toString()}`).digest(),
    );
    for (const key of [keys[0]!, keys[1]!, keys[0]!, keys[2]!]) {
      expect(verifier.verify(message, witness(key, signature))).toBe(false);
    }
    expect(verifier.stats()).toStrictEqual({
      size: 2,
      maxEntries: 2,
      hits: 1,
      misses: 3,
      evictions: 1,
    });
    expect(verifier.verify(message, witness(keys[1]!, signature))).toBe(false);
    expect(verifier.stats()).toStrictEqual({
      size: 2,
      maxEntries: 2,
      hits: 1,
      misses: 4,
      evictions: 2,
    });
  });

  it("caps the production cache and its measured RSS envelope", () => {
    const verifier = new NodeEd25519Verifier();
    const message = Buffer.alloc(32);
    const signature = Buffer.alloc(64);
    const rssBefore = process.memoryUsage().rss;
    for (
      let index = 0;
      index <= DEFAULT_NODE_ED25519_CACHE_MAX_ENTRIES;
      index += 1
    ) {
      const key = createHash("sha256")
        .update(`rss-key-${index.toString()}`)
        .digest();
      expect(verifier.verify(message, witness(key, signature))).toBe(false);
    }
    const rssGrowthBytes = Math.max(0, process.memoryUsage().rss - rssBefore);
    expect(verifier.stats()).toStrictEqual({
      size: DEFAULT_NODE_ED25519_CACHE_MAX_ENTRIES,
      maxEntries: DEFAULT_NODE_ED25519_CACHE_MAX_ENTRIES,
      hits: 0,
      misses: DEFAULT_NODE_ED25519_CACHE_MAX_ENTRIES + 1,
      evictions: 1,
    });
    expect(rssGrowthBytes).toBeLessThan(256 * 1024 * 1024);
    console.log(
      JSON.stringify({
        productionNodeEd25519Cache: verifier.stats(),
        rssGrowthBytes,
      }),
    );
  });
});
