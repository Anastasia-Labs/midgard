import { blake2b as nobleBlake2b } from "@noble/hashes/blake2.js";
import sodium from "libsodium-wrappers-sumo";

const SODIUM_MIN_OUTPUT_BYTES = 16;
const SODIUM_MAX_OUTPUT_BYTES = 64;

/**
 * Longest message handed to libsodium in one call. Longer messages go through
 * its streaming interface in chunks of this size, because a one-shot call
 * copies the whole message into the wasm heap and wasm memory never shrinks.
 */
const SODIUM_CHUNK_BYTES = 64 * 1024;

let sodiumReady = false;

const backendCounts = { sodium: 0, noble: 0 };

/**
 * Resolves `true` once {@link midgardBlake2b} hashes with libsodium, or
 * `false` if libsodium failed to initialise (every digest then stays on noble).
 */
export const midgardBlake2bReady: Promise<boolean> = sodium.ready.then(
  () => {
    sodiumReady = true;
    return true;
  },
  () => false,
);

/**
 * The one Blake2b entry point for Midgard hashing, with the signature of
 * `@noble/hashes`' `blake2b(message, { dkLen })`.
 *
 * Once libsodium has initialised, digests come from its `crypto_generichash`
 * (the audited C Blake2b, compiled to wasm); before that, and for an output
 * length libsodium does not support, from `@noble/hashes`. Both compute
 * unkeyed, unsalted, unpersonalised Blake2b with a `dkLen`-byte output, so a
 * digest never depends on which backend produced it
 * (`tests/blake2b-backend.test.ts` checks them against each other). libsodium
 * is about 8x faster on 64 bytes and 25x on a mebibyte.
 *
 * Initialisation instantiates a wasm module asynchronously and every caller is
 * synchronous, so nothing waits for it: the first digests of a process may be
 * computed by noble.
 */
export const midgardBlake2b = (
  message: Uint8Array,
  options: { readonly dkLen: number },
): Uint8Array => {
  const outputLength = options.dkLen;
  if (
    !sodiumReady ||
    !(message instanceof Uint8Array) ||
    !Number.isInteger(outputLength) ||
    outputLength < SODIUM_MIN_OUTPUT_BYTES ||
    outputLength > SODIUM_MAX_OUTPUT_BYTES
  ) {
    backendCounts.noble++;
    return nobleBlake2b(message, { dkLen: outputLength });
  }
  backendCounts.sodium++;
  if (message.length <= SODIUM_CHUNK_BYTES) {
    return sodium.crypto_generichash(outputLength, message, null);
  }
  const state = sodium.crypto_generichash_init(null, outputLength);
  for (let offset = 0; offset < message.length; offset += SODIUM_CHUNK_BYTES) {
    sodium.crypto_generichash_update(
      state,
      message.subarray(offset, offset + SODIUM_CHUNK_BYTES),
    );
  }
  return sodium.crypto_generichash_final(state, outputLength);
};

/** How many digests each backend has computed in this process. */
export const midgardBlake2bBackendCounts = (): {
  readonly sodium: number;
  readonly noble: number;
} => ({ ...backendCounts });
