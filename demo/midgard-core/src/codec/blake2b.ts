// The ambient declaration has to travel with this file: every workspace
// package type-checks midgard-core from source, and only a reference pulls
// the declaration into their programs.
// eslint-disable-next-line @typescript-eslint/triple-slash-reference
/// <reference path="./libsodium-wrappers-sumo.d.ts" />
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

/**
 * One fixed region of libsodium's wasm heap, allocated once: a message of up
 * to {@link SODIUM_CHUNK_BYTES} is copied to `input` and its digest written
 * to `output`. libsodium's wrapper allocates and frees both regions on every
 * call instead; the digest is the same `crypto_generichash`.
 */
let scratch: { readonly input: number; readonly output: number } | null = null;

/**
 * Resolves `true` once {@link midgardBlake2b} hashes with libsodium, or
 * `false` if libsodium failed to initialise (every digest then stays on noble).
 */
export const midgardBlake2bReady: Promise<boolean> = sodium.ready
  .then(() => {
    const input = sodium.libsodium._malloc(
      SODIUM_CHUNK_BYTES + SODIUM_MAX_OUTPUT_BYTES,
    );
    if (input === 0) return false;
    scratch = { input, output: input + SODIUM_CHUNK_BYTES };
    return true;
  })
  .catch(() => false);

const sodiumOutputLength = (outputLength: number): boolean =>
  Number.isInteger(outputLength) &&
  outputLength >= SODIUM_MIN_OUTPUT_BYTES &&
  outputLength <= SODIUM_MAX_OUTPUT_BYTES;

/**
 * Hashes `parts`, `length` bytes in all and at most
 * {@link SODIUM_CHUNK_BYTES}, in the scratch region; returns the address of
 * the `outputLength`-byte digest, valid until the next call.
 */
const sodiumDigest = (
  region: NonNullable<typeof scratch>,
  parts: readonly Uint8Array[],
  outputLength: number,
): number => {
  const module = sodium.libsodium;
  const heap = module.HEAPU8;
  let length = 0;
  for (const part of parts) {
    heap.set(part, region.input + length);
    length += part.length;
  }
  if (
    module._crypto_generichash(
      region.output,
      outputLength,
      region.input,
      length,
      0,
      0,
      0,
    ) !== 0
  )
    throw new Error("invalid usage");
  return region.output;
};

/** Streams `parts` through libsodium in chunks of {@link SODIUM_CHUNK_BYTES}. */
const sodiumStreamedDigest = (
  parts: readonly Uint8Array[],
  outputLength: number,
): Uint8Array => {
  const state = sodium.crypto_generichash_init(null, outputLength);
  for (const part of parts)
    for (let offset = 0; offset < part.length; offset += SODIUM_CHUNK_BYTES)
      sodium.crypto_generichash_update(
        state,
        part.subarray(offset, offset + SODIUM_CHUNK_BYTES),
      );
  return sodium.crypto_generichash_final(state, outputLength);
};

/**
 * Whether `view` can be copied into the scratch region as it is: a plain
 * `Uint8Array` or `Buffer` (not a subclass, a Proxy or another view type)
 * that is not a view of libsodium's heap, which a digest overwrites.
 */
const plainView = (view: Uint8Array, heap: ArrayBufferLike | null): boolean =>
  ArrayBuffer.isView(view) &&
  (Object.getPrototypeOf(view) === Buffer.prototype ||
    Object.getPrototypeOf(view) === Uint8Array.prototype) &&
  view.buffer !== heap;

/**
 * A non-empty plain view: an empty one may view a detached buffer, which
 * `Buffer.from` reads as empty but copying into the heap refuses.
 */
const plainPart = (part: Uint8Array, heap: ArrayBufferLike | null): boolean =>
  plainView(part, heap) && part.length > 0;

/**
 * `parts` as hashable views of the same bytes. Any other part is copied with
 * `Buffer.from`, as `Buffer.concat(parts.map((part) => Buffer.from(part)))`
 * would read it (and throws as it would), before any byte reaches the heap.
 */
const plainParts = (
  parts: readonly Uint8Array[],
  heap: ArrayBufferLike | null,
): readonly Uint8Array[] => {
  for (const part of parts)
    if (!plainPart(part, heap))
      return parts.map((each) =>
        plainPart(each, heap) ? each : Buffer.from(each),
      );
  return parts;
};

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
 * The digest is a fresh `Uint8Array` of exactly `dkLen` bytes on its own
 * buffer: a later call never changes it.
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
  const region = scratch;
  if (
    region === null ||
    !(message instanceof Uint8Array) ||
    !sodiumOutputLength(outputLength)
  ) {
    return nobleBlake2b(message, { dkLen: outputLength });
  }
  if (message.length > SODIUM_CHUNK_BYTES)
    return sodiumStreamedDigest([message], outputLength);
  // Anything but a plain view goes through libsodium's own wrapper, which
  // copies it to a fresh allocation: a Proxy's getters may hash re-entrantly,
  // and a view of the heap overlaps the scratch region.
  if (!plainView(message, sodium.libsodium.HEAPU8.buffer))
    return sodium.crypto_generichash(outputLength, message, null);
  const output = sodiumDigest(region, [message], outputLength);
  return sodium.libsodium.HEAPU8.slice(output, output + outputLength);
};

/**
 * Blake2b-256 of the concatenation of `parts`, as a fresh 32-byte `Buffer`:
 * the bytes of `Buffer.from(midgardBlake2b(Buffer.concat(parts.map((part) =>
 * Buffer.from(part))), { dkLen: 32 }))`, for any parts, without building the
 * concatenation or copying the digest twice. For the hashes taken once per
 * validation-machine step.
 */
export const midgardBlake2b256 = (
  inputParts: readonly Uint8Array[],
): Buffer => {
  const region = scratch;
  const parts = plainParts(
    inputParts,
    region === null ? null : sodium.libsodium.HEAPU8.buffer,
  );
  let length = 0;
  for (const part of parts) length += part.length;
  if (region === null) {
    const hasher = nobleBlake2b.create({ dkLen: 32 });
    for (const part of parts) hasher.update(part);
    return Buffer.from(hasher.digest());
  }
  if (length > SODIUM_CHUNK_BYTES)
    return Buffer.from(sodiumStreamedDigest(parts, 32));
  const output = sodiumDigest(region, parts, 32);
  const digest = Buffer.allocUnsafe(32);
  digest.set(sodium.libsodium.HEAPU8.subarray(output, output + 32));
  return digest;
};
