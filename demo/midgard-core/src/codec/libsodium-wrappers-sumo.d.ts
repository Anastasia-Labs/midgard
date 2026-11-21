/**
 * The part of libsodium-wrappers-sumo that midgard-core calls. The package
 * ships no type declarations of its own.
 */
declare module "libsodium-wrappers-sumo" {
  type StateAddress = unknown;
  /** The Emscripten module the wrappers call: its heap and C exports. */
  type LibsodiumModule = {
    /** The wasm heap; replaced whenever the heap grows, so read it per use. */
    readonly HEAPU8: Uint8Array;
    _malloc(length: number): number;
    /** `crypto_generichash(out, outlen, in, inlen, key, keylen)`, with the
     * 64-bit `inlen` passed as its low and high 32-bit halves. */
    _crypto_generichash(
      output: number,
      outputLength: number,
      input: number,
      inputLengthLow: number,
      inputLengthHigh: number,
      key: number,
      keyLength: number,
    ): number;
  };
  const sodium: {
    readonly ready: Promise<void>;
    readonly libsodium: LibsodiumModule;
    crypto_generichash(
      hashLength: number,
      message: Uint8Array,
      key: null,
    ): Uint8Array;
    crypto_generichash_init(key: null, hashLength: number): StateAddress;
    crypto_generichash_update(state: StateAddress, chunk: Uint8Array): void;
    crypto_generichash_final(
      state: StateAddress,
      hashLength: number,
    ): Uint8Array;
    crypto_sign_verify_detached(
      signature: Uint8Array,
      message: Uint8Array,
      publicKey: Uint8Array,
    ): boolean;
  };
  export default sodium;
}
