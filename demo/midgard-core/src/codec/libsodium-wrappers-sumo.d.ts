/**
 * The part of libsodium-wrappers-sumo that midgard-core calls. The package
 * ships no type declarations of its own.
 */
declare module "libsodium-wrappers-sumo" {
  type StateAddress = unknown;
  const sodium: {
    readonly ready: Promise<void>;
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
  };
  export default sodium;
}
