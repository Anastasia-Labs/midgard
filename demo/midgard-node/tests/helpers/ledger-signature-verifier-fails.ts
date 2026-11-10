/**
 * Imported first by a test file, before anything loads
 * `@al-ft/midgard-core`: libsodium's initialisation fails in this process,
 * as it does when its WebAssembly cannot instantiate. The ledger signature
 * verifier reads `sodium.ready` when its module loads
 * (`ed25519-ledger-signature.ts`) and shares this file's libsodium instance,
 * so it sees the failure; `libsodiumReadyReads` counts the reads, so a test
 * can tell the failure reached it. Validation workers are threads with their
 * own libsodium, untouched. Nothing of ours is replaced.
 */
import sodium from "libsodium-wrappers-sumo";

export const INJECTED_LIBSODIUM_FAILURE =
  "injected libsodium initialisation failure";

let reads = 0;
const failed = Promise.reject(new Error(INJECTED_LIBSODIUM_FAILURE));
failed.catch(() => undefined);
Object.defineProperty(sodium, "ready", {
  configurable: true,
  get: () => {
    reads += 1;
    return failed;
  },
});

/** How many times this process read libsodium's initialisation. */
export const libsodiumReadyReads = (): number => reads;
