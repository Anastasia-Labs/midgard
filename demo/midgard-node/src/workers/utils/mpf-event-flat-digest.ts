import blake2b from "blake2b";

const HASH_BYTES = 32;
export const EVENT_FLAT_DIGEST_UPDATE_CHUNK_BYTES = 16 * 1024 * 1024;

type Blake2bState = {
  update(value: Uint8Array): Blake2bState;
  digest(): Uint8Array;
};

export type EventFlatBlake2b = {
  (outputBytes: number): Blake2bState;
  readonly WASM_SUPPORTED: boolean;
  readonly WASM_LOADED: boolean;
  readonly ready: (callback: () => void) => void;
};

export type EventFlatDigestState = {
  update(value: Uint8Array): EventFlatDigestState;
  digest(): Buffer;
};

export const createEventFlatDigestAdapter = (
  implementation: EventFlatBlake2b,
) => {
  let digestReady = false;
  let digestFailure: Error | undefined;
  const readiness = new Promise<void>((resolve, reject) => {
    if (implementation.WASM_SUPPORTED !== true) {
      digestFailure = new Error(
        "Event-flat BLAKE2b requires WebAssembly support",
      );
      reject(digestFailure);
      return;
    }
    implementation.ready(() => {
      if (implementation.WASM_LOADED !== true) {
        digestFailure = new Error(
          "Event-flat BLAKE2b WebAssembly failed to initialize",
        );
        reject(digestFailure);
        return;
      }
      digestReady = true;
      resolve();
    });
  });

  // Avoid an unhandled rejection while retaining an explicit fail-closed
  // error for the first caller that awaits or uses the sync adapter.
  void readiness.catch(() => undefined);

  const assertReady = (): void => {
    if (!digestReady) {
      throw (
        digestFailure ??
        new Error(
          "Event-flat BLAKE2b is not ready; await prepareEventFlatDigest()",
        )
      );
    }
  };

  const createDigest = (): EventFlatDigestState => {
    assertReady();
    const state = implementation(HASH_BYTES);
    const adapter: EventFlatDigestState = {
      update(value) {
        for (
          let offset = 0;
          offset < value.length;
          offset += EVENT_FLAT_DIGEST_UPDATE_CHUNK_BYTES
        ) {
          state.update(
            value.subarray(
              offset,
              Math.min(
                value.length,
                offset + EVENT_FLAT_DIGEST_UPDATE_CHUNK_BYTES,
              ),
            ),
          );
        }
        return adapter;
      },
      digest() {
        return Buffer.from(state.digest());
      },
    };
    return adapter;
  };

  return {
    prepare: async (): Promise<void> => readiness,
    digest: (value: Uint8Array): Buffer =>
      createDigest().update(value).digest(),
    createDigest,
    isReady: (): boolean => digestReady,
  };
};

const defaultAdapter = createEventFlatDigestAdapter(
  blake2b as EventFlatBlake2b,
);

export const prepareEventFlatDigest = defaultAdapter.prepare;
export const eventFlatDigest = defaultAdapter.digest;
export const createEventFlatDigest = defaultAdapter.createDigest;
export const eventFlatDigestIsReady = defaultAdapter.isReady;
