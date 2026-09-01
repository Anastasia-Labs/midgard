import { createRequire } from "node:module";
import { parentPort } from "node:worker_threads";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

export type MpfRootBuilderRequest = {
  readonly id: number;
  readonly domain: SDK.RootDomain;
  readonly counted: boolean;
  readonly keys: ArrayBuffer;
  readonly values: ArrayBuffer;
  readonly offsets: ArrayBuffer;
};

export type MpfRootBuilderResponse =
  | {
      readonly id: number;
      readonly rootHex: string;
      readonly phasRoot: string;
      readonly count: bigint;
      readonly timings: {
        readonly unpackMs: number;
        readonly canonicalizeMs: number;
        readonly trieFromListMs: number;
        readonly domainCommitMs: number;
      };
    }
  | { readonly id: number; readonly error: string };

if (parentPort === null) {
  throw new Error("mpf-root-builder must run inside a worker thread");
}

const require = createRequire(import.meta.url);
const rootRuntime = new Promise<{
  readonly mpf: typeof import("./utils/mpf.js");
  readonly forestry: typeof import("@aiken-lang/merkle-patricia-forestry");
}>((resolve, reject) => {
  const blake2b = require("blake2b") as {
    readonly ready: (callback: (error?: unknown) => void) => void;
  };
  blake2b.ready((error?: unknown) => {
    if (error !== undefined) {
      reject(
        error instanceof Error
          ? error
          : new Error("blake2b initialization failed", { cause: error }),
      );
      return;
    }
    // blake2b@2.1.4 swaps its CommonJS export to blake2b-wasm in a
    // readiness callback. Import forestry only after that export is live so
    // its ESM default binding cannot capture the pure-JS bootstrap function.
    setImmediate(() => {
      void Promise.all([
        import("./utils/mpf.js"),
        import("@aiken-lang/merkle-patricia-forestry"),
      ]).then(([mpf, forestry]) => resolve({ mpf, forestry }), reject);
    });
  });
});

parentPort.on("message", (request: MpfRootBuilderRequest) => {
  void Effect.runPromise(
    Effect.gen(function* () {
      const { mpf, forestry } = yield* Effect.promise(() => rootRuntime);
      const unpackStartedAt = performance.now();
      const keyArena = Buffer.from(request.keys);
      const valueArena = Buffer.from(request.values);
      const offsets = new Uint32Array(request.offsets);
      const entries = Array.from({ length: offsets.length / 4 }, (_, index) => {
        const offset = index * 4;
        const keyOffset = offsets[offset]!;
        const keyLength = offsets[offset + 1]!;
        const valueOffset = offsets[offset + 2]!;
        const valueLength = offsets[offset + 3]!;
        return {
          key: keyArena.subarray(keyOffset, keyOffset + keyLength),
          value: valueArena.subarray(valueOffset, valueOffset + valueLength),
        };
      });
      const unpackMs = performance.now() - unpackStartedAt;
      const canonicalizeStartedAt = performance.now();
      const canonical = yield* mpf.canonicalizeKeyValuePhasEntries(
        entries.map(({ key }) => key),
        entries.map(({ value }) => value),
      );
      const canonicalizeMs = performance.now() - canonicalizeStartedAt;
      const trieStartedAt = performance.now();
      const phasRoot =
        canonical.length === 0
          ? SDK.EMPTY_MERKLE_TREE_ROOT
          : Buffer.from(
              (yield* Effect.tryPromise({
                try: () => forestry.Trie.fromList(canonical),
                catch: (cause) => mpf.MpfError.phasRoot(cause),
              })).hash,
            ).toString("hex");
      const trieFromListMs = performance.now() - trieStartedAt;
      const domainCommitStartedAt = performance.now();
      const rootHex = request.counted
        ? yield* SDK.commitCountedRootProgram({
            domain: request.domain,
            phasRoot,
            count: BigInt(canonical.length),
          })
        : phasRoot;
      const domainCommitMs = performance.now() - domainCommitStartedAt;
      return {
        rootHex,
        phasRoot,
        count: BigInt(canonical.length),
        timings: {
          unpackMs,
          canonicalizeMs,
          trieFromListMs,
          domainCommitMs,
        },
      };
    }),
  ).then(
    (result) =>
      parentPort!.postMessage({
        id: request.id,
        ...result,
      } satisfies MpfRootBuilderResponse),
    (error: unknown) =>
      parentPort!.postMessage({
        id: request.id,
        error: error instanceof Error ? error.message : String(error),
      } satisfies MpfRootBuilderResponse),
  );
});
