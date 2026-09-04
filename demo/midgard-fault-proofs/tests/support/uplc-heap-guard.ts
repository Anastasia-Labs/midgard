import { createRequire } from "node:module";

import { afterEach } from "vitest";

/**
 * Two per-test memory duties for the wasm evaluators this suite drives, both
 * living outside the V8 heap that `--max-old-space-size` bounds.
 *
 * 1. Reclaim the CML arena after every test. `@lucid-evolution/lucid` never
 *    frees the cardano-multiplatform-lib objects it creates; they are released
 *    by wasm-bindgen's `FinalizationRegistry`, which only runs after a MAJOR
 *    garbage collection. An emulator journey allocates well over 100 MB of CML
 *    objects, and because the JavaScript heap itself stays small (~300 MB),
 *    V8 sees no reason to run a major collection between journeys — so the
 *    CML wasm memory, which can never shrink, grows by ~150 MB per journey
 *    (measured: 170 MB → 1,070 MB across seven journeys, ~2.2 GB by the
 *    twentieth). That growth is charged to V8 as EXTERNAL memory, and once it
 *    passes half of `--max-old-space-size` V8 runs a full compacting
 *    collection on nearly every external allocation. In
 *    execution-native-script-invalid-lifecycle.test.ts that turned the last
 *    journey from 4 s into 120-180 s of pure GC (115.5 s of a 119 s CPU
 *    profile). A forced collection caps the arena at ~190 MB for the same
 *    seven journeys and never lets external memory near the threshold — but
 *    only if it runs ~200 ms AFTER the test ends: the wrappers are still
 *    reachable from work that drains asynchronously (a collection forced
 *    synchronously in `afterEach`, or after a bare macrotask yield, reclaimed
 *    nothing; one after a 200 ms pause reclaimed everything). The pause is
 *    only paid by tests after which the wasm arenas stand higher than at the
 *    last reclaim: a test that touched no evaluator never grows them and
 *    costs nothing, and once an arena is big enough for a journey's peak the
 *    reclaim keeps it there (a 128 MiB tolerance was tried and let the arenas
 *    saw-tooth to 3× that size, because garbage left behind pushes the next
 *    peak past the high-water mark). `--expose-gc`
 *    is supplied by `isolatedForksPool`; when it is absent the reclaim is
 *    skipped rather than failing the suite.
 *
 * 2. Fail a test file that is walking into the wasm32 address-space ceiling,
 *    with a message that names the real cause. `@lucid-evolution/uplc`
 *    through 0.2.22 grows linear memory on every `eval_phase_two_raw` call
 *    and never reclaims it (its `wee_alloc` global allocator never reuses a
 *    freed block; fixed upstream by dropping the allocator, lucid-evolution
 *    PR #728). Vitest isolates per FILE, so a file of many emulator journeys
 *    accumulates that leak until Rust's allocator aborts and the next
 *    evaluation surfaces as `EvaluatorError: unreachable` — indistinguishable
 *    at a glance from an on-chain validator rejection, and landing on
 *    whichever test happens to run last. It cost this program a confident
 *    wrong diagnosis once already. The remedy is never to change an assertion
 *    or a validator; until the pin moves past 0.2.22 it is to split the file.
 */

const WARN_AT_MIB = 3_000;
const RECLAIM_AFTER_MIB = 1;
const RECLAIM_SETTLE_MS = 200;
const MIB = 1024 * 1024;

/**
 * Every wasm instance this process creates, so the reclaim decision can read
 * the total linear memory (CML's glue does not export its instance, and a
 * setup file runs before any of the evaluators are loaded).
 */
type WasmInstanceLike = { readonly exports: Readonly<Record<string, unknown>> };
type WasmInstanceConstructor = new (
  module: unknown,
  imports?: unknown,
) => WasmInstanceLike;
// The compiler options here carry no WebAssembly lib typings, so the namespace
// is reached structurally.
const wasmNamespace = (
  globalThis as unknown as {
    readonly WebAssembly: { Instance: WasmInstanceConstructor };
  }
).WebAssembly;
const wasmInstances: WasmInstanceLike[] = [];
const OriginalInstance = wasmNamespace.Instance;
const RecordingInstance = function (
  this: unknown,
  module: unknown,
  imports?: unknown,
): WasmInstanceLike {
  const instance = new OriginalInstance(module, imports);
  wasmInstances.push(instance);
  return instance;
} as unknown as WasmInstanceConstructor;
(RecordingInstance as { prototype: unknown }).prototype = (
  OriginalInstance as { prototype: unknown }
).prototype;
wasmNamespace.Instance = RecordingInstance;

const linearMemoryBytes = (memory: unknown): number => {
  if (typeof memory !== "object" || memory === null || !("buffer" in memory)) {
    return 0;
  }
  const buffer: unknown = memory.buffer;
  return buffer instanceof ArrayBuffer ? buffer.byteLength : 0;
};

const totalWasmMib = (): number =>
  wasmInstances.reduce(
    (total, instance) =>
      total + linearMemoryBytes(instance.exports["memory"]) / MIB,
    0,
  );

let reclaimedAtMib = 0;

const reclaimWasmArenas = async (): Promise<void> => {
  const gc = (globalThis as { gc?: () => void }).gc;
  if (gc === undefined || totalWasmMib() - reclaimedAtMib < RECLAIM_AFTER_MIB) {
    return;
  }
  await new Promise<void>((resolve) => setTimeout(resolve, RECLAIM_SETTLE_MS));
  gc();
  await new Promise<void>((resolve) => setImmediate(resolve));
  // Linear memory never shrinks, so this records the new high-water mark;
  // the next reclaim fires only once that much has been allocated on top.
  reclaimedAtMib = totalWasmMib();
};

const readUplcHeapMib = (): number | null => {
  try {
    const require = createRequire(import.meta.url);
    const uplc = require("@lucid-evolution/uplc") as {
      readonly __wasm?: {
        readonly memory?: { readonly buffer: { readonly byteLength: number } };
      };
    };
    const memory = uplc.__wasm?.memory;
    if (memory === undefined) {
      return null;
    }
    return memory.buffer.byteLength / (1024 * 1024);
  } catch {
    // The guard must never be the reason a suite fails to run.
    return null;
  }
};

afterEach(async () => {
  // Duty 1: release the finalizer-managed CML objects this test left behind.
  await reclaimWasmArenas();
  // Duty 2: the uplc wasm32 ceiling guard.
  const mib = readUplcHeapMib();
  if (mib !== null && mib >= WARN_AT_MIB) {
    throw new Error(
      `uplc wasm heap is at ${mib.toFixed(0)} MiB of the ~4096 MiB wasm32 ceiling. ` +
        `@lucid-evolution/uplc leaks linear memory per script evaluation and never ` +
        `reclaims it, so the next evaluation in this worker is liable to trap as ` +
        `"EvaluatorError: unreachable" — a WebAssembly abort, not a validator ` +
        `rejection. Split this test file so each heavy emulator journey runs in its ` +
        `own worker; do not weaken an assertion to make the trap go away.`,
    );
  }
});
