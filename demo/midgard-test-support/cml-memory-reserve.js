/**
 * Worker preload (`--import`) that grows cardano-multiplatform-lib's wasm
 * linear memory once, up front, instead of a page-run at a time.
 *
 * Why it matters: CML's allocator grows its memory only by what each
 * allocation needs, so a busy file grows it thousands of times. On Node 22
 * (V8 12.4) every grow re-registers the WHOLE memory as new external memory:
 * the grown memory gets a fresh ArrayBuffer, which is accounted at its full
 * length, and the old one is unaccounted only once the sweeper reaches it.
 * V8 starts a major GC whenever external memory climbs 64 MB past where the
 * last mark-compact left it, so once CML's memory is past 64 MB every grow
 * costs a full mark-compact of the JS heap.
 *
 * Measured on the many-assets transition trace (Node 22.22.2, two pinned
 * cores, two forks): 2,409 CML grows and 685 major GCs, with the GC helper
 * threads burning 194 s of CPU beside the 157 s of the test thread. With the
 * reservation: one grow, 8 major GCs, 6 s of helper CPU.
 *
 * Allocating one large block and freeing it straight away grows the memory
 * once. The allocator keeps the freed block as its top chunk and serves later
 * allocations from it, so the file runs without further grows. Nothing reads
 * or writes the block, so its pages stay untouched address space until CML
 * actually uses them.
 *
 * Collecting: the grows were also the only thing that made V8 collect CML's
 * garbage. CML frees a value's wasm memory only from a FinalizationRegistry
 * callback, after a GC has found its JS wrapper dead, and a reserved worker
 * barely ever GCs. Its dead wrappers then pin their wasm memory, the
 * allocator keeps taking fresh pages from the top chunk, and a busy file
 * walks through the whole block. Past it, every grow is a full GC again, now
 * of a far bigger heap, inside a synchronous burst where finalizers cannot run
 * to free anything: on the installed-lifecycle transition trace (Node
 * 22.22.2, two pinned cores) that was 720 major GCs in one 84.7 s block of the
 * event loop, long enough for vitest's 60 s RPC timeout to fire on the test
 * updates sent during it, so the file's results were lost to `Timeout calling
 * "onTaskUpdate"`. So the preload restores the external-memory trigger it
 * removed, keyed to what CML actually touches: it runs a full GC whenever
 * resident memory outside the JS heap has climbed 64 MB (V8's own
 * external-memory step) past where the last such GC left it.
 * Wasm pages are never given back, so this fires on new high-water marks
 * only, not on reuse. The check runs between tasks, which is also when the
 * finalizers it unblocks get to run.
 *
 * Only memory layout and GC timing change: the same allocator serves the
 * same requests, a GC only ever reclaims what is already unreachable, and no
 * evaluation, encoding or ledger rule sees any difference.
 *
 * The block is the largest one the allocator accepts: a wasm32 allocation
 * must fit in an `isize`, so just under 2 GiB. The heaviest fault-proof file
 * peaks at about 1 GB; a file that needs more grows past the block the old
 * way. A host that cannot provide the block makes CML's allocator abort that
 * one allocation; the error is swallowed and the worker carries on with the
 * unreserved memory, so the reservation can only ever cost time, not a
 * result.
 */

import { setFlagsFromString } from "node:v8";
import { runInNewContext } from "node:vm";

const RESERVE_BYTES = 2 ** 31 - 65_536;

/** See "Collecting" in the module note. */
const COLLECT_STEP_BYTES = 64 * 2 ** 20;
const COLLECT_POLL_MS = 100;

/**
 * A full-GC function that stays private to this module: the flag is on only
 * while a fresh context is created, so `globalThis.gc` stays undefined for
 * tests that probe for it.
 */
const privateGc = () => {
  setFlagsFromString("--expose-gc");
  try {
    return runInNewContext("gc");
  } finally {
    setFlagsFromString("--no-expose-gc");
  }
};

/** Resident memory outside the JS heap: chiefly touched wasm pages. */
const residentOutsideHeap = () => {
  const { rss, heapTotal } = process.memoryUsage();
  return rss - heapTotal;
};

const collectOnResidentGrowth = () => {
  const collect = privateGc();
  let mark = residentOutsideHeap();
  setInterval(() => {
    if (residentOutsideHeap() < mark + COLLECT_STEP_BYTES) return;
    collect();
    mark = residentOutsideHeap();
  }, COLLECT_POLL_MS).unref();
};

/**
 * True for the wasm-bindgen instance behind
 * `@anastasia-labs/cardano-multiplatform-lib-nodejs`. The other wasm-bindgen
 * modules in the workspace (`@lucid-evolution/uplc`, message signing) do not
 * export CML's class finalizers.
 */
const isCmlInstance = (exports) =>
  exports.memory instanceof WebAssembly.Memory &&
  typeof exports.__wbindgen_malloc === "function" &&
  typeof exports.__wbindgen_free === "function" &&
  typeof exports.__wbg_plutusdata_free === "function";

const reserve = (exports) => {
  try {
    const pointer = exports.__wbindgen_malloc(RESERVE_BYTES, 1);
    exports.__wbindgen_free(pointer, RESERVE_BYTES, 1);
  } catch {
    // See the module note: a failed reservation leaves CML growing on demand.
    return;
  }
  collectOnResidentGrowth();
};

const OriginalInstance = WebAssembly.Instance;

/**
 * CML instantiates its module synchronously while `require` loads it, then
 * runs `__wbindgen_start`. The reservation waits for the next microtask so it
 * lands after that start-up, and the original constructor is put back as
 * soon as CML has been seen, so nothing else ever goes through the wrapper.
 */
WebAssembly.Instance = new Proxy(OriginalInstance, {
  construct(target, argumentsList, newTarget) {
    const instance = Reflect.construct(target, argumentsList, newTarget);
    if (isCmlInstance(instance.exports)) {
      WebAssembly.Instance = OriginalInstance;
      queueMicrotask(() => reserve(instance.exports));
    }
    return instance;
  },
});
