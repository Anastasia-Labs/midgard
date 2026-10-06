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
 * actually uses them; resident memory grows only by what CML would have used
 * anyway, plus whatever the now-rarer GCs leave uncollected for longer.
 *
 * Only memory layout changes: the same allocator serves the same requests,
 * and no evaluation, encoding or ledger rule sees any difference.
 *
 * The block is the largest one the allocator accepts: a wasm32 allocation
 * must fit in an `isize`, so just under 2 GiB. The heaviest fault-proof file
 * peaks at about 1 GB; a file that needs more grows past the block the old
 * way. A host that cannot provide the block makes CML's allocator abort that
 * one allocation; the error is swallowed and the worker carries on with the
 * unreserved memory, so the reservation can only ever cost time, not a
 * result.
 */

const RESERVE_BYTES = 2 ** 31 - 65_536;

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
  }
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
