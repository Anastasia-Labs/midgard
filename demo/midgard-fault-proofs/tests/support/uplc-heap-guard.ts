import { createRequire } from "node:module";

import { afterEach } from "vitest";

/**
 * Fails a test file that is walking into the wasm32 address-space ceiling,
 * with a message that names the real cause.
 *
 * `@lucid-evolution/uplc` instantiates its wasm module once per Node process
 * and grows linear memory on every `eval_phase_two_raw` call without ever
 * reclaiming it — measured at roughly 1.5-3.7 MB per evaluation. Vitest
 * isolates per FILE, not per test, so a file containing many emulator
 * journeys accumulates that leak across all of them and eventually exhausts
 * the ~4 GiB wasm32 ceiling. Rust's allocator then aborts and the next script
 * evaluation surfaces to JavaScript as `EvaluatorError: unreachable`.
 *
 * That signature is indistinguishable at a glance from an on-chain validator
 * rejection, and it lands on whichever test happens to run last, at whichever
 * lifecycle stage happens to allocate first. It cost this program a confident
 * wrong diagnosis once already. Failing early and by name is the whole point
 * of this guard: the remedy is always to split the test file so each heavy
 * journey gets a fresh worker, never to change an assertion or a validator.
 */

const WARN_AT_MIB = 3_000;

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

afterEach(() => {
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
