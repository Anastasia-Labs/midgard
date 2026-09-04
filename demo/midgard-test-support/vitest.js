import { readFileSync } from "node:fs";

/**
 * Config-time pieces shared by every package's `vitest.config.ts`.
 *
 * Plain JavaScript on purpose. A `vitest.config.ts` is read by Vite before any
 * workspace resolution condition is in play, so this module has to be loadable
 * with nothing built and no TypeScript step in the way; `vitest.d.ts` beside it
 * carries the types. Everything else this package exports is ordinary
 * TypeScript resolved through the `midgard-source` condition like any other
 * workspace import.
 *
 * Only settings that are the *same claim* in every consumer belong here. A
 * package's own reason for a fork count, a timeout, or an include glob stays in
 * that package's config, next to the suite it describes.
 */

/**
 * Resolve workspace packages from source via the `midgard-source` exports
 * condition so a stale or missing dist can never shape a test result.
 *
 * Vitest resolves test modules through Vite's SSR pipeline and sets
 * `ssr.resolve.conditions` itself, so the root `resolve.conditions` is not
 * consulted; the trailing entries restate Vitest's server defaults, which
 * assigning this key would otherwise drop.
 *
 * Every package needs this and every package needs it spelled the same way, so
 * it is defined once here rather than copied into nine configs. It is a
 * function so that each config gets its own object to hand to Vite.
 */
export const midgardSourceSsr = () => ({
  resolve: {
    conditions: ["midgard-source", "node", "development|production"],
  },
});

/**
 * One fresh process per test FILE, which is a correctness requirement rather
 * than a performance knob.
 *
 * `@lucid-evolution/uplc` through 0.2.22 grows wasm linear memory on every
 * evaluation and never reclaims it (fixed upstream in lucid-evolution PR #728
 * by dropping its `wee_alloc` allocator), so a long-lived worker eventually
 * exhausts the ~4 GiB wasm32 ceiling and the next evaluation surfaces as
 * `EvaluatorError: unreachable` — a WebAssembly abort wearing a validator
 * rejection's clothes. That is why `isolate` must stay `true` while that pin
 * stands: turning it off re-shares one heap across every emulator journey in
 * the run.
 *
 * `--expose-gc` is here because of a second, still-live allocator behaviour:
 * lucid-evolution leaves every cardano-multiplatform-lib object to
 * wasm-bindgen's `FinalizationRegistry`, which only fires after a MAJOR
 * collection. The JavaScript heap of an emulator suite stays small, so V8
 * rarely schedules one, and the CML wasm arena — external memory that can
 * never shrink — grows by ~150 MB per emulator journey until it passes half of
 * `heapMb`, after which V8 runs a full compacting collection on nearly every
 * external allocation (measured as a 4 s test taking 120-180 s). A suite that
 * runs many journeys per file calls `globalThis.gc()` after each test; see
 * midgard-fault-proofs/tests/support/uplc-heap-guard.ts for the numbers.
 *
 * `maxForks` is the separate, purely-scheduling bound, and is the caller's to
 * justify — the packages that use this differ in why they pick their ceiling.
 * If a run dies on memory, LOWER it: each fork carries its own wasm arenas,
 * and raising `heapMb` only moves the wall, because the wasm evaluators
 * allocate outside the V8 heap this bounds.
 *
 * The heap bound is set here rather than through a blanket `NODE_OPTIONS` from
 * the lane runner, which would also hit pnpm, Vitest's own main process, and
 * every unrelated tool in the lane.
 */
export const isolatedForksPool = ({ maxForks, heapMb = 4096 }) => ({
  pool: "forks",
  poolOptions: {
    forks: {
      isolate: true,
      singleFork: false,
      minForks: 1,
      maxForks,
      execArgv: [`--max-old-space-size=${String(heapMb)}`, "--expose-gc"],
    },
  },
});

/**
 * Serves `.sql` files to test code as default-exported strings, so a suite can
 * assert against the same migration text the runtime executes instead of a
 * transcription of it.
 */
export const rawSqlLoaderPlugin = () => ({
  name: "raw-sql-loader",
  load(id) {
    if (!id.endsWith(".sql")) {
      return null;
    }
    return `export default ${JSON.stringify(readFileSync(id, "utf8"))};`;
  },
});
