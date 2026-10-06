import { readdirSync, readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { fileURLToPath } from "node:url";

import { configDefaults } from "vitest/config";

import { workspaceBundlePlugin } from "./workspace-bundle.js";
import { analyzeSuite } from "./workspace-bundle-analysis.js";

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
 * condition so a stale or missing dist can never shape a test result. That
 * guarantee is unchanged by `workspaceBundleProjects` below: the bundle it
 * loads is built from these same source files, never from a dist.
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
 * The worker flag that preloads `cml-memory-reserve.js`, which grows
 * cardano-multiplatform-lib's wasm memory once at start-up instead of a
 * page-run at a time. `isolatedForksPool` passes it to every worker; the
 * preload's note has the mechanism.
 */
const cmlMemoryReserveExecArgv = `--import=${
  new URL("./cml-memory-reserve.js", import.meta.url).href
}`;

/**
 * One fresh process per test FILE.
 *
 * The wasm evaluators (`@lucid-evolution/uplc`, cardano-multiplatform-lib)
 * allocate linear memory outside the V8 heap, and linear memory never shrinks:
 * whatever a file's heaviest journey needed stays resident for the life of the
 * worker. A fresh process per file hands that memory back when the file ends,
 * so one file's peak never becomes the next file's floor. (Through
 * `@lucid-evolution/uplc` 0.2.22 the same isolation was also what kept the
 * per-evaluation leak, fixed in lucid-evolution PR #728, from reaching the
 * ~4 GiB wasm32 ceiling and surfacing as `EvaluatorError: unreachable`.)
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
 *
 * Every worker also reserves CML's wasm memory up front. On Node 22 (V8 12.4)
 * each grow of a wasm memory past 64 MB starts a major GC, and CML grows its
 * memory a page-run at a time, so any file whose CML memory climbs far past
 * 64 MB pays one full mark-compact per grow. The reservation is a property of
 * the worker, not of any file: a file added later that grows CML gets it
 * without asking, and a file that never grows CML past 64 MB loses nothing
 * but untouched address space. Measured on Node 22.22.2, two forks pinned to
 * two cores: fault-proof heavy files 30-55% faster (the heaviest
 * value-conservation case 819 s -> 383 s, 4,624 -> 25 major GCs), a 30-file
 * fault-proof sample 1242 s -> 773 s, node emulator files 5% and watcher
 * files 1% faster. Fewer GCs also mean garbage CML wrappers are finalized
 * later, so peak worker RSS rises with the file's CML churn: 0.2-0.5 GB on
 * the fault-proof heavy files, about 1 GB on the node emulator files. If a run
 * dies on memory, lower `maxForks` as above.
 */
export const isolatedForksPool = ({ maxForks, heapMb = 4096 }) => ({
  pool: "forks",
  poolOptions: {
    forks: {
      isolate: true,
      singleFork: false,
      minForks: 1,
      maxForks,
      execArgv: [
        `--max-old-space-size=${String(heapMb)}`,
        cmlMemoryReserveExecArgv,
      ],
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

/**
 * The global-setup module that refuses a run against a stale blueprint; see
 * `blueprint-stamp-setup.js`. Every package whose suites read
 * `onchain/aiken/plutus.json` lists it in `globalSetup`.
 */
export const blueprintStampGlobalSetup = fileURLToPath(
  new URL("./blueprint-stamp-setup.js", import.meta.url),
);

const globToRegExp = (pattern) => {
  let source = "";
  const text = pattern.replace(/^\.\//u, "");
  for (let index = 0; index < text.length; index += 1) {
    const character = text[index];
    if ("?*+@!".includes(character) && text[index + 1] === "(") {
      // Extglob group, e.g. Vitest's default `?(c|m)`.
      const close = text.indexOf(")", index);
      const body = text
        .slice(index + 2, close)
        .split("|")
        .map((part) => part.replace(/[.+^$()|[\]\\]/gu, "\\$&"))
        .join("|");
      const quantifier = { "?": "?", "*": "*", "+": "+", "@": "", "!": "" }[
        character
      ];
      if (character === "!") throw new Error(`unsupported glob ${pattern}`);
      source += `(?:${body})${quantifier}`;
      index = close;
    } else if (character === "*" && text[index + 1] === "*") {
      const slash = text[index + 2] === "/";
      source += slash ? "(?:.*/)?" : ".*";
      index += slash ? 2 : 1;
    } else if (character === "[") {
      const close = text.indexOf("]", index);
      source += text.slice(index, close + 1);
      index = close;
    } else if (character === "*") source += "[^/]*";
    else if (character === "?") source += "[^/]";
    else if (character === "{") source += "(?:";
    else if (character === "}") source += ")";
    else if (character === ",") source += "|";
    else source += character.replace(/[.+^$()|[\]\\]/gu, "\\$&");
  }
  return new RegExp(`^${source}$`, "u");
};

const listTestFiles = (directory, include, exclude) => {
  const included = include.map(globToRegExp);
  const excluded = exclude.map(globToRegExp);
  const files = [];
  const walk = (current) => {
    for (const entry of readdirSync(current, { withFileTypes: true })) {
      if (entry.name === "node_modules" || entry.name.startsWith(".")) continue;
      const path = join(current, entry.name);
      if (entry.isDirectory()) walk(path);
      else {
        const name = relative(directory, path);
        if (
          included.some((pattern) => pattern.test(name)) &&
          !excluded.some((pattern) => pattern.test(name))
        )
          files.push(path);
      }
    }
  };
  walk(directory);
  return files.sort();
};

/**
 * Whether this run loads workspace packages from a per-run bundle. Off in
 * watch mode, where an edit to a bundled package must re-run against the
 * edited source, and whenever `MIDGARD_TEST_WORKSPACE_BUNDLE=0` asks for the
 * plain source-mode run (for example to bisect a suspected bundling
 * difference). The watch test mirrors Vitest's own default: `vitest run`, CI,
 * or a non-interactive stdin means a single run.
 */
const workspaceBundleEnabled = () => {
  const setting = process.env.MIDGARD_TEST_WORKSPACE_BUNDLE;
  if (setting === "0") return false;
  if (setting === "1") return true;
  const argv = process.argv.slice(2);
  if (
    argv[0] === "watch" ||
    argv[0] === "dev" ||
    argv.includes("--watch") ||
    argv.includes("-w")
  )
    return false;
  if (argv[0] === "run" || argv.includes("--run")) return true;
  return Boolean(process.env.CI) || !process.stdin.isTTY;
};

const workspaceBundleGuard = fileURLToPath(
  new URL("./workspace-bundle-guard.js", import.meta.url),
);

/**
 * Split one Vitest project into a workspace-bundle project and, when any file
 * needs it, a source project.
 *
 * Why: with one fresh process per test file (`isolatedForksPool`), every file
 * used to have Vite transform and evaluate the whole workspace graph it
 * reaches, module by module, over the worker RPC — 2,300–2,600 modules for a
 * file that touches the fault-proofs barrel, most of a file's start-up time.
 * Here the workspace code the suite reaches, this package's own `src/`
 * included, is bundled by esbuild ONCE per run and each fork imports the
 * bundle natively. Only the test files and this package's `tests/` directory
 * (the source region) still load module by module.
 *
 * What still holds: the bundle is built from current source (the
 * `midgard-source` targets), never from a dist, and it is keyed by the full
 * contents of every bundled package directory and the lockfile, so no edit can
 * be served a stale bundle (`workspace-bundle.js`). Dependency imports inside
 * the bundle are resolved by the same Vite resolver the source-mode run uses,
 * so bundled and unbundled code share one instance of every dependency.
 *
 * What a bundle changes, and how each is held:
 * - A `vi.mock` of anything outside the source region (a package, a builtin,
 *   or this package's `src/`), a `vi.spyOn` on a bundled module's namespace,
 *   and `vi.resetModules` no longer reach bundled code. Files using them
 *   (directly or through local helpers) are routed to the source project;
 *   `workspace-bundle-guard.js` fails any mock or reset the routing misses.
 * - A file that would load a bundled module from source as well (an entry
 *   that cannot be bundled because it reaches back into the source region)
 *   would see two instances of it. Those files are routed to the source
 *   project too, and the bundle plugin fails any load it misses.
 * - A dynamic `import()` of a specifier computed at run time (a path or URL)
 *   is left native in the bundle, so Node, not Vite, would load its target.
 *   Vitest passes this config's resolve conditions to every fork, so Node
 *   would resolve the target's workspace imports to `midgard-source`
 *   TypeScript and fail on it (an enum, or a `.js` specifier naming a `.ts`
 *   file). Modules that do this are not bundled, and the files that reach
 *   them run in the source project, where vite-node loads the target.
 * - Circular imports: bundled modules evaluate in esbuild's order, as native
 *   ESM does, but esbuild lowers top-level `const`, `let` and `class` to
 *   `var` when bundling. A module that reads a cycle partner's binding before
 *   that partner has run therefore sees `undefined`, not the TDZ
 *   ReferenceError native ESM raises, so a cycle that only worked under
 *   vite-node's partial exports can fail later and less clearly, or not at
 *   all if the value goes unused.
 * - Bundling renames colliding identifiers; `keepNames` keeps `.name`.
 * - Only Vite plugins of this project that transform workspace source would
 *   be skipped by the bundle, so never wrap a project that has one (the
 *   interactive-emulator projects rewrite a midgard-core module and stay
 *   source-mode).
 * - A `globalSetup` that imports a bundled package must be declared on the
 *   project passed here, not on the root config, so the analysis sees it and
 *   serves that import from the bundle too; otherwise the bundle plugin
 *   refuses its source load. Vitest runs a root `globalSetup` once more for
 *   every `extends: true` project, so a setup that must run once per run
 *   dedupes itself (see midgard-node's `tests/global-setup.ts`).
 *
 * The source project is named `<name>:source`; a `--project <name>` filter
 * must also name it (or use `--project '<name>*'`).
 */
export const workspaceBundleProjects = (project, { packageDirectory }) => {
  if (!workspaceBundleEnabled()) return [project];
  const test = project.test ?? {};
  const exclude = test.exclude ?? configDefaults.exclude;
  const analysis = analyzeSuite({
    packageDirectory,
    testFiles: listTestFiles(
      packageDirectory,
      test.include ?? configDefaults.include,
      exclude,
    ),
    setupRoots: [test.globalSetup ?? []]
      .flat()
      .filter((file) => file.startsWith("."))
      .map((file) => join(packageDirectory, file)),
  });
  if (analysis.entries.size === 0) return [project];
  const routed = [...analysis.sourceRouted.keys()].map(
    (file) => `./${relative(packageDirectory, file)}`,
  );
  if (process.env.MIDGARD_TEST_WORKSPACE_BUNDLE_REPORT === "1")
    for (const [file, reasons] of analysis.sourceRouted)
      console.log(
        `[workspace-bundle] source: ${relative(packageDirectory, file)} — ${reasons.join("; ")}`,
      );
  return [
    {
      ...project,
      plugins: [
        ...(project.plugins ?? []),
        workspaceBundlePlugin({ analysis }),
      ],
      test: {
        ...test,
        exclude: [...exclude, ...routed],
        setupFiles: [workspaceBundleGuard, ...(test.setupFiles ?? [])],
        env: {
          ...test.env,
          MIDGARD_WORKSPACE_BUNDLE_SOURCE_ROOT: analysis.sourceRoot,
        },
      },
    },
    ...(routed.length === 0
      ? []
      : [
          {
            ...project,
            test: { ...test, name: `${test.name}:source`, include: routed },
          },
        ]),
  ];
};

export {
  interactiveEmulatorBlueprint,
  interactiveEmulatorSetup,
  interactiveEmulatorPlugin,
} from "./interactive-emulator.js";
