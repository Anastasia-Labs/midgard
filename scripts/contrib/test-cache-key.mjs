import { packageBuildTask } from "../../demo/scripts/package-tasks.mjs";
import {
  existsSync,
  readdirSync,
  readFileSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

import {
  buildOutputIdentity,
  MODULE_SPECIFIER,
  packageClosure,
  sha256,
  workspacePackages,
} from "./files.mjs";
import {
  probeGraph,
  reachedTests,
  scriptTexts,
  wholePackageReasons,
} from "./reached.mjs";
import {
  byPath,
  changedSince,
  loadSnapshot,
  readJson,
  repositorySnapshot,
  store,
  storeSnapshot,
  TEST_CACHE_SCHEMA,
} from "./test-cache-store.mjs";

// What a recorded pass of `contrib test --cached` (test-cache.mjs) is bound
// to besides repository files, which test files never reuse one, and the
// package's import graph, cached per content.

const SCRIPT = /\.[cm]?[jt]sx?$/u;
const VITEST_CONFIG = /^vitest(?:\.[\w-]+)?\.config\.[cm]?[jt]s$/u;
const PROBE_SOURCES = ["reached-probe.mjs"].map((name) =>
  fileURLToPath(new URL(name, import.meta.url)),
);

// --- the run key -------------------------------------------------------------

// Every variable can change a result: Node, Vitest and the repository read
// their own, and a dependency, native code or a spawned binary reads others
// (LD_PRELOAD, OPENSSL_CONF, SSL_CERT_DIR, proxies, ...). So all of them are
// bound except these, which name a session or a process rather than
// anything a test can see the effect of, and contrib's own per-invocation
// names (a disposable database family, a per-run evidence path).
const UNKEYED = new Set([
  "_",
  "OLDPWD",
  "PWD",
  "SHLVL",
  "INVOCATION_ID",
  "JOURNAL_STREAM",
  "MANAGERPID",
  "SYSTEMD_EXEC_PID",
  "TERM_SESSION_ID",
  "ITERM_SESSION_ID",
  "WINDOWID",
  "XDG_SESSION_ID",
  "TMUX",
  "TMUX_PANE",
  "STY",
  "WT_SESSION",
  "MIDGARD_TEST_DATABASE_PREFIX",
  "MIDGARD_PUBLISHED_WORKFLOW_RECEIPT_PATH",
]);
const UNKEYED_PREFIXES = ["SSH_"];

/**
 * The environment a suite runs with, by name with each value's digest (no
 * value is ever written down).
 */
export const keyedEnvironment = (env) =>
  Object.fromEntries(
    Object.entries(env)
      .filter(
        ([name, value]) =>
          value !== undefined &&
          !UNKEYED.has(name) &&
          !UNKEYED_PREFIXES.some((prefix) => name.startsWith(prefix)),
      )
      .map(([name, value]) => [name, sha256(String(value))])
      .sort(byPath),
  );

// The runner's own code decides what runs, what a pass is and what is
// recorded: contrib's modules (not its tests) and the task declarations.
export const runnerSources = () => {
  const contrib = fileURLToPath(new URL(".", import.meta.url));
  return [
    ...readdirSync(contrib)
      .filter((name) => name.endsWith(".mjs") && !/\.test[.-]/u.test(name))
      .map((name) => resolve(contrib, name)),
    resolve(contrib, "../contrib.mjs"),
    resolve(contrib, "../../demo/scripts/package-tasks.mjs"),
  ].sort();
};

/** The digest of the runner's own sources, by file name. */
export const runnerIdentity = (paths = runnerSources()) =>
  sha256(
    JSON.stringify(
      paths.map((path) => [
        path.split(sep).slice(-2).join("/"),
        sha256(readFileSync(path)),
      ]),
    ),
  );

const installedPackages = (root) => {
  const path = resolve(root, "demo/node_modules/.pnpm/lock.yaml");
  return existsSync(path) ? sha256(readFileSync(path)) : null;
};

/**
 * Everything a test file's result depends on besides repository files,
 * and its digest: `{ sha256, parts }`.
 */
export const runIdentity = ({
  root,
  pkg,
  command,
  tier,
  seed,
  sourceOnly,
  env,
  artifacts = {},
  postgres,
  runner = runnerIdentity(),
}) => {
  const parts = {
    schema: TEST_CACHE_SCHEMA,
    runner,
    package: pkg.name,
    directory: pkg.directory,
    command: {
      env: command.env,
      config: command.config ?? null,
      exclude: command.exclude,
      maxWorkers: command.maxWorkers ?? null,
      disableConsoleIntercept: command.disableConsoleIntercept,
    },
    tier: tier ?? "all",
    seed,
    sourceOnly,
    environment: keyedEnvironment(env),
    node: [process.version, process.platform, process.arch],
    installed: installedPackages(root),
    // The watcher's durable-store guard refuses temporary directories.
    temporaryCheckout: root.startsWith(`${tmpdir()}${sep}`),
    dists: packageClosure(root, pkg.name)
      .filter(packageBuildTask)
      .map((entry) => [entry.name, buildOutputIdentity(root, entry).sha256])
      .sort(byPath),
    artifacts,
    postgres: postgres ?? null,
  };
  return { sha256: sha256(JSON.stringify(parts)), parts };
};

// --- what never reuses -------------------------------------------------------

// Random inputs drawn afresh each run: a pass covered other cases than the
// next run would. Only unseeded sources count; a generator seeded by the
// test (the shared `plutus-data-fuzz` helper's `makeFuzzRng(seed)`) draws
// the same cases every run, and none of these calls appears in it.
const RANDOM_INPUTS =
  /["']fast-check["']|\bMath\.random\s*\(|\brandom(?:Int|Bytes|UUID|Fill(?:Sync)?)\s*\(|\bgetRandomValues\s*\(|\bgenerate(?:PrivateKey|SeedPhrase)\s*\(/u;
// A test option retrying a failure: the report does not mark such a pass.
const TEST_RETRY = /\b(?:retry|repeats)\s*:\s*\d/u;
// A suite that decides at run time whether it runs, from the environment or
// what the machine has (a devnet, Docker, a binary, a database): when it
// runs, it tests that outside state, which no key holds. Also a value a
// global setup provides (`inject`), which can come from outside the
// checkout (a toolchain it builds with).
const GATED =
  /\b(?:skipIf|runIf)\s*\(|\.skip\s*\(\s*(?:\)|[^\s"'`])|\binject\s*\(/u;
const SHARED_VITEST = "demo/midgard-test-support/vitest.js";
const TEST_SUPPORT = "demo/midgard-test-support";

const resolveScript = (target) => {
  const stem = target.replace(/\.[cm]?[jt]sx?$/u, "");
  return [
    target,
    `${stem}.ts`,
    `${stem}.tsx`,
    `${stem}.mts`,
    `${stem}.cts`,
    `${stem}.js`,
    `${stem}.mjs`,
    `${stem}.cjs`,
    resolve(target, "index.ts"),
    resolve(target, "index.js"),
  ].find((path) => existsSync(path) && statSync(path).isFile());
};

// Every relative target in a package.json `exports`/`imports` entry, under
// any condition.
const targetsOf = (entry) =>
  typeof entry === "string"
    ? [entry]
    : entry && typeof entry === "object"
      ? Object.values(entry).flatMap(targetsOf)
      : [];

// The entry of a subpath map matching `key`, exactly or by its `*` pattern.
const mapped = (map, key) => {
  if (!map || typeof map !== "object") return [];
  if (key in map) return targetsOf(map[key]);
  for (const [pattern, entry] of Object.entries(map)) {
    const star = pattern.indexOf("*");
    if (
      star >= 0 &&
      key.startsWith(pattern.slice(0, star)) &&
      key.endsWith(pattern.slice(star + 1))
    ) {
      const part = key.slice(star, key.length - (pattern.length - star - 1));
      return targetsOf(entry).map((target) => target.replaceAll("*", part));
    }
  }
  return [];
};

/**
 * The files an import of `specifier` from `from` can load inside the
 * checkout: a relative path, a `#` alias of the importer's package, or a
 * workspace package (through its `exports`, every condition).
 */
const importedFiles = (root, packages, from, specifier) => {
  if (specifier.startsWith(".")) {
    const found = resolveScript(resolve(dirname(from), specifier));
    return found ? [found] : [];
  }
  const owner = packages.find((pkg) =>
    from.startsWith(`${resolve(root, pkg.directory)}${sep}`),
  );
  let pkg;
  let targets;
  if (specifier.startsWith("#")) {
    pkg = owner;
    targets = mapped(pkg?.imports, specifier);
  } else {
    pkg = packages.find(
      (entry) =>
        specifier === entry.name || specifier.startsWith(`${entry.name}/`),
    );
    if (!pkg) return [];
    const subpath = `.${specifier.slice(pkg.name.length)}`;
    targets = pkg.exports
      ? mapped(
          typeof pkg.exports === "string" ||
            !Object.keys(pkg.exports).some((key) => key.startsWith("."))
            ? { ".": pkg.exports }
            : pkg.exports,
          subpath,
        )
      : [subpath];
  }
  if (!pkg) return [];
  const base = resolve(root, pkg.directory);
  return targets.flatMap((target) => {
    if (!target.startsWith("./")) return []; // another package's export
    const found = resolveScript(resolve(base, target));
    return found ? [found] : [];
  });
};

/**
 * Whether a file is test-side code, where a test draws its inputs and
 * decides whether it runs: anything in the checkout but a package's code
 * under test (src/, its dist/ and vendor/) and installed modules. The shared
 * test-support package is test-side throughout.
 */
const testSide = (root, packages, path) => {
  const name = relative(root, path).split(sep).join("/");
  if (name.startsWith("../") || name.split("/").includes("node_modules"))
    return false;
  if (name.startsWith(`${TEST_SUPPORT}/`)) return true;
  const owner = packages.find((pkg) => name.startsWith(`${pkg.directory}/`));
  return (
    !owner ||
    !["src", "dist", "vendor"].some((directory) =>
      name.startsWith(`${owner.directory}/${directory}/`),
    )
  );
};

/**
 * Why a test file of `pkg` must always run, or undefined. `evidence` names
 * files whose run writes evidence the receipt binds. The file and the
 * test-side modules it imports (by relative path, `#` alias or workspace
 * package, transitively) are read for random inputs, retries and run-time
 * gates.
 */
export const uncacheableReason = (root, pkg, file, { evidence = [] } = {}) => {
  if (evidence.includes(file))
    return "it writes evidence the run's receipt binds";
  const packages = workspacePackages(root);
  const base = resolve(root, pkg.directory);
  const seen = new Set();
  const queue = [resolve(base, file)];
  while (queue.length) {
    const path = queue.pop();
    if (seen.has(path)) continue;
    seen.add(path);
    const text = readFileSync(path, "utf8");
    const where = relative(root, path);
    if (RANDOM_INPUTS.test(text))
      return `${where} draws unseeded random inputs (fast-check, Math.random, node:crypto random*, getRandomValues, generated keys or seed phrases), so each run covers other cases`;
    if (TEST_RETRY.test(text))
      return `${where} retries or repeats tests, and the report does not mark a pass that took a retry`;
    if (GATED.test(text))
      return `${where} decides at run time whether a suite runs (skipIf, runIf, a dynamic skip) or reads a global setup's value (inject); when it runs it tests state outside the checkout`;
    for (const [, specifier] of text.matchAll(MODULE_SPECIFIER))
      for (const next of importedFiles(root, packages, path, specifier))
        if (testSide(root, packages, next)) queue.push(next);
  }
  return undefined;
};

/**
 * Why a package's runs cannot record passes at all, or undefined: Vitest
 * retries failed tests where a config or the shared harness says so (its
 * report does not mark a pass that took a retry), or runs files without
 * isolation, where a file's pass can rest on what another file left behind
 * in the same worker.
 */
export const unrecordableReason = (root, pkg) => {
  const base = resolve(root, pkg.directory);
  const configs = [
    ...readdirSync(base)
      .filter((entry) => VITEST_CONFIG.test(entry))
      .map((name) => resolve(base, name)),
    resolve(root, SHARED_VITEST),
  ].filter((path) => existsSync(path));
  for (const path of configs) {
    const text = readFileSync(path, "utf8");
    if (/\bretry\s*:/u.test(text))
      return `${relative(root, path)} retries failed tests, and the report does not mark a pass that took a retry`;
    if (/\bisolate\s*:\s*false\b|--no-isolate\b/u.test(text))
      return `${relative(root, path)} runs files without isolation, so a pass can rest on another file's leftovers`;
  }
  if (
    /--no-isolate\b|--isolate[= ]false\b/u.test(
      JSON.stringify(pkg.scripts ?? {}),
    )
  )
    return `${pkg.name}'s scripts run files without isolation, so a pass can rest on another file's leftovers`;
  return undefined;
};

// --- the import graph, cached -------------------------------------------------

/**
 * The files every package's Vitest config reads to build its projects: the
 * configs, what they import by relative path, and every script of a
 * workspace package they import. A change there can change how any module
 * resolves.
 */
const configInputs = (root) => {
  const packages = workspacePackages(root);
  const files = new Set();
  const queue = packages.flatMap((pkg) =>
    readdirSync(resolve(root, pkg.directory))
      .filter((name) => VITEST_CONFIG.test(name))
      .map((name) => resolve(root, pkg.directory, name)),
  );
  const scriptsOf = (pkg) =>
    scriptTexts(root)
      .map(({ path }) => path)
      .filter((path) => path.startsWith(`${pkg.directory}/`))
      .map((path) => resolve(root, path));
  const imported = new Set();
  while (queue.length) {
    const path = queue.pop();
    const name = relative(root, path).split(sep).join("/");
    if (files.has(name) || !existsSync(path)) continue;
    files.add(name);
    for (const [, specifier] of readFileSync(path, "utf8").matchAll(
      MODULE_SPECIFIER,
    )) {
      if (specifier.startsWith(".")) {
        const next = resolveScript(resolve(dirname(path), specifier));
        if (next) queue.push(next);
        continue;
      }
      const pkg = packages.find(
        (entry) =>
          specifier === entry.name || specifier.startsWith(`${entry.name}/`),
      );
      if (pkg && !imported.has(pkg.name)) {
        imported.add(pkg.name);
        queue.push(...scriptsOf(pkg));
      }
    }
  }
  return files;
};

/**
 * The changed scripts an incremental probe may regraph alone, or undefined
 * when the change needs a full probe: a file added or removed, a changed
 * non-script, package or workspace configuration, the shared harness, or
 * anything a Vitest config reads. Otherwise every other module resolves its
 * imports exactly as before, so its recorded edges and spellings stand.
 */
export const incrementalChanges = (root, before, after) => {
  const paths = Object.keys(before.files);
  if (
    paths.length !== Object.keys(after.files).length ||
    paths.some((path) => !(path in after.files))
  )
    return undefined;
  const changed = changedSince(before, after);
  if (!changed.every((path) => SCRIPT.test(path))) return undefined;
  const packages = workspacePackages(root);
  if (
    changed.some((path) =>
      packages.some((pkg) => wholePackageReasons(root, pkg, [path]).length),
    )
  )
    return undefined;
  const config = configInputs(root);
  if (changed.some((path) => config.has(path))) return undefined;
  return changed;
};

// Graphs hold absolute paths; stored relative to the checkout, so another
// worktree with the same files reads the same graph.
const rebase = (graph, from, to) => {
  const prefix = `${from}${sep}`;
  const move = (path) =>
    path.startsWith(prefix) ? `${to}${sep}${path.slice(prefix.length)}` : path;
  const keys = (object, value = (entry) => entry) =>
    Object.fromEntries(
      Object.entries(object ?? {}).map(([key, entry]) => [
        move(key),
        value(entry),
      ]),
    );
  return {
    tests: graph.tests.map(move),
    edges: keys(graph.edges, (list) => list.map(move)),
    failed: keys(graph.failed),
    spelled: keys(graph.spelled),
  };
};
const RELATIVE = "@checkout";

/**
 * The package's import graph for the checkout's current files, probed once
 * per distinct content: stored by its probe inputs and snapshot, regraphed
 * incrementally from the last graph when only scripts outside every
 * configuration changed, else probed in full. `get()` probes lazily, once.
 */
export const cachedGraph = ({
  directory,
  root,
  pkg,
  command,
  env,
  snapshot,
  // The snapshot's extra files as they are now (read again after the probe).
  extra = () => ({}),
  runDirectory,
  signal,
  probe = probeGraph,
}) => {
  const info = { how: "unused", ms: 0, changed: 0 };
  let pending;
  const load = async () => {
    const started = performance.now();
    const base = sha256(
      JSON.stringify({
        schema: TEST_CACHE_SCHEMA,
        package: pkg.name,
        exclude: command.exclude,
        env: command.env,
        environment: keyedEnvironment(env),
        node: [process.version, process.platform, process.arch],
        installed: installedPackages(root),
        probe: PROBE_SOURCES.map((path) => sha256(readFileSync(path))),
      }),
    );
    const folder = resolve(directory, "graphs", base);
    const stored = readJson(resolve(folder, `${snapshot.id}.json`));
    if (stored) {
      Object.assign(info, {
        how: "stored",
        ms: Math.round(performance.now() - started),
      });
      return rebase(stored, RELATIVE, root);
    }
    const latest = readJson(resolve(folder, "latest.json"));
    const before = latest && loadSnapshot(directory, latest.snapshot);
    const previous =
      before && readJson(resolve(folder, `${latest.snapshot}.json`));
    const regraph = previous && incrementalChanges(root, before, snapshot);
    const changed = regraph?.length ? regraph : undefined;
    let previousPath;
    if (changed) {
      previousPath = resolve(runDirectory, "reached-previous-graph.json");
      writeFileSync(
        previousPath,
        JSON.stringify(rebase(previous, RELATIVE, root)),
      );
    }
    const graph = await probe(root, pkg, command, {
      env,
      signal,
      directory: runDirectory,
      ...(changed
        ? {
            previous: previousPath,
            changed: changed.map((path) => resolve(root, path)),
          }
        : {}),
    });
    Object.assign(info, {
      how: changed ? "incremental" : "probed",
      changed: changed?.length ?? 0,
    });
    // The probe read the files as they were while it ran: a graph is the
    // snapshot's only if nothing changed meanwhile. Otherwise it is kept
    // for nothing, and the reach through it widens to the whole package.
    if (repositorySnapshot(root, extra()).id === snapshot.id) {
      storeSnapshot(directory, snapshot);
      store(
        resolve(folder, `${snapshot.id}.json`),
        rebase(graph, root, RELATIVE),
      );
      store(resolve(folder, "latest.json"), {
        snapshot: snapshot.id,
        createdAt: new Date().toISOString(),
      });
    } else info.unstable = true;
    info.ms = Math.round(performance.now() - started);
    return graph;
  };
  return { get: () => (pending ??= load()), info };
};

/**
 * What a set of changed repository paths reaches among the package's test
 * files, through `reachedTests` and the cached graph: `{ whole, files,
 * known }`, package-relative. `known` (the graph's test files, when the
 * reach consulted it) bounds what a reach can speak for. Two answers are
 * the cache's own, wider than `--related`: a changed data file outside
 * every package (configuration, schemas, documents, artifacts) reaches the
 * whole package, since any suite may read it by a computed path; and so
 * does any change when the checkout moved while the graph was probed.
 */
export const reachThroughGraph =
  ({ root, pkg, command, env, signal, runDirectory, graphs }) =>
  async (changed) => {
    const packages = workspacePackages(root);
    const outside = changed.filter(
      (path) =>
        !SCRIPT.test(path) &&
        !packages.some((entry) => path.startsWith(`${entry.directory}/`)),
    );
    if (outside.length)
      return {
        whole: true,
        files: new Set(),
        why: [
          `${outside[0]} is data outside every package, which any suite may read`,
        ],
      };
    let graph;
    const reach = await reachedTests(root, pkg, command, changed, {
      env,
      signal,
      directory: runDirectory,
      probe: async () => (graph = await graphs.get()),
    });
    if (graphs.info.unstable)
      return {
        whole: true,
        files: new Set(),
        why: ["the checkout changed while its import graph was probed"],
      };
    const base = resolve(root, pkg.directory);
    return {
      whole: reach.whole,
      files: new Set(reach.files),
      known: graph
        ? new Set(graph.tests.map((file) => relative(base, file)))
        : undefined,
      why: reach.why,
    };
  };
