import { spawnSync } from "node:child_process";
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { basename, dirname, extname, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

import { packageClosure, workspacePackages } from "./files.mjs";
import { runProcess } from "./process.mjs";

// Which of a package's test files a set of changed files can reach. A list
// that misses a test is a gate that cannot fail, so every rule here adds
// tests and none removes one; what it cannot see it reports and widens.
//
// - Imports: the package's own Vitest projects resolve the import graph
//   (reached-probe.mjs), in source mode so edges into other workspace
//   packages are not hidden inside the per-run bundles. Unlike `--related`
//   the graph is rooted at every script of the package and every script
//   outside the packages, not only at test files.
// - Names: a module whose string literals (outside import specifiers) spell
//   a file's name (a worker entry, a spawned script, a fixture, a document)
//   depends on that file, and on what that file depends on. Scripts answer
//   to their stem with any script extension, data files also to a
//   distinctive bare stem. A name several files share counts only where the
//   module can mean that file: enough of its path to pick it alone, the same
//   owner, or a literal spelling the owner's directory. A module that lists
//   a directory names every file of its own package under the directories
//   it spells.
// - Other packages' scripts the graph does not hold (spawned, not imported)
//   count as changed when their package's closure may have changed, and are
//   named only by a path that picks them alone.
// - Builds: a module that imports a package's dist, or test-side code that
//   spells `dist`, runs compiled output, so a changed build input of that
//   package's closure reaches it.
// - Widening: a changed package manifest or config, the shared test harness,
//   a data file nothing names, or a failed probe selects whole packages; a
//   module the probe cannot analyse is always affected. Each says why.
//
// Known limits: a file named only through computed text with none of its
// name spelled, and a shared name used from another package without any
// literal naming that package.

const SCRIPT = /\.[cm]?[jt]sx?$/u;
const SCRIPT_EXTENSIONS = [
  ".ts",
  ".tsx",
  ".mts",
  ".cts",
  ".js",
  ".jsx",
  ".mjs",
  ".cjs",
];
const MARKDOWN = /\.mdx?$/u;
const BLUEPRINT = "onchain/aiken/plutus.json";
const BLUEPRINT_INPUTS =
  /^onchain\/aiken\/(?:lib\/|validators\/|env\/|aiken\.toml$|aiken\.lock$)/u;
const SHARED_HARNESS = "demo/midgard-test-support/";
// Workspace files every package's install, resolution or run reads.
const WORKSPACE_CONFIG =
  /^demo\/(?:package\.json$|pnpm-[^/]+\.yaml$|\.nvmrc$|tsconfig[^/]*\.json$|vitest[^/]*$|patches\/|vendor\/)/u;
const PROBE = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "reached-probe.mjs",
);

const stemOf = (path) => {
  const name = basename(path);
  return name.slice(0, name.length - extname(name).length);
};
const distinctive = (stem) => /[-_.]/u.test(stem);

/** The spellings by which a module names `path` (outside an import). */
export const namesOf = (path) => {
  const name = basename(path);
  const stem = stemOf(path);
  if (SCRIPT.test(name)) return SCRIPT_EXTENSIONS.map((ext) => `${stem}${ext}`);
  return distinctive(stem) ? [name, stem] : [name];
};

/** The text a substring search needs to find before an edge can exist. */
const needles = (path) => {
  const stem = stemOf(path);
  return [stem === "index" ? basename(dirname(path)) : stem];
};

const ownerOf = (packages, path) =>
  packages.find((pkg) => path.startsWith(`${pkg.directory}/`));

// --- the cheap text pre-filter --------------------------------------------

let fileIndex;
/** Every file, tracked or not ignored, that exists. */
const repositoryFiles = (root) => {
  if (fileIndex?.root === root) return fileIndex.files;
  const listed = spawnSync(
    "git",
    ["ls-files", "-z", "--cached", "--others", "--exclude-standard"],
    { cwd: root, encoding: "utf8", maxBuffer: 256 * 1024 * 1024 },
  );
  if (listed.status !== 0)
    throw new Error(`git ls-files failed: ${listed.stderr.trim()}`);
  const files = [...new Set(listed.stdout.split("\0"))].filter(
    (path) => path && existsSync(resolve(root, path)),
  );
  fileIndex = { root, files };
  return files;
};

let textIndex;
/** Every script file, tracked or not ignored, with its text. */
const scriptTexts = (root) => {
  if (textIndex?.root === root) return textIndex.files;
  const files = repositoryFiles(root)
    .filter((path) => SCRIPT.test(path))
    .map((path) => ({ path, text: readFileSync(resolve(root, path), "utf8") }));
  textIndex = { root, files };
  return files;
};

let usersIndex;
/**
 * Which packages use which, by declared dependency or by spelling another
 * package's directory name in a script (running its files without depending
 * on it), transitively: `users.get(name)` holds every package that may run
 * code of `name`, itself included.
 */
const packageUsers = (root) => {
  const packages = workspacePackages(root);
  const uses = new Map(
    packages.map((pkg) => [
      pkg.name,
      new Set(packageClosure(root, pkg.name).map((dep) => dep.name)),
    ]),
  );
  for (const { path, text } of scriptTexts(root)) {
    const holder = ownerOf(packages, path);
    if (!holder) continue;
    for (const other of packages)
      if (text.includes(basename(other.directory)))
        uses.get(holder.name).add(other.name);
  }
  for (let grown = true; grown; ) {
    grown = false;
    for (const used of uses.values())
      for (const name of [...used])
        for (const next of uses.get(name))
          if (!used.has(next)) {
            used.add(next);
            grown = true;
          }
  }
  return new Map(
    packages.map((pkg) => [
      pkg.name,
      packages
        .filter((user) => uses.get(user.name).has(pkg.name))
        .map((user) => user.name),
    ]),
  );
};

/**
 * The workspace packages whose tests `path` could reach, by a sound
 * over-approximation: an import of a file or a spelled name of it contains
 * its stem, so a package none of whose usable script text contains the
 * stem cannot reach it. Text outside every package (repository scripts) can
 * be imported by any package, so a hit there selects them all.
 */
export const candidatePackages = (root, path) => {
  const packages = workspacePackages(root);
  if (usersIndex?.root !== root)
    usersIndex = { root, users: packageUsers(root) };
  const found = new Set();
  // A file under a package's tests/ is that package's own; another package
  // reaching it spells its name, which the text search below finds.
  const reachers = (file) => {
    const holder = ownerOf(packages, file);
    return file.startsWith(`${holder.directory}/tests/`)
      ? [holder.name]
      : usersIndex.users.get(holder.name);
  };
  if (ownerOf(packages, path))
    for (const name of reachers(path)) found.add(name);
  const targets = BLUEPRINT_INPUTS.test(path) ? [path, BLUEPRINT] : [path];
  for (const target of targets) {
    const sought = needles(target);
    for (const { path: file, text } of scriptTexts(root)) {
      if (!sought.some((needle) => text.includes(needle))) continue;
      if (!ownerOf(packages, file))
        return new Set(packages.map((pkg) => pkg.name));
      for (const name of reachers(file)) found.add(name);
    }
  }
  return found;
};

// --- the reach itself -------------------------------------------------------

/**
 * The reach of `changed` (repository-relative paths) into one package, given
 * its probed graph: `{ tests, edges, failed, spelled }` as reached-probe.mjs
 * writes it, keyed by absolute path.
 */
export const reachIn = ({
  root,
  pkg,
  graph,
  changed,
  files = repositoryFiles(root),
  texts = scriptTexts(root),
}) => {
  const packages = workspacePackages(root);
  const closure = new Set(
    packageClosure(root, pkg.name).map((dep) => dep.directory),
  );
  const absolute = (path) => resolve(root, path);
  const repoPath = (file) => relative(root, file).split(sep).join("/");
  const why = [];
  const tests = new Set(graph.tests);
  const relevant = new Set(changed.map(absolute));
  if (changed.some((path) => BLUEPRINT_INPUTS.test(path)))
    relevant.add(absolute(BLUEPRINT));

  const importers = new Map();
  for (const [file, dependencies] of Object.entries(graph.edges))
    for (const dependency of dependencies) {
      if (!importers.has(dependency)) importers.set(dependency, new Set());
      importers.get(dependency).add(file);
    }
  const modules = Object.keys(graph.edges);
  // Each module's path-like literals, split into segments, and every segment
  // it spells. A module the probe could not spell is in `graph.failed`, so
  // it is affected whatever it names.
  const spelled = new Map(
    Object.entries(graph.spelled ?? {}).map(([file, { paths, walks }]) => {
      const tokens = paths.map((path) => ({
        parts: path
          .split("/")
          .filter((part) => part && part !== "." && part !== ".."),
      }));
      return [
        file,
        {
          tokens: tokens.filter(({ parts }) => parts.length),
          segments: new Set(tokens.flatMap(({ parts }) => parts)),
          walks,
        },
      ];
    }),
  );
  const nothing = { tokens: [], segments: new Set(), walks: false };
  const spelledBy = (file) => spelled.get(file) ?? nothing;
  // The repository files answering to each name, for telling a name several
  // files share apart.
  const answering = new Map();
  for (const path of files)
    for (const name of namesOf(path)) {
      if (!answering.has(name)) answering.set(name, []);
      answering.get(name).push(path.split("/"));
    }
  // A file's owner: its package, or outside packages its top directory.
  const ownerKey = (file) => {
    const path = repoPath(file);
    const owner = ownerOf(packages, path);
    if (owner) return owner.directory;
    const parts = path.split("/");
    return parts[0] === "demo" && parts.length > 2
      ? parts.slice(0, 2).join("/")
      : parts[0];
  };
  /**
   * Whether `module` names `target` by `token`, whose last segment is one of
   * `target`'s names. A name several files share counts when the module
   * spells enough of the target's path to pick it alone, lives beside it
   * (same owner), or spells the target's owner directory name in some
   * literal; for a `strong` match, only the first.
   */
  const names = (module, token, target, { strong = false } = {}) => {
    const path = repoPath(target);
    const parts = path.split("/");
    const name = token.parts.at(-1);
    let matched = 1;
    while (
      matched < token.parts.length &&
      matched < parts.length &&
      token.parts.at(-1 - matched) === parts.at(-1 - matched)
    )
      matched += 1;
    const suffix = token.parts.slice(-matched, -1).join("/");
    const sharing = (answering.get(name) ?? []).filter(
      (other) => other.slice(-matched, -1).join("/") === suffix,
    );
    if (sharing.length <= 1) return true;
    if (strong) return false;
    if (ownerKey(module) === ownerKey(target)) return true;
    return spelledBy(module).segments.has(ownerKey(target).split("/").at(-1));
  };
  // Directories a walker could list to find `file`: those below its package
  // directory (or below the repository root for a file outside packages).
  const directoriesOf = (file) => {
    const owner = ownerOf(packages, repoPath(file));
    const base = owner ? absolute(owner.directory) : root;
    return relative(base, dirname(file)).split(sep).filter(Boolean);
  };

  // Another package's script that the graph does not hold (spawned, not
  // imported) has imports the graph does not know: count it changed when its
  // package's closure holds a changed script, or a script spelling a changed
  // file's stem (which reads that file, or imports it from outside). A
  // package's tests are not part of what other packages import.
  const touched = new Set();
  const importable = (path) => {
    const owner = ownerOf(packages, path);
    return owner && !path.startsWith(`${owner.directory}/tests/`)
      ? owner.directory
      : undefined;
  };
  for (const file of relevant) {
    const path = repoPath(file);
    if (SCRIPT.test(path)) touched.add(importable(path));
    const sought = needles(path);
    for (const { path: other, text } of texts)
      if (sought.some((needle) => text.includes(needle)))
        touched.add(importable(other));
  }
  const closureChanged = new Map(
    packages.map((entry) => [
      entry.name,
      packageClosure(root, entry.name).some((dep) =>
        touched.has(dep.directory),
      ),
    ]),
  );
  const foreign = files
    .filter((path) => {
      if (!SCRIPT.test(path)) return false;
      const owner = ownerOf(packages, path);
      return (
        owner &&
        owner.name !== pkg.name &&
        closureChanged.get(owner.name) &&
        !(absolute(path) in graph.edges)
      );
    })
    .map(absolute);
  const foreignSet = new Set(foreign);
  for (const [file, reason] of Object.entries(graph.failed))
    why.push(
      `${repoPath(file)} could not be analysed (${reason}); what imports or names it always runs`,
    );
  // Compiled output: a module that imports a package's dist, or a test-side
  // module (anything outside a package's src) that spells `dist`, runs what
  // a build of its closure produces from changed scripts or imported data.
  // Package source that spells dist runs as source under test.
  const builtFrom = new Map(
    packages.map((entry) => [
      entry.directory,
      new Set(packageClosure(root, entry.name).map((dep) => dep.directory)),
    ]),
  );
  const builtInputOf = (file) => {
    const path = repoPath(file);
    const owner = ownerOf(packages, path);
    return owner &&
      !path.startsWith(`${owner.directory}/tests/`) &&
      (SCRIPT.test(path) || importers.has(file))
      ? owner.directory
      : undefined;
  };
  const distOf = new Map(
    modules.map((module) => [
      module,
      [
        ...new Set(
          graph.edges[module].flatMap((dependency) => {
            const owner = ownerOf(packages, repoPath(dependency));
            return owner &&
              repoPath(dependency).startsWith(`${owner.directory}/dist/`)
              ? [owner.directory]
              : [];
          }),
        ),
        ...(spelledBy(module).segments.has("dist") &&
        !packages.some((entry) =>
          repoPath(module).startsWith(`${entry.directory}/src/`),
        )
          ? [pkg.directory]
          : []),
      ],
    ]),
  );
  const frontier = new Set([...relevant, ...Object.keys(graph.failed)]);
  const named = new Set();
  let affected;
  for (;;) {
    affected = new Set(frontier);
    const queue = [...frontier];
    while (queue.length) {
      for (const importer of importers.get(queue.pop()) ?? [])
        if (!affected.has(importer)) {
          affected.add(importer);
          queue.push(importer);
        }
    }
    const builtInputs = [
      ...new Set([...affected].map(builtInputOf).filter(Boolean)),
    ];
    const spellings = new Map();
    for (const file of [...affected, ...foreign])
      for (const name of namesOf(file)) {
        if (!spellings.has(name)) spellings.set(name, []);
        spellings.get(name).push(file);
      }
    const added = [];
    for (const module of modules) {
      if (frontier.has(module)) continue;
      const { tokens, segments, walks } = spelledBy(module);
      let target;
      for (const token of tokens) {
        target = spellings
          .get(token.parts.at(-1))
          ?.find(
            (file) =>
              file !== module &&
              names(module, token, file, { strong: foreignSet.has(file) }),
          );
        if (target) break;
      }
      if (!target && walks)
        target = [...affected].find(
          (file) =>
            file !== module &&
            ownerOf(packages, repoPath(file))?.name ===
              ownerOf(packages, repoPath(module))?.name &&
            directoriesOf(file).some((directory) => segments.has(directory)),
        );
      if (target) {
        added.push(module);
        named.add(target);
        if (relevant.has(target))
          why.push(`${repoPath(module)} names ${repoPath(target)}`);
        continue;
      }
      const built = distOf
        .get(module)
        .find((directory) =>
          builtInputs.some((input) => builtFrom.get(directory).has(input)),
        );
      if (built) {
        added.push(module);
        why.push(
          `${repoPath(module)} runs ${built}/dist (imports it or spells dist), and a built input changed`,
        );
      }
    }
    if (!added.length) break;
    for (const module of added) frontier.add(module);
  }

  // A changed data file that nothing imports or names may be read through a
  // computed path; widen to the whole package rather than guess. Another
  // package's test data is read only by that package's tests.
  for (const file of relevant) {
    const path = repoPath(file);
    const owner = ownerOf(packages, path);
    if (
      owner &&
      closure.has(owner.directory) &&
      (owner.name === pkg.name ||
        !path.startsWith(`${owner.directory}/tests/`)) &&
      !SCRIPT.test(path) &&
      !MARKDOWN.test(path) &&
      !named.has(file) &&
      !importers.has(file)
    )
      return {
        whole: true,
        files: [],
        why: [
          `nothing imports or names ${path}; it may be read through a computed path`,
        ],
      };
  }

  const reached = new Set([...affected].filter((file) => tests.has(file)));
  for (const file of relevant) {
    const path = repoPath(file);
    if (
      path.startsWith(`${pkg.directory}/`) &&
      existsSync(file) &&
      ![...reached].some((test) => test === file) &&
      !importers.has(file) &&
      !named.has(file)
    )
      why.push(`no test of ${pkg.name} imports or names ${path}`);
  }
  return {
    whole: false,
    files: [...reached]
      .sort()
      .map((file) => relative(absolute(pkg.directory), file)),
    why: [`${reached.size} test file(s) reached`, ...why],
  };
};

/**
 * Changes that select a whole package without looking further: the shared
 * test harness, a workspace manifest or configuration, or a package-root
 * file (manifest, tsconfig, Vitest or build config) in the package's closure.
 */
export const wholePackageReasons = (root, pkg, changed) => {
  const packages = workspacePackages(root);
  const closure = new Set(
    packageClosure(root, pkg.name).map((dep) => dep.directory),
  );
  return changed.flatMap((path) => {
    if (MARKDOWN.test(path)) return [];
    if (path.startsWith(SHARED_HARNESS))
      return [`${path} is in the shared test harness`];
    const owner = ownerOf(packages, path);
    if (!owner)
      return WORKSPACE_CONFIG.test(path)
        ? [`${path} is workspace configuration`]
        : [];
    return closure.has(owner.directory) && dirname(path) === owner.directory
      ? [`${path} configures ${owner.name}`]
      : [];
  });
};

/**
 * The workspace packages a set of changed files can reach: those
 * `reachedTests` would not answer "nothing reached" for without probing.
 */
export const reachablePackages = (root, changed) => {
  const candidates = changed.map((path) => candidatePackages(root, path));
  return new Set(
    workspacePackages(root)
      .filter(
        (pkg) =>
          wholePackageReasons(root, pkg, changed).length > 0 ||
          candidates.some((names) => names.has(pkg.name)),
      )
      .map((pkg) => pkg.name),
  );
};

/**
 * The script files the probe roots the graph at: the package's own and those
 * outside every package (repository and workspace scripts). Another
 * package's files are graphed through imports with their own resolution;
 * type declarations and dot directories run nothing.
 */
const graphRoots = (root, pkg) => {
  const packages = workspacePackages(root);
  return scriptTexts(root)
    .map(({ path }) => path)
    .filter((path) => {
      if (/\.d\.[cm]?ts$/u.test(path) || /(?:^|\/)\./u.test(path)) return false;
      const owner = ownerOf(packages, path);
      return !owner || owner.name === pkg.name;
    });
};

/** Probe a package's import graph in a child process (reached-probe.mjs). */
export const probeGraph = async (
  root,
  pkg,
  command,
  { env, signal, directory },
) => {
  const output = resolve(directory, "reached-graph.json");
  const request = resolve(directory, "reached-request.json");
  writeFileSync(
    request,
    JSON.stringify({
      output,
      exclude: command.exclude,
      files: graphRoots(root, pkg).map((path) => resolve(root, path)),
    }),
  );
  const run = await runProcess({
    argv: [process.execPath, PROBE, request],
    cwd: resolve(root, pkg.directory),
    env: {
      ...env,
      NODE_ENV: "test",
      ...command.env,
      // Resolve workspace packages from source: the per-run bundles would
      // hide every edge into another package.
      MIDGARD_TEST_WORKSPACE_BUNDLE: "0",
    },
    signal,
    logPath: resolve(directory, "reached-probe.log"),
    timeoutMs: 600_000,
  });
  if (run.exitCode !== 0 || !existsSync(output))
    throw new Error(`the import-graph probe failed; log: ${run.logPath}`);
  return JSON.parse(readFileSync(output, "utf8"));
};

/**
 * The test files of `pkg` that `changed` reaches: `{ whole, files, why }`.
 * `whole` means run the package as its test script does.
 */
export const reachedTests = async (
  root,
  pkg,
  command,
  changed,
  { env = process.env, signal, directory, probe = probeGraph } = {},
) => {
  const whole = wholePackageReasons(root, pkg, changed);
  if (whole.length)
    return { whole: true, files: [], relevant: changed, why: whole };
  const relevant = changed.filter((path) =>
    candidatePackages(root, path).has(pkg.name),
  );
  if (!relevant.length)
    return {
      whole: false,
      files: [],
      relevant,
      why: [`no changed file can reach ${pkg.name}`],
    };
  let graph;
  try {
    graph = await probe(root, pkg, command, { env, signal, directory });
  } catch (error) {
    signal?.throwIfAborted();
    return {
      whole: true,
      files: [],
      relevant,
      why: [`${error.message}; running the whole package`],
    };
  }
  return { ...reachIn({ root, pkg, graph, changed: relevant }), relevant };
};
