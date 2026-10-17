// Facts the preflight registry reads out of the tree instead of restating.
//
// Every trigger list here comes from the tool that owns it: a golden channel's
// inputs from its generator's own path constants and imports, an execution
// ledger's modules from the ledger JSON its verifier names, a package's
// dependents from the workspace manifests. If an owner renames a file, the
// registry follows on the next run; nothing here can go stale on its own.
// A derivation that finds nothing throws, so a refactor that moves the facts
// somewhere this file cannot see fails the registry test instead of quietly
// selecting nothing.

import { existsSync, readdirSync, readFileSync, statSync } from "node:fs";
import { dirname, join, relative, resolve, sep } from "node:path";
import { packageTestTask } from "../../demo/scripts/package-tasks.mjs";

const toPosix = (path) => path.split(sep).join("/");

export const readText = (root, path) =>
  readFileSync(resolve(root, path), "utf8");

// --- globs -----------------------------------------------------------------

const escapeRegExp = (text) => text.replace(/[.+^$()|[\]\\]/gu, "\\$&");

// `**` spans directories, `*` and `?` stay inside one segment, `{a,b}` is a
// literal alternation. That is every form the registry uses.
export const globToRegExp = (glob) => {
  let pattern = "";
  for (let index = 0; index < glob.length; index += 1) {
    const char = glob[index];
    if (char === "*" && glob[index + 1] === "*") {
      index += 1;
      if (glob[index + 1] === "/") {
        index += 1;
        pattern += "(?:.*/)?";
      } else {
        pattern += ".*";
      }
    } else if (char === "*") {
      pattern += "[^/]*";
    } else if (char === "?") {
      pattern += "[^/]";
    } else if (char === "{") {
      const end = glob.indexOf("}", index);
      if (end === -1) {
        throw new Error(`unterminated brace in glob '${glob}'`);
      }
      const alternatives = glob.slice(index + 1, end).split(",");
      pattern += `(?:${alternatives.map(escapeRegExp).join("|")})`;
      index = end;
    } else {
      pattern += escapeRegExp(char);
    }
  }
  return new RegExp(`^${pattern}$`, "u");
};

export const matchesAny = (path, globs) =>
  globs.some((glob) => globToRegExp(glob).test(path));

// Build outputs are neither changed-source triggers nor stable documentation inputs.
export const IGNORED_PATHS = [
  "onchain/aiken/plutus.json",
  "onchain/aiken/plutus.json.deployment.json",
];

// --- source references -------------------------------------------------------

// Comments name files for the reader's sake (specs, sibling channels); only
// code says what a script actually reads or writes.
const stripComments = (source) =>
  source
    .replace(/\/\*[\s\S]*?\*\//gu, "")
    .replace(/(^|[^:"'`\\])\/\/.*$/gmu, "$1");

const nearestPackageRoot = (root, file) => {
  let directory = dirname(file);
  while (directory.startsWith(root) && directory !== root) {
    if (existsSync(join(directory, "package.json"))) {
      return directory;
    }
    directory = dirname(directory);
  }
  return undefined;
};

const isTrackableFile = (path) =>
  existsSync(path) &&
  statSync(path).isFile() &&
  !/(^|\/)(dist|node_modules)\//u.test(toPosix(path));

// The repository files a script names in string literals or relative imports,
// resolved the way the script resolves them: `./` and `../` against the
// script's directory, anything else against the repository root and then the
// script's package root. Follow typed fixture helpers and their exact private
// package imports as well as `.mjs` and `.json` references, so helper-only edits
// select the owning generator. Returns repository-relative paths.
export const referencedFiles = (rootPath, file) => {
  const root = resolve(rootPath);
  const found = new Set();
  const visit = (absolute) => {
    const source = stripComments(readFileSync(absolute, "utf8"));
    const packageRoot = nearestPackageRoot(root, absolute);
    const literals = [
      ...source.matchAll(
        /["'`]((?:\.{1,2}\/)?[A-Za-z0-9_.@-]+(?:\/[A-Za-z0-9_.@-]+)*\.[A-Za-z0-9]+)["'`]/gu,
      ),
    ].map(([, literal]) => literal);
    for (const [, specifier] of source.matchAll(
      /\b(?:from\s+|import\s*(?:\(\s*)?|require\s*\(\s*)["'](#[A-Za-z0-9_./-]+)["']/gu,
    )) {
      const manifest = packageRoot && join(packageRoot, "package.json");
      const target =
        manifest &&
        JSON.parse(readFileSync(manifest, "utf8")).imports?.[specifier];
      if (typeof target !== "string" || !target.startsWith("./")) {
        throw new Error(
          `${absolute}: private import '${specifier}' needs an exact local package target`,
        );
      }
      const resolved = resolve(packageRoot, target);
      if (
        !resolved.startsWith(`${packageRoot}${sep}`) ||
        !isTrackableFile(resolved)
      ) {
        throw new Error(
          `${absolute}: private import '${specifier}' has no repository local target`,
        );
      }
      found.add(toPosix(relative(root, manifest)));
      literals.push(`./${toPosix(relative(dirname(absolute), resolved))}`);
    }
    for (const literal of literals) {
      const candidates = literal.startsWith(".")
        ? [resolve(dirname(absolute), literal)]
        : [
            resolve(root, literal),
            ...(packageRoot === undefined
              ? []
              : [resolve(packageRoot, literal)]),
            // A bare sibling name joined onto the script's own directory.
            ...(literal.includes("/")
              ? []
              : [resolve(dirname(absolute), literal)]),
          ];
      const hit = candidates.find(
        (candidate) =>
          candidate.startsWith(`${root}${sep}`) && isTrackableFile(candidate),
      );
      if (hit === undefined) {
        continue;
      }
      const path = toPosix(relative(root, hit));
      if (
        IGNORED_PATHS.includes(path) ||
        found.has(path) ||
        path === toPosix(relative(root, file))
      ) {
        continue;
      }
      found.add(path);
      if (/\.(?:mjs|json|[cm]?ts)$/u.test(hit)) {
        visit(hit);
      }
    }
  };
  visit(resolve(root, file));
  return [...found].sort();
};

// --- workspace -------------------------------------------------------------

export const DEMO = "demo";

// `pnpm-workspace.yaml` is a flat `packages:` list here; anything richer
// (globs, negations) would have to be read by pnpm itself, so refuse it.
export const workspacePackages = (root) => {
  const manifest = readText(root, `${DEMO}/pnpm-workspace.yaml`);
  const directories = [...manifest.matchAll(/^\s*-\s*["']?([^"'\s#]+)/gmu)].map(
    ([, directory]) => directory,
  );
  if (directories.length === 0 || directories.some((d) => /[*!]/u.test(d))) {
    throw new Error(
      `${DEMO}/pnpm-workspace.yaml must list package directories literally`,
    );
  }
  const packages = directories.map((directory) => {
    const manifestPath = `${DEMO}/${directory}/package.json`;
    const json = JSON.parse(readText(root, manifestPath));
    return {
      ...json,
      directory: `${DEMO}/${directory}`,
      name: json.name,
      scripts: json.scripts ?? {},
      workspaceDependencies: Object.entries({
        ...json.dependencies,
        ...json.devDependencies,
        ...json.peerDependencies,
      })
        .filter(([, spec]) => String(spec).startsWith("workspace:"))
        .map(([name]) => name),
    };
  });
  return packages;
};

const closure = (start, next) => {
  const seen = new Set(start);
  const queue = [...start];
  while (queue.length > 0) {
    for (const item of next(queue.shift())) {
      if (!seen.has(item)) {
        seen.add(item);
        queue.push(item);
      }
    }
  }
  return seen;
};

export const workspaceDependencyClosure = (packages, names) => {
  const byName = new Map(packages.map((pkg) => [pkg.name, pkg]));
  return closure(
    names,
    (name) => byName.get(name)?.workspaceDependencies ?? [],
  );
};

// The packages a change to `names` can break: themselves and every package
// that depends on them, directly or not. `pnpm --filter ...<name>` computes the
// same set; computing it here lets preflight decide capabilities per package
// and print the expansion in `--list`.
export const workspaceDependentClosure = (packages, names) =>
  closure(names, (name) =>
    packages
      .filter((pkg) => pkg.workspaceDependencies.includes(name))
      .map((pkg) => pkg.name),
  );

// Test prerequisites are package contracts; source spelling cannot add or
// remove a database obligation from the same task the runner executes.
export const packageNeedsPostgres = (_root, pkg) =>
  packageTestTask(pkg)?.prerequisites?.some(
    ({ kind }) => kind === "postgres",
  ) ?? false;

// --- CI ---------------------------------------------------------------------

export const workflowText = (root) => {
  const directory = resolve(root, ".github/workflows");
  return readdirSync(directory)
    .filter((name) => /\.ya?ml$/u.test(name))
    .sort()
    .map((name) => readFileSync(join(directory, name), "utf8"))
    .join("\n");
};

// Whether a workflow runs a check by id: `node scripts/preflight.mjs --run
// <id>`, possibly among other --run ids on the same line.
export const workflowRunsCheck = (text, id) =>
  new RegExp(
    `scripts/preflight\\.mjs(?:[ \\t]+--run[ \\t]+\\S+)*[ \\t]+--run[ \\t]+${escapeRegExp(id)}(?=\\s|$)`,
    "mu",
  ).test(text);

// --- golden channels ---------------------------------------------------------

// A channel is any package script named `<kind>:<name>:check` that runs a
// generator with `--check`: the golden fixtures and the generated profile
// document. Its sync twin is the same script without `:check` (or with
// `:sync`), whichever the package declares.
export const goldenChannels = (root, packages = workspacePackages(root)) => {
  const channels = [];
  for (const pkg of packages) {
    for (const [script, command] of Object.entries(pkg.scripts)) {
      if (!/^(fixtures|docs):.+:check$/u.test(script)) {
        continue;
      }
      const generator = command.match(/\bnode\s+(scripts\/\S+\.mjs)\b/u)?.[1];
      if (generator === undefined || !/--check\b/u.test(command)) {
        throw new Error(
          `${pkg.directory}/package.json '${script}' does not run a generator with --check`,
        );
      }
      const stem = script.slice(0, -":check".length);
      const sync = [`${stem}:sync`, stem].find(
        (candidate) => pkg.scripts[candidate] !== undefined,
      );
      const generatorPath = `${pkg.directory}/${generator}`;
      channels.push({
        package: pkg,
        script,
        sync,
        generator: generatorPath,
        references: referencedFiles(root, generatorPath),
        buildsCore: /\.\.\/midgard-core\b/u.test(command),
      });
    }
  }
  return channels;
};

// --- Aiken modules -----------------------------------------------------------

export const AIKEN_PROJECT = "onchain/aiken";
export const AIKEN_SOURCE_ROOTS = ["lib", "validators"];

// Aiken names a module after its path under lib/ or validators/ with hyphens
// folded to underscores; `run-focused-check.mjs` and the ledgers accept either
// spelling, so both resolve here.
export const toAikenModuleName = (sourcePath) =>
  sourcePath.replaceAll("-", "_");

export const indexAikenModules = (root) => {
  const modules = [];
  for (const sourceRoot of AIKEN_SOURCE_ROOTS) {
    const directory = resolve(root, AIKEN_PROJECT, sourceRoot);
    if (!existsSync(directory)) {
      continue;
    }
    for (const entry of readdirSync(directory, { recursive: true })) {
      const path = toPosix(String(entry));
      if (!path.endsWith(".ak")) {
        continue;
      }
      const source = readFileSync(join(directory, path), "utf8");
      const sourcePath = path.slice(0, -".ak".length);
      modules.push({
        file: `${AIKEN_PROJECT}/${sourceRoot}/${path}`,
        sourcePath,
        name: toAikenModuleName(sourcePath),
        imports: [...source.matchAll(/^use\s+([A-Za-z0-9_/-]+)/gmu)].map(
          ([, name]) => toAikenModuleName(name),
        ),
        hasTests: /^test\s+[a-z0-9_]+/mu.test(source),
      });
    }
  }
  modules.sort((a, b) => a.file.localeCompare(b.file));
  const byAlias = new Map();
  for (const module of modules) {
    for (const alias of [module.sourcePath, module.name]) {
      if (!byAlias.has(alias)) {
        byAlias.set(alias, module);
      }
    }
  }
  return { modules, byAlias, byFile: new Map(modules.map((m) => [m.file, m])) };
};

// Every source file a module's compiled code can depend on: itself and what it
// imports, transitively. Standard-library imports resolve to nothing and drop.
export const aikenImportClosure = (index, moduleNames) => {
  const start = moduleNames.map((name) => {
    const module = index.byAlias.get(name);
    if (module === undefined) {
      throw new Error(`no Aiken module '${name}' under lib/ or validators/`);
    }
    return module.file;
  });
  return closure(start, (file) =>
    index.byFile
      .get(file)
      .imports.map((name) => index.byAlias.get(name)?.file)
      .filter((next) => next !== undefined),
  );
};

// --- script budgets ----------------------------------------------------------

export const SCRIPT_BUDGET_TABLE = `${DEMO}/midgard-core/src/script-budgets.json`;
export const SCRIPT_BUDGET_JUDGE = `${AIKEN_PROJECT}/scripts/script-budgets.mjs`;

// The Aiken modules the script-budget table measures, in table order.
export const scriptBudgetModules = (root) => {
  const budgets = JSON.parse(readText(root, SCRIPT_BUDGET_TABLE)).budgets ?? {};
  const modules = new Set(
    Object.values(budgets).map((entry) =>
      entry.test.slice(0, entry.test.lastIndexOf("/")),
    ),
  );
  if (modules.size === 0) {
    throw new Error(`${SCRIPT_BUDGET_TABLE} measures no modules`);
  }
  return [...modules];
};
