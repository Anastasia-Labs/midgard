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
// script's package root. Referenced `.mjs` files are followed, so a shared
// helper's own inputs count too. Returns repository-relative paths.
export const referencedFiles = (rootPath, file) => {
  const root = resolve(rootPath);
  const found = new Set();
  const visit = (absolute) => {
    const source = stripComments(readFileSync(absolute, "utf8"));
    const packageRoot = nearestPackageRoot(root, absolute);
    for (const [, literal] of source.matchAll(
      /["'`]((?:\.{1,2}\/)?[A-Za-z0-9_.@-]+(?:\/[A-Za-z0-9_.@-]+)*\.[A-Za-z0-9]+)["'`]/gu,
    )) {
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
      if (found.has(path) || path === toPosix(relative(root, file))) {
        continue;
      }
      found.add(path);
      if (hit.endsWith(".mjs")) {
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

// A package whose Vitest global setup provisions Postgres shards cannot run its
// suite without the test server; the others can.
export const packageNeedsPostgres = (root, pkg) => {
  const setup = resolve(root, pkg.directory, "tests/global-setup.ts");
  return (
    existsSync(setup) &&
    /PgClient|provisionMidgardNodeTestDatabaseShards/u.test(
      readFileSync(setup, "utf8"),
    )
  );
};

// --- CI ---------------------------------------------------------------------

export const workflowText = (root) => {
  const directory = resolve(root, ".github/workflows");
  return readdirSync(directory)
    .filter((name) => /\.ya?ml$/u.test(name))
    .sort()
    .map((name) => readFileSync(join(directory, name), "utf8"))
    .join("\n");
};

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

// --- execution ledgers -------------------------------------------------------

const LEDGER_SCRIPTS = `${AIKEN_PROJECT}/scripts`;

export const execLedgers = (root) => {
  const ledgers = [];
  for (const name of readdirSync(resolve(root, LEDGER_SCRIPTS)).sort()) {
    if (!/^verify-.+-exec-ledger-v1\.mjs$/u.test(name)) {
      continue;
    }
    const verifier = `${LEDGER_SCRIPTS}/${name}`;
    const source = readText(root, verifier);
    const ledgerNames = new Set(
      [
        ...stripComments(source).matchAll(
          /["'`]([a-z0-9-]+-exec-ledger-v1\.json)["'`]/gu,
        ),
      ].map(([, file]) => file),
    );
    if (ledgerNames.size !== 1) {
      throw new Error(
        `${verifier} must name exactly one ledger JSON; found ${String(ledgerNames.size)}`,
      );
    }
    const ledger = `${LEDGER_SCRIPTS}/${[...ledgerNames][0]}`;
    const modules = (JSON.parse(readText(root, ledger)).modules ?? []).map(
      (entry) => entry.module,
    );
    if (modules.length === 0) {
      throw new Error(`${ledger} measures no modules`);
    }
    // The verifier's own usage line names the Aiken environment it needs.
    const environment = source.match(
      new RegExp(
        `MIDGARD_AIKEN_ENV=([a-z0-9_-]+)[^\\n]*scripts/${escapeRegExp(name)}`,
        "u",
      ),
    )?.[1];
    ledgers.push({
      id: name.slice("verify-".length, -"-exec-ledger-v1.mjs".length),
      verifier,
      ledger,
      modules,
      environment,
      scripts: referencedFiles(root, verifier),
    });
  }
  if (ledgers.length === 0) {
    throw new Error(`no execution-ledger verifiers under ${LEDGER_SCRIPTS}`);
  }
  return ledgers;
};
