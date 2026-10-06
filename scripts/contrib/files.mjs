import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import {
  existsSync,
  lstatSync,
  readdirSync,
  readFileSync,
  realpathSync,
  renameSync,
  mkdirSync,
  writeFileSync,
} from "node:fs";
import { dirname, relative, resolve, sep } from "node:path";
import { randomUUID } from "node:crypto";
import { NATIVE_RECIPES } from "./native-recipes.mjs";

export const sha256 = (value) =>
  createHash("sha256").update(value).digest("hex");

// One freshness verdict hashes the shared closure once per dependency it
// visits. A scope memoizes package lists, file digests and identities for
// one synchronous verdict only: nothing can build or edit while it is open,
// so no entry outlives the state it describes.
let identityScope;
export const withIdentityScope = (action) => {
  if (identityScope) return action();
  identityScope = new Map();
  try {
    return action();
  } finally {
    identityScope = undefined;
  }
};
const scoped = (key, compute) => {
  if (!identityScope) return compute();
  if (!identityScope.has(key)) identityScope.set(key, compute());
  return identityScope.get(key);
};
export const json = (path) => JSON.parse(readFileSync(path, "utf8"));
export const atomicJson = (path, value) => {
  mkdirSync(dirname(path), { recursive: true });
  const temporary = `${path}.${randomUUID()}.tmp`;
  writeFileSync(temporary, `${JSON.stringify(value, null, 2)}\n`, {
    flag: "wx",
    mode: 0o600,
  });
  renameSync(temporary, path);
};

// Verify every existing ancestor as well as the lexical path. A path through
// a symlink must never escape the declared checkout, including new outputs.
export const inside = (root, path) => {
  const base = realpathSync(root);
  const target = resolve(base, path);
  const contained = (value) =>
    value === base || value.startsWith(`${base}${sep}`);
  if (!contained(target) || target === base)
    throw new Error(`expected a file inside ${base}: ${path}`);
  let ancestor = target;
  while (!existsSync(ancestor)) ancestor = dirname(ancestor);
  if (!contained(realpathSync(ancestor)))
    throw new Error(`symlink escapes checkout: ${path}`);
  return target;
};

const managed = new Set(["node_modules", ".git", ".claude"]);
const sourceDirectories = new Set([
  "src",
  "lib",
  "validators",
  "env",
  "scripts",
  "tests",
  "fixtures",
  "testSupport",
  "patches",
  "vendor",
]);
const packageOutputs = new Set([
  "dist",
  "coverage",
  "logs",
  ".tmp",
  ".next",
  ".turbo",
  ".architecture-f-wasm",
  "deploymentInfo",
]);
// Output ownership comes from the containing project, never a basename in a
// source tree: src/build and lib/target are legitimate inputs.
const excludedInput = (directory, name, source) => {
  if (managed.has(name)) return true;
  if (source) return false;
  return (
    (packageOutputs.has(name) &&
      existsSync(resolve(directory, "package.json"))) ||
    (name === "target" && existsSync(resolve(directory, "Cargo.toml"))) ||
    (name === "build" && existsSync(resolve(directory, "aiken.toml")))
  );
};
const sourceAncestor = (path) => {
  let directory = resolve(path);
  while (directory !== dirname(directory)) {
    if (sourceDirectories.has(directory.split(sep).at(-1))) return true;
    if (
      ["package.json", "Cargo.toml", "aiken.toml"].some((file) =>
        existsSync(resolve(directory, file)),
      )
    )
      return false;
    directory = dirname(directory);
  }
  return false;
};
export const sourceInput = (root, path) => {
  let directory = root;
  let source = false;
  for (const name of path.split("/")) {
    if (excludedInput(directory, name, source)) return false;
    source ||= sourceDirectories.has(name);
    directory = resolve(directory, name);
  }
  return true;
};
export const filesUnder = (root, { outputs = false } = {}) => {
  const files = [];
  if (!existsSync(root)) return files;
  const visit = (directory, source) => {
    for (const entry of readdirSync(directory, { withFileTypes: true }).sort(
      (a, b) => a.name.localeCompare(b.name),
    )) {
      if (!outputs && excludedInput(directory, entry.name, source)) continue;
      const path = resolve(directory, entry.name);
      // Linked source inputs cannot be silently omitted from an identity.
      if (entry.isSymbolicLink())
        throw new Error(`symlink in input/output tree: ${path}`);
      if (entry.isDirectory())
        visit(path, source || sourceDirectories.has(entry.name));
      else if (entry.isFile()) files.push(path);
    }
  };
  visit(root, sourceAncestor(root));
  return files;
};

export const hashFiles = (root, paths) => {
  const entries = [...new Set(paths)].sort().map((path) =>
    scoped(`file\0${root}\0${path}`, () => {
      const absolute = inside(root, path);
      if (!lstatSync(absolute).isFile())
        throw new Error(`input is not a regular file: ${path}`);
      return [
        relative(root, absolute).split(sep).join("/"),
        sha256(readFileSync(absolute)),
      ];
    }),
  );
  return {
    sha256: sha256(JSON.stringify(entries)),
    files: Object.fromEntries(entries),
  };
};

export const workspacePackages = (root) =>
  scoped(`packages\0${root}`, () =>
    readdirSync(resolve(root, "demo"), { withFileTypes: true })
      .filter(
        (entry) =>
          entry.isDirectory() &&
          existsSync(resolve(root, "demo", entry.name, "package.json")),
      )
      .map((entry) => ({
        directory: `demo/${entry.name}`,
        ...json(resolve(root, "demo", entry.name, "package.json")),
      })),
  );

export const packageByName = (root, name) => {
  const pkg = workspacePackages(root).find(
    (entry) => entry.name === name || entry.directory === `demo/${name}`,
  );
  if (!pkg)
    throw new Error(
      `unknown workspace package ${name}; use contrib locate to find its declared name`,
    );
  return pkg;
};

export const packageClosure = (root, name) => {
  const packages = workspacePackages(root);
  const found = new Map();
  const visit = (pkg) => {
    if (found.has(pkg.name)) return;
    found.set(pkg.name, pkg);
    for (const dependency of Object.keys({
      ...pkg.dependencies,
      ...pkg.devDependencies,
    })) {
      const next = packages.find((entry) => entry.name === dependency);
      if (next) visit(next);
    }
  };
  visit(packageByName(root, name));
  return [...found.values()];
};

// Runtime imports consume emitted workspace artifacts. Unlike the source
// closure, their dependency graph must be acyclic and built in dependency order.
export const runtimeBuildClosure = (root, name) => {
  const packages = workspacePackages(root);
  const visited = new Set();
  const visiting = new Set();
  const ordered = [];
  const visit = (pkg) => {
    if (visited.has(pkg.name)) return;
    if (visiting.has(pkg.name))
      throw new Error(`runtime build dependency cycle at ${pkg.name}`);
    visiting.add(pkg.name);
    for (const dependency of Object.keys(pkg.dependencies ?? {}).sort()) {
      const next = packages.find((entry) => entry.name === dependency);
      if (next) visit(next);
    }
    visiting.delete(pkg.name);
    visited.add(pkg.name);
    if (pkg.scripts?.build) ordered.push(pkg);
  };
  visit(packageByName(root, name));
  return ordered;
};

const MODULE_SPECIFIER =
  /(?:\bfrom|\bimport|\brequire)\s*\(?\s*["'`]([^"'`\s]+)["'`]/gu;
const SCRIPT_SOURCE = /\.[cm]?[jt]sx?$/u;
const BUILD_CONFIGS = [
  "tsup.config.ts",
  "tsup.config.mts",
  "tsup.config.cts",
  "tsup.config.js",
  "tsup.config.mjs",
  "tsup.config.cjs",
  "tsup.config.json",
];
const packageText = (root, pkg, path) => {
  const absolute = resolve(root, pkg.directory, path);
  return existsSync(absolute) ? readFileSync(absolute, "utf8") : "";
};
const buildConfigText = (root, pkg) =>
  BUILD_CONFIGS.map((path) => packageText(root, pkg, path)).join("\n");

// tsup externalizes `dependencies` only, so a runtime dependency's dist stays
// a separate module whose bytes the stamp already binds. Any other workspace
// package a build source names (a devDependency, or one reached through the
// `midgard-source` condition) can be inlined from its dist, which no source
// digest covers. Those dists join the compiled dependencies.
export const inlinedWorkspacePackages = (root, name) =>
  scoped(`inlined\0${root}\0${name}`, () => {
    const packages = workspacePackages(root);
    const self = packageByName(root, name);
    const throughSource = buildConfigText(root, self).includes(
      "midgard-source",
    );
    const named = new Set();
    const scanned = new Set();
    const scan = (pkg) => {
      if (scanned.has(pkg.name)) return;
      scanned.add(pkg.name);
      for (const file of filesUnder(resolve(root, pkg.directory, "src"))) {
        if (!SCRIPT_SOURCE.test(file)) continue;
        for (const [, specifier] of readFileSync(file, "utf8").matchAll(
          MODULE_SPECIFIER,
        )) {
          const target = packages.find(
            (entry) =>
              specifier === entry.name ||
              specifier.startsWith(`${entry.name}/`),
          );
          if (!target || target.name === self.name) continue;
          named.add(target.name);
          if (throughSource) scan(target);
        }
      }
    };
    scan(self);
    const runtime = new Set(
      runtimeBuildClosure(root, name).map((pkg) => pkg.name),
    );
    return [...named].filter((entry) => !runtime.has(entry)).sort();
  });

export const compiledDependencies = (root, name) => {
  const self = packageByName(root, name);
  const inlined = inlinedWorkspacePackages(root, name)
    .map((entry) => packageByName(root, entry))
    .filter((pkg) => pkg.scripts?.build);
  return [
    ...runtimeBuildClosure(root, name).filter((pkg) => pkg.name !== self.name),
    ...inlined,
  ].map((pkg) => ({
    name: pkg.name,
    outputs: outputIdentity(root, `${pkg.directory}/dist`),
  }));
};

// A build can read inputs only the package's own facts reveal. Report every
// one the input closure cannot bind, so freshness fails closed on it.
export const unboundBuildInputs = (root, name) => {
  const self = packageByName(root, name);
  const closure = packageClosure(root, name);
  const declared = new Set(closure.map((pkg) => pkg.name));
  const reasons = inlinedWorkspacePackages(root, name)
    .filter((entry) => !declared.has(entry))
    .map(
      (entry) =>
        `build sources name workspace package ${entry}, which ${self.name} does not declare`,
    );
  const tsconfig = resolve(root, self.directory, "tsconfig.json");
  if (existsSync(tsconfig)) {
    let include = [];
    try {
      include = json(tsconfig).include ?? [];
    } catch {
      reasons.push(`${self.directory}/tsconfig.json is not plain JSON`);
    }
    const owned = closure.map((pkg) => resolve(root, pkg.directory));
    for (const entry of include) {
      const target = resolve(root, self.directory, entry);
      if (
        !owned.some(
          (directory) =>
            target === directory || target.startsWith(`${directory}${sep}`),
        )
      )
        reasons.push(
          `${self.directory}/tsconfig.json includes ${entry} outside the input closure`,
        );
    }
  }
  return reasons;
};

// A recipe sees its whole environment, but it can only act on the variables
// it names: shell expansions in the recipe, and process.env reads in its tsup
// config and the node scripts it runs. Their values (hashed, since dist is
// copied around) are bound; a recipe that reads the environment some other
// way cannot be bound and is never fresh.
export const buildEnvironment = (root, name, env = process.env) => {
  const pkg = packageByName(root, name);
  const recipe = pkg.scripts?.["build:contrib-raw"] ?? "";
  const programs = [
    buildConfigText(root, pkg),
    ...[...recipe.matchAll(/\bnode\s+([^\s&|;]+\.[cm]?js)\b/gu)].map(
      ([, path]) => packageText(root, pkg, path),
    ),
  ];
  const names = new Set(
    [...recipe.matchAll(/\$\{?([A-Za-z_][A-Za-z0-9_]*)/gu)].map(
      ([, variable]) => variable,
    ),
  );
  for (const text of [recipe, ...programs]) {
    for (const [, variable] of text.matchAll(
      /process\.env\.([A-Za-z_$][\w$]*)/gu,
    ))
      names.add(variable);
    for (const [, variable] of text.matchAll(
      /process\.env\[\s*["']([^"']+)["']\s*\]/gu,
    ))
      names.add(variable);
  }
  const unnamed =
    /`|\$\(/u.test(recipe) ||
    [recipe, ...programs].some(
      (text) =>
        /process\.env(?!\.[A-Za-z_$]|\[\s*["'][^"']+["']\s*\])/u.test(text) ||
        /from\s+["'](?:node:)?process["']|\}\s*=\s*process\b/u.test(text),
    );
  return {
    unnamed,
    variables: Object.fromEntries(
      [...names]
        .sort()
        .map((variable) => [
          variable,
          env[variable] === undefined ? null : sha256(env[variable]),
        ]),
    ),
  };
};

// Conservative closure includes native sources, SQL, generators and fixtures,
// not merely src/. Test edits can cause an unnecessary rebuild, never a false
// fresh result. Missing optional inputs are represented in the digest.
export const inputIdentity = (root, name) =>
  scoped(`input\0${root}\0${name}`, () => uncachedInputIdentity(root, name));
const uncachedInputIdentity = (root, name) => {
  if (name === "@repository") {
    // Builds create ignored LaTeX/site outputs outside dist/. They are not
    // repository source changes. Include tracked inputs and new unignored
    // sources, with the same explicit exclusions as the source walker.
    const repositorySources = execFileSync(
      "git",
      ["ls-files", "-z", "--cached", "--others", "--exclude-standard"],
      { cwd: root, encoding: "utf8", maxBuffer: 32 * 1024 * 1024 },
    )
      .split("\0")
      .filter(
        (path) =>
          path &&
          sourceInput(root, path) &&
          !/onchain\/aiken\/plutus\.json(?:\.|$)/u.test(path),
      )
      .filter((path) => {
        const absolute = inside(root, path);
        if (!existsSync(absolute)) return false;
        const stats = lstatSync(absolute);
        if (stats.isSymbolicLink())
          throw new Error(`symlink in repository input: ${path}`);
        return stats.isFile(); // declared Git submodule directories are not files
      });
    const identity = hashFiles(root, repositorySources);
    return {
      ...identity,
      node: process.version,
      platform: process.platform,
      arch: process.arch,
      sha256: sha256(
        JSON.stringify([
          identity.sha256,
          process.version,
          process.platform,
          process.arch,
        ]),
      ),
    };
  }
  const directories = packageClosure(root, name).map((pkg) => pkg.directory);
  const paths = directories.flatMap((path) => filesUnder(resolve(root, path)));
  for (const directory of [
    "demo/patches",
    "demo/vendor",
    "demo/scripts",
    "config/deployments",
    "onchain/aiken/lib",
    "onchain/aiken/validators",
    "onchain/aiken/env",
    "onchain/aiken/scripts",
    "scripts/contrib",
    "scripts/preflight",
    "scripts/lib",
    "scripts/bin",
    ".github/workflows",
  ]) {
    paths.push(...filesUnder(resolve(root, directory)));
  }
  const shared = [
    "demo/package.json",
    "demo/pnpm-lock.yaml",
    // What is installed, not only what the lockfile asks for: a build run
    // before `pnpm install` caught up compiled against the old packages.
    "demo/node_modules/.pnpm/lock.yaml",
    "demo/pnpm-workspace.yaml",
    "demo/tsconfig.json",
    "scripts/contrib.mjs",
    "scripts/pnpm.mjs",
    "scripts/bin/pnpm",
    ".agents/skills/regenerating-goldens-and-ledgers/scripts/channels.json",
    "onchain/aiken/aiken.toml",
    "onchain/aiken/aiken.lock",
    "onchain/aiken/plutus.json",
    "onchain/aiken/plutus.json.deployment.json",
  ];
  const missing = shared.filter((path) => !existsSync(resolve(root, path)));
  paths.push(
    ...shared
      .filter((path) => !missing.includes(path))
      .map((path) => resolve(root, path)),
  );
  const identity = hashFiles(root, paths);
  return {
    ...identity,
    sha256: sha256(
      JSON.stringify([
        identity.sha256,
        missing,
        process.version,
        process.platform,
        process.arch,
      ]),
    ),
    missing,
    node: process.version,
    platform: process.platform,
    arch: process.arch,
  };
};

export const outputIdentity = (root, directory) =>
  scoped(`output\0${root}\0${directory}`, () =>
    uncachedOutputIdentity(root, directory),
  );
const uncachedOutputIdentity = (root, directory) =>
  hashFiles(
    root,
    filesUnder(resolve(root, directory), { outputs: true }).filter(
      (path) =>
        // The watcher Go owner shares dist, but has its own binary receipt.
        // Other packages' directories named native remain compiled outputs.
        !Object.entries(NATIVE_RECIPES).some(
          ([name, recipe]) =>
            path === resolve(root, "demo", name, recipe.output),
        ) &&
        !path.endsWith(".contrib-build-v1.json") &&
        !path.endsWith(".contrib-native-v1.json"),
    ),
  );
