import { existsSync, readFileSync, realpathSync, statSync } from "node:fs";
import { builtinModules, createRequire } from "node:module";
import { dirname, relative, resolve, sep } from "node:path";

import {
  BUILD_CONFIGS,
  MODULE_SPECIFIER,
  SCRIPT_SOURCE,
  SHARED_INPUT_DIRECTORIES,
  SHARED_INPUT_FILES,
  filesUnder,
  inlinedWorkspacePackages,
  packageByName,
  packageClosure,
  scoped,
  sha256,
  sourceInput,
} from "./files.mjs";

// Freshness can only skip a build whose every input the stamp binds. This
// module decides, before a build, whether that can be proven at all: the
// recipe must be a shape the guard understands, the code it runs must name
// every variable it reads, and every file it points at must be a closure
// input. Anything unrecognised is a refusal, so the build is never fresh.
// After a build, the read trace (build-trace.cjs) is checked the same way.

const posix = (path) => path.split(sep).join("/");
const within = (path, directory) =>
  path === directory || path.startsWith(`${directory}${sep}`);
const realRoot = (root) => realpathSync(root);
const realOrSelf = (path) => {
  try {
    return realpathSync(path);
  } catch {
    return resolve(path);
  }
};

// A path whose contents the input digest binds: a source of a package in the
// build's closure, or a shared repository input. Excluded outputs (dist,
// coverage, ...) of those packages are not.
export const closurePath = (root, name, absolute) =>
  scoped(`closure-path\0${root}\0${name}\0${absolute}`, () => {
    const { base, owners } = scoped(`closure\0${root}\0${name}`, () => ({
      base: realRoot(root),
      owners: [
        ...SHARED_INPUT_DIRECTORIES,
        ...packageClosure(root, name).map((pkg) => pkg.directory),
      ],
    }));
    const path = realOrSelf(absolute);
    if (!within(path, base) || path === base) return false;
    const rel = posix(relative(base, path));
    if (SHARED_INPUT_FILES.includes(rel)) return true;
    const owner = owners.find(
      (directory) => rel === directory || rel.startsWith(`${directory}/`),
    );
    return owner !== undefined && sourceInput(base, rel);
  });

// The installed package store, bound by the installed lock record: a path in
// the checkout's demo/node_modules, or in the store entry that one of its
// .pnpm links resolves to (a linked store resolves elsewhere).
const STORE_ENTRY =
  /^(.*[\\/]node_modules[\\/]\.pnpm[\\/])([^\\/]+)(?:[\\/]|$)/u;
export const installedPath = (root, absolute) => {
  const path = realOrSelf(absolute);
  const modules = resolve(realRoot(root), "demo/node_modules");
  if (within(path, modules)) return true;
  const entry = STORE_ENTRY.exec(path);
  if (!entry) return false;
  const linked = resolve(modules, ".pnpm", entry[2]);
  return (
    existsSync(linked) &&
    realOrSelf(linked) === resolve(entry[1], entry[2]) &&
    within(path, realOrSelf(linked))
  );
};

// --- recipe ----------------------------------------------------------------

const EXPANSION =
  /^\$(?:([A-Za-z_][A-Za-z0-9_]*)|\{([A-Za-z_][A-Za-z0-9_]*)(?::?-([^}$`"'\\]*))?\})/u;
const PLAIN = /[A-Za-z0-9_\-./=:,*@+%]/u;
// Node flags that load or evaluate code the guard does not scan, or read an
// environment file.
const REFUSED_NODE_FLAG =
  /^--(?:require|import|loader|experimental-loader|eval|print|env-file|env-file-if-exists|experimental-config-file|run|watch|watch-path|test|interactive)(?:=|$)/u;
const NODE_FLAG = /^--[a-z0-9][a-z0-9-]*(?:=[^\0]*)?$/u;
// tsup flags that run commands, load another config, or point esbuild at a
// tsconfig the guard has not resolved.
const REFUSED_TSUP_FLAG =
  /^(?:--onSuccess|--config|-c|--watch|-w|--tsconfig|--ignore-watch)(?:=|$)/u;
const NODE_SCRIPT = /\.(?:mjs|cjs|js)$/u;

// Recipes are `&&` sequences of `tsup ...` and `node [flags] script.mjs ...`
// commands, each optionally preceded by NAME=value assignments. Words may use
// single or double quotes and $NAME, ${NAME} or ${NAME:-literal} expansions
// of named variables. Nothing else is parsed, so nothing else can be fresh.
export const parseRecipe = (recipe) => {
  const names = new Set();
  const commands = [];
  const refuse = (reason) => ({
    names,
    commands: [],
    scripts: [],
    reasons: [`build recipe ${reason}`],
  });
  let command = [];
  let word;
  const open = () => (word ??= { text: "", literal: true });
  const close = () => {
    if (word) command.push(word);
    word = undefined;
  };
  const expand = (index) => {
    const match = EXPANSION.exec(recipe.slice(index));
    if (!match) return 0;
    names.add(match[1] ?? match[2]);
    open().literal = false;
    word.text += `\${${match[1] ?? match[2]}}`;
    return match[0].length;
  };
  for (let index = 0; index < recipe.length; ) {
    const character = recipe[index];
    if (character === " " || character === "\t") {
      close();
      index += 1;
    } else if (recipe.startsWith("&&", index)) {
      close();
      if (command.length === 0) return refuse("has an empty command");
      commands.push(command);
      command = [];
      index += 2;
    } else if (character === "'") {
      const end = recipe.indexOf("'", index + 1);
      if (end < 0) return refuse("has an unterminated quote");
      open().text += recipe.slice(index + 1, end);
      index = end + 1;
    } else if (character === '"') {
      open();
      index += 1;
      while (index < recipe.length && recipe[index] !== '"') {
        if (recipe[index] === "$") {
          const length = expand(index);
          if (!length) return refuse(`uses a shell expansion it cannot name`);
          index += length;
        } else if (/[`\\]/u.test(recipe[index]))
          return refuse(`uses ${recipe[index]} inside double quotes`);
        else {
          word.text += recipe[index];
          index += 1;
        }
      }
      if (index >= recipe.length) return refuse("has an unterminated quote");
      index += 1;
    } else if (character === "$") {
      const length = expand(index);
      if (!length) return refuse(`uses a shell expansion it cannot name`);
      index += length;
    } else if (PLAIN.test(character)) {
      open().text += character;
      index += 1;
    } else return refuse(`uses shell syntax ${JSON.stringify(character)}`);
  }
  close();
  if (command.length === 0) return refuse("has an empty command");
  commands.push(command);

  const reasons = [];
  const scripts = [];
  const parsed = [];
  for (const words of commands) {
    let index = 0;
    const assignments = [];
    while (
      index < words.length &&
      /^[A-Za-z_][A-Za-z0-9_]*=/u.test(words[index].text)
    ) {
      const [variable, ...value] = words[index].text.split("=");
      assignments.push({ variable, value: value.join("=") });
      index += 1;
    }
    for (const { variable, value } of assignments)
      if (variable === "NODE_OPTIONS")
        for (const flag of value.split(/\s+/u).filter(Boolean))
          if (
            flag !== "${NODE_OPTIONS}" &&
            (!NODE_FLAG.test(flag) || REFUSED_NODE_FLAG.test(flag))
          )
            reasons.push(`build recipe passes node option ${flag}`);
    const program = words[index];
    const args = words.slice(index + 1);
    if (!program || !program.literal)
      reasons.push("build recipe runs a command it does not name literally");
    else if (program.text === "tsup") {
      for (const arg of args)
        if (REFUSED_TSUP_FLAG.test(arg.text))
          reasons.push(`build recipe passes tsup ${arg.text.split("=")[0]}`);
    } else if (program.text === "node") {
      let at = 0;
      while (at < args.length && args[at].text.startsWith("-")) {
        const flag = args[at];
        if (
          !flag.literal ||
          !NODE_FLAG.test(flag.text) ||
          REFUSED_NODE_FLAG.test(flag.text)
        )
          reasons.push(`build recipe passes node ${flag.text.split("=")[0]}`);
        at += 1;
      }
      const script = args[at];
      if (!script || !script.literal || !NODE_SCRIPT.test(script.text))
        reasons.push(
          `build recipe runs node on ${script?.text ?? "nothing"}, not a .mjs, .cjs or .js script`,
        );
      else scripts.push(script.text);
    } else
      reasons.push(
        `build recipe runs ${program.text}, which the guard does not scan`,
      );
    parsed.push({ assignments, program: program?.text, args });
  }
  return { names, commands: parsed, scripts, reasons };
};

// --- modules the recipe runs -------------------------------------------------

const PROCESS = /\bprocess\b/gu;
const NAMED_PROCESS =
  /^process(?:\.env\.([A-Za-z_$][\w$]*)|\.env\[\s*["']([^"']+)["']\s*\]|\.(?:argv|stdout|stderr|exit|exitCode|cwd|nextTick|hrtime|on|once|emitWarning|platform|arch|version|versions|execPath|pid)\b)/u;
const OPAQUE =
  /\b(?:eval\s*\(|Function\s*\(|globalThis\b|createRequire\b|(?:import|require)\s*\(\s*(?!["'][^"'`$]*["']\s*\)))/u;
const RESOLVE_EXTENSIONS = [
  "",
  ".ts",
  ".mts",
  ".cts",
  ".tsx",
  ".js",
  ".mjs",
  ".cjs",
  ".json",
];
const resolveLocal = (from, specifier) => {
  const base = resolve(dirname(from), specifier);
  const candidates = [
    ...RESOLVE_EXTENSIONS.map((extension) => `${base}${extension}`),
    ...RESOLVE_EXTENSIONS.slice(1).map((extension) =>
      resolve(base, `index${extension}`),
    ),
    // TypeScript sources import their emitted name.
    base.replace(/\.(m|c)?js$/u, ".$1ts"),
  ];
  return candidates.find(
    (candidate) => existsSync(candidate) && statSync(candidate).isFile(),
  );
};
const builtin = (specifier) =>
  specifier.startsWith("node:") ||
  builtinModules.includes(specifier) ||
  builtinModules.includes(specifier.split("/")[0]);

// Follow every module a config or script loads from the checkout. Each must
// be a closure input, may import only Node builtins (and tsup, for the
// config), and may read the environment only by literal name.
const scanModules = (root, name, entries, packages) => {
  const names = new Set();
  const reasons = [];
  const seen = new Set();
  const base = realRoot(root);
  const label = (path) => posix(relative(base, realOrSelf(path)));
  const visit = (file) => {
    if (seen.has(file)) return;
    seen.add(file);
    if (!closurePath(root, name, file)) {
      reasons.push(`build code ${label(file)} is outside the input closure`);
      return;
    }
    if (!SCRIPT_SOURCE.test(file)) return;
    const text = readFileSync(file, "utf8");
    if (OPAQUE.test(text))
      reasons.push(
        `build code ${label(file)} loads code or reads globals the guard cannot follow`,
      );
    for (const match of text.matchAll(PROCESS)) {
      const named = NAMED_PROCESS.exec(text.slice(match.index));
      if (!named) {
        reasons.push(
          `build code ${label(file)} uses process in a way that names no variable`,
        );
        break;
      }
      if (named[1] ?? named[2]) names.add(named[1] ?? named[2]);
    }
    for (const [, specifier] of text.matchAll(MODULE_SPECIFIER)) {
      if (specifier.startsWith(".") || specifier.startsWith("/")) {
        const target = resolveLocal(file, specifier);
        if (!target)
          reasons.push(
            `build code ${label(file)} imports ${specifier}, which does not resolve`,
          );
        else visit(target);
      } else if (!builtin(specifier) && !packages.includes(specifier))
        reasons.push(
          `build code ${label(file)} imports ${specifier}, which the guard does not scan`,
        );
    }
  };
  for (const entry of entries) visit(entry);
  return { names, reasons };
};

// --- tsconfig ---------------------------------------------------------------

// esbuild and the declaration build read the package tsconfig and its whole
// `extends` chain outside Node, so the chain and every path it points at
// must be closure inputs or installed packages.
const tsconfigRefusals = (root, name) => {
  const self = packageByName(root, name);
  const base = realRoot(root);
  const reasons = [];
  const bound = (path) =>
    closurePath(root, name, path) || installedPath(root, path);
  const seen = new Set();
  const visit = (file) => {
    if (seen.has(file)) return;
    seen.add(file);
    const label = posix(relative(base, file));
    if (!bound(file)) {
      reasons.push(`tsconfig ${label} is outside the input closure`);
      return;
    }
    let config;
    try {
      config = JSON.parse(readFileSync(file, "utf8"));
    } catch {
      reasons.push(`tsconfig ${label} is not plain JSON`);
      return;
    }
    const directory = dirname(file);
    const check = (from, entry, verb) => {
      if (typeof entry !== "string") {
        reasons.push(`tsconfig ${label} ${verb} a non-path entry`);
        return;
      }
      const prefix = entry.replace(/[*?{[].*$/u, "") || ".";
      if (!bound(resolve(from, prefix)))
        reasons.push(
          `tsconfig ${label} ${verb} ${entry} outside the input closure`,
        );
    };
    for (const entry of config.include ?? [])
      check(directory, entry, "includes");
    for (const entry of config.files ?? []) check(directory, entry, "lists");
    for (const reference of config.references ?? [])
      check(directory, reference?.path, "references");
    const options = config.compilerOptions ?? {};
    if (options.baseUrl !== undefined)
      check(directory, options.baseUrl, "sets baseUrl to");
    const pathBase =
      typeof options.baseUrl === "string"
        ? resolve(directory, options.baseUrl)
        : directory;
    for (const targets of Object.values(options.paths ?? {}))
      for (const target of [targets].flat())
        check(pathBase, target, "maps a path to");
    for (const entry of options.typeRoots ?? [])
      check(directory, entry, "reads types from");
    for (const parent of [config.extends ?? []].flat()) {
      let target;
      if (typeof parent !== "string") target = undefined;
      else if (parent.startsWith(".") || parent.startsWith("/")) {
        const path = resolve(directory, parent);
        target = [path, `${path}.json`].find((candidate) =>
          existsSync(candidate),
        );
      } else
        try {
          target = createRequire(file).resolve(parent);
        } catch {
          try {
            target = createRequire(file).resolve(`${parent}/tsconfig.json`);
          } catch {
            target = undefined;
          }
        }
      if (!target)
        reasons.push(
          `tsconfig ${label} extends ${parent}, which does not resolve`,
        );
      else visit(realOrSelf(target));
    }
  };
  // tsup, like tsc, takes the nearest tsconfig.json above the package.
  let directory = resolve(root, self.directory);
  for (;;) {
    const candidate = resolve(directory, "tsconfig.json");
    if (existsSync(candidate)) {
      visit(realOrSelf(candidate));
      break;
    }
    if (directory === dirname(directory)) break;
    directory = dirname(directory);
  }
  return reasons;
};

// --- sources ----------------------------------------------------------------

// A relative import that leaves the closure reaches a file no digest binds.
// The read trace catches it after any build; this names it beforehand for
// the package's own sources and the workspace sources it inlines.
const escapingSourceImports = (root, name) => {
  const self = packageByName(root, name);
  const base = realRoot(root);
  const reasons = [];
  for (const pkg of [
    self,
    ...inlinedWorkspacePackages(root, name).map((entry) =>
      packageByName(root, entry),
    ),
  ])
    for (const file of filesUnder(resolve(root, pkg.directory, "src"))) {
      if (!SCRIPT_SOURCE.test(file)) continue;
      for (const [, specifier] of readFileSync(file, "utf8").matchAll(
        MODULE_SPECIFIER,
      ))
        if (
          /^\.\.?\//u.test(specifier) &&
          !closurePath(root, name, resolve(dirname(file), specifier))
        )
          reasons.push(
            `${posix(relative(base, file))} imports ${specifier} outside the input closure of ${self.name}`,
          );
    }
  return reasons;
};

// --- verdict ----------------------------------------------------------------

const recipeFacts = (root, name) =>
  scoped(`recipe\0${root}\0${name}`, () => {
    const pkg = packageByName(root, name);
    const recipe = parseRecipe(pkg.scripts?.["build:contrib-raw"] ?? "");
    const configs = BUILD_CONFIGS.map((path) =>
      resolve(root, pkg.directory, path),
    ).filter((path) => existsSync(path));
    const reasons = [...recipe.reasons];
    const scripts = recipe.scripts.map((path) =>
      resolve(root, pkg.directory, path),
    );
    for (const script of scripts)
      if (!existsSync(script))
        reasons.push(
          `build recipe runs ${posix(relative(realRoot(root), script))}, which does not exist`,
        );
    for (const config of configs) {
      const text = readFileSync(config, "utf8");
      // Options that run commands or redirect esbuild's tsconfig.
      for (const option of ["onSuccess", "tsconfig"])
        if (text.includes(option))
          reasons.push(
            `${posix(relative(realRoot(root), config))} sets ${option}, which the guard does not follow`,
          );
    }
    const configScan = scanModules(root, name, configs, ["tsup"]);
    const scriptScan = scanModules(
      root,
      name,
      scripts.filter((script) => existsSync(script)),
      [],
    );
    return {
      names: new Set([
        ...recipe.names,
        ...configScan.names,
        ...scriptScan.names,
      ]),
      reasons: [...reasons, ...configScan.reasons, ...scriptScan.reasons],
    };
  });

// Every reason this package's dist can never be proven fresh; empty when the
// static facts allow it. A build still has to pass the read trace to be
// stamped at all.
export const buildRefusals = (root, name) =>
  scoped(`refusals\0${root}\0${name}`, () => {
    const self = packageByName(root, name);
    const declared = new Set(packageClosure(root, name).map((pkg) => pkg.name));
    return [
      ...inlinedWorkspacePackages(root, name)
        .filter((entry) => !declared.has(entry))
        .map(
          (entry) =>
            `build sources name workspace package ${entry}, which ${self.name} does not declare`,
        ),
      ...recipeFacts(root, name).reasons,
      ...tsconfigRefusals(root, name),
      ...escapingSourceImports(root, name),
    ];
  });

// The variables the recipe and the code it runs name, each bound by a digest
// of its value (dist is copied around), or null when unset.
export const buildEnvironment = (root, name, env = process.env) => ({
  variables: Object.fromEntries(
    [...recipeFacts(root, name).names]
      .sort()
      .map((variable) => [
        variable,
        env[variable] === undefined ? null : sha256(env[variable]),
      ]),
  ),
});

// --- read trace -------------------------------------------------------------

export const BUILD_TRACE = "midgard-contrib-build-trace/v1";
export const TRACER = new URL("./build-trace.cjs", import.meta.url).pathname;

// Reads the build made that its stamp would not bind. Allowed: closure
// inputs, the installed store, this package's and its compiled
// dependencies' emitted files, and files the build itself wrote.
export const unboundReads = (root, name, records, { dependencies }) => {
  const self = packageByName(root, name);
  const base = realRoot(root);
  const written = new Set(
    records.filter(([kind]) => kind === "write").map(([, path]) => path),
  );
  const emitted = [
    self.directory,
    ...dependencies.map((entry) => packageByName(root, entry.name).directory),
  ].map((directory) => resolve(base, directory, "dist"));
  const reasons = [];
  for (const [kind, path] of records) {
    if (kind === "write" || kind === "virtual") continue;
    if (kind !== "read") {
      reasons.push(`build used ${path}, which is not traced`);
      continue;
    }
    if (written.has(path) || written.has(realOrSelf(path))) continue;
    if (!existsSync(path)) {
      reasons.push(`build read ${path}, which no longer exists`);
      continue;
    }
    const real = realOrSelf(path);
    if (
      closurePath(root, name, real) ||
      installedPath(root, real) ||
      emitted.some((directory) => within(real, directory))
    )
      continue;
    reasons.push(
      `build read ${within(real, base) ? posix(relative(base, real)) : real}, which its input closure does not bind`,
    );
  }
  return reasons;
};
