#!/usr/bin/env node
// Packs the npm publish set exactly as `pnpm publish` would and checks the
// tarballs before anything is published: the packed manifest must name only
// published dependencies, every export must be the registry form of its
// workspace export (publishConfig) and resolve to a shipped file, each tarball
// must carry the LICENSE and any vendored package import byte for byte (see
// pack-files.mjs), no test, fixture or secret may ride along, and every export
// must load by package name through ESM import, CommonJS require and NodeNext
// types.
// Nothing here publishes; the checked tarballs and their hashes are the input
// to the manual publish steps in docs/deployment-release-tooling.md.
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import {
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  realpathSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { createRequire } from "node:module";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { treeDifferences, vendoredImports } from "./pack-files.mjs";

export const PUBLISH_SET = Object.freeze([
  { name: "@al-ft/midgard-core", dir: "demo/midgard-core" },
  { name: "@al-ft/midgard-validation", dir: "demo/midgard-validation" },
  { name: "@al-ft/midgard-sdk", dir: "demo/midgard-sdk" },
  { name: "@al-ft/lucid-midgard", dir: "demo/lucid-midgard" },
]);

/** The workspace-only condition that points at TypeScript sources. */
export const SOURCE_CONDITION = "midgard-source";

const DEPENDENCY_FIELDS = [
  "dependencies",
  "peerDependencies",
  "optionalDependencies",
];
const EXACT_VERSION = /^[0-9]+\.[0-9]+\.[0-9]+(?:-[0-9A-Za-z.-]+)?$/u;
const LOCAL_OR_REMOTE_SPECIFIER =
  /^(?:workspace|file|link|portal|git|git\+[a-z]+|github|gitlab|bitbucket|https?):/u;

const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");
const stripDot = (target) => target.replace(/^\.\//u, "");

/** Why a dependency specifier cannot be installed from the registry, or null. */
export const unpublishableSpecifier = (specifier) => {
  if (typeof specifier !== "string" || specifier.trim() === "")
    return "is empty";
  if (LOCAL_OR_REMOTE_SPECIFIER.test(specifier) || specifier.includes("://"))
    return `is a local, git or URL specifier (${specifier})`;
  if (/^[^@/\s]+\/[^/\s]+(?:#.*)?$/u.test(specifier))
    return `is a GitHub shorthand (${specifier})`;
  if (["latest", "*", "x", "next"].includes(specifier))
    return `is an unpinned range (${specifier})`;
  return null;
};

const subpathMap = (exportsField) => {
  if (exportsField === undefined) return {};
  const isSubpathMap =
    typeof exportsField === "object" &&
    exportsField !== null &&
    Object.keys(exportsField).every((key) => key.startsWith("."));
  return isSubpathMap ? exportsField : { ".": exportsField };
};

/** Flattens an `exports` map into one entry per subpath and condition chain. */
export const exportEntries = (exportsField) =>
  Object.entries(subpathMap(exportsField)).map(([subpath, value]) => {
    const targets = [];
    const walk = (node, conditions) => {
      if (typeof node === "string") targets.push({ conditions, target: node });
      else if (Array.isArray(node))
        node.forEach((item) => walk(item, conditions));
      else if (node !== null && typeof node === "object")
        for (const [condition, child] of Object.entries(node))
          walk(child, [...conditions, condition]);
    };
    walk(value, []);
    return { subpath, targets };
  });

/** Targets a consumer can reach: everything not behind the source condition. */
export const publishedTargets = (entry) =>
  entry.targets.filter(
    ({ conditions }) => !conditions.includes(SOURCE_CONDITION),
  );

/** Why a packed path must not be published, or null. */
export const forbiddenPackedPath = (path) => {
  if (/(?:^|\/)(?:tests?|__tests__|test-support)(?:\/|$)/u.test(path))
    return "test path";
  if (/(?:^|\/)(?:fixtures?|__fixtures__|goldens?)(?:\/|$)/u.test(path))
    return "fixture path";
  if (/\.(?:test|spec)\.(?:d\.)?[cm]?[jt]sx?(?:\.map)?$/u.test(path))
    return "test file";
  if (/(?:^|\/)\.env(?:\.|$)/u.test(path) || /(?:^|\/)\.npmrc$/u.test(path))
    return "environment or registry credentials file";
  if (/(?:^|\/)\.[^/]+$/u.test(path)) return "hidden file";
  if (/\.(?:pem|skey|key|p12|pfx)$/iu.test(path)) return "key file";
  return null;
};

const SECRET_CONTENT = [
  [/-----BEGIN [A-Z ]*PRIVATE KEY-----/u, "PEM private key"],
  [/SigningKey(?:Shelley|Byron)?_?ed25519/u, "Cardano signing-key envelope"],
  [/"type"\s*:\s*"[A-Za-z]*SigningKey/u, "Cardano signing-key envelope"],
  [
    /\b(?:ed25519e?_sk|addr_xsk|root_xsk|acct_xsk|xprv)1[0-9a-z]{20,}/u,
    "bech32 private key",
  ],
];

/** The kind of secret `text` appears to carry, or null. */
export const secretContentReason = (text) => {
  for (const [pattern, reason] of SECRET_CONTENT)
    if (pattern.test(text)) return reason;
  return null;
};

/** The registry form of one workspace `exports` entry, or undefined when it
 * resolves only under the source condition. The source condition is dropped
 * and `types` moves under each runtime condition, so a CommonJS consumer
 * reads the `.d.cts` declarations the build emits beside each `.cjs` file. */
export const publishedExport = (value) => {
  if (value === null || typeof value !== "object" || Array.isArray(value))
    return value;
  const { [SOURCE_CONDITION]: _source, types, ...runtime } = value;
  if (Object.keys(runtime).length === 0) return undefined;
  if (types === undefined) return runtime;
  return Object.fromEntries(
    Object.entries(runtime).map(([condition, target]) => [
      condition,
      typeof target === "string"
        ? {
            types:
              condition === "require"
                ? types.replace(/\.d\.ts$/u, ".d.cts")
                : types,
            default: target,
          }
        : target,
    ]),
  );
};

/** The registry form of a workspace `exports` map (`publishConfig.exports`). */
export const publishedExports = (exportsField) =>
  Object.fromEntries(
    Object.entries(subpathMap(exportsField))
      .map(([subpath, value]) => [subpath, publishedExport(value)])
      .filter(([, value]) => value !== undefined),
  );

/**
 * Problems a registry consumer would hit with this packed manifest and file
 * list. With `workspaceExports`, every packed export must be the registry
 * form of the workspace export of the same subpath.
 */
export const packedManifestProblems = (
  manifest,
  files,
  publishNames,
  workspaceExports,
) => {
  const problems = [];
  const shipped = new Set(files);
  if (manifest.private === true) problems.push("package is private");
  if (!EXACT_VERSION.test(manifest.version ?? ""))
    problems.push(`version ${manifest.version} is not an exact version`);
  if (typeof manifest.license !== "string" || manifest.license === "")
    problems.push("license is missing");
  if (!files.some((file) => /^LICEN[CS]E(?:\.[a-z]+)?$/iu.test(file)))
    problems.push("LICENSE file is not in the tarball");
  if (manifest.repository === undefined) problems.push("repository is missing");
  for (const field of DEPENDENCY_FIELDS)
    for (const [name, specifier] of Object.entries(manifest[field] ?? {})) {
      const reason = unpublishableSpecifier(specifier);
      if (reason !== null) problems.push(`${field}.${name} ${reason}`);
      if (name.startsWith("@al-ft/") && !publishNames.has(name))
        problems.push(
          `${field}.${name} is a workspace package outside the publish set`,
        );
    }
  for (const field of ["main", "module", "types", "typings"])
    if (
      typeof manifest[field] === "string" &&
      !shipped.has(stripDot(manifest[field]))
    )
      problems.push(`${field} ${manifest[field]} is not in the tarball`);
  const entries = exportEntries(manifest.exports);
  if (entries.length === 0) problems.push("exports map is missing");
  for (const entry of entries) {
    const reachable = publishedTargets(entry);
    if (reachable.length === 0)
      problems.push(
        `export ${entry.subpath} resolves only under the ${SOURCE_CONDITION} condition`,
      );
    else if (reachable.length !== entry.targets.length)
      problems.push(
        `export ${entry.subpath} keeps the workspace-only ${SOURCE_CONDITION} condition`,
      );
    for (const { conditions, target } of reachable)
      if (!shipped.has(stripDot(target)))
        problems.push(
          `export ${entry.subpath} [${conditions.join(">") || "default"}] -> ${target} is not in the tarball`,
        );
  }
  if (workspaceExports !== undefined) {
    const expected = publishedExports(workspaceExports);
    const packed = manifest.exports ?? {};
    for (const subpath of new Set([
      ...Object.keys(expected),
      ...Object.keys(packed),
    ]))
      if (JSON.stringify(packed[subpath]) !== JSON.stringify(expected[subpath]))
        problems.push(
          expected[subpath] === undefined
            ? `export ${subpath} has no workspace export`
            : `export ${subpath} is not the registry form of its workspace export; publish it as ${JSON.stringify(expected[subpath])}`,
        );
  }
  for (const [specifier, target] of Object.entries(manifest.imports ?? {}))
    for (const { target: path } of exportEntries(target)[0]?.targets ?? [])
      if (!shipped.has(stripDot(path)))
        problems.push(`imports ${specifier} -> ${path} is not in the tarball`);
  for (const file of files) {
    const reason = forbiddenPackedPath(file);
    if (reason !== null) problems.push(`${file} is a ${reason}`);
  }
  return problems;
};

const listFiles = (root, base = root) =>
  readdirSync(root, { withFileTypes: true }).flatMap((entry) => {
    const path = join(root, entry.name);
    return entry.isDirectory()
      ? listFiles(path, base)
      : [relative(base, path).split("\\").join("/")];
  });

/** Secret-looking content in an extracted package directory. */
export const packedSecretProblems = (packageRoot) =>
  listFiles(packageRoot).flatMap((file) => {
    const reason = secretContentReason(
      readFileSync(join(packageRoot, file), "latin1"),
    );
    return reason === null ? [] : [`${file} contains a ${reason}`];
  });

const rebased = (node, directory) =>
  typeof node === "string"
    ? `./${directory}/${stripDot(node)}`
    : Array.isArray(node)
      ? node.map((item) => rebased(item, directory))
      : node !== null && typeof node === "object"
        ? Object.fromEntries(
            Object.entries(node).map(([key, value]) => [
              key,
              rebased(value, directory),
            ]),
          )
        : node;

/**
 * Problems with the vendored package imports (see pack-files.mjs) of the
 * workspace package in `workspaceDir`, packed and extracted to `packageRoot`
 * with `manifest`: the tarball must carry the installed package byte for
 * byte, the packed `imports` entry must be that package's own export map
 * under its vendored directory, and every dependency of the vendored package
 * must be a dependency of the published one.
 */
export const vendoredImportProblems = (packageRoot, manifest, workspaceDir) =>
  vendoredImports(workspaceDir).flatMap(
    ({ specifier, dependency, installed, directory }) => {
      if (installed === undefined)
        return [`vendored ${dependency} is not installed in the workspace`];
      const problems = [];
      const vendored = JSON.parse(
        readFileSync(join(installed, "package.json"), "utf8"),
      );
      const expected = rebased(
        exportEntries(vendored.exports).length > 0
          ? subpathMap(vendored.exports)["."]
          : (vendored.main ?? "./index.js"),
        directory,
      );
      if (
        JSON.stringify(manifest.imports?.[specifier]) !==
        JSON.stringify(expected)
      )
        problems.push(
          `imports ${specifier} is not the export map of the vendored ${dependency}; publish it as ${JSON.stringify(expected)}`,
        );
      const copy = join(packageRoot, directory);
      if (!existsSync(copy))
        problems.push(`vendored ${dependency} is not in the tarball`);
      else
        for (const file of treeDifferences(installed, copy))
          problems.push(
            `${directory}/${file} differs from the installed ${dependency}`,
          );
      for (const name of Object.keys(vendored.dependencies ?? {}))
        if (manifest.dependencies?.[name] === undefined)
          problems.push(
            `vendored ${dependency} needs ${name}, which is not a dependency`,
          );
      return problems;
    },
  );

const loadScript = (mode, specifiers) =>
  mode === "import"
    ? `const failures = [];
for (const s of ${JSON.stringify(specifiers)}) {
  try { await import(s); } catch (e) { failures.push(s + ": " + (e?.message ?? e).split("\\n")[0]); }
}
console.log(JSON.stringify(failures));`
    : `const failures = [];
for (const s of ${JSON.stringify(specifiers)}) {
  try { require(s); } catch (e) { failures.push(s + ": " + (e?.message ?? e).split("\\n")[0]); }
}
console.log(JSON.stringify(failures));`;

/**
 * Loads every specifier through `mode` ("import" or "require") in a child
 * Node process rooted at `installRoot`, with no workspace conditions.
 */
export const loadFailures = (installRoot, mode, specifiers) => {
  if (specifiers.length === 0) return [];
  const result = spawnSync(
    process.execPath,
    [
      ...(mode === "import" ? ["--input-type=module"] : []),
      "-e",
      loadScript(mode, specifiers),
    ],
    {
      cwd: installRoot,
      encoding: "utf8",
      env: { PATH: process.env.PATH ?? "" },
      maxBuffer: 16 * 1024 * 1024,
    },
  );
  if (result.status !== 0)
    return [`${mode} probe exited ${result.status}: ${result.stderr.trim()}`];
  return JSON.parse(result.stdout.trim().split("\n").at(-1));
};

/** Every package-name specifier a consumer can load through `mode`. */
export const loadableSpecifiers = (name, manifest, mode) =>
  exportEntries(manifest.exports)
    .filter((entry) =>
      publishedTargets(entry).some(
        ({ conditions, target }) =>
          !target.endsWith(".d.ts") &&
          !target.endsWith(".d.cts") &&
          conditions.every((condition) =>
            [mode, "default", "node"].includes(condition),
          ),
      ),
    )
    .map((entry) =>
      entry.subpath === "." ? name : `${name}/${entry.subpath.slice(2)}`,
    )
    .filter((specifier) => !specifier.endsWith("package.json"));

const run = (argv, cwd) => {
  const result = spawnSync(argv[0], argv.slice(1), {
    cwd,
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
  });
  if (result.error || result.status !== 0)
    throw new Error(
      `${argv.join(" ")} failed: ${result.stderr || result.error?.message}`,
    );
  return result.stdout;
};

const linkDependency = (installModules, packageDir, dependency, problems) => {
  const link = join(installModules, dependency);
  if (existsSync(link)) return;
  const source = join(packageDir, "node_modules", dependency);
  if (!existsSync(source)) {
    problems.push(`dependency ${dependency} is not installed in the workspace`);
    return;
  }
  mkdirSync(dirname(link), { recursive: true });
  symlinkSync(realpathSync(source), link, "dir");
};

const typesCheck = (root, installRoot, packages) => {
  const requireFromSdk = createRequire(
    join(root, "demo/midgard-sdk/package.json"),
  );
  const tsc = requireFromSdk.resolve("typescript/bin/tsc");
  const probe = join(installRoot, "types-probe");
  mkdirSync(probe);
  const specifiers = packages.flatMap(({ name, manifest }) =>
    exportEntries(manifest.exports)
      .filter((entry) =>
        publishedTargets(entry).some(({ conditions }) =>
          conditions.includes("types"),
        ),
      )
      .map((entry) =>
        entry.subpath === "." ? name : `${name}/${entry.subpath.slice(2)}`,
      ),
  );
  writeFileSync(
    join(probe, "esm.mts"),
    specifiers
      .map((s, index) => `import * as m${index} from "${s}";\nvoid m${index};`)
      .join("\n"),
  );
  writeFileSync(
    join(probe, "cjs.cts"),
    specifiers
      .map((s, index) => `import m${index} = require("${s}");\nvoid m${index};`)
      .join("\n"),
  );
  writeFileSync(
    join(probe, "tsconfig.json"),
    JSON.stringify({
      compilerOptions: {
        module: "NodeNext",
        moduleResolution: "NodeNext",
        target: "ES2022",
        strict: true,
        noEmit: true,
        skipLibCheck: false,
        types: ["node"],
      },
      files: ["esm.mts", "cjs.cts"],
    }),
  );
  const result = spawnSync(process.execPath, [tsc, "-p", probe], {
    cwd: probe,
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
  });
  // Third-party declarations are their owners' concern, a vendored copy's as
  // much as an installed one's; only errors in the probe files or in the
  // publish set's own declarations are ours.
  return result.status === 0
    ? []
    : result.stdout
        .split("\n")
        .filter(
          (line) =>
            /error TS[0-9]+/u.test(line) &&
            /^(?:esm\.mts|cjs\.cts|\.\.\/node_modules\/@al-ft\/)/u.test(line) &&
            !/^\.\.\/node_modules\/@al-ft\/[^/]+\/vendor\//u.test(line),
        )
        .map((line) => `types: ${line.trim()}`);
};

/** Packs, extracts and checks the publish set into the new `output` directory. */
export const checkPublishSet = ({ root, output, types = true }) => {
  if (existsSync(output))
    throw new Error("Output directory must be new; refusing overwrite");
  const tarballs = join(output, "tarballs");
  const installRoot = join(output, "install");
  const installModules = join(installRoot, "node_modules");
  mkdirSync(tarballs, { recursive: true });
  mkdirSync(installModules, { recursive: true });
  writeFileSync(
    join(installRoot, "package.json"),
    JSON.stringify({ name: "publish-set-probe", private: true }),
  );
  const publishNames = new Set(PUBLISH_SET.map(({ name }) => name));
  const packages = PUBLISH_SET.map(({ name, dir }) => {
    const packageDir = join(root, dir);
    const problems = [];
    if (!existsSync(join(packageDir, "dist")))
      throw new Error(`${name} is not built; build it before packing`);
    const before = new Set(readdirSync(tarballs));
    run(
      ["corepack", "pnpm", "pack", "--pack-destination", tarballs],
      packageDir,
    );
    const [tarball] = readdirSync(tarballs).filter((file) => !before.has(file));
    const tarballPath = join(tarballs, tarball);
    const extracted = join(installModules, name);
    mkdirSync(extracted, { recursive: true });
    run(["tar", "-xzf", tarballPath, "-C", extracted, "--strip-components=1"]);
    const manifest = JSON.parse(
      readFileSync(join(extracted, "package.json"), "utf8"),
    );
    const files = listFiles(extracted);
    const workspaceManifest = JSON.parse(
      readFileSync(join(packageDir, "package.json"), "utf8"),
    );
    problems.push(
      ...packedManifestProblems(
        manifest,
        files,
        publishNames,
        workspaceManifest.exports,
      ),
      ...packedSecretProblems(extracted),
      ...vendoredImportProblems(extracted, manifest, packageDir),
    );
    return {
      name,
      dir,
      packageDir,
      manifest,
      version: manifest.version,
      tarball: relative(output, tarballPath),
      sha256: sha256(readFileSync(tarballPath)),
      fileCount: files.length,
      problems,
    };
  });
  for (const pkg of packages)
    for (const field of ["dependencies", "peerDependencies"])
      for (const dependency of Object.keys(pkg.manifest[field] ?? {}))
        if (!publishNames.has(dependency) && !dependency.startsWith("@al-ft/"))
          linkDependency(
            installModules,
            pkg.packageDir,
            dependency,
            pkg.problems,
          );
  linkDependency(
    installModules,
    join(root, "demo/midgard-sdk"),
    "@types/node",
    [],
  );
  for (const pkg of packages)
    for (const mode of ["import", "require"])
      pkg.problems.push(
        ...loadFailures(
          installRoot,
          mode,
          loadableSpecifiers(pkg.name, pkg.manifest, mode),
        ).map((failure) => `${mode} ${failure}`),
      );
  const typeProblems = types ? typesCheck(root, installRoot, packages) : [];
  const report = {
    schemaVersion: "midgard-sdk-publish-set-check-v1",
    sourceRevision: run(["git", "-C", root, "rev-parse", "HEAD"]).trim(),
    sourceDirty:
      run(["git", "-C", root, "status", "--porcelain", "--", "demo"]).trim() !==
      "",
    status:
      packages.every((pkg) => pkg.problems.length === 0) &&
      typeProblems.length === 0
        ? "passed"
        : "failed",
    packages: packages.map(
      ({ manifest: _manifest, packageDir: _packageDir, ...pkg }) => pkg,
    ),
    types: types
      ? { checked: true, problems: typeProblems }
      : { checked: false },
  };
  writeFileSync(
    join(output, "receipt.json"),
    `${JSON.stringify(report, null, 2)}\n`,
  );
  return report;
};

export const parseArguments = (args) => {
  const usage =
    "Usage: sdk-packages.mjs --root REPOSITORY --output NEW_DIRECTORY [--skip-types]";
  const values = { types: true };
  for (let index = 0; index < args.length; index += 1) {
    const key = args[index];
    if (key === "--skip-types") values.types = false;
    else if (
      (key === "--root" || key === "--output") &&
      args[index + 1] !== undefined &&
      !args[index + 1].startsWith("--")
    ) {
      values[key.slice(2)] = resolve(args[index + 1]);
      index += 1;
    } else throw new Error(usage);
  }
  if (values.root === undefined || values.output === undefined)
    throw new Error(usage);
  return values;
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  try {
    const report = checkPublishSet(parseArguments(process.argv.slice(2)));
    for (const pkg of report.packages) {
      console.log(`${pkg.name}@${pkg.version} ${pkg.sha256} ${pkg.tarball}`);
      for (const problem of pkg.problems) console.log(`  problem: ${problem}`);
    }
    for (const problem of report.types.problems ?? [])
      console.log(`  ${problem}`);
    console.log(`status: ${report.status}`);
    process.exitCode = report.status === "passed" ? 0 : 1;
  } catch (error) {
    console.error(error instanceof Error ? error.message : String(error));
    process.exitCode = 2;
  }
}
