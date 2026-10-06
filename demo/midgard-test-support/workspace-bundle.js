import { createHash } from "node:crypto";
import {
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  renameSync,
  rmSync,
  statSync,
  utimesSync,
  writeFileSync,
} from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { pathToFileURL } from "node:url";

import {
  bundleRoot,
  demoDirectory,
  isInside,
  isNodeBuiltin,
  packageOfFile,
  readJson,
  resolveRelative,
  workspacePackages,
} from "./workspace-bundle-files.js";

/**
 * The workspace bundle: the workspace packages a suite depends on, bundled
 * from their CURRENT SOURCE by esbuild once per run and loaded natively by the
 * test forks, instead of being transformed module-by-module by Vite in every
 * forked test file. `vitest.js` (`workspaceBundleProjects`) states the
 * guarantee this keeps and the hazards it routes around; this file is the
 * mechanism.
 *
 * Plain JavaScript for the same reason as `vitest.js`: it runs at config time.
 */

const BUNDLE_SCHEMA = "midgard-test-bundle/v1";
/** Bundles not used for this long are pruned when a new one is published. */
const PRUNE_AFTER_MS = 24 * 60 * 60 * 1000;

const hashTree = (hash, directory, hashed) => {
  const walk = (current) => {
    const entries = readdirSync(current, { withFileTypes: true }).sort(
      (a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0),
    );
    // A Cargo crate's build output is rewritten by every build (global setup
    // builds the native owner binary) and is never bundled code.
    const crate = entries.some((entry) => entry.name === "Cargo.toml");
    for (const entry of entries) {
      if (
        entry.name === "node_modules" ||
        entry.name === "dist" ||
        entry.name.startsWith(".") ||
        (crate && entry.name === "target")
      )
        continue;
      const path = join(current, entry.name);
      if (entry.isDirectory()) walk(path);
      else if (entry.isFile()) {
        hashed.add(relative(demoDirectory, path));
        hash.update(`${relative(demoDirectory, path)}\0`);
        hash.update(readFileSync(path));
        hash.update("\0");
      }
    }
  };
  walk(directory);
};

const esbuildVersion = async () => (await import("esbuild")).version;

/** This file and the helpers that resolve and route what it bundles. */
const BUNDLER_MODULES = [
  "./workspace-bundle.js",
  "./workspace-bundle-files.js",
  "./workspace-bundle-analysis.js",
];
/** Builds whose inputs changed underneath them before one is published. */
const BUILD_ATTEMPTS = 3;

const bundleKey = ({ analysis, options, packages }) => {
  const hash = createHash("sha256");
  hash.update(JSON.stringify({ options, entries: [...analysis.entries] }));
  // The bundler's own text decides how the bundle is built.
  for (const module of BUNDLER_MODULES) {
    hash.update(`${module}\0`);
    hash.update(readFileSync(new URL(module, import.meta.url)));
    hash.update("\0");
  }
  hash.update(readFileSync(join(demoDirectory, "pnpm-lock.yaml")));
  const hashed = new Set();
  for (const name of [...analysis.bundleable].sort())
    hashTree(hash, packages.get(name).directory, hashed);
  return { key: hash.digest("hex").slice(0, 32), hashed };
};

/**
 * Bundle `entries` (specifier -> source file) into a content-addressed
 * directory and return specifier -> bundled file.
 *
 * The key covers the text of this file and its two helper modules, the full
 * contents of every bundleable package directory (not just the files a
 * previous build read, so a newly added file that changes resolution changes
 * the key), the lockfile, the esbuild version and every option below. A
 * changed byte anywhere means a new bundle, never a reused one. The key is
 * recomputed after esbuild finishes: if an input changed during the build, the
 * output may mix old and new source, so it is discarded and rebuilt rather
 * than published under either key. The directory is published by an atomic
 * rename, so concurrent runs either build the same content or reuse a
 * complete one.
 */
export const buildWorkspaceBundle = async (
  { analysis, target, conditions, resolveExternal },
  attempt = 1,
) => {
  const packages = workspacePackages();
  const started = performance.now();
  const version = await esbuildVersion();
  const options = { target, conditions, version, schema: BUNDLE_SCHEMA };
  const { key, hashed } = bundleKey({ analysis, options, packages });
  const directory = join(bundleRoot, key);
  const manifestPath = join(directory, "manifest.json");

  if (!existsSync(manifestPath)) {
    const { build } = await import("esbuild");
    const staging = join(
      bundleRoot,
      `.staging-${key}-${process.pid}-${Date.now()}`,
    );
    mkdirSync(staging, { recursive: true });
    const entryNames = new Map();
    const entryPoints = {};
    const nameOfFile = new Map();
    for (const [specifier, file] of analysis.entries) {
      if (!nameOfFile.has(file)) {
        const name = `entry-${String(nameOfFile.size)}`;
        nameOfFile.set(file, name);
        entryPoints[name] = file;
      }
      entryNames.set(specifier, nameOfFile.get(file));
    }
    const escapes = [];
    const metaEscapes = [];
    const owner = (file) => packageOfFile(packages, file);
    const label = (file) => relative(demoDirectory, file);
    const result = await build({
      entryPoints,
      absWorkingDir: demoDirectory,
      outdir: staging,
      bundle: true,
      splitting: true,
      format: "esm",
      platform: "node",
      target,
      // The bundle runs as native ESM, where top-level await is supported;
      // `target` only lowers syntax.
      supported: { "top-level-await": true },
      conditions,
      mainFields: ["module", "jsnext:main", "jsnext", "main"],
      outExtension: { ".js": ".mjs" },
      chunkNames: "chunks/[name]-[hash]",
      // Bundling renames colliding identifiers; keep `Function#name` and
      // `constructor.name` what the source says.
      keepNames: true,
      sourcemap: "linked",
      sourcesContent: false,
      metafile: true,
      logLevel: "silent",
      loader: { ".sql": "text" },
      plugins: [
        {
          name: "midgard-workspace-bundle",
          setup(esbuild) {
            // A bundled module sees the same `import.meta` it sees when
            // Vitest's module runner runs it from source ({ url, filename,
            // dirname } of the SOURCE file, and `main: false`), so
            // source-relative fixture paths and every location check behave
            // exactly as in a source-mode run. The runner's `resolve` is
            // Node's own `import.meta.resolve` with the source file as its
            // parent (Vitest starts every worker with
            // `--experimental-import-meta-resolve`, which takes that parent),
            // and the bundled one is the same call.
            esbuild.onLoad({ filter: /\.(?:m?[jt]s|tsx)$/u }, (args) => {
              if (owner(args.path) === undefined) return undefined;
              const text = readFileSync(args.path, "utf8");
              if (!text.includes("import.meta")) return undefined;
              // The runner's `import.meta` has no `hot`; only `env` lacks a
              // faithful bundled form, and the analysis keeps modules reading
              // it out of the bundle.
              if (/\bimport\.meta\.env\b/u.test(text))
                metaEscapes.push(label(args.path));
              const url = pathToFileURL(args.path).href;
              const fields = JSON.stringify({
                url,
                filename: args.path,
                dirname: dirname(args.path),
                main: false,
              });
              const meta = `{ ...${fields}, resolve: (specifier, parent) => import.meta.resolve(specifier, parent ?? ${JSON.stringify(url)}) }`;
              return {
                // Same line as the source's first, so line numbers hold; a
                // shebang is only legal as the very first bytes, so it goes.
                contents: `const __midgardSourceImportMeta = ${meta};${text.replace(/^#!.*/u, "").replace(/\bimport\.meta\b/gu, "__midgardSourceImportMeta")}`,
                loader: /\.tsx$/u.test(args.path)
                  ? "tsx"
                  : /\.m?ts$/u.test(args.path)
                    ? "ts"
                    : "js",
              };
            });
            esbuild.onResolve({ filter: /.*/u }, async (args) => {
              if (args.kind === "entry-point") return undefined;
              if (isNodeBuiltin(args.path))
                return { path: args.path, external: true };
              if (args.path.startsWith(".") || args.path.startsWith("/")) {
                const target = args.path.startsWith("/")
                  ? args.path
                  : resolveRelative(args.importer, args.path);
                if (target === undefined) return undefined;
                const owner = packageOfFile(packages, target);
                if (isInside(analysis.sourceRoot, target))
                  escapes.push(
                    `${relative(demoDirectory, args.importer)} -> ${args.path}`,
                  );
                return owner === undefined
                  ? { path: target, external: true }
                  : { path: target };
              }
              // Bare specifiers resolve exactly as Vite resolves them for the
              // source-mode run, so a bundled package imports the very same
              // dependency file (and module instance) the suite's own code does.
              const resolved = await resolveExternal(args.path, args.importer);
              if (resolved === undefined)
                return { path: args.path, external: true };
              const owner = packageOfFile(packages, resolved);
              if (owner === undefined)
                return { path: resolved, external: true };
              if (isInside(analysis.sourceRoot, resolved)) {
                escapes.push(
                  `${relative(demoDirectory, args.importer)} -> ${args.path}`,
                );
                return { path: resolved, external: true };
              }
              return { path: resolved };
            });
          },
        },
      ],
    });
    if (metaEscapes.length > 0) {
      rmSync(staging, { recursive: true, force: true });
      throw new Error(
        `[workspace-bundle] import.meta.env has no faithful bundled form; these modules must not be bundled:\n  ${metaEscapes.join("\n  ")}`,
      );
    }
    if (escapes.length > 0) {
      rmSync(staging, { recursive: true, force: true });
      throw new Error(
        `[workspace-bundle] the bundle would carry code from the source region (${analysis.sourceRoot}), which the static analysis in workspace-bundle-analysis.js did not predict:\n  ${escapes.slice(0, 20).join("\n  ")}`,
      );
    }
    const outputs = {};
    for (const [output, meta] of Object.entries(result.metafile.outputs))
      if (meta.entryPoint !== undefined)
        outputs[
          relative(demoDirectory, resolve(demoDirectory, meta.entryPoint))
        ] = resolve(demoDirectory, output);
    const files = {};
    for (const [specifier, name] of entryNames) {
      const source = relative(demoDirectory, entryPoints[name]);
      const output = outputs[source];
      if (output === undefined)
        throw new Error(
          `[workspace-bundle] esbuild produced no output for ${specifier}`,
        );
      files[specifier] = relative(staging, output);
    }
    const inputs = Object.keys(result.metafile.inputs).map((input) =>
      relative(demoDirectory, resolve(demoDirectory, input)),
    );
    // A bundled file the key did not read could change without a rebuild.
    const unhashed = inputs.filter((input) => !hashed.has(input));
    if (unhashed.length > 0) {
      rmSync(staging, { recursive: true, force: true });
      throw new Error(
        `[workspace-bundle] the bundle would carry files its key does not cover:\n  ${unhashed.slice(0, 20).join("\n  ")}`,
      );
    }
    // An input edited while esbuild ran: the output matches neither key.
    if (bundleKey({ analysis, options, packages }).key !== key) {
      rmSync(staging, { recursive: true, force: true });
      if (attempt >= BUILD_ATTEMPTS)
        throw new Error(
          `[workspace-bundle] the bundled sources changed during each of ${String(BUILD_ATTEMPTS)} builds; refusing to publish a bundle that may not match its key`,
        );
      return buildWorkspaceBundle(
        { analysis, target, conditions, resolveExternal },
        attempt + 1,
      );
    }
    writeFileSync(
      join(staging, "manifest.json"),
      JSON.stringify({ schema: BUNDLE_SCHEMA, key, options, files, inputs }),
    );
    try {
      renameSync(staging, directory);
    } catch (error) {
      rmSync(staging, { recursive: true, force: true });
      if (!existsSync(manifestPath)) throw error;
    }
    for (const entry of readdirSync(bundleRoot)) {
      const path = join(bundleRoot, entry);
      if (path === directory) continue;
      try {
        if (Date.now() - statSync(path).mtimeMs > PRUNE_AFTER_MS)
          rmSync(path, { recursive: true, force: true });
      } catch {
        // Another run pruned it first.
      }
    }
  }
  const now = new Date();
  utimesSync(directory, now, now);
  if (process.env.MIDGARD_TEST_WORKSPACE_BUNDLE_REPORT === "1")
    console.log(
      `[workspace-bundle] ${key} ready in ${(performance.now() - started).toFixed(0)} ms (${String(analysis.entries.size)} entries)`,
    );
  const manifest = readJson(manifestPath);
  return {
    key,
    directory,
    files: new Map(
      Object.entries(manifest.files).map(([specifier, file]) => [
        specifier,
        join(directory, file),
      ]),
    ),
    inputs: new Set(manifest.inputs.map((input) => join(demoDirectory, input))),
  };
};

const VITE_DEVELOPMENT_CONDITION = "development|production";

/**
 * The Vite plugin that serves the bundle: bare imports of a bundled entry
 * resolve to the bundled file (loaded natively by the fork), and any attempt
 * to load one of the bundle's input files from source fails the file loudly —
 * that would be a second, separate instance of the same module.
 */
export const workspaceBundlePlugin = ({ analysis }) => {
  const packages = workspacePackages();
  const bundledDirectories = analysis.bundleable.map(
    (name) => packages.get(name).directory,
  );
  let bundle;
  let conditions;
  let target;
  const ensure = (context) => {
    bundle ??= buildWorkspaceBundle({
      analysis,
      target,
      conditions,
      resolveExternal: async (specifier, from) => {
        const resolved = await context.resolve(specifier, from, {
          skipSelf: true,
        });
        if (resolved === null || resolved.id.startsWith("\0")) return undefined;
        const id = resolved.id.split("?")[0];
        return id.startsWith("/") ? id : undefined;
      },
    });
    return bundle;
  };
  return {
    name: "midgard-workspace-bundle",
    enforce: "pre",
    configResolved(config) {
      const nodeEnvironment =
        process.env.NODE_ENV === "production" ? "production" : "development";
      conditions = (
        config.ssr?.resolve?.conditions ?? ["node", VITE_DEVELOPMENT_CONDITION]
      ).map((condition) =>
        condition === VITE_DEVELOPMENT_CONDITION ? nodeEnvironment : condition,
      );
      target =
        config.esbuild === false
          ? "esnext"
          : (config.esbuild?.target ?? "esnext");
    },
    async resolveId(source, importer) {
      let key = source;
      if (
        source.startsWith(".") &&
        importer !== undefined &&
        !importer.startsWith("\0")
      )
        key = resolveRelative(importer.split("?")[0], source) ?? source;
      if (!analysis.entries.has(key)) return null;
      const { files } = await ensure(this);
      return { id: files.get(key), external: true };
    },
    async load(id) {
      const file = id.split("?")[0];
      if (!bundledDirectories.some((directory) => isInside(directory, file)))
        return null;
      const { inputs } = await ensure(this);
      if (inputs.has(file))
        throw new Error(
          `[workspace-bundle] ${relative(demoDirectory, file)} is part of the workspace bundle and must not also load from source (two instances of one module). The test file importing it belongs in the source project: extend the routing in midgard-test-support/workspace-bundle-analysis.js.`,
        );
      return null;
    },
  };
};
