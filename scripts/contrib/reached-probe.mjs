// Child process of scripts/contrib/reached.mjs, run in one package directory
// with the environment its `test` script sets. It asks that package's own
// Vitest projects (and so their resolver: conditions, aliases, plugins) for
// the import graph rooted at every collected test file, every setup file and
// every given script file (the package's own and those outside every
// package), so worker entries, spawned scripts and helpers no test imports
// are graphed too. Vitest's `--related` walks the same graph from test files
// only, which is why it cannot see a module reached only through a worker
// entry. A module Vitest cannot transform (a standalone entry with top-level
// await) has its imports resolved one by one; one that cannot be resolved
// is reported failed.
//
// For each graphed module it also records the path-like tokens of its string
// literals outside import specifiers (the names it can read, spawn or load
// files by) and whether it lists directories.
//
// usage: node reached-probe.mjs <request.json>
// request: { output, exclude: [glob], files: [absolute script path] }
// output: { tests, edges: { file: [dependency] }, failed: { file: why },
//           spelled: { file: { paths: [token], walks } } }

import { readFileSync, writeFileSync } from "node:fs";
import { createRequire } from "node:module";
import { join, resolve } from "node:path";
import { pathToFileURL } from "node:url";

const request = JSON.parse(readFileSync(process.argv[2], "utf8"));
const require = createRequire(resolve(process.cwd(), "package.json"));
const vitestManifest = require.resolve("vitest/package.json");
const { createVitest } = await import(
  pathToFileURL(createRequire(vitestManifest).resolve("vitest/node")).href
);
// Vitest's own Vite: its esbuild strips types, its parser reads the rest.
const { parseAst, transformWithEsbuild } = await import(
  pathToFileURL(createRequire(vitestManifest).resolve("vite")).href
);

const SCRIPT = /\.[cm]?[jt]sx?$/u;
const TOKEN = /[A-Za-z0-9_.+@/-]+/gu;
const LISTERS = new Set([
  "readdir",
  "readdirSync",
  "opendir",
  "opendirSync",
  "glob",
  "globSync",
  "readDirectory",
]);

// A module's import specifiers, the path-like tokens of its other string
// literals, and whether it lists directories.
const parse = async (file) => {
  const { code } = await transformWithEsbuild(
    readFileSync(file, "utf8"),
    file,
    { target: "esnext", format: "esm" },
  );
  const specifiers = new Set();
  const paths = new Set();
  let walks = false;
  const text = (node) =>
    node?.type === "Literal" && typeof node.value === "string"
      ? node.value
      : node?.type === "TemplateLiteral" && node.expressions.length === 0
        ? node.quasis[0].value.cooked
        : undefined;
  const visit = (node) => {
    if (!node || typeof node.type !== "string") return;
    if (
      [
        "ImportDeclaration",
        "ExportNamedDeclaration",
        "ExportAllDeclaration",
        "ImportExpression",
      ].includes(node.type) &&
      text(node.source) !== undefined
    )
      specifiers.add(node.source);
    if (node.type === "CallExpression") {
      const callee = node.callee;
      const name =
        callee.type === "Identifier"
          ? callee.name
          : callee.type === "MemberExpression" &&
              callee.property.type === "Identifier"
            ? callee.property.name
            : undefined;
      if (name === "require" && text(node.arguments[0]) !== undefined)
        specifiers.add(node.arguments[0]);
      if (LISTERS.has(name)) walks = true;
    }
    const value =
      node.type === "Literal" && typeof node.value === "string"
        ? node.value
        : node.type === "TemplateElement"
          ? (node.value.cooked ?? node.value.raw)
          : undefined;
    if (value !== undefined && !specifiers.has(node))
      for (const [token] of value.matchAll(TOKEN))
        paths.add(token.replace(/\.+$/u, ""));
    for (const [key, child] of Object.entries(node)) {
      if (key === "parent") continue;
      if (Array.isArray(child)) child.forEach(visit);
      else if (child && typeof child === "object") visit(child);
    }
  };
  visit(parseAst(code));
  return {
    specifiers: [...specifiers].map(text),
    spelled: { paths: [...paths], walks },
  };
};

const vitest = await createVitest(
  "test",
  { watch: false, passWithNoTests: true, cliExclude: request.exclude },
  {},
  {},
);
const tests = new Set();
const edges = {};
const failed = {};
try {
  for (const project of vitest.projects) {
    const { testFiles } = await project.globTestFiles();
    for (const file of testFiles) tests.add(file);
    const setup = [
      ...(project.config.setupFiles ?? []),
      ...(project.config.globalSetup ?? []),
    ]
      .filter((file) => typeof file === "string")
      .map((file) => resolve(project.config.root, file));
    const environment = project.vite.environments.ssr;
    const visited = new Set();
    const visit = async (file) => {
      if (visited.has(file)) return;
      visited.add(file);
      let dependencies;
      try {
        const transformed =
          environment.moduleGraph.getModuleById(file)?.transformResult ??
          (await environment.transformRequest(file));
        dependencies = [
          ...(transformed?.deps ?? []),
          ...(transformed?.dynamicDeps ?? []),
        ]
          .filter((dependency) => !dependency.startsWith("\0"))
          .map((dependency) =>
            dependency.startsWith("/@fs/")
              ? dependency.slice(4)
              : join(project.config.root, dependency),
          );
      } catch (error) {
        // Vitest cannot run this module (a standalone entry with top-level
        // await, say), but something may spawn it: resolve its imports one
        // by one with the same resolver.
        try {
          dependencies = [];
          for (const specifier of (await parse(file)).specifiers) {
            const resolved = await environment.pluginContainer.resolveId(
              specifier,
              file,
            );
            if (!resolved)
              throw new Error(`cannot resolve ${specifier}`, { cause: error });
            if (!resolved.external) dependencies.push(resolved.id);
          }
        } catch (fallback) {
          failed[file] = String(fallback?.message ?? fallback).split("\n")[0];
          edges[file] ??= [];
          return;
        }
      }
      const merged = new Set(edges[file] ?? []);
      for (const dependency of dependencies) {
        const path = dependency.replace(/\?.*$/u, "");
        if (path.includes("/node_modules/")) continue;
        merged.add(path);
      }
      edges[file] = [...merged];
      await Promise.all(
        edges[file].filter((path) => SCRIPT.test(path)).map(visit),
      );
    };
    for (const file of [...testFiles, ...setup, ...request.files])
      await visit(file);
  }
} finally {
  await vitest.close();
}
const spelled = {};
for (const file of Object.keys(edges)) {
  try {
    spelled[file] = (await parse(file)).spelled;
  } catch (error) {
    failed[file] ??= `cannot read: ${error.message}`;
  }
}
writeFileSync(
  request.output,
  JSON.stringify({ tests: [...tests].sort(), edges, failed, spelled }),
);
