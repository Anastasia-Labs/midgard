// The files `node scripts/preflight.mjs --run <id>` executes as its runner:
// preflight.mjs and every repository module it imports, transitively, plus
// the modules the capability probes of the named checks load or run by path
// (PROBE_MODULES). A CI step that runs a check by id runs this code, so a
// workflow calling it must re-run when any of these files changes;
// preflight-runner-filters.test.mjs holds each such workflow's path filter
// to this list.
//
// The check commands the runner spawns (a golden's package script, a ledger
// verifier) are not the runner. They were CI's commands before the steps
// named checks by id, and the workflows' existing filters cover them.
//
// The walk reads import syntax, not a module graph. A dynamic import whose
// target is not a string literal cannot be followed, so each one must be
// declared in DYNAMIC_IMPORTS with the files it loads; an undeclared one is
// reported in `undeclared` and fails the test rather than silently leaving
// its target out.

import { existsSync, readFileSync } from "node:fs";
import { dirname, relative, resolve, sep } from "node:path";

import { PROBE_MODULES } from "../preflight/probes.mjs";

export const RUNNER_ENTRY = "scripts/preflight.mjs";

// Each module with computed dynamic imports: how many it has, and the files
// they load. probes.mjs loads PROBE_MODULES, which the capabilities add.
export const DYNAMIC_IMPORTS = {
  "scripts/preflight/probes.mjs": { sites: 1, loads: [] },
  "demo/scripts/assert-midgard-core-dist-current.mjs": {
    sites: 2,
    loads: [
      "demo/midgard-core/scripts/write-dist-source-digest.mjs",
      // A build output: not walked, but a change to it still means the
      // probe reads something else.
      "demo/midgard-core/dist/consensus-profile.js",
    ],
  },
};

const STATIC_IMPORT =
  /^\s*(?:import|export)\b[^;]*?\bfrom\s*["']([^"']+)["']|^\s*import\s*["']([^"']+)["']/gmu;
const REQUIRE = /\brequire\(\s*["']([^"']+)["']\s*\)/gu;
const DYNAMIC_IMPORT = /\bimport\(\s*(["'][^"']+["']\s*\)|[^\n]*)/gu;
const LITERAL = /^["']([^"']+)["']\s*\)$/u;

const toPosix = (path) => path.split(sep).join("/");

/**
 * `{ files, undeclared }` for a run whose checks need `capabilities`: the
 * runner's files relative to `root`, sorted, and each computed dynamic
 * import DYNAMIC_IMPORTS does not account for.
 */
export const runnerClosure = (root, capabilities = []) => {
  const files = new Set();
  const undeclared = [];
  const visit = (path) => {
    if (files.has(path)) return;
    const absolute = resolve(root, path);
    if (!existsSync(absolute))
      throw new Error(`the runner loads ${path}, which does not exist`);
    files.add(path);
    const source = readFileSync(absolute, "utf8");
    const follow = (specifier) => {
      if (specifier.startsWith("."))
        visit(toPosix(relative(root, resolve(dirname(absolute), specifier))));
    };
    for (const [, from, bare] of source.matchAll(STATIC_IMPORT))
      follow(from ?? bare);
    for (const [, specifier] of source.matchAll(REQUIRE)) follow(specifier);
    const computed = [];
    for (const [, argument] of source.matchAll(DYNAMIC_IMPORT)) {
      const literal = LITERAL.exec(argument);
      if (literal) follow(literal[1]);
      else computed.push(`${path}: import(${argument.trim()}`);
    }
    const declared = DYNAMIC_IMPORTS[path];
    if (computed.length !== (declared?.sites ?? 0)) {
      undeclared.push(...computed);
      return;
    }
    for (const load of declared?.loads ?? []) {
      if (/\/dist\//u.test(load)) files.add(load);
      else visit(load);
    }
  };
  visit(RUNNER_ENTRY);
  for (const capability of capabilities)
    for (const module of PROBE_MODULES[capability] ?? []) visit(module);
  return { files: [...files].sort(), undeclared: undeclared.sort() };
};
