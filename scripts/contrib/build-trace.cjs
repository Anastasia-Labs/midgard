"use strict";
// Preloaded into every Node process of a guarded build (NODE_OPTIONS
// --require). It records what the build actually read, so the guard can
// refuse to stamp a dist that depended on a file its input closure does not
// bind: file contents read through node:fs (tsup, its config loader, the
// TypeScript declaration worker, node scripts), every input esbuild's own
// metafile reports (esbuild reads sources outside Node), and files the build
// wrote, which it may read back. It changes no result.
const { appendFileSync } = require("node:fs");
const fs = require("node:fs");
const Module = require("node:module");
const { resolve } = require("node:path");

const trace = process.env.MIDGARD_CONTRIB_BUILD_TRACE;
// The package manager that launches the recipe reads its own installation
// and configuration; the recipe's processes are the ones traced.
const launcher = /[\\/](corepack|pnpm)([\\/]|\.c?js$|$)/u.test(
  process.argv[1] ?? "",
);
if (trace && !launcher) {
  const seen = new Set();
  const record = (kind, path) => {
    if (kind === "virtual" || kind === "untraced") {
      // Names, not paths.
      const key = `${kind}\0${path}`;
      if (!seen.has(key)) {
        seen.add(key);
        appendFileSync(trace, `${JSON.stringify([kind, path])}\n`);
      }
      return;
    }
    if (typeof path !== "string" && !(path instanceof URL)) {
      if (Buffer.isBuffer(path)) path = path.toString();
      else return; // a file descriptor was recorded when it was opened
    }
    if (path instanceof URL) {
      if (path.protocol !== "file:") return;
      path = require("node:url").fileURLToPath(path);
    }
    const absolute = resolve(path);
    const key = `${kind}\0${absolute}`;
    if (seen.has(key)) return;
    seen.add(key);
    appendFileSync(trace, `${JSON.stringify([kind, absolute])}\n`);
  };
  const writing = (flags) =>
    typeof flags === "string"
      ? /[wa+]/u.test(flags)
      : typeof flags === "number"
        ? (flags & (fs.constants.O_WRONLY | fs.constants.O_RDWR)) !== 0
        : false;
  const flagsOf = (options) =>
    typeof options === "string" || typeof options === "number"
      ? options
      : (options?.flag ?? options?.flags);
  const wrap = (owner, name, kindOf) => {
    const original = owner[name];
    if (typeof original !== "function") return;
    owner[name] = function traced(path, ...rest) {
      record(kindOf(rest), path);
      return original.call(this, path, ...rest);
    };
  };
  const read = () => "read";
  const opened = (rest) => (writing(flagsOf(rest[0])) ? "write" : "read");
  const written = () => "write";
  for (const name of ["readFileSync", "readFile", "createReadStream"])
    wrap(fs, name, read);
  for (const name of ["openSync", "open"]) wrap(fs, name, opened);
  for (const name of [
    "writeFileSync",
    "writeFile",
    "appendFileSync",
    "appendFile",
    "createWriteStream",
  ])
    wrap(fs, name, written);
  wrap(fs.promises, "readFile", read);
  wrap(fs.promises, "open", opened);
  for (const name of ["writeFile", "appendFile"])
    wrap(fs.promises, name, written);
  // Renames and copies produce their destination.
  for (const owner of [fs, fs.promises])
    for (const name of ["renameSync", "rename", "copyFileSync", "copyFile"]) {
      const original = owner[name];
      if (typeof original !== "function") continue;
      owner[name] = function produced(source, destination, ...rest) {
        record("write", destination);
        return original.call(this, source, destination, ...rest);
      };
    }

  // esbuild resolves and reads sources in its own binary; its metafile names
  // every input it read. Contexts (watch and rebuild) are not traced, so a
  // build that uses one cannot be stamped.
  const inputs = (options, result) => {
    const base = options?.absWorkingDir ?? process.cwd();
    for (const input of Object.keys(result?.metafile?.inputs ?? {})) {
      // Namespaced inputs (`ns:name`) are plugin modules made from options.
      if (/^[a-z][\w-]*:/iu.test(input) && !input.startsWith(`file:`))
        record("virtual", input);
      else record("read", resolve(base, input.replace(/^file:/u, "")));
    }
    return result;
  };
  // Node's module loaders read through internal fs; the hooks see each
  // module a build process loads, ES modules included.
  Module.registerHooks?.({
    load(url, context, nextLoad) {
      if (url.startsWith("file:")) record("read", new URL(url));
      return nextLoad(url, context);
    },
  });

  const wrapped = new WeakMap();
  const load = Module._load;
  Module._load = function tracedLoad(request, ...rest) {
    const exported = load.call(this, request, ...rest);
    if (request !== "esbuild" || !exported || typeof exported !== "object")
      return exported;
    if (!wrapped.has(exported)) {
      const copy = { ...exported };
      copy.build = (options) =>
        exported
          .build({ ...options, metafile: true })
          .then((result) => inputs(options, result));
      copy.buildSync = (options) =>
        inputs(options, exported.buildSync({ ...options, metafile: true }));
      copy.context = (...args) => {
        record("untraced", "esbuild context");
        return exported.context(...args);
      };
      wrapped.set(exported, copy);
    }
    return wrapped.get(exported);
  };
}
