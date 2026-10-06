"use strict";
// Preloaded into every Node process of a guarded build (NODE_OPTIONS
// --require). It records what the build actually read, so the guard can
// refuse to stamp a dist that depended on a file its input closure does not
// bind: file contents read through node:fs (tsup, its config loader, the
// TypeScript declaration worker, node scripts), the sources of copies and
// renames, every module loaded, every input esbuild's own metafile reports
// (esbuild reads sources outside Node), and files the build wrote, which it
// may read back afterwards. Anything it cannot follow (a child process other
// than esbuild's service, an esbuild context, missing module hooks) is
// recorded as untraced, which leaves the dist unstamped. It changes no
// result.
//
// Each record is [kind, value, pid]: process (argv[1] of a traced process),
// read, write, bundle (an esbuild metafile input), esbuild (one per esbuild
// build), service (esbuild's own binary), virtual or untraced.
const fs = require("node:fs");
const Module = require("node:module");
const { basename, dirname, resolve } = require("node:path");
const { fileURLToPath } = require("node:url");

const { appendFileSync, lstatSync, readdirSync, realpathSync } = fs;
const trace = process.env.MIDGARD_CONTRIB_BUILD_TRACE;
const real = (path) => {
  try {
    return realpathSync(path);
  } catch {
    return resolve(path);
  }
};
// The package manager that launches the recipe (corepack running the pinned
// pnpm in its own process) reads its own installation and configuration; the
// guard names its resolved entry point, and only that process is exempt.
const launcher =
  process.env.MIDGARD_CONTRIB_BUILD_LAUNCHER !== undefined &&
  process.argv[1] !== undefined &&
  real(process.argv[1]) === process.env.MIDGARD_CONTRIB_BUILD_LAUNCHER;
if (trace && !launcher) {
  const seen = new Set();
  const emit = (kind, value) => {
    const key = `${kind}\0${value}`;
    if (seen.has(key)) return;
    seen.add(key);
    appendFileSync(trace, `${JSON.stringify([kind, value, process.pid])}\n`);
  };
  const pathOf = (path) => {
    if (Buffer.isBuffer(path)) path = path.toString();
    if (path instanceof URL) {
      if (path.protocol !== "file:") return undefined;
      path = fileURLToPath(path);
    }
    // A file descriptor was recorded when it was opened.
    return typeof path === "string" ? resolve(path) : undefined;
  };
  const record = (kind, path) => {
    const absolute = pathOf(path);
    if (absolute !== undefined) emit(kind, absolute);
  };
  emit("process", process.argv[1] ? real(process.argv[1]) : "");

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
  const wrap = (owner, name, before) => {
    const original = owner[name];
    if (typeof original !== "function") return;
    owner[name] = function traced(...args) {
      before(...args);
      return original.apply(this, args);
    };
  };
  const read = (path) => record("read", path);
  const opened = (path, options) =>
    record(writing(flagsOf(options)) ? "write" : "read", path);
  const written = (path) => record("write", path);
  // A copy or rename reads its source (every file below it for cp) and
  // produces its destination.
  const sources = (path) => {
    const absolute = pathOf(path);
    if (absolute === undefined) return;
    let stat;
    try {
      stat = lstatSync(absolute);
    } catch {
      return emit("read", absolute);
    }
    if (!stat.isDirectory()) return emit("read", absolute);
    for (const entry of readdirSync(absolute))
      sources(resolve(absolute, entry));
  };
  const copied = (source, destination) => {
    sources(source);
    record("write", destination);
  };
  for (const owner of [fs, fs.promises]) {
    for (const name of ["readFileSync", "readFile", "createReadStream"])
      wrap(owner, name, read);
    for (const name of ["openSync", "open"]) wrap(owner, name, opened);
    for (const name of [
      "writeFileSync",
      "writeFile",
      "appendFileSync",
      "appendFile",
      "createWriteStream",
    ])
      wrap(owner, name, written);
    for (const name of [
      "copyFileSync",
      "copyFile",
      "cpSync",
      "cp",
      "renameSync",
      "rename",
      "linkSync",
      "link",
    ])
      wrap(owner, name, copied);
    // A symlink exposes its target (relative to the link's directory).
    for (const name of ["symlinkSync", "symlink"])
      wrap(owner, name, (target, path) => {
        const link = pathOf(path);
        const pointed = pathOf(target);
        if (link !== undefined && typeof target === "string")
          sources(resolve(dirname(link), target));
        else if (pointed !== undefined) sources(pointed);
        if (link !== undefined) emit("write", link);
      });
  }

  // Only esbuild's own service binary may run as a child: its reads come
  // back through the metafile. Any other child is not traced.
  const childProcess = require("node:child_process");
  const spawned = (name) => (file, args) => {
    const list = Array.isArray(args) ? args.map(String) : [];
    if (
      typeof file === "string" &&
      ["spawn", "spawnSync", "execFile", "execFileSync"].includes(name) &&
      basename(file) === "esbuild" &&
      list.some((arg) => arg.startsWith("--service="))
    )
      emit("service", real(file));
    else
      emit(
        "untraced",
        `child process ${name} ${String(file)} ${list.join(" ")}`.trim(),
      );
  };
  for (const name of [
    "spawn",
    "spawnSync",
    "execFile",
    "execFileSync",
    "exec",
    "execSync",
    "fork",
  ])
    wrap(childProcess, name, spawned(name));

  // Node's module loaders read through internal fs; the hooks see each
  // module a build process loads, ES modules included.
  if (typeof Module.registerHooks === "function")
    Module.registerHooks({
      load(url, context, nextLoad) {
        if (url.startsWith("file:")) record("read", new URL(url));
        return nextLoad(url, context);
      },
    });
  else emit("untraced", "module loads (no module.registerHooks)");

  // esbuild resolves and reads sources in its own binary; its metafile names
  // every input it read. Contexts (watch and rebuild) are not traced, so a
  // build that uses one cannot be stamped.
  const inputs = (options, result) => {
    emit("esbuild", String(Object.keys(result?.metafile?.inputs ?? {}).length));
    const base = options?.absWorkingDir ?? process.cwd();
    for (const input of Object.keys(result?.metafile?.inputs ?? {})) {
      // Namespaced inputs (`ns:name`) are plugin modules made from options.
      if (/^[a-z][\w-]*:/iu.test(input) && !input.startsWith(`file:`))
        emit("virtual", input);
      else record("bundle", resolve(base, input.replace(/^file:/u, "")));
    }
    return result;
  };
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
        emit("untraced", "esbuild context");
        return exported.context(...args);
      };
      wrapped.set(exported, copy);
    }
    return wrapped.get(exported);
  };
}
