"use strict";
// Preloaded into every Node process of a guarded build (NODE_OPTIONS
// --require). It records what the build actually read, so the guard can
// refuse to stamp a dist that depended on a file its input closure does not
// bind: file contents read through node:fs (tsup, its config loader, the
// TypeScript declaration worker, node scripts), the sources of copies,
// renames and links, every module loaded, every input esbuild's own metafile
// reports (esbuild reads sources outside Node), and files the build created
// by truncating them, which it may read back afterwards. Anything it cannot
// classify (a child process other than esbuild's service, a worker started
// without the tracer, a network connection, other code-loading node
// options, an esbuild context, missing module hooks, an fs path it cannot
// name) is recorded as untraced, which leaves the dist unstamped. It changes
// no result.
//
// Each record is [kind, value, pid]: process (argv[1] of a traced process),
// load (a module the loader hooks saw), read, write (a successful
// truncating create, copy or rename destination), bundle (an esbuild
// metafile input), esbuild (one per esbuild build), service (esbuild's own
// binary), virtual or untraced.
const fs = require("node:fs");
const Module = require("node:module");
const { basename, dirname, resolve } = require("node:path");
const { fileURLToPath } = require("node:url");
const { nodeOptionArguments } = require("./node-options.cjs");

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
  // undefined for a file descriptor (recorded when it was opened); anything
  // else that names no file is untraced.
  const pathOf = (path) => {
    if (typeof path === "number" || typeof path?.fd === "number")
      return undefined;
    if (Buffer.isBuffer(path)) path = path.toString();
    if (path instanceof URL && path.protocol === "file:")
      path = fileURLToPath(path);
    if (typeof path === "string") return resolve(path);
    emit("untraced", `fs call on ${Object.prototype.toString.call(path)}`);
    return undefined;
  };
  const record = (kind, path) => {
    const absolute = pathOf(path);
    if (absolute !== undefined) emit(kind, absolute);
  };
  emit("process", process.argv[1] ? real(process.argv[1]) : "");

  // Node options that load code other than this tracer run outside it.
  const options = [
    ...process.execArgv,
    ...nodeOptionArguments(process.env.NODE_OPTIONS),
  ];
  for (let index = 0; index < options.length; index++) {
    const loads =
      /^(?:-r|--require|--import|--loader|--experimental-loader)(?:=|$)/u.exec(
        options[index],
      );
    if (!loads) continue;
    const value = options[index].includes("=")
      ? options[index].slice(options[index].indexOf("=") + 1)
      : options[++index];
    if (real(value ?? "") !== __filename)
      emit("untraced", `node option ${loads[0].replace(/=$/u, "")} ${value}`);
  }

  // How an open's flags use the file: a truncating create leaves only the
  // build's own bytes; anything else with read access reads it. Unknown
  // flags count as reads. A positional string is open's flags but
  // writeFile's and createWriteStream's encoding.
  const flagsOf = (options, fallback, positional) =>
    typeof options === "number" || (typeof options === "string" && positional)
      ? options
      : typeof options === "string" || typeof options === "function"
        ? fallback
        : (options?.flag ?? options?.flags ?? fallback);
  const usage = (flags) => {
    if (typeof flags === "string")
      return /w/u.test(flags)
        ? { truncates: true }
        : { reads: /[r+]/u.test(flags) };
    if (typeof flags === "number") {
      const { O_WRONLY, O_TRUNC } = fs.constants;
      const access = flags & 3;
      return (flags & O_TRUNC) !== 0 && access !== 0
        ? { truncates: true }
        : { reads: access !== O_WRONLY };
    }
    return { reads: true };
  };
  // Run the original, then `done` once it succeeded: synchronously, through
  // a returned promise, or through a trailing callback without an error.
  const succeeded = (original, self, args, done) => {
    const last = args.length - 1;
    if (typeof args[last] === "function") {
      const callback = args[last];
      args[last] = function tracedCallback(error, ...rest) {
        if (!error) done();
        return callback.call(this, error, ...rest);
      };
      return original.apply(self, args);
    }
    const result = original.apply(self, args);
    if (result && typeof result.then === "function")
      return result.then((value) => {
        done();
        return value;
      });
    done();
    return result;
  };
  const wrap = (owner, name, traced) => {
    const original = owner[name];
    if (typeof original !== "function") return;
    owner[name] = function wrapped(...args) {
      return traced(original, this, args);
    };
  };
  const reading = (original, self, args) => {
    record("read", args[0]);
    return original.apply(self, args);
  };
  // The flags are the second argument of open, the third of writeFile.
  const opening = (fallback, at) => (original, self, args) => {
    const use = usage(flagsOf(args[at], fallback, at === 1));
    if (use.reads) record("read", args[0]);
    if (!use.truncates) return original.apply(self, args);
    return succeeded(original, self, args, () => record("write", args[0]));
  };
  // A copy, rename or link reads its source (every file below it for cp)
  // and, once it succeeded, the destination holds only those bytes.
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
  const copying = (original, self, args) => {
    sources(args[0]);
    return succeeded(original, self, args, () => record("write", args[1]));
  };
  // A symlink exposes its target, relative to the link's directory.
  const linking = (original, self, args) => {
    const link = pathOf(args[1]);
    if (link !== undefined && typeof args[0] === "string")
      sources(resolve(dirname(link), args[0]));
    else sources(args[0]);
    return succeeded(original, self, args, () => record("write", args[1]));
  };
  for (const owner of [fs, fs.promises]) {
    for (const name of [
      "readFileSync",
      "readFile",
      "createReadStream",
      "openAsBlob",
    ])
      wrap(owner, name, reading);
    for (const name of ["openSync", "open"]) wrap(owner, name, opening("r", 1));
    // writeFile truncates unless its flag says otherwise; an append keeps
    // what was there, so it never excuses a later read.
    for (const name of ["writeFileSync", "writeFile"])
      wrap(owner, name, opening("w", 2));
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
      wrap(owner, name, copying);
    for (const name of ["symlinkSync", "symlink"]) wrap(owner, name, linking);
  }
  wrap(fs, "createWriteStream", (original, self, args) => {
    const use = usage(flagsOf(args[1], "w", false));
    if (use.reads) record("read", args[0]);
    const stream = original.apply(self, args);
    if (use.truncates) stream.once("open", () => record("write", args[0]));
    return stream;
  });

  // Only esbuild's own service binary may run as a child: its reads come
  // back through the metafile. Any other child is not traced.
  const childProcess = require("node:child_process");
  const spawned = (name) => (original, self, args) => {
    const [file, list] = args;
    const words = Array.isArray(list) ? list.map(String) : [];
    if (
      typeof file === "string" &&
      ["spawn", "spawnSync", "execFile", "execFileSync"].includes(name) &&
      basename(file) === "esbuild" &&
      words.some((arg) => arg.startsWith("--service="))
    )
      emit("service", real(file));
    else
      emit(
        "untraced",
        `child process ${name} ${String(file)} ${words.join(" ")}`.trim(),
      );
    return original.apply(self, args);
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

  // A worker inherits this tracer through the process options and its
  // environment (this process's, unless it passes its own); one started
  // with its own execArgv, or with an environment that changes the trace or
  // the node options, runs untraced.
  const threads = require("node:worker_threads");
  const { Worker, SHARE_ENV } = threads;
  const nodeOptions = process.env.NODE_OPTIONS;
  threads.Worker = class TracedWorker extends Worker {
    constructor(file, options) {
      const env =
        options?.env === undefined || options.env === SHARE_ENV
          ? process.env
          : options.env;
      if (
        options?.execArgv !== undefined ||
        env?.MIDGARD_CONTRIB_BUILD_TRACE !== trace ||
        env?.NODE_OPTIONS !== nodeOptions
      )
        emit("untraced", `worker ${String(file).slice(0, 120)}`);
      super(file, options);
    }
  };

  // Network access binds nothing a stamp can check: every TCP or IPC
  // connection Node opens (fetch included), datagram sockets, and the
  // global network APIs.
  wrap(
    require("node:net").Socket.prototype,
    "connect",
    (original, self, args) => {
      emit("untraced", "network connection");
      return original.apply(self, args);
    },
  );
  wrap(require("node:dgram"), "createSocket", (original, self, args) => {
    emit("untraced", "network datagram socket");
    return original.apply(self, args);
  });
  for (const name of ["fetch", "WebSocket", "EventSource"]) {
    const descriptor = Object.getOwnPropertyDescriptor(globalThis, name);
    if (!descriptor) continue;
    let wrapped;
    Object.defineProperty(globalThis, name, {
      configurable: true,
      enumerable: descriptor.enumerable,
      get() {
        if (wrapped !== undefined) return wrapped;
        const original = descriptor.get
          ? descriptor.get.call(globalThis)
          : descriptor.value;
        wrapped =
          typeof original === "function"
            ? new Proxy(original, {
                apply(target, self, args) {
                  emit("untraced", `network ${name}`);
                  return Reflect.apply(target, self, args);
                },
                construct(target, args, newTarget) {
                  emit("untraced", `network ${name}`);
                  return Reflect.construct(target, args, newTarget);
                },
              })
            : original;
        return wrapped;
      },
      set(value) {
        wrapped = value;
      },
    });
  }
  Module.syncBuiltinESMExports();

  // Node's module loaders read through internal fs; the hooks see each
  // module a build process loads, ES modules included. Every traced process
  // loads at least its entry this way, which the guard checks.
  if (typeof Module.registerHooks === "function")
    Module.registerHooks({
      load(url, context, nextLoad) {
        if (url.startsWith("file:")) record("load", new URL(url));
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
  const esbuilds = new WeakMap();
  const load = Module._load;
  Module._load = function tracedLoad(request, ...rest) {
    const exported = load.call(this, request, ...rest);
    if (request !== "esbuild" || !exported || typeof exported !== "object")
      return exported;
    if (!esbuilds.has(exported)) {
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
      esbuilds.set(exported, copy);
    }
    return esbuilds.get(exported);
  };
}
