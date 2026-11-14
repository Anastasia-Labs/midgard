/**
 * Aiken functions exported as Flat UPLC programs, compiled once per set of
 * compiler inputs and kept on disk.
 *
 * `aiken export -m <module> -n <function>` type-checks and compiles the whole
 * on-chain project to emit one function. Measured on 2026-11-11 under the
 * capped runner on a loaded shared machine (load average 8 to 14), five
 * exports of `midgard/cek_machine_v1` each took 50 to 62 s of wall time and
 * peaked at 1.5 GB resident (`/usr/bin/time -v`: 1,508,196 to
 * 1,562,108 KB).
 *
 * An export is a function of the compiler's inputs alone, so it is cached
 * under the sha256 of all of them:
 *
 * - `blueprintSourceHash`: every file `aiken build` reads (`lib`, `validators`,
 *   `env`, `aiken.toml`, `aiken.lock`) and the compiler pin `demo/flake.lock`;
 * - the compiler binary's path, a nix store path naming its build;
 * - the module, the function and the extra `aiken export` flags.
 *
 * A key that missed an input would serve a program compiled from other
 * sources and turn a divergence into a green, so the key test changes each
 * input in turn (`tests/aiken-export-cache.test.mjs`).
 *
 * Entries live in `onchain/aiken/build/aiken-exports/`, ignored build output
 * like the traced-refusal blueprints beside it. An entry is one JSON file
 * holding its key inputs, the program and the program's sha256; it is used
 * only when its inputs still give its key and its program still gives its
 * sha256, and is otherwise compiled again. A miss takes the key's lock (a
 * directory published by rename, `build-lock.js`), so files and runs that miss
 * at once compile each key once, and compiles under the host-wide
 * `memory-heavy-build` resource (`scripts/contrib/resources.mjs`). Every
 * compile logs one `[aiken-evaluator] exporting` line: a run that logs none
 * compiled nothing.
 *
 * CI compiles once per run: the `blueprint` job runs this file
 * (`node demo/midgard-test-support/aiken-export-cache.js`), which exports
 * `STEP_EVALUATOR_EXPORTS`, and shares the entries as the `aiken-exports`
 * artifact. A job that restores them sets
 * `MIDGARD_REQUIRE_PREFETCHED_AIKEN_EXPORTS=1`, under which a miss fails
 * instead of compiling.
 *
 * Plain JavaScript, like the build lock, the source stamp and the resource
 * queue it composes, so that its tests run with this package's tooling tests.
 */
import { execFile } from "node:child_process";
import { createHash, randomUUID } from "node:crypto";
import { mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { withResource } from "../../scripts/contrib/resources.mjs";
import { aikenBinary } from "../../scripts/lib/aiken.mjs";
import { blueprintSourceHash } from "../scripts/lib/blueprint-stamp.mjs";
import { withBuildLock } from "./build-lock.js";

const repositoryRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../..",
);

const ENTRY_SCHEMA = "midgard-aiken-export/v1";
const MAX_OUTPUT = 512 * 1024 * 1024;
/** How long a miss waits for another process compiling the same key, which
 * may itself queue for the memory-heavy resource. */
const LOCK_WAIT_MS = 1_800_000;

const sha256 = (value) => createHash("sha256").update(value).digest("hex");

/** The inputs an entry is keyed on, in the order the key serialises them. */
const keyInputs = ({ sourceHash, compiler }, { module, name, flags }) => ({
  schema: ENTRY_SCHEMA,
  sourceHash,
  compiler,
  module,
  name,
  flags: [...flags],
});

/** The key of an entry's inputs; an entry carries its own. */
const keyOf = (inputs) => sha256(JSON.stringify(keyInputs(inputs, inputs)));

/** The cached program at `path`, or `undefined` when it is missing or does
 * not verify against `key`. */
const readEntry = (path, key) => {
  let entry;
  try {
    entry = JSON.parse(readFileSync(path, "utf8"));
  } catch (cause) {
    if (cause?.code === "ENOENT" || cause instanceof SyntaxError)
      return undefined;
    throw cause;
  }
  if (entry?.schema !== ENTRY_SCHEMA || !Array.isArray(entry.flags))
    return undefined;
  if (keyOf(entry) !== key || typeof entry.flat !== "string") return undefined;
  const flat = Buffer.from(entry.flat, "hex");
  return sha256(flat) === entry.sha256 ? Uint8Array.from(flat) : undefined;
};

/** The Flat program inside the CBOR byte string `aiken export` prints. A
 * program needs at most a four-byte length. */
const unwrapCborByteString = (cbor, label) => {
  const head = cbor[0] ?? 0;
  const additional = head & 0x1f;
  const width = additional < 24 ? 0 : { 24: 1, 25: 2, 26: 4 }[additional];
  const length =
    width === undefined || cbor.length < 1 + width
      ? undefined
      : width === 0
        ? additional
        : cbor.readUIntBE(1, width);
  if (
    head >> 5 !== 2 ||
    length === undefined ||
    cbor.length !== 1 + width + length
  )
    throw new Error(`the export of ${label} is not a CBOR byte string`);
  return Uint8Array.from(cbor.subarray(1 + width));
};

const compile = async ({ root, compiler, module, name, flags }) => {
  const label = `${module}.${name}`;
  let stdout;
  try {
    ({ stdout } = await promisify(execFile)(
      compiler,
      ["export", "-m", module, "-n", name, ...flags],
      {
        cwd: resolve(root, "onchain/aiken"),
        encoding: "utf8",
        maxBuffer: MAX_OUTPUT,
      },
    ));
  } catch (cause) {
    throw new Error(
      `aiken export ${label} failed:\n${cause?.stdout ?? ""}${cause?.stderr ?? String(cause)}`,
      { cause },
    );
  }
  const exported = JSON.parse(stdout);
  return unwrapCborByteString(Buffer.from(exported.compiledCode, "hex"), label);
};

const writeEntry = (path, inputs, flat) => {
  const staging = `${path}.${String(process.pid)}-${randomUUID()}`;
  writeFileSync(
    staging,
    JSON.stringify({
      ...keyInputs(inputs, inputs),
      sha256: sha256(flat),
      flat: Buffer.from(flat).toString("hex"),
    }),
  );
  renameSync(staging, path);
};

/**
 * The functions the step evaluator (`src/aiken-evaluator.ts`) runs, listed
 * here so that plain Node can prefetch them.
 */
export const STEP_EVALUATOR_EXPORTS = Object.freeze(
  ["verify_core_step_v1", "hash_state_v1"].map((name) =>
    Object.freeze({ module: "midgard/cek_machine_v1", name, flags: [] }),
  ),
);

const heavyCompile = (action) => withResource("memory-heavy-build", action);

const logLine = (line) => process.stderr.write(`${line}\n`);

/**
 * The Flat program of each function, read from the cache or compiled into
 * it. The options exist for the cache's own tests: `root` names the checkout
 * whose sources are compiled, `compiler` the aiken binary, `lease` what a
 * compile runs under, `log` where compiles are reported.
 */
export const exportAikenFunctions = async (
  functions,
  {
    root = repositoryRoot,
    compiler = aikenBinary(),
    lease = heavyCompile,
    log = logLine,
  } = {},
) => {
  const tree = { sourceHash: blueprintSourceHash(root), compiler };
  const directory = resolve(root, "onchain/aiken/build/aiken-exports");
  mkdirSync(directory, { recursive: true });
  return Promise.all(
    functions.map(async (fn) => {
      const inputs = keyInputs(tree, fn);
      const key = keyOf(inputs);
      const path = join(directory, `${key}.json`);
      const label = `${fn.module}.${fn.name}`;
      let flat = readEntry(path, key);
      if (flat !== undefined) return flat;
      if (process.env.MIDGARD_REQUIRE_PREFETCHED_AIKEN_EXPORTS === "1")
        throw new Error(
          `MIDGARD_REQUIRE_PREFETCHED_AIKEN_EXPORTS=1 forbids compiling ${label}, and no prefetched export has its compiler inputs (key ${key})`,
        );
      await withBuildLock(
        {
          lock: `${path}.lock`,
          ready: () => (flat = readEntry(path, key)) !== undefined,
          timeoutMs: LOCK_WAIT_MS,
        },
        () =>
          lease(async () => {
            log(`[aiken-evaluator] exporting ${label} (key ${key})`);
            const started = Date.now();
            const compiled = await compile({ root, compiler, ...fn });
            writeEntry(path, inputs, compiled);
            log(
              `[aiken-evaluator] exported ${label} in ${String(Math.round((Date.now() - started) / 1000))} s`,
            );
          }),
      );
      flat ??= readEntry(path, key);
      if (flat === undefined)
        throw new Error(`the cached export at ${path} does not verify`);
      return flat;
    }),
  );
};

if (
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
)
  await exportAikenFunctions(STEP_EVALUATOR_EXPORTS);
