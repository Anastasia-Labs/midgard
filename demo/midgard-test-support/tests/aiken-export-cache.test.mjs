import assert from "node:assert/strict";
import {
  chmodSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";
import { setTimeout as sleep } from "node:timers/promises";

import { exportAikenFunctions } from "../aiken-export-cache.js";

// The Flat program every stand-in compiler exports, and its CBOR wrapping.
const PROGRAM = Uint8Array.from([1, 1, 0, 0x48]);
const COMPILED_CODE = `44${Buffer.from(PROGRAM).toString("hex")}`;

const VERIFY = { module: "midgard/probe", name: "verify", flags: [] };

// A tree with every file the source stamp reads, and stand-in compilers that
// log each invocation and print an export.
const withTree = (t) => {
  const root = mkdtempSync(join(tmpdir(), "midgard-aiken-export-cache-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  const write = (path, contents) => {
    mkdirSync(dirname(join(root, path)), { recursive: true });
    writeFileSync(join(root, path), contents);
    return join(root, path);
  };
  write("onchain/aiken/aiken.toml", 'name = "midgard/selftest"\n');
  write("onchain/aiken/aiken.lock", "# lock\n");
  write("onchain/aiken/validators/probe.ak", "validator probe { }\n");
  write("onchain/aiken/lib/midgard/probe.ak", "pub const x = 1\n");
  write("onchain/aiken/env/default.ak", "pub const network = 0\n");
  write("demo/flake.lock", '{"nodes":{}}\n');
  const callLog = join(root, "compiler-calls");
  const compiler = (name) => {
    const path = write(
      `bin/${name}/aiken`,
      `#!/bin/sh\necho "$0 $*" >> "${callLog}"\nprintf '{"compiledCode":"${COMPILED_CODE}"}'\n`,
    );
    chmodSync(path, 0o755);
    return path;
  };
  const calls = () => {
    try {
      return readFileSync(callLog, "utf8").trim().split("\n");
    } catch {
      return [];
    }
  };
  const pinned = compiler("pinned");
  const exportWith = (functions, { using = pinned, lease } = {}) =>
    exportAikenFunctions(functions, {
      root,
      compiler: using,
      lease: lease ?? ((action) => action()),
      log: () => {},
    });
  const entries = () =>
    readdirSync(join(root, "onchain/aiken/build/aiken-exports"))
      .filter((name) => name.endsWith(".json"))
      .map((name) => join(root, "onchain/aiken/build/aiken-exports", name));
  return { write, compiler, calls, exportWith, entries };
};

test("a miss compiles under the lease and a hit compiles nothing", async (t) => {
  const { calls, exportWith } = withTree(t);
  let leased = 0;
  const lease = async (action) => {
    leased += 1;
    await action();
  };
  assert.deepEqual(await exportWith([VERIFY], { lease }), [PROGRAM]);
  assert.equal(calls().length, 1);
  assert.match(calls()[0], /aiken export -m midgard\/probe -n verify$/u);
  assert.equal(leased, 1);
  assert.deepEqual(await exportWith([VERIFY], { lease }), [PROGRAM]);
  assert.equal(calls().length, 1);
  assert.equal(leased, 1);
});

test("changing any compiler input compiles again; restoring it hits again", async (t) => {
  const { write, compiler, calls, exportWith } = withTree(t);
  await exportWith([VERIFY]);
  const changes = [
    [
      "a lib module",
      () => write("onchain/aiken/lib/midgard/probe.ak", "pub const x = 2\n"),
    ],
    [
      "a validator",
      () =>
        write(
          "onchain/aiken/validators/probe.ak",
          "validator probe { else(_) { fail } }\n",
        ),
    ],
    [
      "an env module",
      () => write("onchain/aiken/env/default.ak", "pub const network = 1\n"),
    ],
    [
      "a new module",
      () => write("onchain/aiken/lib/midgard/added.ak", "pub const y = 1\n"),
    ],
    [
      "aiken.toml",
      () => write("onchain/aiken/aiken.toml", 'name = "midgard/other"\n'),
    ],
    ["aiken.lock", () => write("onchain/aiken/aiken.lock", "# other lock\n")],
    [
      "the compiler pin",
      () => write("demo/flake.lock", '{"nodes":{"aiken":{}}}\n'),
    ],
  ];
  for (const [input, change] of changes) {
    const before = calls().length;
    change();
    await exportWith([VERIFY]);
    assert.equal(calls().length, before + 1, `${input} is not in the key`);
    await exportWith([VERIFY]);
    assert.equal(
      calls().length,
      before + 1,
      `${input}: no hit after compiling`,
    );
  }
  // Each request compiles with what it changed.
  const requests = [
    [
      "the compiler binary",
      VERIFY,
      compiler("other"),
      /\/bin\/other\/aiken export -m midgard\/probe -n verify$/u,
    ],
    [
      "the flags",
      { ...VERIFY, flags: ["--trace-level", "verbose"] },
      undefined,
      /-n verify --trace-level verbose$/u,
    ],
    [
      "the module",
      { ...VERIFY, module: "midgard/other" },
      undefined,
      /-m midgard\/other -n verify$/u,
    ],
    [
      "the function",
      { ...VERIFY, name: "hash" },
      undefined,
      /-m midgard\/probe -n hash$/u,
    ],
  ];
  for (const [input, request, using, compiled] of requests) {
    const before = calls().length;
    await exportWith([request], { using });
    assert.equal(calls().length, before + 1, `${input} is not in the key`);
    assert.match(calls().at(-1), compiled);
  }
  const before = calls().length;
  write("onchain/aiken/lib/midgard/probe.ak", "pub const x = 3\n");
  await exportWith([VERIFY]);
  write("onchain/aiken/lib/midgard/probe.ak", "pub const x = 2\n");
  await exportWith([VERIFY]);
  assert.equal(calls().length, before + 1);
});

test("an entry that does not verify is compiled again", async (t) => {
  const { calls, exportWith, entries } = withTree(t);
  await exportWith([VERIFY]);
  const [entry] = entries();
  const record = JSON.parse(readFileSync(entry, "utf8"));
  writeFileSync(entry, JSON.stringify({ ...record, flat: "01010049" }));
  assert.deepEqual(await exportWith([VERIFY]), [PROGRAM]);
  assert.equal(calls().length, 2);
  // An entry under another key's name: its own inputs do not give that key.
  await exportWith([{ ...VERIFY, name: "hash" }]);
  const [first, second] = entries();
  renameSync(first, `${first}.moved`);
  renameSync(second, first);
  await exportWith([VERIFY, { ...VERIFY, name: "hash" }]);
  assert.equal(calls().length, 5);
});

test("misses of one key at once compile it once", async (t) => {
  const { calls, exportWith } = withTree(t);
  // The compile holds its lock long enough for the other miss to wait on it.
  const lease = async (action) => {
    await sleep(500);
    await action();
  };
  const results = await Promise.all([
    exportWith([VERIFY], { lease }),
    exportWith([VERIFY], { lease }),
  ]);
  assert.deepEqual(results, [[PROGRAM], [PROGRAM]]);
  assert.equal(calls().length, 1);
});

test("a job that requires prefetched exports fails on a miss and compiles nothing", async (t) => {
  const { calls, write, exportWith } = withTree(t);
  await exportWith([VERIFY]);
  const previous = process.env.MIDGARD_REQUIRE_PREFETCHED_AIKEN_EXPORTS;
  process.env.MIDGARD_REQUIRE_PREFETCHED_AIKEN_EXPORTS = "1";
  t.after(() => {
    if (previous === undefined)
      delete process.env.MIDGARD_REQUIRE_PREFETCHED_AIKEN_EXPORTS;
    else process.env.MIDGARD_REQUIRE_PREFETCHED_AIKEN_EXPORTS = previous;
  });
  assert.deepEqual(await exportWith([VERIFY]), [PROGRAM]);
  write("onchain/aiken/lib/midgard/probe.ak", "pub const x = 2\n");
  await assert.rejects(
    exportWith([VERIFY]),
    /MIDGARD_REQUIRE_PREFETCHED_AIKEN_EXPORTS=1 forbids compiling midgard\/probe\.verify/u,
  );
  assert.equal(calls().length, 1);
});
