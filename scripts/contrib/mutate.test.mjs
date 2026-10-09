import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { createHash } from "node:crypto";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { createRequire } from "node:module";
import { dirname, resolve } from "node:path";
import test from "node:test";
import { fileURLToPath, pathToFileURL } from "node:url";

import { main, parse } from "../contrib.mjs";
import { fixture } from "./fixture.test-support.mjs";
import { journalPath, mutate, restoreMutant } from "./mutate.mjs";

// A hook running these tests exports GIT_DIR; the fixture's `git init` would
// re-initialise the outer repository under it.
for (const key of Object.keys(process.env))
  if (key.startsWith("GIT_")) delete process.env[key];

const checkout = fileURLToPath(new URL("../..", import.meta.url));
// The workspace root's TypeScript (through its linter), which Repository
// Tools CI installs without the packages.
const typescript = dirname(
  createRequire(
    createRequire(resolve(checkout, "demo/package.json")).resolve(
      "typescript-eslint",
    ),
  ).resolve("typescript/package.json"),
);
const SOURCE = [
  "// The limit a value may reach.",
  "const limit: number = 3;",
  "",
  "export const allowed = (value: number): boolean => value <= limit;",
  "export const twice = (value: number) => value * 2 + value * 2;",
  "",
].join("\n");
const TARGET = "demo/example/src/index.ts";
const sha = (path) =>
  createHash("sha256").update(readFileSync(path)).digest("hex");

const workspace = (t) => {
  const root = fixture(t);
  mkdirSync(resolve(root, "demo/example/node_modules"));
  symlinkSync(
    typescript,
    resolve(root, "demo/example/node_modules/typescript"),
  );
  writeFileSync(resolve(root, TARGET), SOURCE);
  return root;
};

const killed = {
  package: "example",
  status: "failed",
  counts: { executed: 2, passed: 1, failed: 1 },
  failures: [
    {
      file: "tests/limit.test.ts",
      test: "refuses 4",
      message: "expected false",
    },
  ],
  path: "/receipt.json",
};
const passed = {
  package: "example",
  status: "passed",
  counts: { executed: 2, passed: 2, failed: 0 },
  failures: [],
};
const MUTANT = { target: TARGET, from: "value <= limit", to: "value < limit" };

test("a replacement that matches nothing, matches twice, does not parse, or changes no emitted code is refused, untouched and unrun", async (t) => {
  const root = workspace(t);
  const before = sha(resolve(root, TARGET));
  const run = () => assert.fail("a refused mutant must not run tests");
  for (const [from, to, reason] of [
    ["value <== limit", "value < limit", /does not occur/u],
    ["value * 2", "value * 3", /occurs 2 times .*lines 5, 5/u],
    ["value <= limit", "value <= ", /does not parse/u],
    ["The limit", "The bound", /only comments, types or layout/u],
    [
      "limit: number",
      "limit: bigint | number",
      /only comments, types or layout/u,
    ],
    [
      "value <= limit;",
      "value   <=\n  limit ;",
      /only comments, types or layout/u,
    ],
    ["value <= limit", "value <= limit", /only comments, types or layout/u],
  ]) {
    const result = await mutate(root, { target: TARGET, from, to }, { run });
    assert.equal(result.status, "refused", `${from} -> ${to}`);
    assert.equal(result.exitCode, 2);
    assert.match(result.reason, reason);
  }
  for (const [options, reason] of [
    [{ ...MUTANT, target: "demo/example/package.json" }, /not a TypeScript/u],
    [{ ...MUTANT, expect: "(" }, /--expect is not a regular expression/u],
    [{ ...MUTANT, from: "" }, /--from must name/u],
  ])
    assert.match((await mutate(root, options, { run })).reason, reason);
  assert.equal(sha(resolve(root, TARGET)), before);
  assert.equal(existsSync(journalPath(root)), false);
});

test("the tests run with the mutant in place, and the file is restored byte for byte", async (t) => {
  const root = workspace(t);
  const before = sha(resolve(root, TARGET));
  let seen;
  const result = await mutate(root, MUTANT, {
    run: async (_root, name, options) => {
      seen = {
        name,
        options,
        text: readFileSync(resolve(root, TARGET), "utf8"),
      };
      return killed;
    },
  });
  assert.match(seen.text, /value < limit;/u);
  assert.equal(seen.name, "example");
  assert.deepEqual(seen.options.related, [TARGET]);
  assert.equal(seen.options.proofKind, "causal-guard-mutant");
  assert.equal(result.status, "killed");
  assert.equal(result.exitCode, 0);
  assert.deepEqual(result.mutation.line, 4);
  assert.equal(sha(resolve(root, TARGET)), before);
  assert.equal(existsSync(journalPath(root)), false);
});

test("a mutant no test kills, or kills only through another failure than --expect, fails", async (t) => {
  const root = workspace(t);
  const survived = await mutate(root, MUTANT, { run: async () => passed });
  assert.equal(survived.status, "survived");
  assert.equal(survived.exitCode, 1);
  const elsewhere = await mutate(
    root,
    { ...MUTANT, expect: "refuses 5" },
    { run: async () => killed },
  );
  assert.equal(elsewhere.status, "killed-elsewhere");
  assert.equal(elsewhere.exitCode, 1);
  const expected = await mutate(
    root,
    { ...MUTANT, expect: "refuses 4" },
    { run: async () => killed },
  );
  assert.equal(expected.status, "killed");
  const unreached = await mutate(root, MUTANT, {
    run: async () => ({ status: "not-reached", exitCode: 0 }),
  });
  assert.equal(unreached.status, "unreached");
  assert.equal(unreached.exitCode, 1);
  const broken = await mutate(root, MUTANT, {
    run: async () => ({
      ...killed,
      counts: { executed: 0, failed: 0, setupErrors: 1 },
    }),
  });
  assert.equal(broken.status, "inconclusive");
  assert.equal(broken.exitCode, 3);
});

test("a run that throws still restores the file; a mutant that does not build is inconclusive", async (t) => {
  const root = workspace(t);
  const before = sha(resolve(root, TARGET));
  for (const [message, status, exitCode] of [
    ["database unreachable", "failed", 1],
    ["prerequisite build failed: /build/receipt.json", "inconclusive", 3],
  ]) {
    const result = await mutate(root, MUTANT, {
      run: async () => {
        throw new Error(message);
      },
    });
    assert.equal(result.status, status);
    assert.equal(result.exitCode, exitCode);
    assert.match(result.reason, new RegExp(message, "u"));
    assert.equal(sha(resolve(root, TARGET)), before);
    assert.equal(existsSync(journalPath(root)), false);
  }
});

test("a dist the run rebuilt from the mutant is rebuilt from the original", async (t) => {
  const root = workspace(t);
  const stamp = resolve(root, "demo/example/dist/.contrib-build-v1.json");
  const builds = [];
  const result = await mutate(root, MUTANT, {
    run: async () => {
      writeFileSync(stamp, "built from the mutant");
      return killed;
    },
    build: async (_root, name) => {
      builds.push([name, readFileSync(resolve(root, TARGET), "utf8")]);
      writeFileSync(stamp, "built from the original");
      return { exitCode: 0 };
    },
  });
  assert.deepEqual(result.rebuilt, ["example"]);
  assert.deepEqual(builds, [["example", SOURCE]]);
  assert.equal(existsSync(journalPath(root)), false);
});

test("a file someone changed while the mutant was in place is left alone, and the journal kept", async (t) => {
  const root = workspace(t);
  await assert.rejects(
    mutate(root, MUTANT, {
      run: async () => {
        writeFileSync(resolve(root, TARGET), "export const edited = true;\n");
        return killed;
      },
    }),
    /changed while the mutant was in place; it was left as it is/u,
  );
  assert.equal(
    readFileSync(resolve(root, TARGET), "utf8"),
    "export const edited = true;\n",
  );
  const record = JSON.parse(readFileSync(journalPath(root), "utf8"));
  assert.equal(Buffer.from(record.original, "base64").toString(), SOURCE);
});

// A child process that puts the mutant in place, says so, and waits for the
// run to be aborted.
const child = (root) => {
  const script = resolve(root, "mutate-child.mjs");
  writeFileSync(
    script,
    `import { mutate } from ${JSON.stringify(pathToFileURL(resolve(checkout, "scripts/contrib/mutate.mjs")).href)};
const result = await mutate(${JSON.stringify(root)}, ${JSON.stringify(MUTANT)}, {
  run: (_root, _name, { signal }) => new Promise((_, reject) => {
    // What keeps a real run alive: its test process.
    const alive = setInterval(() => {}, 1000);
    signal.addEventListener("abort", () => {
      clearInterval(alive);
      reject(signal.reason);
    });
    console.log("ready");
  }),
});
console.log(JSON.stringify(result));
`,
  );
  const process = spawn("node", [script], {
    stdio: ["ignore", "pipe", "inherit"],
  });
  let output = "";
  const ready = new Promise((resolveReady) =>
    process.stdout.on("data", (chunk) => {
      output += chunk;
      if (output.includes("ready")) resolveReady();
    }),
  );
  const exited = new Promise((resolveExit) =>
    process.on("exit", (code, signal) => resolveExit({ code, signal })),
  );
  return { process, ready, exited, output: () => output };
};

for (const signal of ["SIGINT", "SIGTERM", "SIGHUP"])
  test(`${signal} restores the file before the process ends`, async (t) => {
    const root = workspace(t);
    const before = sha(resolve(root, TARGET));
    const run = child(root);
    await run.ready;
    assert.notEqual(sha(resolve(root, TARGET)), before);
    run.process.kill(signal);
    assert.deepEqual(await run.exited, { code: 0, signal: null });
    assert.equal(sha(resolve(root, TARGET)), before);
    assert.equal(existsSync(journalPath(root)), false);
    const result = JSON.parse(run.output().trim().split("\n").at(-1));
    assert.match(result.reason, new RegExp(`interrupted by ${signal}`, "u"));
  });

test("a killed run leaves a journal: every later mutate refuses until restore puts the original back", async (t) => {
  const root = workspace(t);
  const before = sha(resolve(root, TARGET));
  const run = child(root);
  await run.ready;
  run.process.kill("SIGKILL");
  await run.exited;
  assert.notEqual(sha(resolve(root, TARGET)), before);
  const refused = await mutate(root, MUTANT, {
    run: () => assert.fail("must not run over an unrestored mutant"),
  });
  assert.equal(refused.exitCode, 1);
  assert.match(refused.reason, /was never restored.*contrib mutate restore/u);
  const restored = await restoreMutant(root);
  assert.equal(restored.status, "restored");
  assert.equal(sha(resolve(root, TARGET)), before);
  assert.equal(existsSync(journalPath(root)), false);
  assert.equal((await restoreMutant(root)).status, "clean");
});

test("mutation text is taken verbatim, and the mutation options belong to mutate alone", async (t) => {
  const options = parse(["mutate", "--from", "--count", "--to", ""]);
  assert.equal(options.from, "--count");
  assert.equal(options.to, "");
  assert.throws(() => parse(["mutate", "--from"]), /--from requires a value/u);
  const errors = [];
  t.mock.method(console, "error", (line) => errors.push(line));
  assert.equal(await main(["test", "--package", "x", "--target", TARGET]), 1);
  assert.equal(await main(["mutate", "restore", "--from", "x"]), 1);
  assert.match(errors[0], /--target is supported only for mutate/u);
  assert.match(errors[1], /--from is supported only for mutate/u);
});
