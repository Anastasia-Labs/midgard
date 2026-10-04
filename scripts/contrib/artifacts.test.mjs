import assert from "node:assert/strict";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { execFileSync } from "node:child_process";
import { tmpdir } from "node:os";
import { setTimeout as delay } from "node:timers/promises";
import { resolve } from "node:path";
import test, { after } from "node:test";

import { runArtifact, withScratchCheckout } from "./artifacts.mjs";
import { atomicJson, inputIdentity, outputIdentity } from "./files.mjs";
import { fixture } from "./fixture.test-support.mjs";
import { verifyReceipt } from "./receipts.mjs";

// Tiny compiler fixtures do not consume real build capacity. Their private
// registry keeps a real native/workspace build from exhausting a marker deadline.
const fixtureRegistry = mkdtempSync(
  resolve(tmpdir(), "midgard-artifact-tests-"),
);
const previousTmpdir = process.env.TMPDIR;
process.env.TMPDIR = fixtureRegistry;
after(() => {
  if (previousTmpdir === undefined) delete process.env.TMPDIR;
  else process.env.TMPDIR = previousTmpdir;
  rmSync(fixtureRegistry, { recursive: true, force: true });
});

test("scratch candidates preserve staged deletions and rename removals", async (t) => {
  const root = fixture(t);
  const removed = "demo/example/src/index.ts";
  const renamed = "demo/example/src/replacement.ts";
  execFileSync("git", ["mv", removed, renamed], { cwd: root });
  const directory = mkdtempSync(resolve(tmpdir(), "midgard-staged-removal-"));
  t.after(() => rmSync(directory, { recursive: true, force: true }));
  await withScratchCheckout(root, directory, async (scratch) => {
    assert.equal(existsSync(resolve(scratch, removed)), false);
    assert.equal(
      readFileSync(resolve(scratch, renamed), "utf8"),
      readFileSync(resolve(root, renamed), "utf8"),
    );
    assert.equal(
      inputIdentity(scratch, "@repository").sha256,
      inputIdentity(root, "@repository").sha256,
    );
  });
  execFileSync("git", ["rm", "-f", renamed], { cwd: root });
  await withScratchCheckout(root, directory, async (scratch) => {
    assert.equal(existsSync(resolve(scratch, removed)), false);
    assert.equal(existsSync(resolve(scratch, renamed)), false);
    assert.equal(
      inputIdentity(scratch, "@repository").sha256,
      inputIdentity(root, "@repository").sha256,
    );
  });
});

test("scratch checkouts preserve declared Git submodules without treating gitlinks as files", async (t) => {
  const root = fixture(t);
  const directory = mkdtempSync(resolve(tmpdir(), "midgard-gitlink-check-"));
  t.after(() => rmSync(directory, { recursive: true, force: true }));
  const commit = execFileSync("git", ["rev-parse", "HEAD"], {
    cwd: root,
    encoding: "utf8",
  }).trim();
  const path = "technical-spec/Lean4Midgard";
  mkdirSync(resolve(root, path), { recursive: true });
  execFileSync(
    "git",
    ["update-index", "--add", "--cacheinfo", `160000,${commit},${path}`],
    { cwd: root },
  );
  execFileSync(
    "git",
    [
      "-c",
      "user.name=Fixture",
      "-c",
      "user.email=fixture@example.invalid",
      "-c",
      "core.hooksPath=/dev/null",
      "commit",
      "-qm",
      "Gitlink fixture",
    ],
    { cwd: root },
  );
  await withScratchCheckout(root, directory, async (scratch) => {
    assert.ok(existsSync(resolve(scratch, path)));
    assert.equal(
      readFileSync(resolve(scratch, "demo/example/src/index.ts"), "utf8"),
      "export const x = 1;\n",
    );
  });
});

test("scratch overlays candidate sources inside output-named directories", async (t) => {
  const root = fixture(t);
  const path = "demo/example/src/dist/input.ts";
  mkdirSync(resolve(root, path, ".."), { recursive: true });
  writeFileSync(resolve(root, path), "old");
  execFileSync("git", ["add", path], { cwd: root });
  execFileSync(
    "git",
    [
      "-c",
      "user.name=Fixture",
      "-c",
      "user.email=fixture@example.invalid",
      "-c",
      "core.hooksPath=/dev/null",
      "commit",
      "-qm",
      "Source fixture",
    ],
    { cwd: root },
  );
  writeFileSync(resolve(root, path), "candidate");
  const directory = mkdtempSync(resolve(tmpdir(), "midgard-source-overlay-"));
  t.after(() => rmSync(directory, { recursive: true, force: true }));
  await withScratchCheckout(root, directory, async (scratch) => {
    assert.equal(readFileSync(resolve(scratch, path), "utf8"), "candidate");
  });
});

test("artifact sync refuses destination input drift instead of rebinding unchecked bytes", async (t) => {
  const root = fixture(t);
  const adapter = mkdtempSync(resolve(tmpdir(), "midgard-sync-adapter-"));
  t.after(() => rmSync(adapter, { recursive: true, force: true }));
  const marker = resolve(adapter, "ready");
  const registry =
    ".agents/skills/regenerating-goldens-and-ledgers/scripts/channels.json";
  mkdirSync(resolve(root, registry, ".."), { recursive: true });
  mkdirSync(resolve(root, "external-inputs"));
  writeFileSync(resolve(root, "external-inputs/limit.json"), "42");
  writeFileSync(resolve(root, "demo/example/generated.json"), "0");
  writeFileSync(
    resolve(root, "demo/example/build.mjs"),
    'import {mkdirSync,writeFileSync} from "node:fs";mkdirSync("dist",{recursive:true});writeFileSync("dist/index.js","export const x=1;");',
  );
  writeFileSync(
    resolve(root, "generate.mjs"),
    `import {readFileSync,writeFileSync} from "node:fs";const input=readFileSync("external-inputs/limit.json","utf8");writeFileSync(${JSON.stringify(marker)},"ready");await new Promise(r=>setTimeout(r,100));writeFileSync("demo/example/generated.json",input);`,
  );
  writeFileSync(
    resolve(root, "check.mjs"),
    'import {readFileSync} from "node:fs";if(readFileSync("external-inputs/limit.json","utf8")!==readFileSync("demo/example/generated.json","utf8"))throw Error("mismatch");',
  );
  atomicJson(resolve(root, registry), {
    inputSets: {},
    channels: [
      {
        id: "sync-race",
        inputs: ["external-inputs/*.json"],
        generators: ["demo/example/src/index.ts", "generate.mjs", "check.mjs"],
        outputs: ["demo/example/generated.json"],
        sync: { run: "node generate.mjs" },
        check: { run: "node check.mjs" },
      },
    ],
  });
  // This fixture has no dependencies. Its package-manager adapter runs the
  // actual tiny compiler recipe while avoiding a network install in this unit test.
  writeFileSync(
    resolve(adapter, "corepack"),
    '#!/bin/sh\nif [ "$1" = "pnpm" ] && [ "$2" = "run" ]; then exec node build.mjs; fi\n',
    { mode: 0o755 },
  );
  const git = (...args) =>
    execFileSync("git", args, { cwd: root, stdio: "pipe" });
  git(
    "add",
    "--",
    registry,
    "external-inputs/limit.json",
    "demo/example/generated.json",
    "demo/example/build.mjs",
    "generate.mjs",
    "check.mjs",
  );
  git(
    "-c",
    "user.name=Fixture",
    "-c",
    "user.email=fixture@example.invalid",
    "-c",
    "core.hooksPath=/dev/null",
    "commit",
    "-qm",
    "Sync fixture",
  );
  const env = { ...process.env, PATH: `${adapter}:${process.env.PATH}` };
  const controller = new AbortController();
  const pending = runArtifact(root, "sync-race", {
    sync: true,
    env,
    signal: controller.signal,
  });
  pending.catch(() => {});
  try {
    const deadline = performance.now() + 5000;
    while (!existsSync(marker) && performance.now() < deadline) await delay(10);
    assert.ok(
      existsSync(marker),
      "generator must actually reach its input read",
    );
    writeFileSync(resolve(root, "external-inputs/limit.json"), "43");
  } catch (error) {
    controller.abort();
    await pending.catch(() => {});
    throw error;
  }
  const refused = await pending;
  assert.equal(refused.status, "failed");
  assert.match(refused.reason, /destination inputs changed/u);
  assert.equal(
    readFileSync(resolve(root, "demo/example/generated.json"), "utf8"),
    "0",
  );
  assert.throws(
    () => verifyReceipt(root, refused.path),
    /not a completed pass/u,
  );
  writeFileSync(resolve(root, "external-inputs/limit.json"), "42");
  const passed = await runArtifact(root, "sync-race", { sync: true, env });
  assert.equal(passed.status, "passed", passed.reason);
  assert.equal(verifyReceipt(root, passed.path).status, "passed");
  assert.equal(
    readFileSync(resolve(root, "demo/example/generated.json"), "utf8"),
    "42",
  );
  writeFileSync(
    resolve(root, ".gitignore"),
    "demo/example/src/ignored-input.ts\n",
  );
  writeFileSync(
    resolve(root, "demo/example/src/ignored-input.ts"),
    "candidate input omitted by Git",
  );
  const incomplete = await runArtifact(root, "sync-race", { sync: true, env });
  assert.equal(
    incomplete.status,
    "failed",
    "unexecuted package inputs cannot earn a pass",
  );
  assert.match(incomplete.reason, /scratch build inputs differ/u);
});

test("artifact checks reject self-mutating compiled consumers and receipts bind later dist changes", async (t) => {
  const root = fixture(t);
  const registry = resolve(
    root,
    ".agents/skills/regenerating-goldens-and-ledgers/scripts/channels.json",
  );
  mkdirSync(resolve(registry, ".."), { recursive: true });
  atomicJson(registry, {
    inputSets: {},
    channels: [
      {
        id: "fixture",
        inputs: ["demo/example/src/**"],
        generators: ["demo/example/src/index.ts"],
        outputs: ["demo/example/generated.json"],
        check: {
          run: "node --input-type=module -e 'await import(\"./demo/example/dist/index.js\")'",
        },
      },
    ],
  });
  const path = resolve(root, "demo/example/dist/index.js");
  const stamp = () =>
    atomicJson(resolve(root, "demo/example/dist/.contrib-build-v1.json"), {
      schema: "midgard-contrib-build/v1",
      root,
      package: "example",
      inputs: inputIdentity(root, "example"),
      outputs: outputIdentity(root, "demo/example/dist"),
      dependencies: [],
    });
  writeFileSync(
    path,
    "import {writeFileSync} from 'node:fs';writeFileSync(new URL(import.meta.url), 'export const x = 2;');",
  );
  stamp();
  const failed = await runArtifact(root, "fixture");
  assert.equal(failed.status, "failed");
  assert.match(failed.reason, /compiled artifacts changed/u);
  writeFileSync(path, "export const x = 1;");
  stamp();
  const passed = await runArtifact(root, "fixture");
  assert.equal(verifyReceipt(root, passed.path).status, "passed");
  writeFileSync(path, "export const x = 3;");
  assert.throws(() => verifyReceipt(root, passed.path), /artifact changed/u);
});

test("artifact receipts invalidate changed or newly added explicit channel inputs outside the package closure", async (t) => {
  const root = fixture(t);
  const registry = resolve(
    root,
    ".agents/skills/regenerating-goldens-and-ledgers/scripts/channels.json",
  );
  mkdirSync(resolve(registry, ".."), { recursive: true });
  mkdirSync(resolve(root, "external-inputs"));
  const input = resolve(root, "external-inputs/limit.json");
  writeFileSync(input, "42");
  atomicJson(registry, {
    inputSets: {},
    channels: [
      {
        id: "external",
        inputs: ["external-inputs/*.json"],
        generators: ["demo/example/src/index.ts"],
        outputs: ["demo/example/generated.json"],
        check: {
          run: 'node --input-type=module -e \'import {readFileSync} from "node:fs";if(JSON.parse(readFileSync("external-inputs/limit.json"))!==42)throw Error("wrong limit");await import("./demo/example/dist/index.js")\'',
        },
      },
    ],
  });
  atomicJson(resolve(root, "demo/example/dist/.contrib-build-v1.json"), {
    schema: "midgard-contrib-build/v1",
    root,
    package: "example",
    inputs: inputIdentity(root, "example"),
    outputs: outputIdentity(root, "demo/example/dist"),
    dependencies: [],
  });
  const receipt = await runArtifact(root, "external");
  assert.equal(verifyReceipt(root, receipt.path).status, "passed");
  writeFileSync(input, "43");
  assert.throws(
    () => verifyReceipt(root, receipt.path),
    /artifact channel inputs changed/u,
  );
  writeFileSync(input, "42");
  writeFileSync(resolve(root, "external-inputs/additional.json"), "1");
  assert.throws(
    () => verifyReceipt(root, receipt.path),
    /artifact channel inputs changed/u,
  );
});
