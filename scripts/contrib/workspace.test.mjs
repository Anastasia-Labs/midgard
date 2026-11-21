import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import {
  readFileSync,
  writeFileSync,
  symlinkSync,
  existsSync,
  mkdirSync,
  rmSync,
} from "node:fs";
import { resolve } from "node:path";
import test from "node:test";

import { atomicJson, sha256 } from "./files.mjs";
import { fixture } from "./fixture.test-support.mjs";
import {
  applyPacket,
  createPacket,
  verifyPacket,
  inspectWorkspace,
} from "./workspace.mjs";
import { validateProgram } from "./program.mjs";

test("workspace inventory reports missing registered worktrees without claiming clean state", (t) => {
  const root = fixture(t);
  const missing = resolve(root, "..", `${root.split("/").at(-1)}-missing`);
  execFileSync("git", ["worktree", "add", "--detach", missing, "HEAD"], {
    cwd: root,
    stdio: "pipe",
  });
  rmSync(missing, { recursive: true, force: true });
  const inventory = inspectWorkspace(root);
  assert.equal(inventory.complete, false);
  const unavailable = inventory.worktrees.find((tree) => tree.root === missing);
  assert.equal(unavailable.inspection, "unavailable");
  assert.equal(unavailable.changes, null);
  assert.equal(unavailable.reason, "ENOENT");
  assert.ok(unavailable.head);
  assert.equal(
    inventory.worktrees.find((tree) => tree.root === root).inspection,
    "available",
  );
  assert.match(
    execFileSync("git", ["worktree", "list", "--porcelain"], {
      cwd: root,
      encoding: "utf8",
    }),
    /prunable/u,
  );
});

test("packets refuse modified destinations, altered contents and symlink escapes before writes", async (t) => {
  const source = fixture(t);
  const destination = fixture(t);
  const path = "demo/example/src/index.ts";
  writeFileSync(resolve(source, path), "export const x = 2;");
  const packetPath = resolve(source, "packet.json");
  const packet = createPacket(source, {
    base: "HEAD",
    files: [path],
    output: packetPath,
  });
  // Fixture commits carry timestamps; use the packet's source base in a
  // destination copy of the same git metadata rather than assume equal SHAs.
  const target = source;
  assert.throws(() => verifyPacket(target, packetPath), /destination changed/u);
  writeFileSync(resolve(source, path), "export const x = 1;\n");
  assert.equal(verifyPacket(target, packetPath).sha256, packet.sha256);
  const changed = structuredClone(packet);
  changed.entries[0].content = Buffer.from("unreviewed").toString("base64");
  changed.sha256 = sha256(
    JSON.stringify(
      Object.fromEntries(
        Object.entries(changed).filter(([key]) => key !== "sha256"),
      ),
    ),
  );
  atomicJson(packetPath, changed);
  assert.throws(() => verifyPacket(target, packetPath), /content\/hash/u);
  assert.equal(
    readFileSync(resolve(source, path), "utf8"),
    "export const x = 1;\n",
  );
  atomicJson(packetPath, packet);
  await applyPacket(target, packetPath);
  assert.equal(
    readFileSync(resolve(target, path), "utf8"),
    "export const x = 2;",
  );
  symlinkSync(destination, resolve(source, "escape"));
  assert.throws(
    () =>
      createPacket(source, {
        base: "HEAD",
        files: ["escape/new.ts"],
        output: packetPath,
      }),
    /symlink escapes/u,
  );
  assert.equal(existsSync(resolve(destination, "new.ts")), false);
});

test("packet paths protect blueprint, secrets and the git index", (t) => {
  const root = fixture(t);
  for (const path of [
    ".git/index",
    "onchain/aiken/plutus.json",
    "demo/.env",
    "demo/secrets/key.json",
    "demo/example/dist/index.js",
    "../outside",
  ]) {
    assert.throws(
      () =>
        createPacket(root, {
          base: "HEAD",
          files: [path],
          output: resolve(root, "packet.json"),
        }),
      /protected|inside/u,
    );
  }
  assert.equal(inspectWorkspace(root).worktrees.length, 1);
});

test("packets permit candidate sources inside output-named directories", async (t) => {
  const root = fixture(t);
  const path = "demo/example/src/dist/input.ts";
  mkdirSync(resolve(root, path, ".."), { recursive: true });
  writeFileSync(resolve(root, path), "candidate");
  const output = resolve(root, "packet.json");
  createPacket(root, { base: "HEAD", files: [path], output });
  // A new source has no base bytes; remove only this fixture's own file.
  const { unlinkSync } = await import("node:fs");
  unlinkSync(resolve(root, path));
  await applyPacket(root, output);
  assert.equal(readFileSync(resolve(root, path), "utf8"), "candidate");
});

test("a late packet cancellation rolls back only the paths it applied", async (t) => {
  const root = fixture(t);
  const first = "demo/example/src/index.ts";
  const second = "demo/example/src/new.ts";
  const original = readFileSync(resolve(root, first), "utf8");
  writeFileSync(resolve(root, first), "export const x = 2;");
  writeFileSync(resolve(root, second), "export const y = 1;");
  const path = resolve(root, "packet.json");
  createPacket(root, { base: "HEAD", files: [first, second], output: path });
  writeFileSync(resolve(root, first), original);
  const { unlinkSync } = await import("node:fs");
  unlinkSync(resolve(root, second));
  let checks = 0;
  const signal = {
    aborted: false,
    throwIfAborted() {
      if (++checks === 3) throw new Error("cancelled after first application");
    },
  };
  await assert.rejects(applyPacket(root, path, { signal }), /cancelled/u);
  assert.equal(readFileSync(resolve(root, first), "utf8"), original);
  assert.equal(existsSync(resolve(root, second)), false);
});

test("programs reject cycles, loose issue relationships and evidence-free acceptance", (t) => {
  const root = fixture(t);
  const path = resolve(root, "program.json");
  const task = {
    id: "one",
    state: "planned",
    dependsOn: [],
    paths: ["demo/example/src/index.ts"],
    requiredReceipts: [],
    issues: [],
  };
  const program = {
    schema: "midgard-work-program/v1",
    id: "fixture",
    tasks: [task],
    decisions: [],
  };
  atomicJson(path, program);
  assert.equal(validateProgram(root, path).tasks.length, 1);
  task.dependsOn = ["one"];
  atomicJson(path, program);
  assert.throws(() => validateProgram(root, path), /cyclic/u);
  task.dependsOn = [];
  task.issues = [{ url: "https://example.com/issue", relation: "same-ish" }];
  atomicJson(path, program);
  assert.throws(() => validateProgram(root, path), /relation/u);
  task.issues = [];
  task.state = "accepted";
  atomicJson(path, program);
  assert.throws(() => validateProgram(root, path), /owner|acceptance/u);
});
