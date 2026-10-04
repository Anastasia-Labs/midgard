import assert from "node:assert/strict";
import { existsSync, readFileSync } from "node:fs";
import { spawn } from "node:child_process";
import { once } from "node:events";
import { pathToFileURL } from "node:url";
import { resolve } from "node:path";
import { setTimeout as delay } from "node:timers/promises";
import test from "node:test";
import { randomUUID } from "node:crypto";

import { fixture } from "./fixture.test-support.mjs";
import { runProcess } from "./process.mjs";
import {
  listResources,
  withResource,
  reclaimResource,
  processIdentity,
  registerOwnedLaunch,
} from "./resources.mjs";
import { atomicJson, json } from "./files.mjs";

test("exclusive resources serialize peers and permit only attested child inheritance", async () => {
  const resource = `fixture:${randomUUID()}`;
  const trace = [];
  const first = withResource(resource, async (env) => {
    trace.push("first");
    await withResource(resource, () => trace.push("nested"), {
      env,
      timeoutMs: 100,
    });
    assert.throws(() => reclaimResource(resource), /live/u);
    await delay(150);
    trace.push("released");
  });
  await delay(25);
  const second = withResource(resource, () => trace.push("second"));
  await Promise.all([first, second]);
  assert.deepEqual(trace, ["first", "nested", "released", "second"]);
  assert.equal(
    listResources().some((entry) => entry.resource === resource),
    false,
  );
});

test("a cancelled waiter does not release another owner", async () => {
  const resource = `fixture:${randomUUID()}`;
  await withResource(resource, async () => {
    const controller = new AbortController();
    const waiter = withResource(
      resource,
      () => assert.fail("cancelled waiter entered"),
      { signal: controller.signal },
    );
    controller.abort();
    await assert.rejects(waiter, /abort/iu);
    assert.equal(
      listResources().find((entry) => entry.resource === resource).state,
      "owned",
    );
  });
});

test("output overflow is bounded and never reported as success", async (t) => {
  const root = fixture(t);
  const logPath = resolve(root, "overflow.log");
  const result = await runProcess({
    argv: [
      process.execPath,
      "-e",
      "setInterval(()=>process.stdout.write('x'.repeat(65536)),1)",
    ],
    cwd: root,
    logPath,
    maxBytes: 1024,
  });
  assert.match(result.reason, /output exceeded/u);
  assert.equal(readFileSync(logPath).length, 1024);
});

test("cancellation joins a grandchild which ignores SIGTERM", async (t) => {
  const root = fixture(t);
  const pidPath = resolve(root, "child.pid");
  const controller = new AbortController();
  const script = `const {spawn}=require('node:child_process');spawn(process.execPath,['-e',"process.on('SIGTERM',()=>{});require('node:fs').writeFileSync(process.argv[1],String(process.pid));setInterval(()=>{},1000)",process.argv[1]],{stdio:'inherit'});setInterval(()=>{},1000);`;
  const running = runProcess({
    argv: [process.execPath, "-e", script, pidPath],
    cwd: root,
    logPath: resolve(root, "cancel.log"),
    signal: controller.signal,
  });
  for (let attempt = 0; !existsSync(pidPath) && attempt < 100; attempt += 1)
    await delay(20);
  assert.ok(existsSync(pidPath));
  const pid = Number(readFileSync(pidPath, "utf8"));
  controller.abort();
  const result = await running;
  assert.equal(result.reason, "cancelled");
  assert.ok(
    result.durationMs >= 1500,
    "waited for SIGKILL escalation after the child's handler was ready",
  );
  // A killed orphan can be a zombie until PID 1 reaps it; it can no longer
  // execute or write. Observe state rather than claiming pid absence.
  if (processIdentity(pid)) {
    const stat = readFileSync(`/proc/${pid}/stat`, "utf8");
    assert.equal(stat.slice(stat.lastIndexOf(")") + 2).split(" ")[0], "Z");
  }
});

test("a monotonic deadline fails a stalled child", async (t) => {
  const root = fixture(t);
  const result = await runProcess({
    argv: [process.execPath, "-e", "setInterval(()=>{},1000)"],
    cwd: root,
    logPath: resolve(root, "timeout.log"),
    timeoutMs: 50,
  });
  assert.match(result.reason, /deadline exceeded/u);
});

test("another PID namespace and an unfinished launch remain unknown and cannot be reclaimed", async () => {
  const resource = `fixture:${randomUUID()}`;
  await withResource(resource, async (env) => {
    const lease = listResources().find((entry) => entry.resource === resource);
    const path = resolve(lease.path, "owner.json");
    const original = json(path);
    try {
      atomicJson(path, {
        ...original,
        owner: { ...original.owner, namespace: "pid:[0]" },
      });
      assert.equal(
        listResources().find((entry) => entry.resource === resource).state,
        "unknown",
      );
      assert.throws(() => reclaimResource(resource), /unobservable/u);
      atomicJson(path, original);
      const pending = registerOwnedLaunch(env);
      try {
        atomicJson(path, {
          ...original,
          owner: { ...original.owner, pid: 999999999 },
        });
        assert.equal(
          listResources().find((entry) => entry.resource === resource).state,
          "unknown",
        );
        assert.throws(() => reclaimResource(resource), /unobservable/u);
      } finally {
        pending.joined();
      }
    } finally {
      atomicJson(path, original);
    }
  });
});

test("a killed lease owner cannot be reclaimed while its detached managed child still runs", async (t) => {
  const root = fixture(t);
  const resource = `fixture:${randomUUID()}`;
  const pidPath = resolve(root, "survivor.pid");
  const resourcesUrl = pathToFileURL(
    resolve(import.meta.dirname, "resources.mjs"),
  ).href;
  const processUrl = pathToFileURL(
    resolve(import.meta.dirname, "process.mjs"),
  ).href;
  const childCode =
    "process.on('SIGTERM',()=>{});require('node:fs').writeFileSync(process.argv[1],String(process.pid));setInterval(()=>{},1000)";
  const script = `import {withResource} from ${JSON.stringify(resourcesUrl)};import {runProcess} from ${JSON.stringify(processUrl)};await withResource(${JSON.stringify(resource)},async env=>runProcess({argv:[process.execPath,'-e',${JSON.stringify(childCode)},${JSON.stringify(pidPath)}],cwd:${JSON.stringify(root)},env,logPath:${JSON.stringify(resolve(root, "survivor.log"))}}));`;
  const holder = spawn(
    process.execPath,
    ["--input-type=module", "-e", script],
    { detached: true, stdio: "ignore" },
  );
  let childPid;
  try {
    for (let attempt = 0; !existsSync(pidPath) && attempt < 200; attempt += 1)
      await delay(10);
    assert.ok(existsSync(pidPath), "managed child became ready");
    childPid = Number(readFileSync(pidPath, "utf8"));
    const exited = once(holder, "exit");
    process.kill(holder.pid, "SIGKILL");
    await exited;
    assert.equal(
      listResources().find((entry) => entry.resource === resource).state,
      "owned",
    );
    assert.throws(() => reclaimResource(resource), /live/u);
  } finally {
    if (childPid) {
      process.kill(-childPid, "SIGKILL");
      for (
        let attempt = 0;
        listResources().find((entry) => entry.resource === resource)?.state ===
          "owned" && attempt < 200;
        attempt += 1
      )
        await delay(10);
    }
    if (holder.exitCode === null && holder.signalCode === null) {
      process.kill(-holder.pid, "SIGKILL");
      await once(holder, "exit");
    }
    const lease = listResources().find((entry) => entry.resource === resource);
    if (lease?.state === "abandoned") reclaimResource(resource);
  }
  assert.equal(
    listResources().some((entry) => entry.resource === resource),
    false,
  );
});
