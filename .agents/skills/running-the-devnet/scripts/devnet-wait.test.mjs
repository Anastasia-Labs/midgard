// Tests for devnet-wait.mjs. Every server is a fake started inside this test
// process on an ephemeral loopback port; nothing here touches a real devnet.
import { spawn } from "node:child_process";
import { appendFileSync, mkdtempSync, writeFileSync } from "node:fs";
import { createServer } from "node:http";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { after, test } from "node:test";
import assert from "node:assert/strict";
import { fileURLToPath } from "node:url";

import { classifyResponse, parseCli } from "./devnet-wait.mjs";

const SCRIPT = fileURLToPath(new URL("./devnet-wait.mjs", import.meta.url));
const servers = [];
const scratch = mkdtempSync(join(tmpdir(), "devnet-wait-test-"));

after(async () => {
  await Promise.all(
    servers.map((server) => new Promise((resolve) => server.close(resolve))),
  );
});

/** Starts a fake service; `respond(requestCount)` returns [status, body]. */
const serve = async (respond) => {
  let count = 0;
  const server = createServer((request, response) => {
    count += 1;
    const [status, body] = respond(count);
    response.writeHead(status, { "content-type": "application/json" });
    response.end(JSON.stringify(body));
  });
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  servers.push(server);
  return `http://127.0.0.1:${server.address().port}/readyz`;
};

/** A loopback URL that refuses connections: bind, note the port, close. */
const refusedUrl = async () => {
  const server = createServer();
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  const { port } = server.address();
  await new Promise((resolve) => server.close(resolve));
  return `http://127.0.0.1:${port}/readyz`;
};

// Asynchronous spawn, so the fake servers in this process keep answering.
const run = (args) =>
  new Promise((resolve) => {
    const child = spawn(process.execPath, [
      SCRIPT,
      "--interval",
      "50",
      ...args,
    ]);
    let output = "";
    child.stdout.on("data", (chunk) => (output += chunk));
    child.stderr.on("data", (chunk) => (output += chunk));
    child.on("close", (code) => resolve({ code, output }));
  });

test("ready: every URL answers 200 with ready=true", async () => {
  const node = await serve(() => [200, { ready: true, reasons: [] }]);
  const da = await serve(() => [200, { ok: true }]);
  const { code, output } = await run(["--url", node, "--url", da]);
  assert.equal(code, 0, output);
  assert.match(output, /devnet-wait: ready/);
});

test("ready after a slow start: 503 twice, then 200", async () => {
  const url = await serve((n) =>
    n < 3
      ? [503, { ready: false, reasons: ["warming"] }]
      : [200, { ready: true }],
  );
  const { code, output } = await run(["--url", url, "--timeout", "5"]);
  assert.equal(code, 0, output);
});

test("timed out: the service answers but never becomes ready", async () => {
  const url = await serve(() => [
    503,
    { ready: false, reasons: ["history_owner_not_ready"] },
  ]);
  const { code, output } = await run(["--url", url, "--timeout", "0.5"]);
  assert.equal(code, 2, output);
  assert.match(output, /history_owner_not_ready/);
});

test("unreachable: a refused connection is exit 3, not a timeout", async () => {
  const url = await refusedUrl();
  const { code, output } = await run(["--url", url, "--timeout", "0.5"]);
  assert.equal(code, 3, output);
  assert.match(output, /never-answered/);
});

test("unreachable dominates: one refused URL beside a ready one is exit 3", async () => {
  const ready = await serve(() => [200, { ready: true }]);
  const refused = await refusedUrl();
  const { code } = await run([
    "--url",
    ready,
    "--url",
    refused,
    "--timeout",
    "0.5",
  ]);
  assert.equal(code, 3);
});

test("no services configured is exit 3, never 0", async () => {
  const { code, output } = await run(["--timeout", "0.5"]);
  assert.equal(code, 3, output);
  assert.match(output, /no services configured/);
});

test("usage errors are exit 3", async () => {
  assert.equal((await run(["--url", "not a url"])).code, 3);
  assert.equal((await run(["--url", "ftp://x/"])).code, 3);
  assert.equal((await run(["--bogus"])).code, 3);
  assert.ok(parseCli(["--timeout", "0"]).error);
  assert.ok(parseCli(["--pid", "abc"]).error);
});

test("crashed: a fatal line appended to --log is exit 1 with the tail", async () => {
  const url = await serve(() => [503, { ready: false, reasons: [] }]);
  const log = join(scratch, "node.log");
  writeFileSync(log, "old content\n");
  const pending = run(["--url", url, "--log", log, "--timeout", "5"]);
  setTimeout(
    () =>
      appendFileSync(
        log,
        "starting\nStartup protocol initialization failed: manifest mismatch\n",
      ),
    200,
  );
  const { code, output } = await pending;
  assert.equal(code, 1, output);
  assert.match(output, /--- tail of .*node\.log ---/);
  assert.match(output, /manifest mismatch/);
});

test("a fatal line already in the log before the wait is not a new crash", async () => {
  const url = await serve(() => [200, { ready: true }]);
  const log = join(scratch, "old-crash.log");
  writeFileSync(log, '{"event":"l1_view_unavailable_exit","exitCode":70}\n');
  const { code, output } = await run(["--url", url, "--log", log]);
  assert.equal(code, 0, output);
});

test("crashed: a --pid that has exited is exit 1", async () => {
  const url = await serve(() => [503, { ready: false }]);
  const child = spawn(process.execPath, ["-e", "process.exit(0)"]);
  await new Promise((resolve) => child.on("close", resolve));
  const { code, output } = await run([
    "--url",
    url,
    "--pid",
    String(child.pid),
    "--timeout",
    "2",
  ]);
  assert.equal(code, 1, output);
  assert.match(output, /is not running/);
});

test("crashed: a quarantined DA committee L1 source is terminal", async () => {
  const url = await serve(() => [
    503,
    {
      ready: false,
      l1Source: { status: "quarantined", quarantineReason: "fork" },
    },
  ]);
  const { code, output } = await run(["--url", url, "--timeout", "5"]);
  assert.equal(code, 1, output);
  assert.match(output, /quarantined: fork/);
});

// Negative self-tests: answers that look healthy at a glance must not pass.
test("negative: HTTP 200 carrying ready=false is not ready", async () => {
  const url = await serve(() => [200, { ready: false, reasons: ["x"] }]);
  const { code } = await run(["--url", url, "--timeout", "0.5"]);
  assert.notEqual(code, 0);
  assert.equal(code, 2);
});

test("negative: Kupo's 202 during replay is not ready", () => {
  assert.equal(classifyResponse(202, "{}").state, "not_ready");
  assert.equal(
    classifyResponse(200, '{"connection_status":"disconnected"}').state,
    "not_ready",
  );
  assert.equal(
    classifyResponse(200, '{"connection_status":"connected"}').state,
    "ready",
  );
  assert.equal(classifyResponse(401, "").state, "not_ready");
});
