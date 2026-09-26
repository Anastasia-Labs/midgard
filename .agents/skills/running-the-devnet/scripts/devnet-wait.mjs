#!/usr/bin/env node
// Waits until every given readiness URL reports ready, or says why not.
//
//   node devnet-wait.mjs --url <readyz-or-health-url>... [--timeout S]
//        [--log <file>]... [--pid <pid>]... [--fatal-pattern <regex>]...
//        [--interval MS]
//
// Exit codes:
//   0  ready: every URL answered HTTP 200 and no body said it was not ready
//   1  crashed: a --pid exited, a --log gained a fatal line, or a service
//      reported a terminal state; the tail of every --log is printed
//   2  timed out: every URL answered at least once, but not all became ready
//   3  unreachable: some URL never answered before the deadline, no URL was
//      given ("no services configured"), or the arguments were unusable.
//      Exit 3 means "could not look"; it is never a pass.
//
// Only dependency-free Node builtins are used.

import { closeSync, openSync, readFileSync, readSync, statSync } from "node:fs";
import { pathToFileURL } from "node:url";
import { parseArgs } from "node:util";

export const EXIT = Object.freeze({
  READY: 0,
  CRASHED: 1,
  TIMED_OUT: 2,
  UNREACHABLE: 3,
});

// Lines that mean the process has stopped for good. Each one is quoted from
// the source that emits it; keep this list to messages that are only ever
// written on the way to a process exit.
export const DEFAULT_FATAL_PATTERNS = Object.freeze([
  // demo/midgard-node/src/commands/listen-startup.ts: every startup
  // deployment/initialization refusal is logged with this prefix, then dies.
  "Startup protocol initialization failed:",
  // demo/midgard-node/src/database/init.ts: `listen` refuses a schema that is
  // not exactly the version this binary supports.
  "Database schema is not compatible:",
  // demo/midgard-node/src/services/midgard-contracts.ts: the configured
  // deployment manifest does not match this build or config.
  "cannot be used as contract source",
  // demo/da-committee-node/src/tick-runner.ts: the committee lost its L1 view
  // and exits with code 70.
  '"event":"l1_view_unavailable_exit"',
]);

const USAGE = `usage: devnet-wait.mjs --url <url> [--url <url>]... [--timeout S]
       [--log <file>]... [--pid <pid>]... [--fatal-pattern <regex>]...
       [--interval MS]`;

const REQUEST_TIMEOUT_MS = 5_000;
const BODY_LIMIT_BYTES = 64 * 1024;
const TAIL_LINES = 40;

const escapeRegExp = (text) => text.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");

/** Parses argv into a config, or returns `{ error }` for a usage problem. */
export const parseCli = (argv) => {
  let parsed;
  try {
    parsed = parseArgs({
      args: argv,
      options: {
        url: { type: "string", multiple: true, default: [] },
        timeout: { type: "string", default: "120" },
        log: { type: "string", multiple: true, default: [] },
        pid: { type: "string", multiple: true, default: [] },
        "fatal-pattern": { type: "string", multiple: true, default: [] },
        interval: { type: "string", default: "1000" },
        help: { type: "boolean", default: false },
      },
      allowPositionals: false,
      strict: true,
    });
  } catch (error) {
    return { error: error.message };
  }
  const { values } = parsed;
  if (values.help) return { help: true };
  const timeoutSeconds = Number(values.timeout);
  if (!Number.isFinite(timeoutSeconds) || timeoutSeconds <= 0)
    return { error: `--timeout must be a positive number of seconds` };
  const intervalMs = Number(values.interval);
  if (!Number.isFinite(intervalMs) || intervalMs < 10)
    return { error: `--interval must be at least 10 milliseconds` };
  const urls = [];
  for (const raw of values.url) {
    let url;
    try {
      url = new URL(raw);
    } catch {
      return { error: `not a URL: ${raw}` };
    }
    if (url.protocol !== "http:" && url.protocol !== "https:")
      return { error: `only http(s) URLs can be probed: ${raw}` };
    urls.push(url.toString());
  }
  const pids = [];
  for (const raw of values.pid) {
    if (!/^[1-9][0-9]*$/u.test(raw))
      return { error: `--pid must be a positive integer: ${raw}` };
    pids.push(Number(raw));
  }
  const fatalPatterns = [];
  for (const source of [
    ...DEFAULT_FATAL_PATTERNS.map(escapeRegExp),
    ...values["fatal-pattern"],
  ]) {
    try {
      fatalPatterns.push(new RegExp(source, "u"));
    } catch (error) {
      return { error: `bad --fatal-pattern ${source}: ${error.message}` };
    }
  }
  return {
    urls,
    timeoutMs: timeoutSeconds * 1000,
    intervalMs,
    logs: values.log,
    pids,
    fatalPatterns,
  };
};

/**
 * Classifies one HTTP answer. `ready` needs status 200 exactly: Kupo answers
 * 202 while it is still replaying, and the node and DA committee answer 503
 * with `ready: false`.
 */
export const classifyResponse = (status, bodyText) => {
  let body;
  try {
    body = JSON.parse(bodyText);
  } catch {
    body = undefined;
  }
  const object = body !== null && typeof body === "object" ? body : undefined;
  // A quarantined DA committee L1 source never becomes healthy again without
  // an operator (demo/da-committee-node/src/store.ts, mergeL1SourceState).
  if (object?.l1Source?.status === "quarantined")
    return {
      state: "terminal",
      detail: `l1Source quarantined: ${object.l1Source.quarantineReason ?? "no reason given"}`,
    };
  const reasons = Array.isArray(object?.reasons)
    ? ` reasons=${object.reasons.slice(0, 8).join(",")}`
    : "";
  if (status !== 200)
    return { state: "not_ready", detail: `HTTP ${status}${reasons}` };
  if (typeof object?.ready === "boolean" && object.ready !== true)
    return { state: "not_ready", detail: `ready=false${reasons}` };
  if (
    typeof object?.connection_status === "string" &&
    object.connection_status !== "connected"
  )
    return {
      state: "not_ready",
      detail: `connection_status=${object.connection_status}`,
    };
  return { state: "ready", detail: "HTTP 200" };
};

const probe = async (url, timeoutMs) => {
  let response;
  try {
    response = await fetch(url, {
      signal: AbortSignal.timeout(Math.max(100, timeoutMs)),
      headers: { accept: "application/json" },
    });
  } catch (error) {
    const cause = error?.cause?.code ?? error?.name ?? "error";
    return { state: "unreachable", detail: `no answer (${cause})` };
  }
  let text = "";
  try {
    const buffer = Buffer.from(await response.arrayBuffer());
    text = buffer.subarray(0, BODY_LIMIT_BYTES).toString("utf8");
  } catch {
    text = "";
  }
  return classifyResponse(response.status, text);
};

const processAlive = (pid) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    return error.code === "EPERM";
  }
};

const fileSize = (path) => {
  try {
    return statSync(path).size;
  } catch {
    return 0;
  }
};

/** Reads bytes appended to `path` since `offset`; restarts on truncation. */
const readAppended = (path, offset) => {
  const size = fileSize(path);
  const start = size < offset ? 0 : offset;
  if (size === start) return { text: "", next: start };
  const fd = openSync(path, "r");
  try {
    const buffer = Buffer.alloc(size - start);
    readSync(fd, buffer, 0, buffer.length, start);
    return { text: buffer.toString("utf8"), next: size };
  } finally {
    closeSync(fd);
  }
};

const tail = (path) => {
  try {
    const lines = readFileSync(path, "utf8").split("\n");
    if (lines.at(-1) === "") lines.pop();
    return lines.slice(-TAIL_LINES).join("\n");
  } catch (error) {
    return `(cannot read: ${error.code ?? error.message})`;
  }
};

const sleep = (ms) =>
  new Promise((resolve) => setTimeout(resolve, Math.max(0, ms)));

/**
 * Runs the wait loop. Returns `{ code, lines }`; the caller prints and exits.
 * Logs are scanned only from their size at start, so an old crash earlier in
 * an append-mode log is not mistaken for a new one.
 */
export const waitForDevnet = async (config, now = Date.now) => {
  const lines = [];
  if (config.urls.length === 0) {
    lines.push(
      "devnet-wait: no services configured: pass at least one --url (exit 3, never a pass)",
    );
    return { code: EXIT.UNREACHABLE, lines };
  }
  const deadline = now() + config.timeoutMs;
  const offsets = new Map(config.logs.map((path) => [path, fileSize(path)]));
  const carry = new Map(config.logs.map((path) => [path, ""]));
  const answered = new Set();
  let last = new Map();

  const crashed = (why) => {
    lines.push(`devnet-wait: crashed: ${why}`);
    for (const path of config.logs)
      lines.push(`--- tail of ${path} ---`, tail(path));
    return { code: EXIT.CRASHED, lines };
  };

  for (;;) {
    for (const pid of config.pids)
      if (!processAlive(pid)) return crashed(`process ${pid} is not running`);
    for (const path of config.logs) {
      const { text, next } = readAppended(path, offsets.get(path));
      offsets.set(path, next);
      const chunk = carry.get(path) + text;
      // The unterminated last line is scanned now (a dying process may not
      // write its newline) and carried so the next read completes it.
      const complete = chunk.lastIndexOf("\n");
      carry.set(path, complete === -1 ? chunk : chunk.slice(complete + 1));
      for (const line of chunk.split("\n"))
        for (const pattern of config.fatalPatterns)
          if (pattern.test(line))
            return crashed(`${path}: fatal line: ${line.slice(0, 500)}`);
    }

    const remaining = deadline - now();
    const results = await Promise.all(
      config.urls.map(async (url) => [
        url,
        await probe(url, Math.min(REQUEST_TIMEOUT_MS, remaining)),
      ]),
    );
    last = new Map(results);
    for (const [url, result] of results)
      if (result.state !== "unreachable") answered.add(url);

    const terminal = results.find(([, result]) => result.state === "terminal");
    if (terminal !== undefined)
      return crashed(`${terminal[0]}: ${terminal[1].detail}`);
    if (results.every(([, result]) => result.state === "ready")) {
      for (const [url, result] of results)
        lines.push(`ready        ${url}  ${result.detail}`);
      lines.push("devnet-wait: ready (exit 0)");
      return { code: EXIT.READY, lines };
    }
    if (now() >= deadline) break;
    await sleep(Math.min(config.intervalMs, deadline - now()));
    if (now() >= deadline) {
      // One last look so a service that came up during the sleep counts.
      continue;
    }
  }

  for (const [url, result] of last) {
    const state = answered.has(url) ? result.state : "never-answered";
    lines.push(`${state.padEnd(12)} ${url}  ${result.detail}`);
  }
  const unreachable = config.urls.filter((url) => !answered.has(url));
  if (unreachable.length > 0) {
    lines.push(
      `devnet-wait: unreachable: ${unreachable.length} URL(s) never answered within ${config.timeoutMs / 1000}s (exit 3)`,
    );
    return { code: EXIT.UNREACHABLE, lines };
  }
  lines.push(
    `devnet-wait: timed out after ${config.timeoutMs / 1000}s with services answering but not ready (exit 2)`,
  );
  return { code: EXIT.TIMED_OUT, lines };
};

const main = async () => {
  const config = parseCli(process.argv.slice(2));
  if (config.help) {
    console.log(USAGE);
    return EXIT.READY;
  }
  if (config.error !== undefined) {
    console.error(`devnet-wait: ${config.error}\n${USAGE}`);
    return EXIT.UNREACHABLE;
  }
  const { code, lines } = await waitForDevnet(config);
  (code === EXIT.READY ? console.log : console.error)(lines.join("\n"));
  return code;
};

if (
  process.argv[1] !== undefined &&
  import.meta.url === pathToFileURL(process.argv[1]).href
) {
  process.exitCode = await main();
}
