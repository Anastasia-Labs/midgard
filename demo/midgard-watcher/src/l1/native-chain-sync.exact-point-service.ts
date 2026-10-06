import { type ChildProcessWithoutNullStreams, spawn } from "node:child_process";
import { EventEmitter } from "node:events";
import { PassThrough, Writable } from "node:stream";

import {
  MAX_QUERY_STDOUT_BYTES,
  MAX_STDERR_BYTES,
  MAX_STDERR_DIAGNOSTIC_BYTES,
} from "./native-chain-sync.exact-record.js";

/**
 * Exact-point queries run as sessions of one persistent native helper per
 * binary instead of one spawned process per query. Each session keeps its own
 * node connection, deadline and ingress bound inside the helper, and is
 * presented to the unchanged supervisor as a child process: the same startup
 * line on stdin, the same stdout lines and stderr bytes, and an exit when the
 * session ends. A helper crash, hang or protocol violation kills the helper
 * and ends every in-flight session exactly as its own process exiting would;
 * nothing is retried. The next query starts a fresh helper. The helper admits
 * at most MAX_HELPER_SESSIONS live sessions; opens beyond that wait, in
 * order, for a session to end instead of being refused.
 *
 * Wire protocol (one line per frame):
 *   owner -> helper: "open <id> <startup line>", "close <id>"
 *   helper -> owner: "out <id> <stdout line>", "err <id> <base64 stderr>",
 *                    "end <id> <exit status>"
 */
export const WATCHER_NATIVE_EXACT_POINT_SERVICE_FLAG = "--exact-point-service";

const MAX_STARTUP_LINE_BYTES = 64 * 1024;
// One framed stdout line carries at most one session's whole stdout bound.
const MAX_SERVICE_FRAME_BYTES = MAX_QUERY_STDOUT_BYTES + 64;
const MAX_SERVICE_STDERR_TAIL_BYTES = MAX_STDERR_DIAGNOSTIC_BYTES;
// A killed session must be released by the helper within the same bound the
// child drain grants a terminated process before SIGKILL.
const KILLED_SESSION_RELEASE_MS = 5_000;
const SERVICE_SHUTDOWN_MS = 5_000;
const SERVICE_IDLE_MS = 2_000;
const MAX_SESSION_ID = Number.MAX_SAFE_INTEGER;
// The helper's own session bound (maxServiceSessions in service.go). A
// session counts here from its open frame until its end frame, and the helper
// stops counting it before it frames that end, so the owner's count is never
// below the helper's and the helper never refuses an admitted open.
const MAX_HELPER_SESSIONS = 256;
const SESSION_ID = /^[1-9][0-9]{0,15}$/u;
const BASE64 = /^[A-Za-z0-9+/]*={0,2}$/u;
const NEWLINE = Buffer.from("\n");

export type WatcherNativeServiceSpawn = (
  binaryPath: string,
  args: readonly string[],
) => ChildProcessWithoutNullStreams;

const productionServiceSpawn: WatcherNativeServiceSpawn = (binaryPath, args) =>
  spawn(binaryPath, [...args], {
    stdio: ["pipe", "pipe", "pipe"],
    env: Object.freeze({ PATH: process.env.PATH ?? "/usr/bin:/bin" }),
  });

class ExactPointSession extends EventEmitter {
  readonly stdin: Writable;
  readonly stdout = new PassThrough();
  readonly stderr = new PassThrough();
  killed = false;
  exitCode: number | null = null;
  signalCode: NodeJS.Signals | null = null;
  #service: ExactPointService | undefined;
  #id: number | undefined;
  #finished = false;
  #startup: Buffer[] = [];
  #startupBytes = 0;
  #stdoutBytes = 0;
  #stderrBytes = 0;

  constructor(
    readonly binaryPath: string,
    readonly spawnService: WatcherNativeServiceSpawn,
  ) {
    super();
    this.stdin = new Writable({
      write: (chunk: Buffer, _encoding, callback) => {
        this.#startupBytes += chunk.byteLength;
        if (this.#startupBytes <= MAX_STARTUP_LINE_BYTES)
          this.#startup.push(chunk);
        callback();
      },
      final: (callback) => {
        this.#open();
        callback();
      },
    });
  }

  get pid(): number | undefined {
    return this.#service?.pid;
  }

  #open(): void {
    if (this.#finished || this.killed) return;
    const line = Buffer.concat(this.#startup);
    this.#startup = [];
    // The supervisor writes exactly one bounded line; anything else cannot be
    // framed and fails like a helper that refused its startup.
    if (
      this.#startupBytes > MAX_STARTUP_LINE_BYTES ||
      line.length < 2 ||
      line.indexOf(0x0a) !== line.length - 1
    ) {
      this.finish(64, null);
      return;
    }
    this.#service = serviceFor(this.binaryPath, this.spawnService);
    this.#service.open(this, line.subarray(0, line.length - 1));
  }

  /** The helper session id, once the open frame has been written. */
  opened(id: number): void {
    this.#id = id;
  }

  /** Forwards one stdout line; output beyond the session bound is dropped. */
  deliverStdout(line: Buffer): void {
    if (this.#finished || this.#stdoutBytes > MAX_QUERY_STDOUT_BYTES) return;
    // The line crossing the bound is still delivered, so the supervisor's
    // own stdout bound refuses it exactly as for a process.
    this.#stdoutBytes += line.byteLength + 1;
    this.stdout.write(line);
    this.stdout.write(NEWLINE);
  }

  deliverStderr(chunk: Buffer): void {
    if (this.#finished || this.#stderrBytes > MAX_STDERR_BYTES) return;
    this.#stderrBytes += chunk.byteLength;
    this.stderr.write(chunk);
  }

  get finished(): boolean {
    return this.#finished;
  }

  finish(code: number | null, signal: NodeJS.Signals | null): void {
    if (this.#finished) return;
    this.#finished = true;
    this.exitCode = code;
    this.signalCode = signal;
    this.#service?.released(this, this.#id);
    this.stdout.end();
    this.stderr.end();
    // A process exit is observed asynchronously, never inside kill().
    setImmediate(() => {
      this.emit("exit", code, signal);
      setImmediate(() => this.emit("close", code, signal));
    });
  }

  kill(signal: NodeJS.Signals | number = "SIGTERM"): boolean {
    if (this.#finished) return false;
    this.killed = true;
    const service = this.#service;
    const id = this.#id;
    // An unopened or still waiting session ends as an unstarted process.
    if (service === undefined || id === undefined) {
      this.finish(null, typeof signal === "number" ? "SIGKILL" : signal);
      return true;
    }
    service.close(id);
    if (signal === "SIGKILL" || signal === 9) {
      // A killed process ends now; the helper must confirm the release.
      this.finish(null, "SIGKILL");
      service.awaitRelease(id);
    }
    return true;
  }
}

class ExactPointService {
  readonly #child: ChildProcessWithoutNullStreams;
  readonly #sessions = new Map<number, ExactPointSession>();
  // Locally ended sessions whose helper end frame is still outstanding.
  readonly #releasing = new Map<number, NodeJS.Timeout | undefined>();
  // Opens waiting, in order, for a helper session slot.
  #waiting: { session: ExactPointSession; startup: Buffer }[] = [];
  #lastId = 0;
  #dead = false;
  #inputEnded = false;
  #pending: Buffer[] = [];
  #pendingBytes = 0;
  #stderrTail = Buffer.alloc(0);
  #idleTimer: NodeJS.Timeout | undefined;
  readonly exited: Promise<void>;

  constructor(
    readonly binaryPath: string,
    spawnService: WatcherNativeServiceSpawn,
    readonly onDead: (service: ExactPointService) => void,
  ) {
    this.#child = spawnService(binaryPath, [
      WATCHER_NATIVE_EXACT_POINT_SERVICE_FLAG,
    ]);
    this.exited = new Promise((resolve) => {
      this.#child.once("close", () => resolve());
    });
    this.#child.stdout.on("data", (chunk: Buffer) => this.#receive(chunk));
    // close follows every stdout byte, so trailing session frames are framed
    // before the helper's own end is applied to the remaining sessions.
    this.#child.once("close", (code, signal) => this.#ended(code, signal));
    this.#child.stderr.on("data", (chunk: Buffer) => {
      this.#stderrTail = Buffer.concat([this.#stderrTail, chunk]).subarray(
        -MAX_SERVICE_STDERR_TAIL_BYTES,
      );
    });
    this.#child.stdin.on("error", () =>
      this.fail("native exact-point service input failed"),
    );
    this.#child.once("error", () =>
      this.fail("native exact-point service failed to run"),
    );
  }

  get pid(): number | undefined {
    return this.#child.pid;
  }

  get dead(): boolean {
    return this.#dead;
  }

  open(session: ExactPointSession, startup: Buffer): void {
    if (this.#dead || this.#inputEnded) {
      session.finish(null, "SIGKILL");
      return;
    }
    this.#waiting.push({ session, startup });
    this.#admit();
  }

  // Writes waiting opens while the helper has a session slot for them.
  #admit(): void {
    while (
      this.#waiting.length > 0 &&
      !this.#dead &&
      !this.#inputEnded &&
      this.#sessions.size + this.#releasing.size < MAX_HELPER_SESSIONS
    ) {
      const { session, startup } = this.#waiting.shift()!;
      if (session.finished) continue;
      if (this.#lastId >= MAX_SESSION_ID) {
        session.finish(null, "SIGKILL");
        continue;
      }
      const id = ++this.#lastId;
      this.#sessions.set(id, session);
      session.opened(id);
      this.#child.stdin.write(`open ${id} `);
      this.#child.stdin.write(startup);
      this.#child.stdin.write(NEWLINE);
    }
    this.#refresh();
  }

  close(id: number): void {
    if (!this.#dead && !this.#inputEnded)
      this.#child.stdin.write(`close ${id}\n`);
  }

  awaitRelease(id: number): void {
    if (this.#dead || !this.#releasing.has(id)) return;
    const timer = setTimeout(
      () => this.fail("native exact-point service kept a killed session"),
      KILLED_SESSION_RELEASE_MS,
    );
    timer.unref();
    this.#releasing.set(id, timer);
  }

  /** A session ended locally or by its end frame. */
  released(session: ExactPointSession, id: number | undefined): void {
    if (id !== undefined && this.#sessions.get(id) === session) {
      this.#sessions.delete(id);
      if (!this.#dead) this.#releasing.set(id, undefined);
    }
    if (id === undefined)
      this.#waiting = this.#waiting.filter(
        (entry) => entry.session !== session,
      );
    this.#refresh();
  }

  // Idle helpers are not owner work: they neither keep the owner alive nor
  // outlive a short idle period, and a dead owner closes their stdin.
  #refresh(): void {
    if (this.#dead || this.#inputEnded) return;
    const handles = [
      this.#child,
      this.#child.stdin,
      this.#child.stdout,
      this.#child.stderr,
    ] as unknown as { ref?(): void; unref?(): void }[];
    if (this.#sessions.size > 0 || this.#waiting.length > 0) {
      if (this.#idleTimer !== undefined) clearTimeout(this.#idleTimer);
      this.#idleTimer = undefined;
      for (const handle of handles) handle.ref?.();
      return;
    }
    for (const handle of handles) handle.unref?.();
    if (this.#idleTimer === undefined) {
      this.#idleTimer = setTimeout(() => {
        this.#idleTimer = undefined;
        if (this.#sessions.size === 0 && this.#waiting.length === 0)
          void this.shutdown();
      }, SERVICE_IDLE_MS);
      this.#idleTimer.unref();
    }
  }

  #receive(chunk: Buffer): void {
    let start = 0;
    while (!this.#dead) {
      const newline = chunk.indexOf(0x0a, start);
      if (newline === -1) break;
      const piece = chunk.subarray(start, newline);
      const line =
        this.#pending.length === 0
          ? piece
          : Buffer.concat([...this.#pending, piece]);
      this.#pending = [];
      this.#pendingBytes = 0;
      start = newline + 1;
      if (line.length > MAX_SERVICE_FRAME_BYTES) {
        this.fail("native exact-point service frame exceeded its bound");
        return;
      }
      this.#frame(line);
    }
    if (this.#dead || start >= chunk.length) return;
    this.#pendingBytes += chunk.length - start;
    if (this.#pendingBytes > MAX_SERVICE_FRAME_BYTES) {
      this.fail("native exact-point service frame exceeded its bound");
      return;
    }
    this.#pending.push(chunk.subarray(start));
  }

  #frame(line: Buffer): void {
    const verbEnd = line.indexOf(0x20);
    const idEnd = verbEnd === -1 ? -1 : line.indexOf(0x20, verbEnd + 1);
    if (verbEnd === -1 || idEnd === -1) {
      this.fail("native exact-point service emitted an invalid frame");
      return;
    }
    const verb = line.toString("latin1", 0, verbEnd);
    const rawId = line.toString("latin1", verbEnd + 1, idEnd);
    const payload = line.subarray(idEnd + 1);
    const id = SESSION_ID.test(rawId) ? Number(rawId) : 0;
    if (id < 1 || id > this.#lastId) {
      this.fail("native exact-point service emitted an unknown session");
      return;
    }
    const session = this.#sessions.get(id);
    if (session === undefined && !this.#releasing.has(id)) {
      this.fail("native exact-point service emitted an ended session");
      return;
    }
    if (verb === "out") {
      session?.deliverStdout(payload);
    } else if (verb === "err") {
      const encoded = payload.toString("latin1");
      if (!BASE64.test(encoded) || encoded.length % 4 !== 0) {
        this.fail("native exact-point service emitted invalid diagnostics");
        return;
      }
      session?.deliverStderr(Buffer.from(encoded, "base64"));
    } else if (verb === "end") {
      const status = payload.toString("latin1");
      if (!/^(?:0|[1-9][0-9]{0,2})$/u.test(status) || Number(status) > 255) {
        this.fail("native exact-point service emitted an invalid status");
        return;
      }
      session?.finish(Number(status), null);
      const timer = this.#releasing.get(id);
      if (timer !== undefined) clearTimeout(timer);
      this.#releasing.delete(id);
      this.#admit();
    } else {
      this.fail("native exact-point service emitted an invalid frame");
    }
  }

  /** Kills the helper; every in-flight session ends as its process would. */
  fail(reason: string): void {
    if (this.#dead) return;
    this.#child.kill("SIGKILL");
    this.#end(null, "SIGKILL", reason);
  }

  #ended(code: number | null, signal: NodeJS.Signals | null): void {
    this.#end(code, signal, "native exact-point service exited");
  }

  #end(
    code: number | null,
    signal: NodeJS.Signals | null,
    reason: string,
  ): void {
    if (this.#dead) return;
    this.#dead = true;
    this.onDead(this);
    if (this.#idleTimer !== undefined) clearTimeout(this.#idleTimer);
    for (const timer of this.#releasing.values())
      if (timer !== undefined) clearTimeout(timer);
    this.#releasing.clear();
    const diagnostic = Buffer.concat([
      this.#stderrTail,
      Buffer.from(`${this.#stderrTail.length === 0 ? "" : "\n"}${reason}\n`),
    ]);
    // Waiting opens end with the helper they were queued on, like in-flight
    // sessions; the next query starts a fresh helper.
    const sessions = [
      ...this.#sessions.values(),
      ...this.#waiting.map((entry) => entry.session),
    ];
    this.#sessions.clear();
    this.#waiting = [];
    for (const session of sessions) {
      session.deliverStderr(diagnostic);
      session.finish(code, signal);
    }
    this.#child.stdin.destroy();
  }

  /** Closes stdin; the helper closes every session and exits. */
  async shutdown(): Promise<void> {
    if (!this.#dead) {
      this.onDead(this);
      this.#inputEnded = true;
      // An open still waiting for a slot was never sent; it ends as an open
      // refused by a closing helper does.
      for (const { session } of this.#waiting.splice(0))
        session.finish(null, "SIGKILL");
      this.#child.stdin.end();
      const timer = setTimeout(
        () => this.#child.kill("SIGKILL"),
        SERVICE_SHUTDOWN_MS,
      );
      timer.unref();
      await this.exited;
      clearTimeout(timer);
      return;
    }
    if (this.#child.exitCode === null && this.#child.signalCode === null)
      this.#child.kill("SIGKILL");
    await this.exited;
  }
}

const services = new Map<string, ExactPointService>();

const retire = (service: ExactPointService): void => {
  if (services.get(service.binaryPath) === service)
    services.delete(service.binaryPath);
};

/**
 * Spawns an exact-point session presented as the helper's child process.
 * Production callers use the persistent helper; tests may substitute how the
 * helper itself is spawned.
 */
const serviceFor = (
  binaryPath: string,
  spawnService: WatcherNativeServiceSpawn,
): ExactPointService => {
  let service = services.get(binaryPath);
  if (service === undefined || service.dead) {
    service = new ExactPointService(binaryPath, spawnService, retire);
    services.set(binaryPath, service);
  }
  return service;
};

export const exactPointServiceSession = (
  binaryPath: string,
  spawnService: WatcherNativeServiceSpawn = productionServiceSpawn,
): ChildProcessWithoutNullStreams =>
  new ExactPointSession(
    binaryPath,
    spawnService,
  ) as unknown as ChildProcessWithoutNullStreams;

/** Stops every persistent helper and waits until each process has exited. */
export const closeWatcherNativeExactPointServices = async (): Promise<void> => {
  const running = [...services.values()];
  services.clear();
  await Promise.all(running.map((service) => service.shutdown()));
};

/** Process ids of the live persistent helpers, for lifecycle checks. */
export const watcherNativeExactPointServicePids = (): readonly number[] =>
  [...services.values()].flatMap((service) =>
    service.pid === undefined || service.dead ? [] : [service.pid],
  );
