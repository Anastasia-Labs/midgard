import { type ChildProcessWithoutNullStreams, spawn } from "node:child_process";

import {
  ExactPointSession,
  type ExactPointSessionHost,
} from "./native-chain-sync.exact-point-session.js";
import {
  MAX_QUERY_STDOUT_BYTES,
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
 * nothing is retried. The next query starts a fresh helper. A fault confined
 * to one live session's own frame, behind an intact header, ends only that
 * session (see #oversize and #frame). The helper admits at most
 * MAX_HELPER_SESSIONS live sessions; opens beyond that wait, in order, for a
 * session to end instead of being refused.
 *
 * Wire protocol (one line per frame):
 *   owner -> helper: "open <id> <startup line>", "close <id>"
 *   helper -> owner: "out <id> <stdout line>", "err <id> <base64 stderr>",
 *                    "end <id> <exit status>"
 */
export const WATCHER_NATIVE_EXACT_POINT_SERVICE_FLAG = "--exact-point-service";

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

class ExactPointService implements ExactPointSessionHost {
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
  // The rest of an oversized frame is being skipped up to its newline.
  #skipping = false;
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
      if (this.#skipping) {
        if (newline === -1) return;
        this.#skipping = false;
        start = newline + 1;
        continue;
      }
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
        this.#oversize(line);
        continue;
      }
      this.#frame(line);
    }
    if (this.#dead || start >= chunk.length) return;
    this.#pendingBytes += chunk.length - start;
    this.#pending.push(chunk.subarray(start));
    if (this.#pendingBytes > MAX_SERVICE_FRAME_BYTES) {
      const head = Buffer.concat(this.#pending);
      this.#pending = [];
      this.#pendingBytes = 0;
      this.#oversize(head);
      this.#skipping = !this.#dead;
    }
  }

  /**
   * Splits a frame into its verb, its admitted session and its payload, or
   * kills the helper. A malformed header, an id the owner never allocated and
   * an id the owner already saw end all mean the helper's view of its
   * sessions has diverged from the owner's, so no session can be blamed.
   */
  #header(line: Buffer):
    | Readonly<{
        verb: string;
        id: number;
        session: ExactPointSession | undefined;
        payload: Buffer;
      }>
    | undefined {
    const verbEnd = line.indexOf(0x20);
    const idEnd = verbEnd === -1 ? -1 : line.indexOf(0x20, verbEnd + 1);
    if (verbEnd === -1 || idEnd === -1) {
      this.fail("native exact-point service emitted an invalid frame");
      return undefined;
    }
    const verb = line.toString("latin1", 0, verbEnd);
    const rawId = line.toString("latin1", verbEnd + 1, idEnd);
    const id = SESSION_ID.test(rawId) ? Number(rawId) : 0;
    if (id < 1 || id > this.#lastId) {
      this.fail("native exact-point service emitted an unknown session");
      return undefined;
    }
    const session = this.#sessions.get(id);
    if (session === undefined && !this.#releasing.has(id)) {
      this.fail("native exact-point service emitted an ended session");
      return undefined;
    }
    return { verb, id, session, payload: line.subarray(idEnd + 1) };
  }

  /**
   * Frames are newline-terminated and no payload can hold a newline (stdout
   * lines are single JSON lines, diagnostics are base64), so the frame after
   * an oversized one starts at its next newline and the stream stays in sync.
   * An oversized stdout or diagnostics frame is therefore that session's
   * fault alone. An oversized end frame is not: the end frame is the helper's
   * slot release, and one outside its grammar leaves the helper's session
   * count unknown, so it kills the helper.
   */
  #oversize(head: Buffer): void {
    const frame = this.#header(head);
    if (frame === undefined) return;
    const { verb, session, payload } = frame;
    if (verb === "out") {
      session?.deliverStdoutOverflow(payload);
    } else if (verb === "err") {
      this.#failSession(
        session,
        "native exact-point service emitted invalid diagnostics",
      );
    } else {
      this.fail("native exact-point service frame exceeded its bound");
    }
  }

  /**
   * Ends one live session as a killed process, with the reason as its last
   * diagnostics; the helper must still release it within the kill bound.
   */
  #failSession(session: ExactPointSession | undefined, reason: string): void {
    if (session === undefined) return;
    session.deliverStderr(Buffer.from(`${reason}\n`));
    session.kill("SIGKILL");
  }

  #frame(line: Buffer): void {
    const frame = this.#header(line);
    if (frame === undefined) return;
    const { verb, id, session, payload } = frame;
    if (verb === "out") {
      session?.deliverStdout(payload);
    } else if (verb === "err") {
      const encoded = payload.toString("latin1");
      if (!BASE64.test(encoded) || encoded.length % 4 !== 0) {
        this.#failSession(
          session,
          "native exact-point service emitted invalid diagnostics",
        );
        return;
      }
      session?.deliverStderr(Buffer.from(encoded, "base64"));
    } else if (verb === "end") {
      // The end frame releases the helper's slot; one outside its grammar
      // leaves the helper's session count unknown, so it is not isolated.
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
      // An unknown verb is a helper speaking another protocol.
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
  new ExactPointSession(() =>
    serviceFor(binaryPath, spawnService),
  ) as unknown as ChildProcessWithoutNullStreams;

// Test hooks: this module is outside the package exports map, so only the
// package's own tests reach these by relative path.

/** Stops every persistent helper and waits until each process has exited. */
export const unsafeCloseWatcherNativeExactPointServicesForTest =
  async (): Promise<void> => {
    const running = [...services.values()];
    services.clear();
    await Promise.all(running.map((service) => service.shutdown()));
  };

/** Process ids of the live persistent helpers, for lifecycle checks. */
export const unsafeWatcherNativeExactPointServicePidsForTest =
  (): readonly number[] =>
    [...services.values()].flatMap((service) =>
      service.pid === undefined || service.dead ? [] : [service.pid],
    );
