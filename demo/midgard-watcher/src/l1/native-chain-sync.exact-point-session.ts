import { EventEmitter } from "node:events";
import { PassThrough, Writable } from "node:stream";

import {
  MAX_QUERY_STDOUT_BYTES,
  MAX_STDERR_BYTES,
} from "./native-chain-sync.exact-record.js";

const MAX_STARTUP_LINE_BYTES = 64 * 1024;
const NEWLINE = Buffer.from("\n");

/** The persistent helper an exact-point session runs on. */
export interface ExactPointSessionHost {
  readonly pid: number | undefined;
  open(session: ExactPointSession, startup: Buffer): void;
  close(id: number): void;
  awaitRelease(id: number): void;
  /** A session ended locally or by its end frame. */
  released(session: ExactPointSession, id: number | undefined): void;
}

/**
 * One exact-point session of the persistent helper, presented to the
 * supervisor as the child process a per-query helper would have been.
 */
export class ExactPointSession extends EventEmitter {
  readonly stdin: Writable;
  readonly stdout = new PassThrough();
  readonly stderr = new PassThrough();
  killed = false;
  exitCode: number | null = null;
  signalCode: NodeJS.Signals | null = null;
  #service: ExactPointSessionHost | undefined;
  #id: number | undefined;
  #finished = false;
  #startup: Buffer[] = [];
  #startupBytes = 0;
  #stdoutBytes = 0;
  #stderrBytes = 0;

  constructor(readonly connect: () => ExactPointSessionHost) {
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
    this.#service = this.connect();
    this.#service.open(this, line.subarray(0, line.length - 1));
  }

  /** The helper session id, once the open frame has been written. */
  opened(id: number): void {
    this.#id = id;
  }

  /**
   * Forwards the start of a stdout line longer than the session bound, so the
   * supervisor's own stdout bound refuses it as for a process; the rest of the
   * line is never framed.
   */
  deliverStdoutOverflow(prefix: Buffer): void {
    if (this.#finished || this.#stdoutBytes > MAX_QUERY_STDOUT_BYTES) return;
    this.#stdoutBytes = MAX_QUERY_STDOUT_BYTES + 1;
    this.stdout.write(prefix);
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
