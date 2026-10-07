import { type ChildProcess, spawn } from "node:child_process";

import type { CborInput } from "./cbor.js";
import {
  encodeFrame,
  type Frame,
  FRAME_PROTOCOL_VERSION,
  FrameReader,
} from "./frame.js";
import { headerText, natural, TransportProtocolError } from "./protocol.js";

/** A refusal of one request by the sidecar or the node. */
export class TransportRequestError extends Error {
  override readonly name = "TransportRequestError";
  constructor(
    readonly code: string,
    message: string,
  ) {
    super(`${code}: ${message}`);
  }
}

/** The sidecar ended; every request it held is answered with this. */
export class SidecarExitedError extends Error {
  override readonly name = "SidecarExitedError";
  constructor(
    readonly exit: SidecarExit,
    message = describeExit(exit),
  ) {
    super(message);
  }
}

export type SidecarExit = Readonly<{
  code: number | null;
  signal: NodeJS.Signals | null;
  fatal: Readonly<{ code: string; message: string }> | null;
  diagnostics: string;
}>;

const describeExit = (exit: SidecarExit): string =>
  exit.fatal !== null
    ? `transport sidecar ended: ${exit.fatal.code}: ${exit.fatal.message}`
    : `transport sidecar ended (code ${exit.code}, signal ${exit.signal})`;

export type SidecarOptions = Readonly<{
  binaryPath: string;
  socketPath: string;
  networkMagic: number;
  helloTimeoutMs: number;
  onDiagnostic?: (line: string) => void;
}>;

const ANSWER_TYPES = new Set([
  "ok",
  "error",
  "lsq_result",
  "submit_accepted",
  "submit_rejected",
  "monitor_has_tx_result",
  "monitor_sizes_result",
  "cs_opened",
  "cs_intersect_not_found",
]);
const STREAM_TYPES = new Set([
  "cs_roll_forward",
  "cs_roll_backward",
  "cs_failed",
]);
const DIAGNOSTIC_TAIL = 8 * 1024;

type Pending = Readonly<{
  resolve: (frame: Frame) => void;
  reject: (error: Error) => void;
}>;

/** One running sidecar: one process, one node connection set. */
export class SidecarProcess {
  readonly #child: ChildProcess;
  readonly #reader = new FrameReader();
  readonly #pending = new Map<number, Pending>();
  readonly #streams = new Map<number, (frame: Frame) => void>();
  readonly #exitListeners: Array<(exit: SidecarExit) => void> = [];
  #nextId = 1;
  #fatal: SidecarExit["fatal"] = null;
  #diagnostics = "";
  #exit: SidecarExit | undefined;
  #hello: Pending | undefined;
  nodeToClientVersion = 0;

  private constructor(
    readonly options: SidecarOptions,
    child: ChildProcess,
  ) {
    this.#child = child;
    child.stdout!.on("data", (chunk: Buffer) => this.#data(chunk));
    child.stderr!.setEncoding("utf8");
    let partial = "";
    child.stderr!.on("data", (text: string) => {
      this.#diagnostics = (this.#diagnostics + text).slice(-DIAGNOSTIC_TAIL);
      const lines = (partial + text).split("\n");
      partial = lines.pop()!.slice(-DIAGNOSTIC_TAIL);
      for (const line of lines)
        if (line.length > 0) options.onDiagnostic?.(line.slice(0, 4096));
    });
    child.stdin!.on("error", () => undefined);
    child.on("error", (error) => {
      this.#diagnostics += `\nspawn failed: ${error.message}`;
      if (child.pid === undefined) this.#closed(null, null);
    });
    child.on("close", (code, signal) => this.#closed(code, signal));
  }

  /** Spawns the sidecar and completes the hello exchange. */
  static async start(options: SidecarOptions): Promise<SidecarProcess> {
    const child = spawn(options.binaryPath, [], {
      stdio: ["pipe", "pipe", "pipe"],
      env: { PATH: process.env.PATH ?? "/usr/bin:/bin" },
    });
    const sidecar = new SidecarProcess(options, child);
    await sidecar.#helloExchange();
    return sidecar;
  }

  get exited(): SidecarExit | undefined {
    return this.#exit;
  }

  onExit(listener: (exit: SidecarExit) => void): void {
    if (this.#exit !== undefined) listener(this.#exit);
    else this.#exitListeners.push(listener);
  }

  async #helloExchange(): Promise<void> {
    const answer = new Promise<Frame>((resolve, reject) => {
      this.#hello = { resolve, reject };
    });
    const timer = setTimeout(() => {
      this.#hello?.reject(
        new SidecarExitedError(
          this.#snapshot(null, null),
          "transport sidecar did not answer hello in time",
        ),
      );
      this.kill();
    }, this.options.helloTimeoutMs);
    try {
      this.#write({
        type: "hello",
        version: FRAME_PROTOCOL_VERSION,
        socketPath: this.options.socketPath,
        networkMagic: this.options.networkMagic,
      });
      const frame = await answer;
      if (frame.header.version !== FRAME_PROTOCOL_VERSION)
        throw new TransportProtocolError("sidecar answered another version");
      this.nodeToClientVersion = Number(
        natural(frame.header.nodeToClientVersion, "node-to-client version"),
      );
    } finally {
      clearTimeout(timer);
      this.#hello = undefined;
    }
  }

  #write(
    header: { readonly [key: string]: CborInput | undefined },
    payload?: Uint8Array,
  ): void {
    if (this.#exit !== undefined || this.#child.stdin!.destroyed) return;
    this.#child.stdin!.write(encodeFrame(header, payload));
  }

  /** Sends a frame that has no answer (cs_ack, cs_window). */
  send(header: { readonly [key: string]: CborInput | undefined }): void {
    this.#write(header);
  }

  /** Sends a request and resolves with its answer frame; error frames reject. */
  request(
    header: { readonly [key: string]: CborInput | undefined },
    payload?: Uint8Array,
  ): Promise<Frame> {
    if (this.#exit !== undefined)
      return Promise.reject(new SidecarExitedError(this.#exit));
    const id = this.#nextId++;
    return new Promise<Frame>((resolve, reject) => {
      this.#pending.set(id, { resolve, reject });
      this.#write({ ...header, id }, payload);
    });
  }

  /** Routes a stream's frames; registered before its cs_open is sent. */
  registerStream(stream: number, handler: (frame: Frame) => void): void {
    this.#streams.set(stream, handler);
  }

  unregisterStream(stream: number): void {
    this.#streams.delete(stream);
  }

  get busy(): boolean {
    return this.#pending.size > 0 || this.#streams.size > 0;
  }

  /** Lets the parent process exit while the sidecar idles. */
  setReferenced(referenced: boolean): void {
    const handles = [
      this.#child,
      this.#child.stdin,
      this.#child.stdout,
      this.#child.stderr,
    ];
    for (const handle of handles) {
      const target = handle as unknown as {
        ref?: () => void;
        unref?: () => void;
      };
      if (referenced) target.ref?.();
      else target.unref?.();
    }
  }

  /** Orderly stop: closes the sidecar's input, then kills it after a bound. */
  async close(boundMs = 5000): Promise<void> {
    if (this.#exit !== undefined) return;
    const exited = new Promise<void>((resolve) => this.onExit(() => resolve()));
    this.#child.stdin!.end();
    const timer = setTimeout(() => this.kill(), boundMs);
    await exited;
    clearTimeout(timer);
  }

  kill(): void {
    if (this.#exit === undefined) this.#child.kill("SIGKILL");
  }

  #data(chunk: Buffer): void {
    let frames: Frame[];
    try {
      frames = this.#reader.push(chunk);
    } catch (error) {
      this.#diagnostics += `\nmalformed sidecar output: ${(error as Error).message}`;
      this.kill();
      return;
    }
    for (const frame of frames) this.#dispatch(frame);
  }

  #dispatch(frame: Frame): void {
    const type = frame.header.type as string;
    if (type === "hello_ok") {
      this.#hello?.resolve(frame);
      return;
    }
    if (type === "fatal") {
      this.#fatal = {
        code: headerText(frame.header.code),
        message: headerText(frame.header.message),
      };
      this.#hello?.reject(new SidecarExitedError(this.#snapshot(null, null)));
      return;
    }
    if (ANSWER_TYPES.has(type)) {
      const id = Number(natural(frame.header.id, "answer id"));
      const pending = this.#pending.get(id);
      if (pending === undefined) return;
      this.#pending.delete(id);
      if (type === "error")
        pending.reject(
          new TransportRequestError(
            headerText(frame.header.code),
            headerText(frame.header.message),
          ),
        );
      else pending.resolve(frame);
      return;
    }
    if (STREAM_TYPES.has(type)) {
      const stream = Number(natural(frame.header.stream, "stream id"));
      this.#streams.get(stream)?.(frame);
      return;
    }
    this.#diagnostics += `\nunknown sidecar frame ${type}`;
    this.kill();
  }

  #snapshot(code: number | null, signal: NodeJS.Signals | null): SidecarExit {
    return { code, signal, fatal: this.#fatal, diagnostics: this.#diagnostics };
  }

  #closed(code: number | null, signal: NodeJS.Signals | null): void {
    if (this.#exit !== undefined) return;
    const exit = this.#snapshot(code, signal);
    this.#exit = exit;
    const error = new SidecarExitedError(exit);
    this.#hello?.reject(error);
    for (const pending of this.#pending.values()) pending.reject(error);
    this.#pending.clear();
    this.#streams.clear();
    for (const listener of this.#exitListeners.splice(0)) listener(exit);
  }
}
