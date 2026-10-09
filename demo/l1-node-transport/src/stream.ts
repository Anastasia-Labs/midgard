import type { Frame } from "./frame.js";
import {
  bytesOf,
  bytesToHex,
  type ChainPoint,
  type ChainSyncEvent,
  type ChainTip,
  decodePoint,
  decodeTip,
  encodePoint,
  headerText,
  natural,
  pointKey,
  samePoint,
  TransportProtocolError,
} from "./protocol.js";
import {
  type SidecarExit,
  SidecarExitedError,
  type SidecarProcess,
  TransportRequestError,
} from "./sidecar.js";

/**
 * Credit: the number of events the sidecar may deliver beyond the last
 * acknowledged one. `catchUpWindow` applies while the stream is more than
 * `catchUpDistance` blocks behind the node tip, `tipWindow` otherwise.
 */
export type CreditPolicy = Readonly<{
  catchUpWindow: number;
  tipWindow: number;
  catchUpDistance: bigint;
}>;

export const MAX_WINDOW = 100;
const MAX_POINTS = 256;
const RESUME_POINTS = 64;
const REOPEN_DELAY_MS = 1000;

/**
 * The stream failures a resuming stream reopens from: the node connection
 * dropped (`cs_failed` `node_connection_lost`), or a reopen the sidecar
 * refused because it could not reach the node or was busy. Any other
 * `cs_failed` code (a protocol violation, a block outside the size bounds,
 * an undecodable block) or refusal fails the stream with
 * `TransportRequestError(code)`: a reopen meets the same block or request.
 */
export const STREAM_REOPEN_CODES: ReadonlySet<string> = new Set([
  "node_connection_lost",
  "node_unavailable",
  "busy",
]);

export type ChainSyncOptions = Readonly<{
  /**
   * Intersection candidates in preference order; include the origin to never
   * miss. The first point is the consumer's current position: when the node
   * intersects anywhere else, the stream's first event is a rollback to the
   * intersection, so a consumer that applies every event stays on the node's
   * chain.
   */
  points: readonly ChainPoint[];
  /** The sequence number of the last event the consumer already has. */
  startSeq?: bigint;
  credit: number | CreditPolicy;
  /**
   * Resume after a sidecar restart or a transient stream failure
   * (`STREAM_REOPEN_CODES`; default true). When false, such an interruption
   * ends the stream with an error instead. Any other stream failure ends it
   * either way.
   */
  resume?: boolean;
  /**
   * Told each time a resuming stream fails on its sidecar and is reopened
   * (`interruptions`), so a reopen loop is visible to the consumer. A
   * listener's throw is ignored.
   */
  onInterrupted?: (interruption: StreamInterruption) => void;
}>;

/**
 * The stream's failures that it reopened from: `consecutive` since the last
 * delivered event, `total` over its life, `last` the latest failure's text.
 */
export type StreamInterruption = Readonly<{
  consecutive: number;
  total: number;
  last: string;
}>;

export type Opened = Readonly<{ intersection: ChainPoint; tip: ChainTip }>;

export class IntersectNotFoundError extends Error {
  override readonly name = "IntersectNotFoundError";
  constructor(
    readonly tip: ChainTip,
    readonly resuming: boolean,
  ) {
    super(
      resuming
        ? "the node no longer has any point this stream can resume from"
        : "the node has none of the requested intersection points",
    );
  }
}

export class StreamInterruptedError extends Error {
  override readonly name = "StreamInterruptedError";
}

type StreamHost = Readonly<{
  nextStreamId: () => number;
  forget: (stream: ChainSyncStream) => void;
  activity: (delta: number) => void;
}>;

/**
 * One chain-sync stream. Events arrive in sequence order without gaps; a
 * restart of the sidecar resumes from the last delivered event (which is at
 * or after the last acknowledged one), so nothing is skipped or repeated.
 */
export class ChainSyncStream implements AsyncIterable<ChainSyncEvent> {
  readonly opened: Promise<Opened>;
  /**
   * Settles once the stream ends: with null when it was closed, with the
   * failure otherwise. It settles even while no consumer awaits an event.
   */
  readonly ended: Promise<Error | null>;
  #resolveEnded!: (cause: Error | null) => void;
  #resolveOpened!: (opened: Opened) => void;
  #rejectOpened!: (error: Error) => void;
  #intersection: ChainPoint | undefined;
  #sidecar: SidecarProcess | undefined;
  #streamId = 0;
  #lastSeq: bigint;
  #acked: bigint;
  #lastPoint: ChainPoint | undefined;
  #recent: ChainPoint[] = [];
  #window: number;
  #tip: ChainTip | undefined;
  #interruptions: StreamInterruption | undefined;
  #queue: ChainSyncEvent[] = [];
  #waiters: Array<() => void> = [];
  #failure: Error | undefined;
  #closed = false;
  #reopenTimer: NodeJS.Timeout | undefined;

  constructor(
    readonly options: ChainSyncOptions,
    readonly host: StreamHost,
  ) {
    if (options.points.length === 0 || options.points.length > MAX_POINTS)
      throw new RangeError(`a stream needs 1..${MAX_POINTS} points`);
    this.#lastSeq = options.startSeq ?? 0n;
    this.#acked = this.#lastSeq;
    this.#window =
      typeof options.credit === "number"
        ? options.credit
        : options.credit.catchUpWindow;
    this.#checkWindow(this.#window);
    if (typeof options.credit !== "number")
      this.#checkWindow(options.credit.tipWindow);
    this.opened = new Promise<Opened>((resolve, reject) => {
      this.#resolveOpened = resolve;
      this.#rejectOpened = reject;
    });
    this.opened.catch(() => undefined);
    this.ended = new Promise<Error | null>((resolve) => {
      this.#resolveEnded = resolve;
    });
    host.activity(1);
  }

  #checkWindow(window: number): void {
    if (!Number.isInteger(window) || window < 1 || window > MAX_WINDOW)
      throw new RangeError(`credit must be within 1..${MAX_WINDOW}`);
  }

  get lastSeq(): bigint {
    return this.#lastSeq;
  }

  get ackedSeq(): bigint {
    return this.#acked;
  }

  /** The node tip the last event reported. */
  get tip(): ChainTip | undefined {
    return this.#tip;
  }

  /** The failures this stream reopened from; undefined while there were none. */
  get interruptions(): StreamInterruption | undefined {
    return this.#interruptions;
  }

  /** Records a failure the stream reopens from, and tells the consumer. */
  #interrupted(last: string): void {
    const interruption: StreamInterruption = {
      consecutive: (this.#interruptions?.consecutive ?? 0) + 1,
      total: (this.#interruptions?.total ?? 0) + 1,
      last,
    };
    this.#interruptions = interruption;
    try {
      this.options.onInterrupted?.(interruption);
    } catch {
      // The record stands; a listener's failure does not stop the reopen.
    }
  }

  get catchingUp(): boolean {
    return this.#window !== this.#tipWindow();
  }

  #tipWindow(): number {
    return typeof this.options.credit === "number"
      ? this.options.credit
      : this.options.credit.tipWindow;
  }

  /** Called by the transport whenever a sidecar becomes ready. */
  attach(sidecar: SidecarProcess): void {
    if (this.#closed || this.#failure !== undefined) return;
    const resuming = this.#intersection !== undefined;
    if (resuming && this.options.resume === false) return;
    this.#sidecar = sidecar;
    void this.#open(sidecar, resuming);
  }

  /** Called by the transport when the sidecar ends. */
  /** Ends the stream with `error`: its transport failed and does not restart. */
  failWith(error: Error): void {
    this.#fail(error);
  }

  detach(exit: SidecarExit): void {
    if (this.#sidecar === undefined) return;
    this.#sidecar = undefined;
    if (this.#closed) return;
    if (this.options.resume === false) this.#fail(new SidecarExitedError(exit));
  }

  #resumePoints(): ChainPoint[] {
    const candidates: ChainPoint[] = [];
    if (this.#lastPoint !== undefined) candidates.push(this.#lastPoint);
    candidates.push(...[...this.#recent].reverse());
    if (this.#intersection !== undefined) candidates.push(this.#intersection);
    candidates.push(...this.options.points);
    const seen = new Set<string>();
    return candidates
      .filter((point) => {
        const key = pointKey(point);
        if (seen.has(key)) return false;
        seen.add(key);
        return true;
      })
      .slice(0, MAX_POINTS);
  }

  async #open(sidecar: SidecarProcess, resuming: boolean): Promise<void> {
    const stream = this.host.nextStreamId();
    this.#streamId = stream;
    sidecar.registerStream(stream, (frame) => this.#frame(sidecar, frame));
    try {
      // A resume lists the last delivered point first: the consumer's
      // position, so the sidecar delivers a rollback exactly when that point
      // left the node's chain.
      const answer = await sidecar.request({
        type: "cs_open",
        stream,
        points: (resuming ? this.#resumePoints() : this.options.points).map(
          encodePoint,
        ),
        startSeq: this.#lastSeq,
        ackedSeq: this.#acked,
        window: this.#window,
      });
      const tip = decodeTip(answer.header.tip, "tip");
      if (answer.header.type === "cs_intersect_not_found") {
        sidecar.unregisterStream(stream);
        this.#fail(new IntersectNotFoundError(tip, resuming));
        return;
      }
      const intersection = decodePoint(answer.header.point, "intersection");
      this.#tip = tip;
      if (!resuming) {
        this.#intersection = intersection;
        this.#lastPoint = intersection;
        this.#resolveOpened({ intersection, tip });
      }
      if (this.#closed) await this.#sendClose(sidecar, stream);
    } catch (error) {
      sidecar.unregisterStream(stream);
      if (error instanceof SidecarExitedError) return;
      if (
        error instanceof TransportRequestError &&
        STREAM_REOPEN_CODES.has(error.code) &&
        this.options.resume !== false
      ) {
        this.#interrupted(`chain-sync open failed: ${error.message}`);
        this.#scheduleReopen(sidecar);
      } else this.#fail(error as Error);
    }
  }

  #scheduleReopen(sidecar: SidecarProcess): void {
    if (this.#closed || this.#failure !== undefined) return;
    this.#reopenTimer = setTimeout(() => {
      this.#reopenTimer = undefined;
      if (this.#sidecar === sidecar && sidecar.exited === undefined)
        void this.#open(sidecar, this.#intersection !== undefined);
    }, REOPEN_DELAY_MS);
  }

  #frame(sidecar: SidecarProcess, frame: Frame): void {
    if (this.#closed || this.#failure !== undefined) return;
    try {
      const header = frame.header;
      if (header.type === "cs_failed") {
        sidecar.unregisterStream(this.#streamId);
        const code = headerText(header.code);
        const cause = new StreamInterruptedError(
          `chain-sync stream failed: ${code}: ${headerText(header.message)}`,
        );
        if (!STREAM_REOPEN_CODES.has(code))
          this.#fail(
            new TransportRequestError(
              code,
              `chain-sync stream failed: ${headerText(header.message)}`,
            ),
          );
        else if (this.options.resume === false) this.#fail(cause);
        else {
          this.#interrupted(cause.message);
          this.#scheduleReopen(sidecar);
        }
        return;
      }
      const seq = natural(header.seq, "sequence number");
      if (seq !== this.#lastSeq + 1n)
        throw new TransportProtocolError(
          `sequence ${seq} does not follow ${this.#lastSeq}`,
        );
      const tip = decodeTip(header.tip, "tip");
      let event: ChainSyncEvent;
      if (header.type === "cs_roll_forward") {
        const point = decodePoint(header.point, "block point");
        if (point.kind === "origin")
          throw new TransportProtocolError("a block cannot be at the origin");
        event = Object.freeze({
          kind: "roll_forward",
          seq,
          point,
          blockNo: natural(header.blockNo, "block number"),
          blockType: Number(natural(header.blockType, "block type")),
          prevHash:
            header.prevHash === undefined
              ? null
              : bytesToHex(bytesOf(header.prevHash, 32, "parent hash")),
          tip,
          block: frame.payload,
        });
        this.#recent.push(point);
        if (this.#recent.length > RESUME_POINTS) this.#recent.shift();
        this.#adjustCredit(event.blockNo, tip.blockNo);
      } else {
        const point = decodePoint(header.point, "rollback point");
        event = Object.freeze({ kind: "roll_backward", seq, point, tip });
        const at = this.#recent.findIndex((entry) => samePoint(entry, point));
        this.#recent = at < 0 ? [] : this.#recent.slice(0, at + 1);
      }
      this.#lastSeq = seq;
      this.#lastPoint = event.point;
      this.#tip = tip;
      if (this.#interruptions !== undefined)
        this.#interruptions = { ...this.#interruptions, consecutive: 0 };
      this.#queue.push(event);
      this.#wake();
    } catch (error) {
      this.#fail(error as Error);
      sidecar.kill();
    }
  }

  #adjustCredit(blockNo: bigint, tipBlockNo: bigint): void {
    if (typeof this.options.credit === "number") return;
    const policy = this.options.credit;
    const desired =
      tipBlockNo - blockNo > policy.catchUpDistance
        ? policy.catchUpWindow
        : policy.tipWindow;
    if (desired !== this.#window) this.#setWindow(desired);
  }

  #setWindow(window: number): void {
    this.#window = window;
    this.#sidecar?.send({ type: "cs_window", stream: this.#streamId, window });
  }

  /** Grants a fixed credit (only for a stream opened with a fixed credit). */
  setCredit(window: number): void {
    if (typeof this.options.credit !== "number")
      throw new TypeError("this stream's credit follows its policy");
    this.#checkWindow(window);
    this.#setWindow(window);
  }

  /** Acknowledges every event up to seq as durably consumed. */
  ack(seq: bigint): void {
    if (seq < this.#acked || seq > this.#lastSeq)
      throw new RangeError(
        `ack ${seq} is outside the delivered range ${this.#acked}..${this.#lastSeq}`,
      );
    if (seq === this.#acked) return;
    this.#acked = seq;
    this.#sidecar?.send({ type: "cs_ack", stream: this.#streamId, seq });
  }

  async close(): Promise<void> {
    if (this.#closed) return;
    this.#closed = true;
    // A failed stream already released its sidecar stream and activity.
    if (this.#failure !== undefined) return;
    clearTimeout(this.#reopenTimer);
    this.#rejectOpened(new StreamInterruptedError("stream closed"));
    this.#resolveEnded(null);
    const sidecar = this.#sidecar;
    this.#sidecar = undefined;
    this.#wake();
    this.host.forget(this);
    try {
      if (sidecar !== undefined) await this.#sendClose(sidecar, this.#streamId);
    } finally {
      this.host.activity(-1);
    }
  }

  async #sendClose(sidecar: SidecarProcess, stream: number): Promise<void> {
    sidecar.unregisterStream(stream);
    await sidecar.request({ type: "cs_close", stream }).catch(() => undefined);
  }

  #fail(error: Error): void {
    if (this.#failure !== undefined || this.#closed) return;
    this.#failure = error;
    clearTimeout(this.#reopenTimer);
    this.#rejectOpened(error);
    this.#resolveEnded(error);
    this.#sidecar?.unregisterStream(this.#streamId);
    this.#sidecar = undefined;
    this.host.forget(this);
    this.host.activity(-1);
    this.#wake();
  }

  #wake(): void {
    for (const waiter of this.#waiters.splice(0)) waiter();
  }

  /** The next event, or undefined once the stream is closed. */
  async next(): Promise<ChainSyncEvent | undefined> {
    for (;;) {
      const event = this.#queue.shift();
      if (event !== undefined) return event;
      if (this.#failure !== undefined) throw this.#failure;
      if (this.#closed) return undefined;
      await new Promise<void>((resolve) => this.#waiters.push(resolve));
    }
  }

  async *[Symbol.asyncIterator](): AsyncIterator<ChainSyncEvent> {
    for (;;) {
      const event = await this.next();
      if (event === undefined) return;
      yield event;
    }
  }
}
