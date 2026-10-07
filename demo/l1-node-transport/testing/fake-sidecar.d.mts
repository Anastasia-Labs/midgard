/** A point as the fake's handlers see it. Hashes are lowercase hex. */
export type FakePoint =
  | "origin"
  | Readonly<{ slot: bigint | number | string; hash: string }>;

export type FakeTip = Readonly<{
  point: FakePoint;
  blockNo: bigint | number | string;
}>;

export type FakeRollForward = Readonly<{
  point: FakePoint;
  blockNo: bigint | number | string;
  blockType?: number;
  /** null or absent for the chain's first block. */
  prevHash?: string | null;
  tip: FakeTip;
  block: Uint8Array;
}>;

/** One open chain-sync stream: events are delivered within its credit. */
export type FakeStream = Readonly<{
  readonly closed: boolean;
  readonly unacked: number;
  readonly queued: number;
  rollForward(event: FakeRollForward): void;
  rollBackward(event: Readonly<{ point: FakePoint; tip: FakeTip }>): void;
  /**
   * Ends the stream with `cs_failed` after the events the credit lets
   * through; events still held back are dropped. The session lives on.
   */
  fail(code: string, message?: string): void;
  onClose(listener: () => void): void;
}>;

type Refusal = Readonly<{
  error: Readonly<{ code: string; message?: string }>;
}>;

export type FakeSidecarHandler = Readonly<{
  hello?: (request: { socketPath: string; networkMagic: number }) =>
    | void
    | Readonly<{ nodeToClientVersion?: number }>
    | Readonly<{
        fatal: Readonly<{ code: string; message?: string; status?: number }>;
      }>
    | Promise<unknown>;
  openStream?: (
    request: Readonly<{
      points: readonly FakePoint[];
      startSeq: bigint;
      /** The stream's initial credit window. */
      window: number;
      consumerAt: FakePoint | undefined;
    }>,
    stream: FakeStream,
  ) =>
    | Readonly<{ intersection: FakePoint; tip: FakeTip }>
    | Readonly<{ notFound: FakeTip }>
    | Refusal
    | undefined
    | Promise<
        | Readonly<{ intersection: FakePoint; tip: FakeTip }>
        | Readonly<{ notFound: FakeTip }>
        | Refusal
        | undefined
      >;
  acquire?: (point: FakePoint | undefined) => void | Refusal | Promise<unknown>;
  ledgerQuery?: (
    query: Readonly<Record<string, unknown>>,
    acquired: FakePoint | "tip",
  ) =>
    | Uint8Array
    | Refusal
    | undefined
    | Promise<Uint8Array | Refusal | undefined>;
  submit?: (
    tx: Uint8Array,
    era: number | undefined,
  ) =>
    | Readonly<{ accepted: true }>
    | Readonly<{ rejection: Uint8Array }>
    | Refusal
    | Promise<unknown>;
  hasTx?: (txId: string) => boolean | Promise<boolean>;
  /** Keeps the process running after its input closes, as a wedged sidecar. */
  ignoreInputEnd?: boolean;
  sizes?: () =>
    | Readonly<{ capacity: number; size: number; txCount: number }>
    | Promise<Readonly<{ capacity: number; size: number; txCount: number }>>;
}>;

export type FakeSessionControls = Readonly<{
  /** Ends the session with a `fatal` frame and the given exit status. */
  fatal(code: string, message: string, status?: number): void;
  /** Ends the process without a frame, as a crash would. */
  exit(status: number): void;
}>;

/**
 * The handler module (testing/ledger-handler.mjs) of a node ledger holding
 * one script credential. Options: `magic` (number), `scriptHash` (hex),
 * `pool` (pool key hash hex), `rewards` (lovelace), `silent` (boolean).
 */
export declare const LEDGER_HANDLER: string;

export declare const encodeCbor: (value: unknown) => Buffer;
export declare const decodeCbor: (bytes: Uint8Array) => unknown;
export declare const samePoint: (a: FakePoint, b: FakePoint) => boolean;
export declare const serveFakeSidecar: (
  handler: FakeSidecarHandler,
) => FakeSessionControls;

/**
 * Writes an executable fake sidecar at `path`. The handler module's default
 * export is called with `options` and the session controls, and returns the
 * handler.
 */
export declare const writeFakeSidecar: (
  input: Readonly<{
    path: string;
    handlerModule: string;
    options?: unknown;
  }>,
) => Promise<string>;
