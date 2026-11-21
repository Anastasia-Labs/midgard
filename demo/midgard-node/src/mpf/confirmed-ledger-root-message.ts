/**
 * The messages between the main thread and the worker thread that derives
 * the landed ledger's two roots (`workers/confirmed-ledger-root.ts`). The
 * worker starts with the processed deltas after the frontier, packed into
 * transferable buffers (its `workerData`); the main thread then asks, one
 * request at a time, for the leaves of a page of `confirmed_ledger`
 * entries, feeds it the leaves in path order, and asks for the roots. Each
 * reply carries what was asked, or the failure. Both sides use the
 * constructors and parsers here.
 */
import type * as Ledger from "../database/utils/ledger.js";

/** One change a processed landed row makes to the ledger before it. */
export type ConfirmedLedgerDeltaOp =
  | Readonly<{ kind: "spent"; outref: Buffer }>
  | Readonly<{ kind: "produced"; outref: Buffer; output: Buffer }>;

/** A ledger held whole: `confirmed_ledger` at the frontier, then the
 * deltas after it, applied in order. */
export type ConfirmedLedgerRootsInput = Readonly<{
  confirmed: readonly Ledger.MinimalEntry[];
  delta: readonly ConfirmedLedgerDeltaOp[];
}>;

/** The roots of an input: `confirmed_ledger`'s, then with the deltas. */
export type ConfirmedLedgerRoots = Readonly<{
  frontierRoot: string;
  tailRoot: string;
}>;

/** Entries' outrefs and outputs, packed. */
type PackedEntries = Readonly<{
  outRefs: ArrayBuffer;
  outputs: ArrayBuffer;
  /** Per entry: outref offset, outref length, output offset, output length. */
  offsets: ArrayBuffer;
}>;

/** The worker's `workerData`: the deltas packed, a spent outref as an
 * entry with an empty output. */
export type ConfirmedLedgerRootsRequest = Readonly<{
  delta: PackedEntries;
  /** Per delta entry, one byte: 0 spent, 1 produced. */
  deltaKinds: ArrayBuffer;
}>;

/** What the main thread asks the worker for. */
export type ConfirmedLedgerRootsCommand =
  | Readonly<{ kind: "leaves"; entries: PackedEntries }>
  | Readonly<{ kind: "feed"; leaves: ArrayBuffer }>
  | Readonly<{ kind: "roots" }>;

/** A command as the worker reads it. */
export type ConfirmedLedgerRootsCommandInput =
  | Readonly<{ kind: "leaves"; entries: Ledger.MinimalEntry[] }>
  | Readonly<{ kind: "feed"; leaves: Uint8Array }>
  | Readonly<{ kind: "roots" }>;

/** The worker's reply to one command, or why it failed. */
export type ConfirmedLedgerRootsResponse =
  | Readonly<{ leaves: ArrayBuffer }>
  | Readonly<{ fed: true }>
  | ConfirmedLedgerRoots
  | Readonly<{ error: string }>;

const SPENT = 0;
const PRODUCED = 1;
const EMPTY = Buffer.alloc(0);

const pack = (
  entries: readonly Ledger.MinimalEntry[],
): PackedEntries & { transfer: ArrayBuffer[] } => {
  let outRefBytes = 0;
  let outputBytes = 0;
  for (const entry of entries) {
    outRefBytes += entry.outref.length;
    outputBytes += entry.output.length;
  }
  const outRefs = new ArrayBuffer(outRefBytes);
  const outputs = new ArrayBuffer(outputBytes);
  const offsets = new ArrayBuffer(
    entries.length * 4 * Uint32Array.BYTES_PER_ELEMENT,
  );
  const outRefArena = new Uint8Array(outRefs);
  const outputArena = new Uint8Array(outputs);
  const offsetArena = new Uint32Array(offsets);
  let outRefOffset = 0;
  let outputOffset = 0;
  entries.forEach((entry, index) => {
    outRefArena.set(entry.outref, outRefOffset);
    outputArena.set(entry.output, outputOffset);
    offsetArena.set(
      [outRefOffset, entry.outref.length, outputOffset, entry.output.length],
      index * 4,
    );
    outRefOffset += entry.outref.length;
    outputOffset += entry.output.length;
  });
  return { outRefs, outputs, offsets, transfer: [outRefs, outputs, offsets] };
};

/** The deltas `delta` packed, with the buffers to transfer. */
export const packConfirmedLedgerRootsRequest = (
  delta: readonly ConfirmedLedgerDeltaOp[],
): { request: ConfirmedLedgerRootsRequest; transfer: ArrayBuffer[] } => {
  const { transfer, ...packed } = pack(
    delta.map((op) =>
      op.kind === "spent" ? { outref: op.outref, output: EMPTY } : op,
    ),
  );
  const deltaKinds = new ArrayBuffer(delta.length);
  new Uint8Array(deltaKinds).set(
    delta.map((op) => (op.kind === "spent" ? SPENT : PRODUCED)),
  );
  return {
    request: { delta: packed, deltaKinds },
    transfer: [...transfer, deltaKinds],
  };
};

/** The command for the leaves of `entries`, with the buffers to transfer. */
export const leavesCommand = (
  entries: readonly Ledger.MinimalEntry[],
): { command: ConfirmedLedgerRootsCommand; transfer: ArrayBuffer[] } => {
  const { transfer, ...packed } = pack(entries);
  return { command: { kind: "leaves", entries: packed }, transfer };
};

const hasExactly = (value: object, keys: readonly string[]) =>
  Object.keys(value).sort().join() === [...keys].sort().join();

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const unpack = (
  value: unknown,
  name: string,
): Ledger.MinimalEntry[] | string => {
  if (
    !isRecord(value) ||
    !hasExactly(value, ["outRefs", "outputs", "offsets"]) ||
    !(value.outRefs instanceof ArrayBuffer) ||
    !(value.outputs instanceof ArrayBuffer) ||
    !(value.offsets instanceof ArrayBuffer) ||
    value.offsets.byteLength % (4 * Uint32Array.BYTES_PER_ELEMENT) !== 0
  )
    return `${name} is not three packed buffers`;
  const outRefs = Buffer.from(value.outRefs);
  const outputs = Buffer.from(value.outputs);
  const offsets = new Uint32Array(value.offsets);
  const entries: Ledger.MinimalEntry[] = [];
  for (let index = 0; index < offsets.length; index += 4) {
    const outRefAt = offsets[index]!;
    const outRefLength = offsets[index + 1]!;
    const outputAt = offsets[index + 2]!;
    const outputLength = offsets[index + 3]!;
    if (
      outRefAt + outRefLength > outRefs.length ||
      outputAt + outputLength > outputs.length
    )
      return `${name} entry ${(index / 4).toString()} lies outside its buffers`;
    entries.push({
      outref: outRefs.subarray(outRefAt, outRefAt + outRefLength),
      output: outputs.subarray(outputAt, outputAt + outputLength),
    });
  }
  return entries;
};

/** The deltas a request carries, or a description of why it is not one. */
export const unpackConfirmedLedgerRootsRequest = (
  value: unknown,
): ConfirmedLedgerDeltaOp[] | string => {
  if (
    !isRecord(value) ||
    !hasExactly(value, ["delta", "deltaKinds"]) ||
    !(value.deltaKinds instanceof ArrayBuffer)
  )
    return "the request is not a delta and its kinds";
  const entries = unpack(value.delta, "the delta");
  if (typeof entries === "string") return entries;
  const kinds = new Uint8Array(value.deltaKinds);
  if (kinds.length !== entries.length)
    return `the delta has ${entries.length.toString()} entries and ${kinds.length.toString()} kinds`;
  const delta: ConfirmedLedgerDeltaOp[] = [];
  for (const [index, entry] of entries.entries()) {
    const kind = kinds[index];
    if (kind === SPENT && entry.output.length === 0)
      delta.push({ kind: "spent", outref: entry.outref });
    else if (kind === PRODUCED) delta.push({ kind: "produced", ...entry });
    else
      return `delta entry ${index.toString()} is neither a spent outref nor a produced entry`;
  }
  return delta;
};

/** The command a message carries, or a description of why it is not one. */
export const unpackConfirmedLedgerRootsCommand = (
  value: unknown,
): ConfirmedLedgerRootsCommandInput | string => {
  if (
    isRecord(value) &&
    hasExactly(value, ["kind", "entries"]) &&
    value.kind === "leaves"
  ) {
    const entries = unpack(value.entries, "the entries");
    return typeof entries === "string" ? entries : { kind: "leaves", entries };
  }
  if (
    isRecord(value) &&
    hasExactly(value, ["kind", "leaves"]) &&
    value.kind === "feed" &&
    value.leaves instanceof ArrayBuffer
  )
    return { kind: "feed", leaves: new Uint8Array(value.leaves) };
  if (isRecord(value) && hasExactly(value, ["kind"]) && value.kind === "roots")
    return { kind: "roots" };
  return "the command is not leaves, a feed or the roots";
};

const ROOT = /^[0-9a-f]{64}$/u;

const noReply = (what: string, value: unknown) =>
  new Error(
    `confirmed_ledger root worker sent no ${what}: ${JSON.stringify(value)}`,
  );

/** A reply with exactly the keys `expected`; throws a failure the worker
 * posts, and on anything else, as no `what`. */
const reply = (value: unknown, expected: readonly string[], what: string) => {
  if (isRecord(value)) {
    if (hasExactly(value, expected)) return value;
    if (hasExactly(value, ["error"]) && typeof value.error === "string")
      throw new Error(`confirmed_ledger root worker failed: ${value.error}`);
  }
  throw noReply(what, value);
};

/** The packed leaves a reply carries. */
export const parseConfirmedLedgerLeavesResponse = (
  value: unknown,
): Uint8Array => {
  const { leaves } = reply(value, ["leaves"], "leaves");
  if (!(leaves instanceof ArrayBuffer)) throw noReply("leaves", value);
  return new Uint8Array(leaves);
};

/** That a reply acknowledges a feed. */
export const parseConfirmedLedgerFedResponse = (value: unknown): void => {
  if (reply(value, ["fed"], "feed").fed !== true) throw noReply("feed", value);
};

/** The roots a reply carries. */
export const parseConfirmedLedgerRootsResponse = (
  value: unknown,
): ConfirmedLedgerRoots => {
  const { frontierRoot, tailRoot } = reply(
    value,
    ["frontierRoot", "tailRoot"],
    "roots",
  );
  if (
    typeof frontierRoot !== "string" ||
    !ROOT.test(frontierRoot) ||
    typeof tailRoot !== "string" ||
    !ROOT.test(tailRoot)
  )
    throw noReply("roots", value);
  return { frontierRoot, tailRoot };
};
