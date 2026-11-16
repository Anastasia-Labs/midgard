/**
 * The message between the main thread and the worker thread that derives
 * the landed ledger's two roots (`workers/confirmed-ledger-root.ts`):
 * `confirmed_ledger`'s entries and the processed deltas after the frontier,
 * packed into transferable buffers, and the two roots or the failure back.
 * Both sides use the constructors and parsers here.
 */
import type * as Ledger from "../database/utils/ledger.js";

/** One change a processed landed row makes to the ledger before it. */
export type ConfirmedLedgerDeltaOp =
  | Readonly<{ kind: "spent"; outref: Buffer }>
  | Readonly<{ kind: "produced"; outref: Buffer; output: Buffer }>;

/** What the worker derives the roots of: `confirmed_ledger` at the
 * frontier, then the deltas after it, applied in order. */
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

/** The worker's `workerData`: the input packed, a spent outref as an entry
 * with an empty output. */
export type ConfirmedLedgerRootsRequest = Readonly<{
  confirmed: PackedEntries;
  delta: PackedEntries;
  /** Per delta entry, one byte: 0 spent, 1 produced. */
  deltaKinds: ArrayBuffer;
}>;

/** The worker's one message: the roots its input derives, or why not. */
export type ConfirmedLedgerRootsResponse =
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

/** `input` packed, with the buffers to transfer. */
export const packConfirmedLedgerRootsRequest = (
  input: ConfirmedLedgerRootsInput,
): { request: ConfirmedLedgerRootsRequest; transfer: ArrayBuffer[] } => {
  const { transfer: confirmedTransfer, ...confirmed } = pack(input.confirmed);
  const { transfer: deltaTransfer, ...delta } = pack(
    input.delta.map((op) =>
      op.kind === "spent" ? { outref: op.outref, output: EMPTY } : op,
    ),
  );
  const deltaKinds = new ArrayBuffer(input.delta.length);
  new Uint8Array(deltaKinds).set(
    input.delta.map((op) => (op.kind === "spent" ? SPENT : PRODUCED)),
  );
  return {
    request: { confirmed, delta, deltaKinds },
    transfer: [...confirmedTransfer, ...deltaTransfer, deltaKinds],
  };
};

const hasExactly = (value: object, keys: readonly string[]) =>
  Object.keys(value).sort().join() === [...keys].sort().join();

const unpack = (
  value: unknown,
  name: string,
): Ledger.MinimalEntry[] | string => {
  const packed = value as Partial<Record<keyof PackedEntries, unknown>>;
  if (
    typeof value !== "object" ||
    value === null ||
    !hasExactly(value, ["outRefs", "outputs", "offsets"]) ||
    !(packed.outRefs instanceof ArrayBuffer) ||
    !(packed.outputs instanceof ArrayBuffer) ||
    !(packed.offsets instanceof ArrayBuffer) ||
    packed.offsets.byteLength % (4 * Uint32Array.BYTES_PER_ELEMENT) !== 0
  )
    return `${name} is not three packed buffers`;
  const outRefs = Buffer.from(packed.outRefs);
  const outputs = Buffer.from(packed.outputs);
  const offsets = new Uint32Array(packed.offsets);
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

/** The input a request carries, or a description of why it is not one. */
export const unpackConfirmedLedgerRootsRequest = (
  value: unknown,
): ConfirmedLedgerRootsInput | string => {
  const request = value as Partial<
    Record<keyof ConfirmedLedgerRootsRequest, unknown>
  >;
  if (
    typeof value !== "object" ||
    value === null ||
    !hasExactly(value, ["confirmed", "delta", "deltaKinds"]) ||
    !(request.deltaKinds instanceof ArrayBuffer)
  )
    return "the request is not confirmed_ledger, a delta and its kinds";
  const confirmed = unpack(request.confirmed, "confirmed_ledger");
  if (typeof confirmed === "string") return confirmed;
  const entries = unpack(request.delta, "the delta");
  if (typeof entries === "string") return entries;
  const kinds = new Uint8Array(request.deltaKinds);
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
  return { confirmed, delta };
};

const ROOT = /^[0-9a-f]{64}$/u;

/** The roots a response carries; a failure or anything else is an error. */
export const parseConfirmedLedgerRootsResponse = (
  value: unknown,
): ConfirmedLedgerRoots => {
  if (typeof value === "object" && value !== null) {
    const response = value as Partial<
      Record<"frontierRoot" | "tailRoot" | "error", unknown>
    >;
    if (
      hasExactly(value, ["frontierRoot", "tailRoot"]) &&
      typeof response.frontierRoot === "string" &&
      ROOT.test(response.frontierRoot) &&
      typeof response.tailRoot === "string" &&
      ROOT.test(response.tailRoot)
    )
      return {
        frontierRoot: response.frontierRoot,
        tailRoot: response.tailRoot,
      };
    if (hasExactly(value, ["error"]) && typeof response.error === "string")
      throw new Error(`confirmed_ledger root worker failed: ${response.error}`);
  }
  throw new Error(
    `confirmed_ledger root worker sent no roots: ${JSON.stringify(value)}`,
  );
};
