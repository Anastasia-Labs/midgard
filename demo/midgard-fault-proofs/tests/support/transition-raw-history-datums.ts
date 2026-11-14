/** The raw Deposit datums the transition-trace raw-history emulator proves. */
import type * as SDK from "@al-ft/midgard-sdk";

export const rawDatum = "a3020a010b020c";
/** Deposit datums a typed Data codec cannot carry. */
const RAW_DATUMS: readonly (readonly [string, string])[] = [
  ["an unsorted map that repeats a key", rawDatum],
  ["a tagged small bignum", "c24105"],
  ["a general-form constructor 0", "d866820080"],
  ["a map with a repeated key", "a201020103"],
];
/** A definite list head over `count` items. */
const listHead = (count: number) =>
  count < 24
    ? (0x80 + count).toString(16)
    : count < 256
      ? `98${count.toString(16).padStart(2, "0")}`
      : `99${count.toString(16).padStart(4, "0")}`;
/** `nodes` Data nodes: lists each holding the next, around an empty list. */
const nestedDatum = (nodes: number) => "81".repeat(nodes - 1) + "80";
/** `nodes` Data nodes: one list of `nodes - 1` zeros. */
const flatDatum = (nodes: number) =>
  listHead(nodes - 1) + "00".repeat(nodes - 1);
/** The deployment manifest's own payload bounds (1,024 nodes). */
const MANIFEST_BOUNDS: SDK.EventHistoryPayloadBounds = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 15_000n,
  maxPayloadNodes: 1_024n,
};
/**
 * Deposit datums whose summary takes more than one transaction, or whose
 * constructor index or integer is outside the 64-bit range.
 */
const LARGE_DATUMS: readonly (readonly [string, string])[] = [
  ["512 nested nodes", nestedDatum(512)],
  ["512 flat nodes", flatDatum(512)],
  // The deposit event around the datum takes 12 of the payload's 1,024 nodes.
  ["1,012 nested nodes", nestedDatum(1_012)],
  ["1,012 flat nodes", flatDatum(1_012)],
  ["constructor 1280", "d8668219050080"],
  ["constructor 2^64 - 1", "d866821bffffffffffffffff80"],
  ["integer 2^64", "c249010000000000000000"],
  ["integer 2^512", `c25841${"01"}${"00".repeat(64)}`],
  ["integer -2^512 - 1", `c35841${"01"}${"00".repeat(64)}`],
  ["integer -2^512", `c35840${"ff".repeat(64)}`],
];

/** One emulator row: a datum, the operator's honesty, and payload bounds. */
export type RawHistoryRow = {
  name: string;
  datum: string;
  honest: boolean;
  bounds?: SDK.EventHistoryPayloadBounds;
};

export const RAW_HISTORY_ROWS: readonly RawHistoryRow[] = [
  ...RAW_DATUMS.map(([name, datum]) => ({ name, datum, honest: false })),
  { name: "an unsorted map that repeats a key", datum: rawDatum, honest: true },
  ...LARGE_DATUMS.map(([name, datum]) => ({
    name,
    datum,
    honest: false,
    bounds: MANIFEST_BOUNDS,
  })),
  ...LARGE_DATUMS.filter(([name]) =>
    ["512 nested nodes", "constructor 2^64 - 1", "integer -2^512"].includes(
      name,
    ),
  ).map(([name, datum]) => ({
    name,
    datum,
    honest: true,
    bounds: MANIFEST_BOUNDS,
  })),
];
