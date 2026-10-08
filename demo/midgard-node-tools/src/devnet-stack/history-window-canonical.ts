import { createHash } from "node:crypto";
import { closeSync, constants, fstatSync, openSync, readSync } from "node:fs";
import { join } from "node:path";

import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import {
  admitWatcherNativeRollForwardBlock,
  WATCHER_CARDANO_SECURITY_PARAMETER_K,
  type WatcherNativeChainSyncRollForward,
} from "midgard-watcher";

export class HistoryWindowRefusal extends Error {
  constructor(readonly reason: string) {
    super(`retained history window refused: ${reason}`);
    this.name = "HistoryWindowRefusal";
  }
}
export type HistoryWindowPoint = Readonly<{
  blockHash: string;
  blockNo: string;
  slot: string;
  pointId: string;
}>;
export type Row = Readonly<{
  point: HistoryWindowPoint;
  prevHash: string;
  bytes: string;
}>;
export const digest = (value: string) =>
  createHash("sha256").update(value).digest("hex");
export const refuse = (reason: string): never => {
  throw new HistoryWindowRefusal(reason);
};
const record = (v: unknown): v is Record<string, unknown> =>
  typeof v === "object" && v !== null && !Array.isArray(v);
const keys = (v: Record<string, unknown>, expected: readonly string[]) =>
  Object.keys(v).sort().join() === [...expected].sort().join();
const uint = (v: unknown): v is string =>
  typeof v === "string" &&
  /^(?:0|[1-9][0-9]*)$/u.test(v) &&
  v.length <= 20 &&
  BigInt(v) <= (1n << 64n) - 1n;
const hex = (v: unknown): v is string =>
  typeof v === "string" && /^[0-9a-f]{64}$/u.test(v);
export const point = (v: unknown): HistoryWindowPoint => {
  if (
    !record(v) ||
    !keys(v, ["blockHash", "blockNo", "slot", "pointId"]) ||
    !hex(v.blockHash) ||
    !uint(v.blockNo) ||
    !uint(v.slot) ||
    !hex(v.pointId)
  )
    return refuse("malformed canonical point");
  const p = Object.freeze({
    blockHash: v.blockHash,
    blockNo: v.blockNo,
    slot: v.slot,
    pointId: v.pointId,
  });
  if (computeFraudProofRawL1PointId(p) !== p.pointId)
    return refuse("canonical point identity mismatch");
  return p;
};
const maxRowBytes = Buffer.byteLength(
  JSON.stringify({
    point: {
      blockHash: "f".repeat(64),
      blockNo: ((1n << 64n) - 1n).toString(),
      slot: ((1n << 64n) - 1n).toString(),
      pointId: "f".repeat(64),
    },
    prevHash: "f".repeat(64),
  }),
);
const bytesAt = (path: string): string => {
  try {
    const fd = openSync(path, constants.O_RDONLY | constants.O_NOFOLLOW);
    try {
      const stat = fstatSync(fd);
      if (!stat.isFile() || stat.size > maxRowBytes)
        return refuse("nonregular or oversized retained evidence");
      const bytes = Buffer.alloc(maxRowBytes + 1);
      let size = 0;
      while (size < bytes.length) {
        const n = readSync(fd, bytes, size, bytes.length - size, size);
        if (n === 0) break;
        size += n;
      }
      if (size > maxRowBytes) return refuse("oversized retained evidence");
      try {
        return new TextDecoder("utf8", { fatal: true }).decode(
          bytes.subarray(0, size),
        );
      } catch {
        return refuse("malformed retained encoding");
      }
    } finally {
      closeSync(fd);
    }
  } catch (error) {
    if (error instanceof HistoryWindowRefusal) throw error;
    if (
      error instanceof Error &&
      "code" in error &&
      ["ENOENT", "ENOTDIR", "ELOOP", "EACCES", "EPERM"].some(
        (code) => code === error.code,
      )
    )
      return refuse("missing retained evidence");
    throw error;
  }
};
export const rowAt = (directories: readonly string[], n: bigint): Row => {
  const bytes = directories.map((d) =>
    bytesAt(join(d, "canonical", `${n}.json`)),
  );
  if (bytes[0] === undefined || bytes.some((b) => b !== bytes[0]))
    return refuse("provider canonical disagreement");
  let value: unknown;
  try {
    value = JSON.parse(bytes[0]);
  } catch {
    return refuse("malformed canonical row");
  }
  if (
    !record(value) ||
    !keys(value, ["point", "prevHash"]) ||
    !(hex(value.prevHash) || (n === 0n && value.prevHash === ""))
  )
    return refuse("malformed canonical lineage");
  const p = point(value.point);
  if (
    BigInt(p.blockNo) !== n ||
    JSON.stringify({ point: p, prevHash: value.prevHash }) !== bytes[0]
  )
    return refuse("noncanonical retained bytes");
  return Object.freeze({ point: p, prevHash: value.prevHash, bytes: bytes[0] });
};
export const publication = (directories: readonly string[]): string => {
  const held = directories.map((d) => bytesAt(join(d, "canonical-ready")));
  let value: unknown;
  try {
    value = JSON.parse(held[0] ?? "");
  } catch {
    return refuse("missing coherent publication");
  }
  if (
    typeof value !== "string" ||
    !/^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/u.test(value) ||
    held.some((v) => v !== held[0])
  )
    return refuse("incoherent publication");
  return value;
};
export const rangeAt = (
  directories: readonly string[],
  first: bigint,
  last: bigint,
): readonly Row[] => {
  if (
    first < 0n ||
    last < first ||
    last - first + 1n > BigInt(WATCHER_CARDANO_SECURITY_PARAMETER_K)
  )
    return refuse("required range exceeds configured recovery window");
  const before = publication(directories);
  const rows: Row[] = [];
  for (let n = first; n <= last; n++) {
    const row = rowAt(directories, n);
    const previous = rows[rows.length - 1];
    if (
      previous !== undefined &&
      (row.prevHash !== previous.point.blockHash ||
        BigInt(row.point.slot) <= BigInt(previous.point.slot))
    )
      return refuse("retained lineage is not contiguous");
    rows.push(row);
  }
  if (publication(directories) !== before) return refuse("torn publication");
  return rows;
};
export const rowFromEvent = (event: WatcherNativeChainSyncRollForward): Row => {
  let admitted;
  try {
    admitted = admitWatcherNativeRollForwardBlock(event);
  } catch {
    return refuse("raw native row contradiction");
  }
  const p = Object.freeze({
    blockHash: admitted.blockHash,
    blockNo: admitted.blockNo,
    slot: admitted.slot,
    pointId: computeFraudProofRawL1PointId(admitted),
  });
  return Object.freeze({
    point: p,
    prevHash: admitted.prevHash,
    bytes: JSON.stringify({ point: p, prevHash: admitted.prevHash }),
  });
};
export const rangeDigest = (rows: readonly Row[]) =>
  digest(rows.map((r) => r.bytes).join("\n"));
