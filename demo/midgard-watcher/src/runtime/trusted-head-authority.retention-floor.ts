import { createHmac, timingSafeEqual } from "node:crypto";
import { open, rename, unlink } from "node:fs/promises";
import { join } from "node:path";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  stagedRecordPath,
  syncDirectory,
} from "../storage/exclusive-record-file.js";
import {
  exactRecord,
  parseJson,
  readBounded,
  recordKeyId,
  recordName,
  revision,
  sha256,
  type TrustedHeadAuthorityRecord,
} from "./trusted-head-authority.exact-record.js";

export const RETENTION_FLOOR_FILE = "retention-floor.json";
const SCHEMA = "midgard-watcher-trusted-head-retention-floor-v1";

type FloorContent = Readonly<{
  schemaVersion: typeof SCHEMA;
  record: TrustedHeadAuthorityRecord;
  recordSha256: string;
  recordAuthenticationKeyId: string;
}>;
const mac = (key: Uint8Array, content: FloorContent): string =>
  createHmac("sha256", key)
    .update(`${SCHEMA}:${watcherCanonicalJson(content)}`)
    .digest("hex");

/** This anchor lives in the independent monotonic authority, not the watcher
 * snapshot backend. It summarizes an authenticated prefix, never a new head. */
export const readRetentionFloor = (
  directory: string,
  key: Uint8Array,
  admitRecord: (value: unknown) => TrustedHeadAuthorityRecord,
): Readonly<{
  head: TrustedHeadAuthorityRecord["head"];
  recordSha256: string;
}> => {
  const bytes = readBounded(join(directory, RETENTION_FLOOR_FILE));
  const value = exactRecord(parseJson(bytes), [
    "schemaVersion",
    "record",
    "recordSha256",
    "recordAuthenticationKeyId",
    "floorMac",
  ]);
  if (
    value === null ||
    value.schemaVersion !== SCHEMA ||
    value.recordAuthenticationKeyId !== recordKeyId(key) ||
    typeof value.floorMac !== "string" ||
    !/^[0-9a-f]{64}$/u.test(value.floorMac)
  )
    throw new Error("trusted-head authority retention floor is invalid");
  const record = admitRecord(value.record);
  const content: FloorContent = {
    schemaVersion: SCHEMA,
    record,
    recordSha256: sha256(watcherCanonicalJson(record)),
    recordAuthenticationKeyId: recordKeyId(key),
  };
  const expected = { ...content, floorMac: mac(key, content) };
  if (
    value.recordSha256 !== content.recordSha256 ||
    !timingSafeEqual(
      Buffer.from(value.floorMac, "hex"),
      Buffer.from(expected.floorMac, "hex"),
    ) ||
    new TextDecoder().decode(bytes) !== watcherCanonicalJson(expected)
  )
    throw new Error(
      "trusted-head authority retention floor authentication failed",
    );
  return Object.freeze({
    head: record.head,
    recordSha256: content.recordSha256,
  });
};

/** Publish and fsync the floor before deleting any summarized file. An
 * interrupted unpublished floor is a staging file and never an authority; a
 * failed publication removes its own staging file before rethrowing, so a
 * full disk does not accumulate them between restarts. Interrupted cleanup
 * leaves harmless prefix files below the authenticated floor (`belowFloor`),
 * which the next compaction removes. `names` is the scanned chain at and
 * above the current floor, oldest first. */
export const compactTrustedHeadRecords = async (
  directory: string,
  names: readonly string[],
  belowFloor: readonly string[],
  maxRetainedRecords: number,
  key: Uint8Array,
  admitRecord: (value: unknown) => TrustedHeadAuthorityRecord,
): Promise<void> => {
  if (names.length <= maxRetainedRecords) return;
  const retired = names.slice(0, names.length - maxRetainedRecords);
  const last = retired.at(-1)!;
  const record = admitRecord(parseJson(readBounded(join(directory, last))));
  if (recordName(revision(record.head)) !== last)
    throw new Error("trusted-head authority retention floor identity differs");
  const content: FloorContent = {
    schemaVersion: SCHEMA,
    record,
    recordSha256: sha256(watcherCanonicalJson(record)),
    recordAuthenticationKeyId: recordKeyId(key),
  };
  const pending = stagedRecordPath(directory);
  const handle = await open(pending, "wx", 0o600);
  try {
    try {
      await handle.writeFile(
        watcherCanonicalJson({ ...content, floorMac: mac(key, content) }),
        "utf8",
      );
      await handle.sync();
    } finally {
      await handle.close();
    }
    await rename(pending, join(directory, RETENTION_FLOOR_FILE));
  } catch (error) {
    // Best effort: the original failure is the one to report, and a staging
    // file left behind is still removed when the store next opens.
    await unlink(pending).catch(() => undefined);
    throw error;
  }
  await syncDirectory(directory);
  for (const name of [...belowFloor, ...retired])
    await unlink(join(directory, name));
  await syncDirectory(directory);
};
