import { createHash } from "node:crypto";
import { readFile, stat } from "node:fs/promises";
import { join } from "node:path";

import {
  MAX_RECORD_BYTES,
  recordName,
  revision,
} from "./trusted-head-authority.exact-record.js";
import type { AuthorityRecordCodec } from "./trusted-head-authority.record-codec.js";

export const auditLegacyAuthorityRecords = async (
  input: Readonly<{
    directory: string;
    records: AuthorityRecordCodec;
    liveRecordLimit: number;
    recordNames: readonly string[];
  }>,
) => {
  const names = input.recordNames;
  const chain = createHash("sha256").update(
    "midgard-watcher-authority-legacy-chain-v1:",
  );
  let tip: ReturnType<typeof input.records.admitRecordBytes> | null = null;
  const retained: Uint8Array[] = [];
  let boundary: ReturnType<typeof input.records.admitRecord> | null = null;
  for (let i = 0; i < names.length; i++) {
    const name = names[i]!,
      path = join(input.directory, name);
    if (name !== recordName(BigInt(i)))
      throw new Error("trusted-head legacy chain revision gap");
    const size = (await stat(path)).size;
    if (size < 1 || size > MAX_RECORD_BYTES)
      throw new Error("trusted-head legacy record size invalid");
    const raw = await readFile(path),
      entry = input.records.admitRecordBytes(raw);
    if (
      revision(entry.record.head) !== BigInt(i) ||
      entry.record.priorRecordSha256 !== (tip?.recordSha256 ?? null)
    )
      throw new Error("trusted-head legacy chain differs");
    chain.update(name).update("\0").update(raw);
    tip = entry;
    if (i === names.length - input.liveRecordLimit - 1) boundary = entry.record;
    if (i >= names.length - input.liveRecordLimit) retained.push(raw);
  }
  return Object.freeze({
    head: tip?.record.head ?? null,
    recordSha256: tip?.recordSha256 ?? null,
    sourceChainSha256: chain.digest("hex"),
    initialRecords: retained,
    boundaryRecord: boundary,
  });
};
