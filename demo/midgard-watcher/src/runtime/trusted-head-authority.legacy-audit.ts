import { readdir, realpath } from "node:fs/promises";

import { STAGED_RECORD_FILE } from "../storage/exclusive-record-file.js";
import { RECORD_FILE } from "./trusted-head-authority.exact-record.js";
import { auditLegacyAuthorityRecords } from "./trusted-head-authority.legacy-prefix-audit.js";
import type { AuthorityRecordCodec } from "./trusted-head-authority.record-codec.js";

export const readLegacyAuthorityRecordNames = async (directory: string) => {
  if ((await realpath(directory)) !== directory)
    throw new Error("trusted-head legacy directory traverses a symlink");
  return (await readdir(directory, { withFileTypes: true }))
    .flatMap((entry) => {
      if (entry.isFile() && STAGED_RECORD_FILE.test(entry.name)) return [];
      if (!entry.isFile() || !RECORD_FILE.test(entry.name))
        throw new Error("trusted-head legacy directory has an unknown entry");
      return [entry.name];
    })
    .sort();
};
/** Strict read-only full audit; no excluded/torn entry or implicit repair. */
export const auditLegacyAuthority = async (
  input: Readonly<{
    directory: string;
    records: AuthorityRecordCodec;
    liveRecordLimit: number;
  }>,
) =>
  auditLegacyAuthorityRecords({
    ...input,
    recordNames: await readLegacyAuthorityRecordNames(input.directory),
  });
