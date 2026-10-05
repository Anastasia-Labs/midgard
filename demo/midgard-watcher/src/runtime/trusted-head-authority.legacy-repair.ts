import { lstat, mkdir, readdir, realpath, unlink } from "node:fs/promises";
import { dirname, join } from "node:path";

import type { WatcherFinalityPolicy } from "../l1/finality-engine.js";
import type { WatcherRollbackDurableTrustedHead } from "../l1/rollback-engine.js";
import {
  isTornJsonRecord,
  publishExclusiveFile,
  STAGED_RECORD_FILE,
  stagedRecordPath,
  syncDirectory,
} from "../storage/exclusive-record-file.js";
import {
  canonicalDirectory,
  MAX_RECORD_BYTES,
  RECORD_FILE,
  sameCanonical,
  sameHead,
  sha256,
} from "./trusted-head-authority.exact-record.js";
import { readLegacyAuthorityRecordNames } from "./trusted-head-authority.legacy-audit.js";
import { auditLegacyAuthorityRecords } from "./trusted-head-authority.legacy-prefix-audit.js";
import {
  REPAIR_RECEIPT_MAX_BYTES,
  repairFileBytes,
  repairReceiptCodec,
} from "./trusted-head-authority.legacy-repair-receipt.js";
import { authorityRecordCodec } from "./trusted-head-authority.record-codec.js";

export type WatcherLegacyAuthorityRepairInput = Readonly<{
  legacyDirectory: string;
  recoveryDirectory: string;
  policy: WatcherFinalityPolicy;
  recordAuthenticationKey: Uint8Array;
  /** Persisted offline repair identity, unchanged across acknowledgement loss. */
  attemptId: string;
  expectedPriorHead: WatcherRollbackDurableTrustedHead | null;
  expectedTornRecordName: string;
  expectedTornSha256: string;
  reason: string;
}>;

const isMissing = (error: unknown) =>
  (error as NodeJS.ErrnoException).code === "ENOENT";
const directoryIdentity = async (path: string) => {
  if (!(await lstat(path)).isDirectory() || (await realpath(path)) !== path)
    throw new Error("trusted-head repair directory identity invalid");
};

/** Explicit offline operation only. The caller must exclusively own all legacy
 * writers and prevent restart throughout validation/removal. No marker or scan
 * supplied by this helper is a live fleet fence. Never called by open/import. */
export const repairLegacyWatcherTrustedHeadAuthorityFinalRecord = async (
  input: WatcherLegacyAuthorityRepairInput,
): Promise<void> => {
  const legacyDirectory = canonicalDirectory(input.legacyDirectory),
    recoveryDirectory = canonicalDirectory(input.recoveryDirectory);
  if (
    legacyDirectory === recoveryDirectory ||
    legacyDirectory.startsWith(recoveryDirectory + "/") ||
    recoveryDirectory.startsWith(legacyDirectory + "/")
  )
    throw new Error(
      "trusted-head repair evidence must be in a separate directory",
    );
  if (
    !RECORD_FILE.test(input.expectedTornRecordName) ||
    !/^[0-9a-f]{64}$/u.test(input.expectedTornSha256) ||
    input.reason !== input.reason.trim() ||
    input.reason.length === 0 ||
    input.reason.length > 256
  )
    throw new Error(
      "trusted-head repair requires exact final identity and bounded reason",
    );
  const records = authorityRecordCodec(input),
    codec = repairReceiptCodec(
      records,
      input.attemptId,
      legacyDirectory,
      recoveryDirectory,
    );
  const expectedPriorHead =
    input.expectedPriorHead === null
      ? null
      : records.admitHead(input.expectedPriorHead);
  await directoryIdentity(legacyDirectory);
  await directoryIdentity(dirname(recoveryDirectory));
  const intentPath = join(recoveryDirectory, "intent.json"),
    removedPath = join(recoveryDirectory, "removed-record.bin"),
    completionPath = join(recoveryDirectory, "completed.json");
  const finalPath = join(legacyDirectory, input.expectedTornRecordName);
  const finalRevision = BigInt(input.expectedTornRecordName.slice(0, 20));
  const auditPrefix = async () => {
    const names = await readLegacyAuthorityRecordNames(legacyDirectory);
    const hasFinal = names.at(-1) === input.expectedTornRecordName;
    const prefix = hasFinal ? names.slice(0, -1) : names;
    if (
      BigInt(prefix.length) !== finalRevision ||
      names.some((name) => name > input.expectedTornRecordName)
    )
      throw new Error(
        "trusted-head repair final record is not the exact prefix successor",
      );
    const prior = await auditLegacyAuthorityRecords({
      directory: legacyDirectory,
      records,
      liveRecordLimit: 1,
      recordNames: prefix,
    });
    if (!sameHead(prior.head, expectedPriorHead))
      throw new Error(
        "trusted-head repair prior head differs from explicit expected head",
      );
    return { prior, hasFinal };
  };
  const first = await auditPrefix();
  let retainedIntent: Uint8Array | undefined;
  try {
    retainedIntent = await repairFileBytes(
      intentPath,
      REPAIR_RECEIPT_MAX_BYTES,
    );
  } catch (error) {
    if (!isMissing(error)) throw error;
  }
  if (!first.hasFinal && retainedIntent === undefined)
    throw new Error(
      "trusted-head repair missing final has no authenticated prior repair intent",
    );
  const torn = await repairFileBytes(
    first.hasFinal ? finalPath : removedPath,
    MAX_RECORD_BYTES,
    true,
  );
  if (sha256(torn) !== input.expectedTornSha256 || !isTornJsonRecord(torn))
    throw new Error("trusted-head repair final bytes differ or parse as JSON");
  const payload = {
    priorHead: expectedPriorHead,
    priorChainSha256: first.prior.sourceChainSha256,
    recordName: input.expectedTornRecordName,
    tornSha256: input.expectedTornSha256,
    tornBytesLength: torn.byteLength,
    reason: input.reason,
  };
  const intentBytes = codec.encode("intent", payload),
    intentSha256 = sha256(intentBytes);
  if (retainedIntent !== undefined) {
    const retained = codec.decode(
      "intent",
      retainedIntent,
      Object.keys(payload),
    );
    if (
      !sameCanonical(retained, payload) ||
      sha256(retainedIntent) !== intentSha256
    )
      throw new Error("trusted-head repair retained intent differs");
  }
  try {
    await mkdir(recoveryDirectory, { mode: 0o700 });
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "EEXIST") throw error;
  }
  await directoryIdentity(recoveryDirectory);
  // Matching mkdir state can follow a publication whose parent sync failed.
  await syncDirectory(dirname(recoveryDirectory));
  const entries = await readdir(recoveryDirectory, { withFileTypes: true });
  if (
    entries.some(
      (entry) =>
        !entry.isFile() ||
        (!["intent.json", "removed-record.bin", "completed.json"].includes(
          entry.name,
        ) &&
          !STAGED_RECORD_FILE.test(entry.name)),
    )
  )
    throw new Error("trusted-head repair evidence has an unknown entry");
  const retain = async (
    path: string,
    bytes: Uint8Array,
    maximumBytes: number,
    allowEmpty = false,
  ) => {
    try {
      await publishExclusiveFile({
        stagingPath: stagedRecordPath(recoveryDirectory),
        path,
        bytes,
      });
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== "EEXIST") throw error;
      const existing = await repairFileBytes(path, maximumBytes, allowEmpty);
      if (sha256(existing) !== sha256(bytes))
        throw new Error("trusted-head repair evidence collision");
    }
    // A prior process may have linked these bytes but died before directory
    // fsync. Reassert durability for matching EEXIST receipts before removal.
    await syncDirectory(recoveryDirectory);
  };
  await retain(removedPath, torn, MAX_RECORD_BYTES, true);
  await retain(intentPath, intentBytes, REPAIR_RECEIPT_MAX_BYTES);
  const completion = {
    intentSha256,
    priorHead: expectedPriorHead,
    priorChainSha256: first.prior.sourceChainSha256,
  };
  try {
    const bytes = await repairFileBytes(
      completionPath,
      REPAIR_RECEIPT_MAX_BYTES,
    );
    if (
      !sameCanonical(
        codec.decode("completion", bytes, Object.keys(completion)),
        completion,
      )
    )
      throw new Error("trusted-head repair completion differs");
    if (first.hasFinal)
      throw new Error(
        "trusted-head repair completed but final record reappeared",
      );
    // An existing completion link is not evidence its publication, or the
    // prior removal namespace, survived an interrupted directory sync.
    await syncDirectory(legacyDirectory);
    await syncDirectory(recoveryDirectory);
    return;
  } catch (error) {
    if (!isMissing(error)) throw error;
  }
  const rechecked = await auditPrefix();
  if (rechecked.prior.sourceChainSha256 !== first.prior.sourceChainSha256)
    throw new Error(
      "trusted-head repair verified prefix changed before removal",
    );
  if (rechecked.hasFinal) {
    if (
      sha256(await repairFileBytes(finalPath, MAX_RECORD_BYTES, true)) !==
      input.expectedTornSha256
    )
      throw new Error("trusted-head repair final changed before removal");
    await unlink(finalPath);
  }
  await syncDirectory(legacyDirectory);
  await retain(
    completionPath,
    codec.encode("completion", completion),
    REPAIR_RECEIPT_MAX_BYTES,
  );
};
