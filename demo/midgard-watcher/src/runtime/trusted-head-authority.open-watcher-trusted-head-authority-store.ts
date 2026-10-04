import { timingSafeEqual } from "node:crypto";
import { readFileSync } from "node:fs";
import { mkdir, readdir, realpath, unlink } from "node:fs/promises";
import { join } from "node:path";
import { setImmediate as yieldScan } from "node:timers/promises";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";

import {
  parseWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../l1/finality-engine.js";
import {
  WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
  type WatcherRollbackDurableTrustedHead,
} from "../l1/rollback-engine.js";
import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  isTornJsonRecord,
  publishExclusiveFile,
  removeStagedRecordFiles,
  STAGED_RECORD_FILE,
  stagedRecordPath,
  syncDirectory,
} from "../storage/exclusive-record-file.js";
import { WATCHER_PACKAGE_NAME } from "./scaffold.js";
import {
  canonicalDirectory,
  exactRecord,
  LOOPBACK_HOSTS,
  makeAuthorityRecord,
  MAX_CACHED_RECORDS,
  parseJson,
  readBounded,
  RECORD_FILE,
  RECORD_SCAN_BATCH_SIZE,
  recordKeyId,
  recordMac,
  recordName,
  revision,
  sameCanonical,
  sameHead,
  sha256,
  type TrustedHeadAuthorityRecord,
  TrustedHeadCallerError,
  WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION,
  type WatcherTrustedHeadAuthorityStore,
} from "./trusted-head-authority.exact-record.js";
import {
  compactTrustedHeadRecords,
  readRetentionFloor,
  RETENTION_FLOOR_FILE,
} from "./trusted-head-authority.retention-floor.js";

/**
 * Opens the operationally independent monotonic freshness store. Every
 * startup verifies its authenticated retention floor and the complete retained
 * suffix, and rejects gaps, substitutions, malformed/HMAC-invalid records, and
 * non-canonical bytes. Opening first removes staging files and one torn final
 * record that a crashed writer left (see `scan`); every later read stays strict.
 */
export const openWatcherTrustedHeadAuthorityStore = async (input: {
  readonly directory: string;
  readonly policy: WatcherFinalityPolicy;
  /** Independently authenticates the append-only sidecar record chain. */
  readonly recordAuthenticationKey: Uint8Array;
  /** May lower, never widen, the operational suffix bound. Revisions are not L1 blocks. */
  readonly maxRetainedRecords?: number;
}): Promise<WatcherTrustedHeadAuthorityStore> => {
  const directory = canonicalDirectory(input.directory);
  const policy = parseWatcherFinalityPolicy(input.policy);
  if (policy === null) {
    throw new Error("trusted-head authority finality policy is invalid");
  }
  const recordAuthenticationKey = Uint8Array.from(
    input.recordAuthenticationKey,
  );
  if (recordAuthenticationKey.byteLength !== 32) {
    throw new Error("trusted-head authority authentication key is invalid");
  }
  const maxRetainedRecords = input.maxRetainedRecords ?? MAX_CACHED_RECORDS;
  if (
    !Number.isSafeInteger(maxRetainedRecords) ||
    maxRetainedRecords < 2 ||
    maxRetainedRecords > MAX_CACHED_RECORDS
  )
    throw new Error("trusted-head authority retention bound is invalid");
  await mkdir(directory, { recursive: true, mode: 0o700 });
  if ((await realpath(directory)) !== directory) {
    throw new Error("trusted-head authority directory traverses a symlink");
  }

  const admitHead = (
    value: unknown,
    callerAuthored = false,
  ): WatcherRollbackDurableTrustedHead => {
    const head = exactRecord(value, [
      "schemaVersion",
      "policyDigest",
      "deploymentMarker",
      "authenticationKeyId",
      "revision",
      "snapshotSha256",
      "authorityDigest",
      "headMac",
    ]);
    if (
      head === null ||
      head.schemaVersion !==
        WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION ||
      head.policyDigest !== policy.policyDigest ||
      !sameCanonical(head.deploymentMarker, policy.deploymentMarker) ||
      typeof head.authenticationKeyId !== "string" ||
      !/^[0-9a-f]{64}$/u.test(head.authenticationKeyId) ||
      typeof head.revision !== "string" ||
      !/^(?:0|[1-9][0-9]*)$/u.test(head.revision) ||
      typeof head.snapshotSha256 !== "string" ||
      !/^[0-9a-f]{64}$/u.test(head.snapshotSha256) ||
      typeof head.authorityDigest !== "string" ||
      !/^[0-9a-f]{64}$/u.test(head.authorityDigest) ||
      typeof head.headMac !== "string" ||
      !/^[0-9a-f]{64}$/u.test(head.headMac)
    ) {
      const ErrorType = callerAuthored ? TrustedHeadCallerError : Error;
      throw new ErrorType("trusted-head authority record structure failed");
    }
    const admitted = Object.freeze({
      schemaVersion: WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
      policyDigest: head.policyDigest,
      deploymentMarker: policy.deploymentMarker,
      authenticationKeyId: head.authenticationKeyId,
      revision: head.revision,
      snapshotSha256: head.snapshotSha256,
      authorityDigest: head.authorityDigest,
      headMac: head.headMac,
    }) as WatcherRollbackDurableTrustedHead;
    revision(admitted);
    return admitted;
  };

  const admitRecord = (value: unknown): TrustedHeadAuthorityRecord => {
    const record = exactRecord(value, [
      "schemaVersion",
      "revision",
      "priorRecordSha256",
      "head",
      "recordAuthenticationKeyId",
      "recordMac",
    ]);
    if (
      record === null ||
      record.schemaVersion !==
        WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION ||
      typeof record.revision !== "string" ||
      !/^(?:0|[1-9][0-9]*)$/u.test(record.revision) ||
      (record.priorRecordSha256 !== null &&
        (typeof record.priorRecordSha256 !== "string" ||
          !/^[0-9a-f]{64}$/u.test(record.priorRecordSha256))) ||
      record.recordAuthenticationKeyId !==
        recordKeyId(recordAuthenticationKey) ||
      typeof record.recordMac !== "string" ||
      !/^[0-9a-f]{64}$/u.test(record.recordMac)
    ) {
      throw new Error("trusted-head authority sidecar record is invalid");
    }
    const head = admitHead(record.head);
    if (head.revision !== record.revision) {
      throw new Error(
        "trusted-head authority record revision differs from head",
      );
    }
    const content = Object.freeze({
      schemaVersion: WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION,
      revision: record.revision,
      priorRecordSha256: record.priorRecordSha256 as string | null,
      head,
      recordAuthenticationKeyId: record.recordAuthenticationKeyId as string,
    });
    const expectedMac = recordMac(recordAuthenticationKey, content);
    if (
      !timingSafeEqual(
        Buffer.from(expectedMac, "hex"),
        Buffer.from(record.recordMac, "hex"),
      )
    ) {
      throw new Error("trusted-head authority sidecar record MAC is invalid");
    }
    return Object.freeze({
      ...content,
      recordMac: record.recordMac,
    }) as TrustedHeadAuthorityRecord;
  };

  /** The record names of the last scan's chain, oldest first. */
  let retainedSuffix: readonly string[] = [];
  /** Record files the last scan found below its floor. */
  let belowFloor: readonly string[] = [];
  const admittedRecords = new Map<
    string,
    Readonly<{
      bytes: Uint8Array;
      record: TrustedHeadAuthorityRecord;
      recordSha256: string;
    }>
  >();
  /**
   * Replays the chain. While opening (`dropTornFinal`), a final record that is
   * empty or not UTF-8 JSON is removed after every earlier record is admitted.
   * Publication now stages every record, so only the earlier writer, which
   * created the revision name before writing and fsyncing it, leaves one when
   * it is killed. That compare-and-swap never returned true. The watcher
   * commits its SQLite snapshot before it asks for the swap and, on restart,
   * republishes that snapshot as the one direct successor of the head found
   * here (`prepareWatcherRollbackDurableTrustedHeadReconciliation`), so
   * falling back one revision loses nothing; that reconciliation refuses any
   * wider rollback. Deleting the final record was already undetectable here.
   * A torn record that is not final, or met after opening, fails closed.
   */
  const scan = async (
    dropTornFinal = false,
  ): Promise<Readonly<{
    head: WatcherRollbackDurableTrustedHead;
    recordSha256: string;
  }> | null> => {
    const entries = await readdir(directory, { withFileTypes: true });
    const names = entries.flatMap((entry) => {
      // A compare-and-swap or floor publication in flight stages its bytes here.
      if (entry.isFile() && STAGED_RECORD_FILE.test(entry.name)) return [];
      if (
        !entry.isFile() ||
        (!RECORD_FILE.test(entry.name) && entry.name !== RETENTION_FLOOR_FILE)
      ) {
        throw new Error(
          "trusted-head authority directory has an unknown entry",
        );
      }
      return RECORD_FILE.test(entry.name) ? [entry.name] : [];
    });
    const floor = entries.some(({ name }) => name === RETENTION_FLOOR_FILE)
      ? readRetentionFloor(directory, recordAuthenticationKey, admitRecord)
      : null;
    const firstRevision = floor === null ? 0n : revision(floor.head) + 1n;
    names.sort();
    // Files below the floor are a prefix whose cleanup was interrupted.
    const suffix = names.filter((name) => name >= recordName(firstRevision));
    if (floor !== null && suffix.length === 0)
      throw new Error("trusted-head authority retention floor has no suffix");
    const finalName = suffix.at(-1);
    const tornFinal =
      dropTornFinal &&
      finalName !== undefined &&
      finalName === recordName(firstRevision + BigInt(suffix.length - 1)) &&
      isTornJsonRecord(readFileSync(join(directory, finalName)))
        ? suffix.pop()!
        : null;
    retainedSuffix = suffix;
    belowFloor = names.filter((name) => name < recordName(firstRevision));
    const retainedNames = new Set(suffix.slice(-MAX_CACHED_RECORDS));
    for (const name of admittedRecords.keys()) {
      if (!retainedNames.has(name)) admittedRecords.delete(name);
    }
    let previous: Readonly<{
      head: WatcherRollbackDurableTrustedHead;
      recordSha256: string;
    }> | null = floor;
    for (
      let offset = 0;
      offset < suffix.length;
      offset += RECORD_SCAN_BATCH_SIZE
    ) {
      const batch = suffix.slice(offset, offset + RECORD_SCAN_BATCH_SIZE);
      // This independent service owns tiny local records. Synchronous reads
      // avoid thread-pool round trips per file; yield between bounded batches
      // so another request can run. Every scan still reads every record.
      if (offset !== 0) await yieldScan();
      for (let index = 0; index < batch.length; index += 1) {
        const name = batch[index]!;
        const expectedRevision = firstRevision + BigInt(offset + index);
        if (name !== recordName(expectedRevision)) {
          throw new Error("trusted-head authority revision chain has a gap");
        }
        const bytes = readBounded(join(directory, name));
        const cached = admittedRecords.get(name);
        const unchanged =
          cached !== undefined && Buffer.compare(bytes, cached.bytes) === 0;
        const sidecarRecord = unchanged
          ? cached.record
          : admitRecord(parseJson(bytes));
        const expectedPriorRecordSha256 = previous?.recordSha256 ?? null;
        if (
          revision(sidecarRecord.head) !== expectedRevision ||
          sidecarRecord.priorRecordSha256 !== expectedPriorRecordSha256 ||
          (!unchanged &&
            new TextDecoder().decode(bytes) !==
              watcherCanonicalJson(sidecarRecord))
        ) {
          throw new Error("trusted-head authority record is non-canonical");
        }
        if (
          previous !== null &&
          revision(sidecarRecord.head) !== revision(previous.head) + 1n
        ) {
          throw new Error(
            "trusted-head authority revision chain is discontinuous",
          );
        }
        const recordSha256 = unchanged ? cached.recordSha256 : sha256(bytes);
        if (!unchanged && retainedNames.has(name)) {
          admittedRecords.set(name, {
            bytes,
            record: sidecarRecord,
            recordSha256,
          });
        }
        previous = Object.freeze({
          head: sidecarRecord.head,
          recordSha256,
        });
      }
    }
    if (tornFinal !== null) {
      await unlink(join(directory, tornFinal));
      await syncDirectory(directory);
    }
    return previous;
  };

  // The independent service has one owner; serialize scans with append/compaction
  // so no reader observes a partially published record or changing floor.
  let operation: Promise<unknown> = Promise.resolve();
  const serialize = <T>(run: () => Promise<T>): Promise<T> => {
    const next = operation.then(run);
    operation = next.catch(() => undefined);
    return next;
  };

  await removeStagedRecordFiles(directory);
  await scan(true);

  return Object.freeze({
    readRecordAuthenticationKeyId: async () =>
      recordKeyId(recordAuthenticationKey),
    readCurrent: async () =>
      serialize(async () => (await scan())?.head ?? null),
    compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) =>
      serialize(async () => {
        const expected =
          expectedTrustedHead === null
            ? null
            : admitHead(expectedTrustedHead, true);
        const next = admitHead(nextTrustedHead, true);
        const nextRevision = revision(next);
        if (
          (expected === null && nextRevision !== 0n) ||
          (expected !== null && nextRevision !== revision(expected) + 1n)
        ) {
          return false;
        }
        const current = await scan();
        if (!sameHead(current?.head ?? null, expected)) return false;
        const sidecarRecord = makeAuthorityRecord({
          head: next,
          priorRecordSha256: current?.recordSha256 ?? null,
          recordAuthenticationKey,
        });

        try {
          await publishExclusiveFile({
            stagingPath: stagedRecordPath(directory),
            path: join(directory, recordName(nextRevision)),
            bytes: watcherCanonicalJson(sidecarRecord),
          });
        } catch (error) {
          if ((error as NodeJS.ErrnoException).code === "EEXIST") return false;
          throw error;
        }
        await syncDirectory(directory);
        if (!sameHead((await scan())?.head ?? null, next)) return false;
        try {
          await compactTrustedHeadRecords(
            directory,
            retainedSuffix,
            belowFloor,
            maxRetainedRecords,
            recordAuthenticationKey,
            admitRecord,
          );
        } catch (error) {
          // The swap is already durable and read back. Compaction only bounds
          // disk use, and the next swap's compaction retries it from the
          // authenticated floor, so its failure must not fail the swap.
          process.stderr.write(
            `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level: "warn", event: "trusted_head_compaction_failed", error: formatUnknownError(error) })}\n`,
          );
          return true;
        }
        return sameHead((await scan())?.head ?? null, next);
      }),
  });
};

export type WatcherTrustedHeadAuthorityClient = Readonly<{
  readRecordAuthenticationKeyId(): Promise<string>;
  readCurrent(): Promise<WatcherRollbackDurableTrustedHead | null>;
  compareAndSwap(input: {
    readonly expectedTrustedHead: WatcherRollbackDurableTrustedHead | null;
    readonly nextTrustedHead: WatcherRollbackDurableTrustedHead;
  }): Promise<boolean>;
}>;

export const endpointUrl = (value: unknown): URL => {
  let url: URL;
  try {
    url = new URL(String(value));
  } catch {
    throw new Error("trusted-head authority endpoint is invalid");
  }
  if (
    url.protocol !== "http:" ||
    !LOOPBACK_HOSTS.has(url.hostname.toLowerCase()) ||
    url.username !== "" ||
    url.password !== "" ||
    url.search !== "" ||
    url.hash !== "" ||
    (url.pathname !== "/" && url.pathname !== "")
  ) {
    throw new Error("trusted-head authority endpoint must be loopback HTTP");
  }
  return url;
};
