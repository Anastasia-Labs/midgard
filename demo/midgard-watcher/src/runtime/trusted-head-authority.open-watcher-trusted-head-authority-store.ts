import { timingSafeEqual } from "node:crypto";
import {
  type FileHandle,
  mkdir,
  open,
  readdir,
  realpath,
} from "node:fs/promises";
import { join } from "node:path";
import { setImmediate as yieldScan } from "node:timers/promises";

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
  syncDirectory,
  type TrustedHeadAuthorityRecord,
  TrustedHeadCallerError,
  WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION,
  type WatcherTrustedHeadAuthorityStore,
} from "./trusted-head-authority.exact-record.js";

/**
 * Opens the operationally independent append-only freshness store. Every
 * startup replays the complete directory and rejects gaps, substitutions,
 * malformed/HMAC-invalid records, and non-canonical bytes.
 */
export const openWatcherTrustedHeadAuthorityStore = async (input: {
  readonly directory: string;
  readonly policy: WatcherFinalityPolicy;
  /** Independently authenticates the append-only sidecar record chain. */
  readonly recordAuthenticationKey: Uint8Array;
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

  const admittedRecords = new Map<
    string,
    Readonly<{
      bytes: Uint8Array;
      record: TrustedHeadAuthorityRecord;
      recordSha256: string;
    }>
  >();
  const scan = async (): Promise<Readonly<{
    head: WatcherRollbackDurableTrustedHead;
    recordSha256: string;
  }> | null> => {
    const entries = await readdir(directory, { withFileTypes: true });
    const names = entries.map((entry) => {
      if (!entry.isFile() || !RECORD_FILE.test(entry.name)) {
        throw new Error(
          "trusted-head authority directory has an unknown entry",
        );
      }
      return entry.name;
    });
    names.sort();
    const retainedNames = new Set(names.slice(-MAX_CACHED_RECORDS));
    for (const name of admittedRecords.keys()) {
      if (!retainedNames.has(name)) admittedRecords.delete(name);
    }
    let previous: Readonly<{
      head: WatcherRollbackDurableTrustedHead;
      recordSha256: string;
    }> | null = null;
    for (
      let offset = 0;
      offset < names.length;
      offset += RECORD_SCAN_BATCH_SIZE
    ) {
      const batch = names.slice(offset, offset + RECORD_SCAN_BATCH_SIZE);
      // This independent service owns tiny local records. Synchronous reads
      // avoid thread-pool round trips per file; yield between bounded batches
      // so another request can run. Every scan still reads every record.
      if (offset !== 0) await yieldScan();
      for (let index = 0; index < batch.length; index += 1) {
        const name = batch[index]!;
        const expectedRevision = BigInt(offset + index);
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
    return previous;
  };

  await scan();

  return Object.freeze({
    readRecordAuthenticationKeyId: async () =>
      recordKeyId(recordAuthenticationKey),
    readCurrent: async () => (await scan())?.head ?? null,
    compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
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

      const path = join(directory, recordName(nextRevision));
      let handle: FileHandle | undefined;
      try {
        handle = await open(path, "wx", 0o600);
        await handle.writeFile(watcherCanonicalJson(sidecarRecord), {
          encoding: "utf8",
        });
        await handle.sync();
      } catch (error) {
        if ((error as NodeJS.ErrnoException).code === "EEXIST") return false;
        throw error;
      } finally {
        await handle?.close();
      }
      await syncDirectory(directory);
      return sameHead((await scan())?.head ?? null, next);
    },
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
