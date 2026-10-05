import { timingSafeEqual } from "node:crypto";

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
  exactRecord,
  MAX_RECORD_BYTES,
  parseJson,
  recordKeyId,
  recordMac,
  revision,
  sameCanonical,
  sha256,
  type TrustedHeadAuthorityRecord,
  TrustedHeadCallerError,
  WATCHER_TRUSTED_HEAD_AUTHORITY_RECORD_SCHEMA_VERSION,
} from "./trusted-head-authority.exact-record.js";

export const authorityRecordCodec = (
  input: Readonly<{
    policy: WatcherFinalityPolicy;
    recordAuthenticationKey: Uint8Array;
  }>,
) => {
  const policy = parseWatcherFinalityPolicy(input.policy);
  if (policy === null)
    throw new Error("trusted-head authority finality policy is invalid");
  const recordAuthenticationKey = Uint8Array.from(
    input.recordAuthenticationKey,
  );
  if (recordAuthenticationKey.byteLength !== 32)
    throw new Error("trusted-head authority authentication key is invalid");
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

  const admitRecordBytes = (bytes: Uint8Array) => {
    if (bytes.byteLength === 0 || bytes.byteLength > MAX_RECORD_BYTES)
      throw new Error("trusted-head authority record size is invalid");
    const record = admitRecord(parseJson(bytes));
    if (
      new TextDecoder("utf-8", { fatal: true }).decode(bytes) !==
      watcherCanonicalJson(record)
    )
      throw new Error("trusted-head authority record is non-canonical");
    return Object.freeze({ record, recordSha256: sha256(bytes) });
  };
  return Object.freeze({
    policy,
    recordAuthenticationKey,
    admitHead,
    admitRecord,
    admitRecordBytes,
  });
};
export type AuthorityRecordCodec = ReturnType<typeof authorityRecordCodec>;
