import { createHmac, timingSafeEqual } from "node:crypto";
import { lstat, readFile, realpath } from "node:fs/promises";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  exactRecord,
  parseJson,
  recordKeyId,
  sameCanonical,
} from "./trusted-head-authority.exact-record.js";
import type { AuthorityRecordCodec } from "./trusted-head-authority.record-codec.js";

export const REPAIR_RECEIPT_MAX_BYTES = 32768;
export const repairFileBytes = async (
  path: string,
  maximumBytes: number,
  allowEmpty = false,
): Promise<Uint8Array> => {
  const info = await lstat(path);
  if (
    !info.isFile() ||
    info.size > maximumBytes ||
    (!allowEmpty && info.size === 0) ||
    (await realpath(path)) !== path
  )
    throw new Error("trusted-head repair retained file identity/size invalid");
  const bytes = await readFile(path);
  if (
    bytes.byteLength > maximumBytes ||
    (!allowEmpty && bytes.byteLength === 0)
  )
    throw new Error("trusted-head repair retained bytes changed size");
  return bytes;
};
export const repairReceiptCodec = (
  records: AuthorityRecordCodec,
  attemptId: string,
  legacyDirectory: string,
  recoveryDirectory: string,
) => {
  if (
    !/^repair-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/u.test(
      attemptId,
    )
  )
    throw new Error("trusted-head repair attempt identity invalid");
  const schema = (kind: "intent" | "completion") =>
    `midgard-watcher-authority-legacy-repair-${kind}-v1`;
  const body = (
    kind: "intent" | "completion",
    payload: Readonly<Record<string, unknown>>,
  ) => ({
    schemaVersion: schema(kind),
    policyDigest: records.policy.policyDigest,
    deploymentMarker: records.policy.deploymentMarker,
    recordAuthenticationKeyId: recordKeyId(records.recordAuthenticationKey),
    attemptId,
    legacyDirectory,
    recoveryDirectory,
    payload,
  });
  const mac = (kind: "intent" | "completion", value: unknown) =>
    createHmac("sha256", records.recordAuthenticationKey)
      .update(`${schema(kind)}:${watcherCanonicalJson(value)}`)
      .digest("hex");
  const encode = (
    kind: "intent" | "completion",
    payload: Readonly<Record<string, unknown>>,
  ) => {
    const value = body(kind, payload),
      bytes = Buffer.from(
        watcherCanonicalJson({ ...value, receiptMac: mac(kind, value) }),
      );
    if (bytes.byteLength > REPAIR_RECEIPT_MAX_BYTES)
      throw new Error("trusted-head repair receipt exceeds bound");
    return bytes;
  };
  const decode = (
    kind: "intent" | "completion",
    bytes: Uint8Array,
    keys: readonly string[],
  ) => {
    if (bytes.byteLength === 0 || bytes.byteLength > REPAIR_RECEIPT_MAX_BYTES)
      throw new Error("trusted-head repair receipt size invalid");
    const value = exactRecord(parseJson(bytes), [
      "schemaVersion",
      "policyDigest",
      "deploymentMarker",
      "recordAuthenticationKeyId",
      "attemptId",
      "legacyDirectory",
      "recoveryDirectory",
      "payload",
      "receiptMac",
    ]);
    if (
      value === null ||
      value.schemaVersion !== schema(kind) ||
      value.policyDigest !== records.policy.policyDigest ||
      !sameCanonical(value.deploymentMarker, records.policy.deploymentMarker) ||
      value.recordAuthenticationKeyId !==
        recordKeyId(records.recordAuthenticationKey) ||
      value.attemptId !== attemptId ||
      value.legacyDirectory !== legacyDirectory ||
      value.recoveryDirectory !== recoveryDirectory ||
      typeof value.receiptMac !== "string" ||
      !/^[0-9a-f]{64}$/u.test(value.receiptMac)
    )
      throw new Error("trusted-head repair receipt identity invalid");
    const payload = exactRecord(value.payload, keys);
    if (
      payload === null ||
      watcherCanonicalJson(value) !==
        new TextDecoder("utf-8", { fatal: true }).decode(bytes) ||
      !timingSafeEqual(
        Buffer.from(value.receiptMac, "hex"),
        Buffer.from(mac(kind, body(kind, payload)), "hex"),
      )
    )
      throw new Error("trusted-head repair receipt authentication invalid");
    return payload;
  };
  return { encode, decode };
};
