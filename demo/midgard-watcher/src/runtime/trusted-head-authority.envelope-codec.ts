import { createHmac, timingSafeEqual } from "node:crypto";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  exactRecord,
  parseJson,
  recordKeyId,
  sameCanonical,
} from "./trusted-head-authority.exact-record.js";
import type { AuthorityRecordCodec } from "./trusted-head-authority.record-codec.js";

export const AUTHORITY_ENVELOPE_MAX_BYTES = 32_768;
export const AUTHORITY_MAX_LIVE_RECORDS = 4_096;
export const AUTHORITY_SELECTOR_FILE = "authority-backend.json";
export const AUTHORITY_DATABASE_FILE = "authority.sqlite";
const GENERATION =
  /^generation-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/u;
export type AuthorityEnvelopeKind =
  | "current"
  | "checkpoint"
  | "initialization"
  | "selector";

export const assertAuthorityGeometry = (
  generation: string,
  liveRecordLimit: number,
): void => {
  if (!GENERATION.test(generation))
    throw new Error("trusted-head authority generation is invalid");
  if (
    !Number.isSafeInteger(liveRecordLimit) ||
    liveRecordLimit < 1 ||
    liveRecordLimit > AUTHORITY_MAX_LIVE_RECORDS
  )
    throw new Error(
      "trusted-head authority liveRecordLimit must be explicit and supported",
    );
};

/** All envelopes bind one independent identity, immutable generation and explicit K. */
export const authorityEnvelopeCodec = (
  input: Readonly<{
    records: AuthorityRecordCodec;
    generation: string;
    liveRecordLimit: number;
  }>,
) => {
  const { records, generation, liveRecordLimit } = input;
  assertAuthorityGeometry(generation, liveRecordLimit);
  const schema = (kind: AuthorityEnvelopeKind) =>
    `midgard-watcher-authority-${kind}-v1`;
  const content = (
    kind: AuthorityEnvelopeKind,
    payload: Readonly<Record<string, unknown>>,
  ) => ({
    schemaVersion: schema(kind),
    policyDigest: records.policy.policyDigest,
    deploymentMarker: records.policy.deploymentMarker,
    recordAuthenticationKeyId: recordKeyId(records.recordAuthenticationKey),
    generation,
    liveRecordLimit,
    payload,
  });
  const mac = (kind: AuthorityEnvelopeKind, value: unknown) =>
    createHmac("sha256", records.recordAuthenticationKey)
      .update(`${schema(kind)}:${watcherCanonicalJson(value)}`, "utf8")
      .digest("hex");
  const encode = (
    kind: AuthorityEnvelopeKind,
    payload: Readonly<Record<string, unknown>>,
  ): Uint8Array => {
    const body = content(kind, payload);
    const bytes = Buffer.from(
      watcherCanonicalJson({ ...body, envelopeMac: mac(kind, body) }),
      "utf8",
    );
    if (bytes.byteLength > AUTHORITY_ENVELOPE_MAX_BYTES)
      throw new Error("trusted-head authority envelope exceeds byte bound");
    return bytes;
  };
  const decode = (
    kind: AuthorityEnvelopeKind,
    bytes: Uint8Array,
    payloadKeys: readonly string[],
  ): Readonly<Record<string, unknown>> => {
    if (
      bytes.byteLength === 0 ||
      bytes.byteLength > AUTHORITY_ENVELOPE_MAX_BYTES
    )
      throw new Error("trusted-head authority envelope size is invalid");
    const value = exactRecord(parseJson(bytes), [
      "schemaVersion",
      "policyDigest",
      "deploymentMarker",
      "recordAuthenticationKeyId",
      "generation",
      "liveRecordLimit",
      "payload",
      "envelopeMac",
    ]);
    if (
      value === null ||
      value.schemaVersion !== schema(kind) ||
      value.policyDigest !== records.policy.policyDigest ||
      !sameCanonical(value.deploymentMarker, records.policy.deploymentMarker) ||
      value.recordAuthenticationKeyId !==
        recordKeyId(records.recordAuthenticationKey) ||
      value.generation !== generation ||
      value.liveRecordLimit !== liveRecordLimit ||
      typeof value.envelopeMac !== "string" ||
      !/^[0-9a-f]{64}$/u.test(value.envelopeMac)
    )
      throw new Error("trusted-head authority envelope identity is invalid");
    const payload = exactRecord(value.payload, payloadKeys);
    if (payload === null)
      throw new Error("trusted-head authority envelope payload is invalid");
    if (
      watcherCanonicalJson(value) !==
        new TextDecoder("utf-8", { fatal: true }).decode(bytes) ||
      !timingSafeEqual(
        Buffer.from(value.envelopeMac, "hex"),
        Buffer.from(mac(kind, content(kind, payload)), "hex"),
      )
    )
      throw new Error(
        "trusted-head authority envelope authentication is invalid",
      );
    return Object.freeze(payload);
  };
  return Object.freeze({ encode, decode, generation, liveRecordLimit });
};
export type AuthorityEnvelopeCodec = ReturnType<typeof authorityEnvelopeCodec>;
