import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";

import {
  makeWatcherDurablePayload,
  type WatcherDaProofInput,
} from "./durable-store.js";
import {
  invalidContent,
  LOWER_HEX_32,
  MAX_EVENT_KEY_BYTES,
  WatcherPublicDaClientError,
} from "./public-da-client.strict-inner-payload.js";

export const exactHex = (value: string, pattern: RegExp): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new WatcherPublicDaClientError("invalid_request");
  }
  return value;
};

export const uniqueHashes = (values: readonly string[]): readonly string[] => {
  if (!Array.isArray(values) || values.length === 0 || values.length > 64) {
    throw new WatcherPublicDaClientError("invalid_request");
  }
  const normalized = values.map((value) => exactHex(value, LOWER_HEX_32));
  if (new Set(normalized).size !== normalized.length) {
    throw new WatcherPublicDaClientError("invalid_request");
  }
  return Object.freeze(normalized);
};

export const exactNatural = (value: number): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new WatcherPublicDaClientError("invalid_request");
  }
  return value;
};

export const exactEventKey = (value: string | Uint8Array): Buffer => {
  let bytes: Buffer;
  if (typeof value === "string") {
    if (!/^(?:[0-9a-f]{2})+$/u.test(value)) {
      throw new WatcherPublicDaClientError("invalid_request");
    }
    bytes = Buffer.from(value, "hex");
  } else if (value instanceof Uint8Array) {
    bytes = Buffer.from(value);
  } else {
    throw new WatcherPublicDaClientError("invalid_request");
  }
  if (bytes.length === 0 || bytes.length > MAX_EVENT_KEY_BYTES) {
    throw new WatcherPublicDaClientError("invalid_request");
  }
  return bytes;
};

export const assertEqualBytes = (
  actual: Uint8Array,
  expected: Uint8Array,
  protocol: DaRequestResponseProtocol,
): void => {
  if (!Buffer.from(actual).equals(expected)) {
    invalidContent(protocol);
  }
};

export const boundedBytes = (
  value: Uint8Array | null,
  maximum: number,
  protocol: DaRequestResponseProtocol,
): Buffer => {
  if (value === null) {
    invalidContent(protocol);
  }
  const bytes = Buffer.from(value);
  if (bytes.length === 0 || bytes.length > maximum) {
    invalidContent(protocol);
  }
  return bytes;
};

export const durableInput = (
  kind: WatcherDaProofInput["kind"],
  inputId: string,
  bytes: Buffer,
): WatcherDaProofInput =>
  Object.freeze({
    inputId,
    kind,
    payload: makeWatcherDurablePayload(bytes.toString("hex")),
  });
