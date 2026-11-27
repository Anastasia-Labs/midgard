import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";

import type { RetainedDaFetchAttemptStatus } from "./fetch.admit-retained-da-provenance.js";

export class InvalidRetainedDaResponseError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "InvalidRetainedDaResponseError";
  }
}

export class PeerRejectedDaRequestError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "PeerRejectedDaRequestError";
  }
}

export class PeerConflictDaResponseError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "PeerConflictDaResponseError";
  }
}

export const assertHash = (
  bytes: Buffer,
  expectedHash: Buffer,
  message: string,
): void => {
  if (!computeDaSha256Hash(bytes).equals(expectedHash)) {
    throw new InvalidRetainedDaResponseError(message);
  }
};

export const statusFromError = (
  error: unknown,
): RetainedDaFetchAttemptStatus => {
  if (error instanceof PeerRejectedDaRequestError) {
    return "rejected";
  }
  if (error instanceof PeerConflictDaResponseError) {
    return "conflict";
  }
  if (error instanceof InvalidRetainedDaResponseError) {
    return "invalid_content";
  }
  if (error instanceof Error && error.name === "TimeoutError") {
    return "timeout";
  }
  return "transport_error";
};
