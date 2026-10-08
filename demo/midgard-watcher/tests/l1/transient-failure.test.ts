import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
} from "@al-ft/midgard-fault-proofs";
import { KupmiosError } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { NativeChainSyncStartupFailure } from "../../src/l1/native-chain-sync.exact-record.js";
import { isWatcherL1TransientFailure } from "../../src/l1/transient-failure.js";

const fetchFailed = (code: string) =>
  Object.assign(new TypeError("fetch failed"), {
    cause: Object.assign(new Error(`connect ${code}`), { code }),
  });

describe("watcher L1 transient failure", () => {
  it.each([
    ["an L1 source that did not answer", new FraudProofL1UnavailableError("x")],
    [
      "a chain that moved under a snapshot",
      new FraudProofL1CheckpointChangedError("x"),
    ],
    ["a refused connection", fetchFailed("ECONNREFUSED")],
    ["a reset connection", fetchFailed("ECONNRESET")],
    ["a connect timeout", fetchFailed("UND_ERR_CONNECT_TIMEOUT")],
    [
      "a retryable provider timeout",
      new KupmiosError({ protocol: "kupo", operation: "x", kind: "timeout" }),
    ],
    [
      "a provider HTTP 503",
      new KupmiosError({ protocol: "kupo", operation: "x", status: 503 }),
    ],
    [
      "a native node that did not answer",
      new NativeChainSyncStartupFailure("node_handshake_failed"),
    ],
    [
      "a node transport whose sidecar is restarting",
      new NativeChainSyncStartupFailure("sidecar_restarting"),
    ],
    [
      "a node that dropped its connection",
      new NativeChainSyncStartupFailure("node_connection_lost"),
    ],
    [
      "a transient wrapped in a workflow error",
      new Error("workflow readiness failed", {
        cause: new Error("read failed", {
          cause: new FraudProofL1UnavailableError("x"),
        }),
      }),
    ],
  ])("treats %s as transient", (_label, error) => {
    expect(isWatcherL1TransientFailure(error)).toBe(true);
  });

  it.each([
    ["a plain error", new Error("Transition replay event coverage changed")],
    [
      "an error that only carries a transient name",
      Object.assign(new Error("forged"), {
        name: "FraudProofL1UnavailableError",
      }),
    ],
    [
      "a provider decode failure on HTTP 200",
      new KupmiosError({ protocol: "kupo", operation: "x", status: 200 }),
    ],
    [
      "a forged retryable provider shape without its tag",
      Object.assign(new Error("forged"), {
        name: "KupmiosError",
        provider: "Kupmios",
        retryable: true,
      }),
    ],
    ["an HTTP refusal", fetchFailed("EACCES")],
    [
      "a native intersection the node refused",
      new NativeChainSyncStartupFailure("intersection_failed"),
    ],
    [
      "a transport that was closed",
      new NativeChainSyncStartupFailure("stopped"),
    ],
    ["a non-error", "ECONNREFUSED"],
    [
      "a transient buried past the cause depth bound",
      Array.from({ length: 8 }).reduce<Error>(
        (cause) => new Error("wrapped", { cause }),
        new FraudProofL1UnavailableError("x"),
      ),
    ],
  ])("keeps %s hard", (_label, error) => {
    expect(isWatcherL1TransientFailure(error)).toBe(false);
  });
});
