import {
  SidecarExitedError,
  TransportRequestError,
  TransportTimeoutError,
  TransportUnavailableError,
} from "@al-ft/l1-node-transport";
import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
} from "@al-ft/midgard-fault-proofs";
import { KupmiosError } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { isWatcherL1TransientFailure } from "../../src/l1/transient-failure.js";

const refusal = (code: string) =>
  new TransportRequestError(code, "the sidecar refused the request");

const sidecarExited = (fatal: { code: string; message: string } | null) =>
  new SidecarExitedError({ code: 1, signal: null, fatal, diagnostics: "" });

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
      "a node transport whose sidecar is restarting",
      new TransportUnavailableError("sidecar_restarting", "x"),
    ],
    [
      "a node transport request that missed its bound",
      new TransportTimeoutError("x"),
    ],
    ["a sidecar that exited", sidecarExited(null)],
    [
      "a sidecar that exited on a lost node connection",
      sidecarExited({ code: "node_connection_lost", message: "x" }),
    ],
    ["a busy sidecar session", refusal("busy")],
    ["a node that is not connected", refusal("node_unavailable")],
    ["a ledger state the node could not acquire", refusal("acquire_failed")],
    ["a query the node did not answer in time", refusal("node_timeout")],
    ["a query across an era boundary", refusal("era_mismatch")],
    [
      "a transport refusal wrapped in a read error",
      new Error("protocol parameters read failed", {
        cause: refusal("node_timeout"),
      }),
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
    ["a malformed request", refusal("invalid_request")],
    ["a query the sidecar does not know", refusal("unknown_query")],
    ["a point the ledger no longer holds", refusal("acquire_point_too_old")],
    ["an answer too large to carry", refusal("result_too_large")],
    [
      "an error that only carries a transport refusal's name",
      Object.assign(new Error("forged"), {
        name: "TransportRequestError",
        code: "busy",
      }),
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
