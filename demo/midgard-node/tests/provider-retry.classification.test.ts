import { OgmiosSlotEvidenceUnavailableError } from "@al-ft/midgard-core/ogmios-slot";
import { SqlError } from "@effect/sql";
import { Cause, Runtime } from "effect";
import { describe, expect, it } from "vitest";

import {
  isConnectionClassError,
  isRetryableProviderError,
} from "../src/provider-retry.js";

// A neutral outer message, so a row passes only through its structured
// shape and never through the message fallback.
const wrapped = (cause: unknown): Error =>
  new Error("provider read", { cause });

const withCode = (code: string, message = "x"): Error =>
  Object.assign(new Error(message), { code });

const kupmiosError = (kind: string, retryable: boolean): Error =>
  Object.assign(new Error("kupo getUtxos failed"), {
    name: "KupmiosError",
    _tag: "KupmiosError",
    kind,
    retryable,
  });

// What an `Effect.runPromise` around a provider read rejects with: the
// lc1 reference-script step died on exactly this shape.
const fiberFailure = (error: Error): Error =>
  Runtime.makeFiberFailure(Cause.fail(error));

describe("isRetryableProviderError classifies by structure", () => {
  const transient: ReadonlyArray<readonly [string, unknown]> = [
    ...[
      "ECONNREFUSED",
      "ECONNRESET",
      "ECONNABORTED",
      "ETIMEDOUT",
      "ENOTFOUND",
      "EAI_AGAIN",
      "EPIPE",
      "ENETUNREACH",
      "ENETDOWN",
      "EHOSTUNREACH",
      "EHOSTDOWN",
    ].map((code) => [`node ${code}`, wrapped(withCode(code))] as const),
    ...[
      "UND_ERR_CONNECT_TIMEOUT",
      "UND_ERR_HEADERS_TIMEOUT",
      "UND_ERR_BODY_TIMEOUT",
      "UND_ERR_SOCKET",
      "UND_ERR_CLOSED",
    ].map((code) => [`undici ${code}`, wrapped(withCode(code))] as const),
    ["undici terminated", wrapped(new TypeError("terminated"))],
    ["undici other side closed", wrapped(new Error("other side closed"))],
    ["undici connect timeout", wrapped(new Error("connect timeout"))],
    [
      "AbortSignal.timeout",
      wrapped(Object.assign(new Error("x"), { name: "TimeoutError" })),
    ],
    [
      "dual-stack refusal",
      wrapped(
        new AggregateError([
          withCode("ECONNREFUSED"),
          withCode("ECONNREFUSED"),
        ]),
      ),
    ],
    [
      "postgres starting up",
      wrapped(withCode("57P03", "the database system is starting up")),
    ],
    [
      "postgres in recovery",
      wrapped(withCode("57P03", "the database system is in recovery mode")),
    ],
    [
      "postgres too many clients",
      wrapped(withCode("53300", "sorry, too many clients already")),
    ],
    [
      "postgres message without a code",
      wrapped(new Error("FATAL: the database system is starting up")),
    ],
    ...["57P01", "57P02", "08000", "08001", "08003", "08004", "08006"].map(
      (code) => [`SQLSTATE ${code}`, wrapped(withCode(code))] as const,
    ),
    ...[
      "CONNECTION_CLOSED",
      "CONNECTION_DESTROYED",
      "CONNECTION_ENDED",
      "CONNECT_TIMEOUT",
    ].map((code) => [`postgres.js ${code}`, wrapped(withCode(code))] as const),
    [
      "declared retryable (KupmiosError shape)",
      wrapped(
        Object.assign(new Error("kupo getUtxos failed"), {
          kind: "transport",
          retryable: true,
        }),
      ),
    ],
    [
      "a FiberFailure around a transport KupmiosError",
      wrapped(fiberFailure(kupmiosError("transport", true))),
    ],
    [
      "a FiberFailure defect that refused a connection",
      wrapped(
        Runtime.makeFiberFailure(Cause.die(wrapped(withCode("ECONNRESET")))),
      ),
    ],
    [
      "Ogmios tip stale",
      wrapped(
        new OgmiosSlotEvidenceUnavailableError(
          "ogmios_tip_stale",
          "Ogmios lastTipUpdate is stale: ageMs=1,maxAgeMs=0",
        ),
      ),
    ],
  ];
  it.each(transient)("retries %s", (_name, error) => {
    expect(isRetryableProviderError(error)).toBe(true);
  });

  const terminal: ReadonlyArray<readonly [string, unknown]> = [
    ["malformed datum", wrapped(new Error("Failed to decode datum: bad CBOR"))],
    [
      "wrong network",
      wrapped(
        new Error(
          "Ogmios network magic does not match configured Cardano network authority",
        ),
      ),
    ],
    [
      "malformed Ogmios health",
      wrapped(new Error("Ogmios health response is missing connectionStatus")),
    ],
    [
      "declared non-retryable decode failure",
      wrapped(
        Object.assign(new Error("kupo getUtxos failed"), {
          kind: "decode",
          retryable: false,
        }),
      ),
    ],
    [
      "a FiberFailure around a decode KupmiosError",
      wrapped(fiberFailure(kupmiosError("decode", false))),
    ],
    [
      "a self-declared non-retryable error whose text names a timeout",
      wrapped(
        Object.assign(
          new Error("request_timeout_ms=5000 does not match manifest 10000"),
          { retryable: false },
        ),
      ),
    ],
    [
      "postgres unique violation",
      wrapped(
        withCode("23505", "duplicate key value violates unique constraint"),
      ),
    ],
    ["postgres undefined table", wrapped(withCode("42P01", "no such table"))],
    ["node argument error", wrapped(withCode("ERR_INVALID_ARG_TYPE"))],
    [
      "deterministic witness count",
      new Error("Expected at most one hub-oracle witness UTxO"),
    ],
  ];
  it.each(terminal)("refuses %s", (_name, error) => {
    expect(isRetryableProviderError(error)).toBe(false);
  });
});

describe("isConnectionClassError", () => {
  it.each([
    ["refused", wrapped(withCode("ECONNREFUSED"))],
    ["starting up", wrapped(withCode("57P03"))],
    ["too many clients", wrapped(withCode("53300"))],
    ["closed", wrapped(withCode("CONNECTION_CLOSED"))],
    [
      "recovery message",
      wrapped(new Error("the database system is in recovery mode")),
    ],
    [
      "the @effect/sql-pg pool-open timeout",
      wrapped(
        new SqlError.SqlError({
          cause: new Error("Connection timed out"),
          message: "PgClient: Connection timed out",
        }),
      ),
    ],
  ])("treats %s as a connection failure", (_name, error) => {
    expect(isConnectionClassError(error)).toBe(true);
  });

  it.each([
    ["unique violation", wrapped(withCode("23505"))],
    ["undefined column", wrapped(withCode("42703"))],
    ["a provider-only undici code", wrapped(withCode("UND_ERR_SOCKET"))],
    [
      "a provider that declares itself retryable",
      wrapped(Object.assign(new Error("x"), { retryable: true })),
    ],
    ["a timeout word", wrapped(new Error("statement timeout"))],
    [
      "a pool open the server refused",
      wrapped(
        new SqlError.SqlError({
          cause: withCode("28P01", "password authentication failed"),
          message: "PgClient: Failed to connect",
        }),
      ),
    ],
  ])("does not treat %s as a connection failure", (_name, error) => {
    expect(isConnectionClassError(error)).toBe(false);
  });
});
