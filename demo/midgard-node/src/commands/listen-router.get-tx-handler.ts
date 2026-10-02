import "./listen-router.run-exact-gated-direct-l1-provider-probe.js";

import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { hexToBytes } from "@al-ft/midgard-core/hex";
import * as SDK from "@al-ft/midgard-sdk";
import {
  HttpIncomingMessage,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import type { HttpBodyError } from "@effect/platform/HttpBody";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { Effect, Metric, Option } from "effect";

import { ImmutableDB, MempoolDB } from "../database/index.js";
import { NodeConfig } from "../services/index.js";
import { failWith500 } from "./listen-response.js";
import { TX_ENDPOINT } from "./listen-router.l1-provider-readiness-evidence-is-fresh.js";
import { authorizeAdminRoute, isAdminRoutePath } from "./listen-utils.js";

export const txCounter = Metric.counter("tx_count", {
  description: "A counter for tracking submit transactions",
  bigint: true,
  incremental: true,
});

export const submitHandlerLatencyTimer = Metric.timer(
  "submit_handler_latency",
  "Latency of POST /submit handler responses in milliseconds",
);

export const submitBodyReadDurationTimer = Metric.timer(
  "submit_body_read_duration",
  "Duration of POST /submit request body reads in milliseconds",
);

export const submitNormalizeDurationTimer = Metric.timer(
  "submit_normalize_duration",
  "Duration of POST /submit canonical CBOR validation and normalization in milliseconds",
);

export const submitDurableAdmissionDurationTimer = Metric.timer(
  "submit_durable_admission_duration",
  "Duration of POST /submit durable admission writes in milliseconds",
);

export const submitResponseDurationTimer = Metric.timer(
  "submit_response_duration",
  "Duration of POST /submit response construction in milliseconds",
);

export const submitQueueOfferFailureCounter = Metric.counter(
  "submit_queue_offer_failure_count",
  {
    description:
      "Number of POST /submit requests rejected because the queue was full",
    bigint: true,
    incremental: true,
  },
);

export const V1_SUBMISSION_MEDIA_TYPE = "application/vnd.midgard.v1+cbor";

export const SUBMIT_HTTP_BODY_MAX_BYTES =
  MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes;

type SubmitIngressPool = {
  readonly concurrency: Effect.Semaphore;
  readonly bytes: Effect.Semaphore;
};

const submitIngressPools = new Map<string, SubmitIngressPool>();

const submitIngressPool = ({
  maxConcurrency,
  maxInFlightBytes,
}: {
  readonly maxConcurrency: number;
  readonly maxInFlightBytes: number;
}): SubmitIngressPool => {
  const key = `${maxConcurrency.toString()}:${maxInFlightBytes.toString()}`;
  const existing = submitIngressPools.get(key);
  if (existing !== undefined) return existing;
  const created = {
    concurrency: Effect.unsafeMakeSemaphore(maxConcurrency),
    bytes: Effect.unsafeMakeSemaphore(maxInFlightBytes),
  };
  submitIngressPools.set(key, created);
  return created;
};

export type SubmitIngressReservation =
  | { readonly kind: "invalid_content_length" }
  | { readonly kind: "too_large"; readonly declaredBytes: number }
  | {
      readonly kind: "ready";
      readonly declaredBytes: number | null;
      readonly permitBytes: number;
    };

/**
 * Resolves the request's resource weight without consuming its body. Requests
 * without a trustworthy length reserve the full protocol envelope, while a
 * declared bounded length reserves its exact byte weight.
 */
export const resolveSubmitIngressReservation = (
  contentLength: string | undefined,
  maxBodyBytes = SUBMIT_HTTP_BODY_MAX_BYTES,
): SubmitIngressReservation => {
  if (contentLength === undefined) {
    return {
      kind: "ready",
      declaredBytes: null,
      permitBytes: maxBodyBytes,
    };
  }
  if (!/^(?:0|[1-9][0-9]*)$/u.test(contentLength)) {
    return { kind: "invalid_content_length" };
  }
  const declaredBytes = Number(contentLength);
  if (!Number.isSafeInteger(declaredBytes)) {
    return { kind: "invalid_content_length" };
  }
  if (declaredBytes > maxBodyBytes) {
    return { kind: "too_large", declaredBytes };
  }
  return {
    kind: "ready",
    declaredBytes,
    permitBytes: Math.max(1, declaredBytes),
  };
};

export const readSubmitBodyWithProtocolLimit = (
  request: HttpServerRequest.HttpServerRequest,
) =>
  HttpIncomingMessage.withMaxBodySize(
    request.arrayBuffer,
    Option.some(SUBMIT_HTTP_BODY_MAX_BYTES),
  );

/**
 * Runs the body-read/decode/verification path only while both global
 * request-count and byte-weighted permits are held. Ingress is fail-fast when
 * either bound is exhausted so waiting sockets cannot become an unbounded
 * queue. Both permits are released by Effect on every exit.
 */
export const withSubmitIngressPermit = <A, E, R>({
  maxConcurrency,
  maxInFlightBytes,
  permitBytes,
  effect,
}: {
  readonly maxConcurrency: number;
  readonly maxInFlightBytes: number;
  readonly permitBytes: number;
  readonly effect: Effect.Effect<A, E, R>;
}): Effect.Effect<Option.Option<A>, E, R> => {
  if (
    !Number.isSafeInteger(maxConcurrency) ||
    maxConcurrency <= 0 ||
    !Number.isSafeInteger(maxInFlightBytes) ||
    maxInFlightBytes <= 0 ||
    !Number.isSafeInteger(permitBytes) ||
    permitBytes <= 0 ||
    permitBytes > maxInFlightBytes
  ) {
    return Effect.dieMessage("Invalid submit ingress permit configuration");
  }
  const pool = submitIngressPool({ maxConcurrency, maxInFlightBytes });
  return pool.concurrency
    .withPermitsIfAvailable(
      1,
    )(pool.bytes.withPermitsIfAvailable(permitBytes)(effect))
    .pipe(Effect.map(Option.flatten));
};

export const requestMediaType = (contentType: string | undefined): string =>
  contentType?.split(";")[0]?.trim().toLowerCase() ?? "";

export const parseFixedHexParam = (
  value: unknown,
  byteLength: number,
): Buffer | null => {
  if (typeof value !== "string") {
    return null;
  }
  try {
    return hexToBytes(value, { byteLength, trim: false });
  } catch {
    return null;
  }
};

/**
 * Wraps a route handler with admin-key authorization when the path belongs to
 * the admin-only route set.
 */
export const withAdminAccess = <E, R>(
  endpoint: string,
  handler: Effect.Effect<HttpServerResponse.HttpServerResponse, E, R>,
): Effect.Effect<
  HttpServerResponse.HttpServerResponse,
  E | HttpBodyError,
  R | NodeConfig | HttpServerRequest.HttpServerRequest
> =>
  Effect.gen(function* () {
    const routePath = `/${endpoint}`;
    if (!isAdminRoutePath(routePath)) {
      return yield* handler;
    }
    const request = yield* HttpServerRequest.HttpServerRequest;
    const nodeConfig = yield* NodeConfig;
    const auth = authorizeAdminRoute(
      nodeConfig.ADMIN_API_KEY,
      request.headers["x-midgard-admin-key"],
    );
    if (!auth.authorized) {
      yield* Effect.logWarning(
        `Denied admin route ${routePath}: ${auth.error} (${auth.status})`,
      );
      return yield* HttpServerResponse.json(
        { error: auth.error },
        { status: auth.status },
      );
    }
    return yield* handler;
  });

/**
 * `GET /tx`: returns the CBOR for a known tx hash from mempool or immutable
 * storage.
 */
export const getTxHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const txHashParam = params["tx_hash"];
  const txHashBytes = parseFixedHexParam(txHashParam, 32);
  if (txHashBytes === null) {
    yield* Effect.logInfo(
      `GET /${TX_ENDPOINT} - Invalid transaction hash: ${String(txHashParam)}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: ${String(txHashParam)}` },
      { status: 404 },
    );
  }
  yield* Effect.logInfo("txHashBytes", txHashBytes);
  const foundCbor: Buffer = yield* MempoolDB.retrieveTxCborByHash(
    txHashBytes,
  ).pipe(
    Effect.catchAll((_e) =>
      Effect.gen(function* () {
        const fromImmutable =
          yield* ImmutableDB.retrieveTxCborByHash(txHashBytes);
        yield* Effect.logInfo(
          `GET /${TX_ENDPOINT} - Transaction found in ImmutableDB: ${String(txHashParam)}`,
        );
        return fromImmutable;
      }),
    ),
  );
  yield* Effect.logInfo(
    `GET /${TX_ENDPOINT} - Transaction found in mempool: ${String(txHashParam)}`,
  );
  yield* Effect.logInfo("foundCbor", SDK.bufferToHex(foundCbor));
  return yield* HttpServerResponse.json({
    tx: SDK.bufferToHex(foundCbor),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", TX_ENDPOINT, e)),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      TX_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);
