/** Submission body bounds and process-wide capacity reservations. All routers share the same semaphore pools. */
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { HttpIncomingMessage, HttpServerRequest } from "@effect/platform";
import { Effect, Option } from "effect";

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
    Option.some(
      Math.min(
        SUBMIT_HTTP_BODY_MAX_BYTES,
        Number(request.headers["content-length"] ?? SUBMIT_HTTP_BODY_MAX_BYTES),
      ),
    ),
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
