/** Submission body bounds and process-wide capacity reservations. All routers share the same pools. */
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { HttpIncomingMessage, HttpServerRequest } from "@effect/platform";
import { Effect, Option } from "effect";

export const V1_SUBMISSION_MEDIA_TYPE = "application/vnd.midgard.v1+cbor";

/**
 * The largest `/submit` and `/tx/evaluate` body: the V1 envelope
 * `[version, txCbor]` around one largest transaction. The array head and the
 * version take one byte each, and a byte string of more than 65,535 bytes has
 * a five-byte head.
 */
export const SUBMIT_HTTP_BODY_MAX_BYTES =
  1 + 1 + 5 + MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes;

/** Requests and body bytes a pool holds; plain counts, so a raw handler and an Effect take them alike. */
type SubmitIngressPool = { requests: number; bytes: number };

const submitIngressPools = new Map<string, SubmitIngressPool>();

/**
 * Takes one request and `permitBytes` of the named pool when both fit, and
 * returns their release (idempotent); `undefined` when either is exhausted.
 * Never waits: a full pool refuses at once, so waiting sockets cannot become
 * an unbounded queue. Each named `pool` has its own permits, so traffic of
 * one pool never takes another's.
 */
export const tryTakeSubmitIngressPermit = ({
  pool = "submit",
  maxConcurrency,
  maxInFlightBytes,
  permitBytes,
}: {
  readonly pool?: string;
  readonly maxConcurrency: number;
  readonly maxInFlightBytes: number;
  readonly permitBytes: number;
}): (() => void) | undefined => {
  const key = `${pool}:${maxConcurrency.toString()}:${maxInFlightBytes.toString()}`;
  let held = submitIngressPools.get(key);
  if (held === undefined) {
    held = { requests: 0, bytes: 0 };
    submitIngressPools.set(key, held);
  }
  if (
    held.requests >= maxConcurrency ||
    held.bytes + permitBytes > maxInFlightBytes
  )
    return undefined;
  const taken = held;
  taken.requests += 1;
  taken.bytes += permitBytes;
  let released = false;
  return () => {
    if (released) return;
    released = true;
    taken.requests -= 1;
    taken.bytes -= permitBytes;
  };
};

/** Whether a pool configuration is one `tryTakeSubmitIngressPermit` accepts. */
export const isValidSubmitIngressPermit = ({
  maxConcurrency,
  maxInFlightBytes,
  permitBytes,
}: {
  readonly maxConcurrency: number;
  readonly maxInFlightBytes: number;
  readonly permitBytes: number;
}): boolean =>
  Number.isSafeInteger(maxConcurrency) &&
  maxConcurrency > 0 &&
  Number.isSafeInteger(maxInFlightBytes) &&
  maxInFlightBytes > 0 &&
  Number.isSafeInteger(permitBytes) &&
  permitBytes > 0 &&
  permitBytes <= maxInFlightBytes;

export type SubmitIngressReservation =
  | { readonly kind: "invalid_content_length" }
  | { readonly kind: "too_large"; readonly declaredBytes: number }
  | {
      readonly kind: "ready";
      readonly declaredBytes: number | null;
      readonly permitBytes: number;
    };

/**
 * Resolves the request's resource weight without consuming its body. A
 * request without a length (the HTTP ingress refuses one on `/submit`)
 * reserves the largest envelope, while a declared bounded length reserves its
 * exact byte weight.
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
 * request-count and byte-weighted permits are held (`tryTakeSubmitIngressPermit`).
 * Ingress is fail-fast when either bound is exhausted. Both permits are
 * released on every exit, interruption included.
 */
export const withSubmitIngressPermit = <A, E, R>({
  pool = "submit",
  maxConcurrency,
  maxInFlightBytes,
  permitBytes,
  effect,
}: {
  readonly pool?: string;
  readonly maxConcurrency: number;
  readonly maxInFlightBytes: number;
  readonly permitBytes: number;
  readonly effect: Effect.Effect<A, E, R>;
}): Effect.Effect<Option.Option<A>, E, R> => {
  if (
    !isValidSubmitIngressPermit({
      maxConcurrency,
      maxInFlightBytes,
      permitBytes,
    })
  )
    return Effect.dieMessage("Invalid submit ingress permit configuration");
  return Effect.uninterruptibleMask((restore) =>
    Effect.suspend(() => {
      const release = tryTakeSubmitIngressPermit({
        pool,
        maxConcurrency,
        maxInFlightBytes,
        permitBytes,
      });
      if (release === undefined) return Effect.succeed(Option.none<A>());
      return restore(effect).pipe(
        Effect.map(Option.some),
        Effect.ensuring(Effect.sync(release)),
      );
    }),
  );
};

export const requestMediaType = (contentType: string | undefined): string =>
  contentType?.split(";")[0]?.trim().toLowerCase() ?? "";
