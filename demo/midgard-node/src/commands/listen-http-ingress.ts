import { isIP } from "node:net";

import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  HttpIncomingMessage,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import type { HttpBodyError } from "@effect/platform/HttpBody";
import { Effect, Option } from "effect";

import { PEER_MAX_BODY_BYTES } from "../l2-peer/protocol.js";
import { NodeConfig } from "../services/config.js";
import type { NodeConfigDep } from "../services/config.node-config-dep.js";
import { isTrustedHttpProxy } from "../services/http-address.js";
import {
  DEFAULT_HTTP_RATE_LIMITS,
  type HttpRateClass,
  type HttpRateLimitPolicy,
} from "../services/http-rate-limits.js";
import { HTTP_RESPONSE_MAX_BYTES } from "../services/http-resource-limits.js";
import { HTTP_ROUTE_ACTIONS } from "./http/routes.js";
import { SUBMIT_HTTP_BODY_MAX_BYTES } from "./http/submission-capacity.js";
import { createHttpRateLimiter } from "./listen-http-rate-limit.js";
export { HTTP_RESPONSE_MAX_BYTES } from "../services/http-resource-limits.js";

export const HTTP_JSON_BODY_MAX_BYTES = 65_536;
export const HTTP_QUERY_MAX_BYTES = 2_048;
const RETRY_AFTER = { "retry-after": "5", connection: "close" };

/**
 * Whether a POST answered with `status` keeps its connection. A client
 * retries a rate or capacity refusal (429, 503) and a conflict (409), so
 * those keep it, unless the unread body is larger than one submission: the
 * server would read such a body to its end before the next request. Every
 * other error closes the connection.
 */
const keepsConnection = (status: number, declaredBytes: number): boolean =>
  status < 400 ||
  ((status === 409 || status === 429 || status === 503) &&
    declaredBytes <= SUBMIT_HTTP_BODY_MAX_BYTES);

/** Socket identity only. Forwarded headers cannot mint additional quotas. */
export const httpPeerIdentity = (
  remoteAddress: Option.Option<string>,
): string | undefined => {
  if (Option.isNone(remoteAddress)) return undefined;
  const raw = remoteAddress.value;
  const address =
    raw.startsWith("::ffff:") && isIP(raw.slice(7)) === 4 ? raw.slice(7) : raw;
  return isIP(address) === 0 ? undefined : address.toLowerCase();
};

/**
 * The client a request is charged to. On HTTP_SOCKET_PATH only the local TLS
 * ingress can connect (the socket's directory admits only its users), so its
 * x-midgard-client-ip names the client and a request without one has no
 * identity. Over TCP the socket peer is the client, unless it lies in a
 * configured trusted proxy range, which then names the client instead.
 */
export const httpClientIdentity = (
  request: Pick<
    HttpServerRequest.HttpServerRequest,
    "remoteAddress" | "headers"
  >,
  config: Pick<NodeConfigDep, "HTTP_TRUSTED_PROXY_PEERS" | "HTTP_SOCKET_PATH">,
): string | undefined => {
  const named = () =>
    httpPeerIdentity(
      Option.fromNullable(request.headers["x-midgard-client-ip"]),
    );
  if (config.HTTP_SOCKET_PATH !== undefined)
    return Option.isNone(request.remoteAddress) ? named() : undefined;
  const socket = httpPeerIdentity(request.remoteAddress);
  return socket !== undefined &&
    isTrustedHttpProxy(socket, config.HTTP_TRUSTED_PROXY_PEERS ?? [])
    ? named()
    : socket;
};

export type IngressReservation =
  | { readonly accepted: true; readonly release: () => void }
  | {
      readonly accepted: false;
      readonly status: 429 | 503;
      readonly error: string;
      readonly errorCode: "E_HTTP_RATE_LIMITED" | "E_HTTP_INGRESS_CAPACITY";
      readonly retryAfterSeconds: number;
    };

const capacityFull = {
  accepted: false,
  status: 503,
  error: "HTTP ingress capacity is full",
  errorCode: "E_HTTP_INGRESS_CAPACITY",
  retryAfterSeconds: 5,
} as const satisfies IngressReservation;

/** Public requests in flight; health and admin requests do not count here. */
const PUBLIC_ACTIVE_MAX = 64;
/**
 * Health and admin requests in flight. A budget of their own keeps them
 * answering while public requests hold every public slot. Their bodies are
 * JSON-sized (64 KiB at most), so they stay outside the public byte budget.
 */
const LIVENESS_ACTIVE_MAX = 8;

/** Request rates, concurrent requests and in-flight bytes, checked before any body read. */
export const createHttpIngress = ({
  now = () => performance.now(),
  policy = DEFAULT_HTTP_RATE_LIMITS,
}: {
  readonly now?: () => number;
  readonly policy?: HttpRateLimitPolicy;
} = {}) => {
  const rates = createHttpRateLimiter(policy, now());
  let active = 0;
  let livenessActive = 0;
  let inFlightBytes = 0;
  let advisoryBytes = 0;
  /**
   * Admits a request from `client` (an IP address) of `cost` rate tokens
   * whose body may hold `bytes`. A submission costs one token per
   * transaction it carries; every other request costs one.
   */
  const reserve = (
    client: string,
    kind: HttpRateClass,
    bytes: number,
    cost = 1,
  ): IngressReservation => {
    const checked = rates.check(client, kind, cost, now());
    if (!checked.accepted) return checked;
    const liveness = kind === "health" || kind === "admin";
    if (
      !Number.isSafeInteger(bytes) ||
      bytes < 0 ||
      checked.busy ||
      (liveness
        ? livenessActive >= LIVENESS_ACTIVE_MAX
        : active >= PUBLIC_ACTIVE_MAX ||
          inFlightBytes + bytes >
            2 * MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes ||
          (kind === "evaluate" &&
            advisoryBytes + bytes > MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes))
    )
      return capacityFull;
    rates.admit(checked.client);
    const counted = liveness ? 0 : bytes;
    if (liveness) livenessActive += 1;
    else active += 1;
    inFlightBytes += counted;
    if (kind === "evaluate") advisoryBytes += bytes;
    let released = false;
    return {
      accepted: true,
      release: () => {
        if (released) return;
        released = true;
        rates.release(checked.client);
        if (liveness) livenessActive -= 1;
        else active -= 1;
        inFlightBytes -= counted;
        if (kind === "evaluate") advisoryBytes -= bytes;
      },
    };
  };
  return {
    reserve,
    snapshot: () => ({
      ...rates.snapshot(),
      active,
      livenessActive,
      inFlightBytes,
      advisoryBytes,
    }),
  };
};

const controllers = new WeakMap<object, ReturnType<typeof createHttpIngress>>();
const ingressFor = (config: Pick<NodeConfigDep, "HTTP_RATE_LIMITS">) => {
  let controller = controllers.get(config);
  if (controller === undefined) {
    controller = createHttpIngress({
      policy: config.HTTP_RATE_LIMITS ?? DEFAULT_HTTP_RATE_LIMITS,
    });
    controllers.set(config, controller);
  }
  return controller;
};

/** All installed routes pass here before body reads, database work or evaluation. */
export const withHttpIngress = <E, R>(
  handler: Effect.Effect<HttpServerResponse.HttpServerResponse, E, R>,
  controller?: ReturnType<typeof createHttpIngress>,
) =>
  Effect.gen(function* () {
    const request = yield* HttpServerRequest.HttpServerRequest;
    const config = yield* NodeConfig;
    const identity = httpClientIdentity(request, config);
    if (identity === undefined)
      return yield* HttpServerResponse.json(
        {
          error: "Socket peer identity is unavailable",
          error_code: "E_HTTP_CLIENT_IDENTITY_UNAVAILABLE",
        },
        { status: 503, headers: RETRY_AFTER },
      );
    if (Buffer.byteLength(request.url) > HTTP_QUERY_MAX_BYTES)
      return yield* HttpServerResponse.json(
        { error: "Request target is too long" },
        { status: 414, headers: { connection: "close" } },
      );
    const path = request.url.split("?", 1)[0];
    const actions = HTTP_ROUTE_ACTIONS.filter((action) => action[1] === path);
    if (actions.length === 0)
      return yield* HttpServerResponse.json(
        { error: "Endpoint not found" },
        { status: 404, headers: { connection: "close" } },
      );
    if (!actions.some((action) => action[0] === request.method))
      return yield* HttpServerResponse.json(
        { error: "Method not allowed" },
        {
          status: 405,
          headers: {
            connection: "close",
            allow: actions.map((action) => action[0]).join(", "),
          },
        },
      );
    const kind: HttpRateClass =
      path === "/healthz" || path === "/readyz"
        ? "health"
        : actions[0]![2] === "admin"
          ? "admin"
          : actions[0]![2] === "peer"
            ? "peer"
            : path === "/tx/evaluate"
              ? "evaluate"
              : request.method === "POST" && path === "/tx-status"
                ? "batch"
                : request.method === "POST" && path !== "/utxos"
                  ? "write"
                  : "read";
    // Binary bodies: one submission envelope for a submission or an
    // evaluation, up to the peer bound for a peer's CBOR list.
    const proofBody = path === "/submit" || path === "/tx/evaluate";
    const maxBody =
      request.method === "GET"
        ? 0
        : kind === "peer"
          ? PEER_MAX_BODY_BYTES
          : proofBody
            ? SUBMIT_HTTP_BODY_MAX_BYTES
            : HTTP_JSON_BODY_MAX_BYTES;
    const rawLength = request.headers["content-length"];
    if (rawLength !== undefined && !/^(0|[1-9][0-9]*)$/u.test(rawLength))
      return yield* HttpServerResponse.json(
        { error: "Invalid Content-Length" },
        { status: 400, headers: { connection: "close" } },
      );
    // A submission declares its length, so it holds only the bytes it sends.
    if (
      request.method === "POST" &&
      path === "/submit" &&
      rawLength === undefined
    )
      return yield* HttpServerResponse.json(
        { error: "Content-Length is required" },
        { status: 411, headers: { connection: "close" } },
      );
    const declared = rawLength === undefined ? maxBody : Number(rawLength);
    if (!Number.isSafeInteger(declared) || declared > maxBody)
      return yield* HttpServerResponse.json(
        { error: "Request body exceeds endpoint limit" },
        { status: 413, headers: { connection: "close" } },
      );
    if (
      request.method === "POST" &&
      !proofBody &&
      kind !== "peer" &&
      !["/init", "/commit", "/merge", "/logBlocksDB", "/logGlobals"].includes(
        path,
      ) &&
      request.headers["content-type"]
        ?.split(";", 1)[0]
        ?.trim()
        .toLowerCase() !== "application/json"
    )
      return yield* HttpServerResponse.json(
        { error: "Content-Type must be application/json" },
        { status: 415, headers: { connection: "close" } },
      );
    // Unknown lengths reserve the full allowance. Declared lengths are enforced by the bounded reader.
    // A submission carries one transaction, so every request costs one token.
    const reservation = (controller ?? ingressFor(config)).reserve(
      identity,
      kind,
      declared,
    );
    if (!reservation.accepted)
      return yield* HttpServerResponse.json(
        { error: reservation.error, error_code: reservation.errorCode },
        {
          status: reservation.status,
          headers: {
            "retry-after": reservation.retryAfterSeconds.toString(),
            ...(keepsConnection(reservation.status, declared)
              ? {}
              : { connection: "close" }),
          },
        },
      );
    return yield* HttpIncomingMessage.withMaxBodySize(
      handler,
      Option.some(declared),
    ).pipe(
      Effect.flatMap((response) =>
        response.body._tag === "Empty" ||
        (response.body.contentLength !== undefined &&
          response.body.contentLength <= HTTP_RESPONSE_MAX_BYTES)
          ? Effect.succeed(response)
          : HttpServerResponse.json(
              {
                error:
                  "Response exceeds endpoint limit; request a smaller page",
              },
              { status: 413, headers: { connection: "close" } },
            ),
      ),
      Effect.map((response) =>
        request.method === "POST" && !keepsConnection(response.status, declared)
          ? HttpServerResponse.setHeader(response, "connection", "close")
          : response,
      ),
      Effect.ensuring(Effect.sync(reservation.release)),
    );
  }) satisfies Effect.Effect<
    HttpServerResponse.HttpServerResponse,
    E | HttpBodyError,
    R | NodeConfig | HttpServerRequest.HttpServerRequest
  >;
