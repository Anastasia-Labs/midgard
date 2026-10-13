import { isIP } from "node:net";

import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  HttpIncomingMessage,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import type { HttpBodyError } from "@effect/platform/HttpBody";
import { Effect, Option } from "effect";

import { NodeConfig } from "../services/config.js";
import { HTTP_RESPONSE_MAX_BYTES } from "../services/http-resource-limits.js";
export { HTTP_RESPONSE_MAX_BYTES } from "../services/http-resource-limits.js";
import { HTTP_ROUTE_ACTIONS } from "./listen-route-policy.js";

export const HTTP_JSON_BODY_MAX_BYTES = 65_536;
export const HTTP_QUERY_MAX_BYTES = 2_048;
export const HTTP_PEER_MAX_ENTRIES = 8_192;
const PEER_IDLE_MS = 60_000;
const RETRY_AFTER = { "retry-after": "5", connection: "close" };
type RequestClass = "read" | "write" | "evaluate";
type Bucket = { tokens: number; updatedAt: number };
type Peer = {
  active: number;
  lastUsed: number;
  buckets: Record<RequestClass, Bucket>;
};
const peerRates = {
  read: [30, 10],
  write: [4, 1],
  evaluate: [2, 0.2],
} as const;
const globalRates = {
  read: [200, 100],
  write: [16, 4],
  evaluate: [8, 1],
} as const;
const buckets = (
  rates: typeof peerRates | typeof globalRates,
  now: number,
): Record<RequestClass, Bucket> => ({
  read: { tokens: rates.read[0], updatedAt: now },
  write: { tokens: rates.write[0], updatedAt: now },
  evaluate: { tokens: rates.evaluate[0], updatedAt: now },
});
const refill = (
  bucket: Bucket,
  rate: readonly [number, number],
  now: number,
): void => {
  bucket.tokens = Math.min(
    rate[0],
    bucket.tokens + (Math.max(0, now - bucket.updatedAt) * rate[1]) / 1_000,
  );
  bucket.updatedAt = now;
};

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

/** Only a configured actual proxy socket may supply an overwritten single client identity. */
export const httpClientIdentity = (
  request: Pick<
    HttpServerRequest.HttpServerRequest,
    "remoteAddress" | "headers"
  >,
  trustedPeers: readonly string[],
): string | undefined => {
  const socket = httpPeerIdentity(request.remoteAddress);
  if (socket === undefined || !trustedPeers.includes(socket)) return socket;
  return httpPeerIdentity(
    Option.fromNullable(request.headers["x-midgard-client-ip"]),
  );
};

export type IngressReservation =
  | { readonly accepted: true; readonly release: () => void }
  | {
      readonly accepted: false;
      readonly status: 429 | 503;
      readonly error: string;
    };

/** Fixed-size peer table; active/unexpired peers are never evicted to reset quotas. */
export const createHttpIngress = (
  now: () => number = () => performance.now(),
) => {
  const peers = new Map<string, Peer>();
  const global = buckets(globalRates, now());
  let active = 0;
  let inFlightBytes = 0;
  let advisoryBytes = 0;
  const reserve = (
    identity: string,
    kind: RequestClass,
    bytes: number,
  ): IngressReservation => {
    const timestamp = now();
    refill(global[kind], globalRates[kind], timestamp);
    if (global[kind].tokens < 1)
      return { accepted: false, status: 429, error: "Request rate exceeded" };
    if (
      !Number.isSafeInteger(bytes) ||
      bytes < 0 ||
      active >= 64 ||
      inFlightBytes + bytes > 2 * MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes ||
      (kind === "evaluate" &&
        advisoryBytes + bytes > MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes)
    )
      return {
        accepted: false,
        status: 503,
        error: "HTTP ingress capacity is full",
      };
    let peer = peers.get(identity);
    const newPeer = peer === undefined;
    if (peer === undefined)
      peer = {
        active: 0,
        lastUsed: timestamp,
        buckets: buckets(peerRates, timestamp),
      };
    refill(peer.buckets[kind], peerRates[kind], timestamp);
    if (peer.buckets[kind].tokens < 1)
      return { accepted: false, status: 429, error: "Request rate exceeded" };
    if (peer.active >= 4)
      return {
        accepted: false,
        status: 503,
        error: "HTTP ingress capacity is full",
      };
    if (newPeer) {
      if (peers.size >= HTTP_PEER_MAX_ENTRIES)
        for (const [key, value] of peers)
          if (value.active === 0 && timestamp - value.lastUsed >= PEER_IDLE_MS)
            peers.delete(key);
      if (peers.size >= HTTP_PEER_MAX_ENTRIES)
        return {
          accepted: false,
          status: 429,
          error: "Peer quota capacity is full",
        };
      peers.set(identity, peer);
    }
    // Refused requests neither allocate a peer nor extend its idle lifetime.
    peer.lastUsed = timestamp;
    peer.buckets[kind].tokens -= 1;
    global[kind].tokens -= 1;
    peer.active += 1;
    active += 1;
    inFlightBytes += bytes;
    if (kind === "evaluate") advisoryBytes += bytes;
    let released = false;
    return {
      accepted: true,
      release: () => {
        if (released) return;
        released = true;
        peer.active -= 1;
        active -= 1;
        inFlightBytes -= bytes;
        if (kind === "evaluate") advisoryBytes -= bytes;
      },
    };
  };
  return {
    reserve,
    snapshot: () => ({
      peers: peers.size,
      active,
      inFlightBytes,
      advisoryBytes,
    }),
  };
};

const controllers = new WeakMap<object, ReturnType<typeof createHttpIngress>>();
const ingressFor = (config: object) => {
  let controller = controllers.get(config);
  if (controller === undefined) {
    controller = createHttpIngress();
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
    const identity = httpClientIdentity(
      request,
      config.HTTP_TRUSTED_PROXY_PEERS ?? [],
    );
    if (identity === undefined)
      return yield* HttpServerResponse.json(
        { error: "Socket peer identity is unavailable" },
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
    const kind: RequestClass =
      path === "/tx/evaluate"
        ? "evaluate"
        : request.method === "POST" && !["/utxos", "/tx-status"].includes(path)
          ? "write"
          : "read";
    const proofBody = path === "/submit" || path === "/tx/evaluate";
    const maxBody =
      request.method === "GET"
        ? 0
        : proofBody
          ? MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes
          : HTTP_JSON_BODY_MAX_BYTES;
    const rawLength = request.headers["content-length"];
    if (rawLength !== undefined && !/^(0|[1-9][0-9]*)$/u.test(rawLength))
      return yield* HttpServerResponse.json(
        { error: "Invalid Content-Length" },
        { status: 400, headers: { connection: "close" } },
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
    const reservation = (controller ?? ingressFor(config)).reserve(
      identity,
      kind,
      declared,
    );
    if (!reservation.accepted)
      return yield* HttpServerResponse.json(
        { error: reservation.error },
        { status: reservation.status, headers: RETRY_AFTER },
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
        request.method === "POST" && response.status >= 400
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
