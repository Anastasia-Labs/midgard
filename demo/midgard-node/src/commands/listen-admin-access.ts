import { createHmac, randomBytes, timingSafeEqual } from "node:crypto";

import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { Effect } from "effect";

import { NodeConfig } from "../services/config.js";
import { authorizeAdminRoute, isAdminRoutePath } from "./listen-utils.js";

export const ADMIN_MUTATION_PATHS: ReadonlySet<string> = new Set([
  "/init",
  "/commit",
  "/merge",
  "/logBlocksDB",
  "/logGlobals",
]);
const WINDOW_MS = 30_000;
const MAX_NONCES = 4_096;
type Session = { readonly id: string; readonly nonces: Map<string, number> };
const sessions = new WeakMap<object, Session>();
const sessionFor = (config: object): Session => {
  let session = sessions.get(config);
  if (session === undefined) {
    session = { id: randomBytes(32).toString("hex"), nonces: new Map() };
    sessions.set(config, session);
  }
  return session;
};

/** Empty-body mutations bind method, exact target, process session, timestamp and nonce. */
export const signAdminMutation = (
  key: string,
  session: string,
  target: string,
  timestamp: string,
  nonce: string,
): string =>
  createHmac("sha256", key.trim())
    .update(JSON.stringify(["POST", target, session, timestamp, nonce]))
    .digest("hex");

export const claimAdminMutation = (
  config: object,
  key: string,
  target: string,
  headers: Readonly<Record<string, string | undefined>>,
  now = Date.now(),
): 401 | 409 | 503 | undefined => {
  const session = sessionFor(config);
  const nonce = headers["x-midgard-admin-nonce"];
  const timestamp = headers["x-midgard-admin-timestamp"];
  const signature = headers["x-midgard-admin-signature"];
  if (
    key.trim().length === 0 ||
    headers["x-midgard-admin-session"] !== session.id ||
    nonce === undefined ||
    !/^[0-9a-f]{64}$/u.test(nonce) ||
    timestamp === undefined ||
    !/^[1-9][0-9]{12}$/u.test(timestamp) ||
    Math.abs(now - Number(timestamp)) > WINDOW_MS ||
    signature === undefined ||
    !/^[0-9a-f]{64}$/u.test(signature)
  )
    return 401;
  const expected = signAdminMutation(key, session.id, target, timestamp, nonce);
  if (
    !timingSafeEqual(
      Buffer.from(expected, "hex"),
      Buffer.from(signature, "hex"),
    )
  )
    return 401;
  for (const [old, expiry] of session.nonces)
    if (expiry < now) session.nonces.delete(old);
  if (session.nonces.has(nonce)) return 409;
  if (session.nonces.size >= MAX_NONCES) return 503;
  // Retain until the timestamp is no longer admissible, even when execution fails.
  session.nonces.set(nonce, Number(timestamp) + WINDOW_MS);
  return undefined;
};

export const getAdminSessionHandler = Effect.gen(function* () {
  const config = yield* NodeConfig;
  return yield* HttpServerResponse.json(
    { session: sessionFor(config).id, validityMs: WINDOW_MS },
    { headers: { "cache-control": "no-store" } },
  );
});

export const withAdminAccess = <A, E, R>(
  endpoint: string,
  handler: Effect.Effect<A, E, R>,
) => {
  if (!isAdminRoutePath(endpoint)) return handler;
  return Effect.gen(function* () {
    const request = yield* HttpServerRequest.HttpServerRequest;
    const config = yield* NodeConfig;
    const path = endpoint.startsWith("/") ? endpoint : `/${endpoint}`;
    if (ADMIN_MUTATION_PATHS.has(path)) {
      if (request.method !== "POST")
        return yield* HttpServerResponse.json(
          { error: "Admin mutation requires POST" },
          { status: 405, headers: { allow: "POST" } },
        );
      // Administrative mutations have no input body or query arguments. Refuse a chunked body too.
      if (
        request.url.includes("?") ||
        request.headers["transfer-encoding"] !== undefined ||
        (request.headers["content-length"] !== undefined &&
          request.headers["content-length"] !== "0")
      )
        return yield* HttpServerResponse.json(
          { error: "Admin mutation requires an empty body and no query" },
          { status: 400 },
        );
      const status = claimAdminMutation(
        config,
        config.ADMIN_API_KEY,
        request.url,
        request.headers,
      );
      if (status !== undefined)
        return yield* HttpServerResponse.json(
          {
            error:
              status === 409
                ? "Admin request was already claimed"
                : status === 503
                  ? "Admin replay capacity is full"
                  : "Missing or invalid admin signature",
          },
          { status, headers: { "cache-control": "no-store" } },
        );
    } else {
      const auth = authorizeAdminRoute(
        config.ADMIN_API_KEY,
        request.headers["x-midgard-admin-key"],
      );
      if (!auth.authorized)
        return yield* HttpServerResponse.json(
          { error: auth.error },
          { status: auth.status, headers: { "cache-control": "no-store" } },
        );
    }
    return yield* handler;
  });
};
