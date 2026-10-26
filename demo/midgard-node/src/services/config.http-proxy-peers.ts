import { isIP } from "node:net";

import { Config } from "effect";

import {
  DEFAULT_HTTP_RATE_LIMITS,
  HTTP_PEER_MAX_ENTRIES,
  HTTP_RATE_CLASSES,
  httpPublicKeysPerIdleWindow,
  type HttpRate,
  type HttpRateClass,
  type HttpRateClassPolicy,
  type HttpRateLimitPolicy,
} from "./http-rate-limits.js";

/** A small explicit private proxy set; unset means socket identity, never header trust. */
export const httpTrustedProxyPeersConfig = Config.string(
  "HTTP_TRUSTED_PROXY_PEERS",
).pipe(
  Config.withDefault(""),
  Config.mapAttempt((input) => {
    if (input.trim().length === 0) return [] as readonly string[];
    const peers = input.split(",").map((peer) => peer.trim());
    if (
      peers.length > 8 ||
      new Set(peers).size !== peers.length ||
      peers.some((peer) => {
        if (isIP(peer) !== 4) return true;
        const [first, second] = peer.split(".").map(Number);
        return !(
          first === 10 ||
          (first === 192 && second === 168) ||
          (first === 172 && second! >= 16 && second! <= 31)
        );
      })
    )
      throw new Error(
        "HTTP_TRUSTED_PROXY_PEERS requires 1 to 8 distinct private IPv4 addresses",
      );
    return peers;
  }),
);

const parseRate = (
  name: string,
  burst: string,
  perSecond: string,
): HttpRate => {
  const rate = { burst: Number(burst), perSecond: Number(perSecond) };
  if (
    !/^[0-9]+$/u.test(burst) ||
    !Number.isSafeInteger(rate.burst) ||
    rate.burst < 1 ||
    !/^[0-9]+(\.[0-9]+)?$/u.test(perSecond) ||
    !Number.isFinite(rate.perSecond) ||
    rate.perSecond <= 0
  )
    throw new Error(
      `${name} needs a positive integer burst and a positive per-second refill`,
    );
  return rate;
};

/**
 * `HTTP_RATE_LIMIT_<CLASS>` overrides one class: `peerBurst,peerPerSecond` for
 * HEALTH and ADMIN, and `peerBurst,peerPerSecond,globalBurst,globalPerSecond`
 * for READ, BATCH, WRITE and EVALUATE. Unset keeps the default.
 */
const httpRateClassConfig = (kind: HttpRateClass) => {
  const name = `HTTP_RATE_LIMIT_${kind.toUpperCase()}`;
  const fallback = DEFAULT_HTTP_RATE_LIMITS.classes[kind];
  return Config.string(name).pipe(
    Config.withDefault(""),
    Config.mapAttempt((input): HttpRateClassPolicy => {
      if (input.trim().length === 0) return fallback;
      const fields = input.split(",").map((field) => field.trim());
      const expected = fallback.global === undefined ? 2 : 4;
      if (fields.length !== expected)
        throw new Error(`${name} needs ${expected.toString()} numbers`);
      const peer = parseRate(name, fields[0]!, fields[1]!);
      return expected === 2
        ? { peer }
        : { peer, global: parseRate(name, fields[2]!, fields[3]!) };
    }),
  );
};

/**
 * Testing profiles may disable rate refusals; public profiles refuse that
 * before binding. Enabled global rates must keep the public client table from
 * filling within one idle window, or new clients would be refused.
 */
export const httpRateLimitConfig = Config.all({
  enabled: Config.string("HTTP_RATE_LIMIT_ENABLED").pipe(
    Config.withDefault("true"),
    Config.mapAttempt((input) => {
      const value = input.trim().toLowerCase();
      if (value !== "true" && value !== "false")
        throw new Error("HTTP_RATE_LIMIT_ENABLED must be true or false");
      return value === "true";
    }),
  ),
  classes: Config.all(
    Object.fromEntries(
      HTTP_RATE_CLASSES.map((kind) => [kind, httpRateClassConfig(kind)]),
    ) as Record<HttpRateClass, Config.Config<HttpRateClassPolicy>>,
  ),
}).pipe(
  Config.mapAttempt((policy): HttpRateLimitPolicy => {
    const keys = httpPublicKeysPerIdleWindow(policy);
    if (policy.enabled && keys > HTTP_PEER_MAX_ENTRIES)
      throw new Error(
        `HTTP_RATE_LIMIT_READ, _BATCH, _WRITE and _EVALUATE global rates admit ${Math.ceil(keys).toString()} clients per 60 s; the client table holds ${HTTP_PEER_MAX_ENTRIES.toString()} (sum of globalBurst + 60 x globalPerSecond must not exceed it)`,
      );
    return policy;
  }),
) satisfies Config.Config<HttpRateLimitPolicy>;

/** HTTP configuration is one component; preserve existing admin-key decoding. */
export const httpIngressConfig = Config.all({
  ADMIN_API_KEY: Config.string("ADMIN_API_KEY").pipe(Config.withDefault("")),
  HTTP_TRUSTED_PROXY_PEERS: httpTrustedProxyPeersConfig,
  HTTP_RATE_LIMITS: httpRateLimitConfig,
});
