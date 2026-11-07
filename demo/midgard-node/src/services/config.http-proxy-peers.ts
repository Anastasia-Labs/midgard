import { isIP } from "node:net";

import { Config } from "effect";

import {
  DEFAULT_HTTP_RATE_LIMITS,
  HTTP_RATE_CLASSES,
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

const RATE_TIERS = ["peer", "prefix", "global"] as const;

/**
 * `HTTP_RATE_LIMIT_<CLASS>` overrides one class with a `burst,perSecond` pair
 * for each tier the class has, in the order client, prefix, global:
 * `clientBurst,clientPerSecond` for HEALTH, ADMIN and PEER;
 * `clientBurst,clientPerSecond,prefixBurst,prefixPerSecond` for WRITE; and
 * `clientBurst,clientPerSecond,globalBurst,globalPerSecond` for READ, BATCH
 * and EVALUATE. Unset keeps the default.
 */
const httpRateClassConfig = (kind: HttpRateClass) => {
  const name = `HTTP_RATE_LIMIT_${kind.toUpperCase()}`;
  const fallback = DEFAULT_HTTP_RATE_LIMITS.classes[kind];
  const tiers = RATE_TIERS.filter((tier) => fallback[tier] !== undefined);
  return Config.string(name).pipe(
    Config.withDefault(""),
    Config.mapAttempt((input): HttpRateClassPolicy => {
      if (input.trim().length === 0) return fallback;
      const fields = input.split(",").map((field) => field.trim());
      if (fields.length !== 2 * tiers.length)
        throw new Error(
          `${name} needs ${(2 * tiers.length).toString()} numbers: a burst and a per-second refill for ${tiers.map((tier) => (tier === "peer" ? "client" : tier)).join(", ")}`,
        );
      return Object.fromEntries(
        tiers.map((tier, index) => [
          tier,
          parseRate(name, fields[2 * index]!, fields[2 * index + 1]!),
        ]),
      ) as HttpRateClassPolicy;
    }),
  );
};

/** Testing profiles may disable rate refusals; public profiles refuse that before binding. */
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
}) satisfies Config.Config<HttpRateLimitPolicy>;

/** HTTP configuration is one component; preserve existing admin-key decoding. */
export const httpIngressConfig = Config.all({
  ADMIN_API_KEY: Config.string("ADMIN_API_KEY").pipe(Config.withDefault("")),
  HTTP_TRUSTED_PROXY_PEERS: httpTrustedProxyPeersConfig,
  HTTP_RATE_LIMITS: httpRateLimitConfig,
});
