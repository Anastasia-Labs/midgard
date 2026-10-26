import { isIP } from "node:net";

import {
  HTTP_PEER_IDLE_MS,
  HTTP_PEER_MAX_ENTRIES,
  type HttpRate,
  type HttpRateClass,
  type HttpRateLimitPolicy,
} from "../services/http-rate-limits.js";

export { HTTP_PEER_MAX_ENTRIES } from "../services/http-rate-limits.js";
const TABLE_FULL_RETRY_AFTER_SECONDS = 5;

type Bucket = { tokens: number; updatedAt: number };
export type HttpRatePeer = {
  active: number;
  lastUsed: number;
  readonly buckets: Partial<Record<HttpRateClass, Bucket>>;
};
export type HttpRateRefusal = {
  readonly accepted: false;
  readonly status: 429;
  readonly error: string;
  readonly errorCode: "E_HTTP_RATE_LIMITED";
  readonly retryAfterSeconds: number;
};
export type HttpRateCheck =
  | HttpRateRefusal
  | {
      readonly accepted: true;
      readonly peer: HttpRatePeer;
      readonly isNew: boolean;
    };

const refill = (bucket: Bucket, rate: HttpRate, now: number): void => {
  bucket.tokens = Math.min(
    rate.burst,
    bucket.tokens +
      (Math.max(0, now - bucket.updatedAt) * rate.perSecond) / 1_000,
  );
  bucket.updatedAt = now;
};

const refusal = (
  error: string,
  retryAfterSeconds: number,
): HttpRateRefusal => ({
  accepted: false,
  status: 429,
  error,
  errorCode: "E_HTTP_RATE_LIMITED",
  retryAfterSeconds,
});

/** Refills the bucket; 0 when a token is available, else whole seconds until one is back. */
const waitSeconds = (bucket: Bucket, rate: HttpRate, now: number): number => {
  refill(bucket, rate, now);
  if (bucket.tokens >= 1) return 0;
  return Math.max(1, Math.ceil((1 - bucket.tokens) / rate.perSecond));
};

const ipv6Groups = (part: string): string[] =>
  part === ""
    ? []
    : part
        .split(":")
        .flatMap((group) => (group.includes(".") ? ["0", "0"] : [group]));

/**
 * The rate key of a client address. One IPv6 subscriber is normally handed a
 * whole /64, so IPv6 addresses share one key per /64; IPv4 keys are the address.
 */
export const httpRateKey = (address: string): string => {
  if (isIP(address) !== 6) return address;
  const [head = "", tail] = address.split("%", 1)[0]!.split("::");
  const left = ipv6Groups(head);
  const right = tail === undefined ? [] : ipv6Groups(tail);
  const groups =
    tail === undefined
      ? left
      : [
          ...left,
          ...Array<string>(8 - left.length - right.length).fill("0"),
          ...right,
        ];
  return `${groups
    .slice(0, 4)
    .map((group) => Number.parseInt(group, 16).toString(16))
    .join(":")}::/64`;
};

/** Drops the least recently used key that has no request in flight. */
const evictLeastRecent = (table: Map<string, HttpRatePeer>): boolean => {
  for (const [entry, value] of table)
    if (value.active === 0) {
      table.delete(entry);
      return true;
    }
  return false;
};

/**
 * Token buckets per client key and class, plus one global bucket per public
 * class. Health and admin clients live in their own table, so neither a public
 * flood nor an address spray against public routes can refuse them. Each
 * table keeps its keys in least-recently-used order.
 */
export const createHttpRateLimiter = (
  policy: HttpRateLimitPolicy,
  start: number,
) => {
  const publicPeers = new Map<string, HttpRatePeer>();
  const livenessPeers = new Map<string, HttpRatePeer>();
  const globals: Partial<Record<HttpRateClass, Bucket>> = {};
  for (const [kind, rates] of Object.entries(policy.classes))
    if (rates.global !== undefined)
      globals[kind as HttpRateClass] = {
        tokens: rates.global.burst,
        updatedAt: start,
      };
  const tableFor = (kind: HttpRateClass) =>
    kind === "health" || kind === "admin" ? livenessPeers : publicPeers;

  /** Checks every bucket a request draws from. Allocates nothing. */
  const check = (
    key: string,
    kind: HttpRateClass,
    now: number,
  ): HttpRateCheck => {
    const rates = policy.classes[kind];
    const existing = tableFor(kind).get(key);
    const peer = existing ?? { active: 0, lastUsed: now, buckets: {} };
    if (policy.enabled) {
      const global = globals[kind];
      const bucket = (peer.buckets[kind] ??= {
        tokens: rates.peer.burst,
        updatedAt: now,
      });
      // Retry-After covers every refusing bucket, not only the first one.
      const wait = Math.max(
        global === undefined || rates.global === undefined
          ? 0
          : waitSeconds(global, rates.global, now),
        waitSeconds(bucket, rates.peer, now),
      );
      if (wait > 0) return refusal("Request rate exceeded", wait);
    }
    return { accepted: true, peer, isNew: existing === undefined };
  };

  /**
   * Records an accepted request. When a table is full, a new key needs room:
   * - the enabled public table evicts only keys idle for the whole window, so
   *   eviction cannot reset a public client's quota, and refuses otherwise;
   *   the global rates keep it from filling (checked when config loads);
   * - the health/admin table, and the public table with rate limits disabled,
   *   evict the least recently used key, because losing such a quota costs
   *   nothing and refusing the client is the harm.
   * At most 72 requests are in flight, so an evictable key always exists.
   */
  const admit = (
    key: string,
    kind: HttpRateClass,
    checked: Extract<HttpRateCheck, { accepted: true }>,
    now: number,
  ): HttpRateRefusal | undefined => {
    const table = tableFor(kind);
    if (checked.isNew && table.size >= HTTP_PEER_MAX_ENTRIES) {
      if (table === publicPeers && policy.enabled) {
        for (const [entry, value] of table)
          if (value.active === 0 && now - value.lastUsed >= HTTP_PEER_IDLE_MS)
            table.delete(entry);
      } else evictLeastRecent(table);
      if (table.size >= HTTP_PEER_MAX_ENTRIES)
        return refusal(
          "Peer quota capacity is full",
          TABLE_FULL_RETRY_AFTER_SECONDS,
        );
    }
    // Re-inserting keeps the table in least-recently-used order. Refused
    // requests neither allocate a key nor extend its idle lifetime.
    table.delete(key);
    table.set(key, checked.peer);
    checked.peer.lastUsed = now;
    if (policy.enabled) {
      checked.peer.buckets[kind]!.tokens -= 1;
      const global = globals[kind];
      if (global !== undefined) global.tokens -= 1;
    }
    return undefined;
  };

  return {
    check,
    admit,
    snapshot: () => ({
      peers: publicPeers.size,
      livenessPeers: livenessPeers.size,
    }),
  };
};
