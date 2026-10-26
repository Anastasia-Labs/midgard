import { isIP } from "node:net";

import {
  type HttpRate,
  type HttpRateClass,
  type HttpRateLimitPolicy,
} from "../services/http-rate-limits.js";

/** Each client table holds at most this many keys; idle keys are evicted first. */
export const HTTP_PEER_MAX_ENTRIES = 8_192;
const PEER_IDLE_MS = 60_000;
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

/** Refills the bucket and refuses with the whole seconds until one token is back. */
const drawable = (
  bucket: Bucket,
  rate: HttpRate,
  now: number,
): HttpRateRefusal | undefined => {
  refill(bucket, rate, now);
  if (bucket.tokens >= 1) return undefined;
  return refusal(
    "Request rate exceeded",
    Math.max(1, Math.ceil((1 - bucket.tokens) / rate.perSecond)),
  );
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

/**
 * Token buckets per client key and class, plus one global bucket per public
 * class. Health and admin clients live in their own table, so neither a public
 * flood nor an address spray against public routes can refuse them.
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
    const global = globals[kind];
    if (policy.enabled && global !== undefined && rates.global !== undefined) {
      const refused = drawable(global, rates.global, now);
      if (refused !== undefined) return refused;
    }
    const existing = tableFor(kind).get(key);
    const peer = existing ?? { active: 0, lastUsed: now, buckets: {} };
    if (policy.enabled) {
      const bucket = (peer.buckets[kind] ??= {
        tokens: rates.peer.burst,
        updatedAt: now,
      });
      const refused = drawable(bucket, rates.peer, now);
      if (refused !== undefined) return refused;
    }
    return { accepted: true, peer, isNew: existing === undefined };
  };

  /**
   * Records an accepted request. A new key evicts idle keys when its table is
   * full and is refused if none is idle; active or recent keys are never
   * evicted, so eviction cannot reset a client's quota.
   */
  const admit = (
    key: string,
    kind: HttpRateClass,
    checked: Extract<HttpRateCheck, { accepted: true }>,
    now: number,
  ): HttpRateRefusal | undefined => {
    const table = tableFor(kind);
    if (checked.isNew) {
      if (table.size >= HTTP_PEER_MAX_ENTRIES)
        for (const [entry, value] of table)
          if (value.active === 0 && now - value.lastUsed >= PEER_IDLE_MS)
            table.delete(entry);
      if (table.size >= HTTP_PEER_MAX_ENTRIES)
        return refusal(
          "Peer quota capacity is full",
          TABLE_FULL_RETRY_AFTER_SECONDS,
        );
      table.set(key, checked.peer);
    }
    // Refused requests neither allocate a key nor extend its idle lifetime.
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
