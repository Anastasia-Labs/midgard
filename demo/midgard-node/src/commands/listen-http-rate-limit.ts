import { hash, randomBytes } from "node:crypto";

import { httpAddressBytes } from "../services/http-address.js";
import {
  HTTP_PUBLIC_RATE_CLASSES,
  type HttpPublicRateClass,
  type HttpRate,
  type HttpRateClass,
  type HttpRateLimitPolicy,
  isHttpPublicRateClass,
} from "../services/http-rate-limits.js";

/**
 * Requests one client may have in flight at once. The public ingress caps
 * concurrent requests per address at this number too (`public-ingress.mjs`),
 * and the throughput drill's one-client route never exceeds it.
 */
export const CLIENT_ACTIVE_MAX = 4;
/** Cells in each of the two rows of a hashed array. */
const ROW_CELLS = 2 ** 18;
const CELL_MASK = ROW_CELLS - 1;
/** Health, admin and peer clients remembered; the least recently used idle one goes first. */
export const HTTP_LIVENESS_MAX_ENTRIES = 8_192;
/** Float32 refills are inexact; a bucket this close to the cost pays it. */
const TOKEN_EPSILON = 1e-3;

type Bucket = { tokens: number; updatedAt: number };
type LivenessEntry = {
  active: number;
  readonly buckets: Partial<Record<HttpRateClass, Bucket>>;
};
/** One tier of a public class: a bucket per cell, refilled in whole seconds. */
type HashedTier = {
  readonly rate: HttpRate;
  readonly tokens: Float32Array;
  /** Seconds since the limiter started; Uint32 seconds wrap after 136 years. */
  readonly refilledAt: Uint32Array;
};
type Cells = readonly [number, number];

/** What `admit` and `release` need about a client that passed `check`. */
export type HttpRateClient =
  | {
      readonly public: true;
      readonly kind: HttpPublicRateClass;
      readonly cost: number;
      readonly cells: Cells;
      readonly prefixCells: Cells | undefined;
    }
  | {
      readonly public: false;
      readonly kind: HttpRateClass;
      readonly key: string;
      readonly entry: LivenessEntry;
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
      readonly client: HttpRateClient;
      /** The client already has its maximum number of requests in flight. */
      readonly busy: boolean;
    };

const refusal = (retryAfterSeconds: number): HttpRateRefusal => ({
  accepted: false,
  status: 429,
  error: "Request rate exceeded",
  errorCode: "E_HTTP_RATE_LIMITED",
  retryAfterSeconds,
});

/**
 * The client of an address is the IPv4 address or the IPv6 /56, and its
 * prefix the IPv4 /24 or the IPv6 /48. One IPv6 subscriber is normally
 * handed a /56 or a /48, so a key per /64 would let one subscriber claim
 * 256 to 65,536 keys.
 */
const clientAndPrefix = (bytes: Uint8Array) =>
  bytes.length === 4
    ? { client: bytes, prefix: bytes.subarray(0, 3) }
    : { client: bytes.subarray(0, 7), prefix: bytes.subarray(0, 6) };

const createTier = (rate: HttpRate): HashedTier => ({
  rate,
  tokens: new Float32Array(2 * ROW_CELLS).fill(rate.burst),
  refilledAt: new Uint32Array(2 * ROW_CELLS),
});

/** Refills both cells; 0 when both hold `cost` tokens, else whole seconds until they do. */
const tierWait = (
  tier: HashedTier,
  cells: Cells,
  cost: number,
  seconds: number,
): number => {
  let tokens = tier.rate.burst;
  for (const cell of cells) {
    const elapsed = seconds - tier.refilledAt[cell]!;
    if (elapsed > 0) {
      tier.tokens[cell] = Math.min(
        tier.rate.burst,
        tier.tokens[cell]! + elapsed * tier.rate.perSecond,
      );
      tier.refilledAt[cell] = seconds;
    }
    tokens = Math.min(tokens, tier.tokens[cell]!);
  }
  return tokens >= cost - TOKEN_EPSILON
    ? 0
    : Math.max(1, Math.ceil((cost - tokens) / tier.rate.perSecond));
};

const debit = (tier: HashedTier, cells: Cells, cost: number): void => {
  for (const cell of cells) tier.tokens[cell] = tier.tokens[cell]! - cost;
};

/** Refills a millisecond bucket; 0 when it holds `cost`, else whole seconds until it does. */
const bucketWait = (
  bucket: Bucket,
  rate: HttpRate,
  cost: number,
  now: number,
): number => {
  bucket.tokens = Math.min(
    rate.burst,
    bucket.tokens +
      (Math.max(0, now - bucket.updatedAt) * rate.perSecond) / 1_000,
  );
  bucket.updatedAt = now;
  if (bucket.tokens >= cost) return 0;
  return Math.max(1, Math.ceil((cost - bucket.tokens) / rate.perSecond));
};

/**
 * Token buckets per client and class.
 *
 * The public classes (read, batch, write, evaluate) keep their client and
 * prefix buckets in fixed typed arrays of 2 rows × 2^18 cells, plus one
 * global bucket where the class has one. A key maps to one cell per row
 * through SHA-256 keyed with a secret drawn at start, so clients cannot aim
 * at each other's cells. A request of cost `c` passes only if every cell it
 * maps to holds `c` tokens, and admission debits them all. Nothing is
 * allocated per client and nothing is evicted: a collision only tightens a
 * limit, and wrongly limits an honest key only when saturated keys share its
 * cell in both rows. Each client's in-flight count lives in a Uint16 array on
 * the same cells.
 *
 * Health, admin and peer clients are few, so they keep a small table in
 * least-recently-used order, separate from the public arrays: no public flood
 * or address spray can refuse them.
 */
export const createHttpRateLimiter = (
  policy: HttpRateLimitPolicy,
  start: number,
) => {
  const secret = randomBytes(16);
  const hashInput = Buffer.alloc(secret.length + 1 + 7);
  secret.copy(hashInput);
  /** `tag` keeps the address family and tier of equal-length keys apart. */
  const cellsOf = (tag: number, bytes: Uint8Array): Cells => {
    hashInput[secret.length] = tag;
    hashInput.set(bytes, secret.length + 1);
    const digest = hash(
      "sha256",
      hashInput.subarray(0, secret.length + 1 + bytes.length),
      "buffer",
    );
    return [
      digest.readUInt32LE(0) & CELL_MASK,
      ROW_CELLS + (digest.readUInt32LE(4) & CELL_MASK),
    ];
  };
  const inFlight = new Uint16Array(2 * ROW_CELLS);
  const tiers = Object.fromEntries(
    HTTP_PUBLIC_RATE_CLASSES.map((kind) => {
      const rates = policy.classes[kind];
      return [
        kind,
        {
          client: createTier(rates.peer),
          prefix:
            rates.prefix === undefined ? undefined : createTier(rates.prefix),
          global:
            rates.global === undefined
              ? undefined
              : { tokens: rates.global.burst, updatedAt: start },
        },
      ];
    }),
  ) as Record<
    HttpPublicRateClass,
    {
      readonly client: HashedTier;
      readonly prefix: HashedTier | undefined;
      readonly global: Bucket | undefined;
    }
  >;
  const liveness = new Map<string, LivenessEntry>();

  const checkPublic = (
    bytes: Uint8Array,
    kind: HttpPublicRateClass,
    cost: number,
    now: number,
  ): HttpRateCheck => {
    const { client, prefix } = clientAndPrefix(bytes);
    const cells = cellsOf(bytes.length, client);
    const state = tiers[kind];
    const prefixCells =
      state.prefix === undefined
        ? undefined
        : cellsOf(bytes.length | 0x80, prefix);
    if (policy.enabled) {
      const seconds = Math.max(0, Math.floor((now - start) / 1_000));
      const globalRate = policy.classes[kind].global;
      // Retry-After covers every refusing bucket, not only the first one.
      const wait = Math.max(
        tierWait(state.client, cells, cost, seconds),
        state.prefix === undefined || prefixCells === undefined
          ? 0
          : tierWait(state.prefix, prefixCells, cost, seconds),
        state.global === undefined || globalRate === undefined
          ? 0
          : bucketWait(state.global, globalRate, cost, now),
      );
      if (wait > 0) return refusal(wait);
    }
    return {
      accepted: true,
      client: { public: true, kind, cost, cells, prefixCells },
      busy:
        Math.min(inFlight[cells[0]]!, inFlight[cells[1]]!) >= CLIENT_ACTIVE_MAX,
    };
  };

  const checkLiveness = (
    bytes: Uint8Array,
    kind: HttpRateClass,
    now: number,
  ): HttpRateCheck => {
    const key = Buffer.from(clientAndPrefix(bytes).client).toString("hex");
    const entry = liveness.get(key) ?? { active: 0, buckets: {} };
    if (policy.enabled) {
      const rate = policy.classes[kind].peer;
      const bucket = (entry.buckets[kind] ??= {
        tokens: rate.burst,
        updatedAt: now,
      });
      const wait = bucketWait(bucket, rate, 1, now);
      if (wait > 0) return refusal(wait);
    }
    return {
      accepted: true,
      client: { public: false, kind, key, entry },
      busy: entry.active >= CLIENT_ACTIVE_MAX,
    };
  };

  /**
   * Checks every bucket a request of `cost` draws from, without debiting.
   * `address` must be an IP address.
   */
  const check = (
    address: string,
    kind: HttpRateClass,
    cost: number,
    now: number,
  ): HttpRateCheck => {
    const bytes = httpAddressBytes(address);
    return isHttpPublicRateClass(kind)
      ? checkPublic(bytes, kind, cost, now)
      : checkLiveness(bytes, kind, now);
  };

  /** Debits the buckets of a checked request and counts it in flight. */
  const admit = (client: HttpRateClient): void => {
    if (!client.public) {
      // Re-inserting keeps the table in least-recently-used order. Refused
      // requests neither add a key nor refresh one. At most 72 requests are
      // in flight, so an idle key to drop always exists.
      liveness.delete(client.key);
      liveness.set(client.key, client.entry);
      if (liveness.size > HTTP_LIVENESS_MAX_ENTRIES)
        for (const [key, entry] of liveness)
          if (entry.active === 0) {
            liveness.delete(key);
            break;
          }
      client.entry.active += 1;
      if (policy.enabled) client.entry.buckets[client.kind]!.tokens -= 1;
      return;
    }
    for (const cell of client.cells) inFlight[cell] = inFlight[cell]! + 1;
    if (!policy.enabled) return;
    const state = tiers[client.kind];
    debit(state.client, client.cells, client.cost);
    if (state.prefix !== undefined && client.prefixCells !== undefined)
      debit(state.prefix, client.prefixCells, client.cost);
    if (state.global !== undefined) state.global.tokens -= client.cost;
  };

  /** Ends an admitted request's in-flight count. Call once per admit. */
  const release = (client: HttpRateClient): void => {
    if (!client.public) client.entry.active -= 1;
    else for (const cell of client.cells) inFlight[cell] = inFlight[cell]! - 1;
  };

  return {
    check,
    admit,
    release,
    snapshot: () => ({ livenessPeers: liveness.size }),
  };
};
