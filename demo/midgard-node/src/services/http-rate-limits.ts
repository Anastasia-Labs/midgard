/**
 * Request-rate policy for the node's HTTP ingress: token buckets per client
 * and class, a looser bucket per address prefix for submissions, and a global
 * bucket for the other public classes.
 */

/**
 * `health` (`/healthz`, `/readyz`), `admin` (operator routes) and `peer` (the
 * operator-to-operator sharing routes `/peer/txs` and `/peer/inventory`) are
 * the liveness classes: they have no global bucket, so a public flood cannot
 * starve them, and their few clients live in a table of their own. A `peer`
 * request is charged before its signature is checked, so a shared bucket
 * would let unsigned traffic refuse the operators. `batch` is
 * `POST /tx-status`; `POST /utxos` stays `read` because the CLI pages it at
 * about eight requests a second.
 */
export const HTTP_RATE_CLASSES = [
  "health",
  "admin",
  "read",
  "batch",
  "write",
  "evaluate",
  "peer",
] as const;
export type HttpRateClass = (typeof HTTP_RATE_CLASSES)[number];
/** Classes whose client buckets live in the fixed-size hashed arrays. */
export const HTTP_PUBLIC_RATE_CLASSES = [
  "read",
  "batch",
  "write",
  "evaluate",
] as const satisfies readonly HttpRateClass[];
export type HttpPublicRateClass = (typeof HTTP_PUBLIC_RATE_CLASSES)[number];
export const isHttpPublicRateClass = (
  kind: HttpRateClass,
): kind is HttpPublicRateClass =>
  (HTTP_PUBLIC_RATE_CLASSES as readonly HttpRateClass[]).includes(kind);

export type HttpRate = { readonly burst: number; readonly perSecond: number };
/**
 * `peer` is the per-client bucket: one IPv4 address or one IPv6 /56. `prefix`
 * is shared by every client in the same IPv4 /24 or IPv6 /48, so a holder of
 * a whole prefix cannot multiply its quota by spreading over its addresses.
 * `global` is shared by every client. Only public classes take `prefix` or
 * `global`.
 */
export type HttpRateClassPolicy = {
  readonly peer: HttpRate;
  readonly prefix?: HttpRate;
  readonly global?: HttpRate;
};
export type HttpRateLimitPolicy = {
  readonly enabled: boolean;
  readonly classes: Readonly<Record<HttpRateClass, HttpRateClassPolicy>>;
};

/**
 * Public-profile defaults. A submission is charged per transaction: 20 a
 * second per client with a burst of 100, and 200 a second per /24 or /48. It
 * has no global bucket; load shedding on queue state is the global bound.
 */
export const DEFAULT_HTTP_RATE_LIMITS: HttpRateLimitPolicy = {
  enabled: true,
  classes: {
    health: { peer: { burst: 20, perSecond: 5 } },
    admin: { peer: { burst: 30, perSecond: 10 } },
    read: {
      peer: { burst: 30, perSecond: 10 },
      global: { burst: 200, perSecond: 100 },
    },
    batch: {
      peer: { burst: 4, perSecond: 1 },
      global: { burst: 16, perSecond: 4 },
    },
    write: {
      peer: { burst: 100, perSecond: 20 },
      prefix: { burst: 1_000, perSecond: 200 },
    },
    evaluate: {
      peer: { burst: 2, perSecond: 0.2 },
      global: { burst: 8, perSecond: 1 },
    },
    peer: { peer: { burst: 8, perSecond: 4 } },
  },
};
