/**
 * Request-rate policy for the node's HTTP ingress: one token bucket per client
 * and class, plus one global bucket per public class.
 */

/**
 * `health` (`/healthz`, `/readyz`) and `admin` (operator routes) have no global
 * bucket, so a public flood cannot starve them; their clients live in a table
 * of their own. `batch` is `POST /tx-status`; `POST /utxos` stays `read`
 * because the CLI pages it at about eight requests a second.
 */
export const HTTP_RATE_CLASSES = [
  "health",
  "admin",
  "read",
  "batch",
  "write",
  "evaluate",
] as const;
export type HttpRateClass = (typeof HTTP_RATE_CLASSES)[number];
export const HTTP_GLOBAL_RATE_CLASSES = [
  "read",
  "batch",
  "write",
  "evaluate",
] as const satisfies readonly HttpRateClass[];

export type HttpRate = { readonly burst: number; readonly perSecond: number };
export type HttpRateClassPolicy = {
  readonly peer: HttpRate;
  readonly global?: HttpRate;
};
export type HttpRateLimitPolicy = {
  readonly enabled: boolean;
  readonly classes: Readonly<Record<HttpRateClass, HttpRateClassPolicy>>;
};

/**
 * Public-profile defaults. Accepted requests are what allocate client entries,
 * so the global buckets bound how many entries one idle window (60 s) can
 * create: 6,200 + 256 + 256 + 68 = 6,780, under the 8,192-entry table.
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
      peer: { burst: 4, perSecond: 1 },
      global: { burst: 16, perSecond: 4 },
    },
    evaluate: {
      peer: { burst: 2, perSecond: 0.2 },
      global: { burst: 8, perSecond: 1 },
    },
  },
};
