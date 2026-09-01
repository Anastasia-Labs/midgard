import { Effect } from "effect";

import {
  localOgmiosSubmitSlotEvidence,
  readLocalOgmiosSubmitSlot,
  type SubmitSlotSnapshot,
} from "@/local-ogmios-slot.js";
import {
  classifyProviderHttpResponse,
  getProviderCooldown,
  type L1ProviderSource,
  markProviderCooldown,
  providerRouteSummary,
  redactEndpoint,
  summarizeProviderBody,
} from "@/provider-diagnostics.js";

export type L1ProviderPreflightConfig = {
  readonly L1_PROVIDER: "Kupmios";
  readonly L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: number;
  readonly L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS: number;
  readonly L1_OGMIOS_KEY: string;
  readonly L1_KUPO_KEY: string;
  readonly NETWORK: "Mainnet" | "Preprod" | "Preview" | "Custom";
};

export type L1ProviderHealth = {
  readonly source: L1ProviderSource;
  readonly endpoint: string;
  readonly healthy: boolean;
  readonly degraded: boolean;
  readonly latencyMs?: number;
  readonly status?: number;
  readonly failureKind?: string;
  readonly bodySummary?: string;
  readonly localLedgerSlot?: SubmitSlotSnapshot;
  readonly cooldown?: {
    readonly reason: string;
    readonly retryAtMs: number;
    readonly remainingMs: number;
  };
};

export type L1ProviderPreflightReport = {
  readonly ok: boolean;
  readonly degraded: boolean;
  readonly route: ReturnType<typeof providerRouteSummary>;
  readonly checkedAtMs: number;
  readonly healthySources: readonly L1ProviderSource[];
  readonly unhealthySources: readonly L1ProviderSource[];
  readonly sources: readonly L1ProviderHealth[];
};

type FetchLike = (input: string, init?: RequestInit) => Promise<Response>;

const summarizeFetchFailure = (cause: unknown): string => {
  const primary =
    cause instanceof Error ? `${cause.name}: ${cause.message}` : String(cause);
  const nested = cause instanceof Error ? cause.cause : undefined;
  return summarizeProviderBody(
    nested === undefined
      ? primary
      : `${primary}; cause=${JSON.stringify(nested) ?? "undefined"}`,
  );
};

const joinUrl = (base: string, path: string): string =>
  `${base.replace(/\/+$/, "")}/${path.replace(/^\/+/, "")}`;

const fetchWithTimeout = async (
  fetchImpl: FetchLike,
  url: string,
  init: RequestInit,
  timeoutMs: number,
): Promise<Response> => {
  const controller = new AbortController();
  const upstreamSignal = init.signal;
  const abortFromUpstream = () => controller.abort(upstreamSignal?.reason);
  if (upstreamSignal?.aborted === true) {
    abortFromUpstream();
  } else {
    upstreamSignal?.addEventListener("abort", abortFromUpstream, {
      once: true,
    });
  }
  const timeout = setTimeout(() => controller.abort(), timeoutMs);
  try {
    return await fetchImpl(url, { ...init, signal: controller.signal });
  } finally {
    clearTimeout(timeout);
    upstreamSignal?.removeEventListener("abort", abortFromUpstream);
  }
};

const checkHttpOk = async ({
  fetchImpl,
  url,
  init,
  source,
  endpoint,
  timeoutMs,
  cooldownMs,
  nowMs,
}: {
  readonly fetchImpl: FetchLike;
  readonly url: string;
  readonly init: RequestInit;
  readonly source: L1ProviderSource;
  readonly endpoint: string;
  readonly timeoutMs: number;
  readonly cooldownMs: number;
  readonly nowMs: number;
}): Promise<L1ProviderHealth> => {
  const existingCooldown = getProviderCooldown(source, nowMs);
  if (existingCooldown !== undefined) {
    return {
      source,
      endpoint: redactEndpoint(endpoint),
      healthy: false,
      degraded: true,
      failureKind: "rate_limited",
      cooldown: {
        reason: existingCooldown.reason,
        retryAtMs: existingCooldown.retryAtMs,
        remainingMs: existingCooldown.retryAtMs - nowMs,
      },
    };
  }

  const startedAt = Date.now();
  try {
    const response = await fetchWithTimeout(fetchImpl, url, init, timeoutMs);
    const body = await response.text();
    const latencyMs = Date.now() - startedAt;
    if (!response.ok) {
      const classification = classifyProviderHttpResponse({
        status: response.status,
        body,
        retryAfter: response.headers.get("retry-after"),
      });
      if (classification.rateLimitEligible) {
        const cooldown = markProviderCooldown({
          source,
          reason: classification.kind,
          cooldownMs: classification.retryAfterMs ?? cooldownMs,
          nowMs,
        });
        return {
          source,
          endpoint: redactEndpoint(endpoint),
          healthy: false,
          degraded: true,
          latencyMs,
          status: response.status,
          failureKind: classification.kind,
          bodySummary: classification.summary,
          cooldown: {
            reason: cooldown.reason,
            retryAtMs: cooldown.retryAtMs,
            remainingMs: cooldown.retryAtMs - nowMs,
          },
        };
      }
      return {
        source,
        endpoint: redactEndpoint(endpoint),
        healthy: false,
        degraded: false,
        latencyMs,
        status: response.status,
        failureKind: classification.kind,
        bodySummary: classification.summary,
      };
    }
    return {
      source,
      endpoint: redactEndpoint(endpoint),
      healthy: true,
      degraded: false,
      latencyMs,
      status: response.status,
      ...(body.trim().length === 0
        ? {}
        : { bodySummary: summarizeProviderBody(body) }),
    };
  } catch (cause) {
    return {
      source,
      endpoint: redactEndpoint(endpoint),
      healthy: false,
      degraded: false,
      latencyMs: Date.now() - startedAt,
      failureKind: "network_error",
      bodySummary: summarizeFetchFailure(cause),
    };
  }
};

const checkKupmios = async (
  config: L1ProviderPreflightConfig,
  fetchImpl: FetchLike,
  nowMs: number,
  signal?: AbortSignal,
): Promise<L1ProviderHealth> => {
  const kupo = await checkHttpOk({
    fetchImpl,
    source: "kupmios",
    endpoint: config.L1_KUPO_KEY,
    url: joinUrl(config.L1_KUPO_KEY, "/health"),
    init: { signal },
    timeoutMs: config.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
    cooldownMs: config.L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS,
    nowMs,
  });
  if (!kupo.healthy) {
    return kupo;
  }
  const ogmios = await checkHttpOk({
    fetchImpl,
    source: "kupmios",
    endpoint: config.L1_OGMIOS_KEY,
    url: joinUrl(config.L1_OGMIOS_KEY, "/health"),
    init: { signal },
    timeoutMs: config.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
    cooldownMs: config.L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS,
    nowMs,
  });
  if (!ogmios.healthy) {
    return {
      ...ogmios,
      endpoint: `${redactEndpoint(config.L1_KUPO_KEY)},${redactEndpoint(
        config.L1_OGMIOS_KEY,
      )}`,
    };
  }
  let slotSnapshot: SubmitSlotSnapshot;
  try {
    slotSnapshot = await Effect.runPromise(
      readLocalOgmiosSubmitSlot({
        ogmiosUrl: config.L1_OGMIOS_KEY,
        fetchImpl,
        nowMs,
        timeoutMs: config.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
        signal,
      }),
    );
  } catch (cause) {
    return {
      ...ogmios,
      healthy: false,
      degraded: false,
      endpoint: `${redactEndpoint(config.L1_KUPO_KEY)},${redactEndpoint(
        config.L1_OGMIOS_KEY,
      )}`,
      failureKind: "local_ogmios_slot_unavailable",
      bodySummary: summarizeProviderBody(String(cause)),
    };
  }
  return {
    ...ogmios,
    endpoint: `${redactEndpoint(config.L1_KUPO_KEY)},${redactEndpoint(
      config.L1_OGMIOS_KEY,
    )}`,
    localLedgerSlot: slotSnapshot,
    bodySummary: [
      ogmios.bodySummary,
      localOgmiosSubmitSlotEvidence(slotSnapshot),
    ]
      .filter((part): part is string => part !== undefined && part.length > 0)
      .join("; "),
  };
};

export const runL1ProviderPreflight = async ({
  config,
  fetchImpl = fetch,
  nowMs = Date.now(),
  signal,
}: {
  readonly config: L1ProviderPreflightConfig;
  readonly fetchImpl?: FetchLike;
  readonly nowMs?: number;
  readonly signal?: AbortSignal;
}): Promise<L1ProviderPreflightReport> => {
  const checks: Promise<L1ProviderHealth>[] = [
    checkKupmios(config, fetchImpl, nowMs, signal),
  ];

  const sources = await Promise.all(checks);
  const healthySources = sources
    .filter((source) => source.healthy)
    .map((source) => source.source);
  const unhealthySources = sources
    .filter((source) => !source.healthy)
    .map((source) => source.source);
  const primaryHealthy = healthySources.includes("kupmios");
  return {
    ok: healthySources.length > 0,
    degraded: !primaryHealthy && healthySources.length > 0,
    route: providerRouteSummary({
      provider: config.L1_PROVIDER,
      network: config.NETWORK,
    }),
    checkedAtMs: nowMs,
    healthySources,
    unhealthySources,
    sources,
  };
};
