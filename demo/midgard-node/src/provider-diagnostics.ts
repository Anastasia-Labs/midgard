import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type { Network } from "@lucid-evolution/lucid";

export type L1ProviderSource = "kupmios";

export type L1ProviderFailoverSource = never;

export const L1_REWARD_ACCOUNT_REGISTRATION_SOURCES =
  "__midgardRewardAccountRegistrationSources";

export type L1RewardAccountRegistrationSource = {
  readonly kind: "ogmios";
  readonly source: "kupmios";
  readonly url: string;
  readonly headers?: Record<string, string>;
};

export type L1ProviderQueryKind =
  | "provider-health"
  | "reward-account-registration"
  | "protocol-parameters"
  | "utxos-at"
  | "utxos-by-outref"
  | "datum"
  | "evaluate-tx"
  | "submit-tx"
  | "await-tx"
  | "tx-confirmation";

export type L1ProviderFailureKind =
  | "rate_limited"
  | "html_response"
  | "malformed_json"
  | "network_error"
  | "server_error"
  | "recent_tx_not_found"
  | "permanent_not_found"
  | "unsupported"
  | "ambiguous_submit"
  | "unknown";

export type ProviderFailureClassification = {
  readonly kind: L1ProviderFailureKind;
  readonly retryable: boolean;
  readonly rateLimitEligible: boolean;
  readonly status?: number;
  readonly retryAfterMs?: number;
  readonly summary: string;
};

export type ProviderCooldown = {
  readonly source: L1ProviderSource;
  readonly reason: L1ProviderFailureKind;
  readonly startedAtMs: number;
  readonly retryAtMs: number;
};

const cooldowns = new Map<L1ProviderSource, ProviderCooldown>();

const HTML_PREFIX_PATTERN =
  /^\s*<(?:!doctype\s+html|html|body|head|h\d|p|div)\b/i;

export const redactEndpoint = (endpoint: string): string => {
  try {
    const url = new URL(endpoint);
    url.username = "";
    url.password = "";
    url.search = "";
    url.hash = "";
    return url.toString().replace(/\/$/, "");
  } catch {
    return endpoint.replace(/[?].*$/, "");
  }
};

export const summarizeProviderBody = (
  body: string,
  maxLength = 240,
): string => {
  const normalized = body.replace(/\s+/g, " ").trim();
  if (normalized.length <= maxLength) {
    return normalized;
  }
  return `${normalized.slice(0, maxLength)}...`;
};

export const parseRetryAfterMs = (
  value: string | null | undefined,
  nowMs: number = Date.now(),
): number | undefined => {
  if (value === undefined || value === null || value.trim().length === 0) {
    return undefined;
  }
  const trimmed = value.trim();
  if (/^\d+$/.test(trimmed)) {
    return Number(trimmed) * 1000;
  }
  const retryAtMs = Date.parse(trimmed);
  if (Number.isNaN(retryAtMs)) {
    return undefined;
  }
  return Math.max(0, retryAtMs - nowMs);
};

export const classifyProviderHttpResponse = ({
  status,
  body,
  retryAfter,
  knownRecentTx = false,
}: {
  readonly status: number;
  readonly body: string;
  readonly retryAfter?: string | null;
  readonly knownRecentTx?: boolean;
}): ProviderFailureClassification => {
  const summary = summarizeProviderBody(body);
  const retryAfterMs = parseRetryAfterMs(retryAfter);
  if (status === 402 || status === 429) {
    return {
      kind: "rate_limited",
      retryable: true,
      rateLimitEligible: true,
      status,
      retryAfterMs,
      summary,
    };
  }
  if (HTML_PREFIX_PATTERN.test(body)) {
    return {
      kind: "html_response",
      retryable: true,
      rateLimitEligible: status >= 400,
      status,
      retryAfterMs,
      summary,
    };
  }
  if (status === 404) {
    return {
      kind: knownRecentTx ? "recent_tx_not_found" : "permanent_not_found",
      retryable: knownRecentTx,
      rateLimitEligible: false,
      status,
      retryAfterMs,
      summary,
    };
  }
  if (status >= 500) {
    return {
      kind: "server_error",
      retryable: true,
      rateLimitEligible: false,
      status,
      retryAfterMs,
      summary,
    };
  }
  if (status >= 400) {
    return {
      kind: "unknown",
      retryable: false,
      rateLimitEligible: false,
      status,
      retryAfterMs,
      summary,
    };
  }
  return {
    kind: "unknown",
    retryable: false,
    rateLimitEligible: false,
    status,
    retryAfterMs,
    summary,
  };
};

export const classifyProviderError = (
  error: unknown,
): ProviderFailureClassification => {
  const message = formatUnknownError(error, { includeCause: true });
  const normalized = message.toLowerCase();
  const statusMatch =
    /status[_\s-]*code["']?\s*[:=]\s*(\d{3})/i.exec(message) ??
    /status\s+(\d{3})/i.exec(message);
  const status =
    statusMatch === null ? undefined : Number.parseInt(statusMatch[1], 10);
  if (
    status === 402 ||
    status === 429 ||
    normalized.includes("project over limit") ||
    normalized.includes("usage is over limit") ||
    normalized.includes("too many requests") ||
    normalized.includes("rate limit")
  ) {
    return {
      kind: "rate_limited",
      retryable: true,
      rateLimitEligible: true,
      ...(status === undefined ? {} : { status }),
      summary: summarizeProviderBody(message),
    };
  }
  if (
    normalized.includes("unexpected token '<'") ||
    normalized.includes("<html") ||
    normalized.includes("is not valid json")
  ) {
    return {
      kind: normalized.includes("html") ? "html_response" : "malformed_json",
      retryable: true,
      rateLimitEligible: true,
      ...(status === undefined ? {} : { status }),
      summary: summarizeProviderBody(message),
    };
  }
  if (
    normalized.includes("fetch failed") ||
    normalized.includes("econnreset") ||
    normalized.includes("econnrefused") ||
    normalized.includes("socket") ||
    normalized.includes("network")
  ) {
    return {
      kind: "network_error",
      retryable: true,
      rateLimitEligible: false,
      ...(status === undefined ? {} : { status }),
      summary: summarizeProviderBody(message),
    };
  }
  if (status !== undefined && status >= 500) {
    return {
      kind: "server_error",
      retryable: true,
      rateLimitEligible: false,
      status,
      summary: summarizeProviderBody(message),
    };
  }
  return {
    kind: "unknown",
    retryable: false,
    rateLimitEligible: false,
    ...(status === undefined ? {} : { status }),
    summary: summarizeProviderBody(message),
  };
};

export const markProviderCooldown = ({
  source,
  reason,
  cooldownMs,
  nowMs = Date.now(),
}: {
  readonly source: L1ProviderSource;
  readonly reason: L1ProviderFailureKind;
  readonly cooldownMs: number;
  readonly nowMs?: number;
}): ProviderCooldown => {
  const cooldown = {
    source,
    reason,
    startedAtMs: nowMs,
    retryAtMs: nowMs + Math.max(0, Math.floor(cooldownMs)),
  };
  cooldowns.set(source, cooldown);
  return cooldown;
};

export const getProviderCooldown = (
  source: L1ProviderSource,
  nowMs: number = Date.now(),
): ProviderCooldown | undefined => {
  const cooldown = cooldowns.get(source);
  if (cooldown === undefined) {
    return undefined;
  }
  if (cooldown.retryAtMs <= nowMs) {
    cooldowns.delete(source);
    return undefined;
  }
  return cooldown;
};

export const clearProviderCooldownsForTest = (): void => {
  cooldowns.clear();
};

export const parseProviderFailoverSources = (
  value: string,
): readonly L1ProviderFailoverSource[] => {
  const trimmed = value.trim();
  if (trimmed.length === 0) {
    return [];
  }
  throw new Error(
    "L1_PROVIDER_FAILOVER is no longer supported for demo/midgard-node acceptance; use local Kupmios only.",
  );
};

export const providerRouteSummary = ({
  network,
}: {
  readonly provider: "Kupmios";
  readonly network: Network;
}): {
  readonly primary: L1ProviderSource;
  readonly failover: readonly L1ProviderFailoverSource[];
  readonly network: Network;
} => ({
  primary: "kupmios",
  failover: [],
  network,
});
