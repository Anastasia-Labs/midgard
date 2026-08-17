import type { DaPayloadEmissionMode } from "@al-ft/midgard-core/da-payload-sizing";

import { positiveSafeInteger } from "@/artifact-schema.js";

export type DaPayloadEnvelopeMode = DaPayloadEmissionMode;

export type DaHardeningConfig = {
  readonly envelopeMode: DaPayloadEnvelopeMode;
  readonly zstdLevel: number;
  readonly publishConcurrency: number;
  readonly reconcileIntervalMs: number;
  readonly retryBackoffMs: number;
  readonly retryBackoffMaxMs: number;
};

const positiveInteger = (
  value: string | undefined,
  fallback: number,
  name: string,
): number => {
  if (value === undefined || value.trim().length === 0) {
    return fallback;
  }
  return positiveSafeInteger(Number(value), name);
};

export const readDaHardeningConfig = (
  env: NodeJS.ProcessEnv = process.env,
): DaHardeningConfig => {
  const rawMode = env.MIDGARD_DA_PAYLOAD_ENVELOPE?.trim() || "identity";
  if (rawMode !== "identity" && rawMode !== "zstd") {
    throw new Error("MIDGARD_DA_PAYLOAD_ENVELOPE must be identity or zstd");
  }
  const zstdLevel = positiveInteger(
    env.MIDGARD_DA_ZSTD_LEVEL,
    3,
    "MIDGARD_DA_ZSTD_LEVEL",
  );
  if (zstdLevel > 19) {
    throw new Error("MIDGARD_DA_ZSTD_LEVEL must be an integer in [1, 19]");
  }
  const retryBackoffMs = positiveInteger(
    env.MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS,
    30_000,
    "MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS",
  );
  const retryBackoffMaxMs = positiveInteger(
    env.MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS,
    300_000,
    "MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS",
  );
  if (retryBackoffMaxMs < retryBackoffMs) {
    throw new Error(
      "MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS must be >= MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS",
    );
  }
  return {
    envelopeMode: rawMode,
    zstdLevel,
    publishConcurrency: positiveInteger(
      env.MIDGARD_DA_PUBLISH_CONCURRENCY,
      8,
      "MIDGARD_DA_PUBLISH_CONCURRENCY",
    ),
    reconcileIntervalMs: positiveInteger(
      env.MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS,
      30_000,
      "MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS",
    ),
    retryBackoffMs,
    retryBackoffMaxMs,
  };
};
