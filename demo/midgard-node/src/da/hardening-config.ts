import type { DaPayloadEmissionMode } from "@al-ft/midgard-core/da-payload-sizing";
import { Config } from "effect";

import { positiveSafeInteger } from "../artifact-schema.js";

/** The publication fan-out when `MIDGARD_DA_PUBLISH_CONCURRENCY` is unset. */
export const DEFAULT_DA_PUBLISH_CONCURRENCY = 8;

/** A blank or unset `name` takes `fallback`. */
const positiveInteger = (name: string, fallback: number) =>
  Config.string(name).pipe(
    Config.withDefault(""),
    Config.mapAttempt((value) =>
      value.trim().length === 0
        ? fallback
        : positiveSafeInteger(Number(value), name),
    ),
  );

/**
 * The DA payload envelope and publication settings. `makeConfig` parses them
 * once with the rest of the node configuration. The other DA settings (the
 * committee, owners, threshold and cosigner) are in `deploymentConfig`,
 * src/services/config.deployment.ts.
 */
export const daHardeningConfig = Config.all({
  MIDGARD_DA_PAYLOAD_ENVELOPE: Config.string(
    "MIDGARD_DA_PAYLOAD_ENVELOPE",
  ).pipe(
    Config.withDefault(""),
    Config.mapAttempt((value): DaPayloadEmissionMode => {
      const mode = value.trim() || "identity";
      if (mode !== "identity" && mode !== "zstd") {
        throw new Error("MIDGARD_DA_PAYLOAD_ENVELOPE must be identity or zstd");
      }
      return mode;
    }),
  ),
  MIDGARD_DA_ZSTD_LEVEL: positiveInteger("MIDGARD_DA_ZSTD_LEVEL", 3).pipe(
    Config.validate({
      message: "MIDGARD_DA_ZSTD_LEVEL must be an integer in [1, 19]",
      validation: (level) => level <= 19,
    }),
  ),
  MIDGARD_DA_PUBLISH_CONCURRENCY: positiveInteger(
    "MIDGARD_DA_PUBLISH_CONCURRENCY",
    DEFAULT_DA_PUBLISH_CONCURRENCY,
  ),
  MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS: positiveInteger(
    "MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS",
    30_000,
  ),
  MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS: positiveInteger(
    "MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS",
    30_000,
  ),
  MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS: positiveInteger(
    "MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS",
    300_000,
  ),
}).pipe(
  Config.validate({
    message:
      "MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS must be >= MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS",
    validation: (settings) =>
      settings.MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS >=
      settings.MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS,
  }),
);
