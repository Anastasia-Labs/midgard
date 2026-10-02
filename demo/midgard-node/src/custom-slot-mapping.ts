/**
 * The node's Custom-network Lucid slot mapping and local-Ogmios tip-age
 * bound, resolved once per process tree.
 *
 * The mapping is a pure function of the Shelley genesis, so it is built from
 * the genesis alone and never waits on tip freshness. The main thread still
 * requires one healthy, clock-agreeing submit-slot snapshot before it
 * publishes the mapping, and waits (logging the unready reason) rather than
 * exits while the local Ogmios cannot give one. Worker threads inherit the
 * published mapping through the worker environment data and never re-query.
 */
import {
  getEnvironmentData,
  isMainThread,
  setEnvironmentData,
} from "node:worker_threads";

import {
  customSlotConfigFromShelleyGenesis,
  customSlotConfigFromShelleyGenesisAtWallClock,
  normalizeOgmiosHttpUrl,
  ogmiosSlotEvidenceUnavailableCause,
  ogmiosTipMaxAgeMsFromShelleyGenesis,
} from "@al-ft/midgard-core/ogmios-slot";
import type { SlotConfig } from "@lucid-evolution/lucid";
import { Duration, Effect, Schedule } from "effect";

import {
  type FetchLike,
  fetchLocalOgmiosShelleyGenesisSlotConfig,
  fetchLocalOgmiosSubmitSlotSnapshot,
  type SubmitSlotSnapshot,
} from "./local-ledger-slot.js";
import { runProviderStepWithRetry } from "./provider-retry.js";

export const CUSTOM_SLOT_MAPPING_ENVIRONMENT_KEY =
  "midgard.customLucidSlotMapping.v1";

export type CustomSlotMapping = {
  readonly version: 1;
  /** The normalized Ogmios HTTP URL the mapping was read from. */
  readonly ogmiosUrl: string;
  /** Absent on a named network, where Lucid's built-in mapping applies. */
  readonly slotConfig?: SlotConfig;
  /** The local-Ogmios tip-age bound submit-slot snapshots are held to. */
  readonly tipMaxAgeMs: number;
  readonly genesisConfigurationSha256: string;
};

export type CustomSlotMappingEnvironment = {
  readonly isMainThread: boolean;
  readonly get: (key: string) => unknown;
  readonly set: (key: string, value: unknown) => void;
};

const workerEnvironment: CustomSlotMappingEnvironment = {
  isMainThread,
  get: (key) => getEnvironmentData(key),
  set: (key, value) =>
    setEnvironmentData(key, value as Parameters<typeof setEnvironmentData>[1]),
};

export type ResolveCustomSlotMappingOptions = {
  readonly ogmiosUrl: string;
  readonly timeoutMs: number;
  /** Build Lucid's Custom mapping; false keeps the tip-age bound only. */
  readonly custom: boolean;
  /** Overrides the genesis-derived tip-age bound. */
  readonly tipMaxAgeMs?: number;
  readonly fetchImpl?: FetchLike;
  readonly retry?: {
    readonly baseDelayMs: number;
    readonly maxDelayMs: number;
  };
  readonly environment?: CustomSlotMappingEnvironment;
  readonly nowMs?: () => number;
};

const DEFAULT_RETRY = { baseDelayMs: 1_000, maxDelayMs: 15_000 } as const;

const positiveSafeInteger = (value: unknown): value is number =>
  typeof value === "number" && Number.isSafeInteger(value) && value > 0;

const sharedMapping = (
  value: unknown,
  ogmiosUrl: string,
  custom: boolean,
): CustomSlotMapping | undefined => {
  if (typeof value !== "object" || value === null) {
    return undefined;
  }
  const candidate = value as Partial<CustomSlotMapping>;
  const slotConfig = candidate.slotConfig;
  const slotConfigValid =
    slotConfig === undefined
      ? !custom
      : custom &&
        Number.isSafeInteger(slotConfig.zeroTime) &&
        slotConfig.zeroTime >= 0 &&
        Number.isSafeInteger(slotConfig.zeroSlot) &&
        slotConfig.zeroSlot >= 0 &&
        positiveSafeInteger(slotConfig.slotLength);
  return candidate.version === 1 &&
    candidate.ogmiosUrl === ogmiosUrl &&
    positiveSafeInteger(candidate.tipMaxAgeMs) &&
    typeof candidate.genesisConfigurationSha256 === "string" &&
    slotConfigValid
    ? (candidate as CustomSlotMapping)
    : undefined;
};

/**
 * Runs `step` until it succeeds or fails with something other than the
 * transient Ogmios slot-evidence class, logging each unready reason.
 */
const waitOutUnavailableSlotEvidence = <A>(
  label: string,
  step: Effect.Effect<A, Error>,
  retry: NonNullable<ResolveCustomSlotMappingOptions["retry"]>,
): Effect.Effect<A, Error> =>
  step.pipe(
    Effect.tapError((error) => {
      const unavailable = ogmiosSlotEvidenceUnavailableCause(error);
      return unavailable === undefined
        ? Effect.void
        : Effect.logWarning(
            `L1 slot evidence unready: reason=${unavailable.reason}; ${label} waits and re-reads. cause=${unavailable.message}`,
          );
    }),
    Effect.retry({
      schedule: Schedule.exponential(Duration.millis(retry.baseDelayMs)).pipe(
        Schedule.union(Schedule.spaced(Duration.millis(retry.maxDelayMs))),
      ),
      while: (error) => ogmiosSlotEvidenceUnavailableCause(error) !== undefined,
    }),
  );

const tryMapping = <A>(evaluate: () => A): Effect.Effect<A, Error> =>
  Effect.try({
    try: evaluate,
    catch: (cause) =>
      cause instanceof Error
        ? cause
        : new Error("Custom slot mapping check failed", { cause }),
  });

/**
 * Resolves the slot mapping and tip-age bound, reusing the one the main
 * thread published when it is for the same Ogmios. A malformed genesis, a
 * slot length other than the profile's, or any other non-transient Ogmios
 * answer fails at once.
 */
export const resolveCustomSlotMapping = (
  options: ResolveCustomSlotMappingOptions,
): Effect.Effect<CustomSlotMapping, Error> =>
  Effect.gen(function* () {
    const environment = options.environment ?? workerEnvironment;
    const retry = options.retry ?? DEFAULT_RETRY;
    const nowMs = options.nowMs ?? Date.now;
    const ogmiosUrl = normalizeOgmiosHttpUrl(options.ogmiosUrl);
    const inherited = sharedMapping(
      environment.get(CUSTOM_SLOT_MAPPING_ENVIRONMENT_KEY),
      ogmiosUrl,
      options.custom,
    );
    if (
      inherited !== undefined &&
      (options.tipMaxAgeMs === undefined ||
        options.tipMaxAgeMs === inherited.tipMaxAgeMs)
    ) {
      return inherited;
    }

    const genesis = yield* waitOutUnavailableSlotEvidence(
      "the Shelley genesis query",
      fetchLocalOgmiosShelleyGenesisSlotConfig({
        ogmiosUrl,
        timeoutMs: options.timeoutMs,
        fetchImpl: options.fetchImpl,
      }),
      retry,
    );
    const tipMaxAgeMs =
      options.tipMaxAgeMs ??
      (yield* tryMapping(() => ogmiosTipMaxAgeMsFromShelleyGenesis(genesis)));
    if (!positiveSafeInteger(tipMaxAgeMs)) {
      return yield* Effect.fail(
        new Error(`Invalid Ogmios tip-age bound: ${String(tipMaxAgeMs)}`),
      );
    }
    const slotConfig = options.custom
      ? yield* waitOutUnavailableSlotEvidence(
          "the Custom slot mapping",
          tryMapping(() =>
            customSlotConfigFromShelleyGenesisAtWallClock(genesis, {
              nowMs: nowMs(),
            }),
          ),
          retry,
        )
      : undefined;

    if (options.custom && environment.isMainThread) {
      // One healthy, clock-agreeing snapshot confirms the local Ogmios is on
      // the genesis clock before the mapping is published.
      yield* waitOutUnavailableSlotEvidence(
        "the Custom slot clock check",
        Effect.suspend(() =>
          fetchLocalOgmiosSubmitSlotSnapshot({
            ogmiosUrl,
            timeoutMs: options.timeoutMs,
            fetchImpl: options.fetchImpl,
            nowMs: nowMs(),
            maxHealthAgeMs: tipMaxAgeMs,
          }),
        ).pipe(
          Effect.flatMap((snapshot) =>
            tryMapping(() =>
              customSlotConfigFromShelleyGenesis(genesis, snapshot),
            ),
          ),
        ),
        retry,
      );
    }

    const mapping: CustomSlotMapping = {
      version: 1,
      ogmiosUrl,
      ...(slotConfig === undefined ? {} : { slotConfig }),
      tipMaxAgeMs,
      genesisConfigurationSha256: genesis.configurationSha256,
    };
    if (environment.isMainThread) {
      environment.set(CUSTOM_SLOT_MAPPING_ENVIRONMENT_KEY, mapping);
    }
    yield* Effect.logInfo(
      `L1 slot mapping resolved: ogmiosTipMaxAgeMs=${tipMaxAgeMs.toString()}${
        slotConfig === undefined
          ? ""
          : `,zeroTime=${slotConfig.zeroTime.toString()},slotLength=${slotConfig.slotLength.toString()}`
      }`,
    );
    return mapping;
  });

export type SubmitSlotSnapshotRetryOptions = {
  readonly maxAttempts: number;
  readonly baseDelayMs: number;
  readonly maxDelayMs: number;
};

/**
 * A short bounded wait for submit time: a block gap that crosses the bound
 * often clears within seconds. Past the last attempt the stale snapshot is
 * still refused; the caller never builds against an unvouched clock.
 */
export const DEFAULT_SUBMIT_SLOT_SNAPSHOT_RETRY: SubmitSlotSnapshotRetryOptions =
  { maxAttempts: 4, baseDelayMs: 1_000, maxDelayMs: 5_000 };

export const retryTransientSubmitSlotSnapshot = (
  readOnce: () => Effect.Effect<SubmitSlotSnapshot, Error>,
  retry: SubmitSlotSnapshotRetryOptions = DEFAULT_SUBMIT_SLOT_SNAPSHOT_RETRY,
): Effect.Effect<SubmitSlotSnapshot, Error> =>
  runProviderStepWithRetry(
    "Local Ogmios submit-slot snapshot",
    Effect.suspend(readOnce),
    {
      ...retry,
      isRetryable: (error) =>
        ogmiosSlotEvidenceUnavailableCause(error) !== undefined,
    },
  );
