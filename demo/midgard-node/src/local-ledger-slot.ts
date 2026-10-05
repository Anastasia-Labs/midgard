import {
  type LocalOgmiosShelleyGenesisSlotOptions,
  type LocalOgmiosSubmitSlotOptions,
  queryLocalOgmiosShelleyGenesisSlotConfig,
  queryLocalOgmiosSubmitSlotSnapshot,
  type ShelleyGenesisSlotEvidence,
  type SubmitSlotSnapshot,
} from "@al-ft/midgard-core/ogmios-slot";
import { Effect } from "effect";

export {
  type FetchLike,
  type LocalOgmiosShelleyGenesisSlotOptions,
  type LocalOgmiosSubmitSlotOptions,
  normalizeOgmiosHttpUrl,
  ogmiosSlotEvidenceUnavailableCause,
  OgmiosSlotEvidenceUnavailableError,
  type OgmiosSlotEvidenceUnavailableReason,
  parseOgmiosHealthEvidence,
  parseOgmiosShelleyGenesisSlotConfig,
  parseOgmiosTipSlot,
  queryLocalOgmiosShelleyGenesisSlotConfig,
  queryLocalOgmiosSubmitSlotSnapshot,
  type ShelleyGenesisSlotConfig,
  type ShelleyGenesisSlotEvidence,
  SUBMIT_SLOT_LENGTH_MS,
  SUBMIT_SLOT_VALIDITY_BUFFER,
  type SubmitSlotSnapshot,
} from "@al-ft/midgard-core/ogmios-slot";

export const fetchLocalOgmiosSubmitSlotSnapshot = (
  options: LocalOgmiosSubmitSlotOptions,
): Effect.Effect<SubmitSlotSnapshot, Error> =>
  Effect.tryPromise({
    try: (effectSignal) =>
      queryLocalOgmiosSubmitSlotSnapshot({
        ...options,
        signal:
          options.signal === undefined
            ? effectSignal
            : AbortSignal.any([options.signal, effectSignal]),
      }),
    catch: (cause) =>
      cause instanceof Error
        ? cause
        : new Error("Failed to fetch local Ogmios submit slot", { cause }),
  });

export const fetchLocalOgmiosShelleyGenesisSlotConfig = (
  options: LocalOgmiosShelleyGenesisSlotOptions,
): Effect.Effect<ShelleyGenesisSlotEvidence, Error> =>
  Effect.tryPromise({
    try: (effectSignal) =>
      queryLocalOgmiosShelleyGenesisSlotConfig({
        ...options,
        signal:
          options.signal === undefined
            ? effectSignal
            : AbortSignal.any([options.signal, effectSignal]),
      }),
    catch: (cause) =>
      cause instanceof Error
        ? cause
        : new Error("Failed to fetch local Ogmios Shelley genesis", {
            cause,
          }),
  });

export const makeLocalOgmiosSubmitSlotSnapshotProvider = (
  options: Omit<LocalOgmiosSubmitSlotOptions, "nowMs">,
): (() => Effect.Effect<SubmitSlotSnapshot, unknown>) => {
  return () =>
    fetchLocalOgmiosSubmitSlotSnapshot({ ...options, nowMs: Date.now() });
};

export const localOgmiosSubmitSlotEvidence = (
  snapshot: SubmitSlotSnapshot,
): string => {
  const health = snapshot.health;
  return [
    `submitSlot=${snapshot.currentSlot.toString()}`,
    `slotSource=${snapshot.source}`,
    `observedAtMs=${snapshot.observedAtMs.toString()}`,
    ...(health?.connectionStatus === undefined
      ? []
      : [`connectionStatus=${health.connectionStatus}`]),
    ...(health?.networkSynchronization === undefined
      ? []
      : [`networkSynchronization=${health.networkSynchronization.toString()}`]),
    ...(health?.lastKnownTipSlot === undefined
      ? []
      : [`lastKnownTipSlot=${health.lastKnownTipSlot.toString()}`]),
    ...(health?.lastTipUpdate === undefined
      ? []
      : [`lastTipUpdate=${health.lastTipUpdate}`]),
  ].join(",");
};
