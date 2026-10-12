/**
 * The submit-slot snapshot shape and the Custom-network Lucid slot mapping
 * derived from Shelley genesis evidence, with the check that ties them
 * together. No network access: the Ogmios queries that read the evidence
 * live in `ogmios-slot-query.ts` (`@al-ft/midgard-core/ogmios-slot-query`),
 * a tools-only subpath that role processes never import.
 */
import type { SlotConfig } from "@lucid-evolution/lucid";

import {
  OgmiosSlotEvidenceUnavailableError,
  type ShelleyGenesisSlotConfig,
  type SubmitSlotSnapshot,
} from "./ogmios-slot.parse-ogmios-evidence.js";

export {
  assertNoOgmiosJsonRpcError,
  type FetchLike,
  normalizeOgmiosHttpUrl,
  ogmiosSlotEvidenceUnavailableCause,
  OgmiosSlotEvidenceUnavailableError,
  type OgmiosSlotEvidenceUnavailableReason,
  parseOgmiosHealthEvidence,
  parseOgmiosShelleyGenesisSlotConfig,
  parseOgmiosTipSlot,
  type ShelleyGenesisSlotConfig,
  type ShelleyGenesisSlotEvidence,
  type SubmitSlotSnapshot,
} from "./ogmios-slot.parse-ogmios-evidence.js";

export const SUBMIT_SLOT_LENGTH_MS = 1_000;
export const SUBMIT_SLOT_VALIDITY_BUFFER = 2;

/**
 * The tip-age bound for callers that do not derive one from genesis. The
 * node and the committee derive theirs with
 * {@link ogmiosTipMaxAgeMsFromShelleyGenesis}.
 */
export const DEFAULT_OGMIOS_HEALTH_MAX_AGE_MS = 120_000;

/** Expected block intervals a healthy tip may go without an update. */
export const DEFAULT_OGMIOS_TIP_MAX_AGE_BLOCK_INTERVALS = 10;

export type CustomSlotConfig = SlotConfig;

const assertValidSubmitSlotSnapshot = ({
  currentSlot,
  observedAtMs,
  slotLengthMs,
}: Pick<
  SubmitSlotSnapshot,
  "currentSlot" | "observedAtMs" | "slotLengthMs"
>) => {
  if (!Number.isSafeInteger(currentSlot) || currentSlot < 0) {
    throw new Error(
      `Invalid Custom slot snapshot currentSlot=${String(currentSlot)}`,
    );
  }
  if (!Number.isSafeInteger(observedAtMs) || observedAtMs < 0) {
    throw new Error(
      `Invalid Custom slot snapshot observedAtMs=${String(observedAtMs)}`,
    );
  }
  if (!Number.isSafeInteger(slotLengthMs) || slotLengthMs <= 0) {
    throw new Error(
      `Invalid Custom slot snapshot slotLengthMs=${String(slotLengthMs)}`,
    );
  }
};

const assertValidShelleyGenesis = (genesis: ShelleyGenesisSlotConfig) => {
  if (!Number.isSafeInteger(genesis.startTimeMs) || genesis.startTimeMs < 0) {
    throw new Error(
      `Invalid Shelley genesis startTimeMs=${String(genesis.startTimeMs)}`,
    );
  }
  if (
    !Number.isSafeInteger(genesis.slotLengthMs) ||
    genesis.slotLengthMs <= 0
  ) {
    throw new Error(
      `Invalid Shelley genesis slotLengthMs=${String(genesis.slotLengthMs)}`,
    );
  }
};

/**
 * The longest a healthy local Ogmios tip may go without an update: a number
 * of expected block intervals, each `slotLength / f` for the genesis
 * active-slot coefficient `f` (10 intervals of 20 s on a devnet with
 * `f = 1/20` and one-second slots is 200 s). A genesis without `f` keeps
 * {@link DEFAULT_OGMIOS_HEALTH_MAX_AGE_MS}.
 */
export const ogmiosTipMaxAgeMsFromShelleyGenesis = (
  genesis: ShelleyGenesisSlotConfig,
  blockIntervals: number = DEFAULT_OGMIOS_TIP_MAX_AGE_BLOCK_INTERVALS,
): number => {
  assertValidShelleyGenesis(genesis);
  if (!Number.isFinite(blockIntervals) || blockIntervals <= 0) {
    throw new Error(
      `Invalid Ogmios tip-age block interval count=${String(blockIntervals)}`,
    );
  }
  const coefficient = genesis.activeSlotsCoefficient;
  if (coefficient === undefined) {
    return DEFAULT_OGMIOS_HEALTH_MAX_AGE_MS;
  }
  return Math.ceil((blockIntervals * genesis.slotLengthMs) / coefficient);
};

/**
 * Derives Lucid's Custom slot mapping from the Shelley genesis alone. The
 * mapping is a pure function of the genesis epoch, so no tip evidence enters
 * it; the wall clock only confirms the epoch has begun (a transient
 * `genesis_not_started` until it has) and the slot length is the one the
 * submit-slot arithmetic assumes (terminal otherwise).
 */
export const customSlotConfigFromShelleyGenesisAtWallClock = (
  genesis: ShelleyGenesisSlotConfig,
  {
    nowMs,
    expectedSlotLengthMs = SUBMIT_SLOT_LENGTH_MS,
  }: { readonly nowMs: number; readonly expectedSlotLengthMs?: number },
): CustomSlotConfig => {
  assertValidShelleyGenesis(genesis);
  if (genesis.slotLengthMs !== expectedSlotLengthMs) {
    throw new Error(
      `Custom slot length disagreement: expected=${expectedSlotLengthMs.toString()},genesis=${genesis.slotLengthMs.toString()}`,
    );
  }
  if (nowMs < genesis.startTimeMs) {
    throw new OgmiosSlotEvidenceUnavailableError(
      "genesis_not_started",
      `The Shelley genesis start time is in the future: startTimeMs=${genesis.startTimeMs.toString()},nowMs=${nowMs.toString()}`,
    );
  }
  return {
    zeroTime: genesis.startTimeMs,
    zeroSlot: 0,
    slotLength: genesis.slotLengthMs,
  };
};

/**
 * Derives Lucid's Custom slot mapping from the authoritative Shelley genesis
 * epoch. The submit-slot snapshot remains a required health and clock-domain
 * check, but its wall-clock observation never defines a slot boundary.
 */
export const customSlotConfigFromShelleyGenesis = (
  genesis: ShelleyGenesisSlotConfig,
  snapshot: Pick<
    SubmitSlotSnapshot,
    "currentSlot" | "observedAtMs" | "slotLengthMs"
  >,
): CustomSlotConfig => {
  assertValidSubmitSlotSnapshot(snapshot);
  assertValidShelleyGenesis(genesis);
  if (snapshot.slotLengthMs !== genesis.slotLengthMs) {
    throw new Error(
      `Custom slot length disagreement: snapshot=${snapshot.slotLengthMs.toString()},genesis=${genesis.slotLengthMs.toString()}`,
    );
  }
  if (snapshot.observedAtMs < genesis.startTimeMs) {
    throw new OgmiosSlotEvidenceUnavailableError(
      "genesis_not_started",
      "Custom submit-slot observation precedes the Shelley genesis start time",
    );
  }
  const genesisSlotAtObservation = Math.floor(
    (snapshot.observedAtMs - genesis.startTimeMs) / genesis.slotLengthMs,
  );
  if (
    !Number.isSafeInteger(genesisSlotAtObservation) ||
    Math.abs(snapshot.currentSlot - genesisSlotAtObservation) >
      SUBMIT_SLOT_VALIDITY_BUFFER
  ) {
    throw new OgmiosSlotEvidenceUnavailableError(
      "ogmios_clock_disagreement",
      `Custom slot clock disagreement: snapshot=${snapshot.currentSlot.toString()},genesisAtObservation=${genesisSlotAtObservation.toString()}`,
    );
  }
  return {
    zeroTime: genesis.startTimeMs,
    zeroSlot: 0,
    slotLength: genesis.slotLengthMs,
  };
};
