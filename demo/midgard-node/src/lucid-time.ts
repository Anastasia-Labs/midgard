/**
 * Canonical Lucid slot/unix-time conversion helpers for the node.
 * This module centralizes network-aware time conversion and emulator fallback
 * behavior so transaction code does not guess about clock semantics.
 */
import {
  type LucidEvolution,
  type Network,
  SLOT_CONFIG_NETWORK,
  slotToUnixTime,
} from "@lucid-evolution/lucid";

import {
  type ShelleyGenesisSlotConfig,
  SUBMIT_SLOT_LENGTH_MS,
  SUBMIT_SLOT_VALIDITY_BUFFER,
  type SubmitSlotSnapshot,
} from "@/local-ledger-slot.js";
export type CustomSlotConfig = {
  readonly zeroTime: number;
  readonly zeroSlot: number;
  readonly slotLength: number;
};

const isDefaultCustomSlotConfig = (config: CustomSlotConfig): boolean =>
  config.zeroTime === 0 && config.zeroSlot === 0 && config.slotLength === 0;

const customSlotConfigOrigin = (config: CustomSlotConfig): number =>
  config.zeroTime - config.zeroSlot * config.slotLength;

const slotToUnixTimeFromConfiguredCustomMapping = (
  slot: number,
): number | undefined => {
  const config = SLOT_CONFIG_NETWORK.Custom;
  if (
    isDefaultCustomSlotConfig(config) ||
    !Number.isSafeInteger(slot) ||
    !Number.isSafeInteger(config.zeroTime) ||
    !Number.isSafeInteger(config.zeroSlot) ||
    config.zeroSlot < 0 ||
    !Number.isSafeInteger(config.slotLength) ||
    config.slotLength <= 0
  ) {
    return undefined;
  }
  const unixTime =
    config.zeroTime + (slot - config.zeroSlot) * config.slotLength;
  return Number.isSafeInteger(unixTime) ? unixTime : undefined;
};

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
  if (snapshot.slotLengthMs !== genesis.slotLengthMs) {
    throw new Error(
      `Custom slot length disagreement: snapshot=${snapshot.slotLengthMs.toString()},genesis=${genesis.slotLengthMs.toString()}`,
    );
  }
  if (snapshot.observedAtMs < genesis.startTimeMs) {
    throw new Error(
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
    throw new Error(
      `Custom slot clock disagreement: snapshot=${snapshot.currentSlot.toString()},genesisAtObservation=${genesisSlotAtObservation.toString()}`,
    );
  }
  return {
    zeroTime: genesis.startTimeMs,
    zeroSlot: 0,
    slotLength: genesis.slotLengthMs,
  };
};

/**
 * Installs the process-wide Custom mapping used by both Lucid clients.
 * Existing equivalent mappings are preserved; conflicting mappings fail
 * closed so one client cannot silently use a different clock domain.
 */
export const configureCustomSlotConfigFromShelleyGenesis = (
  genesis: ShelleyGenesisSlotConfig,
  snapshot: Pick<
    SubmitSlotSnapshot,
    "currentSlot" | "observedAtMs" | "slotLengthMs"
  >,
): CustomSlotConfig => {
  const next = customSlotConfigFromShelleyGenesis(genesis, snapshot);
  const current = SLOT_CONFIG_NETWORK.Custom;
  if (!isDefaultCustomSlotConfig(current)) {
    const currentIsValid =
      Number.isFinite(current.zeroTime) &&
      Number.isSafeInteger(current.zeroSlot) &&
      current.zeroSlot >= 0 &&
      Number.isFinite(current.slotLength) &&
      Number.isInteger(current.slotLength) &&
      current.slotLength > 0;
    const equivalent =
      currentIsValid &&
      current.slotLength === next.slotLength &&
      customSlotConfigOrigin(current) === customSlotConfigOrigin(next);
    if (!equivalent) {
      throw new Error(
        "Refusing to replace a conflicting preinitialized Lucid Custom slot mapping",
      );
    }
    return current;
  }
  SLOT_CONFIG_NETWORK.Custom = next;
  return next;
};

/**
 * Converts a slot to unix time using the active Lucid network configuration.
 *
 * Custom/emulator networks derive the mapping from the provider's current
 * `time` and `slot` snapshot instead of Cardano's static network tables.
 */
export const slotToUnixTimeForLucid = (
  lucid: LucidEvolution,
  slot: number,
): number | undefined => {
  const network = lucid.config().network;
  if (network === "Custom") {
    const configuredUnixTime = slotToUnixTimeFromConfiguredCustomMapping(slot);
    if (configuredUnixTime !== undefined) {
      return configuredUnixTime;
    }
    const provider = lucid.config().provider as {
      time?: number;
      slot?: number;
    };
    if (
      typeof provider.time === "number" &&
      typeof provider.slot === "number"
    ) {
      const unixTime =
        provider.time + (slot - provider.slot) * SUBMIT_SLOT_LENGTH_MS;
      return Number.isSafeInteger(unixTime) ? unixTime : undefined;
    }
    return undefined;
  }
  return slotToUnixTime(network as Exclude<Network, "Custom">, slot);
};

/**
 * Converts a slot to unix time, falling back to a 1-second slot emulator model
 * when Lucid cannot provide an exact mapping.
 */
export const slotToUnixTimeForLucidOrEmulatorFallback = (
  lucid: LucidEvolution,
  slot: number,
): number => slotToUnixTimeForLucid(lucid, slot) ?? slot * 1000;
