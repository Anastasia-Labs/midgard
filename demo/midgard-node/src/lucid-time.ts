/**
 * Canonical Lucid slot/unix-time conversion helpers for the node.
 * This module centralizes network-aware time conversion and emulator fallback
 * behavior so transaction code does not guess about clock semantics.
 */
import { type LucidEvolution, type SlotConfig } from "@lucid-evolution/lucid";

import {
  type ShelleyGenesisSlotConfig,
  SUBMIT_SLOT_VALIDITY_BUFFER,
  type SubmitSlotSnapshot,
} from "./local-ledger-slot.js";
export type CustomSlotConfig = SlotConfig;

const assertValidSlotConfig = (
  slotConfig: SlotConfig,
  label: string,
): SlotConfig => {
  if (!Number.isSafeInteger(slotConfig.zeroTime)) {
    throw new Error(`Invalid ${label} zeroTime=${String(slotConfig.zeroTime)}`);
  }
  if (!Number.isSafeInteger(slotConfig.zeroSlot) || slotConfig.zeroSlot < 0) {
    throw new Error(`Invalid ${label} zeroSlot=${String(slotConfig.zeroSlot)}`);
  }
  if (
    !Number.isSafeInteger(slotConfig.slotLength) ||
    slotConfig.slotLength <= 0
  ) {
    throw new Error(
      `Invalid ${label} slotLength=${String(slotConfig.slotLength)}`,
    );
  }
  return {
    zeroTime: slotConfig.zeroTime,
    zeroSlot: slotConfig.zeroSlot,
    slotLength: slotConfig.slotLength,
  };
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
 * Copies the immutable per-instance mapping used by Lucid into a plain value
 * that can cross the commitment worker boundary without carrying a provider.
 */
export const canonicalSlotConfigForLucid = (
  lucid: LucidEvolution,
): SlotConfig => {
  const slotConfig = lucid.config().slotConfig;
  if (slotConfig === undefined) {
    throw new Error("Lucid does not expose a canonical slot configuration");
  }
  return assertValidSlotConfig(slotConfig, "Lucid slot configuration");
};

/**
 * Performs Lucid's enclosing-slot conversion from a serializable mapping.
 * This keeps speculative proof construction independent of L1 provider
 * acquisition while preserving the exact mapping selected at node startup.
 */
export const unixTimeToSlotForConfig = (
  unixTimeMs: number,
  slotConfig: SlotConfig,
): number => {
  if (!Number.isSafeInteger(unixTimeMs)) {
    throw new Error(`Invalid unixTimeMs=${String(unixTimeMs)}`);
  }
  const config = assertValidSlotConfig(slotConfig, "worker slot configuration");
  const slot =
    Math.floor((unixTimeMs - config.zeroTime) / config.slotLength) +
    config.zeroSlot;
  if (!Number.isSafeInteger(slot) || slot < 0) {
    throw new Error(
      `Unix time ${unixTimeMs.toString()} maps to invalid slot ${String(slot)}`,
    );
  }
  return slot;
};

/**
 * Converts a slot to unix time using the active Lucid network configuration.
 *
 * Custom networks receive their authoritative mapping when each Lucid client
 * is constructed, avoiding process-global slot configuration.
 */
export const slotToUnixTimeForLucid = (
  lucid: LucidEvolution,
  slot: number,
): number | undefined => {
  try {
    const unixTime = lucid.slotToUnixTime(slot);
    return Number.isSafeInteger(unixTime) ? unixTime : undefined;
  } catch {
    return undefined;
  }
};

/**
 * Converts a slot to unix time, falling back to a 1-second slot emulator model
 * when Lucid cannot provide an exact mapping.
 */
export const slotToUnixTimeForLucidOrEmulatorFallback = (
  lucid: LucidEvolution,
  slot: number,
): number => slotToUnixTimeForLucid(lucid, slot) ?? slot * 1000;
