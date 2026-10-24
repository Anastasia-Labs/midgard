/**
 * Canonical Lucid slot/unix-time conversion helpers for the node.
 * This module centralizes network-aware time conversion and emulator fallback
 * behavior so transaction code does not guess about clock semantics.
 */
import { type LucidEvolution, type SlotConfig } from "@lucid-evolution/lucid";

export {
  type CustomSlotConfig,
  customSlotConfigFromShelleyGenesis,
} from "@al-ft/midgard-core/ogmios-slot";

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
 * This keeps proof construction independent of L1 provider
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
