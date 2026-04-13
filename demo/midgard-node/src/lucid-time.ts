/**
 * Canonical Lucid slot/unix-time conversion helpers for the node.
 * This module centralizes network-aware time conversion and emulator fallback
 * behavior so transaction code does not guess about clock semantics.
 */
import {
  type LucidEvolution,
  type Network,
  slotToUnixTime,
} from "@lucid-evolution/lucid";

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
    const provider = lucid.config().provider as {
      time?: number;
      slot?: number;
    };
    if (
      typeof provider.time !== "number" ||
      typeof provider.slot !== "number"
    ) {
      return undefined;
    }
    const slotLength = 1000;
    const zeroTime = provider.time - provider.slot * slotLength;
    return zeroTime + slot * slotLength;
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
