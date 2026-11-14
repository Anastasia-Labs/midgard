import { isProxy } from "node:util/types";

import {
  type L1SlotConfig,
  l1SlotConfigMismatch,
  NAMED_NETWORK_L1_SLOT_CONFIGS,
} from "@al-ft/midgard-core/l1-slot-config";
import type { SlotConfig } from "@lucid-evolution/lucid";

export type WatcherCustomNetwork = Readonly<{
  networkMagic: number;
  slotConfig: Readonly<SlotConfig>;
}>;

const record = (value: unknown, keys: readonly string[]) => {
  if (
    typeof value !== "object" ||
    value === null ||
    isProxy(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== keys.length ||
    keys.some((key) => {
      const property = Object.getOwnPropertyDescriptor(value, key);
      return (
        property === undefined || !("value" in property) || !property.enumerable
      );
    })
  )
    throw new Error(
      "Custom network metadata must contain exactly its declared fields",
    );
  return value as Record<string, unknown>;
};

/** Explicit private chain identity; native startup verifies it against genesis. */
export const parseWatcherCustomNetwork = (
  value: unknown,
): WatcherCustomNetwork => {
  const input = record(value, ["networkMagic", "slotConfig"]);
  const clock = record(input.slotConfig, [
    "zeroTime",
    "zeroSlot",
    "slotLength",
  ]);
  if (
    typeof input.networkMagic !== "number" ||
    !Number.isSafeInteger(input.networkMagic) ||
    input.networkMagic < 0 ||
    input.networkMagic > 0xffff_ffff ||
    [1, 2, 764_824_073].includes(input.networkMagic) ||
    typeof clock.zeroTime !== "number" ||
    !Number.isSafeInteger(clock.zeroTime) ||
    clock.zeroTime < 0 ||
    clock.zeroSlot !== 0 ||
    typeof clock.slotLength !== "number" ||
    !Number.isSafeInteger(clock.slotLength) ||
    clock.slotLength <= 0 ||
    clock.slotLength > 60_000
  )
    throw new Error("Custom network magic or genesis slot clock is invalid");
  return Object.freeze({
    networkMagic: input.networkMagic,
    slotConfig: Object.freeze({
      zeroTime: clock.zeroTime,
      zeroSlot: 0,
      slotLength: clock.slotLength,
    }),
  });
};

/**
 * The slot mapping this watcher runs with: Lucid's static mapping for a named
 * network, the configured genesis clock for a Custom one.
 */
export const watcherRuntimeL1SlotConfig = (input: {
  readonly targetNetwork: "Mainnet" | "Preprod" | "Preview" | "Custom";
  readonly customNetwork?: WatcherCustomNetwork;
}): L1SlotConfig => {
  if (input.targetNetwork !== "Custom")
    return NAMED_NETWORK_L1_SLOT_CONFIGS[input.targetNetwork];
  if (input.customNetwork === undefined)
    throw new Error("Custom network metadata is missing its slot clock");
  return input.customNetwork.slotConfig;
};

/**
 * Refuses a watcher whose slot mapping differs from the deployment's: the
 * script contexts it replays would carry different validity ranges than the
 * ones the deployed validator builds.
 */
export const assertWatcherL1SlotConfigMatchesDeployment = (
  deployed: L1SlotConfig,
  input: Parameters<typeof watcherRuntimeL1SlotConfig>[0],
): void => {
  const mismatch = l1SlotConfigMismatch(
    deployed,
    watcherRuntimeL1SlotConfig(input),
  );
  if (mismatch !== null) throw new Error(mismatch);
};
