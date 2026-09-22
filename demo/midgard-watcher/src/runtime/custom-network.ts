import { isProxy } from "node:util/types";

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
