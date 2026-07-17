import type { LucidEvolution } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  resolveUserEventValidTo,
  slotToUnixTimeForLucid,
} from "../src/user-events/index.js";

describe("user-event Lucid time conversion", () => {
  afterEach(() => vi.restoreAllMocks());

  it("delegates slot conversion to the active Lucid instance", () => {
    const lucid = {
      slotToUnixTime: (slot: number) => 90_000 + slot * 2_000,
    } as unknown as LucidEvolution;

    expect(slotToUnixTimeForLucid(lucid, 7)).toBe(104_000);
  });

  it("returns undefined when the instance has no usable slot mapping", () => {
    const lucid = {
      slotToUnixTime: () => {
        throw new Error("slot mapping unavailable");
      },
    } as unknown as LucidEvolution;

    expect(slotToUnixTimeForLucid(lucid, 7)).toBeUndefined();
  });

  it("aligns the user-event deadline with the instance slot configuration", () => {
    vi.spyOn(Date, "now").mockReturnValue(1_000);
    const lucid = {
      unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 2_000),
      slotToUnixTime: (slot: number) => slot * 2_000,
    } as unknown as LucidEvolution;

    expect(resolveUserEventValidTo(lucid, 2_500)).toBe(4_000);
  });
});
