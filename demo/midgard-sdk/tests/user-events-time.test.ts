/**
 * The user-event submission path converts between Lucid's slot grid and unix
 * time in two places: a tolerant slot lookup that must degrade to `undefined`
 * rather than throw when the active provider has no usable slot mapping, and
 * the transaction's `validTo`, which must land on a slot boundary strictly
 * after the requested deadline (a `validTo` at or before the deadline yields a
 * transaction the node rejects as already expired).
 *
 * The slot grid here is a real one: the Lucid Emulator's own slot
 * configuration, driven through `Lucid`, so the arithmetic under test is
 * checked against the same conversion the submission path uses in production
 * rather than against a two-method literal written by this file. Only the
 * clock is controlled, and it is injected rather than patched globally.
 */
import type { LucidEvolution } from "@lucid-evolution/lucid";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  resolveUserEventValidTo,
  slotToUnixTimeForLucid,
} from "../src/user-events/index.js";

const TTL_MS = 90_000;

let lucid: LucidEvolution;

beforeAll(async () => {
  const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  lucid = await Lucid(new Emulator([account]), "Custom");
});

describe("user-event Lucid time conversion", () => {
  it("reads the active instance's real slot grid", () => {
    // Independent oracle: the instance's own inverse mapping. A slot's unix
    // time must map back to that slot, and the next slot must be strictly
    // later -- a monotone, invertible grid is what the deadline arithmetic
    // below relies on.
    const unixTime = slotToUnixTimeForLucid(lucid, 7);
    expect(unixTime).toBeDefined();
    expect(lucid.unixTimeToSlot(unixTime!)).toBe(7);
    expect(slotToUnixTimeForLucid(lucid, 8)!).toBeGreaterThan(unixTime!);
  });

  it("returns undefined when the instance has no usable slot mapping", () => {
    // The tolerant path exists because a provider can be configured without a
    // slot mapping; a throw here would abort the whole submission instead.
    const throwing = {
      slotToUnixTime: () => {
        throw new Error("slot mapping unavailable");
      },
    } as unknown as LucidEvolution;

    expect(slotToUnixTimeForLucid(throwing, 7)).toBeUndefined();
  });

  it("returns undefined when the instance reports an unusable slot time", () => {
    const unusable = {
      slotToUnixTime: () => Number.NaN,
    } as unknown as LucidEvolution;

    expect(slotToUnixTimeForLucid(unusable, 7)).toBeUndefined();
  });

  it("snaps the deadline onto the next slot boundary strictly after it", () => {
    const slotLength =
      slotToUnixTimeForLucid(lucid, 1_001)! -
      slotToUnixTimeForLucid(lucid, 1_000)!;
    // A clock reading placed exactly on a slot boundary is the boundary case:
    // the aligned time equals the deadline, which is not strictly after it, so
    // the contract requires the *following* slot.
    const onBoundary = slotToUnixTimeForLucid(lucid, 1_000)! - TTL_MS;
    const exact = resolveUserEventValidTo(lucid, TTL_MS, () => onBoundary);
    expect(exact).toBe(slotToUnixTimeForLucid(lucid, 1_001));
    expect(exact - (onBoundary + TTL_MS)).toBe(slotLength);

    // A clock reading inside a slot rounds up to that slot's own end boundary.
    const insideSlot = onBoundary + 1;
    const rounded = resolveUserEventValidTo(lucid, TTL_MS, () => insideSlot);
    expect(rounded).toBe(slotToUnixTimeForLucid(lucid, 1_001));
    expect(rounded).toBeGreaterThan(insideSlot + TTL_MS);
  });

  it("never returns a deadline at or before the requested expiry", () => {
    // The property the two cases above are instances of, swept across a whole
    // slot so an off-by-one in either branch is caught wherever it sits.
    const slotLength =
      slotToUnixTimeForLucid(lucid, 1_001)! -
      slotToUnixTimeForLucid(lucid, 1_000)!;
    const base = slotToUnixTimeForLucid(lucid, 1_000)! - TTL_MS;
    for (const offset of [0, 1, Math.floor(slotLength / 2), slotLength - 1]) {
      const now = base + offset;
      const validTo = resolveUserEventValidTo(lucid, TTL_MS, () => now);
      expect(validTo, `offset ${offset.toString()}`).toBeGreaterThan(
        now + TTL_MS,
      );
      // and it is a slot boundary, not an arbitrary instant
      expect(
        slotToUnixTimeForLucid(lucid, lucid.unixTimeToSlot(validTo)),
        `offset ${offset.toString()} lands on a slot boundary`,
      ).toBe(validTo);
    }
  });
});
