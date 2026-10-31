import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  activeNodeFor,
  emptyDirectory,
  registeredNodeFor,
  retiredNodeFor,
  rootNode,
  syntheticScheduler,
} from "./operator-exit-emulator.assert-operator-not-in-directory.js";
import {
  OPERATOR_A,
  OPERATOR_B,
} from "./operator-exit-emulator.duplicate-operator-slashing.js";

describe("deriveOperatorStatus", () => {
  const params = { maxInactivityStrikes: 5n } as const;

  it("reports an unknown key as absent", () => {
    const status = SDK.deriveOperatorStatus(
      { ...emptyDirectory, scheduler: syntheticScheduler("NoActiveOperators") },
      OPERATOR_A,
      1_700_000_000_000n,
      params,
    );
    expect(status.state).toEqual("none");
    expect(status.occupancies).toEqual([]);
    expect(status.duplicate).toBe(false);
    expect(status.bondLovelace).toBeNull();
    expect(status.bondRecoveryAllowedNow).toBe(false);
    expect(status.bondRecoveryAllowedFrom).toBeNull();
    expect(status.scheduledOperator).toBeNull();
  });

  it("reports a pending registration and whether its activation time has arrived", () => {
    const activationTime = 1_700_000_000_000n;
    const view = {
      ...emptyDirectory,
      registered: [
        rootNode("aa".repeat(32), null),
        registeredNodeFor(OPERATOR_A, activationTime),
      ],
      scheduler: syntheticScheduler("NoActiveOperators"),
    };
    const before = SDK.deriveOperatorStatus(
      view,
      OPERATOR_A,
      activationTime - 1n,
      params,
    );
    expect(before.state).toEqual("registered");
    expect(before.registeredActivationTime).toEqual(activationTime);
    expect(before.activationTimeReached).toBe(false);
    expect(before.bondLovelace).toEqual(900_000_000n);
    expect(
      SDK.deriveOperatorStatus(view, OPERATOR_A, activationTime, params)
        .activationTimeReached,
    ).toBe(true);
  });

  it("reports the shift and the forced-retirement threshold for an active operator", () => {
    const startTime = 1_700_000_000_000n;
    const view = {
      ...emptyDirectory,
      active: [
        rootNode("ab".repeat(32), OPERATOR_A),
        activeNodeFor(OPERATOR_A, 5n),
      ],
      scheduler: syntheticScheduler({
        ActiveOperator: { operator: OPERATOR_A, start_time: startTime },
      }),
    };
    const status = SDK.deriveOperatorStatus(
      view,
      OPERATOR_A,
      startTime + 60_000n,
      params,
    );
    expect(status.state).toEqual("active");
    expect(status.inactivityStrikes).toEqual(5n);
    expect(status.forcedRetirementEligible).toBe(true);
    expect(status.holdsShift).toBe(true);
    expect(status.shiftAgeMs).toEqual(60_000n);
    expect(status.bondRecoveryAllowedNow).toBe(false);

    const belowThreshold = SDK.deriveOperatorStatus(
      {
        ...view,
        active: [
          rootNode("ab".repeat(32), OPERATOR_A),
          activeNodeFor(OPERATOR_A, 4n),
        ],
      },
      OPERATOR_A,
      startTime + 60_000n,
      params,
    );
    expect(belowThreshold.forcedRetirementEligible).toBe(false);

    const other = SDK.deriveOperatorStatus(
      view,
      OPERATOR_B,
      startTime + 60_000n,
      params,
    );
    expect(other.holdsShift).toBe(false);
    expect(other.shiftAgeMs).toBeNull();
    expect(other.scheduledOperator).toEqual(OPERATOR_A);
  });

  it("gates bond recovery on the retired node's bond hold", () => {
    const unlockTime = 1_700_000_000_000n;
    const held = SDK.deriveOperatorStatus(
      {
        ...emptyDirectory,
        retired: [
          rootNode("ac".repeat(32), OPERATOR_A),
          retiredNodeFor(OPERATOR_A, unlockTime),
        ],
        scheduler: syntheticScheduler("NoActiveOperators"),
      },
      OPERATOR_A,
      unlockTime,
      params,
    );
    expect(held.state).toEqual("retired");
    expect(held.bondUnlockTime).toEqual(unlockTime);
    // `is_entirely_after` is strict, so equality is still too early.
    expect(held.bondRecoveryAllowedNow).toBe(false);
    expect(held.bondRecoveryAllowedFrom).toEqual(unlockTime + 1n);

    const free = SDK.deriveOperatorStatus(
      {
        ...emptyDirectory,
        retired: [
          rootNode("ac".repeat(32), OPERATOR_A),
          retiredNodeFor(OPERATOR_A, null),
        ],
        scheduler: syntheticScheduler("NoActiveOperators"),
      },
      OPERATOR_A,
      unlockTime,
      params,
    );
    expect(free.bondRecoveryAllowedNow).toBe(true);
    expect(free.bondRecoveryAllowedFrom).toBeNull();
  });

  it("flags a key that holds more than one membership", () => {
    const status = SDK.deriveOperatorStatus(
      {
        registered: [
          rootNode("aa".repeat(32), null),
          registeredNodeFor(OPERATOR_A, 1_700_000_000_000n),
        ],
        active: [
          rootNode("ab".repeat(32), OPERATOR_A),
          activeNodeFor(OPERATOR_A, 0n),
        ],
        retired: [rootNode("ac".repeat(32), null)],
        scheduler: syntheticScheduler("NoActiveOperators"),
      },
      OPERATOR_A,
      1_700_000_000_000n,
      params,
    );
    expect(status.duplicate).toBe(true);
    expect(status.occupancies).toEqual(["registered", "active"]);
    // Precedence: the active membership is the one that owns the bond.
    expect(status.state).toEqual("active");
  });
});
