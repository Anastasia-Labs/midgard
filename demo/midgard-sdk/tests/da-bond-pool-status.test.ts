/**
 * The pool status readout: backing excludes the floor, `belowBond` is exactly
 * `backing < da_bond_lovelace`, and the state (with `unlock_at`) is read
 * independently of it.
 */
import { describe, expect, it } from "vitest";

import {
  availabilityResponseGeometry,
  DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  daAvailabilityParameters,
} from "../src/availability-challenge.js";
import { daBondPoolStatus, planDaBondPoolSlash } from "../src/da-bond-pool.js";
import * as Sdk from "../src/index.js";

const parameters = daAvailabilityParameters({
  responseGeometry: availabilityResponseGeometry(
    DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  ),
  ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  challengerBondLovelace:
    DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  maxOpenFeeLovelace: 500_000n,
  maxPublicationFeeLovelace: 500_000n,
  maxSettlementFeeLovelace: 500_000n,
  maxCloseFeeLovelace: 1_000_000n,
  maxTimeoutFeeLovelace: 1_200_000n,
});
const FLOOR = parameters.da_bond_pool_floor_lovelace;
const BOND = parameters.da_bond_lovelace;
const UNLOCK_AT = 1_900_000_000_000n;

describe("DA bond pool status readout", () => {
  it("is exported from the package entry point", () => {
    expect(Sdk.daBondPoolStatus).toBe(daBondPoolStatus);
    expect(FLOOR).toBeGreaterThan(0n);
    expect(BOND).toBeGreaterThan(1n);
  });

  it("reads a full Bonded pool as backed, with no unlock_at", () => {
    const lovelace = FLOOR + 3n * BOND;
    const status = daBondPoolStatus({ lovelace, datum: "Bonded", parameters });
    expect(status).toStrictEqual({
      state: "bonded",
      lovelace,
      backing: 3n * BOND,
      requiredBacking: BOND,
      belowBond: false,
    });
    expect("unlockAt" in status).toBe(false);
  });

  it("reads a Bonded pool a slash drained below da_bond as below bond", () => {
    const plan = planDaBondPoolSlash({
      poolLovelace: FLOOR + BOND + BOND / 2n,
      parameters,
    });
    expect(plan.taken).toBe(BOND);
    const status = daBondPoolStatus({
      lovelace: plan.poolOutputLovelace,
      datum: "Bonded",
      parameters,
    });
    expect(status.backing).toBe(BOND / 2n);
    expect(status.backing).toBeGreaterThan(0n);
    expect(status.state).toBe("bonded");
    expect(status.belowBond).toBe(true);
  });

  it("never counts the floor: a pool at or under it backs 0", () => {
    for (const lovelace of [FLOOR, FLOOR - 1n, 0n]) {
      const status = daBondPoolStatus({
        lovelace,
        datum: "Bonded",
        parameters,
      });
      expect(status.lovelace).toBe(lovelace);
      expect(status.backing).toBe(0n);
      expect(status.belowBond).toBe(true);
    }
  });

  it("reads Withdrawing with unlock_at, independently of belowBond", () => {
    const datum = { Withdrawing: { unlock_at: UNLOCK_AT } } as const;
    const full = daBondPoolStatus({
      lovelace: FLOOR + BOND,
      datum,
      parameters,
    });
    expect(full).toStrictEqual({
      state: "withdrawing",
      lovelace: FLOOR + BOND,
      backing: BOND,
      requiredBacking: BOND,
      belowBond: false,
      unlockAt: UNLOCK_AT,
    });
    const short = daBondPoolStatus({
      lovelace: FLOOR + BOND - 1n,
      datum,
      parameters,
    });
    expect(short.state).toBe("withdrawing");
    expect(short.unlockAt).toBe(UNLOCK_AT);
    expect(short.belowBond).toBe(true);
  });

  it("clears belowBond exactly when a top-up restores backing to da_bond", () => {
    const drained = FLOOR + BOND - 1n;
    expect(
      daBondPoolStatus({ lovelace: drained, datum: "Bonded", parameters })
        .belowBond,
    ).toBe(true);
    const toppedUp = daBondPoolStatus({
      lovelace: drained + 1n,
      datum: "Bonded",
      parameters,
    });
    expect(toppedUp.backing).toBe(BOND);
    expect(toppedUp.belowBond).toBe(false);
  });
});
