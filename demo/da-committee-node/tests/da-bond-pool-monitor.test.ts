import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  createDaBondPoolMonitor,
  type DaBondPoolCheck,
  daBondPoolCheckFromStatus,
  type DaBondPoolEvent,
  daBondPoolReadinessReasons,
} from "../src/coordinator/pool-monitor.js";

/** The selected deployment profile's canonical parameters. */
const PARAMETERS = SDK.daAvailabilityParameters({
  responseGeometry: SDK.availabilityResponseGeometry({
    chunkByteLength: 14_020,
    trancheByteLength: 4 * 1_024 * 1_024,
    maxTrancheCount: 16,
  }),
  ...SDK.DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  challengerBondLovelace: 10_000_000_000n,
  maxOpenFeeLovelace: 500_000n,
  maxPublicationFeeLovelace: 500_000n,
  maxSettlementFeeLovelace: 500_000n,
  maxCloseFeeLovelace: 1_000_000n,
  maxTimeoutFeeLovelace: 1_200_000n,
});
const BOND = PARAMETERS.da_bond_lovelace.toString();
const BOND_LESS_ONE = (PARAMETERS.da_bond_lovelace - 1n).toString();

/** A pool backing exactly one DA bond above its floor. */
const BONDED_LOVELACE =
  PARAMETERS.da_bond_pool_floor_lovelace + PARAMETERS.da_bond_lovelace;
const UNLOCK_AT = 1_800_000_000_000n;

/** The check the submitter makes of a pool with this lovelace and datum. */
const check = (
  lovelace: bigint,
  datum: SDK.DaBondPoolDatum,
  checkedAt: string,
): DaBondPoolCheck =>
  daBondPoolCheckFromStatus(
    SDK.daBondPoolStatus({ lovelace, datum, parameters: PARAMETERS }),
    checkedAt,
  );

const withdrawing: SDK.DaBondPoolDatum = {
  Withdrawing: { unlock_at: UNLOCK_AT },
};

const harness = () => {
  const events: DaBondPoolEvent[] = [];
  const monitor = createDaBondPoolMonitor({
    writeEvent: (event) => events.push(event),
    now: () => new Date("2026-09-28T00:00:00.000Z"),
  });
  return { events, monitor };
};

describe("pooled DA bond readiness reasons", () => {
  it("gives no reason for a Bonded pool backing exactly one DA bond", () => {
    expect(
      daBondPoolReadinessReasons(check(BONDED_LOVELACE, "Bonded", "t0")),
    ).toEqual([]);
  });

  it("reports a pool one lovelace short of a DA bond, with its backing", () => {
    expect(
      daBondPoolReadinessReasons(check(BONDED_LOVELACE - 1n, "Bonded", "t0")),
    ).toEqual([
      `da_bond_pool_backing_short: backing=${BOND_LESS_ONE}, required=${BOND}, checkedAt=t0`,
    ]);
  });

  it("reports a Withdrawing pool however well backed, with its unlock time", () => {
    expect(
      daBondPoolReadinessReasons(
        check(BONDED_LOVELACE * 10n, withdrawing, "t0"),
      ),
    ).toEqual([
      `da_bond_pool_withdrawing: unlockAt=${UNLOCK_AT.toString()}, checkedAt=t0`,
    ]);
  });

  it("reports both at once for a short Withdrawing pool", () => {
    expect(
      daBondPoolReadinessReasons(
        check(PARAMETERS.da_bond_pool_floor_lovelace, withdrawing, "t0"),
      ),
    ).toEqual([
      `da_bond_pool_backing_short: backing=0, required=${BOND}, checkedAt=t0`,
      `da_bond_pool_withdrawing: unlockAt=${UNLOCK_AT.toString()}, checkedAt=t0`,
    ]);
  });
});

describe("pooled DA bond monitor", () => {
  it("emits nothing for a first read of a pool that can back an attestation", () => {
    const { events, monitor } = harness();
    monitor.record(check(BONDED_LOVELACE, "Bonded", "t0"));
    expect(events).toEqual([]);
    expect(monitor.latest()?.checkedAt).toBe("t0");
  });

  it("alerts on a pool drained by a slash and clears after a top-up", () => {
    const { events, monitor } = harness();
    monitor.record(check(BONDED_LOVELACE, "Bonded", "t0"));

    // The slash takes one DA bond (floor + da_bond - taken).
    const slash = SDK.planDaBondPoolSlash({
      poolLovelace: BONDED_LOVELACE,
      parameters: PARAMETERS,
    });
    expect(slash.poolOutputLovelace).toBe(BONDED_LOVELACE - slash.taken);
    const drained = check(slash.poolOutputLovelace, "Bonded", "t1");
    monitor.record(drained);
    expect(daBondPoolReadinessReasons(monitor.latest()!)).toEqual([
      `da_bond_pool_backing_short: backing=${drained.backing.toString()}, required=${BOND}, checkedAt=t1`,
    ]);

    // A top-up of exactly what was taken restores one DA bond of backing.
    monitor.record(
      check(slash.poolOutputLovelace + slash.taken, "Bonded", "t2"),
    );
    expect(daBondPoolReadinessReasons(monitor.latest()!)).toEqual([]);

    expect(events).toEqual([
      {
        event: "da_bond_pool_backing_short",
        backing: drained.backing.toString(),
        required: BOND,
        checkedAt: "t1",
      },
      {
        event: "da_bond_pool_backing_restored",
        backing: BOND,
        required: BOND,
        checkedAt: "t2",
      },
    ]);
  });

  it("alerts when the pool begins withdrawing and clears when it is cancelled", () => {
    const { events, monitor } = harness();
    monitor.record(check(BONDED_LOVELACE, "Bonded", "t0"));
    monitor.record(check(BONDED_LOVELACE, withdrawing, "t1"));
    expect(daBondPoolReadinessReasons(monitor.latest()!)).toHaveLength(1);
    monitor.record(check(BONDED_LOVELACE, "Bonded", "t2"));
    expect(daBondPoolReadinessReasons(monitor.latest()!)).toEqual([]);

    expect(events).toEqual([
      {
        event: "da_bond_pool_withdrawing",
        backing: BOND,
        required: BOND,
        unlockAt: UNLOCK_AT.toString(),
        checkedAt: "t1",
      },
      {
        event: "da_bond_pool_bonded",
        backing: BOND,
        required: BOND,
        checkedAt: "t2",
      },
    ]);
  });

  it("emits both events for a first read of a short Withdrawing pool, then nothing while it stays so", () => {
    const { events, monitor } = harness();
    monitor.record(
      check(PARAMETERS.da_bond_pool_floor_lovelace, withdrawing, "t0"),
    );
    expect(events.map(({ event }) => event)).toEqual([
      "da_bond_pool_backing_short",
      "da_bond_pool_withdrawing",
    ]);
    monitor.record(
      check(PARAMETERS.da_bond_pool_floor_lovelace, withdrawing, "t1"),
    );
    monitor.record(
      check(PARAMETERS.da_bond_pool_floor_lovelace, withdrawing, "t2"),
    );
    expect(events).toHaveLength(2);
    expect(monitor.latest()?.checkedAt).toBe("t2");
  });

  it("emits nothing on repeated identical reads of a healthy pool", () => {
    const { events, monitor } = harness();
    for (const at of ["t0", "t1", "t2"]) {
      monitor.record(check(BONDED_LOVELACE, "Bonded", at));
    }
    expect(events).toEqual([]);
  });

  it("keeps the last good check across read failures and reports each failure streak once", () => {
    const { events, monitor } = harness();
    monitor.record(check(BONDED_LOVELACE - 1n, "Bonded", "t0"));
    events.length = 0;

    monitor.recordReadFailure(new Error("kupo unavailable"));
    monitor.recordReadFailure(new Error("kupo still unavailable"));
    // The failure adds no reason and keeps the short pool's.
    expect(monitor.latest()?.checkedAt).toBe("t0");
    expect(daBondPoolReadinessReasons(monitor.latest()!)).toEqual([
      `da_bond_pool_backing_short: backing=${BOND_LESS_ONE}, required=${BOND}, checkedAt=t0`,
    ]);
    expect(events).toEqual([
      {
        event: "da_bond_pool_read_failed",
        error: "kupo unavailable",
        failedAt: "2026-09-28T00:00:00.000Z",
      },
    ]);

    // A good read ends the streak; the next failure is reported again. The
    // pool did not change across the failures, so there is no transition.
    monitor.record(check(BONDED_LOVELACE - 1n, "Bonded", "t1"));
    monitor.recordReadFailure("provider timeout");
    expect(events.map(({ event }) => event)).toEqual([
      "da_bond_pool_read_failed",
      "da_bond_pool_read_failed",
    ]);
    expect(events[1]?.error).toBe("provider timeout");
  });

  it("has no check before the first successful read", () => {
    const { monitor } = harness();
    monitor.recordReadFailure(new Error("no pool yet"));
    expect(monitor.latest()).toBeUndefined();
  });
});
