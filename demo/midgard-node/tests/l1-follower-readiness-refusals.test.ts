/**
 * An event the driver refused as undecodable is a readiness detail, never a
 * failing reason: anyone can admit such an event on L1, so it must not take
 * the node out of service. The detail counts the refusals and the report
 * names each, up to `REFUSALS_REPORTED`. An identity conflict is a driver
 * hold instead, so it fails `/readyz` by its name.
 */
import { describe, expect, it } from "vitest";

import {
  type DriverHold,
  EVENT_IDENTITY_CONFLICT,
  EVENT_UNDECODABLE,
  type EventRefusal,
} from "../src/l1-events/driver.js";
import {
  l1FollowerReadiness,
  REFUSALS_REPORTED,
} from "../src/services/l1-follower.readiness.js";
import {
  followingAtTip,
  runningFollower,
} from "./readiness-l1-follower.fixture.js";

const refusal = (index: number): EventRefusal => ({
  kind: "deposit",
  key: index.toString(16).padStart(64, "0"),
  idCbor: "d8",
  reason: EVENT_UNDECODABLE,
  detail: "unsupported committed deposit L2 network id",
});

const refusing = (
  refused: readonly EventRefusal[],
  holds: readonly DriverHold[] = [],
) => ({
  ...runningFollower(followingAtTip(), holds),
  refused: () => refused,
});

describe("the follower's refused-event readiness", () => {
  it("names undecodable events as a counted detail and leaves the node ready", () => {
    const readiness = l1FollowerReadiness(refusing([refusal(1)]));
    expect(readiness.reasons).toEqual([]);
    expect(readiness.details).toEqual([`${EVENT_UNDECODABLE}:1`]);
    expect(readiness.report.refused).toEqual([refusal(1)]);
  });

  it("counts every refusal but reports a bounded list", () => {
    const many = Array.from({ length: REFUSALS_REPORTED + 3 }, (_, index) =>
      refusal(index),
    );
    const readiness = l1FollowerReadiness(refusing(many));
    expect(readiness.details).toEqual([
      `${EVENT_UNDECODABLE}:${(REFUSALS_REPORTED + 3).toString()}`,
    ]);
    expect(readiness.report.refused).toEqual(many.slice(0, REFUSALS_REPORTED));
  });

  it("reports no detail without refusals, and an identity conflict fails readiness by name", () => {
    const conflict: DriverHold = {
      reason: EVENT_IDENTITY_CONFLICT,
      detail:
        "deposit 00 (id d8): a local row of its public id holds another live admission",
    };
    const readiness = l1FollowerReadiness(refusing([], [conflict]));
    expect(readiness.details).toEqual([]);
    expect(readiness.reasons).toEqual([EVENT_IDENTITY_CONFLICT]);
    expect(readiness.report.refused).toEqual([]);
  });
});
