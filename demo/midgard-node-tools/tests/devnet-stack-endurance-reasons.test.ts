import { describe, expect, it } from "vitest";

import {
  lowBalanceFloorLovelace,
  lowBalanceReasons,
} from "../src/devnet-stack/funding.js";
import { enduranceReporter } from "../src/devnet-stack/reserve-float-chain.js";

describe("lowBalanceReasons", () => {
  it("names every role below a fifth of its budget, and only those", () => {
    const floor = lowBalanceFloorLovelace("operator");
    expect(floor).toBe(40_000n * 1_000_000n);
    expect(
      lowBalanceReasons({
        operator: floor - 1n,
        merge: lowBalanceFloorLovelace("merge"),
        userA: 0n,
      }),
    ).toEqual([
      `wallet_balance_low: role=operator, lovelace=${floor - 1n}, floorLovelace=${floor}`,
      `wallet_balance_low: role=userA, lovelace=0, floorLovelace=${lowBalanceFloorLovelace("userA")}`,
    ]);
  });

  it("reports nothing for funded roles or roles it could not read", () => {
    expect(
      lowBalanceReasons({ operator: lowBalanceFloorLovelace("operator") }),
    ).toEqual([]);
    expect(lowBalanceReasons({})).toEqual([]);
  });
});

describe("enduranceReporter", () => {
  it("logs a reason once when it appears and once when it clears", () => {
    const lines: string[] = [];
    const report = enduranceReporter((line) => lines.push(line));
    const low = (lovelace: number) =>
      `wallet_balance_low: role=operator, lovelace=${lovelace}, floorLovelace=10`;
    report([]);
    report([low(5)]);
    // A standing reason whose figures move is still the one reason.
    report([low(4)]);
    report([low(3), "l1_tip_stalled: tipSlot=1, wallSlot=900, lagSlots=899"]);
    report([low(3)]);
    report([]);
    report([]);
    expect(lines).toEqual([
      `endurance: WARNING ${low(5)}`,
      "endurance: WARNING l1_tip_stalled: tipSlot=1, wallSlot=900, lagSlots=899",
      "endurance: cleared l1_tip_stalled/",
      "endurance: cleared wallet_balance_low/operator",
    ]);
  });

  it("tells one role's low wallet from another's", () => {
    const lines: string[] = [];
    const report = enduranceReporter((line) => lines.push(line));
    report(["wallet_balance_low: role=userA, lovelace=1, floorLovelace=2"]);
    report(["wallet_balance_low: role=userB, lovelace=1, floorLovelace=2"]);
    expect(lines).toEqual([
      "endurance: WARNING wallet_balance_low: role=userA, lovelace=1, floorLovelace=2",
      "endurance: WARNING wallet_balance_low: role=userB, lovelace=1, floorLovelace=2",
      "endurance: cleared wallet_balance_low/userA",
    ]);
  });
});
