/** The funded watcher's lost-defense slash (as in
 * validation-trace-dispute-funded-lost-defense-slash.test.ts) across a
 * restart: once our close of the silent operator's thread has minted its
 * receipt, the watcher stops before any slash goes out and starts again.
 * Naming a unit in a capture is what holds its history under the header
 * (own-view.chain-capture.ts), and the new process names nothing until it
 * observes: its first observation names our case, each thread token of it
 * and the receipt our close minted, and the slash it then finds due lands
 * once the test lets it. */
import { expect, it } from "vitest";

import type { ValidationTraceCaseHistory } from "../src/validation-trace-dispute/case-history.js";
import {
  confirmed,
  playToDefenderWon,
  receiptUnit,
  SLASH,
  turnCollecting,
} from "./support/validation-trace-dispute-funded-lost-defense.js";
import {
  caseUnitOf,
  openTwoDefendedFundedCase,
} from "./support/validation-trace-dispute-funded-moves.js";
import type { AdapterWrap } from "./support/validation-trace-dispute-funded-runner.js";
import { TIMEOUT } from "./support/validation-trace-dispute-funded-verify-source.js";

const SLASH_HELD = "the slash is held by the test";

/** Holds every slash the watcher would send, as a wait. */
const holdSlash: AdapterWrap = (adapter) => ({
  ...adapter,
  observe: async (context) => {
    const observed = await adapter.observe(context);
    return observed.kind === "action_required" &&
      observed.action.actionId.startsWith(SLASH)
      ? { kind: "pending", reason: SLASH_HELD }
      : observed;
  },
});

/** The units each capture names while `recording` is set. */
const requests: (readonly string[])[] = [];
let recording = false;
const recordingCaptures = (
  authentic: ValidationTraceCaseHistory,
): ValidationTraceCaseHistory =>
  Object.freeze({
    ...authentic,
    snapshot: async (input) => {
      if (recording) requests.push(input.historyUnits);
      return await authentic.snapshot!(input);
    },
  });

it(
  "validation-trace dispute watcher restarted before a due lost-defense slash names our case, its thread tokens and the receipt our close minted in its first observation, then slashes",
  async () => {
    const journey = await openTwoDefendedFundedCase({
      caseHistory: recordingCaptures,
    });
    const held = await playToDefenderWon(journey, holdSlash, SLASH_HELD);
    expect(held.alarms, held.history.join("\n")).toStrictEqual([]);
    expect(
      (await confirmed(journey.dispute)).map(({ stage }) => stage),
    ).toContain("close_thread");

    journey.dispute.restart();
    recording = true;
    const first = await journey.dispute.turn(holdSlash);
    recording = false;
    expect(first.kind === "pending" ? first.reason : first.kind).toMatch(
      SLASH_HELD,
    );
    const named = new Set(requests.flat());
    for (const unit of [
      caseUnitOf(journey),
      journey.unit,
      journey.second.unit,
      receiptUnit(journey),
    ])
      expect(named.has(unit), unit).toBe(true);

    const { alarms, history } = await turnCollecting(journey);
    expect(alarms, history.join("\n")).toStrictEqual([]);
    expect(
      (await confirmed(journey.dispute)).map(({ stage }) => stage).slice(-2),
    ).toStrictEqual(["close_thread", "slash_lost_defense"]);
  },
  TIMEOUT,
);
