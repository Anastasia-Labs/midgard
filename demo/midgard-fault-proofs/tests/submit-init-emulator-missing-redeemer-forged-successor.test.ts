/**
 * `missingRedeemer` custody refuses a forged successor at every role past
 * source exhaustion: from a genuine Init and step-01, the binding and source
 * prefix advances honestly; each later planned transition is first built with
 * the thread paying another planned state of the same thread, and the
 * spending role's validator must refuse it; the honest transition then
 * advances the thread.
 */
import type { UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildMissingRedeemerTransition,
  type MissingRedeemerRole,
  submitMissingRedeemerTransition,
} from "../src/missing-redeemer/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveMissingRedeemerThread,
  makeMissingRedeemerHarness,
  publishMissingRedeemerRoles,
} from "./support/missing-redeemer-emulator.js";
import {
  plutusScript,
  redeemerMissingVerdict,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";

type Scenario = Parameters<typeof setupMissingRedeemerScenario>[0];

/** The shared binding and source prefix, which locates its own successor. */
const PREFIX = new Set<MissingRedeemerRole>([
  "bindEvent",
  "bindPreState",
  "sourceStart",
  "sourceResolved",
  "sourceAuthenticate",
  "sourceMap",
  "sourceInputFinish",
  "sourceGrammar",
  "sourceWitnesses",
  "sourceFinish",
]);

/** Forges then advances every transition past the prefix; returns the
 * roles that refused a forgery. */
const forgeEveryRole = async (
  shape: Scenario["shape"],
  source: Scenario["source"],
): Promise<readonly MissingRedeemerRole[]> => {
  const harness = await makeMissingRedeemerHarness();
  const references = await publishMissingRedeemerRoles(harness);
  const scenario = await setupMissingRedeemerScenario({
    harness,
    shape,
    source,
  });
  const run = await driveMissingRedeemerThread({
    harness,
    scenario,
    references,
    transitions: () => [],
  });
  const planned = run.prepared.transitions;
  let thread: UTxO = run.thread;
  const refused: MissingRedeemerRole[] = [];
  for (const [index, transition] of planned.entries()) {
    if (PREFIX.has(transition.role)) {
      thread = (
        await submitMissingRedeemerTransition(run.custody, thread, transition)
      ).thread;
      continue;
    }
    // The nearest later planned state that differs from the honest one.
    const forged = [...planned.slice(index + 1), ...planned.slice(0, index)]
      .map(({ datum }) => datum)
      .find((datum) => datum !== transition.datum);
    if (forged === undefined)
      throw new Error(`${transition.label}: no distinct planned state`);
    const built = await buildMissingRedeemerTransition(
      run.custody,
      thread,
      transition,
      { datum: forged },
    );
    await expectOnchainRefusal(() => built.complete());
    thread = (
      await submitMissingRedeemerTransition(run.custody, thread, transition)
    ).thread;
    refused.push(transition.role);
  }
  return refused;
};

describe("missing-redeemer forged successor refusal", () => {
  it("refuses a forged successor at every role of the accepted observer route", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [
          { script: plutusScript(1), redeemer: false },
          { script: plutusScript(2), redeemer: true },
        ],
      },
      { kind: "normal" },
    );
    expect(new Set(refused)).toStrictEqual(
      new Set<MissingRedeemerRole>([
        "familyEntry",
        "purposeSelect",
        "sourceLookup",
        "observedStart",
        "observedGrammar",
        "observedScan",
        "claimStart",
        "claimAdjudicate",
      ]),
    );
  }, 2_400_000);

  it("refuses a forged successor at every role of the forced claimed-redeemer route", async () => {
    const refused = await forgeEveryRole(
      { observers: [{ script: plutusScript(5), redeemer: false }] },
      { kind: "forced", verdict: redeemerMissingVerdict(2n, 0n) },
    );
    expect(new Set(refused)).toStrictEqual(
      new Set<MissingRedeemerRole>([
        "familyEntry",
        "claimStart",
        "claimAdjudicate",
        "claimedRedeemerGrammar",
        "claimedRedeemerScan",
      ]),
    );
  }, 2_400_000);
});
