/**
 * `unusedRedeemer` custody refuses a forged successor at every role past
 * source exhaustion: from a genuine Init and step-01, the binding and source
 * prefix advances honestly; each later planned transition is first built
 * with the thread paying another planned state of the same thread, and the
 * spending role's validator must refuse it; the honest transition then
 * advances the thread.
 */
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildDirectPurposeTransition,
  type DirectPurposeRole,
  submitDirectPurposeTransition,
} from "../src/direct-purpose-family/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  honestForcedObserverShape,
  missingRedeemerLedgerEntries,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  driveUnusedRedeemerThread,
  makeUnusedRedeemerHarness,
  publishUnusedRedeemerRoles,
  unusedRedeemerVerdict,
} from "./support/unused-redeemer-emulator.js";

type Scenario = Parameters<typeof setupMissingRedeemerScenario>[0];

/** The shared binding and source prefix, which locates its own successor. */
const PREFIX = new Set<DirectPurposeRole>([
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
): Promise<readonly DirectPurposeRole[]> => {
  const harness = await makeUnusedRedeemerHarness();
  const references = await publishUnusedRedeemerRoles(harness);
  const scenario = await setupMissingRedeemerScenario({
    harness,
    shape,
    source,
  });
  const run = await driveUnusedRedeemerThread({
    harness,
    scenario,
    references,
    transitions: () => [],
  });
  const planned = run.prepared.transitions;
  let thread: UTxO = run.thread;
  const refused: DirectPurposeRole[] = [];
  for (const [index, transition] of planned.entries()) {
    if (PREFIX.has(transition.role)) {
      thread = (
        await submitDirectPurposeTransition(run.custody, thread, transition)
      ).thread;
      continue;
    }
    // The nearest later planned state that differs from the honest one.
    const forged = [...planned.slice(index + 1), ...planned.slice(0, index)]
      .map(({ datum }) => datum)
      .find((datum) => datum !== transition.datum);
    if (forged === undefined)
      throw new Error(`${transition.label}: no distinct planned state`);
    const built = await buildDirectPurposeTransition(
      run.custody,
      thread,
      transition,
      { datum: forged },
    );
    await expectOnchainRefusal(() => built.complete());
    thread = (
      await submitDirectPurposeTransition(run.custody, thread, transition)
    ).thread;
    refused.push(transition.role);
  }
  return refused;
};

const DATA = Buffer.from(Data.to(0n), "hex");

const WALK = ["familyEntry", "redeemerGrammar", "redeemerScan"] as const;

describe("unused-redeemer forged successor refusal", () => {
  it("refuses a forged successor at every role of the accepted purposeless-redeemer route", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [{ script: plutusScript(1), redeemer: true }],
        paddingRedeemers: [DATA],
      },
      { kind: "normal" },
    );
    expect(new Set(refused)).toStrictEqual(
      new Set<DirectPurposeRole>([...WALK, "claimStart", "claimAdjudicate"]),
    );
  }, 2_400_000);

  it("refuses a forged successor at every role of the accepted repeated-pointer route", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [],
        spend: missingRedeemerLedgerEntries(
          0x41,
          [null],
          scriptAddress(plutusScript(9)),
        ),
        witnesses: [plutusScript(9)],
        spendRedeemers: [0n, 0n],
      },
      { kind: "normal" },
    );
    expect(new Set(refused)).toStrictEqual(new Set<DirectPurposeRole>(WALK));
  }, 2_400_000);

  it("refuses a forged successor at every role of the forced out-of-range route", async () => {
    const refused = await forgeEveryRole(
      {
        ...honestForcedObserverShape(5),
        observers: [
          { script: plutusScript(5), witness: false, redeemer: false },
        ],
        reference: [],
      },
      { kind: "forced", verdict: unusedRedeemerVerdict(0n) },
    );
    expect(new Set(refused)).toStrictEqual(new Set<DirectPurposeRole>(WALK));
  }, 2_400_000);
});
