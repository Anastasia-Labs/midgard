/**
 * `missingScriptSource` custody refuses a forged successor at every role
 * past source exhaustion: from a genuine Init and step-01, the binding and
 * source prefix advances honestly; each later planned transition is first
 * built with the thread paying another planned state of the same thread,
 * and the spending role's validator must refuse it; the honest transition
 * then advances the thread.
 */
import type { UTxO } from "@lucid-evolution/lucid";
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
  driveMissingScriptSourceThread,
  makeMissingScriptSourceHarness,
  publishMissingScriptSourceRoles,
  scriptSourceMissingVerdict,
} from "./support/missing-script-source-emulator.js";

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
  const harness = await makeMissingScriptSourceHarness();
  const references = await publishMissingScriptSourceRoles(harness);
  const scenario = await setupMissingRedeemerScenario({
    harness,
    shape,
    source,
  });
  const run = await driveMissingScriptSourceThread({
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

describe("missing-script-source forged successor refusal", () => {
  it("refuses a forged successor at every role of the accepted observer route", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [
          { script: plutusScript(1), witness: false, redeemer: true },
          { script: plutusScript(2), redeemer: true },
        ],
      },
      { kind: "normal" },
    );
    expect(new Set(refused)).toStrictEqual(
      new Set<DirectPurposeRole>([
        "familyEntry",
        "purposeSelect",
        "sourceLookup",
        "observedDecision",
        "claimStart",
        "claimAdjudicate",
      ]),
    );
  }, 2_400_000);

  it("refuses a forged successor at every role of the admitted forced spend route", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [],
        spend: missingRedeemerLedgerEntries(
          0x41,
          [null],
          scriptAddress(plutusScript(9)),
        ),
      },
      { kind: "forced", verdict: "ForcedTxValid" },
    );
    expect(new Set(refused)).toStrictEqual(
      new Set<DirectPurposeRole>([
        "familyEntry",
        "purposeSelect",
        "spendSelect",
        "spendScan",
        "sourceLookup",
        "observedDecision",
        "claimStart",
        "claimAdjudicate",
      ]),
    );
  }, 2_400_000);

  it("refuses a forged successor at every role of the forced claim route", async () => {
    const refused = await forgeEveryRole(honestForcedObserverShape(5), {
      kind: "forced",
      verdict: scriptSourceMissingVerdict(2n, 0n),
    });
    expect(new Set(refused)).toStrictEqual(
      new Set<DirectPurposeRole>([
        "familyEntry",
        "claimStart",
        "claimAdjudicate",
      ]),
    );
  }, 2_400_000);
});
