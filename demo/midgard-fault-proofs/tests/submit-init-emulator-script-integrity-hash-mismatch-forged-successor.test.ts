/**
 * `scriptIntegrityHashMismatch` custody refuses a forged successor at every
 * role past source exhaustion: from a genuine Init and step-01, the binding
 * and source prefix advances honestly; each later planned transition is first
 * built with the thread paying another planned state of the same thread, and
 * the spending role's validator must refuse it; the honest transition then
 * advances the thread.
 */
import type { UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildScriptIntegrityHashMismatchTransition,
  type ScriptIntegrityHashMismatchRole,
  submitScriptIntegrityHashMismatchTransition,
} from "../src/script-integrity-hash-mismatch/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveScriptIntegrityHashMismatchThread,
  makeScriptIntegrityHashMismatchHarness,
  publishScriptIntegrityHashMismatchRoles,
} from "./support/script-integrity-hash-mismatch-emulator.js";
import {
  expectedHash,
  nativeScript,
  plutusScript,
  rejectedFor,
  scriptLockedEntries,
  setupIntegrityScenario,
  WRONG_HASH,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";

type Scenario = Parameters<typeof setupIntegrityScenario>[0];

/** The shared binding and source prefix, which locates its own successor. */
const PREFIX = new Set<ScriptIntegrityHashMismatchRole>([
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
): Promise<readonly ScriptIntegrityHashMismatchRole[]> => {
  const harness = await makeScriptIntegrityHashMismatchHarness();
  const references = await publishScriptIntegrityHashMismatchRoles(harness);
  const scenario = await setupIntegrityScenario({
    harness,
    shape,
    source,
  });
  const run = await driveScriptIntegrityHashMismatchThread({
    harness,
    scenario,
    references,
    transitions: () => [],
  });
  const planned = run.prepared.transitions;
  let thread: UTxO = run.thread;
  const refused: ScriptIntegrityHashMismatchRole[] = [];
  for (const [index, transition] of planned.entries()) {
    if (PREFIX.has(transition.role)) {
      thread = (
        await submitScriptIntegrityHashMismatchTransition(
          run.custody,
          thread,
          transition,
        )
      ).thread;
      continue;
    }
    // The nearest later planned state that differs from the honest one.
    const forged = [...planned.slice(index + 1), ...planned.slice(0, index)]
      .map(({ datum }) => datum)
      .find((datum) => datum !== transition.datum);
    if (forged === undefined)
      throw new Error(`${transition.label}: no distinct planned state`);
    const built = await buildScriptIntegrityHashMismatchTransition(
      run.custody,
      thread,
      transition,
      { datum: forged },
    );
    await expectOnchainRefusal(() => built.complete());
    thread = (
      await submitScriptIntegrityHashMismatchTransition(
        run.custody,
        thread,
        transition,
      )
    ).thread;
    refused.push(transition.role);
  }
  return refused;
};

const WALK = new Set<ScriptIntegrityHashMismatchRole>([
  "familyEntry",
  "allPurposesSpend",
  "allPurposesSpendResolve",
  "allPurposesMintGrammar",
  "allPurposesMint",
  "allPurposesObserveOpen",
  "allPurposesObserve",
  "allPurposesReceiveGrammar",
  "allPurposesReceive",
]);

describe("script-integrity-hash-mismatch forged successor refusal", () => {
  it("refuses a forged successor at every walk role of an accepted transaction", async () => {
    const spent = plutusScript(21);
    const refused = await forgeEveryRole(
      {
        spend: scriptLockedEntries(0x51, [spent]),
        witnesses: [spent],
        spendRedeemers: [0n],
        mints: [{ script: plutusScript(22), redeemer: true }],
        observers: [{ script: nativeScript(3n), redeemer: false }],
        receives: [{ script: plutusScript(23), redeemer: true }],
        integrity: WRONG_HASH,
      },
      { kind: "normal" },
    );
    expect(new Set(refused)).toStrictEqual(WALK);
  }, 2_400_000);

  it("refuses a forged successor at every walk role of a forced rejection", async () => {
    const refused = await forgeEveryRole(
      {
        mints: [{ script: plutusScript(24), redeemer: true }],
        observers: [{ script: plutusScript(25), redeemer: true }],
        receives: [{ script: plutusScript(26), redeemer: true }],
        integrity: expectedHash("PlutusV3"),
      },
      rejectedFor("ScriptIntegrityHashMismatch"),
    );
    expect(new Set(refused)).toStrictEqual(
      new Set(
        [...WALK].filter(
          (role) =>
            role !== "allPurposesSpendResolve" && role !== "allPurposesSpend",
        ),
      ),
    );
  }, 2_400_000);
});
