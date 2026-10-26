/**
 * `receivePurposeLanguage` custody refuses a forged successor at every role past
 * source exhaustion: from a genuine Init and step-01, the binding and source
 * prefix advances honestly; each later planned transition is first built
 * with the thread paying another planned state of the same thread, and the
 * spending role's validator must refuse it; the honest transition then
 * advances the thread.
 */
import { type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildDirectPurposeTransition,
  type DirectPurposeRole,
  submitDirectPurposeTransition,
} from "../src/direct-purpose-family/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  honestForcedObserverShape,
  type MissingRedeemerSubjectShape,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  driveReceivePurposeLanguageThread,
  makeReceivePurposeLanguageHarness,
  publishReceivePurposeLanguageRoles,
  receivePurposeLanguageVerdict,
} from "./support/receive-purpose-language-emulator.js";

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
  const harness = await makeReceivePurposeLanguageHarness();
  const references = await publishReceivePurposeLanguageRoles(harness);
  const scenario = await setupMissingRedeemerScenario({
    harness,
    shape,
    source,
  });
  const run = await driveReceivePurposeLanguageThread({
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

/** The family entry's observation through the shared receive route. */
const RECEIVE = [
  "familyEntry",
  "receiveStart",
  "receiveGrammar",
  "receiveOutputs",
  "receiveFinish",
  "receiveDeclaration",
  "sourceLookup",
] as const;

const receiving = (redeemer: boolean, witness = true) =>
  ({
    observers: [],
    receives: [{ script: plutusScript(3), redeemer, witness }],
  }) satisfies MissingRedeemerSubjectShape;

describe("receive-purpose-language forged successor refusal", () => {
  it("refuses a forged successor at every role of the accepted forbidden-execution route", async () => {
    const refused = await forgeEveryRole(receiving(true), { kind: "normal" });
    expect(new Set(refused)).toStrictEqual(
      new Set<DirectPurposeRole>([
        ...RECEIVE,
        "claimedRedeemerGrammar",
        "claimedRedeemerScan",
      ]),
    );
  }, 2_400_000);

  it("refuses a forged successor at the family entry of the forced contradiction route", async () => {
    const refused = await forgeEveryRole(honestForcedObserverShape(5), {
      kind: "forced",
      verdict: receivePurposeLanguageVerdict(2n, 0n),
    });
    expect(refused).toStrictEqual(["familyEntry"]);
  }, 2_400_000);

  it.each([
    ["an ordinal past the last receive purpose", receiving(false), 1n],
    ["a receive purpose with no source", receiving(false, false), 0n],
  ] as const)(
    "refuses a forged successor at every role of the forced route to %s",
    async (_label, shape, ordinal) => {
      const refused = await forgeEveryRole(shape, {
        kind: "forced",
        verdict: receivePurposeLanguageVerdict(3n, ordinal),
      });
      expect(new Set(refused)).toStrictEqual(
        new Set<DirectPurposeRole>(RECEIVE),
      );
    },
    2_400_000,
  );
});
