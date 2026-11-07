/**
 * `missingScriptSource` genuine-Init rows under a recorded reason other
 * than the missing source: a purpose-free reason the purpose phase
 * precedes, or a RedeemerMissing at a later observer, convicts through the
 * observed missing source; a reason before the purpose phase reaches the
 * terminal and is refused.
 */
import type * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  MissingScriptSourceHonestLeaf,
  requireConvictingMissingScriptSourceProof,
} from "../src/missing-script-source/index.js";
import {
  honestForcedObserverShape,
  missingRedeemerLedgerEntries,
  type MissingRedeemerSubjectShape,
  plutusScript,
  redeemerMissingVerdict,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  driveMissingScriptSourceThread,
  makeMissingScriptSourceHarness,
  publishMissingScriptSourceRoles,
} from "./support/missing-script-source-emulator.js";
import {
  convictAndCheck,
  expectTerminalRefusal,
} from "./support/missing-script-source-emulator.rows.js";

const signed = honestForcedObserverShape(5);

/** The honest subject: its only observer has no source anywhere. */
const SOURCELESS: MissingRedeemerSubjectShape = {
  ...signed,
  observers: [{ script: plutusScript(5), witness: false, redeemer: false }],
  reference: [],
};

/** plutusScript(11) orders before plutusScript(12): observer 0 has no
 * source, observer 1 is sourced by a reference input and has no redeemer. */
const SOURCELESS_THEN_REDEEMERLESS: MissingRedeemerSubjectShape = {
  ...signed,
  observers: [11, 12].map((seed) => ({
    script: plutusScript(seed),
    witness: false,
    redeemer: false,
  })),
  reference: missingRedeemerLedgerEntries(0x31, [plutusScript(12)]),
};

const rejected = (reason: SDK.RejectionReason): SDK.OperatorVerdict => ({
  ForcedTxInvalid: { reason },
});

describe("missing-script-source genuine-Init rows under a later recorded reason", () => {
  it.each([
    [
      "PlutusExecutionFailed",
      SOURCELESS,
      rejected({
        PlutusExecutionFailed: { purpose_kind: 2n, purpose_index: 0n },
      }),
    ],
    [
      "RedeemerMissing at a later observer",
      SOURCELESS_THEN_REDEEMERLESS,
      redeemerMissingVerdict(2n, 1n),
    ],
  ] as const)(
    "reaches a fraud-proof token for a missing source under a recorded %s",
    async (label, shape, verdict) => {
      const harness = await makeMissingScriptSourceHarness();
      const scenario = await setupMissingRedeemerScenario({
        harness,
        shape,
        source: { kind: "forced", verdict },
      });
      const { run } = await convictAndCheck(harness, scenario, label);
      const observed = run.prepared.final.context.observed;
      expect(observed?.authenticated.source).toBe(null);
      expect(observed?.authenticated.selection.coordinate).toStrictEqual({
        kind: 2n,
        index: 0n,
      });
    },
    1_200_000,
  );

  it("refuses a missing source under a reason recorded before the purpose phase", async () => {
    const harness = await makeMissingScriptSourceHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: SOURCELESS,
      source: {
        kind: "forced",
        verdict: rejected("ScriptIntegrityHashMissing"),
      },
    });
    await expect(
      requireConvictingMissingScriptSourceProof({
        material: scenario.material,
        contracts: harness.missingScriptSource,
        prover: harness.proverSigner.paymentKeyHash,
      }),
    ).rejects.toThrow(MissingScriptSourceHonestLeaf);
    const references = await publishMissingScriptSourceRoles(harness);
    // The prover finds no route; the observed route is forced through the
    // missing source to show the terminal refuse it.
    const run = await driveMissingScriptSourceThread({
      harness,
      scenario,
      references,
      observed: { kind: 2n, index: 0n },
    });
    expect(run.prepared.final.claim).toBe("NotRequired");
    expect(run.prepared.final.context.observed?.authenticated.source).toBe(
      null,
    );
    await expectTerminalRefusal(harness, run);
  }, 1_200_000);
});
