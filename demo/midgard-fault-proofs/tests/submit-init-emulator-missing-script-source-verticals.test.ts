/**
 * `missingScriptSource` emulator verticals from a genuine Init: an accepted
 * transaction whose observer has no source convicts and its block is
 * removed; an admitted forced spend of a script-locked output with no
 * source convicts and its block is removed; a forced rejection whose
 * ScriptSourceMissing names a sourced observer convicts; the node's own
 * ScriptSourceMissing rejection cannot reach a fraud-proof token.
 */
import {
  outRefLabel,
  screenMidgardForcedTxAdmission,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  MissingScriptSourceHonestLeaf,
  requireConvictingMissingScriptSourceProof,
} from "../src/missing-script-source/index.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  buildMissingRedeemerSubject,
  honestForcedObserverShape,
  machineForcedVerdict,
  missingRedeemerLedgerEntries,
  type MissingRedeemerSubjectShape,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  driveMissingScriptSourceThread,
  makeMissingScriptSourceHarness,
  publishMissingScriptSourceRoles,
  scriptSourceMissingVerdict,
} from "./support/missing-script-source-emulator.js";
import {
  convictAndCheck,
  expectTerminalRefusal,
  removeConvictedBlock,
} from "./support/missing-script-source-emulator.rows.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

/** A forced spend of an output locked by a Plutus script that nothing
 * sources: no witness, no reference input, no redeemer. */
const UNSOURCED_SPEND: MissingRedeemerSubjectShape = {
  observers: [],
  spend: missingRedeemerLedgerEntries(
    0x41,
    [null],
    scriptAddress(plutusScript(9)),
  ),
};

/** The honest subject: its only observer has no source anywhere. */
const SOURCELESS: MissingRedeemerSubjectShape = {
  ...honestForcedObserverShape(5),
  observers: [{ script: plutusScript(5), witness: false, redeemer: false }],
  reference: [],
};

describe("missing-script-source genuine-Init verticals", () => {
  it("A-N: convicts an accepted transaction whose required observer has no source and removes its block", async () => {
    const harness = await makeMissingScriptSourceHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: {
        observers: [
          { script: plutusScript(1), witness: false, redeemer: true },
          { script: plutusScript(2), redeemer: true },
        ],
      },
      source: { kind: "normal" },
    });
    const { run, outcome } = await convictAndCheck(harness, scenario, "A-N");
    expect(run.prepared.final.claim).toBe("NotRequired");
    expect(run.prepared.final.context.observed?.authenticated.source).toBe(
      null,
    );
    const token = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.missingScriptSource.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    expect(outRefLabel(token)).toBe(outcome.fraudProofOutRef);
    expect(Data.from(token.datum!, SDK.FraudProofTokenDatum)).toStrictEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
    });
    for (const step of harness.missingScriptSource.steps)
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          run.unit,
        ),
      ).resolves.toHaveLength(0);
    const removal = await removeConvictedBlock(harness, scenario);
    expect(removal.transactions.map((tx) => tx.kind)).toStrictEqual([
      "remove-target",
    ]);
  }, 1_200_000);

  it("A-F: convicts an admitted forced spend whose locking script has no source and removes its block", async () => {
    // The forced screen admits the subject and the node rejects it with
    // ScriptSourceMissing at spend 0; committed as admitted it is fraud.
    expect(
      screenMidgardForcedTxAdmission(
        buildMissingRedeemerSubject(UNSOURCED_SPEND).nativeTx,
      ),
    ).toBeNull();
    await expect(machineForcedVerdict(UNSOURCED_SPEND)).resolves.toStrictEqual(
      scriptSourceMissingVerdict(0n, 0n),
    );
    const harness = await makeMissingScriptSourceHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: UNSOURCED_SPEND,
      source: { kind: "forced", verdict: "ForcedTxValid" },
    });
    expect(scenario.material.bound.subject.source_kind).toBe(
      SDK.PROOF_THREAD_SOURCE_KIND_FORCED,
    );
    const { run } = await convictAndCheck(harness, scenario, "A-F");
    expect(run.prepared.final.claim).toBe("NotRequired");
    expect(
      run.prepared.final.context.observed?.authenticated.selection.coordinate,
    ).toStrictEqual({ kind: 0n, index: 0n });
    await removeConvictedBlock(harness, scenario);
  }, 1_200_000);

  it("R-F: convicts a forced rejection whose ScriptSourceMissing names a sourced observer", async () => {
    const shape = honestForcedObserverShape(5);
    const harness = await makeMissingScriptSourceHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape,
      source: { kind: "forced", verdict: scriptSourceMissingVerdict(2n, 0n) },
    });
    const { run } = await convictAndCheck(harness, scenario, "R-F false");
    expect(run.prepared.final.context.observed).toBe(null);
    expect(run.prepared.final.claim).toHaveProperty("SourcePresent");
  }, 1_200_000);

  it("R-F: refuses the node's own ScriptSourceMissing rejection", async () => {
    await expect(machineForcedVerdict(SOURCELESS)).resolves.toStrictEqual(
      scriptSourceMissingVerdict(2n, 0n),
    );
    const harness = await makeMissingScriptSourceHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: SOURCELESS,
      source: { kind: "forced", verdict: scriptSourceMissingVerdict(2n, 0n) },
    });
    await expect(
      requireConvictingMissingScriptSourceProof({
        material: scenario.material,
        contracts: harness.missingScriptSource,
        prover: harness.proverSigner.paymentKeyHash,
      }),
    ).rejects.toThrow(MissingScriptSourceHonestLeaf);
    const references = await publishMissingScriptSourceRoles(harness);
    const run = await driveMissingScriptSourceThread({
      harness,
      scenario,
      references,
    });
    // The claimed source is absent, as the leaf says.
    expect(run.prepared.final.claim).toHaveProperty("SourceAbsent");
    await expectTerminalRefusal(harness, run);
  }, 1_200_000);
});
