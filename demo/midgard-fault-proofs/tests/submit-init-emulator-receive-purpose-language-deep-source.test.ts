/**
 * The `receivePurposeLanguage` source walk over a deep predecessor ledger,
 * from a genuine Init: every spent and referenced input's membership opens at
 * least five branches deep, each source page is sized to one transaction's
 * budget by the shared page fit, and an accepted PlutusV3 receive execution
 * whose source only a reference input carries convicts.
 */
import { hashMidgardVersionedScript } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  DEEP_SOURCE_REFERENCES,
  DEEP_SOURCE_SPENDS,
  deepSourceInputs,
  deepSourceLedgerDepths,
  deepSourcePageSizes,
  deepSourcePeak,
  expectDeepSourceWithinBounds,
} from "./support/direct-purpose-deep-source.js";
import {
  type MissingRedeemerSubjectShape,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  driveReceivePurposeLanguageThread,
  makeReceivePurposeLanguageHarness,
  publishReceivePurposeLanguageRoles,
  receiveCoordinate,
} from "./support/receive-purpose-language-emulator.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

/** The receiver's PlutusV3 script, sourced only from the first reference
 * input. */
const RECEIVED = plutusScript(1);

/** An accepted transaction executing the PlutusV3 receive with a redeemer
 * over the deep ledger. */
const deepSourceReceiveShape = (): MissingRedeemerSubjectShape => ({
  observers: [],
  receives: [{ script: RECEIVED, witness: false, redeemer: true }],
  ...deepSourceInputs(RECEIVED),
});

describe("receive-purpose-language source walk over a deep ledger", () => {
  it("fits each source page to the budget and convicts an accepted PlutusV3 receive execution", async () => {
    const harness = await makeReceivePurposeLanguageHarness();
    const references = await publishReceivePurposeLanguageRoles(harness);
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: deepSourceReceiveShape(),
      source: { kind: "normal" },
    });
    const ledgerDepths = await deepSourceLedgerDepths(scenario);
    expect(ledgerDepths).toHaveLength(
      DEEP_SOURCE_SPENDS + DEEP_SOURCE_REFERENCES,
    );
    expect(Math.min(...ledgerDepths)).toBeGreaterThanOrEqual(5);

    const run = await driveReceivePurposeLanguageThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(true);
    expect(
      SDK.receivePurposeLanguageFactCoordinate(run.prepared.final.claim),
    ).toStrictEqual(receiveCoordinate(0n));
    const sizes = {
      spend: deepSourcePageSizes(run.steps, 0, DEEP_SOURCE_SPENDS),
      reference: deepSourcePageSizes(run.steps, 1, DEEP_SOURCE_REFERENCES),
    };
    const source = run.steps.filter(({ label }) => /^field [01] /u.test(label));
    console.log(
      "receive-purpose-language deep source walk",
      JSON.stringify({
        ledgerDepth: [Math.min(...ledgerDepths), Math.max(...ledgerDepths)],
        pageSizes: sizes,
        sourcePeak: deepSourcePeak(source),
        peak: deepSourcePeak(run.steps),
        transactions: run.steps.length,
      }),
    );
    expectDeepSourceWithinBounds(run.steps);
    expect(sizes.spend.reduce((sum, size) => sum + size, 0)).toBe(
      DEEP_SOURCE_SPENDS,
    );
    expect(sizes.reference.reduce((sum, size) => sum + size, 0)).toBe(
      DEEP_SOURCE_REFERENCES,
    );
    const outcome = await run.terminal();
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.receivePurposeLanguage.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    // The receiver's only source is the reference input it was ground with.
    expect(
      (await run.prepared.evidence.lookup(hashMidgardVersionedScript(RECEIVED)))
        .selected,
    ).not.toBeNull();
  }, 3_600_000);
});
