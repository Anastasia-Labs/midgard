/**
 * The direct purpose source walk over a deep predecessor ledger, from a
 * genuine Init: sixteen spend inputs and eight reference inputs, each output
 * carrying its own reference script, are ground into one ledger-trie path
 * cluster behind a full branch at each shared digit, so every input's
 * membership opens at least five branches deep. Each source page is sized
 * to one transaction's budget by the shared page fit, and a missing redeemer
 * at a reference-sourced observer convicts.
 */
import { hashMidgardVersionedScript } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  DEEP_SOURCE_REFERENCES as REFERENCES,
  DEEP_SOURCE_SPENDS as SPENDS,
  deepSourceInputs,
  deepSourceLedgerDepths,
  deepSourcePageSizes as pageSizes,
  deepSourcePeak as peak,
  expectDeepSourceWithinBounds as expectWithinBounds,
} from "./support/direct-purpose-deep-source.js";
import {
  driveMissingRedeemerThread,
  makeMissingRedeemerHarness,
  publishMissingRedeemerRoles,
} from "./support/missing-redeemer-emulator.js";
import {
  type MissingRedeemerSubjectShape,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

/** The observer's Plutus script, sourced only from the first reference
 * input. */
const OBSERVED = plutusScript(1);

const deepSourceShape = (): MissingRedeemerSubjectShape => ({
  observers: [{ script: OBSERVED, witness: false, redeemer: false }],
  ...deepSourceInputs(OBSERVED),
});

describe("direct purpose source walk over a deep ledger", () => {
  it("fits each source page to the budget and convicts a missing observer redeemer", async () => {
    const harness = await makeMissingRedeemerHarness();
    const references = await publishMissingRedeemerRoles(harness);
    const shape = deepSourceShape();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape,
      source: { kind: "normal" },
    });
    const ledgerDepths = await deepSourceLedgerDepths(scenario);
    expect(ledgerDepths).toHaveLength(SPENDS + REFERENCES);
    expect(Math.min(...ledgerDepths)).toBeGreaterThanOrEqual(5);

    const run = await driveMissingRedeemerThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(true);
    const sizes = {
      spend: pageSizes(run.steps, 0, SPENDS),
      reference: pageSizes(run.steps, 1, REFERENCES),
    };
    const source = run.steps.filter(({ label }) => /^field [01] /u.test(label));
    console.log(
      "direct purpose deep source walk",
      JSON.stringify({
        ledgerDepth: [Math.min(...ledgerDepths), Math.max(...ledgerDepths)],
        pageSizes: sizes,
        sourcePeak: peak(source),
        peak: peak(run.steps),
        transactions: run.steps.length,
      }),
    );
    expectWithinBounds(run.steps);
    expect(sizes.spend.reduce((sum, size) => sum + size, 0)).toBe(SPENDS);
    expect(sizes.reference.reduce((sum, size) => sum + size, 0)).toBe(
      REFERENCES,
    );
    const outcome = await run.terminal();
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.missingRedeemer.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    // The observer's only source is the reference input it was ground with.
    expect(
      (await run.prepared.evidence.lookup(hashMidgardVersionedScript(OBSERVED)))
        .selected,
    ).not.toBeNull();
  }, 3_600_000);
});
