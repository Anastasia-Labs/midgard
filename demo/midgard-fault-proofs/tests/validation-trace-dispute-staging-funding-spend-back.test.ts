/** The prover funding permit's walk over our spend back of a staging
 * output of a staged builtin step's operands, as the runtime bound walks
 * it: the shape it admits, and each shape beside it, refused. A spend back
 * spends our one staging output that a staging of ours left, with our one
 * native script, to our wallet only. */
import { midgardCekStagingScript } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { exactResolvedOutputCbor } from "../src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import type { ValidationDisputeStagingShape } from "../src/workflow/funding-reservation-permit.validation-dispute-staging-authority.js";
import {
  ADA,
  depositOf,
  KEY,
  OTHER_STAGING,
  OUR_SCRIPT,
  ours,
  outputAt,
  reclaimShape,
  RUN,
  STAGED_OUT_REF,
  stagedOutput,
  STAGING,
  stagingLineage,
  THEIR_SCRIPT,
  theirs,
  walkOf,
  witnessesOf,
} from "./support/validation-trace-dispute-staging-funding.js";

describe("the prover funding permit over our spend back of a staging output", () => {
  it("admits a spend back of our staging output that a staging of ours left, with our one native script, paying our wallet", () => {
    const walk = walkOf(reclaimShape);
    expect(walk.admitsScriptWitnesses(witnessesOf([OUR_SCRIPT]))).toBe(true);
    walk.admitInput(STAGED_OUT_REF, stagedOutput);
    expect(walk.output(outputAt(ours.address, depositOf(RUN)))).toBeNull();
    walk.assertComplete();
  });

  it("refuses a spend back carrying another key's native script, a second native script, a Plutus script or none", () => {
    const walk = walkOf(reclaimShape);
    expect(walk.admitsScriptWitnesses(witnessesOf([THEIR_SCRIPT]))).toBe(false);
    expect(
      walk.admitsScriptWitnesses(witnessesOf([OUR_SCRIPT, THEIR_SCRIPT])),
    ).toBe(false);
    expect(
      walk.admitsScriptWitnesses(witnessesOf([OUR_SCRIPT], { plutus: true })),
    ).toBe(false);
    expect(walk.admitsScriptWitnesses(witnessesOf([]))).toBe(false);
  });

  it("refuses a spend back of any other output", () => {
    const other = { ...stagedOutput, outputIndex: 1 };
    expect(() =>
      walkOf(reclaimShape).admitInput(`${other.txHash}#1`, other),
    ).toThrow("besides its staging output");
    const changed = { ...stagedOutput, assets: { lovelace: 7n * ADA } };
    expect(() =>
      walkOf(reclaimShape).admitInput(STAGED_OUT_REF, changed),
    ).toThrow("besides its staging output");
  });

  it("refuses a spend back of an output that is not a staging run at our staging address, even with a staging lineage of its own", () => {
    const tokenUnit = `${"00".repeat(28)}01`;
    for (const output of [
      { ...stagedOutput, address: OTHER_STAGING },
      { ...stagedOutput, datum: "d87980" },
      { ...stagedOutput, scriptRef: midgardCekStagingScript(KEY) },
      {
        ...stagedOutput,
        assets: { lovelace: depositOf(RUN) + ADA, [tokenUnit]: 1n },
      },
    ]) {
      const shape: ValidationDisputeStagingShape = {
        kind: "reclaim_staging",
        outRef: STAGED_OUT_REF,
        resolvedOutputCborHex: exactResolvedOutputCbor(output),
      };
      expect(() =>
        walkOf(shape, { lineage: stagingLineage(output) }).admitInput(
          STAGED_OUT_REF,
          output,
        ),
      ).toThrow("besides its staging output");
    }
  });

  it("refuses a spend back spending its staging output twice", () => {
    const walk = walkOf(reclaimShape);
    walk.admitInput(STAGED_OUT_REF, stagedOutput);
    expect(() => walk.admitInput(STAGED_OUT_REF, stagedOutput)).toThrow(
      "besides its staging output",
    );
  });

  it("refuses a spend back of an output no staging of ours left", () => {
    for (const lineage of [
      null,
      stagingLineage(stagedOutput, "open"),
      stagingLineage({ ...stagedOutput, assets: { lovelace: 7n * ADA } }),
      stagingLineage({ ...stagedOutput, outputIndex: 1 }),
    ])
      expect(() => walkOf(reclaimShape, { lineage })).toThrow(
        "lacks the lineage of a staging of ours",
      );
  });

  it("refuses a spend back paying anywhere but our wallet", () => {
    for (const address of [STAGING, OTHER_STAGING, theirs.address])
      expect(() =>
        walkOf(reclaimShape).output(
          outputAt(address, depositOf(RUN), { datum: RUN }),
        ),
      ).toThrow("besides its staging output and our wallet");
  });

  it("refuses a spend back that spends no staging output", () => {
    expect(() => walkOf(reclaimShape).assertComplete()).toThrow(
      "omits its staging output",
    );
  });
});
