/** The prover funding permit's walk over our staging of a staged builtin
 * step's operands, as the runtime bound walks it: the shape it admits, and
 * each shape beside it, refused. A staging pays one output at our staging
 * address holding its run's exact datum and deposit, and our wallet's
 * change, from our wallet's coins only. */
import { describe, expect, it } from "vitest";

import {
  ADA,
  depositOf,
  NEXT,
  OTHER_STAGING,
  OUR_SCRIPT,
  ours,
  outputAt,
  RUN,
  STAGED_OUT_REF,
  stagedOutput,
  STAGING,
  stagingShape,
  theirs,
  walkOf,
  witnessesOf,
} from "./support/validation-trace-dispute-staging-funding.js";

describe("the prover funding permit over our staging", () => {
  it("admits a staging's one output at our staging address, holding its run's exact inline datum and deposit, and our wallet's change, from our wallet's coins with no script witness", () => {
    const walk = walkOf(stagingShape());
    expect(walk.admitsScriptWitnesses(witnessesOf([]))).toBe(false);
    expect(
      walk.output(outputAt(STAGING, depositOf(RUN), { datum: RUN })),
    ).toStrictEqual({ custody: depositOf(RUN), allocation: depositOf(RUN) });
    expect(walk.output(outputAt(ours.address, 5n * ADA))).toBeNull();
    walk.assertComplete();
  });

  it("refuses a staging output at another key's staging address", () => {
    expect(() =>
      walkOf(stagingShape()).output(
        outputAt(OTHER_STAGING, depositOf(RUN), { datum: RUN }),
      ),
    ).toThrow("besides its staging output and our wallet");
  });

  it("refuses a staging output at any other address", () => {
    expect(() =>
      walkOf(stagingShape()).output(
        outputAt(theirs.address, depositOf(RUN), { datum: RUN }),
      ),
    ).toThrow("besides its staging output and our wallet");
  });

  it("refuses a staging output holding another run's datum", () => {
    expect(() =>
      walkOf(stagingShape()).output(
        outputAt(STAGING, depositOf(RUN), { datum: NEXT }),
      ),
    ).toThrow("differs from its exact run");
  });

  it("refuses a staging output holding its run's datum by hash", () => {
    expect(() =>
      walkOf(stagingShape()).output(
        outputAt(STAGING, depositOf(RUN), { datum: RUN, byHash: true }),
      ),
    ).toThrow("differs from its exact run");
  });

  it("refuses a staging output holding more than its run's deposit", () => {
    expect(() =>
      walkOf(stagingShape()).output(
        outputAt(STAGING, depositOf(RUN) + 1n, { datum: RUN }),
      ),
    ).toThrow("differs from its exact run");
  });

  it("refuses a staging output holding a token", () => {
    expect(() =>
      walkOf(stagingShape()).output(
        outputAt(STAGING, depositOf(RUN), { datum: RUN, token: true }),
      ),
    ).toThrow("differs from its exact run");
  });

  it("refuses a staging output carrying a reference script", () => {
    expect(() =>
      walkOf(stagingShape()).output(
        outputAt(STAGING, depositOf(RUN), { datum: RUN, scriptRef: true }),
      ),
    ).toThrow("differs from its exact run");
  });

  it("refuses a second staging output", () => {
    const walk = walkOf(stagingShape());
    walk.output(outputAt(STAGING, depositOf(RUN), { datum: RUN }));
    expect(() =>
      walk.output(outputAt(STAGING, depositOf(RUN), { datum: RUN })),
    ).toThrow("differs from its exact run");
  });

  it("refuses a staging that pays no staging output", () => {
    const walk = walkOf(stagingShape());
    walk.output(outputAt(ours.address, 5n * ADA));
    expect(() => walk.assertComplete()).toThrow("omits its staging output");
  });

  it("refuses a staging that spends any input besides our wallet's coins", () => {
    expect(() =>
      walkOf(stagingShape()).admitInput(STAGED_OUT_REF, stagedOutput),
    ).toThrow("besides our wallet's coins");
  });

  it("refuses a staging carrying our native script", () => {
    expect(
      walkOf(stagingShape()).admitsScriptWitnesses(witnessesOf([OUR_SCRIPT])),
    ).toBe(false);
  });
});
