/** The prover funding permit's walk over our staging of a staged builtin
 * step's operands, as the runtime bound walks it: the shape it admits, and
 * each shape beside it, refused. A staging pays one output at our staging
 * address holding its run's exact datum and deposit, and our wallet's
 * change, from our wallet's coins only, and leaves the reservation holding
 * what the rest of its step needs, or raises the operator's alarm. */
import { describe, expect, it } from "vitest";

import {
  VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
  ValidationDisputeWalletMoveUnfundedError,
} from "../src/workflow/funding-reservation-permit.validation-dispute-wallet-move-reserve.js";
import {
  ADA,
  CASE_UNIT,
  DEADLINE_MS,
  depositOf,
  LAST,
  NEXT,
  OTHER_STAGING,
  OUR_SCRIPT,
  ours,
  outputAt,
  RESERVE,
  RUN,
  STAGED_OUT_REF,
  stagedOutput,
  STAGING,
  stagingShape,
  theirs,
  THREAD_OUT_REF,
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
    walk.keepStepReserve(depositOf(RUN) + ADA);
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

  it("keeps, from the plain coins a staging leaves, the deposits of the step's runs still to stage and a wallet-paid move for each of them and each loop hop of the step, and otherwise raises the operator's alarm naming the case, the thread and the deadline", () => {
    const spent = depositOf(RUN) + ADA;
    for (const hops of [0, 2]) {
      const reserve =
        depositOf(NEXT) + depositOf(LAST) + BigInt(2 + hops) * RESERVE;
      walkOf(stagingShape({ walletPaidHops: hops }), {
        plain: spent + reserve,
      }).keepStepReserve(spent);
      const short = () =>
        walkOf(stagingShape({ walletPaidHops: hops }), {
          plain: spent + reserve - 1n,
        }).keepStepReserve(spent);
      expect(short).toThrow(ValidationDisputeWalletMoveUnfundedError);
      expect(short).toThrow(
        `${VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED}: the prover wallet cannot fund validation-trace-stage:run:0 on thread ${THREAD_OUT_REF} of case ${CASE_UNIT}, which must land before its deadline ${DEADLINE_MS.toString()};`,
      );
    }
    // The step's last run keeps only its loop hops.
    walkOf(stagingShape({ remainingDatumCborHexes: [] }), {
      plain: spent,
    }).keepStepReserve(spent);
  });
});
