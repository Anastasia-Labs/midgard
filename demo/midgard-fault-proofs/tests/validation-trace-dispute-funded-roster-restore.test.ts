/** A validation-trace dispute's funding reservation across a watcher
 * restart that changes its one-step resolver roster, on the emulator. The
 * reservation journaled under one resolver roster restores under another
 * and the case opens from it; a reservation journaled under another
 * one-step boundary is not this objective's and is refused.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { beforeEach, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { drainProver } from "./support/installed-prover-wallet.js";
import {
  openFundedDispute,
  turnUntil,
  type ValidationDisputeFundingTerms,
} from "./support/validation-trace-dispute-funded-runner.js";
import {
  confirmedStages,
  FUNDED,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";

type OneStep = NonNullable<
  NonNullable<ValidationDisputeFundingTerms["moves"]>["oneStep"]
>;

/** `terms` with its one-step holders rewritten by `rewrite`. */
const withOneStep = (
  terms: ValidationDisputeFundingTerms,
  rewrite: (oneStep: OneStep) => OneStep,
): ValidationDisputeFundingTerms => {
  const oneStep = terms.moves?.oneStep;
  if (terms.moves === undefined || oneStep === undefined)
    throw new Error("the watcher's terms name no one-step holders");
  return {
    ...terms,
    moves: { ...terms.moves, oneStep: rewrite(oneStep) },
  };
};

/** A reservation's identity as the store keeps it. */
const identities = async (
  dispute: Awaited<ReturnType<typeof openFundedDispute>>,
) =>
  (await dispute.readReservations()).map((record) => {
    const { reservationId, policyDigest } = record as {
      readonly reservationId: string;
      readonly policyDigest: string;
    };
    return { reservationId, policyDigest };
  });

let scene: Scene;

describe("a validation-trace dispute reservation across a resolver roster change", () => {
  // Each case disputes the scene's failing block from a fresh deployment,
  // its prover wallet split into a collateral output and plain funding.
  beforeEach(async () => {
    scene = await setUpScene();
    await drainProver(scene.h as never, FUNDED);
  }, TIMEOUT);

  it(
    "restores a reservation journaled under a smaller resolver roster and opens the case from it",
    async () => {
      // The first process knows one resolver fewer; the restarted one knows
      // the deployment's full roster.
      const dispute = await openFundedDispute(scene, {
        disputeTerms: (terms, boot) =>
          boot === 0
            ? withOneStep(terms, (oneStep) => ({
                ...oneStep,
                resolverAddresses: oneStep.resolverAddresses.slice(1),
              }))
            : terms,
      });
      try {
        const init = await dispute.turn();
        expect("reason" in init && init.reason).toContain(
          "validation-trace-init:first",
        );
        const journaled = await identities(dispute);
        expect(journaled).toHaveLength(1);
        await dispute.restart();
        const { result, history } = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_OPEN,
        );
        expect(result.kind, history.join("\n")).toBe("pending");
        expect(confirmedStages(result)).toContain("open");
        // The same reservation funded the case, under its journaled identity.
        expect(await identities(dispute)).toStrictEqual(journaled);
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );

  it(
    "refuses to restore a reservation journaled under another one-step boundary",
    async () => {
      // The first process names the first resolver as the boundary: a
      // contract of this deployment, but not the boundary it deploys.
      const dispute = await openFundedDispute(scene, {
        disputeTerms: (terms, boot) =>
          boot === 0
            ? withOneStep(terms, (oneStep) => ({
                boundaryAddress: oneStep.resolverAddresses[0]!,
                resolverAddresses: [
                  oneStep.boundaryAddress,
                  ...oneStep.resolverAddresses.slice(1),
                ],
              }))
            : terms,
      });
      try {
        const init = await dispute.turn();
        expect("reason" in init && init.reason).toContain(
          "validation-trace-init:first",
        );
        await dispute.restart();
        await expect(dispute.turn()).rejects.toThrow(
          "restored prover funding reservation identity mismatch",
        );
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );
});
