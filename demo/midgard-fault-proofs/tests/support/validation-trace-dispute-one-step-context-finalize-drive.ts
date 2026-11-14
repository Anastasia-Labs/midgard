/** The validation-trace dispute runner at the CEK context step of our trace
 * that finalizes a Plutus execution's context, on the emulator (the drive
 * of validation-trace-dispute-one-step-context-drive.ts): the finalize
 * authenticate binds the execution's redeemers control and routes the
 * thread to the finalize of its purpose, which writes the context's
 * successor for the context settle: the finalize spend from the spent
 * input's descriptor, for the plain fixture's failing spend, and the
 * finalize mint from the policy, for a failing minting policy beside a
 * passing spend. The spend's test file pins the authenticate's refusal of a
 * pending finalization whose redeemer is not the one the redeemers control
 * holds. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { validationTraceHop } from "../../src/validation-trace-dispute/one-step-builders.js";
import type { ValidationTraceContextFinalizeHolder } from "../../src/validation-trace-dispute/one-step-context-finalize.js";
import { CONTEXT_FOLLOWER_STAGES } from "../../src/validation-trace-dispute/one-step-context-followers.js";
import {
  constr,
  stepDatum,
} from "../../src/validation-trace-dispute/one-step-data.js";
import {
  FAILING_SCRIPT,
  PASSING_SCRIPT,
} from "../helpers/plutus-execution-block-fixture.js";
import {
  type ContextInstalls,
  contextStepDrive,
} from "./validation-trace-dispute-one-step-context-drive.js";

/** The last agreed state before the fixture trace's finalization. */
export const FINALIZE_STEP_FROM = 112;

/** A finalization the drive reaches: the last agreed state before it, the
 * finalize the authenticate routes it to, and what the failing transaction
 * carries for it, with the operator's last agreed state, the first CEK
 * compute step's past it. */
type Finalization = Readonly<{
  from: number;
  follower: ValidationTraceContextFinalizeHolder;
  installs?: ContextInstalls;
}>;

const FINALIZATIONS = Object.freeze({
  spend: { from: FINALIZE_STEP_FROM, follower: "contextFinalizeSpend" },
  // A passing spend, then a failing minting policy: its execution's
  // finalization follows the spend's whole execution.
  mint: {
    from: 202,
    follower: "contextFinalizeMint",
    installs: {
      failingScript: PASSING_SCRIPT,
      failingPlutusPurpose: { purpose: "mint", script: FAILING_SCRIPT },
      operatorFrom: 206,
    },
  },
} as const satisfies Readonly<Record<string, Finalization>>);

/** The drive of the successor's thread through the finalization of
 * `purpose`, by default the plain fixture's spend. */
export const contextFinalizeDrive = (
  purpose: keyof typeof FINALIZATIONS = "spend",
) => {
  const finalization: Finalization = FINALIZATIONS[purpose];
  const { from, follower } = finalization;
  const holder = "contextFinalizeAuthenticate";
  const drive = contextStepDrive({
    from,
    holder,
    path: [follower],
    labels: [
      "FinalizationPending",
      `ContextVerified ${CONTEXT_FOLLOWER_STAGES[follower]}`,
    ],
    ...(finalization.installs === undefined
      ? {}
      : { installs: finalization.installs }),
  });
  return {
    setUp: drive.setUp,

    /** Our plan routes the thread's step to the finalize authenticate, with
     * the finalization of the purpose's finalize. */
    routes: async () => {
      const plan = await drive.contextPlan((await drive.placeOfB()).utxo);
      expect(plan.stateIndex).toBe(from);
      expect(plan.context.stage).toBe("finalizeAuthenticate");
      expect(plan.context.finalization?.holder).toBe(follower);
    },

    /**
     * The finalize authenticate's hop with a redeemer summary of another
     * root in place of the one the redeemers control holds; and a check
     * that the thread has not moved.
     */
    otherRedeemer: async () => {
      const prover = drive.dispute().scene.h.proverSigner.paymentKeyHash;
      return await drive.hopFromPlace((before, plan) => {
        const honest = validationTraceHop({
          plan,
          holder,
          utxo: before.utxo,
          prover,
        });
        const staged = plutusConstrFieldCbor(before.utxo.datum!, [1, 0]);
        const { redeemerItemsCbor, redeemerCbor } = plan.context.finalization!;
        expect(honest.datum).toBe(
          stepDatum(
            prover,
            constr(0, [staged, redeemerItemsCbor, redeemerCbor]),
          ),
        );
        const redeemer = Data.from(redeemerCbor) as Constr<Data>;
        const other = Data.to(
          new Constr(0, ["00".repeat(32), ...redeemer.fields.slice(1)]),
        );
        expect(other).not.toBe(redeemerCbor);
        return {
          ...honest,
          datum: stepDatum(
            prover,
            constr(0, [staged, redeemerItemsCbor, other]),
          ),
        };
      }, follower);
    },

    awards: drive.awards,
    pays: drive.pays,
  };
};
