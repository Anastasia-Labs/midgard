/** The validation-trace dispute runner at the CEK context step of our trace
 * that finalizes a Cardano spend, on the emulator (the drive of
 * validation-trace-dispute-one-step-context-drive.ts): the finalize
 * authenticate binds the spend's redeemers control, and the finalize spend
 * writes the context's successor from the spent input's descriptor for the
 * context settle. The test file pins the authenticate's refusal of a pending
 * finalization whose redeemer is not the one the redeemers control holds. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { validationTraceHop } from "../../src/validation-trace-dispute/one-step-builders.js";
import {
  constr,
  stepDatum,
} from "../../src/validation-trace-dispute/one-step-data.js";
import { contextStepDrive } from "./validation-trace-dispute-one-step-context-drive.js";

/** The last agreed state before the fixture trace's finalization. */
export const FINALIZE_STEP_FROM = 112;

/** The drive of the successor's thread through the finalization. */
export const contextFinalizeDrive = () => {
  const holder = "contextFinalizeAuthenticate";
  const drive = contextStepDrive({
    from: FINALIZE_STEP_FROM,
    holder,
    path: ["contextFinalizeSpend"],
    labels: ["FinalizationPending", "ContextVerified finalizeSpend"],
  });
  return {
    setUp: drive.setUp,

    /** Our plan routes the thread's step to the finalize authenticate, with
     * the finalization of a Cardano spend. */
    routes: async () => {
      const plan = await drive.contextPlan((await drive.placeOfB()).utxo);
      expect(plan.stateIndex).toBe(FINALIZE_STEP_FROM);
      expect(plan.context.stage).toBe("finalizeAuthenticate");
      expect(plan.context.finalization).toBeDefined();
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
      }, "contextFinalizeSpend");
    },

    awards: drive.awards,
    pays: drive.pays,
  };
};
