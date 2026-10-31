/** The validation-trace dispute runner at the CEK context step of our trace
 * that the observer authenticate verifies, on the emulator (the drive of
 * validation-trace-dispute-one-step-context-drive.ts): the observer
 * authenticate binds the step's observer opening, and the observer fold
 * folds it into the context's successor for the context settle. The test
 * file pins the observer authenticate's refusal of an opening other than
 * the one the step's transaction opens. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { expect } from "vitest";

import { validationTraceHop } from "../../src/validation-trace-dispute/one-step-builders.js";
import {
  constr,
  stepDatum,
} from "../../src/validation-trace-dispute/one-step-data.js";
import { contextStepDrive } from "./validation-trace-dispute-one-step-context-drive.js";

/** The last agreed state before the fixture trace's observer step: its
 * transaction requires no observers. */
export const OBSERVER_STEP_FROM = 104;

/** `ObserverOpening`'s EmptyObservers and FinishedObservers. */
const EMPTY_OBSERVERS = constr(0, []);
const FINISHED_OBSERVERS = constr(1, []);

/** The drive of the successor's thread through the observer step. */
export const contextObserverDrive = () => {
  const holder = "contextObserverAuthenticate";
  const drive = contextStepDrive({
    from: OBSERVER_STEP_FROM,
    holder,
    path: ["contextObserverFold"],
    labels: ["ObserverPending", "ContextVerified observerFold"],
  });
  return {
    setUp: drive.setUp,

    /** Our plan routes the thread's step to the observer authenticate, with
     * the opening of a transaction that requires no observers. */
    routes: async () => {
      const plan = await drive.contextPlan((await drive.placeOfB()).utxo);
      expect(plan.stateIndex).toBe(OBSERVER_STEP_FROM);
      expect(plan.context.stage).toBe("observerAuthenticate");
      expect(plan.context.observerOpeningCbor).toBe(EMPTY_OBSERVERS);
    },

    /**
     * The observer authenticate's hop with the opening of finished observers
     * in place of the absent observers the step's transaction opens; and a
     * check that the thread has not moved.
     */
    finishedOpening: async () => {
      const prover = drive.dispute().scene.h.proverSigner.paymentKeyHash;
      return await drive.hopFromPlace((before, plan) => {
        const honest = validationTraceHop({
          plan,
          holder,
          utxo: before.utxo,
          prover,
        });
        const staged = plutusConstrFieldCbor(before.utxo.datum!, [1, 0]);
        expect(honest.datum).toBe(
          stepDatum(prover, constr(0, [staged, EMPTY_OBSERVERS])),
        );
        return {
          ...honest,
          datum: stepDatum(prover, constr(0, [staged, FINISHED_OBSERVERS])),
        };
      }, "contextObserverFold");
    },

    awards: drive.awards,
    pays: drive.pays,
  };
};
