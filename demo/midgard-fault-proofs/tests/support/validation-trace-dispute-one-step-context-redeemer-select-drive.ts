/** The validation-trace dispute runner at the CEK context step of our trace
 * that selects a redeemer, on the emulator (the drive of
 * validation-trace-dispute-one-step-context-drive.ts): the redeemer select
 * authenticate binds the select of the redeemer the execution at the
 * purpose frontier names, the initialize summarises its purpose, the hash
 * opens its item scan and the finish writes the context's successor for the
 * context settle. The test file pins the authenticate's refusal of a select
 * whose purpose frontier it did not move. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { expect } from "vitest";

import { validationTraceHop } from "../../src/validation-trace-dispute/one-step-builders.js";
import {
  constr,
  stepDatum,
} from "../../src/validation-trace-dispute/one-step-data.js";
import { contextStepDrive } from "./validation-trace-dispute-one-step-context-drive.js";

/** The last agreed state before the fixture trace's redeemer select. */
export const REDEEMER_SELECT_STEP_FROM = 106;

/** `RedeemerSelectionPending`'s purpose frontier, past its staged context. */
const PURPOSE_FRONTIER = 6;

/** `CekRedeemerContextControlV1`'s purpose bound. */
const CONTROL_PURPOSE_BOUND = 6;

/** The drive of the successor's thread through the redeemer select. */
export const contextRedeemerSelectDrive = () => {
  const holder = "contextRedeemerSelectAuthenticate";
  const drive = contextStepDrive({
    from: REDEEMER_SELECT_STEP_FROM,
    holder,
    path: [
      "contextRedeemerSelectInitialize",
      "contextRedeemerSelectHash",
      "contextRedeemerSelectFinish",
    ],
    labels: [
      "RedeemerSelectionPending",
      "RedeemerPurposePending",
      "RedeemerInitialPending",
      "ContextVerified redeemerSelectFinish",
    ],
  });
  return {
    setUp: drive.setUp,

    /** Our plan routes the thread's step to the redeemer select
     * authenticate, with a select of a redeemer. */
    routes: async () => {
      const plan = await drive.contextPlan((await drive.placeOfB()).utxo);
      expect(plan.stateIndex).toBe(REDEEMER_SELECT_STEP_FROM);
      expect(plan.context.stage).toBe("redeemerSelectAuthenticate");
      expect(plan.context.redeemerSelect).toBeDefined();
    },

    /**
     * The authenticate's hop with a select that keeps the control's purpose
     * bound as its frontier in place of the one below it; and a check that
     * the thread has not moved.
     */
    unmovedFrontier: async () => {
      const prover = drive.dispute().scene.h.proverSigner.paymentKeyHash;
      return await drive.hopFromPlace((before, plan) => {
        const honest = validationTraceHop({
          plan,
          holder,
          utxo: before.utxo,
          prover,
        });
        const staged = plutusConstrFieldCbor(before.utxo.datum!, [1, 0]);
        const fields = [...plan.context.redeemerSelect!.selectionFieldsCbor];
        expect(honest.datum).toBe(
          stepDatum(prover, constr(0, [staged, ...fields])),
        );
        // The control's own purpose bound.
        fields[PURPOSE_FRONTIER] = plutusConstrFieldCbor(fields[0]!, [
          CONTROL_PURPOSE_BOUND,
        ]);
        expect(fields[PURPOSE_FRONTIER]).not.toBe(
          plan.context.redeemerSelect!.selectionFieldsCbor[PURPOSE_FRONTIER],
        );
        return {
          ...honest,
          datum: stepDatum(prover, constr(0, [staged, ...fields])),
        };
      }, "contextRedeemerSelectInitialize");
    },

    awards: drive.awards,
    pays: drive.pays,
  };
};
