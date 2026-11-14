/** The validation-trace dispute runner at a CEK context step of our trace
 * that the observer authenticate verifies, on the emulator (the drive of
 * validation-trace-dispute-one-step-context-drive.ts): the observer
 * authenticate binds the step's observer opening, and the observer fold
 * folds it into the context's successor for the context settle. The test
 * file pins the observer authenticate's refusal of an opening other than
 * the one the step's transaction opens. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { validationTraceHop } from "../../src/validation-trace-dispute/one-step-builders.js";
import {
  constr,
  stepDatum,
} from "../../src/validation-trace-dispute/one-step-data.js";
import {
  observedBy,
  observerHashAt,
} from "./validation-trace-dispute-native-failing-shapes.js";
import { contextStepDrive } from "./validation-trace-dispute-one-step-context-drive.js";

/** The last agreed state before the fixture trace's observer step: its
 * transaction requires no observers. */
export const OBSERVER_STEP_FROM = 104;

/** `ObserverOpening`'s EmptyObservers and FinishedObservers. */
const EMPTY_OBSERVERS = constr(0, []);
const FINISHED_OBSERVERS = constr(1, []);

/** The last agreed state before the observer step of the trace of a
 * failing transaction requiring `observers` observers
 * (`observedBy(observers)`) that opens the `taken`th observer the fold
 * takes: the fold takes them last first, one step each, and finishes at
 * the step after the last. Each observer past the first adds its native
 * script's steps, a decode step, its script-sources steps, and a step to
 * each context step before the observers'. */
export const observerItemStepFrom = (observers: number, taken = 0) =>
  147 + 38 * (observers - 1) + (observers * (observers - 1)) / 2 + taken;

/** The opening of that step: the observer it takes, beside how many the
 * transaction requires. */
export const observerItemOpening = (observers: number, taken = 0) =>
  Data.to(
    new Constr(2, [
      BigInt(observers),
      observerHashAt(observers, observers - 1 - taken),
    ]),
  );

/** The failing transaction of that trace, with the operator's thread
 * disputing its first CEK compute step, past the observers' finish. */
export const observedTrace = (observers: number) =>
  Object.freeze({
    ...observedBy(observers),
    operatorFrom: observerItemStepFrom(observers, observers) + 13,
  });

/** The drive of the successor's thread through the observer step after our
 * trace's state `from`, over a failing transaction carrying `installs`,
 * whose step opens `opening`: by default, the plain fixture's, which
 * requires no observers. */
export const contextObserverDrive = ({
  from = OBSERVER_STEP_FROM,
  installs,
  opening = EMPTY_OBSERVERS,
  minFeeA,
}: {
  readonly from?: number;
  readonly installs?: Parameters<typeof contextStepDrive>[0]["installs"];
  readonly opening?: string;
  /** The ledger's per-byte fee while the resolution plays. */
  readonly minFeeA?: number;
} = {}) => {
  const holder = "contextObserverAuthenticate";
  const drive = contextStepDrive({
    from,
    holder,
    path: ["contextObserverFold"],
    labels: ["ObserverPending", "ContextVerified observerFold"],
    ...(installs === undefined ? {} : { installs }),
    ...(minFeeA === undefined ? {} : { minFeeA }),
  });
  return {
    dispute: drive.dispute,
    setUp: drive.setUp,
    bytesOf: drive.bytesOf,
    cborOf: drive.cborOf,

    /** Our plan routes the thread's step to the observer authenticate, with
     * the opening the step's transaction opens. */
    routes: async () => {
      const plan = await drive.contextPlan((await drive.placeOfB()).utxo);
      expect(plan.stateIndex).toBe(from);
      expect(plan.context.stage).toBe("observerAuthenticate");
      expect(plan.context.observerOpeningCbor).toBe(opening);
    },

    /**
     * The observer authenticate's hop with the opening of finished observers
     * in place of the one the step's transaction opens; and a check that
     * the thread has not moved.
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
          stepDatum(prover, constr(0, [staged, opening])),
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
