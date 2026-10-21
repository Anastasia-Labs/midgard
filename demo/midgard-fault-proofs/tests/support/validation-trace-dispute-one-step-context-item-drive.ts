/** The validation-trace dispute runner at a CEK context step that our trace
 * routes to the item bind stage, on the emulator (the drive of
 * validation-trace-dispute-one-step-context-drive.ts): the item bind stage
 * binds the step's item control and witness, the shared item chain's
 * holders verify the item step (its CEK entry, the traversal and outer
 * normalizers on the current and the next control, the source
 * authenticator, the executor the step selects and the CEK settlement), the
 * item return routes the verified step by its context and item stages, and
 * the hash or finalize the route needs and its continue or finish write the
 * context's successor for the context settle. Each step's test file pins the
 * item bind stage's refusal of a pending step other than the one it binds. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { expect } from "vitest";

import { validationTraceHop } from "../../src/validation-trace-dispute/one-step-builders.js";
import type { ValidationTraceContextItemRoute } from "../../src/validation-trace-dispute/one-step-context-item.js";
import type { ValidationTraceContextItemHolder } from "../../src/validation-trace-dispute/one-step-context-item-holders.js";
import {
  bytes,
  constr,
  stepDatum,
} from "../../src/validation-trace-dispute/one-step-data.js";
import { contextStepDrive } from "./validation-trace-dispute-one-step-context-drive.js";

/** The item step after each of these states of the fixture's trace: the
 * route its item return takes, and the executor its item step selects. */
export const CONTEXT_ITEM_STEPS = Object.freeze({
  /** A selection scan's step that opens an item's header. */
  95: {
    route: "contextItemSelectionHash",
    executor: "redeemerItemOpenHeaderExecutor",
  },
  /** A selection scan's step that opens the item's tail and finishes. */
  96: {
    route: "contextItemSelectionFinish",
    executor: "redeemerItemOpenTailExecutor",
  },
  /** A data scan's step that opens an item's header. */
  107: {
    route: "contextItemDataHash",
    executor: "redeemerItemOpenHeaderExecutor",
  },
  /** A data scan's step that opens the item's tail and continues. */
  108: {
    route: "contextItemDataHash",
    executor: "redeemerItemOpenTailExecutor",
  },
  /** A data scan's step that finishes the item's value. */
  111: {
    route: "contextItemFinalize",
    executor: "redeemerItemFinishDataExecutor",
  },
} as const satisfies Readonly<
  Record<
    number,
    Readonly<{
      route: ValidationTraceContextItemRoute;
      executor: ValidationTraceContextItemHolder;
    }>
  >
>);

/** The holder each route's hash or finalize hands the thread to. */
const ROUTE_NEXT: Readonly<
  Partial<
    Record<ValidationTraceContextItemRoute, ValidationTraceContextItemHolder>
  >
> = {
  contextItemSelectionHash: "contextItemSelectionContinue",
  contextItemDataHash: "contextItemDataContinue",
  contextItemFinalize: "contextItemDataFinishValue",
};

/** The drive of the successor's thread through the item step after our
 * trace's state `from`. */
export const contextItemDrive = (from: keyof typeof CONTEXT_ITEM_STEPS) => {
  const { route, executor } = CONTEXT_ITEM_STEPS[from];
  const next = ROUTE_NEXT[route];
  const drive = contextStepDrive({
    from,
    holder: "contextItemBind",
    path: [
      "contextItemEntry",
      "redeemerItemTraversalNormalizer",
      "redeemerItemOuterNormalizer",
      "redeemerItemTraversalNormalizer",
      "redeemerItemOuterNormalizer",
      "redeemerItemSourceAuthenticator",
      executor,
      "contextItemSettlement",
      "contextItemReturn",
      route,
      ...(next === undefined ? [] : [next]),
    ],
    labels: [
      "ItemBind",
      "Item entry",
      "Item normalize current traversal",
      "Item normalize current outer",
      "Item normalize next traversal",
      "Item normalize next outer",
      "Item authenticate source",
      `Item execute ${executor}`,
      "Item settle",
      `ItemReturn ${route}`,
      route,
      ...(next === undefined ? [] : [next]),
    ],
  });
  return {
    setUp: drive.setUp,

    /** Our plan routes the thread's step to the item bind stage, and its
     * item step to the route. */
    routes: async () => {
      const plan = await drive.contextPlan((await drive.placeOfB()).utxo);
      expect(plan.stateIndex).toBe(from);
      expect(plan.context.stage).toBe("itemBind");
      expect(plan.context.item?.route).toBe(route);
    },

    /**
     * The item bind stage's hop with a pending step that carries the control
     * the step claims next in place of the one its evidence binds; and a
     * check that the thread has not moved.
     */
    unboundControl: async () => {
      const prover = drive.dispute().scene.h.proverSigner.paymentKeyHash;
      return await drive.hopFromPlace((before, plan) => {
        const honest = validationTraceHop({
          plan,
          holder: "contextItemBind",
          utxo: before.utxo,
          prover,
        });
        const item = plan.context.item!;
        const staged = plutusConstrFieldCbor(before.utxo.datum!, [1, 0]);
        const pending = (control: string) =>
          stepDatum(
            prover,
            constr(0, [
              staged,
              control,
              bytes(item.witnessHash),
              item.claimedNextCbor,
            ]),
          );
        expect(item.claimedNextCbor).not.toBe(item.controlCbor);
        expect(honest.datum).toBe(pending(item.controlCbor));
        return { ...honest, datum: pending(item.claimedNextCbor) };
      }, "contextItemEntry");
    },

    awards: drive.awards,
    pays: drive.pays,
  };
};
