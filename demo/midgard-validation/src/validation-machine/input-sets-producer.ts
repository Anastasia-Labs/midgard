/**
 * The InputSets producer: the descending merge of the spend and reference
 * inputs, which finds a duplicate and builds the resolution schedule.
 */
import { encodeCbor } from "@al-ft/midgard-core";
import { Effect } from "effect";

import {
  initialInputSetMergeControl,
  inputSetMergeStep,
} from "../phase-a.input-set-merge.js";
import { UNSET_REASON_COORDINATE } from "../rejection-reason.js";
import {
  emptyMidgardInputResolutionSchedule,
  prependMidgardInputResolutionSchedule,
} from "./input-resolution.js";
import { proofBinding } from "./prepare-producers.shared.js";
import {
  preparedFieldPreimage,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import {
  phaseContinues,
  type PhaseOutcome,
  phaseStops,
  type TraceWalk,
} from "./trace-walk.js";

export const produceInputSets = (
  prepared: Pick<
    PreparedValidationTransaction,
    | "proofSource"
    | "contextCbor"
    | "fieldPreimages"
    | "spendInputsCollection"
    | "referenceInputsCollection"
    | "resolutionScheduleHash"
  >,
  { push }: Pick<TraceWalk, "push">,
): Effect.Effect<PhaseOutcome, Error> =>
  Effect.gen(function* () {
    const collections = [
      prepared.spendInputsCollection,
      prepared.referenceInputsCollection,
    ] as const;
    const mergeFields = [
      collections[0].items.map((item) => item.bytes),
      collections[1].items.map((item) => item.bytes),
    ] as const;
    let control = initialInputSetMergeControl(mergeFields);
    let scheduleHash = emptyMidgardInputResolutionSchedule();
    const witness = (): Buffer =>
      encodeCbor([
        ...proofBinding(prepared),
        BigInt(control.spendCount),
        BigInt(control.referenceCount),
        BigInt(control.spendSeen),
        BigInt(control.referenceSeen),
        Buffer.from(control.lastKey),
        BigInt(control.lastField),
        BigInt(control.lastItem),
        Buffer.from(control.cachedOtherHead),
        scheduleHash,
      ]);

    if (control.spendCount === 0) {
      push("inputSets", witness());
      return phaseStops(
        "EmptyInputs",
        "bounded input scan found no spend inputs",
      );
    }
    // The descending merge of fields 0 and 1 (`phase-a.input-set-merge.ts`):
    // the control fixes the item each step opens, so the auxiliary is the
    // opened field's carriage alone.
    for (;;) {
      const step = inputSetMergeStep(control, mergeFields);
      const fieldIndex = collections[step.opened.fieldIndex]!.fieldIndex;
      push("inputSets", witness(), {
        kind: "transactionFieldItem",
        fieldIndex,
        fieldPreimage: preparedFieldPreimage(prepared, fieldIndex),
      });
      if (step.kind === "duplicate") {
        return phaseStops(
          {
            DuplicateInput: {
              first_field_index: UNSET_REASON_COORDINATE,
              first_item_index: UNSET_REASON_COORDINATE,
              second_field_index: UNSET_REASON_COORDINATE,
              second_item_index: UNSET_REASON_COORDINATE,
            },
          },
          "bounded input scan found a duplicate",
        );
      }
      control = step.next;
      if (step.kind === "consume") {
        scheduleHash = prependMidgardInputResolutionSchedule({
          sourceKind: step.consumed.fieldIndex === 0 ? "spend" : "reference",
          itemIndex: step.consumed.itemIndex,
          key: Buffer.from(step.key),
          nextHash: scheduleHash,
        });
        if (step.complete) break;
      }
    }
    if (!scheduleHash.equals(prepared.resolutionScheduleHash)) {
      return yield* Effect.fail(
        new Error(
          `bounded input scan schedule ${scheduleHash.toString("hex")} differs from committed ${prepared.resolutionScheduleHash.toString("hex")}`,
        ),
      );
    }
    return phaseContinues;
  });
