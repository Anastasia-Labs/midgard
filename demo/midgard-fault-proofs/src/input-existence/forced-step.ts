/** Forced evidence uses the same source/event/ledger route as the manifest workflow. */
import type { InputExistenceKind } from "./kind.js";
import { inputExistenceStates } from "./prepare-step-carriage.js";
import {
  forcedInputExistenceMaterial,
  reopenForcedInputExistence,
} from "./reopen-forced.js";
import { submitInputExistenceStep } from "./submit.js";
import type { PreparedInputExistenceWrongfulRejection } from "./wrongful-rejection.js";

type ForcedStepArguments = Omit<
  Parameters<typeof submitInputExistenceStep>[0],
  "prepared"
> & {
  readonly prepared: PreparedInputExistenceWrongfulRejection;
};

/** A family's forced proof thread: its field plan, its step states, and the
 * submission of each step. */
export const inputExistenceForcedSteps = ({
  fieldIndex,
}: InputExistenceKind) => ({
  fieldPlan: (
    prepared: PreparedInputExistenceWrongfulRejection,
    owner: string,
  ) => forcedInputExistenceMaterial(prepared, fieldIndex, owner).fieldPlan,
  states: (prepared: PreparedInputExistenceWrongfulRejection, owner: string) =>
    inputExistenceStates(
      forcedInputExistenceMaterial(prepared, fieldIndex, owner),
    ),
  submit: async ({ prepared, ...arguments_ }: ForcedStepArguments) =>
    submitInputExistenceStep({
      ...arguments_,
      prepared: reopenForcedInputExistence(
        prepared,
        fieldIndex,
        arguments_.signer.paymentKeyHash,
      ),
    }),
});
