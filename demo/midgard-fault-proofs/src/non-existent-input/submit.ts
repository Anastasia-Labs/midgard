/** Forced evidence uses the same source/event/ledger route as the manifest workflow. */
import {
  type InputExistenceContracts,
  inputExistenceStates,
} from "../input-existence/prepare-step-carriage.js";
import {
  forcedInputExistenceMaterial,
  reopenForcedInputExistence,
} from "../input-existence/reopen-forced.js";
import { submitInputExistenceStep } from "../input-existence/submit.js";
import type { PreparedNonExistentInputWrongfulRejection } from "./wrongful-rejection.js";

export type NonExistentInputForcedContracts = InputExistenceContracts;

export const nonExistentInputForcedFieldPlan = (
  prepared: PreparedNonExistentInputWrongfulRejection,
  owner: string,
) => forcedInputExistenceMaterial(prepared, 0, owner).fieldPlan;

export const nonExistentInputForcedStates = (
  prepared: PreparedNonExistentInputWrongfulRejection,
  owner: string,
) => inputExistenceStates(forcedInputExistenceMaterial(prepared, 0, owner));

type ForcedStepArguments = Omit<
  Parameters<typeof submitInputExistenceStep>[0],
  "prepared"
> & {
  readonly prepared: PreparedNonExistentInputWrongfulRejection;
};

export const submitNonExistentInputForcedStep = async ({
  prepared,
  ...arguments_
}: ForcedStepArguments) =>
  submitInputExistenceStep({
    ...arguments_,
    prepared: reopenForcedInputExistence(
      prepared,
      0,
      arguments_.signer.paymentKeyHash,
    ),
  });
