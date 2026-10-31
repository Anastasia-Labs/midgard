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
import type { PreparedNoReferenceInputWrongfulRejection } from "./wrongful-rejection.js";

export type NoReferenceInputForcedContracts = InputExistenceContracts;

export const noReferenceInputForcedFieldPlan = (
  prepared: PreparedNoReferenceInputWrongfulRejection,
  owner: string,
) => forcedInputExistenceMaterial(prepared, 1, owner).fieldPlan;

export const noReferenceInputForcedStates = (
  prepared: PreparedNoReferenceInputWrongfulRejection,
  owner: string,
) => inputExistenceStates(forcedInputExistenceMaterial(prepared, 1, owner));

type ForcedStepArguments = Omit<
  Parameters<typeof submitInputExistenceStep>[0],
  "prepared"
> & {
  readonly prepared: PreparedNoReferenceInputWrongfulRejection;
};

export const submitNoReferenceInputForcedStep = async ({
  prepared,
  ...arguments_
}: ForcedStepArguments) =>
  submitInputExistenceStep({
    ...arguments_,
    prepared: reopenForcedInputExistence(
      prepared,
      1,
      arguments_.signer.paymentKeyHash,
    ),
  });
