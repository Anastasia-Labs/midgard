import {
  type InputExistenceForcedSource,
  inputExistenceWrongfulRejection,
  type PreparedInputExistenceWrongfulRejection,
} from "../input-existence/wrongful-rejection.js";

export const NON_EXISTENT_INPUT_WRONGFUL_REJECTION_VIOLATION_ID =
  "non-existent-input-wrongful-rejection" as const;
export type NonExistentInputForcedSource = InputExistenceForcedSource;
export type PreparedNonExistentInputWrongfulRejection =
  PreparedInputExistenceWrongfulRejection;
export const {
  forcedSourceMaterial: nonExistentInputForcedSourceMaterial,
  prepare: prepareNonExistentInputWrongfulRejection,
  detect: detectNonExistentInputWrongfulRejections,
} = inputExistenceWrongfulRejection({
  label: "nonExistentInput",
  violationId: NON_EXISTENT_INPUT_WRONGFUL_REJECTION_VIOLATION_ID,
  sourceKind: 0n,
  fieldIndex: 0,
});
