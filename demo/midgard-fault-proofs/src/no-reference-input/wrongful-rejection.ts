import {
  type InputExistenceForcedSource,
  inputExistenceWrongfulRejection,
  type PreparedInputExistenceWrongfulRejection,
} from "../input-existence/wrongful-rejection.js";

export const NO_REFERENCE_INPUT_WRONGFUL_REJECTION_VIOLATION_ID =
  "no-reference-input-wrongful-rejection" as const;
export type NoReferenceInputForcedSource = InputExistenceForcedSource;
export type PreparedNoReferenceInputWrongfulRejection =
  PreparedInputExistenceWrongfulRejection;
export const {
  forcedSourceMaterial: noReferenceInputForcedSourceMaterial,
  prepare: prepareNoReferenceInputWrongfulRejection,
  detect: detectNoReferenceInputWrongfulRejections,
} = inputExistenceWrongfulRejection({
  label: "noReferenceInput",
  violationId: NO_REFERENCE_INPUT_WRONGFUL_REJECTION_VIOLATION_ID,
  sourceKind: 1n,
  fieldIndex: 1,
});
