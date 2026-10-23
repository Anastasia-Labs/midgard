import { defineDeploymentFamily } from "../members.types.js";

/** The CEK core's semantics steps over staged operands: the byte-string and
 * string builtins (index 7), and the linear integer builtins (index 8). */
const validationTraceCekCoreStagedOperandMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekCoreStagedBytes: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged bytes",
      manifestPosition: 591,
      publicationPosition: 586,
      referenceRolePosition: 586,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_bytes.main.spend",
      familyPaths: [["cekCoreStages", "stagedBytes"]],
    },
    validationTraceDisputeCekCoreStagedInteger: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged integer",
      manifestPosition: 592,
      publicationPosition: 587,
      referenceRolePosition: 587,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_integer.main.spend",
      familyPaths: [["cekCoreStages", "stagedInteger"]],
    },
  },
);

const stagedInteger =
  validationTraceCekCoreStagedOperandMembers.validationTraceDisputeCekCoreStagedInteger;

/** The failure hop of the byte-string, string and integer builtins, which
 * group 7's roots route those builtins to, placed directly after the staged
 * integer member. */
const validationTraceCekCoreStagedFailureMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekCoreStagedFailure: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged failure",
      manifestPosition: stagedInteger.manifestPosition + 1,
      publicationPosition: stagedInteger.publicationPosition + 1,
      referenceRolePosition: stagedInteger.referenceRolePosition + 1,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_failure.main.spend",
      familyPaths: [["cekCoreStages", "stagedFailure"]],
    },
  },
);

const stagedFailure =
  validationTraceCekCoreStagedFailureMembers.validationTraceDisputeCekCoreStagedFailure;

/** byteStringToInteger over a staged input (index 9), placed directly after
 * the staged failure member. */
const validationTraceCekCoreStagedConversionMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekCoreStagedConversion: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged conversion",
      manifestPosition: stagedFailure.manifestPosition + 1,
      publicationPosition: stagedFailure.publicationPosition + 1,
      referenceRolePosition: stagedFailure.referenceRolePosition + 1,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_conversion.main.spend",
      familyPaths: [["cekCoreStages", "stagedConversion"]],
    },
  },
);

export const validationTraceCekCoreStagedMembers = Object.freeze({
  ...validationTraceCekCoreStagedOperandMembers,
  ...validationTraceCekCoreStagedFailureMembers,
  ...validationTraceCekCoreStagedConversionMembers,
});
