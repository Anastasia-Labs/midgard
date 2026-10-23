import { defineDeploymentFamily } from "../members.types.js";

/** The CEK core's semantics steps over staged operands: the byte-string and
 * string builtins (index 7), and the linear integer builtins (index 8). */
export const validationTraceCekCoreStagedMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekCoreStagedBytes: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged bytes",
      manifestPosition: 559,
      publicationPosition: 554,
      referenceRolePosition: 554,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_bytes.main.spend",
      familyPaths: [["cekCoreStages", "stagedBytes"]],
    },
    validationTraceDisputeCekCoreStagedInteger: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged integer",
      manifestPosition: 560,
      publicationPosition: 555,
      referenceRolePosition: 555,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_integer.main.spend",
      familyPaths: [["cekCoreStages", "stagedInteger"]],
    },
  },
);
