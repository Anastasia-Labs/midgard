import { defineDeploymentFamily } from "../members.types.js";

/** The CEK core's semantics steps over staged operands: the byte-string and
 * string builtins (index 7), the linear integer builtins (index 8), and the
 * staged steps placed directly after them, in manifest order. */
export const validationTraceCekCoreStagedMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekCoreStagedBytes: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged bytes",
      manifestPosition: 638,
      publicationPosition: 633,
      referenceRolePosition: 633,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_bytes.main.spend",
      familyPaths: [["cekCoreStages", "stagedBytes"]],
    },
    validationTraceDisputeCekCoreStagedInteger: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged integer",
      manifestPosition: 639,
      publicationPosition: 634,
      referenceRolePosition: 634,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_integer.main.spend",
      familyPaths: [["cekCoreStages", "stagedInteger"]],
    },
    /** multiplyInteger and the division builtins over staged operands (index 9). */
    validationTraceDisputeCekCoreStagedProduct: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged product",
      manifestPosition: 640,
      publicationPosition: 635,
      referenceRolePosition: 635,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_product.main.spend",
      familyPaths: [["cekCoreStages", "stagedProduct"]],
    },
    /** The serialise loop, which proves serialiseData over its staged result
     * (semantic budget index 10). */
    validationTraceDisputeCekCoreSemanticSerialiseLoop: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic serialise loop",
      manifestPosition: 641,
      publicationPosition: 636,
      referenceRolePosition: 636,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_serialise_loop.main.spend",
      familyPaths: [["cekCoreStages", "semanticSerialiseLoop"]],
    },
    /** The failure hop of the byte-string, string and integer builtins, which
     * group 7's roots route those builtins to. */
    validationTraceDisputeCekCoreStagedFailure: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged failure",
      manifestPosition: 642,
      publicationPosition: 637,
      referenceRolePosition: 637,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_failure.main.spend",
      familyPaths: [["cekCoreStages", "stagedFailure"]],
    },
    /** byteStringToInteger, integerToByteString, shiftByteString and
     * rotateByteString over a staged input (index 11). */
    validationTraceDisputeCekCoreStagedConversion: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged conversion",
      manifestPosition: 643,
      publicationPosition: 638,
      referenceRolePosition: 638,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_conversion.main.spend",
      familyPaths: [["cekCoreStages", "stagedConversion"]],
    },
    /** The writeBits loop, which proves writeBits over a long index list
     * (semantic budget index 12) and its failure on an index outside the input. */
    validationTraceDisputeCekCoreStagedWriteBitsLoop: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged write bits loop",
      manifestPosition: 644,
      publicationPosition: 639,
      referenceRolePosition: 639,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_write_bits_loop.main.spend",
      familyPaths: [["cekCoreStages", "stagedWriteBitsLoop"]],
    },
  },
);
