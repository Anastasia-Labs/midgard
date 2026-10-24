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

/** The semantics step for multiplyInteger and the division builtins over
 * staged operands (index 9), placed directly after the staged integer
 * member. */
const validationTraceCekCoreStagedProductMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekCoreStagedProduct: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged product",
      manifestPosition: stagedInteger.manifestPosition + 1,
      publicationPosition: stagedInteger.publicationPosition + 1,
      referenceRolePosition: stagedInteger.referenceRolePosition + 1,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_product.main.spend",
      familyPaths: [["cekCoreStages", "stagedProduct"]],
    },
  },
);

const stagedProduct =
  validationTraceCekCoreStagedProductMembers.validationTraceDisputeCekCoreStagedProduct;

/** The serialise loop, which proves serialiseData over its staged result
 * (semantic budget index 10), placed directly after the staged product
 * member. */
const validationTraceCekCoreSerialiseLoopMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekCoreSemanticSerialiseLoop: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic serialise loop",
      manifestPosition: stagedProduct.manifestPosition + 1,
      publicationPosition: stagedProduct.publicationPosition + 1,
      referenceRolePosition: stagedProduct.referenceRolePosition + 1,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_serialise_loop.main.spend",
      familyPaths: [["cekCoreStages", "semanticSerialiseLoop"]],
    },
  },
);

const serialiseLoop =
  validationTraceCekCoreSerialiseLoopMembers.validationTraceDisputeCekCoreSemanticSerialiseLoop;

/** The failure hop of the byte-string, string and integer builtins, which
 * group 7's roots route those builtins to, placed directly after the
 * serialise loop member. */
const validationTraceCekCoreStagedFailureMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekCoreStagedFailure: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged failure",
      manifestPosition: serialiseLoop.manifestPosition + 1,
      publicationPosition: serialiseLoop.publicationPosition + 1,
      referenceRolePosition: serialiseLoop.referenceRolePosition + 1,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_failure.main.spend",
      familyPaths: [["cekCoreStages", "stagedFailure"]],
    },
  },
);

const stagedFailure =
  validationTraceCekCoreStagedFailureMembers.validationTraceDisputeCekCoreStagedFailure;

/** byteStringToInteger, integerToByteString, shiftByteString and
 * rotateByteString over a staged input (index 11), placed directly after the
 * staged failure member. */
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

const stagedConversion =
  validationTraceCekCoreStagedConversionMembers.validationTraceDisputeCekCoreStagedConversion;

/** The writeBits loop, which proves writeBits over a long index list
 * (semantic budget index 12) and its failure on an index outside the input,
 * placed directly after the staged conversion member. */
const validationTraceCekCoreStagedWriteBitsLoopMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekCoreStagedWriteBitsLoop: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core staged write bits loop",
      manifestPosition: stagedConversion.manifestPosition + 1,
      publicationPosition: stagedConversion.publicationPosition + 1,
      referenceRolePosition: stagedConversion.referenceRolePosition + 1,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_staged_write_bits_loop.main.spend",
      familyPaths: [["cekCoreStages", "stagedWriteBitsLoop"]],
    },
  },
);

export const validationTraceCekCoreStagedMembers = Object.freeze({
  ...validationTraceCekCoreStagedOperandMembers,
  ...validationTraceCekCoreStagedProductMembers,
  ...validationTraceCekCoreSerialiseLoopMembers,
  ...validationTraceCekCoreStagedFailureMembers,
  ...validationTraceCekCoreStagedConversionMembers,
  ...validationTraceCekCoreStagedWriteBitsLoopMembers,
});
