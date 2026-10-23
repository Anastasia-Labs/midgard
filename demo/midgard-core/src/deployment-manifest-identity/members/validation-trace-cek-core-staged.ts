import { defineDeploymentFamily } from "../members.types.js";

/** The CEK core's semantics steps over staged operands: the byte-string and
 * string builtins (index 7), and the linear integer builtins (index 8). */
const validationTraceCekCoreStagedOperandMembers = defineDeploymentFamily(
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

export const validationTraceCekCoreStagedMembers = Object.freeze({
  ...validationTraceCekCoreStagedOperandMembers,
  ...validationTraceCekCoreStagedProductMembers,
  ...validationTraceCekCoreSerialiseLoopMembers,
});
