import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCekCoreMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekCoreSettle: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core settle",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_settle.main.spend",
      familyPaths: [["cekCoreStages", "settle"]],
    },
    validationTraceDisputeCekCoreCompute: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core compute",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_arm_compute.main.spend",
      familyPaths: [["cekCoreStages", "compute"]],
    },
    validationTraceDisputeCekCoreMachine: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core machine",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_arm_machine.main.spend",
      familyPaths: [["cekCoreStages", "machine"]],
    },
    validationTraceDisputeCekCoreCaseConstant: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core case constant",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_arm_case_constant.main.spend",
      familyPaths: [["cekCoreStages", "caseConstant"]],
    },
    validationTraceDisputeCekCoreMapConversion: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core map conversion",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_arm_map_conversion.main.spend",
      familyPaths: [["cekCoreStages", "mapConversion"]],
    },
    validationTraceDisputeCekCoreDirectScalar: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core direct scalar",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_direct_scalar.main.spend",
      familyPaths: [["cekCoreStages", "directScalar"]],
    },
    validationTraceDisputeCekCoreDirectStructured: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core direct structured",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_direct_structured.main.spend",
      familyPaths: [["cekCoreStages", "directStructured"]],
    },
    validationTraceDisputeCekCoreDirectScalarBudget: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core direct scalar budget",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend",
      familyPaths: [["cekCoreStages", "directScalarBudget"]],
    },
    validationTraceDisputeCekCoreDirectStructuredBudget: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core direct structured budget",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend",
      familyPaths: [["cekCoreStages", "directStructuredBudget"]],
    },
    validationTraceDisputeCekCoreDirectScalarRoots: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core direct scalar roots",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend",
      familyPaths: [["cekCoreStages", "directScalarRoots"]],
    },
    validationTraceDisputeCekCoreDirectStructuredRoots: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core direct structured roots",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend",
      familyPaths: [["cekCoreStages", "directStructuredRoots"]],
    },
    validationTraceDisputeCekCoreSemanticPair: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic pair",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_pair.main.spend",
      familyPaths: [["cekCoreStages", "semanticPair"]],
    },
    validationTraceDisputeCekCoreSemanticListConstruct: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic list construct",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_list_construct.main.spend",
      familyPaths: [["cekCoreStages", "semanticListConstruct"]],
    },
    validationTraceDisputeCekCoreSemanticListSelect: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic list select",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_list_select.main.spend",
      familyPaths: [["cekCoreStages", "semanticListSelect"]],
    },
    validationTraceDisputeCekCoreSemanticChoose: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic choose",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_choose.main.spend",
      familyPaths: [["cekCoreStages", "semanticChoose"]],
    },
    validationTraceDisputeCekCoreSemanticDataConstruct: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic data construct",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_data_construct.main.spend",
      familyPaths: [["cekCoreStages", "semanticDataConstruct"]],
    },
    validationTraceDisputeCekCoreSemanticDataScalar: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic data scalar",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_data_scalar.main.spend",
      familyPaths: [["cekCoreStages", "semanticDataScalar"]],
    },
    validationTraceDisputeCekCoreSemanticDataMisc: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic data misc",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_data_misc.main.spend",
      familyPaths: [["cekCoreStages", "semanticDataMisc"]],
    },
    validationTraceDisputeCekCoreSemanticBudget: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic budget",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend",
      familyPaths: [["cekCoreStages", "semanticBudget"]],
    },
    validationTraceDisputeCekCoreSemanticRoots: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic roots",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend",
      familyPaths: [["cekCoreStages", "semanticRoots"]],
    },
    validationTraceDisputeCekCoreSemanticResult: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic result",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_result.main.spend",
      familyPaths: [["cekCoreStages", "semanticResult"]],
    },
    validationTraceDisputeCekCoreMapStartNodes: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core map start nodes",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_map_start_nodes.main.spend",
      familyPaths: [["cekCoreStages", "mapStartNodes"]],
    },
    validationTraceDisputeCekCoreMapStartBudget: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core map start budget",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_map_start_budget.main.spend",
      familyPaths: [["cekCoreStages", "mapStartBudget"]],
    },
    validationTraceDisputeCekCoreMapStartRoots: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core map start roots",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_map_start_roots.main.spend",
      familyPaths: [["cekCoreStages", "mapStartRoots"]],
    },
    validationTraceDisputeCekCoreSemanticFailureMaterial: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic failure material",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_failure_material.main.spend",
      familyPaths: [["cekCoreStages", "semanticFailureMaterial"]],
    },
    validationTraceDisputeCekCoreSemanticFailureRoots: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core semantic failure roots",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_failure_roots.main.spend",
      familyPaths: [["cekCoreStages", "semanticFailureRoots"]],
    },
    validationTraceDisputeCekCoreBlsFinal: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core bls final",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_bls_final.main.spend",
      familyPaths: [["cekCoreStages", "blsFinal"]],
    },
    validationTraceDisputeCekCoreBlsBudget: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core bls budget",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_bls_budget.main.spend",
      familyPaths: [["cekCoreStages", "blsBudget"]],
    },
    validationTraceDisputeCekCoreBlsRoots: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core bls roots",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_bls_roots.main.spend",
      familyPaths: [["cekCoreStages", "blsRoots"]],
    },
    validationTraceDisputeCekCoreFailureBudget: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core failure budget",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_failure_budget.main.spend",
      familyPaths: [["cekCoreStages", "failureBudget"]],
    },
    validationTraceDisputeCekCoreFailureKnown: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core failure known",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_failure_known.main.spend",
      familyPaths: [["cekCoreStages", "failureKnown"]],
    },
    validationTraceDisputeCekCoreTypeFailureKinds: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core type failure kinds",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_type_failure_kinds.main.spend",
      familyPaths: [["cekCoreStages", "typeFailureKinds"]],
    },
    validationTraceDisputeCekCoreTypeFailureRoots: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core type failure roots",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_type_failure_roots.main.spend",
      familyPaths: [["cekCoreStages", "typeFailureRoots"]],
    },
    /** The BLS12-381 uncompress failure arm (core group 12), appended after
     * every earlier member. */
    validationTraceDisputeCekCoreFailureBlsUncompress: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core failure bls uncompress",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_failure_bls_uncompress.main.spend",
      familyPaths: [["cekCoreStages", "failureBlsUncompress"]],
    },
    validationTraceDisputeCekCorePv11Roots: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core pv11 roots",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend",
      familyPaths: [["cekCoreStages", "pv11Roots"]],
    },
    validationTraceDisputeCekCorePv11Budget: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core pv11 budget",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_pv11_budget.main.spend",
      familyPaths: [["cekCoreStages", "pv11Budget"]],
    },
    validationTraceDisputeCekCorePv11Semantics: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK core pv11 semantics",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_pv11_semantics.main.spend",
      familyPaths: [["cekCoreStages", "pv11Semantics"]],
    },
  },
);
