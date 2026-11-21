import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCekCoreMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekCoreSettle: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core settle",
        token: "V1VtCekCoreSet",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_settle.main.spend",
      familyPaths: [["cekCoreStages", "settle"]],
    },
    validationTraceDisputeCekCoreCompute: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core compute",
        token: "V1VtCekCoreCom",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_arm_compute.main.spend",
      familyPaths: [["cekCoreStages", "compute"]],
    },
    validationTraceDisputeCekCoreMachine: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core machine",
        token: "V1VtCekCoreMac",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_arm_machine.main.spend",
      familyPaths: [["cekCoreStages", "machine"]],
    },
    validationTraceDisputeCekCoreCaseConstant: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core case constant",
        token: "V1VtCekCoreCasCon",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_arm_case_constant.main.spend",
      familyPaths: [["cekCoreStages", "caseConstant"]],
    },
    validationTraceDisputeCekCoreMapConversion: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core map conversion",
        token: "V1VtCekCoreMapCon",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_arm_map_conversion.main.spend",
      familyPaths: [["cekCoreStages", "mapConversion"]],
    },
    validationTraceDisputeCekCoreDirectScalar: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core direct scalar",
        token: "V1VtCekCoreDirSca",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_direct_scalar.main.spend",
      familyPaths: [["cekCoreStages", "directScalar"]],
    },
    validationTraceDisputeCekCoreDirectStructured: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core direct structured",
        token: "V1VtCekCoreDirStr",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_direct_structured.main.spend",
      familyPaths: [["cekCoreStages", "directStructured"]],
    },
    validationTraceDisputeCekCoreDirectScalarBudget: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core direct scalar budget",
        token: "V1VtCekCoreDirScaBud",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend",
      familyPaths: [["cekCoreStages", "directScalarBudget"]],
    },
    validationTraceDisputeCekCoreDirectStructuredBudget: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core direct structured budget",
        token: "V1VtCekCoreDirStrBud",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend",
      familyPaths: [["cekCoreStages", "directStructuredBudget"]],
    },
    validationTraceDisputeCekCoreDirectScalarRoots: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core direct scalar roots",
        token: "V1VtCekCoreDirScaRoo",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend",
      familyPaths: [["cekCoreStages", "directScalarRoots"]],
    },
    validationTraceDisputeCekCoreDirectStructuredRoots: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core direct structured roots",
        token: "V1VtCekCoreDirStrRoo",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend",
      familyPaths: [["cekCoreStages", "directStructuredRoots"]],
    },
    validationTraceDisputeCekCoreSemanticPair: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core semantic pair",
        token: "V1VtCekCoreSemPai",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_pair.main.spend",
      familyPaths: [["cekCoreStages", "semanticPair"]],
    },
    validationTraceDisputeCekCoreSemanticListConstruct: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core semantic list construct",
        token: "V1VtCekCoreSemLisCon",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_list_construct.main.spend",
      familyPaths: [["cekCoreStages", "semanticListConstruct"]],
    },
    validationTraceDisputeCekCoreSemanticListSelect: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core semantic list select",
        token: "V1VtCekCoreSemLisSel",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_list_select.main.spend",
      familyPaths: [["cekCoreStages", "semanticListSelect"]],
    },
    validationTraceDisputeCekCoreSemanticChoose: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core semantic choose",
        token: "V1VtCekCoreSemCho",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_choose.main.spend",
      familyPaths: [["cekCoreStages", "semanticChoose"]],
    },
    validationTraceDisputeCekCoreSemanticDataConstruct: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core semantic data construct",
        token: "V1VtCekCoreSemDatCon",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_data_construct.main.spend",
      familyPaths: [["cekCoreStages", "semanticDataConstruct"]],
    },
    validationTraceDisputeCekCoreSemanticDataScalar: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core semantic data scalar",
        token: "V1VtCekCoreSemDatSca",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_data_scalar.main.spend",
      familyPaths: [["cekCoreStages", "semanticDataScalar"]],
    },
    validationTraceDisputeCekCoreSemanticDataMisc: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core semantic data misc",
        token: "V1VtCekCoreSemDatMis",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_data_misc.main.spend",
      familyPaths: [["cekCoreStages", "semanticDataMisc"]],
    },
    validationTraceDisputeCekCoreSemanticBudget: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core semantic budget",
        token: "V1VtCekCoreSemBud",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend",
      familyPaths: [["cekCoreStages", "semanticBudget"]],
    },
    validationTraceDisputeCekCoreSemanticRoots: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core semantic roots",
        token: "V1VtCekCoreSemRoo",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend",
      familyPaths: [["cekCoreStages", "semanticRoots"]],
    },
    validationTraceDisputeCekCoreSemanticResult: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core semantic result",
        token: "V1VtCekCoreSemRes",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_result.main.spend",
      familyPaths: [["cekCoreStages", "semanticResult"]],
    },
    validationTraceDisputeCekCoreMapStartNodes: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core map start nodes",
        token: "V1VtCekCoreMapStaNod",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_map_start_nodes.main.spend",
      familyPaths: [["cekCoreStages", "mapStartNodes"]],
    },
    validationTraceDisputeCekCoreMapStartBudget: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core map start budget",
        token: "V1VtCekCoreMapStaBud",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_map_start_budget.main.spend",
      familyPaths: [["cekCoreStages", "mapStartBudget"]],
    },
    validationTraceDisputeCekCoreMapStartRoots: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core map start roots",
        token: "V1VtCekCoreMapStaRoo",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_map_start_roots.main.spend",
      familyPaths: [["cekCoreStages", "mapStartRoots"]],
    },
    validationTraceDisputeCekCoreSemanticFailureMaterial: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core semantic failure material",
        token: "V1VtCekCoreSemFaiMat",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_failure_material.main.spend",
      familyPaths: [["cekCoreStages", "semanticFailureMaterial"]],
    },
    validationTraceDisputeCekCoreSemanticFailureRoots: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core semantic failure roots",
        token: "V1VtCekCoreSemFaiRoo",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_semantic_failure_roots.main.spend",
      familyPaths: [["cekCoreStages", "semanticFailureRoots"]],
    },
    validationTraceDisputeCekCoreBlsFinal: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core bls final",
        token: "V1VtCekCoreBlsFin",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_bls_final.main.spend",
      familyPaths: [["cekCoreStages", "blsFinal"]],
    },
    validationTraceDisputeCekCoreBlsBudget: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core bls budget",
        token: "V1VtCekCoreBlsBud",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_bls_budget.main.spend",
      familyPaths: [["cekCoreStages", "blsBudget"]],
    },
    validationTraceDisputeCekCoreBlsRoots: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core bls roots",
        token: "V1VtCekCoreBlsRoo",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_bls_roots.main.spend",
      familyPaths: [["cekCoreStages", "blsRoots"]],
    },
    validationTraceDisputeCekCoreFailureBudget: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core failure budget",
        token: "V1VtCekCoreFaiBud",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_failure_budget.main.spend",
      familyPaths: [["cekCoreStages", "failureBudget"]],
    },
    validationTraceDisputeCekCoreFailureKnown: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core failure known",
        token: "V1VtCekCoreFaiKno",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_failure_known.main.spend",
      familyPaths: [["cekCoreStages", "failureKnown"]],
    },
    validationTraceDisputeCekCoreTypeFailureKinds: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core type failure kinds",
        token: "V1VtCekCoreTypFaiKin",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_type_failure_kinds.main.spend",
      familyPaths: [["cekCoreStages", "typeFailureKinds"]],
    },
    validationTraceDisputeCekCoreTypeFailureRoots: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core type failure roots",
        token: "V1VtCekCoreTypFaiRoo",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_type_failure_roots.main.spend",
      familyPaths: [["cekCoreStages", "typeFailureRoots"]],
    },
    /** The BLS12-381 uncompress failure arm (core group 12), appended after
     * every earlier member. */
    validationTraceDisputeCekCoreFailureBlsUncompress: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core failure bls uncompress",
        token: "V1VtCekCoreFaiBlsUnc",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_failure_bls_uncompress.main.spend",
      familyPaths: [["cekCoreStages", "failureBlsUncompress"]],
    },
    validationTraceDisputeCekCorePv11Roots: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core pv11 roots",
        token: "V1VtCekCorePv1Roo",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend",
      familyPaths: [["cekCoreStages", "pv11Roots"]],
    },
    validationTraceDisputeCekCorePv11Budget: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core pv11 budget",
        token: "V1VtCekCorePv1Bud",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_pv11_budget.main.spend",
      familyPaths: [["cekCoreStages", "pv11Budget"]],
    },
    validationTraceDisputeCekCorePv11Semantics: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core pv11 semantics",
        token: "V1VtCekCorePv1Sem",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_pv11_semantics.main.spend",
      familyPaths: [["cekCoreStages", "pv11Semantics"]],
    },
  },
);
