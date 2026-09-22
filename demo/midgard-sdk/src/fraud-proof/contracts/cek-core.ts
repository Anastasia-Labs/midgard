import { type Data, type Network } from "@lucid-evolution/lucid";

import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  makeSpendingValidator,
} from "./blueprint.js";

export const CEK_CORE_STAGE_TITLES = {
  settle: "fraud_proofs/validation_trace/cek_core_settle.main.spend",
  compute: "fraud_proofs/validation_trace/cek_core_arm_compute.main.spend",
  machine: "fraud_proofs/validation_trace/cek_core_arm_machine.main.spend",
  mapConversion:
    "fraud_proofs/validation_trace/cek_core_arm_map_conversion.main.spend",
  directScalar:
    "fraud_proofs/validation_trace/cek_core_direct_scalar.main.spend",
  directStructured:
    "fraud_proofs/validation_trace/cek_core_direct_structured.main.spend",
  directScalarBudget:
    "fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend",
  directStructuredBudget:
    "fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend",
  directScalarRoots:
    "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend",
  directStructuredRoots:
    "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend",
  semanticPair:
    "fraud_proofs/validation_trace/cek_core_semantic_pair.main.spend",
  semanticListConstruct:
    "fraud_proofs/validation_trace/cek_core_semantic_list_construct.main.spend",
  semanticListSelect:
    "fraud_proofs/validation_trace/cek_core_semantic_list_select.main.spend",
  semanticChoose:
    "fraud_proofs/validation_trace/cek_core_semantic_choose.main.spend",
  semanticDataConstruct:
    "fraud_proofs/validation_trace/cek_core_semantic_data_construct.main.spend",
  semanticDataScalar:
    "fraud_proofs/validation_trace/cek_core_semantic_data_scalar.main.spend",
  semanticDataMisc:
    "fraud_proofs/validation_trace/cek_core_semantic_data_misc.main.spend",
  semanticBudget:
    "fraud_proofs/validation_trace/cek_core_builtin_budget.main.spend",
  semanticResult:
    "fraud_proofs/validation_trace/cek_core_semantic_result.main.spend",
  semanticRoots:
    "fraud_proofs/validation_trace/cek_core_builtin_roots.main.spend",
  mapStartNodes:
    "fraud_proofs/validation_trace/cek_core_map_start_nodes.main.spend",
  mapStartBudget:
    "fraud_proofs/validation_trace/cek_core_map_start_budget.main.spend",
  mapStartRoots:
    "fraud_proofs/validation_trace/cek_core_map_start_roots.main.spend",
  semanticFailureMaterial:
    "fraud_proofs/validation_trace/cek_core_semantic_failure_material.main.spend",
  semanticFailureRoots:
    "fraud_proofs/validation_trace/cek_core_semantic_failure_roots.main.spend",
  blsFinal: "fraud_proofs/validation_trace/cek_core_bls_final.main.spend",
  blsRoots: "fraud_proofs/validation_trace/cek_core_bls_roots.main.spend",
  blsBudget: "fraud_proofs/validation_trace/cek_core_bls_budget.main.spend",
  failureBudget:
    "fraud_proofs/validation_trace/cek_core_failure_budget.main.spend",
  failureKnown:
    "fraud_proofs/validation_trace/cek_core_failure_known.main.spend",
  typeFailureKinds:
    "fraud_proofs/validation_trace/cek_core_type_failure_kinds.main.spend",
  typeFailureRoots:
    "fraud_proofs/validation_trace/cek_core_type_failure_roots.main.spend",
} as const;
export const CEK_CORE_ARM_HOP_COUNTS = [
  1n,
  1n,
  1n,
  3n,
  3n,
  4n,
  3n,
  2n,
  3n,
  2n,
  2n,
] as const;
export const buildCekCoreStages = (
  blueprint: FaultProofBlueprint,
  network: Network,
  awardHash: string,
  ctPolicy: string,
) => {
  const build = (key: keyof typeof CEK_CORE_STAGE_TITLES, params: Data[]) =>
    makeSpendingValidator(
      network,
      applyBlueprintParams(blueprint, CEK_CORE_STAGE_TITLES[key], params),
    );
  const settle = build("settle", [
    awardHash,
    [...CEK_CORE_ARM_HOP_COUNTS],
    ctPolicy,
  ]);
  const terminal = (key: keyof typeof CEK_CORE_STAGE_TITLES) =>
    build(key, [settle.spendingScriptHash, ctPolicy]);
  const compute = terminal("compute");
  const machine = terminal("machine");
  const mapConversion = terminal("mapConversion");
  const directScalar = terminal("directScalar");
  const directStructured = terminal("directStructured");
  const semanticPair = terminal("semanticPair");
  const semanticListConstruct = terminal("semanticListConstruct");
  const semanticListSelect = terminal("semanticListSelect");
  const semanticChoose = terminal("semanticChoose");
  const semanticDataConstruct = terminal("semanticDataConstruct");
  const semanticDataScalar = terminal("semanticDataScalar");
  const semanticDataMisc = terminal("semanticDataMisc");
  const mapStartNodes = terminal("mapStartNodes");
  const semanticFailureMaterial = terminal("semanticFailureMaterial");
  const blsFinal = terminal("blsFinal");
  const failureBudget = terminal("failureBudget");
  const typeFailureKinds = terminal("typeFailureKinds");
  const directScalarBudget = build("directScalarBudget", [
    [directScalar.spendingScriptHash],
    ctPolicy,
  ]);
  const directStructuredBudget = build("directStructuredBudget", [
    [directStructured.spendingScriptHash],
    ctPolicy,
  ]);
  const semanticBudget = build("semanticBudget", [
    [
      semanticPair,
      semanticListConstruct,
      semanticListSelect,
      semanticChoose,
      semanticDataConstruct,
      semanticDataScalar,
      semanticDataMisc,
    ].map((stage) => stage.spendingScriptHash),
    ctPolicy,
  ]);
  const directScalarRoots = build("directScalarRoots", [
    directScalarBudget.spendingScriptHash,
    ctPolicy,
  ]);
  const directStructuredRoots = build("directStructuredRoots", [
    directStructuredBudget.spendingScriptHash,
    ctPolicy,
  ]);
  const semanticResult = build("semanticResult", [
    semanticBudget.spendingScriptHash,
    ctPolicy,
  ]);
  const semanticRoots = build("semanticRoots", [
    semanticResult.spendingScriptHash,
    ctPolicy,
  ]);
  const mapStartBudget = build("mapStartBudget", [
    mapStartNodes.spendingScriptHash,
    ctPolicy,
  ]);
  const mapStartRoots = build("mapStartRoots", [
    mapStartBudget.spendingScriptHash,
    ctPolicy,
  ]);
  const semanticFailureRoots = build("semanticFailureRoots", [
    semanticFailureMaterial.spendingScriptHash,
    ctPolicy,
  ]);
  const blsRoots = build("blsRoots", [blsFinal.spendingScriptHash, ctPolicy]);
  const blsBudget = build("blsBudget", [blsRoots.spendingScriptHash, ctPolicy]);
  const failureKnown = build("failureKnown", [
    failureBudget.spendingScriptHash,
    ctPolicy,
  ]);
  const typeFailureRoots = build("typeFailureRoots", [
    typeFailureKinds.spendingScriptHash,
    ctPolicy,
  ]);
  return {
    settle,
    compute,
    machine,
    mapConversion,
    directScalar,
    directStructured,
    directScalarBudget,
    directStructuredBudget,
    directScalarRoots,
    directStructuredRoots,
    semanticPair,
    semanticListConstruct,
    semanticListSelect,
    semanticChoose,
    semanticDataConstruct,
    semanticDataScalar,
    semanticDataMisc,
    semanticBudget,
    semanticRoots,
    semanticResult,
    mapStartNodes,
    mapStartBudget,
    mapStartRoots,
    semanticFailureMaterial,
    semanticFailureRoots,
    blsFinal,
    blsBudget,
    blsRoots,
    failureBudget,
    failureKnown,
    typeFailureKinds,
    typeFailureRoots,
  };
};
export type CekCoreStages = ReturnType<typeof buildCekCoreStages>;
export const cekCoreEntryHashes = (stages: CekCoreStages) =>
  [
    stages.compute,
    stages.machine,
    stages.mapConversion,
    stages.directScalarRoots,
    stages.directStructuredRoots,
    stages.semanticRoots,
    stages.mapStartRoots,
    stages.semanticFailureRoots,
    stages.blsBudget,
    stages.failureKnown,
    stages.typeFailureRoots,
  ].map((stage) => stage.spendingScriptHash);

export const CEK_CORE_STAGE_REFERENCES = {
  settle: {
    deployment: "validationTraceDisputeCekCoreSettle",
    role: "V1 validation-trace CEK core settle",
  },
  compute: {
    deployment: "validationTraceDisputeCekCoreCompute",
    role: "V1 validation-trace CEK core compute",
  },
  machine: {
    deployment: "validationTraceDisputeCekCoreMachine",
    role: "V1 validation-trace CEK core machine",
  },
  mapConversion: {
    deployment: "validationTraceDisputeCekCoreMapConversion",
    role: "V1 validation-trace CEK core map conversion",
  },
  directScalar: {
    deployment: "validationTraceDisputeCekCoreDirectScalar",
    role: "V1 validation-trace CEK core direct scalar",
  },
  directStructured: {
    deployment: "validationTraceDisputeCekCoreDirectStructured",
    role: "V1 validation-trace CEK core direct structured",
  },
  directScalarBudget: {
    deployment: "validationTraceDisputeCekCoreDirectScalarBudget",
    role: "V1 validation-trace CEK core direct scalar budget",
  },
  directStructuredBudget: {
    deployment: "validationTraceDisputeCekCoreDirectStructuredBudget",
    role: "V1 validation-trace CEK core direct structured budget",
  },
  directScalarRoots: {
    deployment: "validationTraceDisputeCekCoreDirectScalarRoots",
    role: "V1 validation-trace CEK core direct scalar roots",
  },
  directStructuredRoots: {
    deployment: "validationTraceDisputeCekCoreDirectStructuredRoots",
    role: "V1 validation-trace CEK core direct structured roots",
  },
  semanticPair: {
    deployment: "validationTraceDisputeCekCoreSemanticPair",
    role: "V1 validation-trace CEK core semantic pair",
  },
  semanticListConstruct: {
    deployment: "validationTraceDisputeCekCoreSemanticListConstruct",
    role: "V1 validation-trace CEK core semantic list construct",
  },
  semanticListSelect: {
    deployment: "validationTraceDisputeCekCoreSemanticListSelect",
    role: "V1 validation-trace CEK core semantic list select",
  },
  semanticChoose: {
    deployment: "validationTraceDisputeCekCoreSemanticChoose",
    role: "V1 validation-trace CEK core semantic choose",
  },
  semanticDataConstruct: {
    deployment: "validationTraceDisputeCekCoreSemanticDataConstruct",
    role: "V1 validation-trace CEK core semantic data construct",
  },
  semanticDataScalar: {
    deployment: "validationTraceDisputeCekCoreSemanticDataScalar",
    role: "V1 validation-trace CEK core semantic data scalar",
  },
  semanticDataMisc: {
    deployment: "validationTraceDisputeCekCoreSemanticDataMisc",
    role: "V1 validation-trace CEK core semantic data misc",
  },
  semanticBudget: {
    deployment: "validationTraceDisputeCekCoreSemanticBudget",
    role: "V1 validation-trace CEK core semantic budget",
  },
  semanticResult: {
    deployment: "validationTraceDisputeCekCoreSemanticResult",
    role: "V1 validation-trace CEK core semantic result",
  },
  semanticRoots: {
    deployment: "validationTraceDisputeCekCoreSemanticRoots",
    role: "V1 validation-trace CEK core semantic roots",
  },
  mapStartNodes: {
    deployment: "validationTraceDisputeCekCoreMapStartNodes",
    role: "V1 validation-trace CEK core map start nodes",
  },
  mapStartBudget: {
    deployment: "validationTraceDisputeCekCoreMapStartBudget",
    role: "V1 validation-trace CEK core map start budget",
  },
  mapStartRoots: {
    deployment: "validationTraceDisputeCekCoreMapStartRoots",
    role: "V1 validation-trace CEK core map start roots",
  },
  semanticFailureMaterial: {
    deployment: "validationTraceDisputeCekCoreSemanticFailureMaterial",
    role: "V1 validation-trace CEK core semantic failure material",
  },
  semanticFailureRoots: {
    deployment: "validationTraceDisputeCekCoreSemanticFailureRoots",
    role: "V1 validation-trace CEK core semantic failure roots",
  },
  blsFinal: {
    deployment: "validationTraceDisputeCekCoreBlsFinal",
    role: "V1 validation-trace CEK core bls final",
  },
  blsRoots: {
    deployment: "validationTraceDisputeCekCoreBlsRoots",
    role: "V1 validation-trace CEK core bls roots",
  },
  blsBudget: {
    deployment: "validationTraceDisputeCekCoreBlsBudget",
    role: "V1 validation-trace CEK core bls budget",
  },
  failureBudget: {
    deployment: "validationTraceDisputeCekCoreFailureBudget",
    role: "V1 validation-trace CEK core failure budget",
  },
  failureKnown: {
    deployment: "validationTraceDisputeCekCoreFailureKnown",
    role: "V1 validation-trace CEK core failure known",
  },
  typeFailureKinds: {
    deployment: "validationTraceDisputeCekCoreTypeFailureKinds",
    role: "V1 validation-trace CEK core type failure kinds",
  },
  typeFailureRoots: {
    deployment: "validationTraceDisputeCekCoreTypeFailureRoots",
    role: "V1 validation-trace CEK core type failure roots",
  },
} as const;
