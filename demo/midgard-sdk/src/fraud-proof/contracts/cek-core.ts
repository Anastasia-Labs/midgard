import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type Data, type Network } from "@lucid-evolution/lucid";

import {
  applyThreadHolderParams,
  type FaultProofBlueprint,
  makeSpendingValidator,
} from "./blueprint.js";

export const CEK_CORE_STAGE_TITLES = {
  settle: deployedTitle("validationTraceDisputeCekCoreSettle"),
  compute: deployedTitle("validationTraceDisputeCekCoreCompute"),
  machine: deployedTitle("validationTraceDisputeCekCoreMachine"),
  caseConstant: deployedTitle("validationTraceDisputeCekCoreCaseConstant"),
  mapConversion: deployedTitle("validationTraceDisputeCekCoreMapConversion"),
  directScalar: deployedTitle("validationTraceDisputeCekCoreDirectScalar"),
  directStructured: deployedTitle(
    "validationTraceDisputeCekCoreDirectStructured",
  ),
  directScalarBudget: deployedTitle(
    "validationTraceDisputeCekCoreDirectScalarBudget",
  ),
  directStructuredBudget: deployedTitle(
    "validationTraceDisputeCekCoreDirectScalarBudget",
  ),
  directScalarRoots: deployedTitle(
    "validationTraceDisputeCekCoreDirectScalarRoots",
  ),
  directStructuredRoots: deployedTitle(
    "validationTraceDisputeCekCoreDirectScalarRoots",
  ),
  semanticPair: deployedTitle("validationTraceDisputeCekCoreSemanticPair"),
  semanticListConstruct: deployedTitle(
    "validationTraceDisputeCekCoreSemanticListConstruct",
  ),
  semanticListSelect: deployedTitle(
    "validationTraceDisputeCekCoreSemanticListSelect",
  ),
  semanticChoose: deployedTitle("validationTraceDisputeCekCoreSemanticChoose"),
  semanticDataConstruct: deployedTitle(
    "validationTraceDisputeCekCoreSemanticDataConstruct",
  ),
  semanticDataScalar: deployedTitle(
    "validationTraceDisputeCekCoreSemanticDataScalar",
  ),
  semanticDataMisc: deployedTitle(
    "validationTraceDisputeCekCoreSemanticDataMisc",
  ),
  stagedBytes: deployedTitle("validationTraceDisputeCekCoreStagedBytes"),
  stagedInteger: deployedTitle("validationTraceDisputeCekCoreStagedInteger"),
  stagedProduct: deployedTitle("validationTraceDisputeCekCoreStagedProduct"),
  semanticSerialiseLoop: deployedTitle(
    "validationTraceDisputeCekCoreSemanticSerialiseLoop",
  ),
  stagedFailure: deployedTitle("validationTraceDisputeCekCoreStagedFailure"),
  stagedConversion: deployedTitle(
    "validationTraceDisputeCekCoreStagedConversion",
  ),
  stagedWriteBitsLoop: deployedTitle(
    "validationTraceDisputeCekCoreStagedWriteBitsLoop",
  ),
  semanticBudget: deployedTitle(
    "validationTraceDisputeCekCoreDirectScalarBudget",
  ),
  semanticResult: deployedTitle("validationTraceDisputeCekCoreSemanticResult"),
  semanticRoots: deployedTitle(
    "validationTraceDisputeCekCoreDirectScalarRoots",
  ),
  mapStartNodes: deployedTitle("validationTraceDisputeCekCoreMapStartNodes"),
  mapStartBudget: deployedTitle("validationTraceDisputeCekCoreMapStartBudget"),
  mapStartRoots: deployedTitle("validationTraceDisputeCekCoreMapStartRoots"),
  semanticFailureMaterial: deployedTitle(
    "validationTraceDisputeCekCoreSemanticFailureMaterial",
  ),
  semanticFailureRoots: deployedTitle(
    "validationTraceDisputeCekCoreSemanticFailureRoots",
  ),
  blsFinal: deployedTitle("validationTraceDisputeCekCoreBlsFinal"),
  blsRoots: deployedTitle("validationTraceDisputeCekCoreBlsRoots"),
  blsBudget: deployedTitle("validationTraceDisputeCekCoreBlsBudget"),
  failureBudget: deployedTitle("validationTraceDisputeCekCoreFailureBudget"),
  failureKnown: deployedTitle("validationTraceDisputeCekCoreFailureKnown"),
  failureBlsUncompress: deployedTitle(
    "validationTraceDisputeCekCoreFailureBlsUncompress",
  ),
  typeFailureKinds: deployedTitle(
    "validationTraceDisputeCekCoreTypeFailureKinds",
  ),
  typeFailureRoots: deployedTitle(
    "validationTraceDisputeCekCoreTypeFailureRoots",
  ),
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
  1n,
  2n,
] as const;
export const buildCekCoreStages = (
  blueprint: FaultProofBlueprint,
  network: Network,
  awardHash: string,
  ctPolicy: string,
  threadExpiryHash: string,
) => {
  const build = (key: keyof typeof CEK_CORE_STAGE_TITLES, params: Data[]) =>
    makeSpendingValidator(
      network,
      applyThreadHolderParams(
        blueprint,
        CEK_CORE_STAGE_TITLES[key],
        params,
        threadExpiryHash,
      ),
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
  const caseConstant = terminal("caseConstant");
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
  const stagedBytes = terminal("stagedBytes");
  const stagedInteger = terminal("stagedInteger");
  const stagedProduct = terminal("stagedProduct");
  const semanticSerialiseLoop = terminal("semanticSerialiseLoop");
  const stagedConversion = terminal("stagedConversion");
  const stagedWriteBitsLoop = terminal("stagedWriteBitsLoop");
  const mapStartNodes = terminal("mapStartNodes");
  const semanticFailureMaterial = terminal("semanticFailureMaterial");
  const stagedFailure = terminal("stagedFailure");
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
      stagedBytes,
      stagedInteger,
      stagedProduct,
      semanticSerialiseLoop,
      stagedConversion,
      stagedWriteBitsLoop,
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
    stagedFailure.spendingScriptHash,
    stagedWriteBitsLoop.spendingScriptHash,
    ctPolicy,
  ]);
  const blsRoots = build("blsRoots", [blsFinal.spendingScriptHash, ctPolicy]);
  const blsBudget = build("blsBudget", [blsRoots.spendingScriptHash, ctPolicy]);
  const failureKnown = build("failureKnown", [
    failureBudget.spendingScriptHash,
    ctPolicy,
  ]);
  const failureBlsUncompress = build("failureBlsUncompress", [
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
    caseConstant,
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
    stagedBytes,
    stagedInteger,
    stagedProduct,
    semanticSerialiseLoop,
    stagedConversion,
    stagedWriteBitsLoop,
    semanticBudget,
    semanticRoots,
    semanticResult,
    mapStartNodes,
    mapStartBudget,
    mapStartRoots,
    semanticFailureMaterial,
    semanticFailureRoots,
    stagedFailure,
    blsFinal,
    blsBudget,
    blsRoots,
    failureBudget,
    failureKnown,
    failureBlsUncompress,
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
    stages.caseConstant,
    stages.failureBlsUncompress,
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
  caseConstant: {
    deployment: "validationTraceDisputeCekCoreCaseConstant",
    role: "V1 validation-trace CEK core case constant",
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
  stagedBytes: {
    deployment: "validationTraceDisputeCekCoreStagedBytes",
    role: "V1 validation-trace CEK core staged bytes",
  },
  stagedInteger: {
    deployment: "validationTraceDisputeCekCoreStagedInteger",
    role: "V1 validation-trace CEK core staged integer",
  },
  stagedProduct: {
    deployment: "validationTraceDisputeCekCoreStagedProduct",
    role: "V1 validation-trace CEK core staged product",
  },
  semanticSerialiseLoop: {
    deployment: "validationTraceDisputeCekCoreSemanticSerialiseLoop",
    role: "V1 validation-trace CEK core semantic serialise loop",
  },
  stagedFailure: {
    deployment: "validationTraceDisputeCekCoreStagedFailure",
    role: "V1 validation-trace CEK core staged failure",
  },
  stagedConversion: {
    deployment: "validationTraceDisputeCekCoreStagedConversion",
    role: "V1 validation-trace CEK core staged conversion",
  },
  stagedWriteBitsLoop: {
    deployment: "validationTraceDisputeCekCoreStagedWriteBitsLoop",
    role: "V1 validation-trace CEK core staged write bits loop",
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
  failureBlsUncompress: {
    deployment: "validationTraceDisputeCekCoreFailureBlsUncompress",
    role: "V1 validation-trace CEK core failure bls uncompress",
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
