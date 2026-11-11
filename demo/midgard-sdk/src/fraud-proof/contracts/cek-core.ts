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
  pv11Roots: deployedTitle("validationTraceDisputeCekCorePv11Roots"),
  pv11Budget: deployedTitle("validationTraceDisputeCekCorePv11Budget"),
  pv11Semantics: deployedTitle("validationTraceDisputeCekCorePv11Semantics"),
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
  3n,
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
  const pv11Semantics = terminal("pv11Semantics");
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
  const pv11Budget = build("pv11Budget", [
    pv11Semantics.spendingScriptHash,
    ctPolicy,
  ]);
  const pv11Roots = build("pv11Roots", [
    pv11Budget.spendingScriptHash,
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
    pv11Roots,
    pv11Budget,
    pv11Semantics,
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
    stages.pv11Roots,
    stages.pv11Budget,
  ].map((stage) => stage.spendingScriptHash);
