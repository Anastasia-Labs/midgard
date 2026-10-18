import { spend } from "./recipes.types.js";

export const validationTraceCekCoreRecipes = {
  validationTraceDisputeCekCoreSettle: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekCoreStages.settle,
  ),
  validationTraceDisputeCekCoreCompute: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekCoreStages.compute,
  ),
  validationTraceDisputeCekCoreMachine: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekCoreStages.machine,
  ),
  validationTraceDisputeCekCoreCaseConstant: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.caseConstant,
  ),
  validationTraceDisputeCekCoreMapConversion: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.mapConversion,
  ),
  validationTraceDisputeCekCoreDirectScalar: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.directScalar,
  ),
  validationTraceDisputeCekCoreDirectStructured: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .directStructured,
  ),
  validationTraceDisputeCekCoreDirectScalarBudget: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .directScalarBudget,
  ),
  validationTraceDisputeCekCoreDirectStructuredBudget: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .directStructuredBudget,
  ),
  validationTraceDisputeCekCoreDirectScalarRoots: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .directScalarRoots,
  ),
  validationTraceDisputeCekCoreDirectStructuredRoots: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .directStructuredRoots,
  ),
  validationTraceDisputeCekCoreSemanticPair: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.semanticPair,
  ),
  validationTraceDisputeCekCoreSemanticListConstruct: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .semanticListConstruct,
  ),
  validationTraceDisputeCekCoreSemanticListSelect: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .semanticListSelect,
  ),
  validationTraceDisputeCekCoreSemanticChoose: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.semanticChoose,
  ),
  validationTraceDisputeCekCoreSemanticDataConstruct: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .semanticDataConstruct,
  ),
  validationTraceDisputeCekCoreSemanticDataScalar: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .semanticDataScalar,
  ),
  validationTraceDisputeCekCoreSemanticDataMisc: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .semanticDataMisc,
  ),
  validationTraceDisputeCekCoreSemanticBudget: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.semanticBudget,
  ),
  validationTraceDisputeCekCoreSemanticRoots: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.semanticRoots,
  ),
  validationTraceDisputeCekCoreSemanticResult: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.semanticResult,
  ),
  validationTraceDisputeCekCoreMapStartNodes: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.mapStartNodes,
  ),
  validationTraceDisputeCekCoreMapStartBudget: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.mapStartBudget,
  ),
  validationTraceDisputeCekCoreMapStartRoots: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.mapStartRoots,
  ),
  validationTraceDisputeCekCoreSemanticFailureMaterial: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .semanticFailureMaterial,
  ),
  validationTraceDisputeCekCoreSemanticFailureRoots: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .semanticFailureRoots,
  ),
  validationTraceDisputeCekCoreBlsFinal: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekCoreStages.blsFinal,
  ),
  validationTraceDisputeCekCoreBlsBudget: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekCoreStages.blsBudget,
  ),
  validationTraceDisputeCekCoreBlsRoots: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekCoreStages.blsRoots,
  ),
  validationTraceDisputeCekCoreFailureBudget: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.failureBudget,
  ),
  validationTraceDisputeCekCoreFailureKnown: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages.failureKnown,
  ),
  validationTraceDisputeCekCoreTypeFailureKinds: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .typeFailureKinds,
  ),
  validationTraceDisputeCekCoreTypeFailureRoots: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekCoreStages
        .typeFailureRoots,
  ),
} as const;
