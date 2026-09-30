import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
  WithdrawalValidator,
} from "../../../common.js";
import { type CekContextStages } from "../cek-context.js";
import { type CekCoreStages } from "../cek-core.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";
import { type SharedRedeemerItemStages } from "./shared-redeemer-item.js";

export type ValidationTraceDisputeFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly validationTraceDispute: FraudProofChain & {
    readonly cekProgramMaterial: SpendingValidator;
    readonly cekMaterialTraversal: SpendingValidator;
    readonly cekCoreStages: CekCoreStages;
    readonly cekContextStages: CekContextStages;
    readonly cekContextItemStages: SharedRedeemerItemStages;
    readonly opener: SpendingValidator;
    readonly source: SpendingValidator;
    readonly game: SpendingValidator;
    readonly boundary: SpendingValidator;
    readonly timeout: SpendingValidator;
    readonly award: SpendingValidator;
    readonly proofItem: SpendingValidator;
    readonly canonicalDecodeItemStages: {
      readonly source: SpendingValidator;
      readonly observe: SpendingValidator;
      readonly proof: SpendingValidator;
      readonly settlement: SpendingValidator;
    };
    readonly scriptSourcesStageOneRedeemerStages: {
      readonly envelope: SpendingValidator;
      readonly traversalNormalizer: SpendingValidator;
      readonly outerNormalizer: SpendingValidator;
      readonly sourceAuthenticator: SpendingValidator;
      readonly executors: readonly SpendingValidator[];
      readonly foldMapExecutor: SpendingValidator;
      readonly finalizeFrameExecutor: SpendingValidator;
      readonly settlement: SpendingValidator;
    };
    readonly prepareResolvers: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
    readonly yields: {
      readonly scriptSourcesStageTwoAdvance: WithdrawalValidator;
      readonly scriptSourcesStageThreeReplay: WithdrawalValidator;
      readonly scriptSourcesStageThreeFinish: WithdrawalValidator;
      readonly scriptSourcesStageFourBegin: WithdrawalValidator;
      readonly scriptSourcesStageFourFinish: WithdrawalValidator;
      readonly scriptSourcesStageSixBeginPolicy: WithdrawalValidator;
      readonly scriptSourcesStageSixFoldAsset: WithdrawalValidator;
      readonly scriptSourcesStageSixFinish: WithdrawalValidator;
      readonly scriptSourcesObserverItem: WithdrawalValidator;
      readonly scriptSourcesObserverBound: WithdrawalValidator;
      readonly scriptSourcesRedeemerDescriptor: WithdrawalValidator;
      readonly ledgerOutputProofStructure: WithdrawalValidator;
      readonly ledgerOutputProofValue: WithdrawalValidator;
      readonly ledgerOutputProofDatumFoldMap: WithdrawalValidator;
      readonly ledgerOutputProofDatumFinalizeFrame: WithdrawalValidator;
      readonly ledgerOutputProofDatumHeadScalar: WithdrawalValidator;
      readonly ledgerOutputProofDatumAttachInteger: WithdrawalValidator;
      readonly ledgerOutputProofDatumFoldList: WithdrawalValidator;
      readonly ledgerOutputProofDatumAdvanceInteger: WithdrawalValidator;
      readonly ledgerOutputProofReferenceScript: WithdrawalValidator;
      readonly ledgerOutputProofScriptHash: WithdrawalValidator;
      readonly ledgerOutputProofNativeScript: WithdrawalValidator;
      readonly ledgerOutputProofStructureAssets: WithdrawalValidator;
      readonly ledgerOutputProofStructureOptional: WithdrawalValidator;
      readonly ledgerOutputProofStructureFinish: WithdrawalValidator;
      readonly ledgerOutputProofDatumHeadSequence: WithdrawalValidator;
      readonly ledgerOutputProofDatumHeadMap: WithdrawalValidator;
      readonly ledgerOutputProofDatumHeadLargeConstructor: WithdrawalValidator;
      readonly ledgerOutputProofDatumAttachBytes: WithdrawalValidator;
      readonly ledgerOutputProofDatumAdvanceBytes: WithdrawalValidator;
      readonly ledgerOutputProofDatumFinish: WithdrawalValidator;
      readonly ledgerOutputProofDatumLargeConstructor: WithdrawalValidator;
      readonly ledgerOutputProofDatumLargeFields: WithdrawalValidator;
      readonly ledgerOutputProofDatumClose: WithdrawalValidator;
      readonly ledgerOutputProofSpan: WithdrawalValidator;
      readonly ledgerOutputProofScalarInteger: WithdrawalValidator;
      readonly ledgerOutputProofScalarBytes: WithdrawalValidator;
      readonly ledgerOutputDescriptorScanFacts: WithdrawalValidator;
      readonly ledgerOutputDescriptorReferenceScript: WithdrawalValidator;
      readonly ledgerOutputDescriptorDatumSummary: WithdrawalValidator;
      readonly ledgerOutputDescriptorValueSummary: WithdrawalValidator;
      readonly phaseANativeItemNative: WithdrawalValidator;
      readonly phaseANativeItemForeign: WithdrawalValidator;
      readonly cekMaterialProgramTask: WithdrawalValidator;
      readonly cekMaterialDataTask: WithdrawalValidator;
      readonly cekSelectionAuthenticate: WithdrawalValidator;
      readonly cekSelectionSuccessor: WithdrawalValidator;
      readonly cekSelectionMaterialProgram: WithdrawalValidator;
      readonly cekSelectionMaterialData: WithdrawalValidator;
      readonly valueAndMintAssetFold: WithdrawalValidator;
    };
    readonly semanticResolvers: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
    readonly resolvers: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildValidationTraceDisputeFaultProofContractsParams =
  BuildFaultProofContractsParams & {
    readonly referenceScriptAuthPolicyId: string;
  };
