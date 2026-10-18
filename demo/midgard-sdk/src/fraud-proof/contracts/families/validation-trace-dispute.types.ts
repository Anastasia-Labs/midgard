import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import { type OutRefLike } from "@al-ft/midgard-core/out-ref";

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
  readonly fieldPreimageCertificate: MintingValidator;
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
    /** Spend: awarded threads and receipts. Mint: the lost-defense receipts. */
    readonly award: AuthenticatedValidator;
    /**
     * Holds each case's challenger bond apart from the case; spent only by
     * the case's close. Not a step.
     */
    readonly disputeBond: SpendingValidator;
    /**
     * The validation case: the case NFT, the bond marker and the thread
     * tokens, whose policy every thread holder takes as its thread policy.
     * Not a step.
     */
    readonly case: AuthenticatedValidator;
    /**
     * The one-shot policy of the case configuration token, whose inline
     * `CaseConfigV1` datum names the case's hand-off scripts. Not a step.
     */
    readonly caseConfig: MintingValidator;
    /** The withdraw-zero script that releases threads past D_case. */
    readonly threadExpiry: WithdrawalValidator;
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
    readonly canonicalDecodePrepare: SpendingValidator;
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
    ];
  };
};

export type BuildValidationTraceDisputeFaultProofContractsParams =
  BuildFaultProofContractsParams & {
    readonly referenceScriptAuthPolicyId: string;
    /**
     * The deployment's L1 slot mapping, applied to the txInfo stage and
     * recorded in the deployment manifest.
     */
    readonly l1SlotConfig: L1SlotConfig;
    /**
     * The validation-dispute bond VerifySource locks at `dispute_bond_v1`, in
     * lovelace: the deployment's `MIDGARD_DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE`.
     * A parameter of the source validator.
     */
    readonly disputeBondLovelace: bigint;
    /**
     * The out-ref the case configuration policy is a one-shot on: the hub
     * oracle nonce, spent by the atomic protocol initialization that mints
     * the configuration token.
     */
    readonly caseConfigInitializationNonce: OutRefLike;
  };
