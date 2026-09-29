import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  type ValidationClaimWitness,
  ValidationDisputeOpenSpendRedeemer,
  ValidationGameSpendRedeemer,
  ValidationSourceSpendRedeemer,
  ValidationTimeoutSpendRedeemer,
  type ValidationTraceDescriptor,
  type ValidationTraceProof,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../../tx-layout.js";

export type ContinueLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

export type OpenLayout = ContinueLayout & {
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

export const makeOpenRedeemer = ({
  threadUtxo,
  hubOracleUtxo,
  stateQueueBlockUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  claim,
  challengerDescriptor,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly hubOracleUtxo: UTxO;
  readonly stateQueueBlockUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly claim: ValidationClaimWitness;
  readonly challengerDescriptor: ValidationTraceDescriptor;
  readonly onLayout: (layout: OpenLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "validation dispute open");
    const layout: OpenLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "validation dispute open"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute open",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "validation dispute open hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "validation dispute open state-queue block",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            hub_ref_input_index: layout.hubOracleRefInputIndex,
            state_queue_node_ref_input_index:
              layout.stateQueueNodeRefInputIndex,
            claim,
            challenger_descriptor: challengerDescriptor,
          },
        ],
      },
      ValidationDisputeOpenSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const makeVerifySourceRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "validation dispute verify source");
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute verify source",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute verify source",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
          },
        ],
      },
      ValidationSourceSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const makeRevealRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  role,
  proof,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly role: "operator" | "challenger";
  readonly proof: ValidationTraceProof;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      `validation dispute reveal ${role}`,
    );
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        `validation dispute reveal ${role}`,
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        `validation dispute reveal ${role}`,
      ),
    };
    onLayout(layout);
    const action =
      role === "operator"
        ? {
            RevealOperator: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              proof,
            },
          }
        : {
            RevealChallenger: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              proof,
            },
          };
    return Data.to({ Continue: [action] }, ValidationGameSpendRedeemer);
  }) satisfies BuildTxWithRedeemer;

export const makeGameHandoffRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  destination,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly destination: "resolution" | "challengerTimeout";
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    const label = `validation dispute enter ${destination}`;
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        label,
      ),
    };
    onLayout(layout);
    const action =
      destination === "resolution"
        ? {
            EnterResolution: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
            },
          }
        : {
            EnterChallengerTimeout: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
            },
          };
    return Data.to({ Continue: [action] }, ValidationGameSpendRedeemer);
  }) satisfies BuildTxWithRedeemer;

export type SubmitValidationDisputeRevealResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly role: "operator" | "challenger";
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly responseDeadline: number;
  readonly awaitedConfirmation: boolean;
};

export type FinalizeLayout = ContinueLayout & {
  readonly fraudProofMintRedeemerIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
};

export const makeTimeoutSpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly onLayout: (
    layout: Omit<FinalizeLayout, "computationThreadMintRedeemerIndex">,
  ) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "validation dispute timeout");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute timeout",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputWithDatumAndUnitPredicate({
          address: fraudProofAddress,
          datum: fraudProofDatum,
          unit: fraudProofUnit,
        }),
        "validation dispute timeout fraud proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "validation dispute timeout fraud-proof mint",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
          },
        ],
      },
      ValidationTimeoutSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const makeFraudProofMintRedeemer = ({
  fraudProofPolicyId,
  computationThreadPolicyId,
  computationThreadAssetName,
  onComputationThreadMintRedeemerIndex,
}: {
  readonly fraudProofPolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly onComputationThreadMintRedeemerIndex: (index: bigint) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      fraudProofPolicyId,
      "validation dispute fraud-proof mint",
    );
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "validation dispute computation-thread burn",
    );
    onComputationThreadMintRedeemerIndex(computationThreadMintRedeemerIndex);
    return Data.to(
      {
        computation_thread_token_asset_name: computationThreadAssetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const makeComputationThreadSuccessRedeemer = ({
  computationThreadPolicyId,
  computationThreadAssetName,
}: {
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      computationThreadPolicyId,
      "validation dispute computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
