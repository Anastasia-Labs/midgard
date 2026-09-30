import {
  CompletedFraudWitness,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  TransitionFaultProof,
  TransitionTraceFinalSpendRedeemer,
  TransitionTraceL1EventFinalSpendRedeemer,
  TransitionTraceYieldFinalSpendRedeemer,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type RedeemerContext,
  type UTxO,
} from "@lucid-evolution/lucid";

import { transitionTraceError } from "./errors.js";
import {
  readTransitionProof,
  type TransitionProofMaterial,
} from "./proof-material.js";
import {
  fraudProofOutputPredicate,
  type TransitionTraceFinalSpendLayout,
} from "./submit.make-transition-trace-route-spend-redeemer.js";

export const makeTransitionTraceFinalSpendRedeemer = ({
  threadUtxo,
  hubOracleUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  yieldReferences = [],
  proofReferences = [],
  completedFraudWitness,
  l1Event = false,
  eventReference,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly hubOracleUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly yieldReferences?: readonly UTxO[];
  readonly proofReferences?: readonly UTxO[];
  readonly l1Event?: boolean;
  readonly eventReference?: { readonly order: UTxO; readonly external?: UTxO };
  readonly completedFraudWitness?: (
    ctx: RedeemerContext,
  ) => CompletedFraudWitness;
  readonly onLayout: (layout: TransitionTraceFinalSpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "transition-trace final proof");
    const layout: TransitionTraceFinalSpendLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "transition-trace final proof",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputPredicate({
          fraudProofAddress,
          fraudProofUnit,
          fraudProofDatum,
        }),
        "transition-trace fraud-proof output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "transition-trace proof hub oracle",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "transition-trace fraud-proof mint",
      ),
    };
    onLayout(layout);
    const args = {
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      hub_ref_input_index: layout.hubOracleRefInputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    };
    if (l1Event) {
      if (
        completedFraudWitness === undefined ||
        eventReference === undefined ||
        yieldReferences.length !== 1
      )
        throw new Error(
          "Timed transition final requires its event reference and completed-fraud queue witness",
        );
      return Data.to(
        {
          Continue: [
            {
              ...args,
              event_reference: {
                order_index: requireReferenceInputIndex(
                  ctx,
                  eventReference.order,
                  "timed transition Order",
                ),
                external_data_index:
                  eventReference.external === undefined
                    ? null
                    : requireReferenceInputIndex(
                        ctx,
                        eventReference.external,
                        "timed transition retained data",
                      ),
              },
              completed_fraud_witness: Data.from<Data>(
                Data.to(completedFraudWitness(ctx), CompletedFraudWitness),
              ),
              yield_ref_input_index: requireReferenceInputIndex(
                ctx,
                yieldReferences[0]!,
                "timed transition semantic yield",
              ),
            },
          ],
        },
        TransitionTraceL1EventFinalSpendRedeemer,
      );
    }
    return proofReferences.length === 0
      ? Data.to({ Continue: [args] }, TransitionTraceFinalSpendRedeemer)
      : Data.to(
          {
            Continue: [
              {
                ...args,
                output_ref_indices: [],
                deposit_event_ref_index: 0n,
                deposit_external_ref_index: null,
                deposit_opening: null,
                completed_fraud_witness:
                  completedFraudWitness === undefined
                    ? null
                    : Data.from<Data>(
                        Data.to(
                          completedFraudWitness(ctx),
                          CompletedFraudWitness,
                        ),
                      ),
                yield_ref_input_indices: yieldReferences.map((utxo) =>
                  requireReferenceInputIndex(
                    ctx,
                    utxo,
                    "transition-trace semantic yield",
                  ),
                ),
                proof_ref_indices: proofReferences.map((utxo) =>
                  requireReferenceInputIndex(
                    ctx,
                    utxo,
                    "transition-trace proof chunk",
                  ),
                ),
              },
            ],
          },
          TransitionTraceYieldFinalSpendRedeemer,
        );
  }) satisfies BuildTxWithRedeemer;

const unreachableTransitionVariant = (value: never): never => {
  const variant =
    typeof value === "object" && value !== null
      ? Object.keys(value).join(",")
      : String(value);
  throw transitionTraceError(
    "submissionRejected",
    `Unsupported transition-trace proof variant: ${variant}`,
  );
};

export const transitionTraceFinalIndex = (
  input: Pick<TransitionFaultProof, "fault"> | TransitionProofMaterial,
): number => {
  const { fault } = "proofCbor" in input ? readTransitionProof(input) : input;
  if (
    "TraceBoundaryFault" in fault ||
    "TraceLinkFault" in fault ||
    "EventToStepMismatch" in fault ||
    "CountFault" in fault
  ) {
    return 0;
  }
  if ("SourceMembershipMismatch" in fault) {
    return 1;
  }
  if ("InvalidOneStepTransition" in fault) {
    const { witness } = fault.InvalidOneStepTransition;
    if (
      "ValidWithdrawalTransition" in witness ||
      "InvalidWithdrawalNoOpTransition" in witness
    ) {
      return 2;
    }
    if ("InvalidForcedTransactionNoOpTransition" in witness) {
      return 3;
    }
    if ("L2TransactionTransition" in witness) {
      return 4;
    }
    if ("ValidDepositTransition" in witness) {
      return 5;
    }
    return unreachableTransitionVariant(witness);
  }
  if ("AcceptedTransactionTransitionMismatch" in fault) {
    return 4;
  }
  if ("OmittedDueL1Event" in fault || "OutOfWindowSourceEvent" in fault) {
    return 6;
  }
  if ("DuplicateTraceEvent" in fault) {
    return 7;
  }
  return unreachableTransitionVariant(fault);
};

/** Semantic identity only; mutable Order pointers are fetched at final capture. */
export const transitionTraceHistoryTimingTarget = (
  proof: TransitionFaultProof,
) => {
  const fault = proof.fault;
  if ("OmittedDueL1Event" in fault) {
    const witness = fault.OmittedDueL1Event.witness;
    if ("OmittedDueDeposit" in witness)
      return {
        kind: "Deposit" as const,
        id: witness.OmittedDueDeposit.source_non_membership.key,
      };
    if ("OmittedDueWithdrawal" in witness)
      return {
        kind: "Withdrawal" as const,
        id: witness.OmittedDueWithdrawal.source_non_membership.key,
      };
  }
  if ("OutOfWindowSourceEvent" in fault) {
    const witness = fault.OutOfWindowSourceEvent.witness;
    if ("OutOfWindowDeposit" in witness)
      return {
        kind: "Deposit" as const,
        id: witness.OutOfWindowDeposit.source_membership.key,
      };
    if ("OutOfWindowWithdrawal" in witness)
      return {
        kind: "Withdrawal" as const,
        id: witness.OutOfWindowWithdrawal.source_membership.key,
      };
  }
  return null;
};

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
      "transition-trace fraud-proof mint",
    );
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "transition-trace computation-thread burn",
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
      "transition-trace computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
