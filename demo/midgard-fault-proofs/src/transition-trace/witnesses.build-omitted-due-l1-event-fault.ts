import * as SDK from "@al-ft/midgard-sdk";

import { transitionTraceError } from "./errors.js";
import { type TransitionTraceReconstruction } from "./reconstruct.js";
import {
  buildEventToStepMembershipProof,
  buildIndexedTraceProof,
  buildRawL2TransactionSourceMembershipProof,
  membershipProof,
  requireTraceEntry,
  sourceEventOrThrow,
} from "./witnesses.build-source-membership-proof.js";
import {
  type AcceptedTransactionTransitionMismatchEvidence,
  buildSourceNonMembershipProof,
  type L2TransactionTransitionEvidence,
  type ValidDepositTransitionEvidence,
} from "./witnesses.build-source-non-membership-proof.js";

export const buildInvalidForcedTransactionNoOpWitness = async ({
  reconstruction,
  stepIndex,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly stepIndex: bigint;
}): Promise<SDK.InvalidOneStepTransitionWitness> => {
  const trace = requireTraceEntry(reconstruction, stepIndex);
  const source = sourceEventOrThrow(reconstruction, trace.value.event_key);
  if (source.phase !== "ForcedTransaction") {
    throw transitionTraceError(
      "missingWitnessData",
      `Trace step ${stepIndex.toString()} is not a forced-transaction step.`,
    );
  }
  if (source.entry.value.verdict === "ForcedTxValid") {
    throw transitionTraceError(
      "missingWitnessData",
      "InvalidForcedTransactionNoOpTransition requires a forced source classified as invalid; ForcedTxValid sources use the accepted-transition or validation-verdict proof path.",
    );
  }
  return {
    InvalidForcedTransactionNoOpTransition: {
      trace_proof: await buildIndexedTraceProof({ reconstruction, stepIndex }),
      event_to_step: await buildEventToStepMembershipProof({
        reconstruction,
        eventKey: trace.value.event_key,
      }),
      source_membership: await membershipProof({
        root: reconstruction.rootData.forcedTransactions,
        entry: source.entry,
      }),
    },
  };
};

export const buildValidDepositTransitionWitness = async ({
  reconstruction,
  stepIndex,
  evidence,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly stepIndex: bigint;
  readonly evidence: ValidDepositTransitionEvidence;
}): Promise<SDK.InvalidOneStepTransitionWitness> => {
  const trace = requireTraceEntry(reconstruction, stepIndex);
  const source = sourceEventOrThrow(reconstruction, trace.value.event_key);
  if (source.phase !== "Deposit") {
    throw transitionTraceError(
      "missingWitnessData",
      `Trace step ${stepIndex.toString()} is not a deposit step.`,
    );
  }
  return {
    ValidDepositTransition: {
      trace_proof: await buildIndexedTraceProof({ reconstruction, stepIndex }),
      event_to_step: await buildEventToStepMembershipProof({
        reconstruction,
        eventKey: trace.value.event_key,
      }),
      source_membership: await membershipProof({
        root: reconstruction.rootData.deposits,
        entry: source.entry,
      }),
      projected_utxo: evidence.projectedUtxo,
    },
  };
};

export const buildL2TransactionTransitionWitness = async ({
  reconstruction,
  stepIndex,
  evidence,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly stepIndex: bigint;
  readonly evidence: L2TransactionTransitionEvidence;
}): Promise<SDK.InvalidOneStepTransitionWitness> => {
  const trace = requireTraceEntry(reconstruction, stepIndex);
  const source = sourceEventOrThrow(reconstruction, trace.value.event_key);
  if (source.phase !== "L2Transaction") {
    throw transitionTraceError(
      "missingWitnessData",
      `Trace step ${stepIndex.toString()} is not an L2-transaction step.`,
    );
  }
  if (source.entry.validity !== "TxIsValid") {
    throw transitionTraceError(
      "missingWitnessData",
      "L2TransactionTransition requires a transaction source classified as TxIsValid.",
    );
  }
  return {
    L2TransactionTransition: {
      trace_proof: await buildIndexedTraceProof({ reconstruction, stepIndex }),
      event_to_step: await buildEventToStepMembershipProof({
        reconstruction,
        eventKey: trace.value.event_key,
      }),
      source_membership: await buildRawL2TransactionSourceMembershipProof({
        reconstruction,
        txId: source.entry.txId,
      }),
      spend_inputs_preimage: source.entry.spendInputsPreimage.toString("hex"),
      outputs_preimage: source.entry.outputsPreimage.toString("hex"),
      spent_utxos: evidence.spentUtxos,
      produced_utxos: evidence.producedUtxos,
    },
  };
};

export const buildAcceptedTransactionTransitionMismatchFault = ({
  claim,
  terminalAcceptanceWitnessCbor,
}: AcceptedTransactionTransitionMismatchEvidence): SDK.TransitionFault =>
  SDK.acceptedTransactionTransitionMismatchFault({
    claim,
    terminalAcceptanceWitnessCbor,
  });

export type OmittedDueL1EventEvidence =
  | {
      readonly kind: "deposit";
      readonly depositId: SDK.OutputReference;
    }
  | {
      readonly kind: "withdrawal";
      readonly withdrawalId: SDK.OutputReference;
    }
  | {
      readonly kind: "forcedTransaction";
      readonly txOrderId: SDK.OutputReference;
      readonly eventRefInputIndex: bigint;
      readonly eventAssetName: string;
      readonly validityOverride: SDK.OperatorVerdict;
    };

export const eventKeyFromOmittedEvidence = (
  evidence: OmittedDueL1EventEvidence,
): SDK.EventKey => {
  switch (evidence.kind) {
    case "deposit":
      return { DepositEventKey: { deposit_id: evidence.depositId } };
    case "withdrawal":
      return { WithdrawalEventKey: { withdrawal_id: evidence.withdrawalId } };
    case "forcedTransaction":
      return {
        ForcedTransactionEventKey: { tx_order_id: evidence.txOrderId },
      };
  }
};

export const buildOmittedDueL1EventFault = async ({
  reconstruction,
  evidence,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly evidence: OmittedDueL1EventEvidence;
}): Promise<SDK.TransitionFault> => {
  const eventKey = eventKeyFromOmittedEvidence(evidence);
  const sourceNonMembership = await buildSourceNonMembershipProof({
    reconstruction,
    eventKey,
  });
  switch (evidence.kind) {
    case "deposit":
      if (!("DepositSourceNonMembership" in sourceNonMembership)) {
        throw transitionTraceError(
          "proofConstructionFailed",
          "Wrong deposit non-membership variant.",
        );
      }
      return SDK.omittedDueL1EventFault({
        OmittedDueDeposit: {
          source_non_membership:
            sourceNonMembership.DepositSourceNonMembership.non_membership,
        },
      });
    case "withdrawal":
      if (!("WithdrawalSourceNonMembership" in sourceNonMembership)) {
        throw transitionTraceError(
          "proofConstructionFailed",
          "Wrong withdrawal non-membership variant.",
        );
      }
      return SDK.omittedDueL1EventFault({
        OmittedDueWithdrawal: {
          source_non_membership:
            sourceNonMembership.WithdrawalSourceNonMembership.non_membership,
        },
      });
    case "forcedTransaction":
      if (!("ForcedTransactionSourceNonMembership" in sourceNonMembership)) {
        throw transitionTraceError(
          "proofConstructionFailed",
          "Wrong forced transaction non-membership variant.",
        );
      }
      return SDK.omittedDueL1EventFault({
        OmittedDueForcedTransaction: {
          event_ref_input_index: evidence.eventRefInputIndex,
          event_asset_name: evidence.eventAssetName,
          validity_override: evidence.validityOverride,
          source_non_membership:
            sourceNonMembership.ForcedTransactionSourceNonMembership
              .non_membership,
        },
      });
  }
};

export type OutOfWindowSourceEventEvidence =
  | {
      readonly kind: "deposit";
      readonly depositId: SDK.OutputReference;
    }
  | {
      readonly kind: "withdrawal";
      readonly withdrawalId: SDK.OutputReference;
    }
  | {
      readonly kind: "forcedTransaction";
      readonly txOrderId: SDK.OutputReference;
      readonly eventRefInputIndex: bigint;
      readonly eventAssetName: string;
      readonly validityOverride: SDK.OperatorVerdict;
    };

export const eventKeyFromOutOfWindowEvidence = (
  evidence: OutOfWindowSourceEventEvidence,
): SDK.EventKey => {
  switch (evidence.kind) {
    case "deposit":
      return { DepositEventKey: { deposit_id: evidence.depositId } };
    case "withdrawal":
      return { WithdrawalEventKey: { withdrawal_id: evidence.withdrawalId } };
    case "forcedTransaction":
      return {
        ForcedTransactionEventKey: { tx_order_id: evidence.txOrderId },
      };
  }
};
