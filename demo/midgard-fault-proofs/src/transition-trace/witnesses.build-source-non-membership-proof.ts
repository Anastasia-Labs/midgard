import * as SDK from "@al-ft/midgard-sdk";

import { transitionTraceError } from "./errors.js";
import {
  encodeData,
  type TransitionTraceReconstruction,
} from "./reconstruct.js";
import {
  buildAdjacentTraceProof,
  buildEventToStepMembershipProof,
  buildEventToStepNonMembershipProof,
  buildEventToStepProof,
  buildIndexedTraceProof,
  buildSourceMembershipProof,
  membershipProof,
  nonMembershipProof,
  requireTraceEntry,
  sourceEventOrThrow,
} from "./witnesses.build-source-membership-proof.js";

export const buildSourceNonMembershipProof = async ({
  reconstruction,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly eventKey: SDK.EventKey;
}): Promise<SDK.TransitionSourceNonMembershipProof> => {
  if ("WithdrawalEventKey" in eventKey) {
    return {
      WithdrawalSourceNonMembership: {
        non_membership: await nonMembershipProof({
          root: reconstruction.rootData.withdrawals,
          key: eventKey.WithdrawalEventKey.withdrawal_id,
          keyBytes: encodeData(
            eventKey.WithdrawalEventKey.withdrawal_id,
            SDK.OutputReference as never,
          ),
        }),
      },
    };
  }
  if ("ForcedTransactionEventKey" in eventKey) {
    return {
      ForcedTransactionSourceNonMembership: {
        non_membership: await nonMembershipProof({
          root: reconstruction.rootData.forcedTransactions,
          key: eventKey.ForcedTransactionEventKey.tx_order_id,
          keyBytes: encodeData(
            eventKey.ForcedTransactionEventKey.tx_order_id,
            SDK.OutputReference as never,
          ),
        }),
      },
    };
  }
  if ("DepositEventKey" in eventKey) {
    return {
      DepositSourceNonMembership: {
        non_membership: await nonMembershipProof({
          root: reconstruction.rootData.deposits,
          key: eventKey.DepositEventKey.deposit_id,
          keyBytes: encodeData(
            eventKey.DepositEventKey.deposit_id,
            SDK.OutputReference as never,
          ),
        }),
      },
    };
  }
  return {
    L2TransactionSourceNonMembership: {
      non_membership: await nonMembershipProof({
        root: reconstruction.rootData.transactions,
        key: eventKey.L2TransactionEventKey.tx_id,
        keyBytes: Buffer.from(eventKey.L2TransactionEventKey.tx_id, "hex"),
      }),
    },
  };
};

export const buildTraceBoundaryFault = async ({
  reconstruction,
  side,
  stepIndex,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly side: SDK.TraceBoundarySide;
  readonly stepIndex: bigint;
}): Promise<SDK.TransitionFault> =>
  SDK.traceBoundaryFault({
    side,
    traceProof: await buildIndexedTraceProof({ reconstruction, stepIndex }),
  });

export const buildTraceLinkFault = async ({
  reconstruction,
  lowerStepIndex,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly lowerStepIndex: bigint;
}): Promise<SDK.TransitionFault> =>
  SDK.traceLinkFault(
    await buildAdjacentTraceProof({ reconstruction, lowerStepIndex }),
  );

export const buildEventToStepMismatchFault = async ({
  reconstruction,
  stepIndex,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly stepIndex: bigint;
}): Promise<SDK.TransitionFault> => {
  const traceProof = await buildIndexedTraceProof({
    reconstruction,
    stepIndex,
  });
  return SDK.eventToStepMismatchFault({
    traceProof,
    eventToStep: await buildEventToStepProof({
      reconstruction,
      eventKey: traceProof.value.event_key,
    }),
  });
};

export const buildMappedEventMissingFromSourceFault = async ({
  reconstruction,
  stepIndex,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly stepIndex: bigint;
  readonly eventKey: SDK.EventKey;
}): Promise<SDK.TransitionFault> =>
  SDK.sourceMembershipMismatchFault({
    MappedEventMissingFromSource: {
      trace_proof: await buildIndexedTraceProof({ reconstruction, stepIndex }),
      event_to_step: await buildEventToStepMembershipProof({
        reconstruction,
        eventKey,
      }),
      source_non_membership: await buildSourceNonMembershipProof({
        reconstruction,
        eventKey,
      }),
    },
  });

export const buildSourceEventMissingTraceFault = async ({
  reconstruction,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly eventKey: SDK.EventKey;
}): Promise<SDK.TransitionFault> =>
  SDK.sourceMembershipMismatchFault({
    SourceEventMissingTrace: {
      source_membership: await buildSourceMembershipProof({
        reconstruction,
        eventKey,
      }),
      event_to_step_non_membership: await buildEventToStepNonMembershipProof({
        reconstruction,
        eventKey,
      }),
    },
  });

export const buildSourcePhaseMismatchFault = async ({
  reconstruction,
  stepIndex,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly stepIndex: bigint;
}): Promise<SDK.TransitionFault> => {
  const trace = requireTraceEntry(reconstruction, stepIndex);
  return SDK.sourceMembershipMismatchFault({
    SourcePhaseMismatch: {
      trace_proof: await buildIndexedTraceProof({ reconstruction, stepIndex }),
      source_membership: await buildSourceMembershipProof({
        reconstruction,
        eventKey: trace.value.event_key,
      }),
    },
  });
};

export type ValidWithdrawalTransitionEvidence = {
  readonly spentUtxo: SDK.LedgerDeleteWitness;
};

export type ValidDepositTransitionEvidence = {
  readonly projectedUtxo: SDK.LedgerInsertWitness;
};

/**
 * Mutation proofs for replaying an authenticated L2 transaction against its
 * committed pre-state root. The transaction input/output preimages themselves
 * are taken from the authenticated retained-DA transaction, not caller input.
 */
export type L2TransactionTransitionEvidence = {
  readonly spentUtxos: readonly SDK.LedgerDeleteWitness[];
  readonly producedUtxos: readonly SDK.LedgerInsertWitness[];
};

export type AcceptedTransactionTransitionMismatchEvidence = {
  readonly claim: SDK.ValidationClaimWitness;
  readonly terminalAcceptanceWitnessCbor: string;
};

export const buildInvalidWithdrawalNoOpWitness = async ({
  reconstruction,
  stepIndex,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly stepIndex: bigint;
}): Promise<SDK.InvalidOneStepTransitionWitness> => {
  const trace = requireTraceEntry(reconstruction, stepIndex);
  const source = sourceEventOrThrow(reconstruction, trace.value.event_key);
  if (source.phase !== "Withdrawal") {
    throw transitionTraceError(
      "missingWitnessData",
      `Trace step ${stepIndex.toString()} is not a withdrawal step.`,
    );
  }
  if (source.entry.value.validity === "WithdrawalIsValid") {
    throw transitionTraceError(
      "missingWitnessData",
      "InvalidWithdrawalNoOpTransition requires a withdrawal source classified as invalid.",
    );
  }
  const sourceMembership = await membershipProof({
    root: reconstruction.rootData.withdrawals,
    entry: source.entry,
  });
  return {
    InvalidWithdrawalNoOpTransition: {
      trace_proof: await buildIndexedTraceProof({ reconstruction, stepIndex }),
      event_to_step: await buildEventToStepMembershipProof({
        reconstruction,
        eventKey: trace.value.event_key,
      }),
      source_membership: sourceMembership,
    },
  };
};

export const buildValidWithdrawalTransitionWitness = async ({
  reconstruction,
  stepIndex,
  evidence,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly stepIndex: bigint;
  readonly evidence: ValidWithdrawalTransitionEvidence;
}): Promise<SDK.InvalidOneStepTransitionWitness> => {
  const trace = requireTraceEntry(reconstruction, stepIndex);
  const source = sourceEventOrThrow(reconstruction, trace.value.event_key);
  if (source.phase !== "Withdrawal") {
    throw transitionTraceError(
      "missingWitnessData",
      `Trace step ${stepIndex.toString()} is not a withdrawal step.`,
    );
  }
  if (source.entry.value.validity !== "WithdrawalIsValid") {
    throw transitionTraceError(
      "missingWitnessData",
      "ValidWithdrawalTransition requires a withdrawal source classified as valid.",
    );
  }
  return {
    ValidWithdrawalTransition: {
      trace_proof: await buildIndexedTraceProof({ reconstruction, stepIndex }),
      event_to_step: await buildEventToStepMembershipProof({
        reconstruction,
        eventKey: trace.value.event_key,
      }),
      source_membership: await membershipProof({
        root: reconstruction.rootData.withdrawals,
        entry: source.entry,
      }),
      spent_utxo: evidence.spentUtxo,
    },
  };
};
