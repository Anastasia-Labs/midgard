import * as SDK from "@al-ft/midgard-sdk";

import { transitionTraceError } from "./errors.js";
import {
  eventKeyPhase,
  type TransitionTraceReconstruction,
} from "./reconstruct.js";
import {
  eventKeyFromOutOfWindowEvidence,
  type OutOfWindowSourceEventEvidence,
} from "./witnesses.build-omitted-due-l1-event-fault.js";
import {
  buildIndexedTraceProof,
  membershipProof,
  sourceEventOrThrow,
} from "./witnesses.build-source-membership-proof.js";

export const buildOutOfWindowSourceEventFault = async ({
  reconstruction,
  evidence,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly evidence: OutOfWindowSourceEventEvidence;
}): Promise<SDK.TransitionFault> => {
  const eventKey = eventKeyFromOutOfWindowEvidence(evidence);
  const source = sourceEventOrThrow(reconstruction, eventKey);
  if (source.phase !== eventKeyPhase(eventKey)) {
    throw transitionTraceError(
      "proofConstructionFailed",
      "Out-of-window source evidence phase does not match the source event key.",
    );
  }
  switch (evidence.kind) {
    case "deposit":
      if (source.phase !== "Deposit") {
        throw transitionTraceError(
          "proofConstructionFailed",
          "Wrong deposit source variant.",
        );
      }
      return SDK.outOfWindowSourceEventFault({
        OutOfWindowDeposit: {
          source_membership: await membershipProof({
            root: reconstruction.rootData.deposits,
            entry: source.entry,
          }),
        },
      });
    case "withdrawal":
      if (source.phase !== "Withdrawal") {
        throw transitionTraceError(
          "proofConstructionFailed",
          "Wrong withdrawal source variant.",
        );
      }
      return SDK.outOfWindowSourceEventFault({
        OutOfWindowWithdrawal: {
          source_membership: await membershipProof({
            root: reconstruction.rootData.withdrawals,
            entry: source.entry,
          }),
        },
      });
    case "forcedTransaction":
      if (source.phase !== "ForcedTransaction") {
        throw transitionTraceError(
          "proofConstructionFailed",
          "Wrong forced transaction source variant.",
        );
      }
      return SDK.outOfWindowSourceEventFault({
        OutOfWindowForcedTransaction: {
          event_ref_input_index: evidence.eventRefInputIndex,
          event_asset_name: evidence.eventAssetName,
          validity_override: evidence.validityOverride,
          source_membership: await membershipProof({
            root: reconstruction.rootData.forcedTransactions,
            entry: source.entry,
          }),
        },
      });
  }
};

export const buildDuplicateTraceEventFault = async ({
  reconstruction,
  leftStepIndex,
  rightStepIndex,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly leftStepIndex: bigint;
  readonly rightStepIndex: bigint;
}): Promise<SDK.TransitionFault> =>
  SDK.duplicateTraceEventFault({
    leftTrace: await buildIndexedTraceProof({
      reconstruction,
      stepIndex: leftStepIndex,
    }),
    rightTrace: await buildIndexedTraceProof({
      reconstruction,
      stepIndex: rightStepIndex,
    }),
  });

export const buildCountFault = (
  witness: SDK.CountFaultWitness,
): SDK.TransitionFault => SDK.countFault(witness);

export const buildTransitionFaultProof = ({
  reconstruction,
  fault,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly fault: SDK.TransitionFault;
}): SDK.TransitionFaultProof =>
  SDK.makeTransitionFaultProof({
    challengedHeaderHash: reconstruction.headerHash,
    header: reconstruction.header,
    fault,
  });
