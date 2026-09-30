import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

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
  buildEventToStepMembershipProof,
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

/** Reopens the operator's retained endpoints and exact counted memberships.
 * The caller supplies an authenticated reconstruction, never an honest replay
 * tree in place of the operator's committed trace. */
export const buildRetainedValidationClaimWitness = async ({
  reconstruction,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly eventKey: SDK.EventKey;
}): Promise<{
  readonly claim: SDK.ValidationClaimWitness;
  readonly terminalWorkWitnessCbor: string;
}> => {
  const keyBytes = Buffer.from(Data.to(eventKey, SDK.EventKey), "hex");
  const entries = reconstruction.rootData.validationTraces.entries.filter(
    (entry) => entry.key.equals(keyBytes),
  );
  if (entries.length !== 1)
    throw transitionTraceError(
      "missingWitnessData",
      "Selected retained validation descriptor is absent or duplicated.",
    );
  const descriptor = Data.from(
    entries[0]!.value.toString("hex"),
    SDK.ValidationTraceDescriptor,
  );
  const endpoints = SDK.readRetainedValidationEndpoints({
    entries: reconstruction.payload.block_body.validation_trace_witnesses,
    eventKey,
    descriptor,
  });
  const source = sourceEventOrThrow(reconstruction, eventKey);
  let sourceMembership: SDK.ValidationClaimWitness["source_membership"];
  if (source.phase === "L2Transaction") {
    sourceMembership = {
      NormalValidationSource: {
        membership: await membershipProof({
          root: reconstruction.rootData.transactions,
          entry: {
            key: source.entry.txId,
            keyBytes: source.entry.keyBytes,
            value: source.entry.value,
            valueBytes: source.entry.valueBytes,
          },
        }),
      },
    };
  } else if (source.phase === "ForcedTransaction") {
    sourceMembership = {
      ForcedValidationSource: {
        membership: await membershipProof({
          root: reconstruction.rootData.forcedTransactions,
          entry: source.entry,
        }),
      },
    };
  } else
    throw transitionTraceError(
      "missingWitnessData",
      "Validation claims require a transaction source.",
    );
  const eventToStep = await buildEventToStepMembershipProof({
    reconstruction,
    eventKey,
  });
  const claim: SDK.ValidationClaimWitness = {
    version: 1n,
    descriptor_membership: await membershipProof({
      root: reconstruction.rootData.validationTraces,
      entry: {
        key: eventKey,
        value: descriptor,
        keyBytes,
        valueBytes: entries[0]!.value,
      },
    }),
    transition_step_membership: await buildIndexedTraceProof({
      reconstruction,
      stepIndex: eventToStep.value.step_index,
    }),
    event_to_step_membership: eventToStep,
    source_membership: sourceMembership,
    validation_context_cbor: endpoints.initial.witness_cbor,
    initial_state: endpoints.initial.machine_state,
    terminal_state: endpoints.terminal.machine_state,
    initial_state_proof: endpoints.initial.trace_proof,
    terminal_state_proof: endpoints.terminal.trace_proof,
  };
  return Object.freeze({
    claim,
    terminalWorkWitnessCbor: endpoints.terminal.witness_cbor,
  });
};
