import * as SDK from "@al-ft/midgard-sdk";

import { transitionTraceError } from "./errors.js";
import {
  type CountedRoot,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  type KeyValuePhasRoot,
} from "./phas.js";
import {
  type DecodedRootEntry,
  encodeData,
  eventKeyFingerprint,
  eventKeyPhase,
  type SourceEventRecord,
  type TransitionTraceReconstruction,
} from "./reconstruct.js";

const countedPhasView = (root: CountedRoot): KeyValuePhasRoot => ({
  root: root.phasRoot,
  count: root.count,
  entries: root.entries,
});

const membershipProof = async <K, V>({
  root,
  entry,
}: {
  readonly root: CountedRoot;
  readonly entry: DecodedRootEntry<K, V>;
}): Promise<SDK.RootMembershipProof<K, V>> => ({
  domain: root.domain,
  root: root.root,
  phas_root: root.phasRoot,
  count: root.count,
  key: entry.key,
  value: entry.value,
  proof: await keyValuePhasProof(
    countedPhasView(root),
    entry.keyBytes,
    entry.valueBytes,
  ),
});

const nonMembershipProof = async <K>({
  root,
  key,
  keyBytes,
}: {
  readonly root: CountedRoot;
  readonly key: K;
  readonly keyBytes: Buffer;
}): Promise<SDK.RootNonMembershipProof<K>> => ({
  domain: root.domain,
  root: root.root,
  phas_root: root.phasRoot,
  count: root.count,
  key,
  proof: await keyValuePhasNonMembershipProof(countedPhasView(root), keyBytes),
});

export const rootCountProof = (root: CountedRoot): SDK.RootCountProof => ({
  domain: root.domain,
  root: root.root,
  phas_root: root.phasRoot,
  count: root.count,
});

const requireTraceEntry = (
  reconstruction: TransitionTraceReconstruction,
  stepIndex: bigint,
): DecodedRootEntry<bigint, SDK.TransitionStep> => {
  const entry = reconstruction.traceByStepIndex.get(stepIndex);
  if (entry === undefined) {
    throw transitionTraceError(
      "missingWitnessData",
      `Cannot build trace proof: step_index ${stepIndex.toString()} is absent from DA payload.`,
    );
  }
  return entry;
};

export const buildIndexedTraceProof = async ({
  reconstruction,
  stepIndex,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly stepIndex: bigint;
}): Promise<SDK.IndexedTraceProof> =>
  await membershipProof({
    root: reconstruction.rootData.transitionTrace,
    entry: requireTraceEntry(reconstruction, stepIndex),
  });

export const buildAdjacentTraceProof = async ({
  reconstruction,
  lowerStepIndex,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly lowerStepIndex: bigint;
}): Promise<SDK.AdjacentTraceProof> => ({
  lower: await buildIndexedTraceProof({
    reconstruction,
    stepIndex: lowerStepIndex,
  }),
  upper: await buildIndexedTraceProof({
    reconstruction,
    stepIndex: lowerStepIndex + 1n,
  }),
});

const eventToStepEntry = (
  reconstruction: TransitionTraceReconstruction,
  eventKey: SDK.EventKey,
): DecodedRootEntry<SDK.EventKey, SDK.EventToStepValue> | undefined =>
  reconstruction.eventToStepByFingerprint.get(eventKeyFingerprint(eventKey));

export const buildEventToStepMembershipProof = async ({
  reconstruction,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly eventKey: SDK.EventKey;
}): Promise<SDK.EventToStepMembershipProof> => {
  const entry = eventToStepEntry(reconstruction, eventKey);
  if (entry === undefined) {
    throw transitionTraceError(
      "missingWitnessData",
      `Cannot build event_to_step membership proof: event key ${eventKeyFingerprint(
        eventKey,
      )} is absent.`,
    );
  }
  return await membershipProof({
    root: reconstruction.rootData.eventToStep,
    entry,
  });
};

export const buildEventToStepNonMembershipProof = async ({
  reconstruction,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly eventKey: SDK.EventKey;
}): Promise<SDK.EventToStepNonMembershipProof> =>
  await nonMembershipProof({
    root: reconstruction.rootData.eventToStep,
    key: eventKey,
    keyBytes: encodeData(eventKey, SDK.EventKeySchema),
  });

export const buildEventToStepProof = async ({
  reconstruction,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly eventKey: SDK.EventKey;
}): Promise<SDK.EventToStepProof> => {
  const entry = eventToStepEntry(reconstruction, eventKey);
  if (entry === undefined) {
    return {
      EventToStepNonMembership: {
        non_membership: await buildEventToStepNonMembershipProof({
          reconstruction,
          eventKey,
        }),
      },
    };
  }
  return {
    EventToStepMembership: {
      membership: await buildEventToStepMembershipProof({
        reconstruction,
        eventKey,
      }),
    },
  };
};

const sourceEvent = (
  reconstruction: TransitionTraceReconstruction,
  eventKey: SDK.EventKey,
): SourceEventRecord | undefined =>
  reconstruction.sourceEventsByFingerprint.get(eventKeyFingerprint(eventKey));

const sourceEventOrThrow = (
  reconstruction: TransitionTraceReconstruction,
  eventKey: SDK.EventKey,
): SourceEventRecord => {
  const event = sourceEvent(reconstruction, eventKey);
  if (event === undefined) {
    throw transitionTraceError(
      "missingWitnessData",
      `Cannot build source membership proof: event key ${eventKeyFingerprint(
        eventKey,
      )} is absent from source roots.`,
    );
  }
  return event;
};

export const buildSourceMembershipProof = async ({
  reconstruction,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly eventKey: SDK.EventKey;
}): Promise<SDK.TransitionSourceMembershipProof> => {
  const event = sourceEventOrThrow(reconstruction, eventKey);
  switch (event.phase) {
    case "Withdrawal":
      return {
        WithdrawalSourceMembership: {
          membership: await membershipProof({
            root: reconstruction.rootData.withdrawals,
            entry: event.entry,
          }),
        },
      };
    case "ForcedTransaction":
      return {
        ForcedTransactionSourceMembership: {
          membership: await membershipProof({
            root: reconstruction.rootData.forcedTransactions,
            entry: event.entry,
          }),
        },
      };
    case "Deposit":
      return {
        DepositSourceMembership: {
          membership: await membershipProof({
            root: reconstruction.rootData.deposits,
            entry: event.entry,
          }),
        },
      };
    case "L2Transaction":
      return {
        L2TransactionSourceMembership: {
          membership: await buildRawL2TransactionSourceMembershipProof({
            reconstruction,
            txId: event.entry.txId,
          }),
        },
      };
  }
};

export const buildRawL2TransactionSourceMembershipProof = async ({
  reconstruction,
  txId,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly txId: string;
}): Promise<SDK.RawRootMembershipProof> => {
  const normalized = txId.toLowerCase();
  const entry = reconstruction.transactions.find(
    (item) => item.txId === normalized,
  );
  if (entry === undefined) {
    throw transitionTraceError(
      "missingWitnessData",
      `Cannot build L2 transaction source membership proof: tx_id ${txId} is absent.`,
    );
  }
  return {
    domain: reconstruction.rootData.transactions.domain,
    root: reconstruction.rootData.transactions.root,
    phas_root: reconstruction.rootData.transactions.phasRoot,
    count: reconstruction.rootData.transactions.count,
    key: entry.txId,
    value: entry.compactTransactionCbor.toString("hex"),
    proof: await keyValuePhasProof(
      countedPhasView(reconstruction.rootData.transactions),
      entry.keyBytes,
      entry.compactTransactionCbor,
    ),
  };
};

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
  readonly eventRefInputIndex: bigint;
  readonly eventAssetName: string;
  readonly projectedUtxo: SDK.LedgerInsertWitness;
};

export type L2TransactionTransitionEvidence = {
  readonly spendInputsPreimage: string;
  readonly outputsPreimage: string;
  readonly spentUtxos: readonly SDK.LedgerDeleteWitness[];
  readonly producedUtxos: readonly SDK.LedgerInsertWitness[];
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
  if (source.entry.value.operator_validity === "TxIsValid") {
    throw transitionTraceError(
      "unsupportedWitness",
      "Valid forced transaction transition proofs remain fail-closed until forced transaction ledger deltas/preimages exist.",
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

export const buildValidForcedTransactionUnsupportedWitness = async ({
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
  if (source.entry.value.operator_validity !== "TxIsValid") {
    throw transitionTraceError(
      "missingWitnessData",
      "ValidForcedTransactionUnsupported requires a forced transaction source classified as TxIsValid.",
    );
  }
  return {
    ValidForcedTransactionUnsupported: {
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
      event_ref_input_index: evidence.eventRefInputIndex,
      event_asset_name: evidence.eventAssetName,
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
  if (!("L2TransactionEventKey" in trace.value.event_key)) {
    throw transitionTraceError(
      "missingWitnessData",
      `Trace step ${stepIndex.toString()} is not an L2 transaction step.`,
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
        txId: trace.value.event_key.L2TransactionEventKey.tx_id,
      }),
      spend_inputs_preimage: evidence.spendInputsPreimage,
      outputs_preimage: evidence.outputsPreimage,
      spent_utxos: evidence.spentUtxos,
      produced_utxos: evidence.producedUtxos,
    },
  };
};

export type OmittedDueL1EventEvidence =
  | {
      readonly kind: "deposit";
      readonly depositId: SDK.OutputReference;
      readonly eventRefInputIndex: bigint;
      readonly eventAssetName: string;
    }
  | {
      readonly kind: "withdrawal";
      readonly withdrawalId: SDK.OutputReference;
      readonly eventRefInputIndex: bigint;
      readonly eventAssetName: string;
    }
  | {
      readonly kind: "forcedTransaction";
      readonly txOrderId: SDK.OutputReference;
      readonly eventRefInputIndex: bigint;
      readonly eventAssetName: string;
      readonly validityOverride: SDK.MidgardTxValidity;
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
          event_ref_input_index: evidence.eventRefInputIndex,
          event_asset_name: evidence.eventAssetName,
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
          event_ref_input_index: evidence.eventRefInputIndex,
          event_asset_name: evidence.eventAssetName,
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
      readonly eventRefInputIndex: bigint;
      readonly eventAssetName: string;
    }
  | {
      readonly kind: "withdrawal";
      readonly withdrawalId: SDK.OutputReference;
      readonly eventRefInputIndex: bigint;
      readonly eventAssetName: string;
      readonly validityOverride: SDK.WithdrawalValidity;
    }
  | {
      readonly kind: "forcedTransaction";
      readonly txOrderId: SDK.OutputReference;
      readonly eventRefInputIndex: bigint;
      readonly eventAssetName: string;
      readonly validityOverride: SDK.MidgardTxValidity;
    };

const eventKeyFromOutOfWindowEvidence = (
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
          event_ref_input_index: evidence.eventRefInputIndex,
          event_asset_name: evidence.eventAssetName,
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
          event_ref_input_index: evidence.eventRefInputIndex,
          event_asset_name: evidence.eventAssetName,
          validity_override: evidence.validityOverride,
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
