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
  type SourceEventRecord,
  type TransitionTraceReconstruction,
} from "./reconstruct.js";

const countedPhasView = (root: CountedRoot): KeyValuePhasRoot => ({
  root: root.phasRoot,
  count: root.count,
  entries: root.entries,
});

export const membershipProof = async <K, V>({
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

export const nonMembershipProof = async <K>({
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

export const requireTraceEntry = (
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

export const sourceEventOrThrow = (
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

/**
 * The raw forced-transaction leaf membership — the
 * `RootMembershipProof<OutputReference, ForcedInclusionTxV1>` shape itself,
 * outside the `TransitionSourceMembershipProof` enum wrapper
 * `buildSourceMembershipProof` returns. The decoding-fault family's step-02
 * (`forced_membership`) consumes the leaf directly.
 */
export const buildForcedTransactionLeafMembershipProof = async ({
  reconstruction,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly eventKey: SDK.EventKey;
}): Promise<
  SDK.RootMembershipProof<SDK.OutputReference, SDK.ForcedInclusionTxV1>
> => {
  const event = sourceEventOrThrow(reconstruction, eventKey);
  if (event.phase !== "ForcedTransaction") {
    throw transitionTraceError(
      "missingWitnessData",
      `Cannot build forced-transaction leaf membership proof: event key ${eventKeyFingerprint(
        eventKey,
      )} is not a forced-transaction event.`,
    );
  }
  return await membershipProof({
    root: reconstruction.rootData.forcedTransactions,
    entry: event.entry,
  });
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
    value: entry.valueBytes.toString("hex"),
    proof: await keyValuePhasProof(
      countedPhasView(reconstruction.rootData.transactions),
      entry.keyBytes,
      entry.valueBytes,
    ),
  };
};
