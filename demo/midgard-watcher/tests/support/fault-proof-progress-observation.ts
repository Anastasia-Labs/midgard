import { Header, StateQueueNode } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { unsafeAdmitWatcherStateQueueObservationForReplayTest } from "../../src/indexers/authenticated-state-queue-observation.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";

/** Canonical test observation admitted through the real persisted-data parser. */
export const progressObservation = (input: {
  deploymentFingerprint: string;
  header?: { header: Header; headerHash: string };
  revision?: number;
}) => {
  const revision = input.revision ?? 1;
  const point = {
    blockHash: revision.toString(16).padStart(64, "0"),
    parentBlockHash: null,
    slot: (4242 + revision).toString(),
    blockNo: (9000 + revision).toString(),
    chainPointId: (revision + 100).toString(16).padStart(64, "0"),
    finalityDepth: "30",
  };
  const header =
    input.header === undefined
      ? undefined
      : {
          headerHash: input.header.headerHash,
          headerCborHex: Data.to(input.header.header, Header),
          stateQueueNodeCborHex: Data.to(
            {
              proven_fraud: null,
              header: input.header.header,
              da_attestation: "Unattested",
            },
            StateQueueNode,
          ),
          linkedListDatumCborHex: "80",
          daAvailability: "Unattested" as const,
          queueOutRef: `${"41".repeat(32)}#1`,
          nextHeaderHash: null,
          observedTransactionHash: "42".repeat(32),
          observedBlockHash: point.blockHash,
          observedSlot: point.slot,
          observedBlockNo: point.blockNo,
          observedChainPointId: point.chainPointId,
          finalityDepth: "30",
        };
  const value = {
    schemaVersion: "midgard-watcher-production-state-queue-observation-v1",
    deploymentIdentityDigest: input.deploymentFingerprint,
    protocolScriptAuthorityDigest: "45".repeat(32),
    stateQueuePolicyId: "46".repeat(28),
    hubOraclePolicyId: "47".repeat(28),
    nativePoint: point,
    sourceId: "local-kupmios:test",
    previousObservationDigest: null,
    checkpoints: [],
    finalizedQueue: [
      { headerHash: null, outRef: `${"41".repeat(32)}#0` },
      ...(header === undefined
        ? []
        : [{ headerHash: header.headerHash, outRef: header.queueOutRef }]),
    ],
    finalizedHeaders: header === undefined ? [] : [header],
    finalizedCorrectionLock: null,
    correctionLockWitnesses: [],
  };
  return unsafeAdmitWatcherStateQueueObservationForReplayTest({
    ...value,
    observationDigest: watcherSha256CanonicalJson(value),
  });
};
