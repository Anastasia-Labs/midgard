import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import {
  type CorrectionLockDatum,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  Header,
  type Header as HeaderType,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/fault-proof-application.js";
import { type WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";

export const DEPLOYMENT = "dd".repeat(32);

export const OBSERVATION_DIGEST = "11".repeat(32);

export const headerFixture = (suffix = "00"): HeaderType => ({
  prevUtxosRoot: "00".repeat(32),
  transactionsRoot: "01".repeat(32),
  utxosRoot: "02".repeat(32),
  depositsRoot: "03".repeat(32),
  withdrawalsRoot: "04".repeat(32),
  forcedTransactionsRoot: "05".repeat(32),
  transitionTraceRoot: "06".repeat(32),
  eventToStepRoot: "07".repeat(32),
  validationTracesRoot: "08".repeat(32),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 1n,
  depositCount: 0n,
  totalEventCount: 1n,
  transitionStepCount: 1n,
  validationTraceCount: 1n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: BigInt(`0x${suffix}`),
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "08".repeat(28),
  operatorVkey: "09".repeat(28),
  protocolVersion: 1n,
});

export const encodedHeader = (header: HeaderType) => {
  const cbor = Data.to(header, Header);
  return {
    cbor,
    hash: computeHash28(Buffer.from(cbor, "hex")).toString("hex"),
  };
};

export const observation = (
  headers: readonly HeaderType[],
  lockDatum: CorrectionLockDatum = "Idle",
): WatcherAuthenticatedStateQueueObservation => {
  const encoded = headers.map(encodedHeader);
  return Object.freeze({
    schemaVersion: "midgard-watcher-production-state-queue-observation-v1",
    deploymentIdentityDigest: DEPLOYMENT,
    protocolScriptAuthorityDigest: "10".repeat(32),
    stateQueuePolicyId: "11".repeat(28),
    hubOraclePolicyId: "12".repeat(28),
    nativePoint: Object.freeze({
      blockHash: "13".repeat(32),
      parentBlockHash: "14".repeat(32),
      slot: "1000",
      blockNo: "100",
      chainPointId: "15".repeat(32),
      finalityDepth: "30",
    }),
    sourceId: "watcher-test-local-node",
    previousObservationDigest: null,
    checkpoints: Object.freeze([]),
    finalizedQueue: Object.freeze([
      Object.freeze({ headerHash: null, outRef: `${"16".repeat(32)}#0` }),
      ...encoded.map(({ hash }, index) =>
        Object.freeze({
          headerHash: hash,
          outRef: `${"17".repeat(32)}#${index.toString()}`,
        }),
      ),
    ]),
    finalizedHeaders: Object.freeze(
      encoded.map(({ cbor, hash }, index) =>
        Object.freeze({
          headerHash: hash,
          headerCborHex: cbor,
          stateQueueNodeCborHex: "d87980",
          linkedListDatumCborHex: "d87980",
          daAvailability: "Unattested",
          queueOutRef: `${"17".repeat(32)}#${index.toString()}`,
          nextHeaderHash: encoded[index + 1]?.hash ?? null,
          observedTransactionHash: "18".repeat(32),
          observedBlockHash: "19".repeat(32),
          observedSlot: (900 + index).toString(),
          observedBlockNo: (90 + index).toString(),
          observedChainPointId: "20".repeat(32),
          finalityDepth: "30",
        }),
      ),
    ),
    finalizedCorrectionLock: Object.freeze({
      outRef: `${"21".repeat(32)}#0`,
      datum: lockDatum,
      observedTransactionHash: "22".repeat(32),
      observedBlockHash: "23".repeat(32),
      observedSlot: "950",
      observedBlockNo: "95",
      observedChainPointId: "24".repeat(32),
      finalityDepth: "30",
    }),
    correctionLockWitnesses: Object.freeze([]),
    observationDigest: "25".repeat(32),
  });
};

export const decision = (
  headerHash: string,
  category: (typeof WATCHER_INSTALLED_WORKFLOW_CATEGORIES)[number],
  decisionDigest = `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category]}${headerHash}`,
) =>
  Object.freeze({
    schemaVersion: "midgard-production-header-decision-v1" as const,
    classifierVersion: "midgard-production-header-classifier-v1" as const,
    deploymentFingerprint: DEPLOYMENT,
    headerHash,
    authenticatedObservationDigest: OBSERVATION_DIGEST,
    payloadEnvelopeSha256: "26".repeat(32),
    payloadSha256: "27".repeat(32),
    replayVersion: "midgard-complete-canonical-replay-v1" as const,
    replayDigest: "28".repeat(32),
    launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    launchScopeDigest: "29".repeat(32),
    classificationDigest: "2a".repeat(32),
    decisionDigest,
    decision: "fault_detected" as const,
    category,
    violationId: `${category}_v1`,
    detectionId: `${category}_v1:0`,
    position: "0",
  });
