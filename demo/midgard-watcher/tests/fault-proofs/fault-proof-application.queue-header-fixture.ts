import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  EMPTY_MERKLE_TREE_ROOT,
  Header,
  type Header as HeaderType,
} from "@al-ft/midgard-sdk/core";
import type { AuthenticatedStateQueueHeaderObservation } from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherStateQueueHeaderObservation,
} from "../../src/indexers/authenticated-state-queue-observation.js";
import { DEPLOYMENT } from "./fault-proof-application.raw-config.js";

/**
 * Two finalized queue headers of the test deployment, with the queue
 * observation that names them and the header observations a classifier is
 * handed. A test file admits the queue observation and headers through its
 * own mock of the production admission.
 */
const SOURCE_ID = "watcher-test-local-node";
const OBSERVED_SLOT = 900n;
const OBSERVED_BLOCK_HASH = "19".repeat(32);
const CONFIRMATION_DEPTH = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;
export const OBSERVATION_DIGEST = "11".repeat(32);

/** A challenged block with a predecessor: non-empty previous UTxO set. */
const header: HeaderType = {
  prevUtxosRoot: "01".repeat(32),
  transactionsRoot: "02".repeat(32),
  utxosRoot: "03".repeat(32),
  depositsRoot: "04".repeat(32),
  withdrawalsRoot: "05".repeat(32),
  forcedTransactionsRoot: "06".repeat(32),
  transitionTraceRoot: "07".repeat(32),
  eventToStepRoot: "08".repeat(32),
  validationTracesRoot: "09".repeat(32),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 1n,
  depositCount: 0n,
  totalEventCount: 1n,
  transitionStepCount: 1n,
  validationTraceCount: 1n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 1n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "0a".repeat(28),
  operatorVkey: "0b".repeat(28),
  protocolVersion: 1n,
};
const HEADER_CBOR = Data.to(header, Header);
export const HEADER_HASH = computeHash28(
  Buffer.from(HEADER_CBOR, "hex"),
).toString("hex");

export const queueHeader: WatcherStateQueueHeaderObservation = Object.freeze({
  headerHash: HEADER_HASH,
  headerCborHex: HEADER_CBOR,
  stateQueueNodeCborHex: "d87980",
  linkedListDatumCborHex: "d87980",
  daAvailability: "Unattested",
  queueOutRef: `${"17".repeat(32)}#0`,
  nextHeaderHash: null,
  observedTransactionHash: "18".repeat(32),
  observedBlockHash: OBSERVED_BLOCK_HASH,
  observedSlot: OBSERVED_SLOT.toString(),
  observedBlockNo: "90",
  observedChainPointId: "20".repeat(32),
  finalityDepth: CONFIRMATION_DEPTH.toString(),
});

/** The genesis-ledger block: empty previous UTxO set, so no predecessor. */
const genesisHeader: HeaderType = {
  ...header,
  prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
};
const GENESIS_HEADER_CBOR = Data.to(genesisHeader, Header);
export const GENESIS_HEADER_HASH = computeHash28(
  Buffer.from(GENESIS_HEADER_CBOR, "hex"),
).toString("hex");
export const genesisQueueHeader: WatcherStateQueueHeaderObservation =
  Object.freeze({
    ...queueHeader,
    headerHash: GENESIS_HEADER_HASH,
    headerCborHex: GENESIS_HEADER_CBOR,
    queueOutRef: `${"1a".repeat(32)}#0`,
    observedTransactionHash: "1b".repeat(32),
  });

export const queueObservation: WatcherAuthenticatedStateQueueObservation =
  Object.freeze({
    schemaVersion: "midgard-watcher-production-state-queue-observation-v1",
    hubOraclePolicyId: DEPLOYMENT,
    protocolScriptAuthorityDigest: "10".repeat(32),
    stateQueuePolicyId: "11".repeat(28),
    nativePoint: Object.freeze({
      blockHash: "13".repeat(32),
      parentBlockHash: "14".repeat(32),
      slot: "1000",
      blockNo: "100",
      chainPointId: "15".repeat(32),
      finalityDepth: CONFIRMATION_DEPTH.toString(),
    }),
    sourceId: SOURCE_ID,
    previousObservationDigest: null,
    checkpoints: Object.freeze([]),
    finalizedQueue: Object.freeze([
      Object.freeze({ headerHash: null, outRef: `${"16".repeat(32)}#0` }),
      Object.freeze({
        headerHash: HEADER_HASH,
        outRef: queueHeader.queueOutRef,
      }),
      Object.freeze({
        headerHash: GENESIS_HEADER_HASH,
        outRef: genesisQueueHeader.queueOutRef,
      }),
    ]),
    finalizedHeaders: Object.freeze([queueHeader, genesisQueueHeader]),
    finalizedCorrectionLock: null,
    correctionLockWitnesses: Object.freeze([]),
    observationDigest: OBSERVATION_DIGEST,
  });

export const observation: AuthenticatedStateQueueHeaderObservation =
  Object.freeze({
    schemaVersion: "midgard-canonical-evidence-source-v1",
    sourceMode: "local_node",
    provenance: Object.freeze({
      trustClass: "authenticated_cardano_l1",
      sourceId: SOURCE_ID,
      grade: "security",
    }),
    chainPoint: Object.freeze({
      slot: OBSERVED_SLOT,
      blockHash: OBSERVED_BLOCK_HASH,
    }),
    confirmationDepth: CONFIRMATION_DEPTH,
    headerHash: HEADER_HASH,
    header,
  });
export const genesisObservation: AuthenticatedStateQueueHeaderObservation =
  Object.freeze({
    ...observation,
    headerHash: GENESIS_HEADER_HASH,
    header: genesisHeader,
  });
