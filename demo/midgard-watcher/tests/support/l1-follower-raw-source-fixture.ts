import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofRawL1SnapshotRequest,
  type ReleaseL1FinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs";
import {
  cbor,
  encodeTxBody,
  encodeWitnessSet,
  type SimTx,
  simTxHash,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { scriptHashToCredential } from "@lucid-evolution/lucid";
import { credentialToAddress } from "@lucid-evolution/lucid";

import {
  createWatcherFaultProofL1Source,
  WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX,
} from "../../src/l1-follower/fault-proof-l1-source.js";
import type { FollowerRawReads } from "../../src/l1-follower/raw-reads.types.js";
import { D, hex, K } from "./l1-follower-raw-reads-fixture.js";

/**
 * The release policy, snapshot request, signed bytes and node double the
 * watcher's fault-proof L1 source tests (ticket W1) use over the raw-read
 * fixture chain (`l1-follower-raw-reads-fixture.ts`, k = 3).
 */

export const SOURCE_ID = `${WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX}watcher-test`;

/** confirmationDepth 2; the recovery depth scaled to the fixture's k (k + 2 = 5). */
const POLICY = {
  confirmationDepth: 2,
  automaticRecoveryMaxDepth:
    K as ReleaseL1FinalityPolicy["automaticRecoveryMaxDepth"],
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const satisfies ReleaseL1FinalityPolicy;

export const RELEASE: VerifiedFraudProofReleaseFinalityPolicy = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: "d1".repeat(32),
  blueprintHash: "b1".repeat(32),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(POLICY),
  policy: POLICY,
};
export const RECOVERY_DEPTH = K + 2;

export const STATE_QUEUE_ADDRESS = credentialToAddress(
  D.network,
  scriptHashToCredential(D.stateQueueSpend),
);

export const nodeUnit = (header: string): string =>
  `${D.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header}`;

/** The state-queue scope and one header's node-unit history. */
export const requestFor = (
  header: string,
  overrides: Partial<FraudProofRawL1SnapshotRequest> = {},
): FraudProofRawL1SnapshotRequest => ({
  deploymentIdentityDigest: RELEASE.deploymentIdentityDigest,
  blueprintHash: RELEASE.blueprintHash,
  finalityPolicyDigest: RELEASE.policyDigest,
  headerHash: header,
  scopes: [{ role: "state_queue", address: STATE_QUEUE_ADDRESS }],
  historyUnits: [nodeUnit(header)],
  ...overrides,
});

/** A key witness the simulator never writes: other witness bytes, same body. */
export const FOREIGN_WITNESS = cbor.map([
  cbor.uint(0),
  cbor.array(
    cbor.array(
      cbor.bytes(Buffer.alloc(32, 7)),
      cbor.bytes(Buffer.alloc(64, 8)),
    ),
  ),
]);

/** The exact signed bytes of a simulator transaction (its own witness set unless given). */
export const signedOf = (
  tx: SimTx,
  witnessSet: Buffer = encodeWitnessSet(tx),
): Readonly<{ transactionHash: string; signedTransactionCborHex: string }> => ({
  transactionHash: hex(simTxHash(tx)),
  signedTransactionCborHex: hex(
    cbor.array(encodeTxBody(tx), witnessSet, cbor.bool(true), cbor.nul),
  ),
});

type SubmitResult = Awaited<ReturnType<L1NodeTransport["submit"]>>;

/** A node double recording calls; `mempool` and `submit` answers are settable. */
export const nodeDouble = () => {
  const calls: string[] = [];
  const state: {
    mempool: Set<string>;
    hasTx: (txId: string) => Promise<boolean>;
    submit: (tx: Uint8Array) => Promise<SubmitResult>;
  } = {
    mempool: new Set(),
    hasTx: async (txId) => state.mempool.has(txId),
    submit: async () => ({ accepted: true }),
  };
  const node: Pick<L1NodeTransport, "submit" | "hasTx"> = {
    submit: async (tx) => {
      calls.push(`submit:${hex(Buffer.from(tx))}`);
      return await state.submit(tx);
    },
    hasTx: async (txId) => {
      calls.push(`hasTx:${txId}`);
      return await state.hasTx(txId);
    },
  };
  return { node, calls, state };
};

export const sourceOver = (
  fx: Readonly<{
    store: Parameters<typeof createWatcherFaultProofL1Source>[0]["store"];
  }>,
  rawReads: FollowerRawReads,
  node: Pick<L1NodeTransport, "submit" | "hasTx"> = nodeDouble().node,
) =>
  createWatcherFaultProofL1Source({
    store: fx.store,
    rawReads,
    node,
    sourceId: SOURCE_ID,
  });
