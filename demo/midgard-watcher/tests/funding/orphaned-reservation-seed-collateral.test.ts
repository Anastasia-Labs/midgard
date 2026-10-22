import { createHash } from "node:crypto";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  fraudProofSignedTransactionRecovery,
} from "@al-ft/midgard-fault-proofs";
import { encodeTxBody, type SimTx } from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import { createWatcherOrphanedReservations } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import type {
  WatcherProverFundingReservationRecord,
  WatcherProverFundingReservationStore,
} from "../../src/funding/prover-funding-reservation.js";
import { harness, T } from "../support/l1-follower-raw-reads-fixture.js";
import {
  nodeDouble,
  signedOf,
  sourceOver,
} from "../support/l1-follower-raw-source-fixture.js";

/** The deployment's own release policy: an attempt retires only past its
 * automatic recovery depth. */
const POLICY = {
  confirmationDepth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
  automaticRecoveryMaxDepth:
    DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
const RELEASE = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: "d1".repeat(32),
  blueprintHash: "b1".repeat(32),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(POLICY),
  policy: POLICY,
};
/** Blocks on top of a landing once it can no longer roll back. */
const RETIRED = DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth + 1;

/** A prepared transition of `tx`, consuming `consumed`. */
const transitionOf = (tx: SimTx, consumed: readonly string[]) => ({
  actionKind: "proof.init",
  ...signedOf(tx),
  transactionBodySha256: createHash("sha256")
    .update(encodeTxBody(tx))
    .digest("hex"),
  consumedOutRefs: [...consumed],
  producedInputs: [],
});

/**
 * An orphaned reservation, read through the watcher's own L1 observer, whose
 * attempts spend one tracked funding output and name a collateral coin from
 * before the follower's origin, which the follower has no history of and
 * nothing ever spends. `pending` is its last attempt; `recorded` are the
 * ones before it.
 */
const orphanWithSeedCollateral = async () => {
  const h = await harness();
  const funding: SimTx = {
    inputs: [h.chain.outsideInput()],
    outputs: [{ address: T, lovelace: 5_000_000n }],
    nonce: h.chain.nonce(),
  };
  const { hashes } = await h.forward([funding]);
  for (let block = 0; block < 4; block += 1) await h.forward([]);
  const fundingOutRef = { txHash: Buffer.from(hashes[0]!, "hex"), index: 0 };
  const consumed = [`${hashes[0]!}#0`];
  const collateral = h.chain.outsideInput();
  const attemptOf = (lovelace: bigint): SimTx => ({
    inputs: [fundingOutRef],
    collaterals: [collateral],
    outputs: [{ address: T, lovelace }],
    nonce: h.chain.nonce(),
  });
  const record = {
    reservationId: "a1".repeat(32),
    decisionDigest: "d1".repeat(32),
  } as unknown as WatcherProverFundingReservationRecord;
  const held: {
    pending?: SimTx;
    recorded: SimTx[];
  } = { recorded: [] };
  const settlements: { transactionHash: string; reason: string }[][] = [];
  let clock = 1_000_000_000;
  const orphans = createWatcherOrphanedReservations({
    store: {
      readPendingTransition: async () =>
        held.pending === undefined
          ? null
          : transitionOf(held.pending, consumed),
      readRecordedSubmissions: async () =>
        held.recorded.map((tx) => ({ transition: transitionOf(tx, consumed) })),
      readCompletionHandoff: async () => null,
      settleOrphanedReservation: async ({
        retirements,
      }: {
        retirements: readonly {
          transactionHash: string;
          retirement: { reason: string };
        }[];
      }) => {
        settlements.push(
          retirements.map(({ transactionHash, retirement }) => ({
            transactionHash,
            reason: retirement.reason,
          })),
        );
        return true;
      },
    } as unknown as WatcherProverFundingReservationStore,
    observe: fraudProofSignedTransactionRecovery(
      sourceOver(h, h.reads(), nodeDouble().node),
      RELEASE,
    ).observeSignedTransaction,
    now: () => clock,
  });
  return {
    h,
    held,
    attemptOf,
    record,
    settlements,
    /** `blocks` empty blocks on top of the tip. */
    deepen: async (blocks: number) => {
      for (let block = 0; block < blocks; block += 1) await h.forward([]);
    },
    /** One sweep, after every read's backoff has passed. */
    sweep: async () => {
      clock += 600_000;
      return await orphans.sweep([record]);
    },
  };
};

const hashOf = (tx: SimTx) => signedOf(tx).transactionHash;

describe("an orphan whose attempts name a collateral coin from before the follower's origin", () => {
  it("settles once a replacement sharing that collateral landed past release finality", async () => {
    const o = await orphanWithSeedCollateral();
    const superseded = o.attemptOf(4_000_000n);
    const replacement = o.attemptOf(3_900_000n);
    o.held.recorded = [superseded];
    o.held.pending = replacement;
    await o.h.forward([replacement]);
    await o.deepen(RETIRED);
    expect(await o.sweep()).toEqual([]);
    expect(o.settlements).toEqual([
      expect.arrayContaining([
        { transactionHash: hashOf(superseded), reason: "invalidated" },
        { transactionHash: hashOf(replacement), reason: "included" },
      ]),
    ]);
    expect(o.settlements[0]).toHaveLength(2);
  });
});
