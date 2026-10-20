import { createHash } from "node:crypto";

import { fraudProofSignedTransactionRecovery } from "@al-ft/midgard-fault-proofs";
import { encodeTxBody, type SimTx } from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import { createWatcherOrphanedReservations } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import type {
  WatcherProverFundingReservationRecord,
  WatcherProverFundingReservationStore,
} from "../../src/funding/prover-funding-reservation.js";
import { harness, T } from "../support/l1-follower-raw-reads-fixture.js";
import {
  FOREIGN_WITNESS,
  nodeDouble,
  RELEASE,
  signedOf,
  sourceOver,
} from "../support/l1-follower-raw-source-fixture.js";

/** An orphaned reservation whose one signed attempt spends a tracked output
 * of the follower's chain, read through the watcher's own L1 observer. The
 * reservation recorded the attempt with `witnessSet`. */
const orphanOverFollower = async (witnessSet?: Buffer) => {
  const h = await harness();
  const funding: SimTx = {
    inputs: [h.chain.outsideInput()],
    outputs: [{ address: T, lovelace: 5_000_000n }],
    nonce: h.chain.nonce(),
  };
  const { hashes } = await h.forward([funding]);
  for (let block = 0; block < 4; block += 1) await h.forward([]);
  const attempt: SimTx = {
    inputs: [{ txHash: Buffer.from(hashes[0]!, "hex"), index: 0 }],
    outputs: [{ address: T, lovelace: 4_000_000n }],
    nonce: h.chain.nonce(),
  };
  const signed = signedOf(attempt, witnessSet);
  const transition = {
    actionKind: "proof.init",
    ...signed,
    transactionBodySha256: createHash("sha256")
      .update(encodeTxBody(attempt))
      .digest("hex"),
    consumedOutRefs: [`${hashes[0]!}#0`],
    producedInputs: [],
  };
  const record = {
    reservationId: "a1".repeat(32),
    decisionDigest: "d1".repeat(32),
  } as unknown as WatcherProverFundingReservationRecord;
  const orphans = createWatcherOrphanedReservations({
    store: {
      readPendingTransition: async () => transition,
      readCompletionHandoff: async () => null,
      settleOrphanedReservation: async () => true,
    } as unknown as WatcherProverFundingReservationStore,
    observe: fraudProofSignedTransactionRecovery(
      sourceOver(h, h.reads(), nodeDouble().node),
      RELEASE,
    ).observeSignedTransaction,
  });
  return {
    land: async () => await h.forward([attempt]),
    resolve: async () =>
      await orphans.resolveTarget({
        target: "doubleSpend/target",
        orphans: [record],
        handOver: true,
      }),
    record,
  };
};

describe("an orphaned attempt that landed under other witness bytes", () => {
  it("is the same inclusion: a fresh execution takes it over", async () => {
    const orphan = await orphanOverFollower(FOREIGN_WITNESS);
    await orphan.land();
    expect(await orphan.resolve()).toEqual({
      handOver: [orphan.record],
      settled: [],
    });
  });
});
