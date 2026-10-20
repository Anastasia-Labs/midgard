import { createHash } from "node:crypto";

import { encodeTxBody, type SimTx } from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import { createWatcherOrphanedReservations } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import type {
  WatcherProverFundingReservationRecord,
  WatcherProverFundingReservationStore,
} from "../../src/funding/prover-funding-reservation.js";
import { signedReadObservation } from "../support/fault-proof-funding-fixture.signed-read.js";
import { T } from "../support/l1-follower-raw-reads-fixture.js";
import { signedOf } from "../support/l1-follower-raw-source-fixture.js";

const FUNDING = { txHash: Buffer.alloc(32, 0x11), index: 0 };
const FUNDING_OUT_REF = `${FUNDING.txHash.toString("hex")}#0`;
const signedTx = (lovelace: bigint, nonce: number): SimTx => ({
  inputs: [FUNDING],
  outputs: [{ address: T, lovelace }],
  nonce,
});
const ATTEMPT = signedTx(4_000_000n, 1);
const CANCEL = signedTx(4_800_000n, 2);

/**
 * A completed orphan whose one attempt without expiry left the mempool with
 * its funding unspent, and a cancel that is never signed: its fees cannot be
 * read. `cancel` is its recorded cancel, which the mempool holds.
 */
const completedOrphan = () => {
  const record = {
    reservationId: "a1".repeat(32),
    decisionDigest: "d1".repeat(32),
    activeInputs: [{ outRef: FUNDING_OUT_REF, role: "funding" }],
  } as unknown as WatcherProverFundingReservationRecord;
  const held: { cancel?: SimTx } = {};
  const submitted: string[] = [];
  let clock = 1_000_000_000;
  const orphans = createWatcherOrphanedReservations({
    store: {
      readPendingTransition: async () => ({
        actionKind: "proof.init",
        ...signedOf(ATTEMPT),
        transactionBodySha256: createHash("sha256")
          .update(encodeTxBody(ATTEMPT))
          .digest("hex"),
        consumedOutRefs: [FUNDING_OUT_REF],
        producedInputs: [],
      }),
      readOrphanCancel: async () =>
        held.cancel === undefined
          ? null
          : { ...signedOf(held.cancel), inputs: [FUNDING_OUT_REF] },
      recordOrphanCancel: async () => undefined,
      dropRetiredOrphanCancel: async () => undefined,
      readCompletionHandoff: async () => ({
        completion: { kind: "completed" },
      }),
      settleOrphanedReservation: async () => false,
    } as unknown as WatcherProverFundingReservationStore,
    observe: async (input) =>
      held.cancel !== undefined &&
      input.transactionHash === signedOf(held.cancel).transactionHash
        ? signedReadObservation(input, {
            kind: "pending",
            txHash: input.transactionHash,
          })
        : {
            ...signedReadObservation(input, { kind: "unknown", reason: "" }),
            status: "rebroadcast",
            reason: "absent, every input unspent",
          },
    now: () => clock,
    cancel: {
      wallet: {
        address: "addr_test1",
        open: async () => {
          throw new Error("the wallet is never opened");
        },
        submit: async (signedTransactionCborHex) => {
          submitted.push(signedTransactionCborHex);
        },
      },
      targetGone: () => true,
      fees: async () => {
        throw new Error("no fees");
      },
    },
  });
  return {
    held,
    submitted,
    /** One sweep over `listed`, after every read's backoff has passed. */
    sweep: async (listed: readonly WatcherProverFundingReservationRecord[]) => {
      clock += 600_000;
      return (await orphans.sweep(listed)).map(
        (hold) => (hold as { detail: string }).detail,
      );
    },
    record,
  };
};

const signing = expect.stringContaining("its funding cancel failed: no fees");

describe("an orphan that leaves the sweep", () => {
  it("starts its count of agreeing reads again when it returns", async () => {
    const o = completedOrphan();
    for (let read = 0; read < 2; read += 1)
      expect(await o.sweep([o.record])).not.toContainEqual(signing);
    expect(await o.sweep([])).toEqual([]);
    for (let read = 0; read < 2; read += 1)
      expect(await o.sweep([o.record])).not.toContainEqual(signing);
    expect(await o.sweep([o.record])).toContainEqual(signing);
  });

  it("is no longer known to have had its cancel submitted when it returns", async () => {
    const o = completedOrphan();
    o.held.cancel = CANCEL;
    const cancel = signedOf(CANCEL).signedTransactionCborHex;
    await o.sweep([o.record]);
    await o.sweep([o.record]);
    expect(o.submitted).toEqual([cancel]);
    await o.sweep([]);
    await o.sweep([o.record]);
    expect(o.submitted).toEqual([cancel, cancel]);
  });
});
