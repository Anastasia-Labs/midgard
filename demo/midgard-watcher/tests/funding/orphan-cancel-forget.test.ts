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
import { retirementFixture } from "./sqlite-prover-funding-reservation-store.signed-transition.js";

const FUNDING = { txHash: Buffer.alloc(32, 0x11), index: 0 };
const FUNDING_OUT_REF = `${FUNDING.txHash.toString("hex")}#0`;
const signedTx = (lovelace: bigint, nonce: number): SimTx => ({
  inputs: [FUNDING],
  outputs: [{ address: T, lovelace }],
  nonce,
});
const ATTEMPT = signedTx(4_000_000n, 1);
const CANCEL = signedTx(4_800_000n, 2);
const LATER_ATTEMPT = signedTx(4_000_000n, 3);

const recordOf = (byte: string) =>
  ({
    reservationId: byte.repeat(32),
    decisionDigest: "d1".repeat(32),
    activeInputs: [{ outRef: FUNDING_OUT_REF, role: "funding" }],
  }) as unknown as WatcherProverFundingReservationRecord;

/**
 * Two orphans of one target whose one attempt without expiry left the
 * mempool with its funding unspent, and a cancel that is never signed: its
 * fees cannot be read. While `chain.completed` holds, their workflow
 * completed; `cancel` is the first orphan's recorded cancel, which the
 * mempool holds; with `chain.final`, every transaction read retires past
 * release finality.
 */
const completedOrphans = () => {
  const record = recordOf("a1");
  const other = recordOf("a2");
  const chain: {
    attempt: SimTx;
    cancel?: SimTx;
    final: boolean;
    completed: boolean;
  } = { attempt: ATTEMPT, final: false, completed: true };
  const settles = { value: false };
  const submitted: string[] = [];
  let clock = 1_000_000_000;
  const orphans = createWatcherOrphanedReservations({
    store: {
      readPendingTransition: async () => ({
        actionKind: "proof.init",
        ...signedOf(chain.attempt),
        transactionBodySha256: createHash("sha256")
          .update(encodeTxBody(chain.attempt))
          .digest("hex"),
        consumedOutRefs: [FUNDING_OUT_REF],
        producedInputs: [],
      }),
      readOrphanCancel: async ({ reservationId }: { reservationId: string }) =>
        chain.cancel === undefined || reservationId !== record.reservationId
          ? null
          : { ...signedOf(chain.cancel), inputs: [FUNDING_OUT_REF] },
      recordOrphanCancel: async () => undefined,
      dropRetiredOrphanCancel: async () => undefined,
      readCompletionHandoff: async () =>
        chain.completed ? { completion: { kind: "completed" } } : null,
      settleOrphanedReservation: async () => settles.value,
    } as unknown as WatcherProverFundingReservationStore,
    observe: async (input) =>
      chain.final
        ? signedReadObservation(input, {
            kind: "not_found",
            retirement: retirementFixture(input.transactionHash),
          })
        : chain.cancel !== undefined &&
            input.transactionHash === signedOf(chain.cancel).transactionHash
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
  const later = () => (clock += 600_000);
  return {
    chain,
    settles,
    submitted,
    record,
    other,
    /** One sweep over `listed`, after every read's backoff has passed. */
    sweep: async (listed: readonly WatcherProverFundingReservationRecord[]) => {
      later();
      return (await orphans.sweep(listed)).map(
        (hold) => (hold as { detail: string }).detail,
      );
    },
    /** A fresh execution of the target, before it reserves. */
    resolve: async () => {
      later();
      return await orphans.resolveTarget({
        target: "doubleSpend/target",
        orphans: [record],
        handOver: true,
      });
    },
  };
};

const signing = expect.stringContaining("its funding cancel failed: no fees");
const CANCEL_BYTES = signedOf(CANCEL).signedTransactionCborHex;

describe("an orphan's cancel progress", () => {
  it("is kept across a per-run release of another orphan", async () => {
    const o = completedOrphans();
    for (let read = 0; read < 2; read += 1)
      expect(await o.sweep([o.record])).not.toContainEqual(signing);
    await o.sweep([o.other]);
    expect(await o.sweep([o.record])).toContainEqual(signing);

    const sent = completedOrphans();
    sent.chain.cancel = CANCEL;
    await sent.sweep([sent.record]);
    expect(sent.submitted).toEqual([CANCEL_BYTES]);
    await sent.sweep([sent.other]);
    await sent.sweep([sent.record]);
    expect(sent.submitted).toEqual([CANCEL_BYTES]);
  });

  it("is forgotten once the orphan settles", async () => {
    const o = completedOrphans();
    for (let read = 0; read < 2; read += 1)
      expect(await o.sweep([o.record])).not.toContainEqual(signing);
    o.chain.final = true;
    o.settles.value = true;
    expect(await o.sweep([o.record])).toEqual([]);
    // Were it listed again with a fresh attempt, its count starts over.
    o.chain.final = false;
    o.settles.value = false;
    o.chain.attempt = LATER_ATTEMPT;
    for (let read = 0; read < 2; read += 1)
      expect(await o.sweep([o.record])).not.toContainEqual(signing);
    expect(await o.sweep([o.record])).toContainEqual(signing);
  });

  it("is forgotten once the orphan settles before a fresh execution", async () => {
    const o = completedOrphans();
    for (let read = 0; read < 2; read += 1)
      expect(await o.sweep([o.record])).not.toContainEqual(signing);
    o.chain.final = true;
    o.settles.value = true;
    expect(await o.resolve()).toEqual({ handOver: [], settled: [o.record] });
    o.chain.final = false;
    o.settles.value = false;
    o.chain.attempt = LATER_ATTEMPT;
    for (let read = 0; read < 2; read += 1)
      expect(await o.sweep([o.record])).not.toContainEqual(signing);
    expect(await o.sweep([o.record])).toContainEqual(signing);
  });

  it("is forgotten once the orphan is handed over to a fresh execution", async () => {
    const o = completedOrphans();
    for (let read = 0; read < 2; read += 1)
      expect(await o.sweep([o.record])).not.toContainEqual(signing);
    o.chain.completed = false;
    expect(await o.resolve()).toEqual({ handOver: [o.record], settled: [] });
    // Were it listed again, its count of agreeing reads starts over.
    o.chain.completed = true;
    for (let read = 0; read < 2; read += 1)
      expect(await o.sweep([o.record])).not.toContainEqual(signing);
    expect(await o.sweep([o.record])).toContainEqual(signing);

    const sent = completedOrphans();
    sent.chain.cancel = CANCEL;
    await sent.sweep([sent.record]);
    expect(sent.submitted).toEqual([CANCEL_BYTES]);
    sent.chain.cancel = undefined;
    sent.chain.completed = false;
    expect(await sent.resolve()).toEqual({
      handOver: [sent.record],
      settled: [],
    });
    // Were its cancel recorded again, it is no longer known as submitted.
    sent.chain.cancel = CANCEL;
    await sent.sweep([sent.record]);
    expect(sent.submitted).toEqual([CANCEL_BYTES, CANCEL_BYTES]);
  });
});
