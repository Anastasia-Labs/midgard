import type * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  eventKeyFingerprint,
  type SourceEventRecord,
} from "../src/transition-trace/reconstruct.js";
import {
  BLOCK_SUBJECT,
  depositSubject,
  detectionEventOrder,
  forcedTransactionSubject,
  introducedOutputSubject,
  sourceEventSubject,
} from "../src/workflow/detection-subject.js";

/**
 * The producer of an introduced output is the accepted event that applies it
 * earliest on the trace order. Committed list order is key order, which for
 * forced sources is the `tx_order_id`, so two L1 orders of one forced
 * transaction list in an order unrelated to the order they applied in.
 */

const T = "ab".repeat(32);
const OUTPUT = { transactionId: T, outputIndex: 0n };

const orderKey = (byte: string): SDK.OutputReference => ({
  transactionId: byte.repeat(32),
  outputIndex: 0n,
});

const REJECTED = {
  ForcedTxInvalid: { reason: "InputNotFound" },
} as unknown as SDK.ForcedInclusionTxV1["verdict"];

const record = (
  phase: SourceEventRecord["phase"],
  eventKey: SDK.EventKey,
  entry: unknown,
): SourceEventRecord =>
  ({
    phase,
    eventKey,
    fingerprint: eventKeyFingerprint(eventKey),
    entry,
  }) as SourceEventRecord;

const forced = (
  key: SDK.OutputReference,
  verdict: SDK.ForcedInclusionTxV1["verdict"],
) =>
  record(
    "ForcedTransaction",
    { ForcedTransactionEventKey: { tx_order_id: key } },
    { key, value: { tx_id: T, verdict } },
  );

const l2 = (txId: string, validity: SDK.MidgardTxValidity) =>
  record(
    "L2Transaction",
    { L2TransactionEventKey: { tx_id: txId } },
    { txId, validity },
  );

const deposit = (key: SDK.OutputReference) =>
  record("Deposit", { DepositEventKey: { deposit_id: key } }, { key });

const withdrawal = (key: SDK.OutputReference) =>
  record("Withdrawal", { WithdrawalEventKey: { withdrawal_id: key } }, { key });

/**
 * A reconstruction whose committed source list is `listed` and whose trace
 * steps `applied`, in that order.
 */
const reconstruction = (
  listed: readonly SourceEventRecord[],
  applied: readonly SourceEventRecord[],
) =>
  ({
    sourceEvents: listed,
    sourceEventsByFingerprint: new Map(
      listed.map((source) => [source.fingerprint, source] as const),
    ),
    transitionTrace: applied.map(({ eventKey, phase }, index) => ({
      key: BigInt(index),
      value: { step_index: BigInt(index), event_key: eventKey, phase },
    })),
  }) as unknown as Parameters<typeof introducedOutputSubject>[0];

const orderOf = (
  block: Parameters<typeof introducedOutputSubject>[0],
  subject: ReturnType<typeof introducedOutputSubject>,
) => detectionEventOrder(block, { ...subject, detectionId: "introduced" });

describe("introducedOutputSubject", () => {
  it("names the accepted forced copy applied first, not the rejected copy listed first", () => {
    const listedFirst = forced(orderKey("11"), REJECTED);
    const producer = forced(orderKey("22"), "ForcedTxValid");
    const block = reconstruction(
      [listedFirst, producer],
      [producer, listedFirst],
    );
    const subject = introducedOutputSubject(block, OUTPUT);
    expect(subject).toEqual(forcedTransactionSubject(orderKey("22")));
    expect(orderOf(block, subject)).toBe(0);
  });

  it("names the accepted forced copy applied second, not the rejected copy applied first", () => {
    const producer = forced(orderKey("11"), "ForcedTxValid");
    const appliedFirst = forced(orderKey("22"), REJECTED);
    const block = reconstruction(
      [producer, appliedFirst],
      [appliedFirst, producer],
    );
    const subject = introducedOutputSubject(block, OUTPUT);
    expect(subject).toEqual(forcedTransactionSubject(orderKey("11")));
    expect(orderOf(block, subject)).toBe(1);
  });

  it("names the accepted L2 transaction, not a rejected forced copy of it", () => {
    const rejected = forced(orderKey("11"), REJECTED);
    const producer = l2(T, "TxIsValid");
    const block = reconstruction([rejected, producer], [rejected, producer]);
    const subject = introducedOutputSubject(block, OUTPUT);
    expect(subject).toEqual(sourceEventSubject(producer));
    expect(orderOf(block, subject)).toBe(1);
  });

  it("names the earliest applied of two accepted copies", () => {
    const listedFirst = forced(orderKey("11"), "ForcedTxValid");
    const appliedFirst = forced(orderKey("22"), "ForcedTxValid");
    const block = reconstruction(
      [listedFirst, appliedFirst],
      [appliedFirst, listedFirst],
    );
    expect(introducedOutputSubject(block, OUTPUT)).toEqual(
      forcedTransactionSubject(orderKey("22")),
    );
  });

  it("names the deposit whose event id is the output reference", () => {
    const other = deposit({ transactionId: T, outputIndex: 1n });
    const producer = deposit(OUTPUT);
    const block = reconstruction([producer, other], [other, producer]);
    expect(introducedOutputSubject(block, OUTPUT)).toEqual(
      depositSubject(OUTPUT),
    );
  });

  it("is block-level when only non-accepted copies name the transaction", () => {
    const rejectedForced = forced(orderKey("11"), REJECTED);
    const invalidL2 = l2(T, "TxIsInvalid");
    const block = reconstruction(
      [withdrawal(OUTPUT), rejectedForced, invalidL2],
      [rejectedForced, invalidL2],
    );
    const subject = introducedOutputSubject(block, OUTPUT);
    expect(subject).toEqual(BLOCK_SUBJECT);
    expect(orderOf(block, subject)).toBe(-1);
  });
});
