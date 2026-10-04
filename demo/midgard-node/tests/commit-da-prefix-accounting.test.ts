import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ledgerPayloadAggregatesByOrdinaryPrefix } from "../src/mpf/payload-size.prefix-aggregates.js";
import type { TransitionTraceSourceEvent } from "../src/mpf/trace-events.js";
import type { MpfBatchOp } from "../src/mpf/types.js";

const key = (index: number) => Buffer.from([index]);
const insert = (index: number): MpfBatchOp => ({
  type: "insert",
  key: key(index),
  value: Buffer.alloc(3),
});
const remove = (index: number): MpfBatchOp => ({
  type: "delete",
  key: key(index),
});
const event = (
  phase: SDK.TransitionPhase,
  ledgerOps: readonly MpfBatchOp[],
): TransitionTraceSourceEvent => ({
  phase,
  ledgerOps,
  eventKey:
    phase === "Withdrawal"
      ? {
          WithdrawalEventKey: {
            withdrawal_id: { transactionId: "aa".repeat(32), outputIndex: 0n },
          },
        }
      : phase === "ForcedTransaction"
        ? {
            ForcedTransactionEventKey: {
              tx_order_id: { transactionId: "aa".repeat(32), outputIndex: 0n },
            },
          }
        : phase === "Deposit"
          ? {
              DepositEventKey: {
                deposit_id: { transactionId: "aa".repeat(32), outputIndex: 0n },
              },
            }
          : { L2TransactionEventKey: { tx_id: "aa".repeat(32) } },
});
const aggregate = (values: ReadonlyMap<string, Buffer>) => ({
  entryCount: values.size,
  encodedTupleBytes: [...values].reduce(
    (sum, [outref, output]) =>
      sum + SDK.daPayloadEntryEncodedSize([outref, output.toString("hex")]),
    0,
  ),
});

describe("complete ordinary-prefix DA accounting", () => {
  it("matches independent prefix replay with dependent spends and final mandatory deposit overwrites", async () => {
    const initialValues = new Map([
      ["01", Buffer.alloc(14_000)],
      ["02", Buffer.alloc(100)],
      ["03", Buffer.alloc(300)],
    ]);
    const insertedValues = new Map([
      ["04", Buffer.alloc(200)],
      ["05", Buffer.alloc(40)],
      ["06", Buffer.alloc(160)],
      ["07", Buffer.alloc(80)],
    ]);
    const depositValues = new Map([
      ["03", Buffer.alloc(90)],
      ["08", Buffer.alloc(256)],
    ]);
    const before = [
      event("Withdrawal", [remove(2)]),
      event("ForcedTransaction", [insert(4)]),
    ];
    const normal = [
      event("L2Transaction", [remove(1), insert(5)]),
      event("L2Transaction", [remove(5), insert(6)]),
      event("L2Transaction", [remove(3), insert(7)]),
    ];
    const deposits = [
      event("Deposit", [insert(3)]),
      event("Deposit", [insert(8)]),
    ];
    const actual = await Effect.runPromise(
      ledgerPayloadAggregatesByOrdinaryPrefix({
        base: aggregate(initialValues),
        sourceEvents: [...before, ...normal, ...deposits],
        initialValues,
        insertedValues,
        depositValues,
      }),
    );
    expect(actual).toHaveLength(4);
    for (let prefix = 0; prefix <= normal.length; prefix += 1) {
      const state = new Map(initialValues);
      for (const source of [...before, ...normal.slice(0, prefix), ...deposits])
        for (const op of source.ledgerOps) {
          const outref = op.key.toString("hex");
          if (op.type === "delete") state.delete(outref);
          else
            state.set(
              outref,
              (source.phase === "Deposit" ? depositValues : insertedValues).get(
                outref,
              )!,
            );
        }
      expect(actual[prefix]).toEqual(aggregate(state));
    }
    // Large consumed bytes must disappear from ledger accounting, even though
    // production validation-witness carriage can outweigh that reduction.
    expect(actual[1]!.encodedTupleBytes).toBeLessThan(
      actual[0]!.encodedTupleBytes,
    );
  });

  it("visits each mandatory deposit once across every ordinary prefix", async () => {
    const depositValues = new Map([
      ["03", Buffer.alloc(90)],
      ["08", Buffer.alloc(256)],
    ]);
    let reads = 0;
    const originalGet = depositValues.get.bind(depositValues);
    depositValues.get = (outref: string) => {
      reads += 1;
      return originalGet(outref);
    };
    const initialValues = new Map([["01", Buffer.alloc(32)]]);
    const insertedValues = new Map<string, Buffer>();
    const normal = Array.from({ length: 32 }, (_, index) => {
      insertedValues.set(key(index + 10).toString("hex"), Buffer.alloc(32));
      return event("L2Transaction", [insert(index + 10)]);
    });
    const result = await Effect.runPromise(
      ledgerPayloadAggregatesByOrdinaryPrefix({
        base: aggregate(initialValues),
        initialValues,
        insertedValues,
        depositValues,
        sourceEvents: [...normal, event("Deposit", [insert(3), insert(8)])],
      }),
    );
    expect(result).toHaveLength(33);
    expect(reads).toBe(2);
  });

  it("refuses phase inversion and a deletion in the deposit overlay", async () => {
    const base = { entryCount: 0, encodedTupleBytes: 0 };
    for (const sourceEvents of [
      [event("Deposit", []), event("L2Transaction", [])],
      [event("Deposit", [remove(1)])],
    ]) {
      const result = await Effect.runPromise(
        Effect.either(
          ledgerPayloadAggregatesByOrdinaryPrefix({
            base,
            sourceEvents,
            initialValues: new Map(),
            insertedValues: new Map(),
            depositValues: new Map(),
          }),
        ),
      );
      expect(result._tag).toBe("Left");
    }
  });
});
