import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import {
  buildEventToStepMembersFromTrace,
  buildTransitionTraceResult,
  type TransitionTraceSourceEvent,
  type UtxoPayloadEntry,
} from "@/workers/utils/mpf.js";

import {
  depositEventKey,
  forcedTransactionEventKey,
  l2TransactionEventKey,
  withdrawalEventKey,
} from "./helpers/transition-fixtures.js";

const outRef = (byte: number) => Buffer.from([byte]);
const output = (byte: number) => Buffer.from([byte, byte]);

const initialUtxo = (byte: number): UtxoPayloadEntry => ({
  outref: outRef(byte),
  output: output(byte),
});

const noOpEvent = (
  phase: SDK.TransitionPhase,
  eventKey: SDK.EventKey,
): TransitionTraceSourceEvent => ({
  phase,
  eventKey,
  ledgerOps: [],
});

describe("transition trace builder", () => {
  it.effect(
    "builds exact phase order and one event-to-step member per source",
    () =>
      Effect.gen(function* () {
        const events: TransitionTraceSourceEvent[] = [
          noOpEvent("Withdrawal", withdrawalEventKey(1)),
          noOpEvent("ForcedTransaction", forcedTransactionEventKey(2)),
          {
            phase: "L2Transaction",
            eventKey: l2TransactionEventKey(3),
            ledgerOps: [
              { type: "delete", key: outRef(10) },
              { type: "insert", key: outRef(11), value: output(11) },
            ],
          },
          {
            phase: "Deposit",
            eventKey: depositEventKey(4),
            ledgerOps: [{ type: "insert", key: outRef(12), value: output(12) }],
          },
        ];

        const result = yield* buildTransitionTraceResult({
          initialUtxos: [initialUtxo(10)],
          sourceEvents: events,
          withdrawalCount: 1,
          forcedTransactionCount: 1,
          l2TransactionCount: 1,
          depositCount: 1,
        });

        expect(
          result.transitionTraceMembers.map((member) => member.value.phase),
        ).toEqual([
          "Withdrawal",
          "ForcedTransaction",
          "L2Transaction",
          "Deposit",
        ]);
        expect(
          result.transitionTraceMembers.map(
            (member) => member.value.step_index,
          ),
        ).toEqual([0n, 1n, 2n, 3n]);
        expect(
          result.eventToStepMembers.map((member) => member.value.step_index),
        ).toEqual([0n, 1n, 2n, 3n]);
        expect(result.transitionTraceRoot).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
        expect(result.eventToStepRoot).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
        expect(result.totalEventCount).toBe(4);
        expect(result.transitionStepCount).toBe(4);

        const [withdrawalStep, forcedStep, l2Step, depositStep] =
          result.transitionTraceMembers.map((member) => member.value);
        expect(withdrawalStep!.pre_utxos_root).toBe(
          withdrawalStep!.post_utxos_root,
        );
        expect(forcedStep!.pre_utxos_root).toBe(forcedStep!.post_utxos_root);
        expect(l2Step!.pre_utxos_root).not.toBe(l2Step!.post_utxos_root);
        expect(depositStep!.pre_utxos_root).not.toBe(
          depositStep!.post_utxos_root,
        );
        expect(result.finalUtxosRoot).toBe(depositStep!.post_utxos_root);
      }),
  );

  it.effect("rejects a same-block deposit spend before deposits execute", () =>
    Effect.gen(function* () {
      const result = yield* buildTransitionTraceResult({
        initialUtxos: [],
        sourceEvents: [
          {
            phase: "L2Transaction",
            eventKey: l2TransactionEventKey(1),
            ledgerOps: [{ type: "delete", key: outRef(20) }],
          },
          {
            phase: "Deposit",
            eventKey: depositEventKey(2),
            ledgerOps: [{ type: "insert", key: outRef(20), value: output(20) }],
          },
        ],
        withdrawalCount: 0,
        forcedTransactionCount: 0,
        l2TransactionCount: 1,
        depositCount: 1,
      }).pipe(Effect.either);

      expect(result._tag).toBe("Left");
    }),
  );

  it.effect("changes the trace root when an intermediate root changes", () =>
    Effect.gen(function* () {
      const build = (value: Buffer) =>
        buildTransitionTraceResult({
          initialUtxos: [],
          sourceEvents: [
            {
              phase: "L2Transaction",
              eventKey: l2TransactionEventKey(1),
              ledgerOps: [{ type: "insert", key: outRef(30), value }],
            },
            {
              phase: "L2Transaction",
              eventKey: l2TransactionEventKey(2),
              ledgerOps: [{ type: "delete", key: outRef(30) }],
            },
          ],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 2,
          depositCount: 0,
        });
      const left = yield* build(output(30));
      const right = yield* build(output(31));

      expect(left.finalUtxosRoot).toBe(right.finalUtxosRoot);
      expect(left.transitionTraceRoot).not.toBe(right.transitionTraceRoot);
    }),
  );

  it.effect("changes the event-to-step root when step indexes change", () =>
    Effect.gen(function* () {
      const build = (sourceEvents: readonly TransitionTraceSourceEvent[]) =>
        buildTransitionTraceResult({
          initialUtxos: [],
          sourceEvents,
          withdrawalCount: sourceEvents.filter(
            (event) => event.phase === "Withdrawal",
          ).length,
          forcedTransactionCount: 0,
          l2TransactionCount: 0,
          depositCount: sourceEvents.filter(
            (event) => event.phase === "Deposit",
          ).length,
        });
      const first = noOpEvent("Withdrawal", withdrawalEventKey(1));
      const second = noOpEvent("Withdrawal", withdrawalEventKey(2));
      const left = yield* build([first, second]);
      const right = yield* build([second, first]);

      expect(left.eventToStepRoot).not.toBe(right.eventToStepRoot);
    }),
  );

  it.effect("rejects duplicate source keys before root construction", () =>
    Effect.gen(function* () {
      const duplicate = withdrawalEventKey(1);
      const result = yield* buildTransitionTraceResult({
        initialUtxos: [],
        sourceEvents: [
          noOpEvent("Withdrawal", duplicate),
          noOpEvent("Withdrawal", duplicate),
        ],
        withdrawalCount: 2,
        forcedTransactionCount: 0,
        l2TransactionCount: 0,
        depositCount: 0,
      }).pipe(Effect.either);

      expect(result._tag).toBe("Left");
    }),
  );

  it.effect("rejects duplicate forced transaction order event keys", () =>
    Effect.gen(function* () {
      const duplicate = forcedTransactionEventKey(1);
      const result = yield* buildTransitionTraceResult({
        initialUtxos: [],
        sourceEvents: [
          noOpEvent("ForcedTransaction", duplicate),
          noOpEvent("ForcedTransaction", duplicate),
        ],
        withdrawalCount: 0,
        forcedTransactionCount: 2,
        l2TransactionCount: 0,
        depositCount: 0,
      }).pipe(Effect.either);

      expect(result._tag).toBe("Left");
    }),
  );

  it.effect("rejects non-canonical phase ordering", () =>
    Effect.gen(function* () {
      const result = yield* buildTransitionTraceResult({
        initialUtxos: [],
        sourceEvents: [
          noOpEvent("Deposit", depositEventKey(1)),
          noOpEvent("L2Transaction", l2TransactionEventKey(2)),
        ],
        withdrawalCount: 0,
        forcedTransactionCount: 0,
        l2TransactionCount: 1,
        depositCount: 1,
      }).pipe(Effect.either);

      expect(result._tag).toBe("Left");
    }),
  );

  it.effect("rejects duplicate trace event keys", () =>
    Effect.gen(function* () {
      const sourceEvents = [
        noOpEvent("Withdrawal", withdrawalEventKey(1)),
        noOpEvent("Deposit", depositEventKey(2)),
      ];
      const result = yield* buildTransitionTraceResult({
        initialUtxos: [],
        sourceEvents,
        withdrawalCount: 1,
        forcedTransactionCount: 0,
        l2TransactionCount: 0,
        depositCount: 1,
      });
      const duplicateTraceMembers = [
        result.transitionTraceMembers[0]!,
        {
          ...result.transitionTraceMembers[1]!,
          value: {
            ...result.transitionTraceMembers[1]!.value,
            event_key: result.transitionTraceMembers[0]!.value.event_key,
            phase: result.transitionTraceMembers[0]!.value.phase,
          },
        },
      ];

      const duplicate = yield* buildEventToStepMembersFromTrace({
        sourceEvents,
        transitionTraceMembers: duplicateTraceMembers,
      }).pipe(Effect.either);

      expect(duplicate._tag).toBe("Left");
    }),
  );

  it.effect("rejects source count mismatches", () =>
    Effect.gen(function* () {
      const result = yield* buildTransitionTraceResult({
        initialUtxos: [],
        sourceEvents: [noOpEvent("Withdrawal", withdrawalEventKey(1))],
        withdrawalCount: 1,
        forcedTransactionCount: 0,
        l2TransactionCount: 0,
        depositCount: 0,
        expectedTotalEventCount: 2,
      }).pipe(Effect.either);

      expect(result._tag).toBe("Left");
    }),
  );
});
