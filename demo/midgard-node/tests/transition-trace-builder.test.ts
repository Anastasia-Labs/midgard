import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, beforeAll, describe, expect } from "vitest";

import {
  buildEventToStepMembersFromTrace,
  buildTransitionTraceResult as buildTransitionTraceResultFromMpf,
  deleteMpfStore,
  encodeTransitionEventKeyCbor,
  indexTransitionTraceMembersByEventKey,
  keyValuePhasRoot,
  keyValuePhasRootWithCount,
  MidgardMpf,
  type MpfBatchOp,
  type RetainedTransitionTraceMember,
  type TransitionTraceSourceEvent,
  type UtxoPayloadEntry,
} from "../src/workers/utils/mpf.js";
import {
  depositEventKey,
  forcedTransactionEventKey,
  l2TransactionEventKey,
  withdrawalEventKey,
} from "./helpers/transition-fixtures.js";

const TRACE_PERSIST_DB = `test-transition-trace-builder-${process.pid}`;
const TRACE_LEGACY_DB = `${TRACE_PERSIST_DB}-legacy`;
const TRACE_OVERLAY_DB = `${TRACE_PERSIST_DB}-overlay`;

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

const makeLedgerMpf = (initialUtxos: readonly UtxoPayloadEntry[]) =>
  Effect.gen(function* () {
    const mpf = yield* MidgardMpf.createScratch("transition-trace");
    yield* mpf.applyBatch(
      initialUtxos.map((entry) => ({
        type: "insert" as const,
        key: entry.outref,
        value: entry.output,
      })),
    );
    return mpf;
  });

const buildTransitionTraceResult = ({
  initialUtxos,
  sourceEvents,
  withdrawalCount,
  forcedTransactionCount,
  l2TransactionCount,
  depositCount,
  expectedTotalEventCount,
}: {
  readonly initialUtxos: readonly UtxoPayloadEntry[];
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly withdrawalCount: number;
  readonly forcedTransactionCount: number;
  readonly l2TransactionCount: number;
  readonly depositCount: number;
  readonly expectedTotalEventCount?: number;
}) =>
  Effect.gen(function* () {
    const ledgerMpf = yield* makeLedgerMpf(initialUtxos);
    return yield* buildTransitionTraceResultFromMpf({
      ledgerMpf,
      sourceEvents,
      withdrawalCount,
      forcedTransactionCount,
      l2TransactionCount,
      depositCount,
      expectedTotalEventCount,
    });
  });

const encodePlutusData = <A>(
  value: A,
  schema: Parameters<typeof LucidData.Nullable>[0],
): Buffer => Buffer.from(LucidData.to(value as never, schema as never), "hex");

const countedRootFromEncodedEntries = (
  domain: SDK.RootDomain,
  entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
) =>
  Effect.gen(function* () {
    const phas = yield* keyValuePhasRootWithCount(
      entries.map((entry) => entry.key),
      entries.map((entry) => entry.value),
    );
    return yield* SDK.commitCountedRootProgram({
      domain,
      phasRoot: phas.root,
      count: phas.count,
    });
  });

const utxoRootFromMap = (entries: ReadonlyMap<string, UtxoPayloadEntry>) =>
  keyValuePhasRoot(
    [...entries.values()].map((entry) => entry.outref),
    [...entries.values()].map((entry) => entry.output),
  );

const applySnapshotLedgerOps = (
  workingUtxos: Map<string, UtxoPayloadEntry>,
  ops: readonly MpfBatchOp[],
): void => {
  for (const op of ops) {
    const keyHex = op.key.toString("hex");
    if (op.type === "delete") {
      if (!workingUtxos.has(keyHex)) {
        throw new Error(`missing delete: ${keyHex}`);
      }
      workingUtxos.delete(keyHex);
      continue;
    }
    if (workingUtxos.has(keyHex)) {
      throw new Error(`duplicate insert: ${keyHex}`);
    }
    workingUtxos.set(keyHex, {
      outref: Buffer.from(op.key),
      output: Buffer.from(op.value),
    });
  }
};

const snapshotTraceOracle = ({
  initialUtxos,
  sourceEvents,
}: {
  readonly initialUtxos: readonly UtxoPayloadEntry[];
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
}) =>
  Effect.gen(function* () {
    const workingUtxos = new Map<string, UtxoPayloadEntry>(
      initialUtxos.map((entry) => [
        entry.outref.toString("hex"),
        {
          outref: Buffer.from(entry.outref),
          output: Buffer.from(entry.output),
        },
      ]),
    );
    const transitionTraceMembers: RetainedTransitionTraceMember[] = [];
    for (const [index, sourceEvent] of sourceEvents.entries()) {
      const preUtxosRoot = yield* utxoRootFromMap(workingUtxos);
      applySnapshotLedgerOps(workingUtxos, sourceEvent.ledgerOps);
      const postUtxosRoot = yield* utxoRootFromMap(workingUtxos);
      const value: SDK.TransitionStep = {
        schema_version: 1n,
        step_index: BigInt(index),
        event_key: sourceEvent.eventKey,
        phase: sourceEvent.phase,
        pre_utxos_root: preUtxosRoot,
        post_utxos_root: postUtxosRoot,
      };
      transitionTraceMembers.push({
        stepIndex: value.step_index,
        keyCbor: encodePlutusData(value.step_index, LucidData.Integer()),
        valueCbor: encodePlutusData(value, SDK.TransitionStepSchema),
        value,
      });
    }
    const eventToStepMembers = yield* buildEventToStepMembersFromTrace({
      sourceEvents,
      transitionTraceMembers,
    });
    return {
      finalUtxosRoot: yield* utxoRootFromMap(workingUtxos),
      transitionTraceRoot: yield* countedRootFromEncodedEntries(
        SDK.ROOT_DOMAINS.transitionTrace,
        transitionTraceMembers.map((member) => ({
          key: member.keyCbor,
          value: member.valueCbor,
        })),
      ),
      eventToStepRoot: yield* countedRootFromEncodedEntries(
        SDK.ROOT_DOMAINS.eventToStep,
        eventToStepMembers.map((member) => ({
          key: member.keyCbor,
          value: member.valueCbor,
        })),
      ),
      transitionTraceMembers,
      eventToStepMembers,
    };
  });

const expectIncrementalTraceMatchesSnapshot = ({
  initialUtxos,
  sourceEvents,
  withdrawalCount,
  forcedTransactionCount,
  l2TransactionCount,
  depositCount,
}: {
  readonly initialUtxos: readonly UtxoPayloadEntry[];
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly withdrawalCount: number;
  readonly forcedTransactionCount: number;
  readonly l2TransactionCount: number;
  readonly depositCount: number;
}) =>
  Effect.gen(function* () {
    const expected = yield* snapshotTraceOracle({ initialUtxos, sourceEvents });
    const actual = yield* buildTransitionTraceResult({
      initialUtxos,
      sourceEvents,
      withdrawalCount,
      forcedTransactionCount,
      l2TransactionCount,
      depositCount,
    });

    expect(actual.finalUtxosRoot).toBe(expected.finalUtxosRoot);
    expect(actual.transitionTraceRoot).toBe(expected.transitionTraceRoot);
    expect(actual.eventToStepRoot).toBe(expected.eventToStepRoot);
    expect(actual.transitionTraceMembers).toStrictEqual(
      expected.transitionTraceMembers,
    );
    expect(actual.eventToStepMembers).toStrictEqual(
      expected.eventToStepMembers,
    );
    expect(actual.withdrawalCount).toBe(withdrawalCount);
    expect(actual.forcedTransactionCount).toBe(forcedTransactionCount);
    expect(actual.l2TransactionCount).toBe(l2TransactionCount);
    expect(actual.depositCount).toBe(depositCount);
    expect(actual.totalEventCount).toBe(sourceEvents.length);
    expect(actual.transitionStepCount).toBe(sourceEvents.length);
    return actual;
  });

beforeAll(async () => {
  await Effect.runPromise(deleteMpfStore(TRACE_PERSIST_DB, "transition-trace"));
  await Effect.runPromise(deleteMpfStore(TRACE_LEGACY_DB, "trace-legacy"));
  await Effect.runPromise(deleteMpfStore(TRACE_OVERLAY_DB, "trace-overlay"));
});

afterAll(async () => {
  await Effect.runPromise(deleteMpfStore(TRACE_PERSIST_DB, "transition-trace"));
  await Effect.runPromise(deleteMpfStore(TRACE_LEGACY_DB, "trace-legacy"));
  await Effect.runPromise(deleteMpfStore(TRACE_OVERLAY_DB, "trace-overlay"));
});

describe("transition trace builder", () => {
  it.effect(
    "indexes validation-trace source material by canonical event key rather than step key",
    () =>
      Effect.gen(function* () {
        const eventKey = l2TransactionEventKey(91);
        const result = yield* buildTransitionTraceResult({
          initialUtxos: [],
          sourceEvents: [noOpEvent("L2Transaction", eventKey)],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 1,
          depositCount: 0,
        });
        const [member] = result.transitionTraceMembers;
        const byEventKey = yield* indexTransitionTraceMembersByEventKey(
          result.transitionTraceMembers,
        );
        const eventKeyHex =
          encodeTransitionEventKeyCbor(eventKey).toString("hex");

        expect(byEventKey.get(eventKeyHex)).toBe(member);
        expect(byEventKey.has(member!.keyCbor.toString("hex"))).toBe(false);
      }),
  );

  it.effect(
    "rejects duplicate event keys while indexing validation-trace source material",
    () =>
      Effect.gen(function* () {
        const result = yield* buildTransitionTraceResult({
          initialUtxos: [],
          sourceEvents: [noOpEvent("L2Transaction", l2TransactionEventKey(92))],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 1,
          depositCount: 0,
        });
        const [member] = result.transitionTraceMembers;
        const duplicate = yield* indexTransitionTraceMembersByEventKey([
          member!,
          {
            ...member!,
            stepIndex: 1n,
            keyCbor: encodePlutusData(1n, LucidData.Integer()),
            value: {
              ...member!.value,
              step_index: 1n,
            },
          },
        ]).pipe(Effect.either);

        expect(duplicate._tag).toBe("Left");
      }),
  );

  it.effect("always emits the exact V1 transition-step schema", () =>
    Effect.gen(function* () {
      const ledgerMpf = yield* makeLedgerMpf([initialUtxo(10)]);
      const result = yield* buildTransitionTraceResultFromMpf({
        ledgerMpf,
        sourceEvents: [
          noOpEvent("ForcedTransaction", forcedTransactionEventKey(2)),
        ],
        withdrawalCount: 0,
        forcedTransactionCount: 1,
        l2TransactionCount: 0,
        depositCount: 0,
      });

      expect(result.transitionTraceMembers).toHaveLength(1);
      expect(result.transitionTraceMembers[0]!.value.schema_version).toBe(1n);
    }),
  );

  it.effect(
    "replays every transition step with byte-identical legacy and overlay roots",
    () =>
      Effect.gen(function* () {
        const initialUtxos = [initialUtxo(10), initialUtxo(20)];
        const sourceEvents: TransitionTraceSourceEvent[] = [
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
        const initialOps: MpfBatchOp[] = initialUtxos.map((entry) => ({
          type: "insert",
          key: entry.outref,
          value: entry.output,
        }));
        const legacy = yield* MidgardMpf.create(
          "trace-legacy",
          TRACE_LEGACY_DB,
        );
        const overlay = yield* MidgardMpf.create(
          "trace-overlay",
          TRACE_OVERLAY_DB,
          { engine: "overlay" },
        );
        yield* legacy.applyBatch(initialOps);
        yield* overlay.applyBatch(initialOps);
        yield* overlay.beginBlockOverlay();

        const legacyResult = yield* buildTransitionTraceResultFromMpf({
          ledgerMpf: legacy,
          sourceEvents,
          withdrawalCount: 1,
          forcedTransactionCount: 1,
          l2TransactionCount: 1,
          depositCount: 1,
        });
        const overlayResult = yield* buildTransitionTraceResultFromMpf({
          ledgerMpf: overlay,
          sourceEvents,
          withdrawalCount: 1,
          forcedTransactionCount: 1,
          l2TransactionCount: 1,
          depositCount: 1,
        });

        expect(overlayResult.finalUtxosRoot).toBe(legacyResult.finalUtxosRoot);
        expect(overlayResult.transitionTraceRoot).toBe(
          legacyResult.transitionTraceRoot,
        );
        expect(overlayResult.eventToStepRoot).toBe(
          legacyResult.eventToStepRoot,
        );
        expect(
          overlayResult.transitionTraceMembers.map((member) => ({
            pre: member.value.pre_utxos_root,
            post: member.value.post_utxos_root,
          })),
        ).toEqual(
          legacyResult.transitionTraceMembers.map((member) => ({
            pre: member.value.pre_utxos_root,
            post: member.value.post_utxos_root,
          })),
        );

        yield* overlay.flushBlockOverlay(
          Buffer.from(overlayResult.finalUtxosRoot, "hex"),
        );
        yield* legacy.close();
        yield* overlay.close();
        const reopened = yield* MidgardMpf.create(
          "trace-overlay",
          TRACE_OVERLAY_DB,
        );
        expect(yield* reopened.rootHex()).toBe(legacyResult.finalUtxosRoot);
        const proof = yield* reopened.prove(outRef(12));
        expect((yield* reopened.verify(proof, true)).toString("hex")).toBe(
          legacyResult.finalUtxosRoot,
        );
        yield* reopened.close();
      }),
  );

  it.effect(
    "incremental MPF builder matches the full-snapshot builder for a multi-event fixture",
    () =>
      Effect.gen(function* () {
        const initialUtxos = [initialUtxo(10), initialUtxo(20)];
        const sourceEvents: TransitionTraceSourceEvent[] = [
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

        yield* expectIncrementalTraceMatchesSnapshot({
          initialUtxos,
          sourceEvents,
          withdrawalCount: 1,
          forcedTransactionCount: 1,
          l2TransactionCount: 1,
          depositCount: 1,
        });
      }),
  );

  it.effect(
    "preserves snapshot semantics for delete-insert, insert-delete, and no-op events",
    () =>
      Effect.gen(function* () {
        const result = yield* expectIncrementalTraceMatchesSnapshot({
          initialUtxos: [initialUtxo(30)],
          sourceEvents: [
            {
              phase: "L2Transaction",
              eventKey: l2TransactionEventKey(10),
              ledgerOps: [
                { type: "delete", key: outRef(30) },
                { type: "insert", key: outRef(30), value: output(31) },
              ],
            },
            {
              phase: "L2Transaction",
              eventKey: l2TransactionEventKey(11),
              ledgerOps: [
                { type: "insert", key: outRef(32), value: output(32) },
                { type: "delete", key: outRef(32) },
              ],
            },
            noOpEvent("L2Transaction", l2TransactionEventKey(12)),
          ],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 3,
          depositCount: 0,
        });

        const [deleteInsert, insertDelete, noOp] =
          result.transitionTraceMembers.map((member) => member.value);
        expect(deleteInsert!.pre_utxos_root).not.toBe(
          deleteInsert!.post_utxos_root,
        );
        expect(insertDelete!.pre_utxos_root).toBe(
          insertDelete!.post_utxos_root,
        );
        expect(noOp!.pre_utxos_root).toBe(noOp!.post_utxos_root);
        expect(result.finalUtxosRoot).toBe(noOp!.post_utxos_root);
      }),
  );

  it.effect(
    "incremental builder rejects missing deletes without moving root",
    () =>
      Effect.gen(function* () {
        const ledgerMpf = yield* makeLedgerMpf([initialUtxo(40)]);
        const before = yield* ledgerMpf.rootHex();
        const result = yield* buildTransitionTraceResultFromMpf({
          ledgerMpf,
          sourceEvents: [
            {
              phase: "L2Transaction",
              eventKey: l2TransactionEventKey(20),
              ledgerOps: [{ type: "delete", key: outRef(41) }],
            },
          ],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 1,
          depositCount: 0,
        }).pipe(Effect.either);

        expect(result._tag).toBe("Left");
        expect(yield* ledgerMpf.rootHex()).toBe(before);
      }),
  );

  it.effect(
    "incremental builder rejects duplicate inserts without moving root",
    () =>
      Effect.gen(function* () {
        const ledgerMpf = yield* makeLedgerMpf([initialUtxo(42)]);
        const before = yield* ledgerMpf.rootHex();
        const result = yield* buildTransitionTraceResultFromMpf({
          ledgerMpf,
          sourceEvents: [
            {
              phase: "L2Transaction",
              eventKey: l2TransactionEventKey(21),
              ledgerOps: [
                { type: "insert", key: outRef(42), value: output(43) },
              ],
            },
          ],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 1,
          depositCount: 0,
        }).pipe(Effect.either);

        expect(result._tag).toBe("Left");
        expect(yield* ledgerMpf.rootHex()).toBe(before);
      }),
  );

  it.effect(
    "failed incremental trace construction restores the failed event pre-root",
    () =>
      Effect.gen(function* () {
        const ledgerMpf = yield* makeLedgerMpf([]);
        const expectedPreFailedEventMpf = yield* makeLedgerMpf([]);
        yield* expectedPreFailedEventMpf.applyBatch([
          { type: "insert", key: outRef(50), value: output(50) },
        ]);
        const expectedPreFailedEventRoot =
          yield* expectedPreFailedEventMpf.rootHex();

        const result = yield* buildTransitionTraceResultFromMpf({
          ledgerMpf,
          sourceEvents: [
            {
              phase: "L2Transaction",
              eventKey: l2TransactionEventKey(30),
              ledgerOps: [
                { type: "insert", key: outRef(50), value: output(50) },
              ],
            },
            {
              phase: "L2Transaction",
              eventKey: l2TransactionEventKey(31),
              ledgerOps: [
                { type: "insert", key: outRef(50), value: output(51) },
              ],
            },
          ],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 2,
          depositCount: 0,
        }).pipe(Effect.either);

        expect(result._tag).toBe("Left");
        expect(yield* ledgerMpf.rootHex()).toBe(expectedPreFailedEventRoot);
      }),
  );

  it.effect(
    "persists the final root marker after an incremental trace succeeds",
    () =>
      Effect.gen(function* () {
        yield* deleteMpfStore(TRACE_PERSIST_DB, "transition-trace");
        const ledgerMpf = yield* MidgardMpf.create(
          "transition-trace",
          TRACE_PERSIST_DB,
        );
        yield* ledgerMpf.applyBatch([
          { type: "insert", key: outRef(60), value: output(60) },
        ]);
        const result = yield* buildTransitionTraceResultFromMpf({
          ledgerMpf,
          sourceEvents: [
            {
              phase: "L2Transaction",
              eventKey: l2TransactionEventKey(40),
              ledgerOps: [
                { type: "delete", key: outRef(60) },
                { type: "insert", key: outRef(61), value: output(61) },
              ],
            },
          ],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 1,
          depositCount: 0,
        });
        yield* ledgerMpf.close();

        const reopened = yield* MidgardMpf.create(
          "transition-trace",
          TRACE_PERSIST_DB,
        );
        const reopenedRoot = yield* reopened.rootHex();
        const inserted = yield* reopened.get(outRef(61));
        yield* reopened.close();

        expect(reopenedRoot).toBe(result.finalUtxosRoot);
        expect(inserted._tag).toBe("Some");
      }),
  );

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

  it.effect(
    "rejects source counts and actual ledger operations above the V1 consensus profile",
    () =>
      Effect.gen(function* () {
        const excessiveSourceCount = yield* buildTransitionTraceResult({
          initialUtxos: [],
          sourceEvents: [],
          withdrawalCount: MIDGARD_CONSENSUS_LIMITS_V1.maxWithdrawalCount + 1,
          forcedTransactionCount: 0,
          l2TransactionCount: 0,
          depositCount: 0,
        }).pipe(Effect.either);
        expect(excessiveSourceCount._tag).toBe("Left");

        const excessiveOperations = yield* buildTransitionTraceResult({
          initialUtxos: [],
          sourceEvents: [
            {
              phase: "L2Transaction",
              eventKey: l2TransactionEventKey(99),
              ledgerOps: Array.from(
                {
                  length:
                    MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOperationCount + 1,
                },
                () => ({ type: "delete" as const, key: outRef(1) }),
              ),
            },
          ],
          withdrawalCount: 0,
          forcedTransactionCount: 0,
          l2TransactionCount: 1,
          depositCount: 0,
        }).pipe(Effect.either);
        expect(excessiveOperations._tag).toBe("Left");
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
