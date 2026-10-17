import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  buildEventToStepMembersFromTrace,
  buildTransitionTraceResult as buildTransitionTraceResultFromMpf,
  keyValuePhasRoot,
  keyValuePhasRootWithCount,
  MidgardMpf,
  type MpfBatchOp,
  type RetainedTransitionTraceMember,
  type TransitionTraceSourceEvent,
  type UtxoPayloadEntry,
} from "../src/mpf/index.js";

export const TRACE_PERSIST_DB = `test-transition-trace-builder-${process.pid}`;

export const TRACE_DIRECT_DB = `${TRACE_PERSIST_DB}-direct`;

export const TRACE_OVERLAY_DB = `${TRACE_PERSIST_DB}-overlay`;

export const outRef = (byte: number) => Buffer.from([byte]);

export const output = (byte: number) => Buffer.from([byte, byte]);

export const initialUtxo = (byte: number): UtxoPayloadEntry => ({
  outref: outRef(byte),
  output: output(byte),
});

export const noOpEvent = (
  phase: SDK.TransitionPhase,
  eventKey: SDK.EventKey,
): TransitionTraceSourceEvent => ({
  phase,
  eventKey,
  ledgerOps: [],
});

export const makeLedgerMpf = (initialUtxos: readonly UtxoPayloadEntry[]) =>
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

export const buildTransitionTraceResult = ({
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

export const encodePlutusData = <A>(
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

export const expectIncrementalTraceMatchesSnapshot = ({
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
