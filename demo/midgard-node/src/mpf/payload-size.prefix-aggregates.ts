import { canonicalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { Effect } from "effect";

import * as DepositsDB from "../database/deposits.js";
import * as Ledger from "../database/utils/ledger.js";
import { MpfError } from "./errors.js";
import { transitionEffectToLedgerOps } from "./ledger-delta.js";
import {
  applyLedgerOpsToUtxoPayloadAggregateFromFullValues,
  createUtxoPayloadSizeAccumulator,
  type UtxoPayloadSizeAggregate,
} from "./payload-size.js";
import type { TransitionTraceSourceEvent } from "./trace-events.js";
import { depositTraceEventKey } from "./trace-events.js";

/**
 * Every ordinary prefix includes the same mandatory work. Deposits are the last
 * ledger phase: their fixed writes override any earlier ordinary write at that
 * key. Apply that overlay first for size accounting, then ignore overridden
 * ordinary keys; this visits ledger operations once rather than replaying each
 * prefix's deposit phase. It does not change actual validation or trie replay.
 */
export const ledgerPayloadAggregatesByOrdinaryPrefix = ({
  base,
  sourceEvents,
  initialValues,
  insertedValues,
  depositValues,
}: {
  readonly base: UtxoPayloadSizeAggregate;
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly initialValues: ReadonlyMap<string, Buffer>;
  readonly insertedValues: ReadonlyMap<string, Buffer>;
  readonly depositValues: ReadonlyMap<string, Buffer>;
}): Effect.Effect<readonly UtxoPayloadSizeAggregate[], MpfError> =>
  Effect.try({
    try: () => {
      const accumulator = createUtxoPayloadSizeAccumulator(base, initialValues);
      const normalEvents: TransitionTraceSourceEvent[] = [];
      const deposits: TransitionTraceSourceEvent[] = [];
      let phase = 0;
      for (const event of sourceEvents) {
        const nextPhase =
          event.phase === "Withdrawal"
            ? 0
            : event.phase === "ForcedTransaction"
              ? 1
              : event.phase === "L2Transaction"
                ? 2
                : 3;
        if (nextPhase < phase)
          throw new Error("DA prefix ledger phases are not ordered");
        phase = nextPhase;
        if (phase < 2)
          for (const op of event.ledgerOps)
            accumulator.apply(op, insertedValues);
        else if (phase === 2) normalEvents.push(event);
        else deposits.push(event);
      }
      const overriddenKeys = new Set<string>();
      for (const event of deposits)
        for (const op of event.ledgerOps) {
          if (op.type !== "insert")
            throw new Error(
              "DA deposit prefix overlay must contain only insertions",
            );
          overriddenKeys.add(op.key.toString("hex"));
          accumulator.apply(op, depositValues);
        }
      const prefixes = [accumulator.snapshot()];
      for (const event of normalEvents) {
        for (const op of event.ledgerOps)
          if (!overriddenKeys.has(op.key.toString("hex")))
            accumulator.apply(op, insertedValues);
        prefixes.push(accumulator.snapshot());
      }
      return prefixes;
    },
    catch: (cause) =>
      MpfError.rootBuild("DA prefix UTxO size aggregates", cause),
  });

/** Builds and cross-checks prefix accounting against the existing full replay. */
export const buildLedgerPayloadPrefixAccounting = ({
  base,
  sourceEvents,
  initialLedgerEntries,
  insertedValues,
  depositValues,
  acceptedTxCount,
}: {
  readonly base: UtxoPayloadSizeAggregate;
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly initialLedgerEntries: readonly Ledger.MinimalEntry[];
  readonly insertedValues: ReadonlyMap<string, Buffer>;
  readonly depositValues: ReadonlyMap<string, Buffer>;
  readonly acceptedTxCount: number;
}) =>
  Effect.gen(function* () {
    const initialValues = new Map(
      initialLedgerEntries.map((entry) => [
        entry[Ledger.Columns.OUTREF].toString("hex"),
        Buffer.from(entry[Ledger.Columns.OUTPUT]),
      ]),
    );
    const utxoPayloadAggregate =
      yield* applyLedgerOpsToUtxoPayloadAggregateFromFullValues(
        base,
        sourceEvents.flatMap((event) => event.ledgerOps),
        initialValues,
        insertedValues,
      );
    const utxoPayloadAggregatesByPrefix =
      yield* ledgerPayloadAggregatesByOrdinaryPrefix({
        base,
        sourceEvents,
        initialValues,
        insertedValues,
        depositValues,
      });
    const fullPrefixAggregate = utxoPayloadAggregatesByPrefix.at(-1)!;
    if (
      utxoPayloadAggregatesByPrefix.length !== acceptedTxCount + 1 ||
      fullPrefixAggregate.entryCount !== utxoPayloadAggregate.entryCount ||
      fullPrefixAggregate.encodedTupleBytes !==
        utxoPayloadAggregate.encodedTupleBytes
    ) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "DA prefix aggregate",
          new Error(
            "DA prefix aggregates disagree with the actual full ledger",
          ),
        ),
      );
    }
    return { utxoPayloadAggregate, utxoPayloadAggregatesByPrefix };
  });

/** Retains each deposit's full output for the final mandatory size overlay. */
export const buildDepositPrefixSources = (
  entries: readonly DepositsDB.Entry[],
) =>
  Effect.gen(function* () {
    const outputs = new Map<string, Buffer>();
    const sourceEvents = yield* Effect.forEach(entries, (entry) =>
      Effect.gen(function* () {
        const ledgerEntry = yield* DepositsDB.toLedgerEntry(entry);
        outputs.set(
          ledgerEntry[Ledger.Columns.OUTREF].toString("hex"),
          ledgerEntry[Ledger.Columns.OUTPUT],
        );
        const effect = canonicalDepositTransitionEffect({
          outRefCbor: ledgerEntry[Ledger.Columns.OUTREF],
          outputCbor: ledgerEntry[Ledger.Columns.OUTPUT],
        });
        return {
          eventKey: yield* depositTraceEventKey(entry),
          phase: "Deposit" as const,
          ledgerOps: transitionEffectToLedgerOps(effect),
        } satisfies TransitionTraceSourceEvent;
      }),
    );
    return { sourceEvents, outputs };
  });
