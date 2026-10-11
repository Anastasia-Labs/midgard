import { LOVELACE_UNIT, normalizeAssets } from "@al-ft/midgard-core/assets";
import { classifyWithdrawalFromLedger } from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import { DatabaseError } from "../../../database/utils/common.js";
import * as Ledger from "../../../database/utils/ledger.js";
import * as WithdrawalsDB from "../../../database/withdrawals.js";

export type ClassifiedWithdrawal = {
  readonly entry: WithdrawalsDB.Entry;
  readonly ledgerOutRef: Buffer;
  readonly validity: WithdrawalsDB.Validity;
  readonly validityDetail: unknown;
  readonly settlementEventInfo: Buffer;
  readonly shouldDeleteLedgerUtxo: boolean;
};

export { LOVELACE_UNIT, normalizeAssets };

export const indexSelectedLedgerOutputs = (
  entries: readonly Ledger.MinimalEntry[],
): Effect.Effect<ReadonlyMap<string, Buffer>, DatabaseError, never> =>
  Effect.try({
    try: () => {
      const outputs = new Map<string, Buffer>();
      for (const entry of entries) {
        const outRef = entry[Ledger.Columns.OUTREF].toString("hex");
        if (outputs.has(outRef)) {
          throw new Error(
            `selected ledger snapshot contains duplicate outref ${outRef}`,
          );
        }
        outputs.set(outRef, Buffer.from(entry[Ledger.Columns.OUTPUT]));
      }
      return outputs;
    },
    catch: (cause) =>
      new DatabaseError({
        table: WithdrawalsDB.tableName,
        message:
          "Failed to index selected ledger snapshot for withdrawal classification",
        cause,
      }),
  });
export const classifyWithdrawal = ({
  entry,
  ledgerOutRef,
  ledgerOutput,
}: {
  readonly entry: WithdrawalsDB.Entry;
  readonly ledgerOutRef: Buffer;
  readonly ledgerOutput: Option.Option<Buffer>;
}): Effect.Effect<ClassifiedWithdrawal, DatabaseError, never> =>
  classifyWithdrawalFromLedger({
    l2Owner: entry[WithdrawalsDB.Columns.L2_OWNER].toString("hex"),
    l2ValueCbor: entry[WithdrawalsDB.Columns.L2_VALUE].toString("hex"),
    eventInfoCbor: entry[WithdrawalsDB.Columns.RAW_EVENT_INFO].toString("hex"),
    ledgerOutRef,
    ledgerOutput: Option.getOrNull(ledgerOutput),
  }).pipe(
    Effect.map((classification) => ({
      entry,
      ledgerOutRef,
      ...classification,
    })),
    Effect.mapError(
      ({ message, cause }) =>
        new DatabaseError({ table: WithdrawalsDB.tableName, message, cause }),
    ),
  );

/**
 * Classifies a block's withdrawals in trace-step order. Each one is judged
 * against the ledger the earlier payable withdrawals leave: a payable
 * withdrawal deletes its outref, a non-payable one changes nothing, and the
 * withdrawal phase runs before any other phase. A later withdrawal of an
 * outref an earlier one drained finds no output, so it is
 * `NonExistentWithdrawalUtxo`, which is what foreign replay derives.
 */
export const classifyWithdrawalsInOrder = ({
  entries,
  baseOutputs,
}: {
  readonly entries: readonly WithdrawalsDB.Entry[];
  readonly baseOutputs: ReadonlyMap<string, Buffer>;
}): Effect.Effect<readonly ClassifiedWithdrawal[], DatabaseError, never> =>
  Effect.gen(function* () {
    const drained = new Set<string>();
    const classified: ClassifiedWithdrawal[] = [];
    for (const entry of entries) {
      const ledgerOutRef = yield* WithdrawalsDB.toLedgerOutRef(entry);
      const ledgerOutRefHex = ledgerOutRef.toString("hex");
      const output = drained.has(ledgerOutRefHex)
        ? undefined
        : baseOutputs.get(ledgerOutRefHex);
      const withdrawal = yield* classifyWithdrawal({
        entry,
        ledgerOutRef,
        ledgerOutput:
          output === undefined
            ? Option.none<Buffer>()
            : Option.some(Buffer.from(output)),
      });
      if (withdrawal.shouldDeleteLedgerUtxo) drained.add(ledgerOutRefHex);
      classified.push(withdrawal);
    }
    return classified;
  });
