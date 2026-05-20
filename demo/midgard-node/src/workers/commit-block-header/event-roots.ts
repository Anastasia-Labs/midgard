import { Effect, Option } from "effect";
import { DepositsDB, WithdrawalsDB } from "@/database/index.js";
import type { DatabaseError } from "@/database/utils/common.js";
import { keyValuePhasRoot, type MpfError } from "@/workers/utils/mpf.js";

export const resolveDepositsRoot = (
  depositEntries: readonly DepositsDB.Entry[],
): Effect.Effect<Option.Option<string>, MpfError, never> =>
  Effect.gen(function* () {
    if (depositEntries.length <= 0) {
      return Option.none();
    }
    const eventIds = depositEntries.map(
      (entry) => entry[DepositsDB.Columns.ID],
    );
    const eventInfos = depositEntries.map(
      (entry) => entry[DepositsDB.Columns.INFO],
    );
    const root = yield* keyValuePhasRoot(eventIds, eventInfos);
    return Option.some(root);
  });

export const resolveWithdrawalsRoot = (
  withdrawalEntries: readonly WithdrawalsDB.Entry[],
): Effect.Effect<Option.Option<string>, MpfError | DatabaseError, never> =>
  Effect.gen(function* () {
    if (withdrawalEntries.length <= 0) {
      return Option.none();
    }
    const keyValues = yield* Effect.forEach(
      withdrawalEntries,
      WithdrawalsDB.toRootKeyValue,
    );
    const root = yield* keyValuePhasRoot(
      keyValues.map((keyValue) => keyValue.key),
      keyValues.map((keyValue) => keyValue.value),
    );
    return Option.some(root);
  });
