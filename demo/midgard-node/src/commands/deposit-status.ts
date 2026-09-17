import * as SDK from "@al-ft/midgard-sdk";
import { Data as EffectData, Effect, Option } from "effect";

import * as DepositsDB from "../database/deposits.js";
import { DatabaseError } from "../database/utils/common.js";
import { Database } from "../services/database.js";
import { parseEventId, parseHexBytes } from "./command-utils.js";

/**
 * Query selectors accepted by `GET /deposit-status`.
 */
export type DepositStatusLookup = {
  readonly eventId?: Buffer;
  readonly cardanoTxHash?: Buffer;
};

/**
 * Stable JSON response shape for one deposit row.
 */
export type SerializedDepositStatus = {
  readonly eventId: string;
  readonly eventInfo: string;
  readonly inclusionTime: string;
  readonly cardanoTxHash: string;
  readonly ledgerTxId: string;
  readonly ledgerOutput: string;
  readonly ledgerAddress: string;
  readonly projectedHeaderHash: string | null;
  readonly status: DepositsDB.Status;
};

/**
 * Tagged error for request parsing and singular-lookup enforcement.
 */
export class DepositStatusCommandError extends EffectData.TaggedError(
  "DepositStatusCommandError",
)<{
  readonly message: string;
  readonly status: 400 | 404 | 409;
}> {}

/**
 * Parses `GET /deposit-status` selectors from query params.
 */
export const parseDepositStatusLookup = (
  params: Readonly<Record<string, unknown>>,
): DepositStatusLookup => {
  const eventIdParam = params.eventId;
  const cardanoTxHashParam = params.cardanoTxHash;

  if (eventIdParam === undefined && cardanoTxHashParam === undefined) {
    throw new Error(
      "GET /deposit-status requires `eventId` or `cardanoTxHash`.",
    );
  }

  return {
    ...(eventIdParam === undefined
      ? {}
      : { eventId: parseEventId(eventIdParam) }),
    ...(cardanoTxHashParam === undefined
      ? {}
      : {
          cardanoTxHash: parseHexBytes(cardanoTxHashParam, "cardanoTxHash", 32),
        }),
  };
};

/**
 * Serializes one deposit row into the stable HTTP response shape.
 */
export const encodeDepositStatus = (
  entry: DepositsDB.Entry,
): SerializedDepositStatus => ({
  eventId: SDK.bufferToHex(entry[DepositsDB.Columns.ID]),
  eventInfo: SDK.bufferToHex(entry[DepositsDB.Columns.INFO]),
  inclusionTime: entry[DepositsDB.Columns.INCLUSION_TIME].toISOString(),
  cardanoTxHash: SDK.bufferToHex(entry[DepositsDB.Columns.DEPOSIT_L1_TX_HASH]),
  ledgerTxId: SDK.bufferToHex(entry[DepositsDB.Columns.LEDGER_TX_ID]),
  ledgerOutput: SDK.bufferToHex(entry[DepositsDB.Columns.LEDGER_OUTPUT]),
  ledgerAddress: entry[DepositsDB.Columns.LEDGER_ADDRESS],
  projectedHeaderHash:
    entry[DepositsDB.Columns.PROJECTED_HEADER_HASH] === null
      ? null
      : SDK.bufferToHex(entry[DepositsDB.Columns.PROJECTED_HEADER_HASH]),
  status: entry[DepositsDB.Columns.STATUS],
});

/**
 * Resolves exactly one deposit row for the provided selector set.
 */
export const resolveDepositStatusProgram = (
  lookup: DepositStatusLookup,
): Effect.Effect<
  DepositsDB.Entry,
  DatabaseError | DepositStatusCommandError,
  Database
> =>
  Effect.gen(function* () {
    const byEventId =
      lookup.eventId === undefined
        ? Option.none<DepositsDB.Entry>()
        : yield* DepositsDB.retrieveByEventId(lookup.eventId);

    if (lookup.eventId !== undefined && Option.isNone(byEventId)) {
      return yield* Effect.fail(
        new DepositStatusCommandError({
          status: 404,
          message: `Deposit not found for eventId ${lookup.eventId.toString("hex")}`,
        }),
      );
    }

    if (lookup.cardanoTxHash === undefined) {
      if (Option.isNone(byEventId)) {
        return yield* Effect.fail(
          new DepositStatusCommandError({
            status: 404,
            message: "Deposit not found",
          }),
        );
      }
      return byEventId.value;
    }

    if (Option.isSome(byEventId)) {
      if (
        !byEventId.value[DepositsDB.Columns.DEPOSIT_L1_TX_HASH].equals(
          lookup.cardanoTxHash,
        )
      ) {
        return yield* Effect.fail(
          new DepositStatusCommandError({
            status: 409,
            message:
              "Provided eventId and cardanoTxHash do not refer to the same deposit.",
          }),
        );
      }
      return byEventId.value;
    }

    const matches = yield* DepositsDB.retrieveByCardanoTxHash(
      lookup.cardanoTxHash,
    );
    if (matches.length <= 0) {
      return yield* Effect.fail(
        new DepositStatusCommandError({
          status: 404,
          message: `Deposit not found for cardanoTxHash ${lookup.cardanoTxHash.toString("hex")}`,
        }),
      );
    }
    if (matches.length > 1) {
      return yield* Effect.fail(
        new DepositStatusCommandError({
          status: 409,
          message:
            "Multiple deposits found for the provided cardanoTxHash; query by eventId to disambiguate.",
        }),
      );
    }
    return matches[0]!;
  });
