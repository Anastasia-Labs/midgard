import { isDeepStrictEqual } from "node:util";

import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as ProjectedEvents from "./utils/projected-events.js";
import * as UserEvents from "./utils/user-events.js";

export const tableName = "withdrawal_utxos";

export enum Columns {
  ID = UserEvents.Columns.ID,
  RAW_EVENT_INFO = "raw_event_info",
  SETTLEMENT_EVENT_INFO = "settlement_event_info",
  INCLUSION_TIME = UserEvents.Columns.INCLUSION_TIME,
  // Latest authenticated history location; admission identity is the stable ID.
  WITHDRAWAL_L1_TX_HASH = "withdrawal_l1_tx_hash",
  WITHDRAWAL_L1_OUTPUT_INDEX = "withdrawal_l1_output_index",
  ASSET_NAME = "asset_name",
  L2_OUTREF = "l2_outref",
  L2_OWNER = "l2_owner",
  L2_VALUE = "l2_value",
  L1_ADDRESS = "l1_address",
  L1_DATUM = "l1_datum",
  REFUND_ADDRESS = "refund_address",
  REFUND_DATUM = "refund_datum",
  VALIDITY = "validity",
  VALIDITY_DETAIL = "validity_detail",
  CLASSIFICATION_REVISION = "classification_revision",
  REOPENED_FROM_HEADER_HASH = "reopened_from_header_hash",
  PROJECTED_HEADER_HASH = "projected_header_hash",
  STATUS = "status",
}

export const Status = {
  Awaiting: "awaiting",
  Projected: "projected",
  Finalized: "finalized",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export const Validity = {
  WithdrawalIsValid: "WithdrawalIsValid",
  NonExistentWithdrawalUtxo: "NonExistentWithdrawalUtxo",
  SpentWithdrawalUtxo: "SpentWithdrawalUtxo",
  IncorrectWithdrawalOwner: "IncorrectWithdrawalOwner",
  IncorrectWithdrawalValue: "IncorrectWithdrawalValue",
  IncorrectWithdrawalSignature: "IncorrectWithdrawalSignature",
  TooManyTokensInWithdrawal: "TooManyTokensInWithdrawal",
  UnpayableWithdrawalValue: "UnpayableWithdrawalValue",
} as const;

export type Validity = (typeof Validity)[keyof typeof Validity];

export type Entry = {
  [Columns.ID]: Buffer;
  [Columns.RAW_EVENT_INFO]: Buffer;
  [Columns.SETTLEMENT_EVENT_INFO]: Buffer | null;
  [Columns.INCLUSION_TIME]: Date;
  [Columns.WITHDRAWAL_L1_TX_HASH]: Buffer;
  [Columns.WITHDRAWAL_L1_OUTPUT_INDEX]: number;
  [Columns.ASSET_NAME]: Buffer;
  [Columns.L2_OUTREF]: Buffer;
  [Columns.L2_OWNER]: Buffer;
  [Columns.L2_VALUE]: Buffer;
  [Columns.L1_ADDRESS]: Buffer;
  [Columns.L1_DATUM]: Buffer;
  [Columns.REFUND_ADDRESS]: Buffer;
  [Columns.REFUND_DATUM]: Buffer;
  [Columns.VALIDITY]: Validity | null;
  [Columns.VALIDITY_DETAIL]: unknown;
  [Columns.CLASSIFICATION_REVISION]: number;
  [Columns.REOPENED_FROM_HEADER_HASH]: Buffer | null;
  [Columns.PROJECTED_HEADER_HASH]: Buffer | null;
  [Columns.STATUS]: Status;
};

export type SettlementInfoAssignment = {
  readonly eventId: Buffer;
  readonly expectedClassificationRevision: number;
  readonly settlementEventInfo: Buffer;
  readonly validity: Validity;
  readonly validityDetail?: unknown;
};

const projectedEventsTable = {
  tableName,
  idColumn: Columns.ID,
  inclusionTimeColumn: Columns.INCLUSION_TIME,
  projectedHeaderHashColumn: Columns.PROJECTED_HEADER_HASH,
  statusColumn: Columns.STATUS,
  awaitingStatus: Status.Awaiting,
  projectedStatus: Status.Projected,
  terminalStatus: Status.Finalized,
  entitySingular: "withdrawal",
  entityPlural: "withdrawals",
  idLabel: "event_id",
  touchUpdatedAt: true,
  validateHeaderAssignment: (row, idHex) =>
    row[Columns.SETTLEMENT_EVENT_INFO] === null ||
    row[Columns.VALIDITY] === null
      ? Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Failed to assign projected header because the withdrawal has not been classified",
            cause: `event_id=${idHex}`,
          }),
        )
      : Effect.void,
} as const satisfies ProjectedEvents.ProjectedEventTable;

export const projectedEventAdapter =
  ProjectedEvents.makeProjectedEventAdapter<Entry>({
    config: projectedEventsTable,
    pendingHeaderStatuses: [Status.Awaiting, Status.Projected],
    projectedPendingStatuses: [Status.Projected],
    messages: {
      retrieveByProjectedHeaderHash:
        "Failed to retrieve withdrawals by projected header hash",
      retrievePendingHeaderEntriesUpTo:
        "Failed to retrieve withdrawals pending header assignment",
      retrieveProjectedPendingHeaderEntries:
        "Failed to retrieve projected withdrawals awaiting header assignment",
      markAwaitingAsProjected:
        "Failed to mark awaiting withdrawals as projected",
      markProjectedByEventIds:
        "Failed to mark withdrawals as assigned to the given header",
      clearProjectedHeaderAssignmentByEventIds:
        "Failed to clear projected header assignments for withdrawals",
    },
  });

export const validityDetailEquals = (left: unknown, right: unknown): boolean =>
  isDeepStrictEqual(left ?? {}, right ?? {});

export const retrieveAllEntries = (): Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntries ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve withdrawals"),
  );

export const retrieveByEventId = (
  eventId: Buffer,
): Effect.Effect<Option.Option<Entry>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.ID)} = ${eventId}
      LIMIT 1`;
    return rows.length === 0 ? Option.none() : Option.some(rows[0]!);
  }).pipe(
    Effect.withLogSpan(`retrieveByEventId ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve withdrawal by event id",
    ),
  );
