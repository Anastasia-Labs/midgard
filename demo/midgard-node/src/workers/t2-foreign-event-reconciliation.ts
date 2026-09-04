import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { assertDeploymentMarkerMatches } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import {
  DaPayloadsDB,
  DepositsDB,
  ForcedTransactionsDB,
  ForeignTipReconciliationsDB,
  MempoolLedgerDB,
  WithdrawalsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { ContractDeploymentIdentity, Database } from "../services/index.js";
import { computeDaPayloadRoots } from "./commit-block-header/da-payload.js";

export type T2CandidateEventIds = {
  readonly deposits: readonly string[];
  readonly forcedTransactions: readonly string[];
  readonly withdrawals: readonly string[];
};

export type T2ForeignEventResolution =
  | {
      readonly type: "Ready";
      readonly absent: T2CandidateEventIds;
    }
  | {
      readonly type: "AwaitingForeignDa";
      readonly foreignHeaderHash: string;
      readonly reason:
        | "missing"
        | "invalid"
        | "foreign_event_present_requires_finalization";
      readonly detail: string;
      readonly present: T2CandidateEventIds;
    };

const emptyIds = (): T2CandidateEventIds => ({
  deposits: [],
  forcedTransactions: [],
  withdrawals: [],
});

const normalizedIds = (ids: readonly string[]): readonly string[] =>
  [...new Set(ids.map((id) => id.toLowerCase()))].sort();

const countsMatchHeader = (
  payload: SDK.DaPayload,
  header: SDK.Header,
): boolean => {
  const body = payload.block_body;
  const counts = body.counts;
  return (
    BigInt(body.deposits.length) === header.depositCount &&
    BigInt(body.forced_transactions.length) === header.forcedTransactionCount &&
    BigInt(body.withdrawals.length) === header.withdrawalCount &&
    BigInt(body.transactions.length) === header.l2TransactionCount &&
    BigInt(body.transition_trace.length) === header.transitionStepCount &&
    counts.depositCount === header.depositCount &&
    counts.forcedTransactionCount === header.forcedTransactionCount &&
    counts.withdrawalCount === header.withdrawalCount &&
    counts.l2TransactionCount === header.l2TransactionCount &&
    counts.totalEventCount === header.totalEventCount &&
    counts.transitionStepCount === header.transitionStepCount &&
    counts.validationTraceCount === header.validationTraceCount &&
    BigInt(body.validation_traces.length) === header.validationTraceCount
  );
};

const verifyForeignPayload = ({
  foreignHeaderHash,
  header,
  payload,
}: {
  readonly foreignHeaderHash: string;
  readonly header: SDK.Header;
  readonly payload: SDK.DaPayload;
}): Effect.Effect<boolean> =>
  Effect.gen(function* () {
    const [computedHeaderHash, payloadHeaderHash, roots] = yield* Effect.all(
      [
        SDK.hashBlockHeader(header),
        SDK.hashBlockHeader(payload.block_body.header),
        computeDaPayloadRoots(payload),
      ],
      { concurrency: "unbounded" },
    );
    return (
      computedHeaderHash === foreignHeaderHash &&
      payload.block_body.header_hash === foreignHeaderHash &&
      payloadHeaderHash === foreignHeaderHash &&
      roots.depositsRoot === header.depositsRoot &&
      roots.forcedTransactionsRoot === header.forcedTransactionsRoot &&
      roots.withdrawalsRoot === header.withdrawalsRoot &&
      roots.transactionsRoot === header.transactionsRoot &&
      roots.utxosRoot === header.utxosRoot &&
      roots.transitionTraceRoot === header.transitionTraceRoot &&
      roots.eventToStepRoot === header.eventToStepRoot &&
      roots.validationTracesRoot === header.validationTracesRoot &&
      countsMatchHeader(payload, header)
    );
  }).pipe(Effect.catchAll(() => Effect.succeed(false)));

const classifyCategory = ({
  candidateIds,
  root,
  count,
  payloadEntries,
}: {
  readonly candidateIds: readonly string[];
  readonly root: string;
  readonly count: bigint;
  readonly payloadEntries?: readonly SDK.DaPayloadEntry[];
}):
  | { readonly status: "ready"; readonly absent: readonly string[] }
  | { readonly status: "missing" | "invalid" }
  | {
      readonly status: "present";
      readonly present: readonly string[];
      readonly absent: readonly string[];
    } => {
  const normalized = normalizedIds(candidateIds);
  if (root === SDK.EMPTY_MERKLE_TREE_ROOT) {
    return count === 0n
      ? { status: "ready", absent: normalized }
      : { status: "invalid" };
  }
  if (count === 0n) return { status: "invalid" };
  if (normalized.length === 0) return { status: "ready", absent: [] };
  if (payloadEntries === undefined) return { status: "missing" };
  const foreignIds = new Set(payloadEntries.map(([key]) => key.toLowerCase()));
  const present = normalized.filter((id) => foreignIds.has(id));
  const absent = normalized.filter((id) => !foreignIds.has(id));
  return present.length === 0
    ? { status: "ready", absent }
    : { status: "present", present, absent };
};

export const resolveT2ForeignEventEvidence = ({
  foreignHeaderHash,
  header,
  candidateIds,
  payload,
  payloadError,
}: {
  readonly foreignHeaderHash: string;
  readonly header: SDK.Header;
  readonly candidateIds: T2CandidateEventIds;
  readonly payload?: SDK.DaPayload;
  readonly payloadError?: string;
}): Effect.Effect<T2ForeignEventResolution> =>
  Effect.gen(function* () {
    const boundHeaderHash = yield* SDK.hashBlockHeader(header).pipe(
      Effect.catchAll(() => Effect.succeed("")),
    );
    if (boundHeaderHash !== foreignHeaderHash) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "invalid",
        detail: "foreign header hash binding is invalid",
        present: emptyIds(),
      };
    }
    // Resolve the whole retained foreign evidence window, not only the IDs
    // visible on this pass. A non-empty category needs canonical DA even when
    // no local candidate is visible yet, otherwise a late-indexed event could
    // arrive after the marker was incorrectly declared reusable.
    const needsPayload =
      header.depositsRoot !== SDK.EMPTY_MERKLE_TREE_ROOT ||
      header.forcedTransactionsRoot !== SDK.EMPTY_MERKLE_TREE_ROOT ||
      header.withdrawalsRoot !== SDK.EMPTY_MERKLE_TREE_ROOT;
    if (needsPayload && payloadError !== undefined) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "invalid",
        detail: payloadError,
        present: emptyIds(),
      };
    }
    if (needsPayload && payload === undefined) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "missing",
        detail: "verified foreign DA payload is not locally available",
        present: emptyIds(),
      };
    }
    if (
      payload !== undefined &&
      !(yield* verifyForeignPayload({ foreignHeaderHash, header, payload }))
    ) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "invalid",
        detail: "foreign DA payload failed header, root, or count verification",
        present: emptyIds(),
      };
    }
    const deposits = classifyCategory({
      candidateIds: candidateIds.deposits,
      root: header.depositsRoot,
      count: header.depositCount,
      payloadEntries: payload?.block_body.deposits,
    });
    const forcedTransactions = classifyCategory({
      candidateIds: candidateIds.forcedTransactions,
      root: header.forcedTransactionsRoot,
      count: header.forcedTransactionCount,
      payloadEntries: payload?.block_body.forced_transactions,
    });
    const withdrawals = classifyCategory({
      candidateIds: candidateIds.withdrawals,
      root: header.withdrawalsRoot,
      count: header.withdrawalCount,
      payloadEntries: payload?.block_body.withdrawals,
    });
    const classifications = { deposits, forcedTransactions, withdrawals };
    if (
      Object.values(classifications).some(
        (classification) => classification.status === "invalid",
      )
    ) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "invalid",
        detail: "foreign header event root/count evidence is inconsistent",
        present: emptyIds(),
      };
    }
    if (
      Object.values(classifications).some(
        (classification) => classification.status === "missing",
      )
    ) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "missing",
        detail: "foreign event membership requires verified DA",
        present: emptyIds(),
      };
    }
    const present: T2CandidateEventIds = {
      deposits: deposits.status === "present" ? deposits.present : [],
      forcedTransactions:
        forcedTransactions.status === "present"
          ? forcedTransactions.present
          : [],
      withdrawals: withdrawals.status === "present" ? withdrawals.present : [],
    };
    if (Object.values(present).some((ids) => ids.length > 0)) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "foreign_event_present_requires_finalization",
        detail:
          "foreign DA proves candidate events were included, but foreign-finalization is not yet available",
        present,
      };
    }
    return {
      type: "Ready",
      absent: {
        deposits: deposits.status === "ready" ? deposits.absent : [],
        forcedTransactions:
          forcedTransactions.status === "ready"
            ? forcedTransactions.absent
            : [],
        withdrawals: withdrawals.status === "ready" ? withdrawals.absent : [],
      },
    };
  });

const decodeStoredPayload = ({
  payloadCbor,
  schemaVersion,
}: {
  readonly payloadCbor: Buffer;
  readonly schemaVersion: number;
}): Promise<SDK.DaPayload> =>
  schemaVersion !== Number(SDK.DA_PAYLOAD_VERSION)
    ? Promise.reject(
        new Error("Stored DA payload schema version must equal canonical V1"),
      )
    : unwrapDaPayload(payloadCbor, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      }).then((unwrapped) => SDK.decodeDaPayload(unwrapped.innerBytes));

export const reconcileOverdueAwaitingEventsAgainstForeignTip = ({
  foreignHeaderHash,
  header,
}: {
  readonly foreignHeaderHash: string;
  readonly header: SDK.Header;
}): Effect.Effect<
  T2ForeignEventResolution,
  DatabaseError,
  Database | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const suppliedHash = yield* SDK.hashBlockHeader(header).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table: ForeignTipReconciliationsDB.tableName,
            message: "Failed to authenticate supplied foreign header",
            cause,
          }),
      ),
    );
    if (suppliedHash !== foreignHeaderHash) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message: "Supplied foreign header does not match reconciliation key",
          cause: `provided=${foreignHeaderHash},recomputed=${suppliedHash}`,
        }),
      );
    }
    const reconciliation =
      yield* ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
        foreignHeaderHash,
      );
    return Option.isNone(reconciliation)
      ? ({ type: "Ready", absent: emptyIds() } as const)
      : yield* reconcileRetainedForeignTipEntry(reconciliation.value);
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof DatabaseError
        ? cause
        : new DatabaseError({
            table: "t2_foreign_event_reconciliation",
            message: "Failed to reconcile overdue events against foreign tip",
            cause,
          }),
    ),
  );

const decodeRetainedHeader = (
  entry: ForeignTipReconciliationsDB.Entry,
): Effect.Effect<SDK.Header, DatabaseError> =>
  Effect.gen(function* () {
    const header = yield* Effect.try({
      try: () =>
        LucidData.from(
          entry[
            ForeignTipReconciliationsDB.Columns.FOREIGN_HEADER_CBOR
          ].toString("hex"),
          SDK.Header as never,
        ) as SDK.Header,
      catch: (cause) =>
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message: "Retained foreign header CBOR is invalid",
          cause,
        }),
    });
    const foreignHeaderHash =
      entry[ForeignTipReconciliationsDB.Columns.FOREIGN_HEADER_HASH].toString(
        "hex",
      );
    const recomputedHash = yield* SDK.hashBlockHeader(header).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table: ForeignTipReconciliationsDB.tableName,
            message: "Failed to hash retained foreign header",
            cause,
          }),
      ),
    );
    const explicitEvidenceMatches =
      recomputedHash === foreignHeaderHash &&
      Number(header.startTime) ===
        entry[ForeignTipReconciliationsDB.Columns.BLOCK_START_TIME].getTime() &&
      Number(header.endTime) ===
        entry[ForeignTipReconciliationsDB.Columns.BLOCK_END_TIME].getTime() &&
      header.depositsRoot ===
        entry[ForeignTipReconciliationsDB.Columns.DEPOSITS_ROOT] &&
      header.forcedTransactionsRoot ===
        entry[ForeignTipReconciliationsDB.Columns.FORCED_TRANSACTIONS_ROOT] &&
      header.withdrawalsRoot ===
        entry[ForeignTipReconciliationsDB.Columns.WITHDRAWALS_ROOT] &&
      header.depositCount ===
        BigInt(entry[ForeignTipReconciliationsDB.Columns.DEPOSIT_COUNT]) &&
      header.forcedTransactionCount ===
        BigInt(
          entry[ForeignTipReconciliationsDB.Columns.FORCED_TRANSACTION_COUNT],
        ) &&
      header.withdrawalCount ===
        BigInt(entry[ForeignTipReconciliationsDB.Columns.WITHDRAWAL_COUNT]);
    if (!explicitEvidenceMatches) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message:
            "Retained foreign header does not match its immutable evidence columns",
          cause: foreignHeaderHash,
        }),
      );
    }
    return header;
  });

const reconcileRetainedForeignTipEntry = (
  initialEntry: ForeignTipReconciliationsDB.Entry,
): Effect.Effect<
  T2ForeignEventResolution,
  DatabaseError,
  Database | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const foreignHeaderHash =
          initialEntry[
            ForeignTipReconciliationsDB.Columns.FOREIGN_HEADER_HASH
          ].toString("hex");
        const currentEntry =
          yield* ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
            foreignHeaderHash,
          );
        if (Option.isNone(currentEntry)) {
          return { type: "Ready", absent: emptyIds() } as const;
        }
        const entry = currentEntry.value;
        const reconciliation =
          ForeignTipReconciliationsDB.decodeForeignTipReconciliation(entry);
        const deploymentIdentity = yield* ContractDeploymentIdentity;
        if (deploymentIdentity.deploymentMarker === undefined) {
          return yield* Effect.fail(
            new DatabaseError({
              table: ForeignTipReconciliationsDB.tableName,
              message:
                "Foreign-tip recovery requires the exact active deployment marker",
              cause: "missing_deployment_marker",
            }),
          );
        }
        yield* Effect.try({
          try: () =>
            assertDeploymentMarkerMatches(
              reconciliation.deploymentMarker,
              deploymentIdentity.deploymentMarker,
              "ForeignTipReconciliationV1 recovery",
            ),
          catch: (cause) =>
            new DatabaseError({
              table: ForeignTipReconciliationsDB.tableName,
              message:
                "Foreign-tip reconciliation belongs to a different deployment",
              cause,
            }),
        });
        const header = yield* decodeRetainedHeader(entry);
        const startTime =
          entry[ForeignTipReconciliationsDB.Columns.BLOCK_START_TIME];
        const endTime =
          entry[ForeignTipReconciliationsDB.Columns.BLOCK_END_TIME];
        const [deposits, forcedTransactions, withdrawals] = yield* Effect.all(
          [
            DepositsDB.retrievePendingHeaderEntriesUpTo(endTime),
            ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(endTime),
            WithdrawalsDB.retrievePendingHeaderEntriesUpTo(endTime),
          ],
          { concurrency: 1 },
        );
        const candidateDeposits = deposits.filter(
          (candidate) =>
            candidate[DepositsDB.Columns.STATUS] ===
              DepositsDB.Status.Awaiting &&
            candidate[DepositsDB.Columns.INCLUSION_TIME].getTime() >
              startTime.getTime(),
        );
        const candidateForcedTransactions = forcedTransactions.filter(
          (candidate) =>
            candidate[ForcedTransactionsDB.Columns.STATUS] ===
              ForcedTransactionsDB.Status.Awaiting &&
            candidate[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime() >
              startTime.getTime(),
        );
        const candidateWithdrawals = withdrawals.filter(
          (candidate) =>
            candidate[WithdrawalsDB.Columns.STATUS] ===
              WithdrawalsDB.Status.Awaiting &&
            candidate[WithdrawalsDB.Columns.INCLUSION_TIME].getTime() >
              startTime.getTime(),
        );
        const candidateIds: T2CandidateEventIds = {
          deposits: candidateDeposits.map((candidate) =>
            candidate[DepositsDB.Columns.ID].toString("hex"),
          ),
          forcedTransactions: candidateForcedTransactions.map((candidate) =>
            candidate[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
          ),
          withdrawals: candidateWithdrawals.map((candidate) =>
            candidate[WithdrawalsDB.Columns.ID].toString("hex"),
          ),
        };

        const retainedPayloadCbor =
          entry[ForeignTipReconciliationsDB.Columns.VERIFIED_DA_PAYLOAD_CBOR];
        const retainedSchemaVersion =
          entry[ForeignTipReconciliationsDB.Columns.VERIFIED_DA_SCHEMA_VERSION];
        const retainedPayloadSha256 =
          entry[ForeignTipReconciliationsDB.Columns.VERIFIED_DA_PAYLOAD_SHA256];
        const availablePayload =
          retainedPayloadCbor !== null &&
          retainedSchemaVersion !== null &&
          retainedPayloadSha256 !== null
            ? {
                headerHash:
                  entry[
                    ForeignTipReconciliationsDB.Columns.FOREIGN_HEADER_HASH
                  ],
                consensusProfileId:
                  entry[
                    ForeignTipReconciliationsDB.Columns.CONSENSUS_PROFILE_ID
                  ],
                payloadCbor: retainedPayloadCbor,
                schemaVersion: retainedSchemaVersion,
                payloadSha256: retainedPayloadSha256,
              }
            : Option.getOrUndefined(
                yield* DaPayloadsDB.retrieveByHeaderHash(
                  Buffer.from(foreignHeaderHash, "hex"),
                ),
              );
        const daIdentityCandidate =
          availablePayload === undefined
            ? undefined
            : "payloadCbor" in availablePayload
              ? availablePayload
              : {
                  headerHash:
                    availablePayload[DaPayloadsDB.Columns.HEADER_HASH],
                  schemaVersion: availablePayload[DaPayloadsDB.Columns.VERSION],
                  consensusProfileId:
                    availablePayload[DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID],
                  payloadCbor:
                    availablePayload[DaPayloadsDB.Columns.PAYLOAD_CBOR],
                  payloadSha256:
                    availablePayload[DaPayloadsDB.Columns.PAYLOAD_SHA256],
                };
        let authenticatedDa:
          | ForeignTipReconciliationsDB.ForeignTipDaIdentity
          | undefined;
        let payload: SDK.DaPayload | undefined;
        let payloadError: string | undefined;
        if (daIdentityCandidate !== undefined) {
          const authenticated = yield* Effect.either(
            Effect.try({
              try: () =>
                ForeignTipReconciliationsDB.authenticateForeignTipDaEvidence({
                  reconciliation,
                  deploymentMarker: deploymentIdentity.deploymentMarker,
                  evidence: daIdentityCandidate,
                }),
              catch: (cause) => cause,
            }),
          );
          if (authenticated._tag === "Left") {
            payloadError = String(authenticated.left);
          } else {
            authenticatedDa = authenticated.right;
          }
        }
        if (authenticatedDa !== undefined) {
          const decoded = yield* Effect.either(
            Effect.tryPromise({
              try: () =>
                decodeStoredPayload({
                  payloadCbor: authenticatedDa.payloadCbor,
                  schemaVersion: authenticatedDa.schemaVersion,
                }),
              catch: (cause) => cause,
            }),
          );
          if (decoded._tag === "Right") payload = decoded.right;
          else payloadError = String(decoded.left);
        }
        const resolution = yield* resolveT2ForeignEventEvidence({
          foreignHeaderHash,
          header,
          candidateIds,
          payload,
          payloadError,
        });
        if (resolution.type === "AwaitingForeignDa") {
          yield* ForeignTipReconciliationsDB.markAwaiting({
            foreignHeaderHash,
            reason: `${resolution.reason}:${resolution.detail}`,
          });
          return resolution;
        }

        if (candidateDeposits.length > 0) {
          const mempoolEntries = yield* Effect.forEach(
            candidateDeposits,
            DepositsDB.toMempoolLedgerEntry,
          );
          yield* MempoolLedgerDB.reconcileDepositEntries(mempoolEntries);
          yield* DepositsDB.markAwaitingAsProjected(
            candidateDeposits.map(
              (candidate) => candidate[DepositsDB.Columns.ID],
            ),
          );
        }
        if (candidateForcedTransactions.length > 0) {
          yield* ForcedTransactionsDB.markAwaitingAsProjected(
            candidateForcedTransactions.map(
              (candidate) =>
                candidate[ForcedTransactionsDB.Columns.TX_ORDER_ID],
            ),
          );
        }
        if (candidateWithdrawals.length > 0) {
          yield* WithdrawalsDB.markAwaitingAsProjected(
            candidateWithdrawals.map(
              (candidate) => candidate[WithdrawalsDB.Columns.ID],
            ),
          );
        }
        const requiresDaEvidence =
          header.depositsRoot !== SDK.EMPTY_MERKLE_TREE_ROOT ||
          header.forcedTransactionsRoot !== SDK.EMPTY_MERKLE_TREE_ROOT ||
          header.withdrawalsRoot !== SDK.EMPTY_MERKLE_TREE_ROOT;
        const resolvedEvidence: ForeignTipReconciliationsDB.ResolveForeignTipEvidence =
          !requiresDaEvidence
            ? {
                kind: ForeignTipReconciliationsDB.EvidenceKind.VerifiedEmpty,
              }
            : authenticatedDa !== undefined && payload !== undefined
              ? {
                  kind: ForeignTipReconciliationsDB.EvidenceKind.VerifiedDa,
                  daIdentity: authenticatedDa,
                }
              : yield* Effect.fail(
                  new DatabaseError({
                    table: ForeignTipReconciliationsDB.tableName,
                    message:
                      "Foreign-tip resolution lost its authenticated DA identity",
                    cause: foreignHeaderHash,
                  }),
                );
        yield* ForeignTipReconciliationsDB.markResolved({
          foreignHeaderHash,
          deploymentMarker: deploymentIdentity.deploymentMarker,
          evidence: resolvedEvidence,
        });
        return resolution;
      }),
    );
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof DatabaseError
        ? cause
        : new DatabaseError({
            table: ForeignTipReconciliationsDB.tableName,
            message: "Failed to reconcile retained foreign-tip evidence",
            cause,
          }),
    ),
  );

const resolvedWindowHasAwaitingEvents = (
  entry: ForeignTipReconciliationsDB.Entry,
): Effect.Effect<boolean, unknown, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const startTime =
      entry[ForeignTipReconciliationsDB.Columns.BLOCK_START_TIME];
    const endTime = entry[ForeignTipReconciliationsDB.Columns.BLOCK_END_TIME];
    const rows = yield* sql<{ readonly actionable: boolean }>`
      SELECT (
        EXISTS (
          SELECT 1 FROM ${sql(DepositsDB.tableName)}
          WHERE ${sql(DepositsDB.Columns.STATUS)} = ${DepositsDB.Status.Awaiting}
            AND ${sql(DepositsDB.Columns.PROJECTED_HEADER_HASH)} IS NULL
            AND ${sql(DepositsDB.Columns.INCLUSION_TIME)} > ${startTime}
            AND ${sql(DepositsDB.Columns.INCLUSION_TIME)} <= ${endTime}
        )
        OR EXISTS (
          SELECT 1 FROM ${sql(ForcedTransactionsDB.tableName)}
          WHERE ${sql(ForcedTransactionsDB.Columns.STATUS)} = ${ForcedTransactionsDB.Status.Awaiting}
            AND ${sql(ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH)} IS NULL
            AND ${sql(ForcedTransactionsDB.Columns.INCLUSION_TIME)} > ${startTime}
            AND ${sql(ForcedTransactionsDB.Columns.INCLUSION_TIME)} <= ${endTime}
        )
        OR EXISTS (
          SELECT 1 FROM ${sql(WithdrawalsDB.tableName)}
          WHERE ${sql(WithdrawalsDB.Columns.STATUS)} = ${WithdrawalsDB.Status.Awaiting}
            AND ${sql(WithdrawalsDB.Columns.PROJECTED_HEADER_HASH)} IS NULL
            AND ${sql(WithdrawalsDB.Columns.INCLUSION_TIME)} > ${startTime}
            AND ${sql(WithdrawalsDB.Columns.INCLUSION_TIME)} <= ${endTime}
        )
      ) AS actionable
    `;
    return rows[0]?.actionable === true;
  });

/**
 * Replays every retained foreign window, including resolved evidence, so
 * indexer-late Awaiting rows remain recoverable after restart or after the
 * live state-queue tip advances beyond the foreign header.
 */
export const reconcileOverdueAwaitingEventsAgainstRetainedForeignTips =
  (): Effect.Effect<
    T2ForeignEventResolution,
    DatabaseError,
    Database | ContractDeploymentIdentity
  > =>
    Effect.gen(function* () {
      const history =
        yield* ForeignTipReconciliationsDB.retrieveEvidenceHistory;
      const combinedAbsent = {
        deposits: [] as string[],
        forcedTransactions: [] as string[],
        withdrawals: [] as string[],
      };
      let firstAwaiting: T2ForeignEventResolution | undefined;
      for (const entry of history) {
        if (
          entry[ForeignTipReconciliationsDB.Columns.STATUS] ===
            ForeignTipReconciliationsDB.Status.Resolved &&
          !(yield* resolvedWindowHasAwaitingEvents(entry))
        ) {
          continue;
        }
        const resolution = yield* reconcileRetainedForeignTipEntry(entry);
        if (resolution.type === "AwaitingForeignDa") {
          firstAwaiting ??= resolution;
        } else {
          combinedAbsent.deposits.push(...resolution.absent.deposits);
          combinedAbsent.forcedTransactions.push(
            ...resolution.absent.forcedTransactions,
          );
          combinedAbsent.withdrawals.push(...resolution.absent.withdrawals);
        }
      }
      return (
        firstAwaiting ??
        ({
          type: "Ready",
          absent: {
            deposits: normalizedIds(combinedAbsent.deposits),
            forcedTransactions: normalizedIds(
              combinedAbsent.forcedTransactions,
            ),
            withdrawals: normalizedIds(combinedAbsent.withdrawals),
          },
        } as const)
      );
    }).pipe(
      Effect.mapError((cause) =>
        cause instanceof DatabaseError
          ? cause
          : new DatabaseError({
              table: "t2_foreign_event_reconciliation",
              message:
                "Failed to reconcile overdue events against retained foreign tips",
              cause,
            }),
      ),
    );
