import { assertDeploymentMarkerMatches } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
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
import { withHistoryWrite } from "../services/event-history-producer.js";
import { ContractDeploymentIdentity, Database } from "../services/index.js";
import { decodeRetainedHeader } from "./t2-foreign-event-reconciliation.decode-retained-header.js";
import {
  decodeStoredPayload,
  emptyIds,
  resolveT2ForeignEventEvidence,
  type T2CandidateEventIds,
  type T2ForeignEventResolution,
} from "./t2-foreign-event-reconciliation.resolve-t2-foreign-event-evidence.js";

/** How the `markAwaiting` call below stores an `invalid` verdict. */
const STORED_INVALID_PREFIX = "invalid:";

export const reconcileRetainedForeignTipEntry = (
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
        // A stored `invalid` is positive evidence against the block, so it
        // stays until a payload held this pass verifies against the header (a
        // held payload that fails yields a fresh `invalid`). A pass that only
        // lost the failing payload must not weaken it to `missing` or `Ready`.
        const storedReason =
          entry[ForeignTipReconciliationsDB.Columns.BLOCKING_REASON];
        const storedInvalidDetail = storedReason?.startsWith(
          STORED_INVALID_PREFIX,
        )
          ? storedReason.slice(STORED_INVALID_PREFIX.length)
          : undefined;
        if (
          storedInvalidDetail !== undefined &&
          payload === undefined &&
          !(
            resolution.type === "AwaitingForeignDa" &&
            resolution.reason === "invalid"
          )
        ) {
          return {
            type: "AwaitingForeignDa",
            foreignHeaderHash,
            reason: "invalid",
            detail: storedInvalidDetail,
            present: emptyIds(),
          } as const;
        }
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
            candidateWithdrawals.map((candidate) => ({
              eventId: candidate[WithdrawalsDB.Columns.ID],
              expectedClassificationRevision:
                candidate[WithdrawalsDB.Columns.CLASSIFICATION_REVISION],
            })),
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
    withHistoryWrite,
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
