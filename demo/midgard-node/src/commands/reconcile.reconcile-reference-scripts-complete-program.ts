import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import {
  DepositsDB,
  ImmutableDB,
  MempoolDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  TxAdmissionsDB,
  TxRejectionsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { Database, Lucid, MidgardContracts } from "../services/index.js";
import {
  ensureNodeRuntimeReferenceScriptsProgram,
  verifyNodeRuntimeReferenceScriptsProgram,
} from "../transactions/reference-scripts.js";
import {
  bufferHex,
  evidence,
  optionRecordEvidence,
  type ReconciliationEvidence,
  type ReconciliationResult,
  type ReconciliationStatus,
  result,
} from "./reconcile.parse-reconciliation-result.js";
import { resolveTxStatus } from "./tx-status.js";

export const reconcileReferenceScriptsCompleteProgram = ({
  repair,
}: {
  readonly repair: boolean;
}): Effect.Effect<ReconciliationResult, unknown, Lucid | MidgardContracts> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const verified = yield* Effect.either(
      verifyNodeRuntimeReferenceScriptsProgram(
        lucid.api,
        lucid.referenceScriptsAddress,
        contracts,
        contracts.referenceScriptAuth,
      ),
    );
    if (verified._tag === "Right") {
      return result({
        milestone: "reference-scripts-complete",
        target: {
          scope: "node-runtime",
          address: lucid.referenceScriptsAddress,
          authPolicyId: contracts.referenceScriptAuth.policyId,
        },
        status: "satisfied",
        evidence: [
          evidence("reference_scripts", {
            count: verified.right.length,
            outRefs: verified.right.map(
              (resolved) =>
                `${resolved.utxo.txHash}#${resolved.utxo.outputIndex.toString()}`,
            ),
          }),
        ],
      });
    }
    if (!repair) {
      return result({
        milestone: "reference-scripts-complete",
        target: {
          scope: "node-runtime",
          address: lucid.referenceScriptsAddress,
          authPolicyId: contracts.referenceScriptAuth.policyId,
        },
        status: "blocked",
        evidence: [
          evidence("reference_script_verification_error", {
            error: formatUnknownError(verified.left, { includeCause: true }),
          }),
        ],
        nextAction:
          "Run with --repair to publish only missing node-runtime reference scripts under the configured auth policy.",
      });
    }
    const repaired = yield* ensureNodeRuntimeReferenceScriptsProgram(
      lucid.referenceScriptsApi,
      contracts,
      contracts.referenceScriptAuth,
      lucid.api,
      lucid.referenceScriptsAddress,
    );
    return result({
      milestone: "reference-scripts-complete",
      target: {
        scope: "node-runtime",
        address: lucid.referenceScriptsAddress,
        authPolicyId: contracts.referenceScriptAuth.policyId,
      },
      status: "repaired",
      evidence: [
        evidence("reference_scripts", {
          count: repaired.length,
          outRefs: repaired.map(
            (resolved) =>
              `${resolved.utxo.txHash}#${resolved.utxo.outputIndex.toString()}`,
          ),
        }),
      ],
      repairActions: ["ensure_node_runtime_reference_scripts"],
    });
  });

const lookupDepositRows = ({
  eventId,
  cardanoTxHash,
}: {
  readonly eventId?: Buffer;
  readonly cardanoTxHash?: Buffer;
}): Effect.Effect<readonly DepositsDB.Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (eventId !== undefined) {
      const byEventId = yield* DepositsDB.retrieveByEventId(eventId);
      if (Option.isNone(byEventId)) {
        return [];
      }
      if (
        cardanoTxHash !== undefined &&
        !byEventId.value[DepositsDB.Columns.DEPOSIT_L1_TX_HASH].equals(
          cardanoTxHash,
        )
      ) {
        return [];
      }
      return [byEventId.value];
    }
    if (cardanoTxHash !== undefined) {
      return yield* DepositsDB.retrieveByCardanoTxHash(cardanoTxHash);
    }
    return [];
  });

const serializeDepositEvidence = (
  rows: readonly DepositsDB.Entry[],
): readonly ReconciliationEvidence[] =>
  rows.map((row) =>
    evidence("deposit_row", {
      eventId: row[DepositsDB.Columns.ID].toString("hex"),
      cardanoTxHash: row[DepositsDB.Columns.DEPOSIT_L1_TX_HASH].toString("hex"),
      status: row[DepositsDB.Columns.STATUS],
      inclusionTime: row[DepositsDB.Columns.INCLUSION_TIME].toISOString(),
      projectedHeaderHash: bufferHex(
        row[DepositsDB.Columns.PROJECTED_HEADER_HASH],
      ),
      ledgerAddress: row[DepositsDB.Columns.LEDGER_ADDRESS],
    }),
  );

/**
 * Read-only: reports whether the deposit is visible and projected. Projection
 * itself runs only under the node's history owner, which projects every due
 * deposit; a standalone CLI process holds no history-ingestion permit, so
 * there is no repair action here.
 */
export const reconcileDepositProjectedProgram = ({
  eventId,
  cardanoTxHash,
}: {
  readonly eventId?: Buffer;
  readonly cardanoTxHash?: Buffer;
}): Effect.Effect<ReconciliationResult, DatabaseError, Database> =>
  Effect.gen(function* () {
    const target = {
      ...(eventId === undefined ? {} : { eventId: eventId.toString("hex") }),
      ...(cardanoTxHash === undefined
        ? {}
        : { cardanoTxHash: cardanoTxHash.toString("hex") }),
    };
    const rows = yield* lookupDepositRows({ eventId, cardanoTxHash });

    const depositEvidence = serializeDepositEvidence(rows);
    if (
      rows.some(
        (row) =>
          row[DepositsDB.Columns.STATUS] === DepositsDB.Status.Projected ||
          row[DepositsDB.Columns.STATUS] === DepositsDB.Status.Consumed,
      )
    ) {
      return result({
        milestone: "deposit-projected",
        target,
        status: "satisfied",
        evidence: depositEvidence,
      });
    }

    if (rows.length > 0) {
      return result({
        milestone: "deposit-projected",
        target,
        status: "pending",
        safeToRetryOriginalStep: false,
        evidence: depositEvidence,
        nextAction:
          "Deposit is visible but not projected yet; the running node projects it once its inclusion time is due.",
      });
    }

    return result({
      milestone: "deposit-projected",
      target,
      status: "ambiguous",
      safeToRetryOriginalStep: false,
      evidence: depositEvidence,
      nextAction:
        "No matching deposit row is visible. Do not resubmit until the Cardano tx hash has been reconciled (reconcile-deposit-submission) or proven absent.",
    });
  });

export const reconcileTxCommittedProgram = ({
  txHash,
}: {
  readonly txHash: Buffer;
}): Effect.Effect<ReconciliationResult, DatabaseError, Database> =>
  Effect.gen(function* () {
    const rejected = yield* TxRejectionsDB.retrieveByTxId(txHash);
    const admission = yield* TxAdmissionsDB.getByTxId(txHash);
    const inImmutable = yield* ImmutableDB.retrieveTxCborsByHashes([txHash]);
    const inMempool = yield* MempoolDB.retrieveTxCborsByHashes([txHash]);
    const inProcessedMempool =
      yield* ProcessedMempoolDB.retrieveTxCborsByHashes([txHash]);
    const active = yield* PendingBlockFinalizationsDB.retrieveActive();
    const status = resolveTxStatus({
      txIdHex: txHash.toString("hex"),
      rejection:
        rejected.length > 0
          ? {
              rejectCode: rejected[0]!.reject_code,
              rejectDetail: rejected[0]!.reject_detail,
              createdAtIso: rejected[0]!.created_at.toISOString(),
            }
          : null,
      admissionStatus: admission?.status ?? null,
      inImmutable: inImmutable.length > 0,
      inMempool: inMempool.length > 0,
      inProcessedMempool: inProcessedMempool.length > 0,
      localFinalizationPending: Option.isSome(active),
    });

    const milestoneStatus: ReconciliationStatus =
      status.status === "committed"
        ? "satisfied"
        : status.status === "rejected"
          ? "failed"
          : status.status === "not_found"
            ? "ambiguous"
            : "pending";
    return result({
      milestone: "tx-committed",
      target: { txHash: txHash.toString("hex") },
      status: milestoneStatus,
      safeToRetryOriginalStep: false,
      evidence: [evidence("tx_status", status), optionRecordEvidence(active)],
      nextAction:
        milestoneStatus === "pending"
          ? "Wait for tx processing/commit workers or inspect readiness and pending finalization state."
          : milestoneStatus === "ambiguous"
            ? "The node has no local evidence for this tx id; verify the submit response before retrying."
            : null,
    });
  });

export type CanonicalDaAttestationObservation = {
  readonly datumHeaderHash: string;
  readonly computedHeaderHash: string;
  readonly daAvailability: SDK.DaAvailabilityStateQueueStatus;
  readonly outRef: string;
};

export type CanonicalDaAttestationDecision = {
  readonly status: ReconciliationStatus;
  readonly reason:
    | "attestation_applied"
    | "attestation_pending"
    | "canonical_header_absent"
    | "canonical_header_not_unique"
    | "header_hash_mismatch"
    | "local_payload_missing";
  readonly nextAction: string | null;
};
