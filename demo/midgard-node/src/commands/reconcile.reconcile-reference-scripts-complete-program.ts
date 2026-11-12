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
import { loadDeploymentRunState } from "../e2e/run-state.js";
import { Database, Lucid, MidgardContracts } from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import { readConfiguredDeploymentManifest } from "../services/midgard-contracts.load-reference-script-auth-validator.js";
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

/**
 * The node-runtime publication before the deployment records an auth policy.
 * Publication records its policy before it sends anything, so nothing was
 * published yet, and no contracts can be derived to look with: it is
 * reported blocked, not started. `null` once a policy is recorded, in the run
 * state or a configured deployment manifest.
 */
export const reconcileReferenceScriptsBeforeAuthPolicy = async ({
  runStatePath,
  address,
}: {
  readonly runStatePath: string;
  readonly address: string;
}): Promise<ReconciliationResult | null> => {
  if (readConfiguredDeploymentManifest() !== undefined) return null;
  const state = await loadDeploymentRunState(runStatePath);
  if (state?.identity.referenceScriptAuthPolicy !== undefined) return null;
  return result({
    milestone: "reference-scripts-complete",
    target: { scope: "node-runtime", address, authPolicyId: null },
    status: "blocked",
    evidence: [
      evidence("reference_script_auth_policy", {
        recorded: false,
        runStatePath,
      }),
    ],
    nextAction:
      "No reference-script auth policy is recorded yet, so nothing was published; run deploy-reference-script-node-runtime to create one and publish.",
  });
};

export const reconcileReferenceScriptsCompleteProgram = ({
  repair,
}: {
  readonly repair: boolean;
}): Effect.Effect<
  ReconciliationResult,
  unknown,
  Lucid | MidgardContracts | IntentJournal
> =>
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
 * itself runs only in the running node's follower-change driver, which
 * projects every due deposit; a standalone CLI process is no driver, so
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
        "No matching deposit row is visible. Do not resubmit until the admission tx hash has been checked with `midgard-user status` or proven absent.",
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
    const live = yield* PendingBlockFinalizationsDB.retrieveLive();
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
      evidence: [
        evidence("tx_status", status),
        optionRecordEvidence(
          live,
          Option.isSome(live)
            ? PendingBlockFinalizationsDB.OwnBlockStatus.Unlanded
            : undefined,
        ),
      ],
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
