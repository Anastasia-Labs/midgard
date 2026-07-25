import { randomUUID } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import { resolveTxStatus } from "@/commands/tx-status.js";
import {
  BlocksDB,
  DaPayloadsDB,
  DepositsDB,
  ImmutableDB,
  MempoolDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
  TxRejectionsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { reconcileVisibleDepositUTxOs } from "@/fibers/fetch-and-insert-deposit-utxos.js";
import { mergeAction } from "@/fibers/merge.js";
import { projectDepositsToMempoolLedger } from "@/fibers/project-deposits-to-mempool-ledger.js";
import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import {
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import {
  ensurePhasMembershipRewardAccountRegisteredProgram,
  queryPhasMembershipRewardAccountRegisteredProgram,
} from "@/transactions/phas-membership-registration.js";
import {
  ensureNodeRuntimeReferenceScriptsProgram,
  verifyNodeRuntimeReferenceScriptsProgram,
} from "@/transactions/reference-scripts.js";
import {
  type DepositSubmissionReconciliationResult,
  reconcileDepositSubmissionAttemptProgram,
} from "@/transactions/submit-deposit.js";
import { runCommitBlockHeaderWorkerProgram } from "@/workers/commit-block-header.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "@/workers/commit-block-header/da-payload-backfill.js";
import {
  serializeStateQueueUTxO,
  type WorkerInput as CommitBlockWorkerInput,
} from "@/workers/utils/commit-block-header.js";

export const RECONCILIATION_SCHEMA_VERSION =
  "midgard-e2e-reconciliation-v1" as const;

export type ReconciliationStatus =
  | "satisfied"
  | "pending"
  | "repaired"
  | "blocked"
  | "ambiguous"
  | "failed";

export type ReconciliationEvidence = {
  readonly kind: string;
  readonly detail: Readonly<Record<string, unknown>>;
};

export type ReconciliationResult = {
  readonly schemaVersion: typeof RECONCILIATION_SCHEMA_VERSION;
  readonly milestone: string;
  readonly target: Readonly<Record<string, unknown>>;
  readonly status: ReconciliationStatus;
  readonly safeToRetryOriginalStep: boolean;
  readonly evidence: readonly ReconciliationEvidence[];
  readonly nextAction: string | null;
  readonly repairActions: readonly string[];
};

const evidence = (
  kind: string,
  detail: Readonly<Record<string, unknown>>,
): ReconciliationEvidence => ({ kind, detail });

type ReconciliationResultInput = Pick<
  ReconciliationResult,
  "milestone" | "target" | "status"
> &
  Partial<
    Pick<
      ReconciliationResult,
      "safeToRetryOriginalStep" | "evidence" | "nextAction" | "repairActions"
    >
  >;

const result = ({
  milestone,
  target,
  status,
  safeToRetryOriginalStep = false,
  evidence: evidenceEntries = [],
  nextAction = null,
  repairActions = [],
}: ReconciliationResultInput): ReconciliationResult => ({
  schemaVersion: RECONCILIATION_SCHEMA_VERSION,
  milestone,
  target,
  status,
  safeToRetryOriginalStep,
  evidence: evidenceEntries,
  nextAction,
  repairActions,
});

const bufferHex = (value: Buffer | null | undefined): string | null =>
  value === null || value === undefined ? null : value.toString("hex");

const optionRecordEvidence = (
  record: Option.Option<PendingBlockFinalizationsDB.Record>,
): ReconciliationEvidence =>
  evidence(
    "pending_block_finalization",
    Option.isNone(record)
      ? { present: false }
      : {
          present: true,
          headerHash:
            record.value[
              PendingBlockFinalizationsDB.Columns.HEADER_HASH
            ].toString("hex"),
          submittedTxHash: bufferHex(
            record.value[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH],
          ),
          status: record.value[PendingBlockFinalizationsDB.Columns.STATUS],
          depositEventIds: record.value.depositEventIds.map((id) =>
            id.toString("hex"),
          ),
          forcedTransactionEventIds: record.value.forcedTransactionEventIds.map(
            (id) => id.toString("hex"),
          ),
          withdrawalEventIds: record.value.withdrawalEventIds.map((id) =>
            id.toString("hex"),
          ),
          txIds: record.value.mempoolTxIds.map((id) => id.toString("hex")),
        },
  );

type CanonicalStateQueueHeader = {
  readonly headerHash: string;
  readonly outRef: string;
  readonly utxo: SDK.StateQueueUTxO;
};

const stateQueueOutRef = (utxo: SDK.StateQueueUTxO): string =>
  `${utxo.utxo.txHash}#${utxo.utxo.outputIndex.toString()}`;

const fetchCanonicalStateQueueHeaders = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const sorted = yield* SDK.fetchSortedStateQueueUTxOsProgram(lucid.api, {
    stateQueuePolicyId: contracts.stateQueue.policyId,
    stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
  });
  return sorted.flatMap((utxo): CanonicalStateQueueHeader[] =>
    utxo.datum.key === "Empty"
      ? []
      : [
          {
            headerHash: utxo.datum.key.Key.key,
            outRef: stateQueueOutRef(utxo),
            utxo,
          },
        ],
  );
});

const fetchCanonicalStateQueueHeaderHashes =
  fetchCanonicalStateQueueHeaders.pipe(
    Effect.map((headers) => headers.map((header) => header.headerHash)),
  );

const localFinalizationJobId = (headerHashHex: string): string =>
  `local_block_finalization:${headerHashHex}`;

const unfinishedLocalFinalizationJobEvidence = (
  headerHashHex: string,
  jobs: readonly MutationJobsDB.Entry[],
): ReconciliationEvidence => {
  const jobId = localFinalizationJobId(headerHashHex);
  const job = jobs.find(
    (entry) => entry[MutationJobsDB.Columns.JOB_ID] === jobId,
  );
  return evidence(
    "local_finalization_job",
    job === undefined
      ? { present: false, jobId }
      : {
          present: true,
          jobId,
          kind: job[MutationJobsDB.Columns.KIND],
          status: job[MutationJobsDB.Columns.STATUS],
          attempts: job[MutationJobsDB.Columns.ATTEMPTS],
          lastError: job[MutationJobsDB.Columns.LAST_ERROR],
          updatedAt: job[MutationJobsDB.Columns.UPDATED_AT].toISOString(),
        },
  );
};

export const reconcilePhasRegisteredProgram = ({
  repair,
}: {
  readonly repair: boolean;
}): Effect.Effect<ReconciliationResult, unknown, Lucid> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const identity = yield* Effect.try({
      try: () => {
        const network = lucid.api.config().network;
        if (network === undefined) {
          throw new Error("Lucid network is undefined");
        }
        return SDK.phasMembershipIdentity(
          network,
          loadPhasMembershipWithdrawalScript(),
        );
      },
      catch: (cause) =>
        new SDK.UnspecifiedNetworkError({
          message: "Failed to derive PHAS membership identity",
          cause,
        }),
    });

    const registeredAttempt = yield* Effect.either(
      queryPhasMembershipRewardAccountRegisteredProgram(
        lucid.api,
        identity.rewardAddress,
      ),
    );
    if (registeredAttempt._tag === "Left") {
      if (!repair) {
        return result({
          milestone: "phas-registered",
          target: {
            rewardAddress: identity.rewardAddress,
            scriptHash: identity.scriptHash,
          },
          status: "blocked",
          safeToRetryOriginalStep: false,
          evidence: [
            evidence("reward_account_registration_error", {
              error: formatUnknownError(registeredAttempt.left, {
                includeCause: true,
              }),
            }),
          ],
          nextAction:
            "Fix provider reward-account registration lookup or rerun with --repair only when submission is safe.",
        });
      }
      const repairAttempt = yield* Effect.either(
        ensurePhasMembershipRewardAccountRegisteredProgram(lucid.api),
      );
      if (repairAttempt._tag === "Left") {
        return result({
          milestone: "phas-registered",
          target: {
            rewardAddress: identity.rewardAddress,
            scriptHash: identity.scriptHash,
          },
          status: "failed",
          safeToRetryOriginalStep: false,
          evidence: [
            evidence("phas_registration_error", {
              error: formatUnknownError(repairAttempt.left, {
                includeCause: true,
              }),
            }),
          ],
          repairActions: ["register_phas_membership_reward_account"],
          nextAction:
            "Registration repair failed; inspect provider and transaction error before retrying.",
        });
      }
      return result({
        milestone: "phas-registered",
        target: {
          rewardAddress: repairAttempt.right.rewardAddress,
          scriptHash: repairAttempt.right.scriptHash,
        },
        status:
          repairAttempt.right.status === "already_registered"
            ? "satisfied"
            : "repaired",
        safeToRetryOriginalStep: true,
        evidence: [evidence("phas_registration_result", repairAttempt.right)],
        repairActions:
          repairAttempt.right.status === "already_registered"
            ? []
            : ["register_phas_membership_reward_account"],
      });
    }
    const registered = registeredAttempt.right;
    if (registered) {
      return result({
        milestone: "phas-registered",
        target: {
          rewardAddress: identity.rewardAddress,
          scriptHash: identity.scriptHash,
        },
        status: "satisfied",
        safeToRetryOriginalStep: true,
        evidence: [
          evidence("reward_account_registration", { registered: true }),
        ],
      });
    }
    if (!repair) {
      return result({
        milestone: "phas-registered",
        target: {
          rewardAddress: identity.rewardAddress,
          scriptHash: identity.scriptHash,
        },
        status: "pending",
        safeToRetryOriginalStep: true,
        evidence: [
          evidence("reward_account_registration", { registered: false }),
        ],
        nextAction:
          "Run this reconciler with --repair or rerun the idempotent PHAS registration step.",
      });
    }

    const repairedAttempt = yield* Effect.either(
      ensurePhasMembershipRewardAccountRegisteredProgram(lucid.api),
    );
    if (repairedAttempt._tag === "Left") {
      return result({
        milestone: "phas-registered",
        target: {
          rewardAddress: identity.rewardAddress,
          scriptHash: identity.scriptHash,
        },
        status: "failed",
        safeToRetryOriginalStep: false,
        evidence: [
          evidence("phas_registration_error", {
            error: formatUnknownError(repairedAttempt.left, {
              includeCause: true,
            }),
          }),
        ],
        repairActions: ["register_phas_membership_reward_account"],
        nextAction:
          "Registration repair failed; inspect provider and transaction error before retrying.",
      });
    }
    const repaired = repairedAttempt.right;
    return result({
      milestone: "phas-registered",
      target: {
        rewardAddress: repaired.rewardAddress,
        scriptHash: repaired.scriptHash,
      },
      status: "repaired",
      safeToRetryOriginalStep: true,
      evidence: [evidence("phas_registration_result", repaired)],
      repairActions: ["register_phas_membership_reward_account"],
    });
  });

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

export const reconcileDepositProjectedProgram = ({
  eventId,
  cardanoTxHash,
  repair,
}: {
  readonly eventId?: Buffer;
  readonly cardanoTxHash?: Buffer;
  readonly repair: boolean;
}): Effect.Effect<
  ReconciliationResult,
  unknown,
  Database | Globals | MidgardContracts | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const target = {
      ...(eventId === undefined ? {} : { eventId: eventId.toString("hex") }),
      ...(cardanoTxHash === undefined
        ? {}
        : { cardanoTxHash: cardanoTxHash.toString("hex") }),
    };
    let rows = yield* lookupDepositRows({ eventId, cardanoTxHash });
    const repairActions: string[] = [];

    if (rows.length === 0 && repair && cardanoTxHash !== undefined) {
      const reconciliation = yield* Effect.either(
        reconcileDepositSubmissionAttemptProgram(cardanoTxHash.toString("hex")),
      );
      repairActions.push("reconcile_deposit_submission_attempt");
      if (
        reconciliation._tag === "Right" &&
        reconciliation.right.status === "ambiguous"
      ) {
        return result({
          milestone: "deposit-projected",
          target,
          status: "ambiguous",
          evidence: [
            evidence("deposit_submission_reconciliation", {
              ...(reconciliation.right as DepositSubmissionReconciliationResult),
            }),
          ],
          repairActions,
          nextAction: reconciliation.right.nextSafeAction,
        });
      }
    }

    if (repair) {
      yield* reconcileVisibleDepositUTxOs();
      yield* projectDepositsToMempoolLedger;
      repairActions.push("reconcile_visible_deposit_utxos");
      repairActions.push("project_deposits_to_mempool_ledger");
      rows = yield* lookupDepositRows({ eventId, cardanoTxHash });
    }

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
        status: repairActions.length > 0 ? "repaired" : "satisfied",
        evidence: depositEvidence,
        repairActions,
      });
    }

    if (rows.length > 0) {
      return result({
        milestone: "deposit-projected",
        target,
        status: "pending",
        safeToRetryOriginalStep: false,
        evidence: depositEvidence,
        repairActions,
        nextAction:
          "Deposit is visible but not projected yet; wait for inclusion time or run with --repair after it is due.",
      });
    }

    return result({
      milestone: "deposit-projected",
      target,
      status: "ambiguous",
      safeToRetryOriginalStep: false,
      evidence: depositEvidence,
      repairActions,
      nextAction:
        "No matching deposit row is visible. Do not resubmit until the Cardano tx hash has been reconciled or proven absent.",
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
      safeToRetryOriginalStep: status.status === "not_found",
      evidence: [evidence("tx_status", status), optionRecordEvidence(active)],
      nextAction:
        milestoneStatus === "pending"
          ? "Wait for tx processing/commit workers or inspect readiness and pending finalization state."
          : milestoneStatus === "ambiguous"
            ? "The node has no local evidence for this tx id; verify the submit response before retrying."
            : null,
    });
  });

const readWatcherHeaderStatus = ({
  watcherUrl,
  deploymentFingerprint,
  headerHash,
}: {
  readonly watcherUrl: string;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
}): Effect.Effect<unknown, never> =>
  Effect.tryPromise({
    try: async () => {
      const url = `${watcherUrl.replace(/\/+$/, "")}/v1/deployments/${encodeURIComponent(
        deploymentFingerprint,
      )}/headers/${encodeURIComponent(headerHash)}/status`;
      const response = await fetch(url);
      const body = await response.text();
      if (!response.ok) {
        return {
          ok: false,
          status: response.status,
          body: body.slice(0, 1_000),
        };
      }
      return JSON.parse(body) as unknown;
    },
    catch: (cause) => ({
      ok: false,
      error: formatUnknownError(cause),
    }),
  }).pipe(Effect.catchAll((value) => Effect.succeed(value)));

const arrayField = (value: unknown, field: string): readonly unknown[] =>
  typeof value === "object" &&
  value !== null &&
  Array.isArray((value as Record<string, unknown>)[field])
    ? ((value as Record<string, unknown>)[field] as readonly unknown[])
    : [];

export const reconcileDaAttestedProgram = ({
  headerHash,
  watcherUrl,
  deploymentFingerprint,
  repair,
}: {
  readonly headerHash: Buffer;
  readonly watcherUrl?: string;
  readonly deploymentFingerprint?: string;
  readonly repair: boolean;
}): Effect.Effect<ReconciliationResult, DatabaseError, Database> =>
  Effect.gen(function* () {
    const headerHashHex = headerHash.toString("hex");
    let localPayload = yield* DaPayloadsDB.retrieveByHeaderHash(headerHash);
    const repairActions: string[] = [];
    let backfillSkipped: readonly { readonly reason: string }[] = [];
    if (Option.isNone(localPayload) && repair) {
      const backfill = yield* backfillMissingDaPayloadsFromFinalizedJournals({
        headerHash,
        limit: 1,
      });
      repairActions.push("backfill_missing_da_payload");
      backfillSkipped = backfill.skipped;
      if (backfill.backfilled.includes(headerHashHex)) {
        localPayload = yield* DaPayloadsDB.retrieveByHeaderHash(headerHash);
      }
    }

    const evidenceEntries: ReconciliationEvidence[] = [
      evidence("local_da_payload", {
        present: Option.isSome(localPayload),
      }),
    ];
    if (backfillSkipped.length > 0) {
      evidenceEntries.push(
        evidence("da_payload_backfill_skipped", {
          reasons: backfillSkipped.map((entry) => entry.reason),
        }),
      );
    }
    if (watcherUrl !== undefined && deploymentFingerprint !== undefined) {
      const watcher = yield* readWatcherHeaderStatus({
        watcherUrl,
        deploymentFingerprint,
        headerHash: headerHashHex,
      });
      const l1Submissions = arrayField(watcher, "l1Submissions");
      const candidates = arrayField(watcher, "attestationCandidates");
      const signatures = arrayField(watcher, "signatures");
      evidenceEntries.push(
        evidence("watcher_header_status", {
          l1SubmissionCount: l1Submissions.length,
          attestationCandidateCount: candidates.length,
          signatureCount: signatures.length,
          raw: watcher,
        }),
      );
      if (l1Submissions.length > 0) {
        return result({
          milestone: "da-attested",
          target: { headerHash: headerHashHex },
          status: "satisfied",
          evidence: evidenceEntries,
          repairActions,
        });
      }
      if (candidates.length > 0 || signatures.length > 0) {
        return result({
          milestone: "da-attested",
          target: { headerHash: headerHashHex },
          status: "pending",
          evidence: evidenceEntries,
          repairActions,
          nextAction:
            "Watcher has DA evidence but no L1 submission yet; wait for submitter reconciliation or run one copied DA watcher tick.",
        });
      }
    }

    return result({
      milestone: "da-attested",
      target: { headerHash: headerHashHex },
      status: Option.isSome(localPayload) ? "pending" : "blocked",
      evidence: evidenceEntries,
      repairActions,
      nextAction: backfillSkipped.some((entry) =>
        entry.reason.includes("journal excluded by status: abandoned"),
      )
        ? "Canonical journal is abandoned locally; revive it through confirmation recovery and complete local finalization before DA payload backfill."
        : watcherUrl === undefined || deploymentFingerprint === undefined
          ? "Provide --watcher-url and --contract-deployment-info to prove DA attestation; local payload presence alone is not DA submission."
          : "DA payload or watcher witness evidence is missing.",
    });
  });

export const reconcileBlockCommittedProgram = ({
  headerHash,
}: {
  readonly headerHash: Buffer;
}): Effect.Effect<
  ReconciliationResult,
  unknown,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const headerHashHex = headerHash.toString("hex");
    const journal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
    const canonicalHeaders = yield* fetchCanonicalStateQueueHeaderHashes;
    const canonical = canonicalHeaders.includes(headerHashHex);
    const journalStatus = Option.isSome(journal)
      ? journal.value[PendingBlockFinalizationsDB.Columns.STATUS]
      : null;
    const status: ReconciliationStatus =
      canonical ||
      journalStatus === PendingBlockFinalizationsDB.Status.Finalized
        ? "satisfied"
        : journalStatus === null
          ? "ambiguous"
          : "pending";
    return result({
      milestone: "block-committed",
      target: { headerHash: headerHashHex },
      status,
      evidence: [
        evidence("canonical_state_queue", {
          containsHeader: canonical,
          headers: canonicalHeaders,
        }),
        optionRecordEvidence(journal),
      ],
      nextAction:
        status === "pending"
          ? "Wait for block confirmation/local finalization worker or inspect state-queue lease."
          : status === "ambiguous"
            ? "No local journal or canonical state-queue evidence exists for this header."
            : null,
    });
  });

export const reconcileLocalFinalizationProgram = ({
  headerHash,
  repair,
}: {
  readonly headerHash: Buffer;
  readonly repair: boolean;
}): Effect.Effect<
  ReconciliationResult,
  unknown,
  | Database
  | Lucid
  | MidgardContracts
  | ContractDeploymentIdentity
  | NodeConfig
> =>
  Effect.gen(function* () {
    const headerHashHex = headerHash.toString("hex");
    const journal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
    let canonicalHeaders = yield* fetchCanonicalStateQueueHeaders;
    let canonicalHeader = canonicalHeaders.find(
      (entry) => entry.headerHash === headerHashHex,
    );
    let txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
    let unfinishedJobs = yield* MutationJobsDB.retrieveUnfinished;
    const repairActions: string[] = [];
    const evidenceEntries = (): ReconciliationEvidence[] => [
      evidence("canonical_state_queue", {
        containsHeader: canonicalHeader !== undefined,
        headers: canonicalHeaders.map((entry) => entry.headerHash),
        outRef: canonicalHeader?.outRef ?? null,
      }),
      optionRecordEvidence(journal),
      unfinishedLocalFinalizationJobEvidence(headerHashHex, unfinishedJobs),
      evidence("local_block_rows", { txCount: txHashes.length }),
    ];

    const journalStatus = Option.isSome(journal)
      ? journal.value[PendingBlockFinalizationsDB.Columns.STATUS]
      : null;
    const alreadyFinalized =
      journalStatus === PendingBlockFinalizationsDB.Status.Finalized &&
      txHashes.length > 0 &&
      !unfinishedJobs.some(
        (entry) =>
          entry[MutationJobsDB.Columns.JOB_ID] ===
          localFinalizationJobId(headerHashHex),
      );
    if (alreadyFinalized) {
      return result({
        milestone: "local-finalization",
        target: { headerHash: headerHashHex },
        status: "satisfied",
        evidence: evidenceEntries(),
      });
    }

    if (!repair) {
      const status: ReconciliationStatus =
        canonicalHeader === undefined
          ? "pending"
          : Option.isNone(journal)
            ? "ambiguous"
            : "pending";
      return result({
        milestone: "local-finalization",
        target: { headerHash: headerHashHex },
        status,
        evidence: evidenceEntries(),
        nextAction:
          canonicalHeader === undefined
            ? "Wait for the header to become canonical before local finalization recovery."
            : Option.isNone(journal)
              ? "No durable pending-finalization journal exists for this canonical header."
              : "Run with --repair to replay local finalization from the durable pending-finalization journal.",
      });
    }

    repairActions.push("recover_local_finalization");
    if (canonicalHeader === undefined) {
      return result({
        milestone: "local-finalization",
        target: { headerHash: headerHashHex },
        status: "blocked",
        evidence: evidenceEntries(),
        repairActions,
        nextAction:
          "Cannot recover local finalization until the header is present in canonical state_queue.",
      });
    }
    if (Option.isNone(journal)) {
      return result({
        milestone: "local-finalization",
        target: { headerHash: headerHashHex },
        status: "ambiguous",
        evidence: evidenceEntries(),
        repairActions,
        nextAction:
          "Cannot recover local finalization without a durable pending-finalization journal.",
      });
    }

    const serialized = yield* serializeStateQueueUTxO(canonicalHeader.utxo);
    const workerInput = {
      data: {
        availableConfirmedBlock: "",
        availableLocalFinalizationBlock: serialized,
        currentBlockStartTimeMs: 0,
        ledgerStoreLeaseOwner: `commit:${randomUUID()}`,
        localFinalizationPending: true,
        mempoolTxsCountSoFar: 0,
        sizeOfProcessedTxsSoFar: 0,
      },
    } satisfies CommitBlockWorkerInput;
    const workerOutput = yield* runCommitBlockHeaderWorkerProgram(workerInput);
    canonicalHeaders = yield* fetchCanonicalStateQueueHeaders;
    canonicalHeader = canonicalHeaders.find(
      (entry) => entry.headerHash === headerHashHex,
    );
    txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
    unfinishedJobs = yield* MutationJobsDB.retrieveUnfinished;
    const afterJournal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
    const recovered =
      workerOutput.type === "SuccessfulLocalFinalizationRecoveryOutput" &&
      Option.isSome(afterJournal) &&
      afterJournal.value[PendingBlockFinalizationsDB.Columns.STATUS] ===
        PendingBlockFinalizationsDB.Status.Finalized &&
      txHashes.length > 0 &&
      !unfinishedJobs.some(
        (entry) =>
          entry[MutationJobsDB.Columns.JOB_ID] ===
          localFinalizationJobId(headerHashHex),
      );

    return result({
      milestone: "local-finalization",
      target: { headerHash: headerHashHex },
      status: recovered ? "repaired" : "failed",
      evidence: [
        evidence(
          "worker_output",
          workerOutput as unknown as Record<string, unknown>,
        ),
        evidence("canonical_state_queue", {
          containsHeader: canonicalHeader !== undefined,
          headers: canonicalHeaders.map((entry) => entry.headerHash),
          outRef: canonicalHeader?.outRef ?? null,
        }),
        optionRecordEvidence(afterJournal),
        unfinishedLocalFinalizationJobEvidence(headerHashHex, unfinishedJobs),
        evidence("local_block_rows", { txCount: txHashes.length }),
      ],
      repairActions,
      nextAction: recovered
        ? null
        : "Local finalization repair did not reach finalized state; inspect worker_output and logs.",
    });
  });

export const reconcileMergeCompleteProgram = ({
  headerHash,
  repair,
}: {
  readonly headerHash: Buffer;
  readonly repair: boolean;
}): Effect.Effect<
  ReconciliationResult,
  unknown,
  Database | Lucid | MidgardContracts | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    const headerHashHex = headerHash.toString("hex");
    let canonicalHeaders = yield* fetchCanonicalStateQueueHeaderHashes;
    let txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
    const repairActions: string[] = [];
    if (repair && canonicalHeaders.includes(headerHashHex)) {
      const mergeResult = yield* mergeAction(true);
      repairActions.push("merge_action");
      canonicalHeaders = yield* fetchCanonicalStateQueueHeaderHashes;
      txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
      return result({
        milestone: "merge-complete",
        target: { headerHash: headerHashHex },
        status: canonicalHeaders.includes(headerHashHex)
          ? "pending"
          : "repaired",
        evidence: [
          evidence("merge_result", mergeResult as Record<string, unknown>),
          evidence("canonical_state_queue", {
            containsHeader: canonicalHeaders.includes(headerHashHex),
            headers: canonicalHeaders,
          }),
          evidence("local_block_rows", { txCount: txHashes.length }),
        ],
        repairActions,
        nextAction: canonicalHeaders.includes(headerHashHex)
          ? "Merge did not remove the header yet; inspect merge result and state-queue lease."
          : null,
      });
    }

    const canonical = canonicalHeaders.includes(headerHashHex);
    return result({
      milestone: "merge-complete",
      target: { headerHash: headerHashHex },
      status: !canonical && txHashes.length > 0 ? "satisfied" : "pending",
      evidence: [
        evidence("canonical_state_queue", {
          containsHeader: canonical,
          headers: canonicalHeaders,
        }),
        evidence("local_block_rows", { txCount: txHashes.length }),
        evidence(
          "state_queue_lease",
          yield* StateQueueMutationLeasesDB.inspect(),
        ),
      ],
      repairActions,
      nextAction: canonical
        ? "HeaderV1 is still queued; run with --repair only after DA/finality gates are satisfied."
        : "Local block rows are missing for this header; inspect local finalization before claiming merge complete.",
    });
  });
