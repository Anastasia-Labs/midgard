import { randomUUID } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import { parseEventId } from "@/commands/command-utils.js";
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
import {
  arrayOf,
  booleanValue,
  exactRecord,
  nonEmptyString,
  oneOf,
  openRecord,
} from "@/e2e/exact-artifact.js";
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

const parseReconciliationEvidenceV1 = (
  value: unknown,
  label: string,
): ReconciliationEvidence => {
  const input = exactRecord(value, label, ["kind", "detail"]);
  return {
    kind: nonEmptyString(input.kind, `${label}.kind`),
    // Evidence detail is deliberately open because each milestone has a
    // different diagnostic payload. The enclosing evidence record is exact.
    detail: openRecord(input.detail, `${label}.detail`),
  };
};

const RECONCILIATION_MILESTONES = [
  "phas-registered",
  "reference-scripts-complete",
  "deposit-projected",
  "tx-committed",
  "da-attested",
  "block-committed",
  "local-finalization",
  "merge-complete",
] as const;

type ReconciliationMilestone = (typeof RECONCILIATION_MILESTONES)[number];

const canonicalString = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (parsed !== parsed.trim()) {
    throw new Error(`${label} must not contain surrounding whitespace`);
  }
  return parsed;
};

const lowerHex = (
  value: unknown,
  label: string,
  byteLength: number,
): string => {
  const parsed = canonicalString(value, label);
  if (parsed.length !== byteLength * 2 || !/^[0-9a-f]+$/u.test(parsed)) {
    throw new Error(
      `${label} must be ${byteLength.toString()} bytes of lowercase hexadecimal`,
    );
  }
  return parsed;
};

const parseReconciliationTargetV1 = (
  value: unknown,
  milestone: ReconciliationMilestone,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (milestone === "phas-registered") {
    const target = exactRecord(value, label, ["rewardAddress", "scriptHash"]);
    return {
      rewardAddress: canonicalString(
        target.rewardAddress,
        `${label}.rewardAddress`,
      ),
      scriptHash: lowerHex(target.scriptHash, `${label}.scriptHash`, 28),
    };
  }
  if (milestone === "reference-scripts-complete") {
    const target = exactRecord(value, label, [
      "scope",
      "address",
      "authPolicyId",
    ]);
    if (target.scope !== "node-runtime") {
      throw new Error(`${label}.scope must be node-runtime`);
    }
    return {
      scope: "node-runtime",
      address: canonicalString(target.address, `${label}.address`),
      authPolicyId: lowerHex(target.authPolicyId, `${label}.authPolicyId`, 28),
    };
  }
  if (milestone === "deposit-projected") {
    const target = exactRecord(value, label, [], ["eventId", "cardanoTxHash"]);
    if (target.eventId === undefined && target.cardanoTxHash === undefined) {
      throw new Error(`${label} must identify eventId or cardanoTxHash`);
    }
    const eventId =
      target.eventId === undefined
        ? undefined
        : canonicalString(target.eventId, `${label}.eventId`);
    if (
      eventId !== undefined &&
      (!/^(?:[0-9a-f]{2})+$/u.test(eventId) ||
        parseEventId(eventId, `${label}.eventId`).toString("hex") !== eventId)
    ) {
      throw new Error(
        `${label}.eventId must be canonical OutputReference CBOR`,
      );
    }
    return {
      ...(eventId === undefined ? {} : { eventId }),
      ...(target.cardanoTxHash === undefined
        ? {}
        : {
            cardanoTxHash: lowerHex(
              target.cardanoTxHash,
              `${label}.cardanoTxHash`,
              32,
            ),
          }),
    };
  }
  const target = exactRecord(value, label, [
    milestone === "tx-committed" ? "txHash" : "headerHash",
  ]);
  return milestone === "tx-committed"
    ? { txHash: lowerHex(target.txHash, `${label}.txHash`, 32) }
    : { headerHash: lowerHex(target.headerHash, `${label}.headerHash`, 28) };
};

export const parseReconciliationResultV1 = (
  value: unknown,
): ReconciliationResult => {
  const label = "E2E reconciliation";
  const input = exactRecord(value, label, [
    "schemaVersion",
    "milestone",
    "target",
    "status",
    "safeToRetryOriginalStep",
    "evidence",
    "nextAction",
    "repairActions",
  ]);
  if (input.schemaVersion !== RECONCILIATION_SCHEMA_VERSION) {
    throw new Error(
      `${label}.schemaVersion must be ${RECONCILIATION_SCHEMA_VERSION}`,
    );
  }
  const milestone = oneOf(
    input.milestone,
    `${label}.milestone`,
    RECONCILIATION_MILESTONES,
  );
  const parsed: ReconciliationResult = {
    schemaVersion: RECONCILIATION_SCHEMA_VERSION,
    milestone,
    target: parseReconciliationTargetV1(
      input.target,
      milestone,
      `${label}.target`,
    ),
    status: oneOf(input.status, `${label}.status`, [
      "satisfied",
      "pending",
      "repaired",
      "blocked",
      "ambiguous",
      "failed",
    ]),
    safeToRetryOriginalStep: booleanValue(
      input.safeToRetryOriginalStep,
      `${label}.safeToRetryOriginalStep`,
    ),
    evidence: arrayOf(
      input.evidence,
      `${label}.evidence`,
      parseReconciliationEvidenceV1,
    ),
    nextAction:
      input.nextAction === null
        ? null
        : nonEmptyString(input.nextAction, `${label}.nextAction`),
    repairActions: arrayOf(
      input.repairActions,
      `${label}.repairActions`,
      (entry, entryLabel) =>
        oneOf(entry, entryLabel, [
          "register_phas_membership_reward_account",
          "ensure_node_runtime_reference_scripts",
          "reconcile_deposit_submission_attempt",
          "reconcile_visible_deposit_utxos",
          "project_deposits_to_mempool_ledger",
          "backfill_missing_da_payload",
          "recover_local_finalization",
          "merge_action",
        ]),
    ),
  };
  if (
    new Set(parsed.repairActions).size !== parsed.repairActions.length ||
    ((parsed.status === "ambiguous" ||
      parsed.status === "blocked" ||
      parsed.status === "failed") &&
      parsed.safeToRetryOriginalStep) ||
    (parsed.status === "satisfied" && parsed.nextAction !== null) ||
    (parsed.status === "repaired" &&
      (parsed.repairActions.length === 0 || parsed.nextAction !== null))
  ) {
    throw new Error(
      `${label} status, retry, or repair binding is inconsistent`,
    );
  }
  return parsed;
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
}: ReconciliationResultInput): ReconciliationResult =>
  parseReconciliationResultV1({
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
  readonly daAttestation: string;
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
    | "local_payload_missing"
    | "unexpected_attestation_marker";
  readonly nextAction: string | null;
};

export const classifyCanonicalDaAttestation = ({
  headerHash,
  expectedDaAttestationPolicyId,
  localPayloadPresent,
  observations,
}: {
  readonly headerHash: string;
  readonly expectedDaAttestationPolicyId: string;
  readonly localPayloadPresent: boolean;
  readonly observations: readonly CanonicalDaAttestationObservation[];
}): CanonicalDaAttestationDecision => {
  const matches = observations.filter(
    (observation) => observation.datumHeaderHash === headerHash,
  );
  if (matches.length === 0) {
    return {
      status: "ambiguous",
      reason: "canonical_header_absent",
      nextAction:
        "The configured Cardano source has no exact canonical state-queue node for this header; do not claim DA attestation from local or watcher-only evidence.",
    };
  }
  if (matches.length !== 1) {
    return {
      status: "blocked",
      reason: "canonical_header_not_unique",
      nextAction:
        "The configured Cardano source returned multiple canonical state-queue nodes for this header; resolve the inconsistent L1 view before continuing.",
    };
  }

  const matched = matches[0]!;
  if (matched.computedHeaderHash !== headerHash) {
    return {
      status: "blocked",
      reason: "header_hash_mismatch",
      nextAction:
        "The canonical state-queue datum key does not match its recomputed HeaderV1 hash; quarantine this observation and reconcile the L1 source.",
    };
  }
  if (matched.daAttestation === expectedDaAttestationPolicyId) {
    return {
      status: "satisfied",
      reason: "attestation_applied",
      nextAction: null,
    };
  }
  if (matched.daAttestation !== SDK.NO_DA_ATTESTATION) {
    return {
      status: "blocked",
      reason: "unexpected_attestation_marker",
      nextAction:
        "The canonical state-queue node contains an unexpected DA-attestation policy marker; quarantine it until the deployment/source mismatch is resolved.",
    };
  }
  if (!localPayloadPresent) {
    return {
      status: "blocked",
      reason: "local_payload_missing",
      nextAction:
        "The canonical state-queue node is not DA-attested and the exact local payload is missing; restore the canonical payload before attestation.",
    };
  }
  return {
    status: "pending",
    reason: "attestation_pending",
    nextAction:
      "The exact canonical state-queue node is present but has no on-chain DA-attestation marker yet; wait for the canonical watcher/submitter pipeline.",
  };
};

const fetchCanonicalDaAttestationObservations = Effect.gen(function* () {
  const canonicalHeaders = yield* fetchCanonicalStateQueueHeaders;
  const observations: CanonicalDaAttestationObservation[] = [];
  for (const canonicalHeader of canonicalHeaders) {
    const node = yield* SDK.getStateQueueNodeV1FromStateQueueDatum(
      canonicalHeader.utxo.datum,
    );
    observations.push({
      datumHeaderHash: canonicalHeader.headerHash,
      computedHeaderHash: yield* SDK.hashBlockHeaderV1(node.header),
      daAttestation: node.da_attestation,
      outRef: canonicalHeader.outRef,
    });
  }
  return observations;
});

export const reconcileDaAttestedProgram = (options: {
  readonly headerHash: Buffer;
  readonly watcherUrl?: string;
  readonly deploymentFingerprint?: string;
  readonly repair: boolean;
}): Effect.Effect<
  ReconciliationResult,
  DatabaseError,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const { headerHash, repair } = options;
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
      evidence(
        "local_da_payload",
        Option.isNone(localPayload)
          ? { present: false, headerHash: headerHashHex }
          : {
              present: true,
              headerHash: headerHashHex,
              consensusProfileId:
                localPayload.value[DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID],
              payloadSha256:
                localPayload.value[
                  DaPayloadsDB.Columns.PAYLOAD_SHA256
                ].toString("hex"),
            },
      ),
    ];
    if (backfillSkipped.length > 0) {
      evidenceEntries.push(
        evidence("da_payload_backfill_skipped", {
          reasons: backfillSkipped.map((entry) => entry.reason),
        }),
      );
    }

    const contracts = yield* MidgardContracts;
    const canonicalAttempt = yield* Effect.either(
      fetchCanonicalDaAttestationObservations,
    );
    if (canonicalAttempt._tag === "Left") {
      evidenceEntries.push(
        evidence("canonical_l1_da_attestation_query_error", {
          stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
          stateQueuePolicyId: contracts.stateQueue.policyId,
          expectedDaAttestationPolicyId: contracts.daAttestation.policyId,
          error: formatUnknownError(canonicalAttempt.left, {
            includeCause: true,
          }),
        }),
      );
      return result({
        milestone: "da-attested",
        target: { headerHash: headerHashHex },
        status: "blocked",
        evidence: evidenceEntries,
        repairActions,
        nextAction:
          "The configured Cardano source could not prove the canonical state-queue attestation marker; restore a consistent L1 query path before continuing.",
      });
    }

    const decision = classifyCanonicalDaAttestation({
      headerHash: headerHashHex,
      expectedDaAttestationPolicyId: contracts.daAttestation.policyId,
      localPayloadPresent: Option.isSome(localPayload),
      observations: canonicalAttempt.right,
    });
    evidenceEntries.push(
      evidence("canonical_l1_da_attestation", {
        source: "configured_cardano_l1_query",
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
        expectedDaAttestationPolicyId: contracts.daAttestation.policyId,
        targetHeaderHash: headerHashHex,
        decisionReason: decision.reason,
        observations: canonicalAttempt.right,
      }),
    );
    return result({
      milestone: "da-attested",
      target: { headerHash: headerHashHex },
      status: decision.status,
      evidence: evidenceEntries,
      repairActions,
      nextAction:
        decision.status === "satisfied"
          ? null
          : backfillSkipped.some((entry) =>
                entry.reason.includes("journal excluded by status: abandoned"),
              )
            ? "Canonical journal is abandoned locally; revive it through confirmation recovery and complete local finalization before DA payload backfill."
            : decision.nextAction,
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
  Database | Lucid | MidgardContracts | ContractDeploymentIdentity | NodeConfig
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
