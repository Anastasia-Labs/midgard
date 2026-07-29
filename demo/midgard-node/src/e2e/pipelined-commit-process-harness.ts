import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";

import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import {
  DepositsDB,
  MempoolDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
  TxUtils,
} from "@/database/index.js";
import type { Database } from "@/services/database.js";

import {
  PIPELINED_COMMIT_E2E_HARNESS_MODE,
  type PipelinedCommitCrashCheckpoint,
  pipelinedCommitCrashCheckpointMarker,
} from "./pipelined-commit-crash-checkpoint.js";
import {
  type HostProcessServiceSpec,
  type ServiceSupervisorSummary,
  superviseHostProcess,
} from "./service-supervisor.js";

export type PipelinedCommitNodeProcessSpec = {
  readonly nodeId: string;
  readonly postgresIdentity: string;
  readonly ledgerMpfDbPath: string;
  readonly transactionsMpfDbPath: string;
  readonly stateQueueMutationLeaseTtlMs: number;
  readonly process: HostProcessServiceSpec;
};

export type PipelinedCommitDatabaseState = {
  readonly activeJournalCount: number;
  readonly activeJournal: null | {
    readonly headerHash: string;
    readonly headerCbor: string;
    readonly journalPayloadIdentity: {
      readonly deposits: readonly unknown[];
      readonly forcedTransactions: readonly unknown[];
      readonly withdrawals: readonly unknown[];
      readonly transactions: readonly unknown[];
      readonly transitionTrace: readonly unknown[];
      readonly eventToStep: readonly unknown[];
      readonly ledgerDelta: {
        readonly spent: readonly string[];
        readonly produced: readonly unknown[];
      };
    };
    readonly submittedTxHash: string | null;
    readonly status: PendingBlockFinalizationsDB.Status;
    readonly baseTailHeaderHash: string | null;
    readonly baseTailOutRef: string | null;
    readonly baseTailDatumCbor: string | null;
    readonly baseRoots: {
      readonly utxos: string;
      readonly forcedTransactions: string;
      readonly transactions: string;
      readonly deposits: string;
      readonly withdrawals: string;
    };
    readonly expectedRoots: {
      readonly utxos: string;
      readonly forcedTransactions: string;
      readonly transactions: string;
      readonly deposits: string;
      readonly withdrawals: string;
      readonly transitionTrace: string;
      readonly eventToStep: string;
    };
    readonly mpfReplay: {
      readonly baseRoot: string | null;
      readonly candidateRoot: string | null;
      readonly eventLogDigest: string | null;
      readonly eventRoots: string | null;
      readonly eventCount: number | null;
    };
    readonly leaseToken: string;
    readonly depositCount: number;
    readonly mempoolTxCount: number;
  };
  readonly activeLease: null | {
    readonly holder: string;
    readonly token: string;
    readonly status: StateQueueMutationLeasesDB.Status;
  };
  readonly recentLeases: readonly {
    readonly holder: string;
    readonly status: StateQueueMutationLeasesDB.Status;
    readonly lastError: string | null;
  }[];
  readonly deposits: readonly {
    readonly id: string;
    readonly status: string;
    readonly projectedHeaderHash: string | null;
  }[];
  readonly mempool: readonly { readonly txId: string; readonly tx: string }[];
  readonly processed: readonly {
    readonly txId: string;
    readonly tx: string;
  }[];
};

export type PipelinedCommitEquivalentDatabaseState = Omit<
  PipelinedCommitDatabaseState,
  "activeJournal" | "activeLease"
> & {
  readonly activeJournal:
    | null
    | (Omit<
        NonNullable<PipelinedCommitDatabaseState["activeJournal"]>,
        "leaseToken"
      > & { readonly leaseTokenPresent: boolean });
  readonly activeLease: null | {
    readonly holder: string;
    readonly status: StateQueueMutationLeasesDB.Status;
    readonly tokenPresent: boolean;
  };
};

/**
 * Drops only the generated lease token value before flag-on/flag-off
 * comparison.  The token's presence, journal payload identity, roots and
 * submitted transaction hash remain part of the comparison below.
 */
export const normalizePipelinedCommitDatabaseState = (
  state: PipelinedCommitDatabaseState,
): PipelinedCommitEquivalentDatabaseState => ({
  ...state,
  activeJournal:
    state.activeJournal === null
      ? null
      : {
          headerHash: state.activeJournal.headerHash,
          headerCbor: state.activeJournal.headerCbor,
          journalPayloadIdentity: state.activeJournal.journalPayloadIdentity,
          submittedTxHash: state.activeJournal.submittedTxHash,
          status: state.activeJournal.status,
          baseTailHeaderHash: state.activeJournal.baseTailHeaderHash,
          baseTailOutRef: state.activeJournal.baseTailOutRef,
          baseTailDatumCbor: state.activeJournal.baseTailDatumCbor,
          baseRoots: state.activeJournal.baseRoots,
          expectedRoots: state.activeJournal.expectedRoots,
          mpfReplay: state.activeJournal.mpfReplay,
          leaseTokenPresent: state.activeJournal.leaseToken.length > 0,
          depositCount: state.activeJournal.depositCount,
          mempoolTxCount: state.activeJournal.mempoolTxCount,
        },
  activeLease:
    state.activeLease === null
      ? null
      : {
          holder: state.activeLease.holder,
          status: state.activeLease.status,
          tokenPresent: state.activeLease.token.length > 0,
        },
});

export const assertNoJournalBeyondBase = (
  state: PipelinedCommitDatabaseState,
  expectedBaseHeaderHash: string,
): void => {
  if (state.activeJournalCount > 1) {
    throw new Error(
      `Crash violated the single-active-journal invariant: active_count=${state.activeJournalCount.toString()}`,
    );
  }
  if (
    state.activeJournal !== null &&
    state.activeJournal.headerHash !== expectedBaseHeaderHash
  ) {
    throw new Error(
      `Crash persisted a journal beyond the submitted base: expected=${expectedBaseHeaderHash},actual=${state.activeJournal.headerHash}`,
    );
  }
};

const normalizeTxEntries = (entries: readonly TxUtils.Entry[]) =>
  entries
    .map((entry) => ({
      txId: entry[TxUtils.Columns.TX_ID].toString("hex"),
      tx: entry[TxUtils.Columns.TX].toString("hex"),
    }))
    .sort((left, right) => left.txId.localeCompare(right.txId));

const normalizeJournalMembers = (
  members: readonly PendingBlockFinalizationsDB.MemberRecord[],
) =>
  members
    .map((member) => ({
      memberId:
        member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID].toString(
          "hex",
        ),
      ordinal: member[PendingBlockFinalizationsDB.MemberColumns.ORDINAL],
      payloadSha256:
        member[
          PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_SHA256
        ].toString("hex"),
      sourceTable:
        member[PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE],
      sourceId:
        member[PendingBlockFinalizationsDB.MemberColumns.SOURCE_ID]?.toString(
          "hex",
        ) ?? null,
    }))
    .sort((left, right) =>
      JSON.stringify(left).localeCompare(JSON.stringify(right)),
    );

/** Read-only evidence used by the real-process crash and contention gates. */
export const capturePipelinedCommitDatabaseState: Effect.Effect<
  PipelinedCommitDatabaseState,
  unknown,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const activeStatuses = [
    PendingBlockFinalizationsDB.Status.PendingSubmission,
    PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending,
    PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
    PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
  ];
  const [journal, leaseInspection, deposits, mempool, processed, activeCount] =
    yield* Effect.all([
      PendingBlockFinalizationsDB.retrieveActive(),
      StateQueueMutationLeasesDB.inspect(),
      DepositsDB.retrieveAllEntries(),
      TxUtils.retrieveAllEntries(MempoolDB.tableName),
      ProcessedMempoolDB.retrieve,
      sql<{ readonly count: string }>`SELECT COUNT(*)::text AS count
        FROM ${sql(PendingBlockFinalizationsDB.tableName)}
        WHERE ${sql(PendingBlockFinalizationsDB.Columns.STATUS)} IN ${sql.in(activeStatuses)}`,
    ]);
  const activeJournal = Option.match(journal, {
    onNone: () => null,
    onSome: (record) => ({
      headerHash:
        record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex"),
      headerCbor:
        record[PendingBlockFinalizationsDB.Columns.HEADER_CBOR].toString("hex"),
      journalPayloadIdentity: {
        deposits: normalizeJournalMembers(record.depositMembers),
        forcedTransactions: normalizeJournalMembers(
          record.forcedTransactionMembers,
        ),
        withdrawals: normalizeJournalMembers(record.withdrawalMembers),
        transactions: normalizeJournalMembers(record.txMembers),
        transitionTrace: normalizeJournalMembers(record.transitionTraceMembers),
        eventToStep: normalizeJournalMembers(record.eventToStepMembers),
        ledgerDelta: {
          spent: record.ledgerDelta.spent
            .map((outref) => outref.toString("hex"))
            .sort(),
          produced: record.ledgerDelta.produced
            .map((member) => ({
              outref:
                member[PendingBlockFinalizationsDB.UtxoColumns.OUTREF].toString(
                  "hex",
                ),
              output:
                member[PendingBlockFinalizationsDB.UtxoColumns.OUTPUT].toString(
                  "hex",
                ),
            }))
            .sort((left, right) =>
              JSON.stringify(left).localeCompare(JSON.stringify(right)),
            ),
        },
      },
      submittedTxHash:
        record[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]?.toString(
          "hex",
        ) ?? null,
      status: record[PendingBlockFinalizationsDB.Columns.STATUS],
      baseTailHeaderHash:
        record[
          PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH
        ]?.toString("hex") ?? null,
      baseTailOutRef:
        record[PendingBlockFinalizationsDB.Columns.BASE_TAIL_OUT_REF] ?? null,
      baseTailDatumCbor:
        record[PendingBlockFinalizationsDB.Columns.BASE_TAIL_DATUM_CBOR] ??
        null,
      baseRoots: {
        utxos: record[PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT],
        forcedTransactions:
          record[
            PendingBlockFinalizationsDB.Columns.BASE_FORCED_TRANSACTIONS_ROOT
          ],
        transactions:
          record[PendingBlockFinalizationsDB.Columns.BASE_TRANSACTIONS_ROOT],
        deposits:
          record[PendingBlockFinalizationsDB.Columns.BASE_DEPOSITS_ROOT],
        withdrawals:
          record[PendingBlockFinalizationsDB.Columns.BASE_WITHDRAWALS_ROOT],
      },
      expectedRoots: {
        utxos: record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT],
        forcedTransactions:
          record[
            PendingBlockFinalizationsDB.Columns
              .EXPECTED_FORCED_TRANSACTIONS_ROOT
          ],
        transactions:
          record[
            PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT
          ],
        deposits:
          record[PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT],
        withdrawals:
          record[PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT],
        transitionTrace:
          record[
            PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSITION_TRACE_ROOT
          ],
        eventToStep:
          record[
            PendingBlockFinalizationsDB.Columns.EXPECTED_EVENT_TO_STEP_ROOT
          ],
      },
      mpfReplay: {
        baseRoot:
          record[
            PendingBlockFinalizationsDB.Columns.MPF_REPLAY_BASE_ROOT
          ]?.toString("hex") ?? null,
        candidateRoot:
          record[
            PendingBlockFinalizationsDB.Columns.MPF_REPLAY_CANDIDATE_ROOT
          ]?.toString("hex") ?? null,
        eventLogDigest:
          record[
            PendingBlockFinalizationsDB.Columns.MPF_REPLAY_EVENT_LOG_DIGEST
          ]?.toString("hex") ?? null,
        eventRoots:
          record[
            PendingBlockFinalizationsDB.Columns.MPF_REPLAY_EVENT_ROOTS
          ]?.toString("hex") ?? null,
        eventCount:
          record[PendingBlockFinalizationsDB.Columns.MPF_REPLAY_EVENT_COUNT] ??
          null,
      },
      leaseToken:
        record[PendingBlockFinalizationsDB.Columns.STATE_QUEUE_LEASE_TOKEN],
      depositCount: record.depositEventIds.length,
      mempoolTxCount: record.mempoolTxIds.length,
    }),
  });
  const activeLease =
    leaseInspection.activeLease === undefined
      ? null
      : {
          holder:
            leaseInspection.activeLease[
              StateQueueMutationLeasesDB.Columns.HOLDER
            ],
          token:
            leaseInspection.activeLease[
              StateQueueMutationLeasesDB.Columns.TOKEN
            ],
          status:
            leaseInspection.activeLease[
              StateQueueMutationLeasesDB.Columns.STATUS
            ],
        };
  return {
    activeJournalCount: Number(activeCount[0]?.count ?? "0"),
    activeJournal,
    activeLease,
    recentLeases: leaseInspection.recentLeases.map((lease) => ({
      holder: lease[StateQueueMutationLeasesDB.Columns.HOLDER],
      status: lease[StateQueueMutationLeasesDB.Columns.STATUS],
      lastError: lease[StateQueueMutationLeasesDB.Columns.LAST_ERROR],
    })),
    deposits: deposits.map((entry) => ({
      id: entry[DepositsDB.Columns.ID].toString("hex"),
      status: entry[DepositsDB.Columns.STATUS],
      projectedHeaderHash:
        entry[DepositsDB.Columns.PROJECTED_HEADER_HASH]?.toString("hex") ??
        null,
    })),
    mempool: normalizeTxEntries(mempool),
    processed: normalizeTxEntries(processed),
  };
});

const processEnvForCheckpoint = ({
  spec,
  checkpoint,
  armFile,
}: {
  readonly spec: PipelinedCommitNodeProcessSpec;
  readonly checkpoint: PipelinedCommitCrashCheckpoint;
  readonly armFile: string;
}): Readonly<Record<string, string | undefined>> => ({
  ...spec.process.env,
  NODE_ENV: "emulator",
  SPECULATIVE_COMMIT_BUILD: "true",
  LEDGER_MPF_DB_PATH: spec.ledgerMpfDbPath,
  TRANSACTIONS_MPF_DB_PATH: spec.transactionsMpfDbPath,
  STATE_QUEUE_MUTATION_LEASE_TTL_MS:
    spec.stateQueueMutationLeaseTtlMs.toString(),
  STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: Math.max(
    1,
    Math.floor(spec.stateQueueMutationLeaseTtlMs / 3),
  ).toString(),
  MIDGARD_E2E_PIPELINED_COMMIT_HARNESS: PIPELINED_COMMIT_E2E_HARNESS_MODE,
  MIDGARD_E2E_PIPELINED_COMMIT_CRASH_CHECKPOINT: checkpoint,
  MIDGARD_E2E_PIPELINED_COMMIT_CRASH_ARM_FILE: armFile,
});

const assertCheckpointTermination = ({
  summary,
  marker,
}: {
  readonly summary: ServiceSupervisorSummary;
  readonly marker: string;
}): void => {
  const checkpointAttempts = summary.attempts.filter(
    (attempt) => attempt.outputTermination?.marker === marker,
  );
  if (checkpointAttempts.length !== 1) {
    throw new Error(
      `Expected exactly one supervised checkpoint termination for ${marker}; observed ${checkpointAttempts.length.toString()}`,
    );
  }
  if (checkpointAttempts[0]?.signal !== "SIGKILL") {
    throw new Error(
      `Expected checkpoint process to exit via SIGKILL; observed ${checkpointAttempts[0]?.signal ?? "none"}`,
    );
  }
};

/**
 * Starts the actual node command, waits for a production-path checkpoint, and
 * externally SIGKILLs the detached process group. The one-shot arm file is
 * consumed by the child, so the same process spec can subsequently restart.
 */
export const runPipelinedCommitCheckpointCrash = async ({
  spec,
  checkpoint,
  armFile,
}: {
  readonly spec: PipelinedCommitNodeProcessSpec;
  readonly checkpoint: PipelinedCommitCrashCheckpoint;
  readonly armFile: string;
}): Promise<ServiceSupervisorSummary> => {
  await mkdir(dirname(armFile), { recursive: true });
  await writeFile(armFile, `${spec.nodeId}:${checkpoint}\n`, {
    encoding: "utf8",
    flag: "wx",
  });
  const marker = pipelinedCommitCrashCheckpointMarker(checkpoint);
  const summary = await superviseHostProcess({
    ...spec.process,
    service: `${spec.process.service}:${spec.nodeId}:${checkpoint}`,
    env: processEnvForCheckpoint({ spec, checkpoint, armFile }),
    maxRestarts: 0,
    terminateOnOutput: { marker, signal: "SIGKILL" },
  });
  assertCheckpointTermination({ summary, marker });
  return summary;
};

const assertMarkerTermination = ({
  summary,
  marker,
  signal,
}: {
  readonly summary: ServiceSupervisorSummary;
  readonly marker: string;
  readonly signal: NodeJS.Signals;
}): void => {
  const observation = summary.attempts[0]?.outputTermination;
  if (observation?.marker !== marker || observation.signal !== signal) {
    throw new Error(
      `Expected supervised ${signal} at marker ${marker}; observed ${observation?.signal ?? "none"} at ${observation?.marker ?? "none"}`,
    );
  }
};

/** Restarts the same node state and stops only after a newly built candidate. */
export const restartPipelinedCommitNodeUntilFreshCandidate = async ({
  spec,
  checkpoint,
  consumedArmFile,
}: {
  readonly spec: PipelinedCommitNodeProcessSpec;
  readonly checkpoint: PipelinedCommitCrashCheckpoint;
  readonly consumedArmFile: string;
}): Promise<ServiceSupervisorSummary> => {
  const marker = "pipeline_trace phase=candidate_ready";
  const summary = await superviseHostProcess({
    ...spec.process,
    service: `${spec.process.service}:${spec.nodeId}:restart`,
    env: processEnvForCheckpoint({
      spec,
      checkpoint,
      armFile: consumedArmFile,
    }),
    maxRestarts: 0,
    terminateOnOutput: { marker, signal: "SIGTERM" },
  });
  assertMarkerTermination({ summary, marker, signal: "SIGTERM" });
  return summary;
};

/** Restarts after the ready-candidate proof and waits for the real submission. */
export const restartPipelinedCommitNodeUntilSubmission = async ({
  spec,
  checkpoint,
  consumedArmFile,
}: {
  readonly spec: PipelinedCommitNodeProcessSpec;
  readonly checkpoint: PipelinedCommitCrashCheckpoint;
  readonly consumedArmFile: string;
}): Promise<ServiceSupervisorSummary> => {
  const marker = "pipeline_trace phase=candidate_submitted";
  const summary = await superviseHostProcess({
    ...spec.process,
    service: `${spec.process.service}:${spec.nodeId}:restart-submit`,
    env: processEnvForCheckpoint({
      spec,
      checkpoint,
      armFile: consumedArmFile,
    }),
    maxRestarts: 0,
    terminateOnOutput: { marker, signal: "SIGTERM" },
  });
  assertMarkerTermination({ summary, marker, signal: "SIGTERM" });
  return summary;
};

/** Runs the non-speculative control node to a caller-selected stable marker. */
export const runPipelinedCommitFlagOffControl = async ({
  spec,
  stopMarker,
  stopOccurrence = 1,
}: {
  readonly spec: PipelinedCommitNodeProcessSpec;
  readonly stopMarker: string;
  readonly stopOccurrence?: number;
}): Promise<ServiceSupervisorSummary> => {
  const summary = await superviseHostProcess({
    ...spec.process,
    service: `${spec.process.service}:${spec.nodeId}:flag-off-control`,
    env: {
      ...spec.process.env,
      NODE_ENV: "emulator",
      SPECULATIVE_COMMIT_BUILD: "false",
      LEDGER_MPF_DB_PATH: spec.ledgerMpfDbPath,
      TRANSACTIONS_MPF_DB_PATH: spec.transactionsMpfDbPath,
      STATE_QUEUE_MUTATION_LEASE_TTL_MS:
        spec.stateQueueMutationLeaseTtlMs.toString(),
      STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: Math.max(
        1,
        Math.floor(spec.stateQueueMutationLeaseTtlMs / 3),
      ).toString(),
    },
    maxRestarts: 0,
    terminateOnOutput: {
      marker: stopMarker,
      occurrence: stopOccurrence,
      signal: "SIGTERM",
    },
  });
  assertMarkerTermination({
    summary,
    marker: stopMarker,
    signal: "SIGTERM",
  });
  return summary;
};

export type PipelinedCommitLeaseContentionResult = {
  readonly winnerNodeId: string;
  readonly loserNodeId: string;
  readonly winner: ServiceSupervisorSummary;
  readonly loser: ServiceSupervisorSummary;
  readonly winnerLog: string;
  readonly loserLog: string;
};

const assertSharedPostgresAndPrivateMpfStores = (
  left: PipelinedCommitNodeProcessSpec,
  right: PipelinedCommitNodeProcessSpec,
): void => {
  if (left.postgresIdentity !== right.postgresIdentity) {
    throw new Error(
      "Lease-contention nodes must use the same Postgres identity",
    );
  }
  if (
    left.ledgerMpfDbPath === right.ledgerMpfDbPath ||
    left.transactionsMpfDbPath === right.transactionsMpfDbPath
  ) {
    throw new Error("Lease-contention nodes must use distinct MPF store paths");
  }
  if (
    left.process.timeoutMs === undefined ||
    right.process.timeoutMs === undefined
  ) {
    throw new Error(
      "Lease-contention node specs require bounded timeouts longer than the test lease TTL",
    );
  }
  for (const spec of [left, right]) {
    if (
      !Number.isSafeInteger(spec.stateQueueMutationLeaseTtlMs) ||
      spec.stateQueueMutationLeaseTtlMs <= 0 ||
      spec.process.timeoutMs! <= spec.stateQueueMutationLeaseTtlMs * 2
    ) {
      throw new Error(
        `Lease-contention timeout for ${spec.nodeId} must exceed twice its positive test lease TTL`,
      );
    }
  }
};

const processEnvForSpeculation = (
  spec: PipelinedCommitNodeProcessSpec,
): Readonly<Record<string, string | undefined>> => ({
  ...spec.process.env,
  NODE_ENV: "emulator",
  SPECULATIVE_COMMIT_BUILD: "true",
  LEDGER_MPF_DB_PATH: spec.ledgerMpfDbPath,
  TRANSACTIONS_MPF_DB_PATH: spec.transactionsMpfDbPath,
  STATE_QUEUE_MUTATION_LEASE_TTL_MS:
    spec.stateQueueMutationLeaseTtlMs.toString(),
  STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: Math.max(
    1,
    Math.floor(spec.stateQueueMutationLeaseTtlMs / 3),
  ).toString(),
});

/** Runs one real speculative node until a marker is observed and terminated. */
export const runPipelinedCommitNodeUntilMarker = async ({
  spec,
  marker,
  signal = "SIGTERM",
  suffix = "marker",
}: {
  readonly spec: PipelinedCommitNodeProcessSpec;
  readonly marker: string;
  readonly signal?: NodeJS.Signals;
  readonly suffix?: string;
}): Promise<ServiceSupervisorSummary> => {
  const summary = await superviseHostProcess({
    ...spec.process,
    service: `${spec.process.service}:${spec.nodeId}:${suffix}`,
    env: processEnvForSpeculation(spec),
    maxRestarts: 0,
    terminateOnOutput: { marker, signal },
  });
  assertMarkerTermination({ summary, marker, signal });
  return summary;
};

const writeStopFileAfterLogMarker = async ({
  logPath,
  marker,
  stopFile,
  timeoutMs,
}: {
  readonly logPath: string;
  readonly marker: string;
  readonly stopFile: string;
  readonly timeoutMs: number;
}): Promise<void> => {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const text = await readFile(logPath, "utf8").catch(() => "");
    if (text.includes(marker)) {
      await writeFile(stopFile, "stop\n", { encoding: "utf8", flag: "wx" });
      return;
    }
    await new Promise((resolve) => setTimeout(resolve, 50));
  }
  throw new Error(`Timed out waiting for ${marker} in ${logPath}`);
};

/**
 * Normal two-node contention gate: one process submits and the other either
 * records a DB-lease Busy deferral or loses the journal race at the database's
 * exact single-active-journal guard before invalidating its candidate.
 */
export const runPipelinedCommitNormalLeaseContention = async ({
  left,
  right,
}: {
  readonly left: PipelinedCommitNodeProcessSpec;
  readonly right: PipelinedCommitNodeProcessSpec;
}): Promise<PipelinedCommitLeaseContentionResult> => {
  assertSharedPostgresAndPrivateMpfStores(left, right);
  const submittedMarker = "pipeline_trace phase=candidate_submitted";
  const invalidatedMarkers = [
    "pipeline_trace phase=candidate_invalidated reason=T2",
    "pipeline_trace phase=candidate_invalidated reason=T7",
  ] as const;
  const run = (spec: PipelinedCommitNodeProcessSpec) =>
    superviseHostProcess({
      ...spec.process,
      service: `${spec.process.service}:${spec.nodeId}:normal-contention`,
      env: processEnvForSpeculation(spec),
      maxRestarts: 0,
      terminateOnOutput: {
        marker: submittedMarker,
        additionalMarkers: invalidatedMarkers,
        signal: "SIGTERM",
      },
    });
  const [leftSummary, rightSummary] = await Promise.all([
    run(left),
    run(right),
  ]);
  const entries = [
    { spec: left, summary: leftSummary },
    { spec: right, summary: rightSummary },
  ];
  const submitted = entries.filter(
    ({ summary }) =>
      summary.attempts[0]?.outputTermination?.marker === submittedMarker,
  );
  const invalidated = entries.filter(({ summary }) =>
    invalidatedMarkers.some(
      (marker) => summary.attempts[0]?.outputTermination?.marker === marker,
    ),
  );
  if (submitted.length !== 1 || invalidated.length !== 1) {
    throw new Error(
      `Expected one submitted winner and one invalidated loser; observed submitted=${submitted.length.toString()},invalidated=${invalidated.length.toString()}`,
    );
  }
  const winner = submitted[0]!;
  const loser = invalidated[0]!;
  const [winnerLog, loserLog] = await Promise.all([
    readFile(winner.summary.rawLogPath, "utf8"),
    readFile(loser.summary.rawLogPath, "utf8"),
  ]);
  const recordedLeaseBusy = loserLog.includes("reason=state_queue_lease_busy");
  const recordedActiveJournalRefusal = loserLog.includes(
    "Refusing to prepare a new pending block while another active pending-finalization record exists",
  );
  if (!recordedLeaseBusy && !recordedActiveJournalRefusal) {
    throw new Error(
      "Contention loser recorded neither a state-queue lease Busy deferral nor the single-active-journal refusal",
    );
  }
  return {
    winnerNodeId: winner.spec.nodeId,
    loserNodeId: loser.spec.nodeId,
    winner: winner.summary,
    loser: loser.summary,
    winnerLog,
    loserLog,
  };
};

/**
 * Runs two actual node commands concurrently against an explicitly shared
 * Postgres identity. Both have private MPF stores. Exactly one process can
 * consume the journal-before-submit arm file; that process is SIGKILLed while
 * holding the mutation lease and the other remains available for TTL recovery.
 */
export const runPipelinedCommitLeaseContention = async ({
  left,
  right,
  armFile,
}: {
  readonly left: PipelinedCommitNodeProcessSpec;
  readonly right: PipelinedCommitNodeProcessSpec;
  readonly armFile: string;
}): Promise<PipelinedCommitLeaseContentionResult> => {
  assertSharedPostgresAndPrivateMpfStores(left, right);
  const checkpoint = "journal_prepared_before_submit" as const;
  await mkdir(dirname(armFile), { recursive: true });
  await writeFile(armFile, `contention:${left.nodeId}:${right.nodeId}\n`, {
    encoding: "utf8",
    flag: "wx",
  });
  const marker = pipelinedCommitCrashCheckpointMarker(checkpoint);
  const survivorSubmittedMarker = "pipeline_trace phase=candidate_submitted";
  const start = (spec: PipelinedCommitNodeProcessSpec) => {
    const stopFile = join(dirname(armFile), `${spec.nodeId}.submitted.stop`);
    const summary = superviseHostProcess({
      ...spec.process,
      service: `${spec.process.service}:${spec.nodeId}:contention`,
      env: processEnvForCheckpoint({ spec, checkpoint, armFile }),
      maxRestarts: 0,
      terminateOnOutput: { marker, signal: "SIGKILL" },
      terminateOnFile: { path: stopFile, signal: "SIGTERM" },
    });
    const stopAfterSubmission = Promise.race([
      writeStopFileAfterLogMarker({
        logPath: spec.process.rawLogPath,
        marker: survivorSubmittedMarker,
        stopFile,
        timeoutMs: spec.process.timeoutMs!,
      }),
      summary.then(() => undefined),
    ]);
    return { summary, stopAfterSubmission };
  };
  const leftRun = start(left);
  const rightRun = start(right);
  const [leftSummary, rightSummary] = await Promise.all([
    leftRun.summary,
    rightRun.summary,
  ]);
  await Promise.all([
    leftRun.stopAfterSubmission,
    rightRun.stopAfterSubmission,
  ]);
  const terminated = [
    { spec: left, summary: leftSummary },
    { spec: right, summary: rightSummary },
  ].filter(({ summary }) =>
    summary.attempts.some(
      (attempt) => attempt.outputTermination?.marker === marker,
    ),
  );
  if (terminated.length !== 1) {
    throw new Error(
      `Expected one journal winner to be SIGKILLed; observed ${terminated.length.toString()}`,
    );
  }
  const winner = terminated[0]!;
  assertCheckpointTermination({ summary: winner.summary, marker });
  const loser =
    winner.spec.nodeId === left.nodeId
      ? { spec: right, summary: rightSummary }
      : { spec: left, summary: leftSummary };
  const [winnerLog, loserLog] = await Promise.all([
    readFile(winner.summary.rawLogPath, "utf8"),
    readFile(loser.summary.rawLogPath, "utf8"),
  ]);
  if (!loserLog.includes("reason=state_queue_lease_busy")) {
    throw new Error(
      "Journal-kill survivor did not record state-queue lease contention",
    );
  }
  if (
    !loserLog.includes("abandoning unsubmitted journal") &&
    !loserLog.includes("submitted_tx=unknown")
  ) {
    throw new Error(
      "Journal-kill survivor did not execute unsubmitted-journal recovery after lease expiry",
    );
  }
  if (!loserLog.includes("pipeline_trace phase=candidate_submitted")) {
    throw new Error(
      "Journal-kill survivor did not submit after unsubmitted-journal recovery",
    );
  }
  return {
    winnerNodeId: winner.spec.nodeId,
    loserNodeId: loser.spec.nodeId,
    winner: winner.summary,
    loser: loser.summary,
    winnerLog,
    loserLog,
  };
};
