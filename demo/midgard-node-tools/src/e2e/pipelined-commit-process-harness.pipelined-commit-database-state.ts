import {
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
  TxUtils,
} from "midgard-node/database/index";

import { type HostProcessServiceSpec } from "./service-supervisor.js";

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

export const normalizeTxEntries = (entries: readonly TxUtils.Entry[]) =>
  entries
    .map((entry) => ({
      txId: entry[TxUtils.Columns.TX_ID].toString("hex"),
      tx: entry[TxUtils.Columns.TX].toString("hex"),
    }))
    .sort((left, right) => left.txId.localeCompare(right.txId));

export const normalizeJournalMembers = (
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
