import {
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
  TxUtils,
} from "midgard-node/database/index";

import { type HostProcessServiceSpec } from "./service-supervisor.js";

export type JournalKillNodeProcessSpec = {
  readonly nodeId: string;
  readonly postgresIdentity: string;
  readonly ledgerMpfDbPath: string;
  readonly transactionsMpfDbPath: string;
  readonly stateQueueMutationLeaseTtlMs: number;
  readonly process: HostProcessServiceSpec;
};

export type JournalKillDatabaseState = {
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
