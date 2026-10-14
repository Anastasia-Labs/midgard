import type * as SDK from "@al-ft/midgard-sdk";

import type { CommitAnchor } from "../../database/commit-anchor.js";
import type {
  DepositsDB,
  ForcedTransactionsDB,
  TxUtils as TxTable,
  WithdrawalsDB,
} from "../../database/index.js";
import type * as Ledger from "../../database/utils/ledger.js";
import type {
  LedgerDelta,
  MidgardMpf,
  NativeMpfReplayBuild,
  RetainedEventToStepMember,
  RetainedTransitionTraceMember,
  RetainedValidationTraceMember,
} from "../../mpf/index.js";
import type { ContractDeploymentIdentityValue } from "../../services/index.js";
import type { WorkerInput } from "../utils/commit-block-header.js";
import type { CommitSubmissionHooks } from "./submission-hooks.js";

/** The transaction-backed submission boundary, including separate coverage and commit clocks. */
export type SubmitTxBackedCommitInput = CommitSubmissionHooks & {
  readonly contracts: SDK.MidgardValidators;
  readonly consensusProfile: ContractDeploymentIdentityValue["consensusProfile"];
  readonly deploymentMarker: NonNullable<
    ContractDeploymentIdentityValue["deploymentMarker"]
  >;
  readonly latestBlock: SDK.StateQueueUTxO;
  readonly endTime: Date;
  readonly blockTimeMs?: number;
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedDepositEventIds: readonly Buffer[];
  readonly includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly includedForcedTransactionEventIds: readonly Buffer[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
  readonly includedWithdrawalEventIds: readonly Buffer[];
  readonly utxoRoot: string;
  readonly txRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
  readonly validationTracesRoot: string;
  readonly transitionTraceMembers: readonly RetainedTransitionTraceMember[];
  readonly eventToStepMembers: readonly RetainedEventToStepMember[];
  readonly validationTraceMembers: readonly RetainedValidationTraceMember[];
  readonly transitionStepCount: number;
  readonly validationTraceCount: number;
  readonly ledgerDelta: LedgerDelta;
  readonly selectedBaseUtxosRoot: string;
  readonly implicitGenesisEntries: readonly Ledger.MinimalEntry[];
  readonly nativeMpfReplay: NativeMpfReplayBuild;
  readonly transactionsMpf: MidgardMpf;
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxHashes: Buffer[];
  readonly mempoolTxSourceTable: string;
  readonly workerInput: WorkerInput;
  /** The commit anchor the end time was planned under (`commitEventHorizon`). */
  readonly commitAnchor?: CommitAnchor;
  readonly sizeOfProcessedTxs: number;
  readonly blockEndTimeCapMs?: number;
};
