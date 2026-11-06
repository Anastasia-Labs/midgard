/**
 * Rejection metadata recorded for a transaction that failed validation.
 */
export type TxRejectionDetails = {
  readonly rejectCode: string;
  readonly rejectDetail: string | null;
  readonly createdAtIso: string;
};

/**
 * Inputs used to resolve a user-facing transaction status from local node
 * state.
 */
export type ResolveTxStatusInput = {
  readonly txIdHex: string;
  readonly rejection: TxRejectionDetails | null;
  readonly admissionStatus:
    | "queued"
    | "validating"
    | "accepted"
    | "rejected"
    | null;
  readonly inImmutable: boolean;
  readonly inMempool: boolean;
  readonly inProcessedMempool: boolean;
  readonly localFinalizationPending: boolean;
  /** Which block holds the tx: this node's own (`own_block`) or a landed
   * block of any operator that includes a tx this node accepted
   * (`landed_block`). */
  readonly inclusion?: "own_block" | "landed_block";
  readonly headerHash?: string | null;
  readonly headerStatus?: string | null;
  readonly mergeStatus?: string | null;
  readonly confirmedLedgerFinalized?: boolean;
};

/** Every `committedMeaning` a `committed` status can carry: this node's own
 * block holds the tx, or another operator's landed block includes it. */
export const TX_STATUS_COMMITTED_MEANINGS = [
  "immutable_db_inclusion_not_confirmed_ledger_merge",
  "landed_block_inclusion_not_confirmed_ledger_merge",
] as const;

export type TxStatusCommittedMeaning =
  (typeof TX_STATUS_COMMITTED_MEANINGS)[number];

/**
 * Canonical transaction-status response exposed by the node.
 */
export type ResolvedTxStatus =
  | {
      readonly txId: string;
      readonly status: "rejected";
      readonly reasonCode: string;
      readonly reasonDetail?: string;
      readonly timestamps: {
        readonly createdAt: string;
      };
    }
  | {
      readonly txId: string;
      readonly status:
        | "committed"
        | "accepted"
        | "pending_commit"
        | "awaiting_local_recovery"
        | "validating"
        | "queued"
        | "not_found";
      readonly committedMeaning?: TxStatusCommittedMeaning;
      readonly headerHash?: string;
      readonly headerStatus?: string;
      readonly mergeStatus?: string;
      readonly confirmedLedgerFinalized?: boolean;
    };

/**
 * Resolves the highest-priority status that applies to a transaction id.
 */
export const resolveTxStatus = (
  input: ResolveTxStatusInput,
): ResolvedTxStatus => {
  // Another operator's landed block included it: this node's immutable
  // storage never holds it, and its inclusion mark names that block.
  const inLandedBlock =
    !input.inImmutable && input.inclusion === "landed_block";
  if (input.inImmutable || inLandedBlock) {
    return {
      txId: input.txIdHex,
      status: "committed",
      committedMeaning: inLandedBlock
        ? "landed_block_inclusion_not_confirmed_ledger_merge"
        : "immutable_db_inclusion_not_confirmed_ledger_merge",
      ...(input.headerHash === undefined || input.headerHash === null
        ? {}
        : { headerHash: input.headerHash }),
      ...(input.headerStatus === undefined || input.headerStatus === null
        ? {}
        : { headerStatus: input.headerStatus }),
      ...(input.mergeStatus === undefined || input.mergeStatus === null
        ? {}
        : { mergeStatus: input.mergeStatus }),
      confirmedLedgerFinalized: input.confirmedLedgerFinalized ?? false,
    };
  }

  if (input.localFinalizationPending && input.inProcessedMempool) {
    return {
      txId: input.txIdHex,
      status: "awaiting_local_recovery",
    };
  }

  if (input.inProcessedMempool) {
    return {
      txId: input.txIdHex,
      status: "pending_commit",
    };
  }

  if (input.inMempool) {
    return {
      txId: input.txIdHex,
      status: "accepted",
    };
  }

  if (input.rejection !== null) {
    return {
      txId: input.txIdHex,
      status: "rejected",
      reasonCode: input.rejection.rejectCode,
      reasonDetail: input.rejection.rejectDetail ?? undefined,
      timestamps: {
        createdAt: input.rejection.createdAtIso,
      },
    };
  }

  if (input.admissionStatus === "validating") {
    return {
      txId: input.txIdHex,
      status: "validating",
    };
  }

  if (input.admissionStatus === "queued") {
    return {
      txId: input.txIdHex,
      status: "queued",
    };
  }

  return {
    txId: input.txIdHex,
    status: "not_found",
  };
};

export type ResolveTxStatusBatchInput = {
  readonly txIdsHex: readonly string[];
  readonly rejectionsByTxId: ReadonlyMap<string, TxRejectionDetails>;
  readonly admissionStatusByTxId: ReadonlyMap<
    string,
    ResolveTxStatusInput["admissionStatus"]
  >;
  readonly immutableTxIds: ReadonlySet<string>;
  readonly mempoolTxIds: ReadonlySet<string>;
  readonly processedMempoolTxIds: ReadonlySet<string>;
  readonly localFinalizationPending: boolean;
  readonly headerEvidenceByTxId?: ReadonlyMap<
    string,
    {
      readonly inclusion?: "own_block" | "landed_block";
      readonly headerHash?: string;
      readonly headerStatus?: string;
      readonly mergeStatus?: string;
      readonly confirmedLedgerFinalized?: boolean;
    }
  >;
};

export const resolveTxStatusBatch = (
  input: ResolveTxStatusBatchInput,
): readonly ResolvedTxStatus[] =>
  input.txIdsHex.map((txIdHex) => {
    const headerEvidence = input.headerEvidenceByTxId?.get(txIdHex);
    return resolveTxStatus({
      txIdHex,
      rejection: input.rejectionsByTxId.get(txIdHex) ?? null,
      admissionStatus: input.admissionStatusByTxId.get(txIdHex) ?? null,
      inImmutable: input.immutableTxIds.has(txIdHex),
      inMempool: input.mempoolTxIds.has(txIdHex),
      inProcessedMempool: input.processedMempoolTxIds.has(txIdHex),
      localFinalizationPending: input.localFinalizationPending,
      ...(headerEvidence === undefined ? {} : headerEvidence),
    });
  });
