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
  readonly headerHash?: string | null;
  readonly headerStatus?: string | null;
  readonly mergeStatus?: string | null;
  readonly confirmedLedgerFinalized?: boolean;
};

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
      readonly committedMeaning?: "immutable_db_inclusion_not_confirmed_ledger_merge";
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
  if (input.inImmutable) {
    return {
      txId: input.txIdHex,
      status: "committed",
      committedMeaning: "immutable_db_inclusion_not_confirmed_ledger_merge",
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
