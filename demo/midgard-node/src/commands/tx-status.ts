export type TxRejectionDetails = {
  readonly rejectCode: string;
  readonly rejectDetail: string | null;
  readonly createdAtIso: string;
};

export type ResolveTxStatusInput = {
  readonly txIdHex: string;
  readonly rejection: TxRejectionDetails | null;
  readonly inImmutable: boolean;
  readonly inMempool: boolean;
  readonly inProcessedMempool: boolean;
  readonly localFinalizationPending: boolean;
};

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
        | "not_found";
    };

export const resolveTxStatus = (input: ResolveTxStatusInput): ResolvedTxStatus => {
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

  if (input.inImmutable) {
    return {
      txId: input.txIdHex,
      status: "committed",
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

  return {
    txId: input.txIdHex,
    status: "not_found",
  };
};
