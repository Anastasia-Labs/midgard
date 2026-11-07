import { describe, expect, it } from "vitest";

import {
  resolveTxStatus,
  resolveTxStatusBatch,
} from "../src/commands/tx-status.js";

describe("resolveTxStatus", () => {
  it("returns rejected when rejection entry exists", () => {
    const status = resolveTxStatus({
      admissionStatus: null,
      txIdHex: "ab",
      rejection: {
        rejectCode: "E_MIN_FEE",
        rejectDetail: "10 < 20",
        createdAtIso: "2026-01-01T00:00:00.000Z",
      },
      inImmutable: false,
      inMempool: false,
      inProcessedMempool: false,
      localFinalizationPending: false,
    });

    expect(status.status).toBe("rejected");
  });

  it("returns committed when immutable hit exists", () => {
    const status = resolveTxStatus({
      admissionStatus: null,
      txIdHex: "ab",
      rejection: null,
      inImmutable: true,
      inMempool: false,
      inProcessedMempool: false,
      localFinalizationPending: false,
    });

    expect(status.status).toBe("committed");
    expect(status).toMatchObject({
      committedMeaning: "immutable_db_inclusion_not_confirmed_ledger_merge",
      confirmedLedgerFinalized: false,
    });
  });

  it("returns pending_commit when tx is in processed mempool", () => {
    const status = resolveTxStatus({
      admissionStatus: null,
      txIdHex: "ab",
      rejection: null,
      inImmutable: false,
      inMempool: false,
      inProcessedMempool: true,
      localFinalizationPending: false,
    });

    expect(status.status).toBe("pending_commit");
  });

  it("returns accepted when tx is in mempool", () => {
    const status = resolveTxStatus({
      admissionStatus: null,
      txIdHex: "ab",
      rejection: null,
      inImmutable: false,
      inMempool: true,
      inProcessedMempool: false,
      localFinalizationPending: false,
    });

    expect(status.status).toBe("accepted");
  });

  it("returns awaiting_local_recovery when pending finalization is active", () => {
    const status = resolveTxStatus({
      admissionStatus: null,
      txIdHex: "ab",
      rejection: null,
      inImmutable: false,
      inMempool: false,
      inProcessedMempool: true,
      localFinalizationPending: true,
    });

    expect(status.status).toBe("awaiting_local_recovery");
  });

  it("returns not_found when tx is unknown", () => {
    const status = resolveTxStatus({
      admissionStatus: null,
      txIdHex: "ab",
      rejection: null,
      inImmutable: false,
      inMempool: false,
      inProcessedMempool: false,
      localFinalizationPending: false,
    });

    expect(status.status).toBe("not_found");
  });

  it("resolves batch statuses with the same priority as single status", () => {
    const statuses = resolveTxStatusBatch({
      txIdsHex: ["aa", "bb", "cc"],
      rejectionsByTxId: new Map([
        [
          "cc",
          {
            rejectCode: "E_PHASE_B",
            rejectDetail: "double spend",
            createdAtIso: "2026-01-01T00:00:00.000Z",
          },
        ],
      ]),
      admissionStatusByTxId: new Map([
        ["aa", "queued"],
        ["bb", "validating"],
      ]),
      immutableTxIds: new Set(["aa"]),
      mempoolTxIds: new Set(["bb"]),
      processedMempoolTxIds: new Set<string>(),
      localFinalizationPending: false,
      headerEvidenceByTxId: new Map([
        [
          "aa",
          {
            headerHash: "11".repeat(28),
            headerStatus: "finalized",
            mergeStatus: "finalized",
            confirmedLedgerFinalized: true,
          },
        ],
      ]),
    });

    expect(statuses.map((status) => status.status)).toEqual([
      "committed",
      "accepted",
      "rejected",
    ]);
    expect(statuses[0]).toMatchObject({
      headerStatus: "finalized",
      confirmedLedgerFinalized: true,
    });
  });
});
