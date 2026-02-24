import { describe, expect, it } from "vitest";
import { resolveTxStatus } from "@/commands/tx-status.js";

describe("resolveTxStatus", () => {
  it("returns rejected when rejection entry exists", () => {
    const status = resolveTxStatus({
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
      txIdHex: "ab",
      rejection: null,
      inImmutable: true,
      inMempool: false,
      inProcessedMempool: false,
      localFinalizationPending: false,
    });

    expect(status.status).toBe("committed");
  });

  it("returns pending_commit when tx is in processed mempool", () => {
    const status = resolveTxStatus({
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
      txIdHex: "ab",
      rejection: null,
      inImmutable: false,
      inMempool: false,
      inProcessedMempool: false,
      localFinalizationPending: false,
    });

    expect(status.status).toBe("not_found");
  });
});
