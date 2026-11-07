import { describe, expect, it } from "vitest";

import {
  publicSubmissionDiagnostic,
  publicTxStatusPresentation,
} from "../src/commands/listen-public-error-presentation.js";
import { resolveTxStatus } from "../src/commands/tx-status.js";

const unsafe = "arbitrary private provider words cHJpdmF0ZS1rZXktbWF0ZXJpYWw=";
describe("public error presentation", () => {
  it("omits arbitrary persisted rejection detail while preserving authoritative status/code/identity/time and private diagnostics", () => {
    const status = resolveTxStatus({
      txIdHex: "ab".repeat(32),
      rejection: {
        rejectCode: "E_KNOWN_REJECTION",
        rejectDetail: unsafe,
        createdAtIso: "2026-10-13T00:00:00.000Z",
      },
      admissionStatus: "rejected",
      inImmutable: false,
      inMempool: false,
      inProcessedMempool: false,
      localFinalizationPending: false,
    });
    const publicBody = publicTxStatusPresentation(status, "preprod-public");
    expect(publicBody).toMatchObject({
      txId: "ab".repeat(32),
      status: "rejected",
      reasonCode: "E_KNOWN_REJECTION",
      timestamps: { createdAt: "2026-10-13T00:00:00.000Z" },
    });
    expect(publicBody).not.toHaveProperty("reasonDetail");
    expect(JSON.stringify(publicBody)).not.toContain(unsafe);
    expect(publicTxStatusPresentation(status, "preprod-testing")).toBe(status);
  });
  it("publishes the declared submission code instead of any free-form diagnostic, keeping explicit private behavior", () => {
    expect(
      publicSubmissionDiagnostic(unsafe, "E_INVALID_CANONICAL_TX", "mainnet"),
    ).toBe("E_INVALID_CANONICAL_TX");
    expect(
      publicSubmissionDiagnostic(
        unsafe,
        "E_ADMISSION_WRITER_UNAVAILABLE",
        "preprod-public",
      ),
    ).toBe("E_ADMISSION_WRITER_UNAVAILABLE");
    expect(
      publicSubmissionDiagnostic(
        unsafe,
        "E_INVALID_CANONICAL_TX",
        "preprod-testing",
      ),
    ).toBe(unsafe);
  });
});
