import { afterEach, expect, vi } from "vitest";

import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofWorkflowTerminalVerifier,
} from "../../src/workflow/orchestrator.js";

/**
 * A read of a superseded attempt's signed bytes no row here expects. The
 * workflow holds on a read that fails instead of failing, so the read is
 * counted: every row of a suite that imports this module fails if it made
 * one.
 */
export const noSignedRead = vi.fn(async (): Promise<never> => {
  throw new Error("no signed transaction read expected");
});

afterEach(() => {
  const reads = noSignedRead.mock.calls.length;
  noSignedRead.mockClear();
  expect(reads, "signed transaction reads").toBe(0);
});

/** A terminal verifier for a workflow that reads no superseded attempt. */
export const terminalVerifierOf = (
  verify: FraudProofWorkflowTerminalVerifier["verify"],
): FraudProofWorkflowTerminalVerifier => ({
  verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  verify,
  observeSignedTransaction: noSignedRead,
});
