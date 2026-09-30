import { expect } from "vitest";

import {
  submitTransactionOutputNonCanonicalStep03,
  transactionOutputScanControlData,
} from "../src/transaction-output-non-canonical/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import {
  readOutputScanState,
  scanStateOf,
  scanWindowAt,
  submitOutputStep03Raw,
} from "./support/transaction-output-non-canonical-emulator.js";
import {
  controlCbor,
  coverage,
  type Evidence,
  record,
  type Registered,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";

/**
 * Drives step 03 to its terminal through fresh builder calls. After the
 * first transition it verifies the on-chain checkpoint is exactly the trace
 * position the evidence reproduces, refuses the successor and checkpoint
 * seams at that checkpoint, then resumes from evidence derived afresh from
 * the retained field bytes.
 */
export const scanToTerminal = async ({
  registered,
  threadOutRef,
  evidence,
  rederive,
  label,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
}: {
  readonly registered: Registered;
  readonly threadOutRef: string;
  readonly evidence: Evidence;
  readonly rederive: () => Evidence;
  readonly label: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
}) => {
  let current = threadOutRef;
  let scans = 0;
  let active = evidence;
  for (;;) {
    const scan = await captureEmulatorSubmission(
      registered.harness.emulator,
      () =>
        submitTransactionOutputNonCanonicalStep03({
          lucid: registered.harness.proverLucid,
          contracts: registered.contracts,
          categoryId: registered.category.categoryId,
          signer: registered.harness.proverSigner,
          threadOutRef: current,
          evidence: active,
          nativeTxCompactCbor,
          witnessSetCompactCbor,
          referenceScriptUtxo: registered.references[2]!,
        }),
    );
    scans += 1;
    current = scan.result.nextThreadOutRef;
    if (scans === 1) record(`${label}-scan-first`, scan.measurement);
    if (scan.result.terminal) {
      record(`${label}-scan-final`, scan.measurement);
      return { threadOutRef: current, scans };
    }
    if (scans === 1) {
      // A real checkpoint: the thread now carries the trace position after
      // one window, and it must be exactly what the evidence reproduces.
      const observed = await readOutputScanState(
        registered.common(current, 2),
        2,
      );
      expect(observed.outcome).toBe(0n);
      expect(observed.control.cursor).toBeGreaterThan(0n);
      expect(controlCbor(observed.control)).toBe(
        controlCbor(
          transactionOutputScanControlData(evidence.scanControls[1]!),
        ),
      );
      const window = scanWindowAt(
        Buffer.from(evidence.itemHex, "hex"),
        observed.control,
      );
      // Wrong successor while scanning: the self-loop may not hand over early.
      await expectOnchainRefusal(
        async () =>
          await submitOutputStep03Raw({
            ...registered.common(current, 2),
            window,
            nextState: scanStateOf(evidence, 2, 0n),
            nextStepIndex: 3,
          }),
      );
      coverage.seamMutated("step_03_successor");
      // Forged checkpoint: a canonical outcome claimed before the terminal.
      await expectOnchainRefusal(
        async () =>
          await submitOutputStep03Raw({
            ...registered.common(current, 2),
            window,
            nextState: { ...scanStateOf(evidence, 2, 0n), outcome: 1n },
            nextStepIndex: 3,
          }),
      );
      // Replayed checkpoint: re-emitting the consumed position is no progress.
      await expectOnchainRefusal(
        async () =>
          await submitOutputStep03Raw({
            ...registered.common(current, 2),
            window,
            nextState: scanStateOf(evidence, 1, 0n),
            nextStepIndex: 2,
          }),
      );
      coverage.seamMutated("scan_checkpoint");
      // Resume: the remaining windows are driven from evidence derived afresh.
      active = rederive();
      expect(active).toStrictEqual(evidence);
      coverage.resumed();
    }
  }
};
