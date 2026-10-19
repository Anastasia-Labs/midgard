/**
 * Drives one step submit turn by turn the way the watcher's recovery runs a
 * step that needs field carriage: each turn hands the submit a single
 * pre-submit boundary (the recovery passes the same boundary for chunk
 * publication, certification and the step) and captures the first
 * transaction that reaches it. A captured carriage transaction is submitted,
 * as the workflow's prerequisite actions do, and the next turn resumes. The
 * turn whose captured transaction spends the thread input is the step; it is
 * submitted and the drive ends.
 */
import type { Emulator, LucidEvolution } from "@lucid-evolution/lucid";

import { fieldPreimageCertificateAddress } from "../../src/field-opening.js";
import {
  captureLocallyEvaluatedTransaction,
  type FraudProofPreSubmitBoundary,
  type LocallyEvaluatedTransaction,
  submitCapturedTransaction,
  workflowTransactionInputOutRefs,
} from "../../src/workflow/transaction-boundary.js";

export type CapturedStepDrive = Readonly<{
  /** Carriage transactions captured and submitted before the step. */
  prerequisites: readonly LocallyEvaluatedTransaction[];
  /** The captured and submitted step transaction. */
  step: LocallyEvaluatedTransaction;
}>;

export const driveCapturedStep = async ({
  emulator,
  threadOutRef,
  maxTurns,
  invoke,
}: {
  readonly emulator: Emulator;
  readonly threadOutRef: string;
  readonly maxTurns: number;
  readonly invoke: (boundary: FraudProofPreSubmitBoundary) => Promise<unknown>;
}): Promise<CapturedStepDrive> => {
  const prerequisites: LocallyEvaluatedTransaction[] = [];
  for (let turn = 0; turn < maxTurns; turn += 1) {
    const captured = await captureLocallyEvaluatedTransaction(invoke);
    await submitCapturedTransaction(captured);
    emulator.awaitBlock();
    if (
      workflowTransactionInputOutRefs(captured.signed).includes(threadOutRef)
    ) {
      return Object.freeze({
        prerequisites: Object.freeze(prerequisites),
        step: captured,
      });
    }
    prerequisites.push(captured);
  }
  throw new Error(
    `no captured transaction spent thread input ${threadOutRef} within ${maxTurns.toString()} turns; ` +
      `${prerequisites.length.toString()} carriage transactions were captured instead`,
  );
};

/** Field-preimage certificate tokens held at the certificate address. */
export const fieldPreimageCertificateCount = async ({
  lucid,
  certificatePolicyId,
}: {
  readonly lucid: LucidEvolution;
  readonly certificatePolicyId: string;
}): Promise<bigint> =>
  (
    await lucid.utxosAt(
      fieldPreimageCertificateAddress({
        network: lucid.config().network!,
        certificatePolicyId,
      }),
    )
  ).reduce(
    (total, utxo) =>
      total +
      Object.entries(utxo.assets)
        .filter(([unit]) => unit.startsWith(certificatePolicyId))
        .reduce((sum, [, quantity]) => sum + quantity, 0n),
    0n,
  );
