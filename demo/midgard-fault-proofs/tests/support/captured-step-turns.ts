/**
 * Drives one step build turn by turn the way the watcher's recovery runs a
 * step that needs field carriage: each turn calls the builder once and takes
 * the one transaction it returns, a chunk publication, a certificate or the
 * step itself. A carriage transaction is submitted, as the workflow's
 * prerequisite actions do, and the next turn builds again, finding it on
 * chain. The turn whose transaction spends the thread input is
 * the step; it is submitted and the drive ends.
 */
import {
  coreToTxOutput,
  type Emulator,
  type LucidEvolution,
} from "@lucid-evolution/lucid";

import { isPrerequisiteTurn } from "../../src/fault-proof-turn.js";
import { fieldPreimageCertificateAddress } from "../../src/field-opening.certify-fault-proof-field-carriage.js";
import {
  type LocallyEvaluatedTransaction,
  submitCapturedTransaction,
  workflowTransactionInputOutRefs,
} from "../../src/workflow/transaction-boundary.js";

export type CapturedStepDrive = Readonly<{
  /** Carriage transactions built and submitted before the step. */
  prerequisites: readonly LocallyEvaluatedTransaction[];
  /** The built and submitted step transaction. */
  step: LocallyEvaluatedTransaction;
  /** What each turn built, in order: a prerequisite's kind, or "step". */
  turns: readonly string[];
}>;

const builtTransaction = (
  built: unknown,
): { kind: string; transaction: LocallyEvaluatedTransaction } => {
  if (isPrerequisiteTurn(built))
    return { kind: built.kind, transaction: built.transaction };
  if (typeof built === "object" && built !== null && "transaction" in built)
    return {
      kind: "step",
      transaction: (built as { transaction: LocallyEvaluatedTransaction })
        .transaction,
    };
  if (typeof built === "object" && built !== null && "signed" in built)
    return { kind: "step", transaction: built as LocallyEvaluatedTransaction };
  throw new Error("builder returned no transaction");
};

export const driveCapturedStep = async ({
  emulator,
  threadOutRef,
  maxTurns,
  invoke,
}: {
  readonly emulator: Emulator;
  readonly threadOutRef: string;
  readonly maxTurns: number;
  readonly invoke: () => Promise<unknown>;
}): Promise<CapturedStepDrive> => {
  const prerequisites: LocallyEvaluatedTransaction[] = [];
  const turns: string[] = [];
  for (let turn = 0; turn < maxTurns; turn += 1) {
    const { kind, transaction } = builtTransaction(await invoke());
    await submitCapturedTransaction(transaction);
    emulator.awaitBlock();
    if (
      workflowTransactionInputOutRefs(transaction.signed).includes(threadOutRef)
    ) {
      turns.push("step");
      return Object.freeze({
        prerequisites: Object.freeze(prerequisites),
        step: transaction,
        turns: Object.freeze(turns),
      });
    }
    turns.push(kind);
    prerequisites.push(transaction);
  }
  throw new Error(
    `no built transaction spent thread input ${threadOutRef} within ${maxTurns.toString()} turns; ` +
      `${prerequisites.length.toString()} carriage transactions were built instead`,
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

/** The out-ref of the built transaction's output holding `unit`. */
export const capturedOutputHolding = (
  captured: LocallyEvaluatedTransaction,
  unit: string,
): string => {
  const outputs = captured.signed.toTransaction().body().outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    if ((coreToTxOutput(outputs.get(index)).assets[unit] ?? 0n) > 0n)
      return `${captured.txHash}#${index.toString()}`;
  }
  throw new Error(`built transaction ${captured.txHash} holds no ${unit}`);
};
