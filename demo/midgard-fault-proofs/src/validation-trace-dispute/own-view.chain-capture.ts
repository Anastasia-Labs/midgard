/** The capture the validation-trace dispute's chain view (own-view.chain.ts)
 * reads its own state from: one authenticated raw L1 snapshot of the
 * challenged block's dispute. It scopes the opener, source and case
 * addresses and the fraud-proof record address, and names the histories of
 * the header's thread token, of each case an Open of ours started, of each
 * thread token of those cases and of each receipt their closes mint. Each
 * unit past the thread token is found in the histories of the units before
 * it, so a capture that finds a unit it did not name is taken again with
 * it, and every unit found stays named under the header for the process's
 * life: naming a unit in a capture is what holds its history. Each capture
 * admits and decodes only the transactions the header's previous capture
 * did not hold in the same bytes: the histories grow by a transaction or
 * two per move, and a long resolution makes hundreds of moves. */
import * as SDK from "@al-ft/midgard-sdk/da";
import { CML, coreToTxOutput, type TxOutput } from "@lucid-evolution/lucid";

import { datumProver } from "../fraud-proof-record-selection.js";
import { parseOwnOutRef } from "../workflow/own-outputs.js";
import { historicalTransactions } from "../workflow/raw-l1-family-derivation.derive-retained-state-queue-header-observation-from-raw-l1.js";
import type {
  FraudProofRawL1Snapshot,
  FraudProofRawL1Transaction,
} from "../workflow/raw-l1-snapshot.js";
import type { ValidationTraceDisputeArtifact } from "./artifact.js";
import {
  recordedValidationTraceDefenders,
  type ValidationTraceCaseHistory,
  validationTraceThreadTokenName,
} from "./case-history.js";
import { validationTraceComputationSteps } from "./removal.js";
import {
  type ValidationTraceDisputeDeployment,
  validationTraceThreadUnit,
} from "./transactions.js";

/** An output a transaction of the capture paid. */
export type ValidationTraceCapturedOutput = Readonly<{
  outRef: string;
  output: TxOutput;
  transaction: FraudProofRawL1Transaction;
}>;

/** The outputs a transaction paid, by its body. */
type Paid = Readonly<{
  bodyCbor: string;
  outputs: readonly Readonly<{ outRef: string; output: TxOutput }>[];
}>;

/** Each output a transaction of a capture paid. */
export type ValidationTraceCapturedOutputs = (
  transaction: FraudProofRawL1Transaction,
) => readonly ValidationTraceCapturedOutput[];

/** One capture of the dispute, with the units it names. */
export type ValidationTraceDisputeCapture = Readonly<{
  snapshot: FraudProofRawL1Snapshot;
  outputsOf: ValidationTraceCapturedOutputs;
  threadUnit: string;
  /** Our case units, by the out-ref of the pending claim each started at. */
  cases: ReadonlyMap<string, string>;
  /** Each thread token of our cases, with the receipt its close mints. */
  threads: readonly Readonly<{ unit: string; receiptUnit: string }>[];
}>;

/** The most captures one read takes: the thread token's history names our
 * cases, theirs name the thread tokens and receipts, and a further round
 * only follows a defense that landed between two captures. */
const ROUNDS = 4;

const slotOf = (transaction: FraudProofRawL1Transaction): bigint =>
  BigInt(transaction.inclusionPoint.slot);

/** The outputs `transaction` paid, decoded from its body. */
const paidBy = (transaction: FraudProofRawL1Transaction): Paid => {
  const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
  const outputs = body.outputs();
  const found = Array.from({ length: outputs.len() }, (_, index) => {
    const output = outputs.get(index);
    const decoded = coreToTxOutput(output);
    output.free();
    return {
      outRef: `${transaction.txHash}#${index.toString()}`,
      output: decoded,
    };
  });
  outputs.free();
  body.free();
  return { bodyCbor: transaction.bodyCbor, outputs: found };
};

/** The transactions of `unit`'s history in the capture, oldest first, or
 * none while the capture does not name it. */
export const unitTransactions = (
  snapshot: FraudProofRawL1Snapshot,
  unit: string,
): readonly FraudProofRawL1Transaction[] =>
  snapshot.historyUnits.includes(unit)
    ? [...historicalTransactions({ snapshot, unit })].sort((left, right) =>
        slotOf(left) < slotOf(right)
          ? -1
          : slotOf(left) > slotOf(right)
            ? 1
            : 0,
      )
    : [];

/** The outputs of ours at the opener (`opener`) or at the source holding
 * the thread token `threadUnit` that its history shows paid, oldest first:
 * each datum's fraud prover is us. */
export const ownStepOutputs = ({
  snapshot,
  outputsOf,
  deployment,
  threadUnit,
  prover,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly outputsOf: ValidationTraceCapturedOutputs;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly threadUnit: string;
  readonly prover: string;
}) => {
  const [opener, source] = validationTraceComputationSteps(deployment.chain);
  const ours = (step: typeof opener) =>
    unitTransactions(snapshot, threadUnit)
      .flatMap(outputsOf)
      .filter(
        ({ output }) =>
          output.address === step.address &&
          output.assets[threadUnit] === 1n &&
          datumProver(output.datum, step.datumSchema) === prover,
      );
  return { opener: ours(opener), source: ours(source) };
};

/** The capture of the dispute over `artifact`'s block, for `prover`. */
export const validationTraceDisputeCapture = ({
  caseHistory,
  deployment,
  named,
}: {
  readonly caseHistory: ValidationTraceCaseHistory;
  readonly deployment: ValidationTraceDisputeDeployment;
  /** The units found under each header so far, kept for the process. */
  readonly named: Map<string, Set<string>>;
}) => {
  /** What the latest capture of each header admitted, and the outputs its
   * reads decoded, by transaction hash. Each capture replaces its header's
   * entry, so it holds no more than that capture does. */
  const latest = new Map<
    string,
    Readonly<{
      admitted: ReadonlyMap<string, FraudProofRawL1Transaction>;
      paid: ReadonlyMap<string, Paid>;
    }>
  >();
  if (caseHistory.snapshot === undefined)
    throw new Error("validation-trace chain view needs the raw L1 capture");
  const capture = caseHistory.snapshot.bind(caseHistory);
  const steps = validationTraceComputationSteps(deployment.chain);
  const scopes = [
    ...steps.map(({ role, address }) => ({ role, address })),
    {
      role: "permanent_proof_token" as const,
      address: deployment.fraudProof.spendingScriptAddress,
    },
  ];
  /** The units `snapshot`'s histories show beyond the thread token. */
  const derive = (
    snapshot: FraudProofRawL1Snapshot,
    outputsOf: ValidationTraceCapturedOutputs,
    artifact: ValidationTraceDisputeArtifact,
    prover: string,
  ): ValidationTraceDisputeCapture => {
    const threadUnit = validationTraceThreadUnit(
      deployment,
      artifact.headerHash,
    );
    const eventKey = artifact.claim.claimWitness.event_to_step_membership.key;
    const cases = new Map(
      ownStepOutputs({
        snapshot,
        outputsOf,
        deployment,
        threadUnit,
        prover,
      }).source.map(
        ({ outRef }) =>
          [
            outRef,
            deployment.chain.case.policyId +
              SDK.validationCaseId(
                artifact.headerHash,
                eventKey,
                parseOwnOutRef(outRef),
              ),
          ] as const,
      ),
    );
    const threads = [...cases.values()].flatMap((caseUnit) =>
      recordedValidationTraceDefenders({
        transactions: unitTransactions(snapshot, caseUnit),
        caseUnit,
      }).map((defenderVkh) => {
        const name = validationTraceThreadTokenName(
          caseUnit.slice(56),
          defenderVkh,
        );
        return {
          unit: deployment.chain.case.policyId + name,
          receiptUnit: deployment.chain.award.policyId + name,
        };
      }),
    );
    return { snapshot, outputsOf, threadUnit, cases, threads };
  };
  return async (
    artifact: ValidationTraceDisputeArtifact,
    prover: string,
  ): Promise<ValidationTraceDisputeCapture> => {
    const { headerHash } = artifact;
    const units = named.get(headerHash) ?? new Set<string>();
    named.set(headerHash, units);
    units.add(validationTraceThreadUnit(deployment, headerHash));
    const prior = latest.get(headerHash);
    const paid = new Map<string, Paid>();
    const outputsOf: ValidationTraceCapturedOutputs = (transaction) => {
      let entry = paid.get(transaction.txHash);
      if (entry?.bodyCbor !== transaction.bodyCbor) {
        const known = prior?.paid.get(transaction.txHash);
        entry =
          known?.bodyCbor === transaction.bodyCbor
            ? known
            : paidBy(transaction);
        paid.set(transaction.txHash, entry);
      }
      return entry.outputs.map((output) => ({ ...output, transaction }));
    };
    let admitted = prior?.admitted;
    let found: ValidationTraceDisputeCapture | undefined;
    for (let round = 0; round < ROUNDS; round += 1) {
      const snapshot = await capture({
        headerHash,
        scopes,
        historyUnits: [...units],
        admitted,
      });
      admitted = new Map(
        snapshot.transactions.map(
          (transaction) => [transaction.txHash, transaction] as const,
        ),
      );
      latest.set(headerHash, { admitted, paid });
      found = derive(snapshot, outputsOf, artifact, prover);
      const wanted = [
        ...found.cases.values(),
        ...found.threads.flatMap(({ unit, receiptUnit }) => [
          unit,
          receiptUnit,
        ]),
      ].filter((unit) => !snapshot.historyUnits.includes(unit));
      if (wanted.length === 0) return found;
      for (const unit of wanted) units.add(unit);
    }
    return found!;
  };
};
