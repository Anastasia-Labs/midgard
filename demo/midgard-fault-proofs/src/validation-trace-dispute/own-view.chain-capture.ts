/** The capture the validation-trace dispute's chain view (own-view.chain.ts)
 * reads its own state from: one authenticated raw L1 snapshot of the
 * challenged block's dispute. It scopes the opener, source and case
 * addresses and the fraud-proof record address, and names the histories of
 * the header's thread token, of each case an Open of ours started, of each
 * thread token of those cases and of each receipt their closes mint. Each
 * unit past the thread token is found in the histories of the units before
 * it, so a capture that finds a unit it did not name is taken again with
 * it, and every unit found stays named under the header for the run (a
 * workflow, and this capture with it, is built per run): naming a unit in
 * a capture is what holds its history.
 *
 * Every read is of the next view (workflow/next-state.ts): the scopes with
 * our node's mempool applied, and each unit's history with the mempool
 * transactions touching the unit after the ledger's, in mempool order. A
 * mempool transaction has no inclusion point; it is never evidence. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  bodyRawOutputs,
  type PendingTransaction,
} from "@al-ft/midgard-l1-follower/mempool";
import * as SDK from "@al-ft/midgard-sdk/da";
import { CML, coreToTxOutput, type TxOutput } from "@lucid-evolution/lucid";

import { datumProver } from "../fraud-proof-record-selection.js";
import { type FraudProofNextView, nextViewOf } from "../workflow/next-state.js";
import { historicalTransactions } from "../workflow/raw-l1-family-derivation.derive-retained-state-queue-header-observation-from-raw-l1.js";
import { rawToUtxo } from "../workflow/raw-l1-family-derivation.state-queue-topology.js";
import type {
  FraudProofRawL1Snapshot,
  FraudProofRawL1Transaction,
  FraudProofRawL1Utxo,
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

/** One transaction of a unit's history in the next view: a ledger
 * transaction of the snapshot, or a transaction of our node's mempool. */
export type ValidationTraceHistoryTransaction = Readonly<{
  txHash: string;
  bodyCbor: string;
  witnessSetCbor: string;
  /** The inputs it spends that the capture resolves, as their outputs. */
  resolvedInputs: readonly FraudProofRawL1Utxo[];
  /** The reference inputs it reads, as out-refs. */
  referenceInputs: readonly string[];
  /** Those of its reference inputs the capture resolves, as their outputs. */
  resolvedReferenceInputs: readonly FraudProofRawL1Utxo[];
  /** How new it is: its inclusion slot on the ledger, and after every
   * ledger transaction, in mempool order, while it is pending. */
  rank: bigint;
  /** Whether our node's mempool holds it, not the ledger. */
  pending: boolean;
  /** Its depth on the ledger, the tip one, as the snapshot's admission
   * checks it; none while pending. */
  confirmationDepth: number | undefined;
}>;

/** An output a transaction of the capture paid. */
export type ValidationTraceCapturedOutput = Readonly<{
  outRef: string;
  output: TxOutput;
  transaction: ValidationTraceHistoryTransaction;
}>;

/** The outputs a transaction paid, by its body. */
type Paid = Readonly<{
  bodyCbor: string;
  outputs: readonly Readonly<{ outRef: string; output: TxOutput }>[];
}>;

/** Each output a transaction of a capture paid. */
export type ValidationTraceCapturedOutputs = (
  transaction: ValidationTraceHistoryTransaction,
) => readonly ValidationTraceCapturedOutput[];

/** The transactions of a unit's history in the next view, oldest first:
 * none while the capture does not name the unit. */
export type ValidationTraceUnitHistory = (
  unit: string,
) => readonly ValidationTraceHistoryTransaction[];

/** One capture of the dispute, with the units it names. */
export type ValidationTraceDisputeCapture = Readonly<{
  snapshot: FraudProofRawL1Snapshot;
  /** The snapshot's scopes with our node's mempool applied. */
  view: FraudProofNextView;
  history: ValidationTraceUnitHistory;
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

/** The outputs `transaction` paid, decoded from its body. */
const paidBy = (transaction: ValidationTraceHistoryTransaction): Paid => {
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

/** `transaction` as a history transaction of rank `rank`. */
const ledgerTransaction = (
  transaction: FraudProofRawL1Transaction,
  rank: bigint,
): ValidationTraceHistoryTransaction =>
  Object.freeze({
    txHash: transaction.txHash,
    bodyCbor: transaction.bodyCbor,
    witnessSetCbor: transaction.witnessSetCbor,
    resolvedInputs: transaction.resolvedInputs,
    referenceInputs: transaction.resolvedReferenceInputs.map(
      ({ outRef }) => outRef,
    ),
    resolvedReferenceInputs: transaction.resolvedReferenceInputs,
    rank,
    pending: false,
    confirmationDepth: transaction.confirmationDepth,
  });

/** Whether `raw` holds the unit `unit`. */
const holds = (raw: FraudProofRawL1Utxo, unit: string): boolean =>
  (rawToUtxo(raw).assets[unit] ?? 0n) > 0n;

/**
 * The unit histories of the next view over `snapshot` and `pending`. A
 * pending transaction is in a unit's history when an output it spends or
 * creates holds the unit. What it spends and reads is resolved from the
 * outputs the snapshot holds (its scopes, the inputs its transactions
 * resolve, the outputs its transactions pay) and those earlier pending
 * transactions create; a reference input none of them holds is kept by
 * out-ref alone.
 */
const nextHistories = (
  snapshot: FraudProofRawL1Snapshot,
  view: FraudProofNextView,
): ValidationTraceUnitHistory => {
  const ledger = new Map<
    string,
    readonly ValidationTraceHistoryTransaction[]
  >();
  const ledgerOf = (unit: string) => {
    let found = ledger.get(unit);
    if (found === undefined) {
      found = [...historicalTransactions({ snapshot, unit })]
        .map((transaction) =>
          ledgerTransaction(
            transaction,
            BigInt(transaction.inclusionPoint.slot),
          ),
        )
        .sort((left, right) =>
          left.rank < right.rank ? -1 : left.rank > right.rank ? 1 : 0,
        );
      ledger.set(unit, found);
    }
    return found;
  };
  let pendingRead: ReturnType<typeof resolvePending> | undefined;
  const pendingOf = () => (pendingRead ??= resolvePending(snapshot, view));
  return (unit) =>
    snapshot.historyUnits.includes(unit)
      ? [
          ...ledgerOf(unit),
          ...pendingOf().flatMap(({ transaction, outputs }) =>
            [...transaction.resolvedInputs, ...outputs].some((raw) =>
              holds(raw, unit),
            )
              ? [transaction]
              : [],
          ),
        ]
      : [];
};

/** `view`'s pending transactions as history transactions, in mempool
 * order, each input resolved where the snapshot or an earlier pending
 * transaction holds it, with the outputs each creates. */
const resolvePending = (
  snapshot: FraudProofRawL1Snapshot,
  view: FraudProofNextView,
): readonly Readonly<{
  transaction: ValidationTraceHistoryTransaction;
  outputs: readonly FraudProofRawL1Utxo[];
}>[] => {
  const known = new Map<string, FraudProofRawL1Utxo>();
  for (const { utxos } of snapshot.scopes)
    for (const raw of utxos) known.set(raw.outRef, raw);
  const byHash = new Map(
    snapshot.transactions.map((transaction) => [
      transaction.txHash,
      transaction,
    ]),
  );
  for (const transaction of snapshot.transactions)
    for (const raw of [
      ...transaction.resolvedInputs,
      ...transaction.resolvedReferenceInputs,
    ])
      known.set(raw.outRef, raw);
  const resolve = (outRef: string): FraudProofRawL1Utxo | undefined => {
    const found = known.get(outRef);
    if (found !== undefined) return found;
    const creator = byHash.get(outRef.split("#")[0]!);
    if (creator === undefined) return undefined;
    for (const raw of bodyRawOutputs(creator.txHash, creator.bodyCbor))
      known.set(raw.outRef, raw);
    return known.get(outRef);
  };
  const resolved = (outRefs: readonly string[]) =>
    outRefs.flatMap((outRef) => {
      const raw = resolve(outRef);
      return raw === undefined ? [] : [raw];
    });
  return view.pending.map((pending) => {
    const outputs = pending.creates.map(({ raw }) => raw);
    const transaction = Object.freeze({
      txHash: pending.txHash,
      bodyCbor: pending.bodyCbor,
      witnessSetCbor: pending.witnessSetCbor,
      resolvedInputs: resolved(pending.spends),
      referenceInputs: pending.reads,
      resolvedReferenceInputs: resolved(pending.reads),
      rank: view.createdRank(pending.txHash),
      pending: true,
      confirmationDepth: undefined,
    });
    for (const raw of outputs) known.set(raw.outRef, raw);
    return { transaction, outputs };
  });
};

/** The outputs of ours at the opener (`opener`) or at the source holding
 * the thread token `threadUnit` that its history shows paid, oldest first:
 * each datum's fraud prover is us. */
export const ownStepOutputs = ({
  history,
  outputsOf,
  deployment,
  threadUnit,
  prover,
}: {
  readonly history: ValidationTraceUnitHistory;
  readonly outputsOf: ValidationTraceCapturedOutputs;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly threadUnit: string;
  readonly prover: string;
}) => {
  const [opener, source] = validationTraceComputationSteps(deployment.chain);
  const ours = (step: typeof opener) =>
    history(threadUnit)
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
}: {
  readonly caseHistory: ValidationTraceCaseHistory;
  readonly deployment: ValidationTraceDisputeDeployment;
}) => {
  /** The units found under each header so far. */
  const named = new Map<string, Set<string>>();
  /** The outputs the latest read decoded, by transaction hash. Each read
   * replaces it, so it holds no more than that read does. */
  let latest: ReadonlyMap<string, Paid> = new Map();
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
    pending: readonly PendingTransaction[],
    outputsOf: ValidationTraceCapturedOutputs,
    artifact: ValidationTraceDisputeArtifact,
    prover: string,
  ): ValidationTraceDisputeCapture => {
    const view = nextViewOf({ snapshot, pending });
    const history = nextHistories(snapshot, view);
    const threadUnit = validationTraceThreadUnit(
      deployment,
      artifact.headerHash,
    );
    const eventKey = artifact.claim.claimWitness.event_to_step_membership.key;
    const cases = new Map(
      ownStepOutputs({
        history,
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
                parseOutRefLabel(outRef),
              ),
          ] as const,
      ),
    );
    const threads = [...cases.values()].flatMap((caseUnit) =>
      recordedValidationTraceDefenders({
        transactions: history(caseUnit),
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
    return { snapshot, view, history, outputsOf, threadUnit, cases, threads };
  };
  return async (
    artifact: ValidationTraceDisputeArtifact,
    prover: string,
  ): Promise<ValidationTraceDisputeCapture> => {
    const { headerHash } = artifact;
    const units = named.get(headerHash) ?? new Set<string>();
    named.set(headerHash, units);
    units.add(validationTraceThreadUnit(deployment, headerHash));
    const prior = latest;
    const paid = new Map<string, Paid>();
    const outputsOf: ValidationTraceCapturedOutputs = (transaction) => {
      let entry = paid.get(transaction.txHash);
      if (entry?.bodyCbor !== transaction.bodyCbor) {
        const known = prior.get(transaction.txHash);
        entry =
          known?.bodyCbor === transaction.bodyCbor
            ? known
            : paidBy(transaction);
        paid.set(transaction.txHash, entry);
      }
      return entry.outputs.map((output) => ({ ...output, transaction }));
    };
    latest = paid;
    let found: ValidationTraceDisputeCapture | undefined;
    for (let round = 0; round < ROUNDS; round += 1) {
      const { snapshot, pending } = await capture({
        headerHash,
        scopes,
        historyUnits: [...units],
      });
      found = derive(snapshot, pending, outputsOf, artifact, prover);
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
