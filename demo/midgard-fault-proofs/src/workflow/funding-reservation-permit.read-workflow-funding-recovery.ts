import { CML, coreToUtxo, type TxSigned } from "@lucid-evolution/lucid";

import { readFraudSlashFundingAuthority } from "../remove-fraudulent-block.js";
import { assertRuntimeTransactionBound } from "./funding-reservation-permit.assert-runtime-transaction-bound.js";
import { readValidationDisputeFundingAuthority } from "./funding-reservation-permit.assert-validation-dispute-funding.js";
import { readValidationDisputeMoveAuthority } from "./funding-reservation-permit.assert-validation-dispute-move.js";
import {
  assertCurrentFundingCollateralLimit,
  assertFundingSubmissionAuthority,
  bodySha256,
} from "./funding-reservation-permit.begin-workflow-funding-reservation-action.js";
import {
  actionKind,
  refresh,
  stateForJournal,
} from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import { parseWorkflowFundingSupersededAttempt } from "./funding-reservation-permit.parse-workflow-funding-attempt.js";
import { parseWorkflowFundingPreparedTransition } from "./funding-reservation-permit.parse-workflow-funding-prepared-transition.js";
import { parseStateSnapshot } from "./funding-reservation-permit.parse-workflow-funding-snapshot.js";
import {
  canonicalOutRefs,
  type WorkflowFundingPreparedTransition,
  type WorkflowFundingReservedInput,
  type WorkflowFundingSupersededAttempt,
  WorkflowFundingSupersededAttemptLandedError,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import {
  appendWorkflowJournalEvents,
  commitWithJournal,
  type WorkflowJournalWriter,
} from "./funding-reservation-permit.workflow-journal-writer.js";
import {
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
} from "./journal.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import { supersededAttemptReadSchedule } from "./superseded-attempt-read-schedule.js";
import {
  workflowPreflightTransaction,
  workflowTransactionCollateralInputOutRefs,
  workflowTransactionInputOutRefs,
} from "./transaction-boundary.js";

const producedFundingInputs = ({
  signed,
  walletAddress,
  allowEmpty,
}: {
  readonly signed: TxSigned;
  readonly walletAddress: string;
  readonly allowEmpty: boolean;
}): readonly WorkflowFundingReservedInput[] => {
  const body = signed.toTransaction().body();
  const outputs = body.outputs();
  const transactionHash = signed.toHash().toLowerCase();
  const produced: WorkflowFundingReservedInput[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    if (output.address().to_bech32() !== walletAddress) continue;
    const utxo = CML.TransactionUnspentOutput.new(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(transactionHash),
        BigInt(index),
      ),
      output,
    );
    const decoded = coreToUtxo(utxo);
    const assets = Object.entries(decoded.assets)
      .filter(([unit]) => unit !== "lovelace")
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([unit, quantity]) =>
        Object.freeze({ unit, quantity: quantity.toString() }),
      );
    produced.push(
      Object.freeze({
        outRef: `${transactionHash}#${index.toString()}`,
        role: "funding" as const,
        lovelace: decoded.assets.lovelace!.toString(),
        assets: Object.freeze(assets),
      }),
    );
  }
  if (produced.length === 0 && !allowEmpty) {
    throw new Error("production transaction omitted reserved-wallet change");
  }
  return Object.freeze(produced);
};

/** Records a signed attempt of `action` with its preflight and intent
 * (`events`), together when the port keeps the workflow journal. */
export const prepareWorkflowFundingReservationTransaction = async ({
  writer,
  action,
  preflight,
  events,
}: {
  readonly writer: WorkflowJournalWriter;
  readonly action: FraudProofWorkflowAction;
  readonly preflight: object;
  readonly events: readonly [
    FraudProofWorkflowJournalEvent,
    FraudProofWorkflowJournalEvent,
  ];
}): Promise<void> => {
  const [passed, intent] = events;
  if (
    passed.kind !== "preflight_passed" ||
    intent.kind !== "submission_intent" ||
    passed.actionId !== action.actionId ||
    intent.actionId !== action.actionId ||
    intent.txHash !== passed.txHash ||
    journalJsonDigest(intent.actionInput) !== journalJsonDigest(action.input)
  )
    throw new Error("funding attempt changed the evaluated workflow action");
  const state = stateForJournal(writer.journal);
  if (state === undefined) {
    await appendWorkflowJournalEvents(writer, events);
    return;
  }
  const signed = workflowPreflightTransaction(preflight);
  if (signed === undefined) {
    throw new Error(
      "production preflight omitted its captured signed transaction",
    );
  }
  const kind = actionKind(action);
  if (state.currentActionKind !== kind) {
    throw new Error(
      "production funding action changed after reservation admission",
    );
  }
  const candidates = canonicalOutRefs(
    state.currentFundingOutRefs,
    "reserved funding inputs",
  );
  const collateralCandidates = canonicalOutRefs(
    state.currentCollateralOutRefs,
    "reserved collateral inputs",
  );
  const bodyInputs = [...workflowTransactionInputOutRefs(signed)].sort();
  const collateralOutRefs = [
    ...workflowTransactionCollateralInputOutRefs(signed),
  ].sort();
  const fundingOutRefs = Object.freeze(
    bodyInputs.filter((outRef) => candidates.includes(outRef)),
  );
  if (
    bodyInputs.some((outRef) => collateralCandidates.includes(outRef)) ||
    collateralOutRefs.some((outRef) => !collateralCandidates.includes(outRef))
  )
    throw new Error(
      "signed transaction changed reserved ordinary/collateral separation",
    );
  await assertRuntimeTransactionBound({
    state,
    action,
    signed,
    bodyInputs,
    fundingOutRefs,
    collateralOutRefs,
  });
  const transactionHash = signed.toHash().toLowerCase();
  if (intent.txHash !== transactionHash)
    throw new Error("funding attempt changed its signed transaction");
  const transition = Object.freeze({
    actionKind: kind,
    signedTransactionCborHex: signed.toTransaction().to_cbor_hex(),
    transactionHash,
    transactionBodySha256: bodySha256(signed),
    consumedOutRefs: fundingOutRefs,
    producedInputs: producedFundingInputs({
      signed,
      walletAddress: state.snapshot.walletAddress,
      // A validated bond-backed slash spends no reserved wallet funding and
      // may pay a different authenticated prover. Its unspent reservation
      // inputs remain owned by the caller; the reward is not caller change.
      // A dispute thread move spends only its thread, the wallet only lending
      // collateral.
      allowEmpty:
        readFraudSlashFundingAuthority(signed) !== null ||
        readValidationDisputeMoveAuthority(signed) !== null,
    }),
  });
  const landed = await state.port.readLandedSupersededAttempt({
    actionId: action.actionId,
    signedTransactionCborHex: transition.signedTransactionCborHex,
  });
  if (landed !== null) {
    // Read it at once: the next pass adopts it.
    supersededAttemptReadSchedule.forget(landed);
    throw new WorkflowFundingSupersededAttemptLandedError(landed);
  }
  state.snapshot = parseStateSnapshot(
    state,
    await commitWithJournal({
      port: state.port,
      writer,
      events,
      change: async ({ journal, entries }) =>
        await state.port.prepare({
          expectedRevision: state.snapshot.revision,
          transition,
          attempt: {
            workflowId: entries.at(-1)!.workflowId,
            intentSequence: entries.at(-1)!.sequence,
            category: state.category,
            actionId: action.actionId,
            actionInput: action.input,
          },
          journal,
        }),
    }),
  );
  state.pendingTransactionHash = transactionHash;
  state.idleReleaseAuthorized = false;
  state.preparedTransaction = Object.freeze({
    signed,
    cborHex: transition.signedTransactionCborHex,
  });
};

export const assertWorkflowFundingReservationReadyToSubmit = async ({
  journal,
  transactionHash,
}: {
  readonly journal: object;
  readonly transactionHash: string;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
  assertFundingSubmissionAuthority(state);
  await state.port.assertSubmissionAuthority?.();
  const expectedRevision = state.snapshot.revision;
  const expectedPending = state.pendingTransactionHash;
  await refresh(state);
  assertFundingSubmissionAuthority(state);
  await state.port.assertSubmissionAuthority?.();
  assertCurrentFundingCollateralLimit(state);
  if (state.preparedTransaction !== undefined) {
    const { signed, cborHex } = state.preparedTransaction;
    if (
      signed.toHash().toLowerCase() !== transactionHash ||
      signed.toTransaction().to_cbor_hex() !== cborHex
    ) {
      throw new Error("prepared funding transaction changed before submission");
    }
    readFraudSlashFundingAuthority(signed);
    readValidationDisputeFundingAuthority(signed);
    readValidationDisputeMoveAuthority(signed);
  }
  if (
    state.snapshot.state !== "active" ||
    state.snapshot.revision !== expectedRevision ||
    expectedPending !== transactionHash ||
    state.pendingTransactionHash !== transactionHash
  ) {
    throw new Error("production funding reservation changed before submission");
  }
};

/** Whether a funding reservation is bound to the journal. Only then does the
 * reservation store keep a replacement mutually exclusive with an attempt
 * superseded without retirement. */
export const workflowJournalHasFundingReservation = (journal: object) =>
  stateForJournal(journal) !== undefined;

/** The reservation's pending signed attempt, read without requiring its
 * already-spent inputs to remain live. */
export const readWorkflowFundingRecovery = async (
  journal: object,
): Promise<
  Readonly<{ transition: WorkflowFundingPreparedTransition | null }>
> => {
  const state = stateForJournal(journal);
  if (state === undefined) return { transition: null };
  const raw = await state.port.readPendingTransition();
  const transition =
    raw === null ? null : parseWorkflowFundingPreparedTransition(raw);
  state.pendingTransactionHash = transition?.transactionHash;
  return Object.freeze({ transition });
};

/** Every superseded attempt of the bound reservation, retired or not. */
export const readLegacyWorkflowFundingAbandonedTransactions = async (
  journal: object,
): Promise<readonly WorkflowFundingSupersededAttempt[]> => {
  const state = stateForJournal(journal);
  const saved = (await state?.port.readLegacyAbandonedTransactions?.()) ?? [];
  return saved.map(parseWorkflowFundingSupersededAttempt);
};

/** The exact signed bytes of `transactionHash` as the bound reservation
 * recorded them, or null without a reservation or such a record. */
export const readWorkflowFundingRecordedSignedTransaction = async (
  journal: object,
  transactionHash: string,
): Promise<string | null> => {
  const state = stateForJournal(journal);
  if (state === undefined) return null;
  const signed = await state.port.readRecordedSignedTransaction({
    transactionHash,
  });
  if (signed !== null && typeof signed !== "string")
    throw new Error("funding store returned malformed signed bytes");
  return signed;
};
