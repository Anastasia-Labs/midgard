import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML, coreToUtxo, type TxSigned } from "@lucid-evolution/lucid";

import { readFraudSlashFundingAuthority } from "../remove-fraudulent-block.js";
import { assertRuntimeTransactionBound } from "./funding-reservation-permit.assert-runtime-transaction-bound.js";
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
import {
  parseWorkflowFundingAbandonmentHandoff,
  parseWorkflowFundingCompletionHandoff,
} from "./funding-reservation-permit.parse-workflow-funding-abandonment-handoff.js";
import {
  parseWorkflowFundingPreparedTransition,
  parseWorkflowFundingSubmissionHandoff,
} from "./funding-reservation-permit.parse-workflow-funding-prepared-transition.js";
import { parseStateSnapshot } from "./funding-reservation-permit.reconcile-workflow-funding-submission-handoff.js";
import {
  canonicalOutRefs,
  exact,
  type WorkflowFundingAbandonmentHandoff,
  type WorkflowFundingCompletionHandoff,
  type WorkflowFundingPreparedTransition,
  type WorkflowFundingReservedInput,
  type WorkflowFundingSubmissionHandoff,
} from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import { journalJsonDigest } from "./journal.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
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

export const prepareWorkflowFundingReservationTransaction = async ({
  journal,
  action,
  preflight,
  handoff,
}: {
  readonly journal: object;
  readonly action: FraudProofWorkflowAction;
  readonly preflight: object;
  readonly handoff: WorkflowFundingSubmissionHandoff;
}): Promise<void> => {
  const state = stateForJournal(journal);
  if (state === undefined) return;
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
  const admittedHandoff = parseWorkflowFundingSubmissionHandoff(handoff);
  if (
    admittedHandoff.identity.deploymentFingerprint !==
      state.snapshot.deploymentFingerprint ||
    admittedHandoff.identity.decisionDigest !== state.snapshot.decisionDigest ||
    admittedHandoff.submissionIntent.txHash !== transactionHash ||
    admittedHandoff.submissionIntent.actionId !== action.actionId ||
    journalJsonDigest(admittedHandoff.submissionIntent.actionInput) !==
      journalJsonDigest(action.input)
  )
    throw new Error("funding handoff changed the evaluated workflow action");
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
      allowEmpty: readFraudSlashFundingAuthority(signed) !== null,
    }),
  });
  state.snapshot = parseStateSnapshot(
    state,
    await state.port.prepare({
      expectedRevision: state.snapshot.revision,
      transition,
      handoff: admittedHandoff,
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
  if ((await state.port.readAbandonmentHandoff()) !== null)
    throw new Error(
      "funding abandonment outcome awaits journal acknowledgment",
    );
  const expectedRevision = state.snapshot.revision;
  const expectedPending = state.pendingTransactionHash;
  await refresh(state);
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

/** Read durable signed material without requiring its already-spent inputs to remain live. */
export const readWorkflowFundingRecovery = async (
  journal: object,
): Promise<
  Readonly<{
    transition: WorkflowFundingPreparedTransition | null;
    submissionHandoff: WorkflowFundingSubmissionHandoff | null;
    completionHandoff: WorkflowFundingCompletionHandoff | null;
    abandonmentHandoff: WorkflowFundingAbandonmentHandoff | null;
  }>
> => {
  const state = stateForJournal(journal);
  if (state === undefined)
    return {
      transition: null,
      submissionHandoff: null,
      completionHandoff: null,
      abandonmentHandoff: null,
    };
  const rawTransition = await state.port.readPendingTransition();
  let transition =
    rawTransition === null
      ? null
      : parseWorkflowFundingPreparedTransition(rawTransition);
  const rawSubmission = await state.port.readPendingHandoff();
  let submissionHandoff: WorkflowFundingSubmissionHandoff | null = null;
  if (rawSubmission !== null) {
    const record = exact(
      rawSubmission,
      ["transition", "handoff"],
      "funding pending handoff record",
    );
    const savedTransition = parseWorkflowFundingPreparedTransition(
      record.transition,
    );
    if (
      transition === null ||
      computeDeploymentManifestJsonDigest(savedTransition) !==
        computeDeploymentManifestJsonDigest(transition)
    )
      throw new Error(
        "funding handoff differs from its pending signed transaction",
      );
    submissionHandoff = parseWorkflowFundingSubmissionHandoff(record.handoff);
    if (
      submissionHandoff.submissionIntent.txHash !== transition.transactionHash
    )
      throw new Error("funding handoff changed its transaction hash");
  }
  const rawAbandonment = await state.port.readAbandonmentHandoff();
  let abandonmentHandoff: WorkflowFundingAbandonmentHandoff | null = null;
  if (rawAbandonment !== null) {
    if (transition !== null || submissionHandoff !== null)
      throw new Error(
        "funding recovery has both pending and abandoned transactions",
      );
    const record = exact(
      rawAbandonment,
      ["transition", "handoff"],
      "funding abandonment record",
    );
    transition = parseWorkflowFundingPreparedTransition(record.transition);
    abandonmentHandoff = parseWorkflowFundingAbandonmentHandoff(record.handoff);
    if (
      abandonmentHandoff.submissionIntent.txHash !== transition.transactionHash
    )
      throw new Error(
        "funding abandonment changed its signed transaction hash",
      );
  }
  const rawCompletion = await state.port.readCompletionHandoff();
  const completionHandoff =
    rawCompletion === null
      ? null
      : parseWorkflowFundingCompletionHandoff(rawCompletion);
  for (const handoff of [
    submissionHandoff,
    completionHandoff,
    abandonmentHandoff,
  ]) {
    if (
      handoff !== null &&
      (handoff.identity.deploymentFingerprint !==
        state.snapshot.deploymentFingerprint ||
        handoff.identity.decisionDigest !== state.snapshot.decisionDigest ||
        handoff.identity.category !== state.category)
    )
      throw new Error(
        "funding recovery handoff has a foreign workflow identity",
      );
  }
  if (transition !== null && completionHandoff !== null)
    throw new Error(
      "released funding recovery retains an unresolved transaction",
    );
  state.pendingTransactionHash = transition?.transactionHash;
  return Object.freeze({
    transition,
    submissionHandoff,
    completionHandoff,
    abandonmentHandoff,
  });
};
