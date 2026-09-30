import { decodeMidgardLedgerOutputCommitment } from "@al-ft/midgard-core";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";

import {
  detectCountFaults,
  detection,
  detectTraceBoundaryFaults,
  type TransitionTraceDetection,
  type TransitionTraceDetectionEvidence,
} from "./detect.detect-count-faults.js";
import {
  detectDuplicateTraceEvents,
  detectEventToStepMismatches,
  detectInvalidNoOpTransitions,
  detectSourceMembershipMismatches,
  detectTraceLinkFaults,
} from "./detect.detect-source-membership-mismatches.js";
import {
  detectAcceptedTransactionTransitionMismatches,
  mpfProofFromWitness,
  normalizedMpfRoot,
} from "./detect.mpf-proof-from-witness.js";
import { detectL2TransactionTransitions } from "./detect.replay-l2-transaction-transition.js";
import {
  eventKeyFingerprint,
  eventKeyPhase,
  type TransitionTraceReconstruction,
} from "./reconstruct.js";
import {
  buildOmittedDueL1EventFault,
  buildOutOfWindowSourceEventFault,
  buildValidDepositTransitionWitness,
  buildValidWithdrawalTransitionWitness,
  type OmittedDueL1EventEvidence,
  type OutOfWindowSourceEventEvidence,
} from "./witnesses.js";

const detectSingleLedgerTransitions = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: TransitionTraceDetectionEvidence,
): Promise<readonly TransitionTraceDetection[]> => {
  const results: TransitionTraceDetection[] = [];
  for (const kind of ["deposit", "withdrawal"] as const) {
    const items =
      kind === "deposit"
        ? (evidence.depositTransitions ?? [])
        : (evidence.withdrawalTransitions ?? []);
    for (const item of items) {
      const inserting = "projectedUtxo" in item;
      const witness = inserting
        ? await buildValidDepositTransitionWitness({
            reconstruction,
            stepIndex: item.stepIndex,
            evidence: item,
          })
        : await buildValidWithdrawalTransitionWitness({
            reconstruction,
            stepIndex: item.stepIndex,
            evidence: item,
          });
      const source =
        "ValidDepositTransition" in witness
          ? witness.ValidDepositTransition
          : "ValidWithdrawalTransition" in witness
            ? witness.ValidWithdrawalTransition
            : undefined;
      if (source === undefined)
        throw new Error(
          "Single ledger transition witness has a different kind",
        );
      const mutation = inserting ? item.projectedUtxo : item.spentUtxo;
      const outRef =
        "ValidDepositTransition" in witness
          ? witness.ValidDepositTransition.source_membership.key
          : (
              witness as Extract<
                SDK.InvalidOneStepTransitionWitness,
                { ValidWithdrawalTransition: unknown }
              >
            ).ValidWithdrawalTransition.source_membership.value.body.l2_outref;
      const key = encodeMidgardSpendInputItem({
        txId: Buffer.from(outRef.transactionId, "hex"),
        outputIndex: Number(outRef.outputIndex),
      });
      if (mutation.key !== key.toString("hex"))
        throw new Error(
          "Single ledger transition key differs from authenticated source",
        );
      decodeMidgardLedgerOutputCommitment(Buffer.from(mutation.value, "hex"));
      const proof = mpfProofFromWitness({
        key,
        value: Buffer.from(mutation.value, "hex"),
        proof:
          "insert_proof" in mutation
            ? mutation.insert_proof
            : mutation.delete_proof,
        label: `${kind} transition mutation`,
      });
      const membership = mpfProofFromWitness({
        key,
        value: inserting ? undefined : Buffer.from(mutation.value, "hex"),
        proof:
          "non_membership_proof" in mutation
            ? mutation.non_membership_proof
            : mutation.membership_proof,
        label: `${kind} transition membership`,
      });
      for (const candidate of [proof, membership]) {
        if (
          normalizedMpfRoot(
            candidate.verify(!inserting),
            "transition pre-root",
          ) !== source.trace_proof.value.pre_utxos_root
        )
          throw new Error(
            "Single ledger transition proof differs from authenticated pre-root",
          );
      }
      const after = normalizedMpfRoot(
        proof.verify(inserting),
        "transition post-root",
      );
      if (after !== source.trace_proof.value.post_utxos_root)
        results.push(
          detection({
            reconstruction,
            kind: "invalidOneStepTransition",
            invariant: `${kind}_transition_matches_authenticated_replay`,
            diagnostic: `${kind} trace step ${item.stepIndex} differs from its authenticated ledger mutation.`,
            fault: SDK.invalidOneStepTransitionFault(witness),
          }),
        );
    }
  }
  return results;
};

const detectOmittedDueL1Events = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: readonly OmittedDueL1EventEvidence[],
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const item of evidence) {
    const eventKey =
      item.kind === "deposit"
        ? ({ DepositEventKey: { deposit_id: item.depositId } } as SDK.EventKey)
        : item.kind === "withdrawal"
          ? ({
              WithdrawalEventKey: { withdrawal_id: item.withdrawalId },
            } as SDK.EventKey)
          : ({
              ForcedTransactionEventKey: { tx_order_id: item.txOrderId },
            } as SDK.EventKey);
    const fingerprint = eventKeyFingerprint(eventKey);
    if (!reconstruction.sourceEventsByFingerprint.has(fingerprint)) {
      detections.push(
        detection({
          reconstruction,
          kind: "omittedDueL1Event",
          invariant: "due_l1_event_is_in_source_root",
          diagnostic: `Due ${item.kind} L1 event ${fingerprint} is absent from the committed ${eventKeyPhase(
            eventKey,
          )} source root.`,
          fault: await buildOmittedDueL1EventFault({
            reconstruction,
            evidence: item,
          }),
        }),
      );
    }
  }
  return detections;
};

const detectOutOfWindowSourceEvents = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: readonly OutOfWindowSourceEventEvidence[],
): Promise<readonly TransitionTraceDetection[]> => {
  const detections: TransitionTraceDetection[] = [];
  for (const item of evidence) {
    const eventKey =
      item.kind === "deposit"
        ? ({ DepositEventKey: { deposit_id: item.depositId } } as SDK.EventKey)
        : item.kind === "withdrawal"
          ? ({
              WithdrawalEventKey: { withdrawal_id: item.withdrawalId },
            } as SDK.EventKey)
          : ({
              ForcedTransactionEventKey: { tx_order_id: item.txOrderId },
            } as SDK.EventKey);
    const fingerprint = eventKeyFingerprint(eventKey);
    if (reconstruction.sourceEventsByFingerprint.has(fingerprint)) {
      detections.push(
        detection({
          reconstruction,
          kind: "outOfWindowSourceEvent",
          invariant: "source_event_is_within_block_window",
          diagnostic: `Out-of-window ${item.kind} L1 event ${fingerprint} is present in the committed source root.`,
          fault: await buildOutOfWindowSourceEventFault({
            reconstruction,
            evidence: item,
          }),
        }),
      );
    }
  }
  return detections;
};

export const detectTransitionTraceFaults = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: TransitionTraceDetectionEvidence = {},
): Promise<readonly TransitionTraceDetection[]> => [
  ...detectCountFaults(reconstruction),
  ...(await detectTraceBoundaryFaults(reconstruction)),
  ...(await detectTraceLinkFaults(reconstruction)),
  ...(await detectDuplicateTraceEvents(reconstruction)),
  ...(await detectEventToStepMismatches(reconstruction)),
  ...(await detectSourceMembershipMismatches(reconstruction)),
  ...(await detectInvalidNoOpTransitions(reconstruction)),
  ...(await detectL2TransactionTransitions(
    reconstruction,
    evidence.l2TransactionTransitions ?? [],
  )),
  ...(await detectSingleLedgerTransitions(reconstruction, evidence)),
  ...detectAcceptedTransactionTransitionMismatches(
    reconstruction,
    evidence.acceptedTransactionTransitionMismatches ?? [],
  ),
  ...(await detectOmittedDueL1Events(
    reconstruction,
    evidence.omittedDueL1Events ?? [],
  )),
  ...(await detectOutOfWindowSourceEvents(
    reconstruction,
    evidence.outOfWindowSourceEvents ?? [],
  )),
];

export const detectFirstTransitionTraceFault = async (
  reconstruction: TransitionTraceReconstruction,
  evidence: TransitionTraceDetectionEvidence = {},
): Promise<TransitionTraceDetection | undefined> =>
  (await detectTransitionTraceFaults(reconstruction, evidence))[0];
