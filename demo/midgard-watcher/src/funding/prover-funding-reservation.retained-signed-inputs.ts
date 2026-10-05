import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import type {
  WorkflowFundingCompletionHandoff,
  WorkflowFundingPreparedTransition,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

import type { WatcherProverFundingReservationRecord } from "./prover-funding-reservation.js";

export const finalProverFundingCompletion = (
  handoff: WorkflowFundingCompletionHandoff,
): boolean =>
  handoff.completion.kind === "completed" &&
  handoff.completion.terminal.observedAt.confirmationDepth >
    DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth + 1;

/** Consumed inputs and change of a signed attempt stay leased until final
 * completion: re-landing its exact bytes, or a replacement that must spend one
 * of its inputs, needs them. Collateral is never retained for an attempt; a
 * confirmed attempt's collateral is free once the reservation stops using it,
 * and a rollback whose collateral was spent meanwhile re-signs. */
export const retainedProverFundingInputs = ({
  record,
  submissions,
  abandonedTransactionHashes,
  completed,
}: {
  readonly record: WatcherProverFundingReservationRecord;
  readonly submissions: readonly WorkflowFundingPreparedTransition[];
  readonly abandonedTransactionHashes: ReadonlySet<string>;
  readonly completed: boolean;
}) => {
  const inputs = new Map<string, "funding" | "collateral">();
  for (const transition of completed ? [] : submissions) {
    if (abandonedTransactionHashes.has(transition.transactionHash)) continue;
    for (const outRef of transition.consumedOutRefs)
      inputs.set(outRef, "funding");
    for (const value of transition.producedInputs)
      inputs.set(value.outRef, value.role);
  }
  for (const { outRef, role } of record.activeInputs) inputs.set(outRef, role);
  return [...inputs].map(([outRef, role]) => ({ outRef, role }));
};

export const signedCollateralOutRefs = (
  signedTransactionCborHex: string,
): readonly string[] => {
  const collateral = CML.Transaction.from_cbor_hex(signedTransactionCborHex)
    .body()
    .collateral_inputs();
  return Array.from({ length: collateral?.len() ?? 0 }, (_, i) => {
    const ref = collateral!.get(i);
    return `${ref.transaction_id().to_hex()}#${ref.index()}`;
  });
};

export const proverFundingReobservationInputs = ({
  record,
  submissions,
  transactionHash,
}: {
  readonly record: WatcherProverFundingReservationRecord;
  readonly submissions: readonly WorkflowFundingPreparedTransition[];
  readonly transactionHash: string;
}) => {
  const candidates = new Map(
    record.activeInputs.map(({ outRef, role }) => [outRef, role]),
  );
  for (const transition of submissions) {
    if (transition.transactionHash !== transactionHash) continue;
    for (const outRef of transition.consumedOutRefs)
      candidates.set(outRef, "funding");
    for (const outRef of signedCollateralOutRefs(
      transition.signedTransactionCborHex,
    ))
      candidates.set(outRef, "collateral");
  }
  return [...candidates].map(([outRef, role]) => ({ outRef, role }));
};

export const pendingProverFundingLineage = (input: {
  readonly reservationId: string;
  readonly transition: import("./prover-funding-reservation.js").WatcherProverFundingReservationTransition;
  readonly signedTransactionCborHex: string;
}) => {
  const outputs = CML.Transaction.from_cbor_hex(input.signedTransactionCborHex)
    .body()
    .outputs();
  return Array.from({ length: outputs.len() }, (_, outputIndex) =>
    Object.freeze({
      reservationId: input.reservationId,
      sourceActionKind: input.transition.actionKind,
      sourceOutputIndex: outputIndex,
      outRef: `${input.transition.transactionHash}#${outputIndex.toString()}`,
      resolvedOutputCborHex: outputs.get(outputIndex).to_canonical_cbor_hex(),
      transitionDigest: input.transition.transitionDigest,
    }),
  );
};

export const pendingProverFundingLineageParameters = (
  identity: ReturnType<typeof pendingProverFundingLineage>[number],
) =>
  [
    identity.reservationId,
    identity.sourceActionKind,
    identity.sourceOutputIndex,
    identity.outRef,
    identity.resolvedOutputCborHex,
    identity.transitionDigest,
    computeDeploymentManifestJsonDigest(identity),
  ] as const;
