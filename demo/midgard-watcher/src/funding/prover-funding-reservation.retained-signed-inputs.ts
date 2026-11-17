import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core";
import type { WorkflowFundingPreparedTransition } from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

import type {
  WatcherProverFundingReobservationCandidate,
  WatcherProverFundingReservationRecord,
} from "./prover-funding-reservation.js";

/** Consumed inputs and change of a signed attempt stay leased until final
 * completion: re-landing its exact bytes, or a replacement that must spend one
 * of its inputs, needs them. Once its workflow recorded its correction's
 * inclusion (`producedReleased`), only the consumed inputs stay: no later
 * step spends its outputs, which then fund other proofs, and an exact
 * re-landing needs only what it spent. A coin an attempt only posted as
 * collateral is not retained for it: it stays in the reservation's single
 * pool, which supersedes releasing it at inclusion (G5) while keeping that
 * mechanism's goal, since the coin held there blocks none of the
 * reservation's own actions and lets a rollback re-land the identical bytes.
 * A rollback that finds it spent meanwhile re-signs. */
export const retainedProverFundingInputs = ({
  record,
  submissions,
  abandonedTransactionHashes,
  completed,
  producedReleased = false,
}: {
  readonly record: WatcherProverFundingReservationRecord;
  readonly submissions: readonly WorkflowFundingPreparedTransition[];
  readonly abandonedTransactionHashes: ReadonlySet<string>;
  readonly completed: boolean;
  readonly producedReleased?: boolean;
}): readonly string[] => {
  const inputs = new Set<string>();
  for (const transition of completed ? [] : submissions) {
    if (abandonedTransactionHashes.has(transition.transactionHash)) continue;
    for (const outRef of transition.consumedOutRefs) inputs.add(outRef);
    if (producedReleased) continue;
    for (const { outRef } of transition.producedInputs) inputs.add(outRef);
  }
  for (const { outRef } of record.activeInputs) inputs.add(outRef);
  return [...inputs];
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
}): readonly WatcherProverFundingReobservationCandidate[] => {
  const candidates = new Map<string, boolean>();
  for (const transition of submissions) {
    if (transition.transactionHash !== transactionHash) continue;
    for (const outRef of signedCollateralOutRefs(
      transition.signedTransactionCborHex,
    ))
      candidates.set(outRef, true);
    for (const outRef of transition.consumedOutRefs)
      candidates.set(outRef, false);
  }
  for (const { outRef } of record.activeInputs) candidates.set(outRef, false);
  return [...candidates].map(([outRef, collateralOnly]) =>
    Object.freeze({ outRef, collateralOnly }),
  );
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
