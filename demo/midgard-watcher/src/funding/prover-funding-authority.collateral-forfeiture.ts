import {
  assertWorkflowActuationPermitIdentity,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type { WorkflowFundingReservationPort } from "@al-ft/midgard-fault-proofs/workflow/funding-reservation-permit";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
import type { UTxO } from "@lucid-evolution/lucid";

import { snapshot } from "./prover-funding-authority.watcher-prover-funding-authority-factory.js";
import type {
  WatcherProverFundingReservationPlan,
  WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";
import { signedCollateralLanding } from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";

/** A wallet coin as the reservation records it. */
export const reservationInput = (utxo: UTxO) => ({
  outRef: `${utxo.txHash}#${utxo.outputIndex}`,
  lovelace: (utxo.assets.lovelace ?? 0n).toString(),
  assets: Object.entries(utxo.assets)
    .filter(([unit]) => unit !== "lovelace")
    .map(([unit, quantity]) => ({
      unit,
      quantity: quantity.toString(),
    }))
    .sort((a, b) => a.unit.localeCompare(b.unit)),
});

/**
 * The reservation's side of a phase-2-invalid landing of one of its
 * attempts: the chain took the attempt's collateral and created its
 * collateral return, and a rollback past that block undoes both.
 */
export const collateralForfeiturePort = ({
  input,
  plan,
}: {
  readonly input: Readonly<{
    category: FraudProofCatalogueCategoryName;
    actuationPermit: WorkflowActuationPermit;
    rollbackGeneration: string;
    store: WatcherProverFundingReservationStore;
    resolveInputs: (outRefs: readonly string[]) => Promise<readonly UTxO[]>;
  }>;
  readonly plan: WatcherProverFundingReservationPlan;
}): Pick<
  WorkflowFundingReservationPort,
  "forfeitCollateral" | "restoreCollateral"
> => {
  const reserved = { reservationId: plan.reservationId };
  return Object.freeze({
    forfeitCollateral: async ({
      expectedRevision,
      transactionHash,
      journal,
    }: Parameters<
      NonNullable<WorkflowFundingReservationPort["forfeitCollateral"]>
    >[0]) => {
      if (input.store.forfeitCollateral === undefined)
        throw new Error("Funding store cannot account forfeited collateral");
      assertWorkflowActuationPermitIdentity({
        permit: input.actuationPermit,
        category: input.category,
        rollbackGeneration: input.rollbackGeneration,
      });
      return snapshot({
        plan,
        record: await input.store.forfeitCollateral({
          plan,
          expectedRevision,
          transactionHash,
          journal,
        }),
        rollbackGeneration: input.rollbackGeneration,
      });
    },
    restoreCollateral: async ({
      expectedRevision,
      transactionHash,
      journal,
    }: Parameters<
      NonNullable<WorkflowFundingReservationPort["restoreCollateral"]>
    >[0]) => {
      if (input.store.restoreCollateral === undefined)
        throw new Error("Funding store cannot account forfeited collateral");
      const signed = await input.store.readRecordedSignedTransaction({
        ...reserved,
        transactionHash,
      });
      if (typeof signed !== "string")
        throw new Error("forfeited collateral has no signed funding attempt");
      // The collateral comes back as the chain now shows it: live, paying
      // the wallet, and held by no other reservation.
      const elsewhere = new Set(
        await input.store.readReservedOutRefs({
          excludingReservationId: plan.reservationId,
        }),
      );
      const collateral = signedCollateralLanding(
        signed,
        plan.walletAddress,
      ).collateralOutRefs.filter((outRef) => !elsewhere.has(outRef));
      const recorded = new Set(collateral);
      const inputs = (await input.resolveInputs(collateral)).map((utxo) => {
        const value = reservationInput(utxo);
        if (!recorded.has(value.outRef) || utxo.address !== plan.walletAddress)
          throw new Error(
            "restored collateral source substituted a wallet input",
          );
        return value;
      });
      assertWorkflowActuationPermitIdentity({
        permit: input.actuationPermit,
        category: input.category,
        rollbackGeneration: input.rollbackGeneration,
      });
      return snapshot({
        plan,
        record: await input.store.restoreCollateral({
          plan,
          expectedRevision,
          transactionHash,
          inputs,
          journal,
        }),
        rollbackGeneration: input.rollbackGeneration,
      });
    },
  });
};
