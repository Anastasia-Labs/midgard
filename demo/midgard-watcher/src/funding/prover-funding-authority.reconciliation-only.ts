import type { WatcherProverFundingReservationStore } from "./prover-funding-reservation.js";

export const readProverFundingSubmissionAuthority = async (
  store: WatcherProverFundingReservationStore,
  hasExistingReservation: boolean,
) => {
  if (
    store.isReconciliationOnly !== undefined &&
    store.assertSubmissionAuthority === undefined
  )
    throw new Error("prover funding store omitted its durable submission gate");
  const held = (await store.isReconciliationOnly?.()) ?? false;
  if (held && !hasExistingReservation)
    throw new Error(
      "overlapping legacy funding permits only an existing reservation",
    );
  return {
    reconciliationOnly: held,
    assertSubmissionAuthority: async () => {
      await store.assertSubmissionAuthority?.();
    },
  };
};
