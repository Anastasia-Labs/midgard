import type { WatcherProverFundingReservationStore } from "./prover-funding-reservation.js";

/** A legacy overlap holds only the reservations whose claims share an output.
 * A reservation without durable claims cannot overlap; the store refuses any
 * operation that would create a new overlap. */
export const readProverFundingSubmissionAuthority = async (
  store: WatcherProverFundingReservationStore,
  reservationId: string,
  hasExistingReservation: boolean,
) => {
  if (
    store.isReconciliationOnly !== undefined &&
    store.assertSubmissionAuthority === undefined
  )
    throw new Error("prover funding store omitted its durable submission gate");
  const held =
    hasExistingReservation &&
    ((await store.isReconciliationOnly?.({ reservationId })) ?? false);
  return {
    reconciliationOnly: held,
    assertSubmissionAuthority: async () => {
      await store.assertSubmissionAuthority?.({ reservationId });
    },
  };
};
