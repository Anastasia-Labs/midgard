import type { WatcherProverFundingReservationRecord } from "./prover-funding-reservation.js";

type ProverFundingLeaseClaim = Readonly<{
  outRef: string;
  reservationId: string;
  phase: "active" | "pending";
  role: "funding" | "collateral";
}>;

/** One lease per output: a reservation's active claim on an output takes
 * precedence over its pending one, and two reservations never share one. */
export const projectRetainedProverFundingLeases = ({
  records,
  readInputs,
  transferred,
}: {
  records: readonly WatcherProverFundingReservationRecord[];
  readInputs: (
    record: WatcherProverFundingReservationRecord,
  ) => readonly { outRef: string; role: "funding" | "collateral" }[];
  transferred: (reservationId: string, outRef: string) => boolean;
}) => {
  const byOutput = new Map<string, ProverFundingLeaseClaim>();
  const claim = (
    record: WatcherProverFundingReservationRecord,
    value: { outRef: string; role: "funding" | "collateral" },
    phase: "active" | "pending",
  ) => {
    const previous = byOutput.get(value.outRef);
    if (
      previous !== undefined &&
      previous.reservationId !== record.reservationId
    )
      throw new Error("prover funding reservation repeats an output lease");
    if (previous === undefined || phase === "active")
      byOutput.set(value.outRef, {
        ...value,
        reservationId: record.reservationId,
        phase,
      });
  };
  for (const record of records) {
    for (const value of readInputs(record)) claim(record, value, "active");
    for (const value of record.pendingTransition?.producedInputs ?? [])
      if (!transferred(record.reservationId, value.outRef))
        claim(record, value, "pending");
  }
  return { leases: [...byOutput.values()] };
};
