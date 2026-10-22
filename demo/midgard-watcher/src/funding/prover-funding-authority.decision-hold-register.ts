import type { WatcherDecisionHold } from "../fault-proofs/watcher-decision-hold.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.js";

/**
 * The reservations held because no workflow drives their signed attempts
 * again. A reservation's failure alert is active exactly while it is held.
 */
export const createWatcherReservationHoldRegister = (
  alertSink?: () => Pick<WatcherOperationsSink, "setAlert">,
) => {
  const holds = new Map<string, WatcherDecisionHold>();
  const raised = new Set<string>();
  const syncAlerts = () => {
    let sink;
    try {
      sink = alertSink?.();
    } catch {
      return; // Startup: the operations server is not bound yet.
    }
    if (sink === undefined) return;
    const held = new Set(
      [...holds.values()].flatMap((hold) =>
        hold.kind === "reservation" ? [hold.reservationId] : [],
      ),
    );
    for (const id of new Set([...raised, ...held])) {
      if (raised.has(id) === held.has(id)) continue;
      sink.setAlert({
        code: "proof_submission_failure",
        subjectDigest: id,
        active: held.has(id),
        observedAtMs: Date.now().toString(),
      });
      if (held.has(id)) raised.add(id);
      else raised.delete(id);
    }
  };
  return Object.freeze({
    /** Replaces the holds of `checked` (every hold when null) with `next`. */
    record: (
      checked: readonly string[] | null,
      next: readonly WatcherDecisionHold[],
    ): void => {
      if (checked === null) holds.clear();
      else for (const reservationId of checked) holds.delete(reservationId);
      for (const hold of next)
        if (hold.kind === "reservation") holds.set(hold.reservationId, hold);
      syncAlerts();
    },
    ids: (): readonly string[] => [...holds.keys()],
    current: (): readonly WatcherDecisionHold[] => {
      syncAlerts();
      return Object.freeze([...holds.values()]);
    },
  });
};
