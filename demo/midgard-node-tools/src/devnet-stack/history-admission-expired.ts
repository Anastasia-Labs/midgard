import { historyProofRemaining } from "./history-proof-deadline.js";

/** Only expiry of this exact admission attempt permits a same-owner retry. */
export class HistoryAdmissionExpired extends Error {
  constructor(readonly deadline: number) {
    super("history role admission deadline elapsed");
    this.name = "HistoryAdmissionExpired";
  }
}

export const historyAdmissionExpired = (error: unknown, deadline: number) =>
  error instanceof HistoryAdmissionExpired &&
  error.deadline === deadline &&
  historyProofRemaining(deadline) === 0;
