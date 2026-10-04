import { WatcherDurableAuthorityConflict } from "../storage/durable-runtime.load-published-authority.js";
import type { WatcherChainCoordinator } from "./chain-coordinator.js";

export type WatcherCoordinatorHoldReason =
  | "durable_authority_conflict"
  | "rollback_evidence_rejected"
  | "native_generation_changed";

export class WatcherCoordinatorIntegrityHeld extends Error {
  constructor(
    readonly reason: WatcherCoordinatorHoldReason,
    message: string,
  ) {
    super(message);
  }
}

export const coordinatorHoldReason = (
  error: unknown,
): WatcherCoordinatorHoldReason | null =>
  error instanceof WatcherCoordinatorIntegrityHeld
    ? error.reason
    : error instanceof WatcherDurableAuthorityConflict
      ? "durable_authority_conflict"
      : null;

export const coordinatorHoldsReadiness = (
  status: ReturnType<WatcherChainCoordinator["status"]> | null,
): boolean =>
  status !== null &&
  (status.integrityHold !== null ||
    status.quarantined ||
    status.rollbackPoint !== null ||
    status.deliveryHeld);
