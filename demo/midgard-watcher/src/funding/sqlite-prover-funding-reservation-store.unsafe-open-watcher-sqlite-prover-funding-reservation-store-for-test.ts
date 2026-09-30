import {
  assertWatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationPlan,
} from "./prover-funding-reservation.js";
import { type WatcherSqliteProverFundingReservationStoreRuntime } from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";
import { openInternal } from "./sqlite-prover-funding-reservation-store.open-internal.js";

export const openWatcherSqliteProverFundingReservationStore = async (input: {
  readonly path: string;
  readonly busyTimeoutMs?: number;
}): Promise<WatcherSqliteProverFundingReservationStoreRuntime> =>
  await openInternal(input, assertWatcherProverFundingReservationPlan);

/** Test-only storage seam. Production always requires an opaque admitted plan. */
export const unsafeOpenWatcherSqliteProverFundingReservationStoreForTest =
  async (
    input: Readonly<{ path: string; busyTimeoutMs?: number }>,
    unsafeAssertPlanForTest: (
      plan: WatcherProverFundingReservationPlan,
    ) => void,
  ): Promise<WatcherSqliteProverFundingReservationStoreRuntime> =>
    await openInternal(input, unsafeAssertPlanForTest);
