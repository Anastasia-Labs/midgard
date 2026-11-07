import { DatabaseSync } from "node:sqlite";

import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import { createWatcherProtocolParameterHistory } from "./prover-funding.js";
import {
  assertWatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationPlan,
} from "./prover-funding-reservation.js";
import { type WatcherSqliteProverFundingReservationStoreRuntime } from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";
import { openInternal } from "./sqlite-prover-funding-reservation-store.open-internal.js";

export const openWatcherSqliteProverFundingReservationStore = async (input: {
  readonly path: string;
  readonly busyTimeoutMs?: number;
  readonly protocolParameterHistory?: Readonly<{
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    authenticationKey: Uint8Array;
  }>;
}): Promise<WatcherSqliteProverFundingReservationStoreRuntime> => {
  const runtime = await openInternal(
    input,
    assertWatcherProverFundingReservationPlan,
  );
  if (input.protocolParameterHistory === undefined) return runtime;
  let database: DatabaseSync | undefined;
  try {
    database = new DatabaseSync(input.path, {
      enableForeignKeyConstraints: true,
    });
    database.exec(
      `PRAGMA synchronous = FULL; PRAGMA trusted_schema = OFF; PRAGMA busy_timeout = ${(input.busyTimeoutMs ?? 5_000).toString()};`,
    );
    const historyDatabase = database;
    const protocolParameterHistory = createWatcherProtocolParameterHistory({
      database,
      ...input.protocolParameterHistory,
    });
    return Object.freeze({
      ...runtime,
      protocolParameterHistory,
      close: () => {
        try {
          historyDatabase.close();
        } finally {
          runtime.close();
        }
      },
    });
  } catch (cause) {
    database?.close();
    runtime.close();
    throw cause;
  }
};

/** Test-only storage seam. Production always requires an opaque admitted plan. */
export const unsafeOpenWatcherSqliteProverFundingReservationStoreForTest =
  async (
    input: Readonly<{ path: string; busyTimeoutMs?: number }>,
    unsafeAssertPlanForTest: (
      plan: WatcherProverFundingReservationPlan,
    ) => void,
  ): Promise<WatcherSqliteProverFundingReservationStoreRuntime> =>
    await openInternal(input, unsafeAssertPlanForTest);
