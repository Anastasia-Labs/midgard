import type { WatcherInstalledWorkflowCategory } from "../fault-proofs/fault-proof-application.js";
import type { WatcherProtocolParameterRuntimeAuthority } from "../funding/prover-funding.js";
import { createWatcherProverFundingAuthorityFactory } from "../funding/prover-funding-authority.js";
import { openWatcherSqliteProverFundingReservationStore } from "../funding/sqlite-prover-funding-reservation-store.js";
import type { VerifiedWatcherDeploymentIdentity } from "./deployment-identity.js";

/**
 * Opens the funding store, then admits the live protocol parameters through
 * `createProtocolParameters`. The caller owns how that read is retried; the
 * store is opened once and closed again if admission fails.
 */
export const openWatcherProverFundingRuntime = async (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly path: string;
  readonly authenticationKey: Uint8Array;
  readonly createProtocolParameters: () => Promise<WatcherProtocolParameterRuntimeAuthority>;
  readonly launchScope: readonly WatcherInstalledWorkflowCategory[];
  readonly journalRoot: string;
}) => {
  const store = await openWatcherSqliteProverFundingReservationStore({
    path: input.path,
    protocolParameterHistory: {
      deploymentIdentity: input.deploymentIdentity,
      authenticationKey: input.authenticationKey,
    },
  });
  try {
    const protocolParameters = await input.createProtocolParameters();
    const factory = createWatcherProverFundingAuthorityFactory({
      launchScope: input.launchScope,
      journalRoot: input.journalRoot,
      journalAuthenticationKey: input.authenticationKey,
      deploymentIdentity: input.deploymentIdentity,
      protocolParameters,
      protocolParameterHistory: store.protocolParameterHistory,
      store: store.store,
    });
    // No supervisor jobs exist yet; reclaim reservations left before any signed attempt.
    await factory.releaseUnused();
    return { store, factory };
  } catch (cause) {
    store.close();
    throw cause;
  }
};
