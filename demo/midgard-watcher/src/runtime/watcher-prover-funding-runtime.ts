import type { WatcherInstalledWorkflowCategory } from "../fault-proofs/fault-proof-application.js";
import {
  isWatcherJournalIntegrityError,
  isWatcherJournalUnavailableError,
} from "../fault-proofs/watcher-journal-database.js";
import type { WatcherProtocolParameterRuntimeAuthority } from "../funding/prover-funding.js";
import {
  createWatcherProverFundingAuthorityFactory,
  type WatcherProverFundingAuthorityFactory,
} from "../funding/prover-funding-authority.js";
import type { WatcherFundingInputFacts } from "../funding/prover-funding-input-facts.js";
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
  /** The follower's facts that reclaim reservations whose decision is missing. */
  readonly fundingInputFacts?: WatcherFundingInputFacts;
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
      ...(input.fundingInputFacts === undefined
        ? {}
        : { fundingInputFacts: input.fundingInputFacts }),
    });
    // No supervisor jobs exist yet; reclaim reservations left before any signed attempt.
    // A refused decision journal, or one that could not be opened, keeps them
    // reserved: the supervisor reports journal_integrity or
    // journal_unavailable once the operations server binds. A reservation
    // whose decision is missing is held (journal_decision_missing).
    await factory.releaseUnused().catch((error: unknown) => {
      if (
        !isWatcherJournalIntegrityError(error) &&
        !isWatcherJournalUnavailableError(error)
      )
        throw error;
    });
    return { store, factory };
  } catch (cause) {
    store.close();
    throw cause;
  }
};

/**
 * Reads the held reservations' L1 facts again on every follower change,
 * one read at a time, until none is held. A read that fails keeps the
 * holds as they are; the next change reads again.
 */
export const recheckFundingDecisionHolds = (
  factory: Pick<
    WatcherProverFundingAuthorityFactory,
    "recheckDecisionHolds" | "decisionHolds"
  >,
  onChange: (listener: () => void) => () => void,
): (() => void) => {
  let running = false;
  let again = false;
  const run = async (): Promise<void> => {
    if (running) {
      again = true;
      return;
    }
    running = true;
    try {
      do {
        again = false;
        await factory.recheckDecisionHolds().catch(() => undefined);
      } while (again);
    } finally {
      running = false;
    }
  };
  return onChange(() => {
    if (factory.decisionHolds().length > 0) void run();
  });
};
