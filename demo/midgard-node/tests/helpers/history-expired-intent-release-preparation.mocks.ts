/** The module mocks of a preparation test, which model only its L1 reads.
 * Each test file hoists a `Fixture` and passes it here from its `vi.mock`
 * factories; this module imports nothing from src at load time. */
import type { Fixture } from "./history-expired-intent-release-preparation.js";

type Original = <M>() => Promise<M>;

/** The exact-point capture: no outputs (the queue comes from the fixture). */
export const ledgerSnapshot = async (importOriginal: Original) => ({
  ...(await importOriginal<
    typeof import("../../src/l1-event-history-source.js")
  >()),
  readBoundRecoveryLedgerSnapshot: async () => ({ ledger: { outputs: [] } }),
});

/** Its authentication: `fixture.queue`. */
export const queueAuthentication = async (
  importOriginal: Original,
  fixture: Fixture,
) => {
  const { Effect } = await import("effect");
  return {
    ...(await importOriginal<
      typeof import("../../src/services/history-expired-intent-release.signed-commit-node.js")
    >()),
    authenticateQueue: () => Effect.sync(() => fixture.queue!),
  };
};

/** The canonical coverage loader: `fixture.coverage`. */
export const canonicalCoverage = async (
  importOriginal: Original,
  fixture: Fixture,
) => {
  const { Effect } = await import("effect");
  const { DatabaseError } = await import("../../src/database/utils/common.js");
  const { coverageOf } = await import(
    "./history-expired-intent-release-preparation.coverage.js"
  );
  const actual =
    await importOriginal<
      typeof import("../../src/database/eventHistoryCanonicalCoverage.js")
    >();
  return {
    ...actual,
    loadCanonicalHistoryCoverage: () =>
      Effect.suspend(() =>
        fixture.coverage === "unavailable"
          ? Effect.fail(
              new DatabaseError({
                table: "event_history_block_applications",
                message: actual.CANONICAL_COVERAGE_UNAVAILABLE,
                cause: undefined,
              }),
            )
          : Effect.succeed(coverageOf(fixture.coverage) as never),
      ),
  };
};

/** The node serialization (its datum is a model). */
export const nodeSerialization = async (importOriginal: Original) => {
  const { Effect } = await import("effect");
  return {
    ...(await importOriginal<
      typeof import("../../src/workers/utils/commit-block-header.js")
    >()),
    serializeStateQueueUTxO: (node: object) =>
      Effect.succeed({ ...node, utxo: "serialized", datum: "serialized" }),
  };
};
