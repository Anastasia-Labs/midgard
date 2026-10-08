import type { TemporalTableSpec } from "./registry.js";
import type { MigrationSet } from "./schema/migrate.js";
import type { DialectName } from "./sql/backend.js";
import type {
  DerivationHook,
  PruneHook,
  RetentionPins,
} from "./store/context.js";
import type { FactStore, FactStoreOptions } from "./store/fact-store.js";
import type { SimOutput } from "./testing/block-cbor.js";
import type { ForkStep, ScenarioTraffic } from "./testing/episodes.js";
import type { SimChain } from "./testing/sim-chain.js";
import type { TrackedSet } from "./types.js";

/**
 * A role's projection over the follower's facts. Its tracked set, D-t
 * tables, migrations and derivations join the store (`projectionStoreOptions`);
 * in the fork simulator its D-t tables join the after-every-event comparison
 * with a fresh replay, its `traffic` joins the filler and `check` adds the
 * projection's own §5.5 cases. Role tickets (C1, W1, N1, ...) add one.
 */
export type FollowerProjection = Readonly<{
  name: string;
  trackedSet?: TrackedSet;
  temporalTables?: readonly TemporalTableSpec[];
  migrations?: (dialect: DialectName) => MigrationSet;
  derivations?: readonly DerivationHook[];
  retentionPins?: RetentionPins;
  pruneHooks?: readonly PruneHook[];
  traffic?: ScenarioTraffic;
  /**
   * Outputs only this projection's `traffic` may spend: the filler never
   * picks them as inputs or collateral (a contract's own UTxOs, which no
   * third party can spend on chain). Referencing them stays allowed.
   */
  protects?: (output: SimOutput) => boolean;
  /**
   * Returns a failure text, or null when the projection holds at this step.
   * `reference` is a fresh forward-only replay of the current chain, never
   * pruned, for comparing the projection's reads with.
   */
  check?: (
    context: Readonly<{
      store: FactStore;
      reference: FactStore;
      step: ForkStep;
      chain: SimChain;
    }>,
  ) => Promise<string | null>;
}>;

/** The union of tracked sets (a base set and each plugged projection's). */
export const mergeTrackedSets = (
  ...sets: readonly TrackedSet[]
): TrackedSet => ({
  addresses: new Set(sets.flatMap((set) => [...set.addresses])),
  paymentCredentials: new Set(
    sets.flatMap((set) => [...set.paymentCredentials]),
  ),
  policies: new Set(sets.flatMap((set) => [...set.policies])),
});

/** Store options carrying every projection's tracked set, tables and hooks. */
export const projectionStoreOptions = (
  projections: readonly FollowerProjection[],
  base: Readonly<{ securityParameter: number; trackedSet: TrackedSet }>,
  dialect: DialectName,
): FactStoreOptions => {
  const pins = projections.flatMap((p) =>
    p.retentionPins === undefined ? [] : [p.retentionPins],
  );
  return {
    securityParameter: base.securityParameter,
    trackedSet: mergeTrackedSets(
      base.trackedSet,
      ...projections.flatMap((p) =>
        p.trackedSet === undefined ? [] : [p.trackedSet],
      ),
    ),
    temporalTables: projections.flatMap((p) => p.temporalTables ?? []),
    migrations: projections.flatMap((p) =>
      p.migrations === undefined ? [] : [p.migrations(dialect)],
    ),
    derivations: projections.flatMap((p) => p.derivations ?? []),
    pruneHooks: projections.flatMap((p) => p.pruneHooks ?? []),
    ...(pins.length === 0
      ? {}
      : {
          retentionPins: {
            txs: pins.flatMap((pin) => pin.txs ?? []),
            blocks: pins.flatMap((pin) => pin.blocks ?? []),
          },
        }),
  };
};
