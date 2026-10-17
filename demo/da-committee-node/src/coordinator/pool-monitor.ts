/**
 * The committee's view of the pooled DA bond, for readiness and alerts.
 *
 * Apply is refused on L1 while the pool backs less than one `da_bond_lovelace`
 * above its floor or is `Withdrawing`, so a committee node that sees either
 * cannot attest and reports itself not ready. A node that submits to L1
 * reads the pool after every tick and before every apply; each read lands
 * here.
 */
import type * as SDK from "@al-ft/midgard-sdk";

import type { CommitteeTickRunnerDeps } from "../tick-runner.js";
import type { OnChainCoordinatorHooks } from "./factory.js";

/** One successful read of the authentic pool, classified. */
export type DaBondPoolCheck = Readonly<{
  checkedAt: string;
  state: "bonded" | "withdrawing";
  /** The pool UTxO's whole lovelace, floor included. */
  lovelace: bigint;
  /** Lovelace above the pool floor, clamped at 0. */
  backing: bigint;
  /** `da_bond_lovelace`: the backing one attestation needs. */
  requiredBacking: bigint;
  /** `backing < requiredBacking`. */
  short: boolean;
  /** Present iff `state` is `withdrawing`. */
  unlockAt?: bigint;
}>;

/** The check for one SDK pool status readout, read at `checkedAt`. */
export const daBondPoolCheckFromStatus = (
  status: SDK.DaBondPoolStatus,
  checkedAt: string,
): DaBondPoolCheck => ({
  checkedAt,
  state: status.state,
  lovelace: status.lovelace,
  backing: status.backing,
  requiredBacking: status.requiredBacking,
  short: status.belowBond,
  ...(status.unlockAt === undefined ? {} : { unlockAt: status.unlockAt }),
});

/**
 * The readiness reasons one pool check contributes: one for a short pool, one
 * for a `Withdrawing` pool, both when it is both, none when it can back an
 * attestation.
 */
export const daBondPoolReadinessReasons = (
  check: DaBondPoolCheck,
): readonly string[] => [
  ...(check.short
    ? [
        `da_bond_pool_backing_short: backing=${check.backing.toString()}, required=${check.requiredBacking.toString()}, checkedAt=${check.checkedAt}`,
      ]
    : []),
  ...(check.state === "withdrawing"
    ? [
        `da_bond_pool_withdrawing: unlockAt=${check.unlockAt?.toString() ?? "unknown"}, checkedAt=${check.checkedAt}`,
      ]
    : []),
];

export type DaBondPoolEvent = Readonly<Record<string, string>>;

export type DaBondPoolMonitor = {
  /** Records a successful pool read and emits one event per transition. */
  readonly record: (check: DaBondPoolCheck) => void;
  /**
   * Records a failed pool read. The last good check stays the latest, no
   * readiness reason is added, and one `da_bond_pool_read_failed` event is
   * emitted per streak of failures.
   */
  readonly recordReadFailure: (error: unknown) => void;
  /** The last successful check, if any. */
  readonly latest: () => DaBondPoolCheck | undefined;
};

/**
 * Tracks the pool across reads and emits a JSON event on each transition:
 *
 * - `da_bond_pool_backing_short` / `da_bond_pool_backing_restored` when the
 *   backing falls below, or returns to, one DA bond;
 * - `da_bond_pool_withdrawing` / `da_bond_pool_bonded` when the pool enters,
 *   or leaves, `Withdrawing`.
 *
 * The first read is compared against a Bonded pool that backs a bond, so it
 * emits only when the pool is short or withdrawing. Identical reads emit
 * nothing.
 */
export const createDaBondPoolMonitor = (deps: {
  readonly writeEvent: (event: DaBondPoolEvent) => void;
  readonly now?: () => Date;
}): DaBondPoolMonitor => {
  let last: DaBondPoolCheck | undefined;
  let readFailing = false;

  const transition = (event: string, check: DaBondPoolCheck): void => {
    deps.writeEvent({
      event,
      backing: check.backing.toString(),
      required: check.requiredBacking.toString(),
      ...(check.unlockAt === undefined
        ? {}
        : { unlockAt: check.unlockAt.toString() }),
      checkedAt: check.checkedAt,
    });
  };

  return {
    record: (check) => {
      readFailing = false;
      if (check.short !== (last?.short ?? false)) {
        transition(
          check.short
            ? "da_bond_pool_backing_short"
            : "da_bond_pool_backing_restored",
          check,
        );
      }
      if (check.state !== (last?.state ?? "bonded")) {
        transition(
          check.state === "withdrawing"
            ? "da_bond_pool_withdrawing"
            : "da_bond_pool_bonded",
          check,
        );
      }
      last = check;
    },
    recordReadFailure: (error) => {
      if (readFailing) return;
      readFailing = true;
      deps.writeEvent({
        event: "da_bond_pool_read_failed",
        error: error instanceof Error ? error.message : String(error),
        failedAt: (deps.now?.() ?? new Date()).toISOString(),
      });
    },
    latest: () => last,
  };
};

/**
 * The pool monitor as the node's `main()` wires it, in one place so the whole
 * chain is testable: the submitter reports every pool read through
 * `coordinatorHooks`, the tick runner reads the pool after each tick through
 * `tickRunnerDeps`, and `/readyz` takes its pool reasons from `readiness`.
 */
export const createDaBondPoolWiring = (
  deps: Parameters<typeof createDaBondPoolMonitor>[0],
) => {
  const monitor = createDaBondPoolMonitor(deps);
  return {
    monitor,
    coordinatorHooks: {
      recordDaBondPool: monitor.record,
      recordDaBondPoolReadFailure: monitor.recordReadFailure,
    } satisfies OnChainCoordinatorHooks,
    /** No L1 submitter means no pool read. */
    tickRunnerDeps: (
      coordinator:
        | { readonly checkDaBondPool: () => Promise<unknown> }
        | undefined,
    ): Pick<CommitteeTickRunnerDeps, "readDaBondPool"> =>
      coordinator === undefined
        ? {}
        : {
            // A failed read is already recorded by the monitor's hook.
            readDaBondPool: () =>
              coordinator.checkDaBondPool().then(
                () => undefined,
                () => undefined,
              ),
          },
    readiness: (): { readonly daBondPool?: DaBondPoolCheck } => {
      const daBondPool = monitor.latest();
      return daBondPool === undefined ? {} : { daBondPool };
    },
  };
};
