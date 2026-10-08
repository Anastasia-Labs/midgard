/**
 * The landed-block fork simulator's settle step (N3): the node's
 * landed-block hook at the follower's view, then what the history owner's
 * reconcile and the node's journal resolution do about its holds (the
 * rebase, an abandoned journal revived, an active journal abandoned),
 * until nothing is left to do. A rebase is deferred now and then, and
 * every third one crashes after the native move and resumes after a
 * reopen.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import { Effect } from "effect";

import type { DriverHold } from "../../src/l1-events/driver.js";
import {
  LANDED_BLOCK_AWAITING_DA,
  LANDED_BLOCK_OWN_JOURNAL_ABANDONED,
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCK_REPLAY_FAILED,
} from "../../src/landed-blocks/holds.js";
import { landedBlockHook } from "../../src/landed-blocks/hook.js";
import { moveNativeRoot, rebaseSql } from "../../src/landed-blocks/rebase.js";
import {
  rebasePlan,
  rebaseTargetOf,
  walkTarget,
} from "../../src/landed-blocks/rebase-target.js";
import { retrieveRows } from "../../src/landed-blocks/store.js";
import type { Database } from "../../src/services/database.js";
import { withHistoryWrite } from "../../src/services/event-history-producer.js";
import {
  type ModelQueueHeaders,
  rootLineage,
} from "./landed-blocks-sim.model.js";
import type { simNode } from "./landed-blocks-sim.node.js";
import {
  type Faults,
  type LandedSimEnv,
  simPorts,
} from "./landed-blocks-sim.ports.js";
import { SIM_QUEUE_CONFIG } from "./state-queue-sim.fixtures.js";

export const holdNames = (
  hold: { reason: string; detail: string } | undefined,
  reason: string,
) =>
  hold !== undefined &&
  (hold.reason === reason || hold.detail.includes(`also ${reason}:`));

/** The simulated follower's depth parameters, for the published level. */
const SIM_DEPTH = { confirmationDepth: 2, securityParameter: 6 } as const;

const OWN_ABANDONED =
  /own block ([0-9a-f]+) landed but its journal is abandoned/;
const AWAITING_OWN = "awaiting own journal resolution";

export type Settled =
  | Readonly<{ error: string }>
  | Readonly<{ hold: DriverHold | undefined; deferred?: true }>;

type Run = <A, E>(effect: Effect.Effect<A, E, Database>) => Promise<A>;

/** The settle step; `rebuilds()` counts the working-ledger rebuilds so far. */
export const simSettler = (
  env: LandedSimEnv,
  node: ReturnType<typeof simNode>,
  run: Run,
  served: Set<string>,
) => {
  const { stats, book } = env;
  let rebuilds = 0;
  // A rollback's rebase is held back for the next few events now and then,
  // so the blocks it removed can reland before it runs.
  let deferUntil = 0;
  let rollbacks = 0;
  const settle = async (
    store: FactStore,
    faults: Faults,
    rollback: boolean,
    queue: ModelQueueHeaders | undefined,
  ): Promise<Settled> => {
    const view = (await store.currentView())!;
    const rooted = new Set(
      queue === undefined ? [] : rootLineage(env.registry, queue.root),
    );
    const onQueue = new Set([...rooted, ...(queue?.nodes ?? [])]);
    const requested = { value: false };
    const hook = landedBlockHook({
      store,
      config: SIM_QUEUE_CONFIG,
      ports: simPorts(env, store, faults, served, requested),
      run,
      publish: {
        depth: SIM_DEPTH,
        position: (position) => (env.published.position = position),
      },
    });
    const removedBefore = (await run(retrieveRows))
      .filter((row) => row.state === "removed")
      .map((row) => row.headerHash);
    // A revived or abandoned journal moves the working ledger onto the
    // processed chain once processing settled.
    let restore = false;
    for (let round = 0; round < 40; round++) {
      faults.missing = [];
      faults.transient = 0;
      requested.value = false;
      const hold = await hook({ kind: "unchanged", view });
      if (faults.violation !== undefined) return { error: faults.violation };
      if (round === 0) {
        const after = new Map(
          (await run(retrieveRows)).map((row) => [row.headerHash, row.state]),
        );
        stats.relands += removedBefore.filter(
          (hash) => after.get(hash) === "processed",
        ).length;
      }
      if (holdNames(hold, LANDED_BLOCK_OWN_JOURNAL_ABANDONED)) {
        const header = OWN_ABANDONED.exec(hold!.detail)?.[1];
        if (header === undefined || !book.blocks.has(header))
          return {
            error: `an abandoned own block held ${JSON.stringify(hold)}`,
          };
        await node.revive(header, rooted.has(header), onQueue);
        restore = true;
        continue;
      }
      if (faults.missing.length > 0) {
        if (hold?.reason !== LANDED_BLOCK_AWAITING_DA)
          return { error: `missing DA held ${JSON.stringify(hold)}` };
        stats.awaitingDaHeld += 1;
        continue;
      }
      if (faults.transient > 0) {
        if (!holdNames(hold, LANDED_BLOCK_REPLAY_FAILED))
          return { error: `a replay fault held ${JSON.stringify(hold)}` };
        stats.transientHeld += 1;
        continue;
      }
      const plan = await run(rebasePlan);
      if (plan.kind === "blocked") {
        if (!plan.detail.startsWith(AWAITING_OWN) || book.active === undefined)
          return { error: `rebase blocked: ${plan.detail}` };
        await node.abandonActive(onQueue);
        continue;
      }
      if (plan.kind === "none") {
        // A base that left the tip without a rebase is resolved here too.
        const target = await run(Effect.flatMap(retrieveRows, rebaseTargetOf));
        if (
          target.kind === "blocked" &&
          target.detail.startsWith(AWAITING_OWN) &&
          book.active !== undefined
        ) {
          await node.abandonActive(onQueue);
          restore = true;
        }
        if (restore) {
          const moved = await node.rebaseOnto();
          if (typeof moved === "string") return { error: moved };
          rebuilds += 1;
        }
        return { hold };
      }
      if (!holdNames(hold, LANDED_BLOCK_REBASE_PENDING))
        return { error: `a due rebase held ${JSON.stringify(hold)}` };
      if (!requested.value) return { error: "a due rebase was not requested" };
      if (round === 0 && rollback && (rollbacks += 1) % 2 === 0)
        deferUntil = stats.checks + 3;
      if (
        round === 0 &&
        (stats.checks % 5 === 4 || stats.checks <= deferUntil)
      ) {
        stats.deferredRebases += 1;
        return { deferred: true, hold };
      }
      stats.rebases += 1;
      if (plan.target.live !== undefined) stats.liveRebases += 1;
      const { durableRoot } = await env.owner.current.diagnostics();
      if (!walkTarget(plan.target).roots.includes(durableRoot))
        stats.restoredRoots += 1;
      const preparation = { assertCurrent: Effect.void };
      await run(moveNativeRoot(env.owner.current, plan.target, preparation));
      if (stats.rebases % 3 === 0) {
        await env.owner.reopen();
        stats.crashResumes += 1;
        continue;
      }
      await run(withHistoryWrite(rebaseSql(plan.target)));
      rebuilds += 1;
    }
    return { error: "landed-block processing did not settle" };
  };

  return { settle, rebuilds: () => rebuilds };
};
