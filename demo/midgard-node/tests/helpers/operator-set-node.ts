/**
 * One node process's operator set over a follower store, and a simulated
 * operator-set chain to drive it, shared by the operator-set tests.
 */
import {
  currentViewIn,
  type FactStore,
  type SqlTx,
} from "@al-ft/midgard-l1-follower";
import { type SimTx, simUniverse } from "@al-ft/midgard-l1-follower/testing";
import { Effect, Ref } from "effect";

import {
  createOperatorSetMirror,
  type OperatorActivityRecord,
  operatorSetHook,
  type OperatorSetRun,
  operatorSetTrackedSet,
  publishOperatorMembership,
} from "../../src/l1-operator-set/index.js";
import { Globals } from "../../src/services/globals.js";
import { HaltSource } from "../../src/services/liveness-halt.js";
import { ChainDriver } from "./l1-events-store.js";
import {
  operatorKey,
  OperatorSetChain,
  type OperatorSetChainFixture,
  txOf,
  type TxParts,
} from "./operator-set-chain.js";

export const K = 6;
export const DEPTH = { confirmationDepth: 2, securityParameter: K };
export const OWN = operatorKey(0x50);

/**
 * One node process's operator set over `store` (own key `OWN`): a fresh mirror, hook and
 * globals. `step` runs the hook once and returns the run, the hold and the
 * `/readyz` reason it left, with the rows every statement returned.
 */
export const operatorSetNodeProcess = async (
  fixture: OperatorSetChainFixture,
  store: FactStore,
  activity: OperatorActivityRecord,
) => {
  const globals = await Effect.runPromise(
    Effect.provide(Globals, Globals.Default),
  );
  let rows = 0;
  const counted: Pick<FactStore, "dialect" | "transaction"> = {
    dialect: store.dialect,
    transaction: (mode, work) =>
      store.transaction(mode, (tx) =>
        work({
          query: async (text, params) => {
            const result = await tx.query(text, params);
            rows += result.length;
            return result;
          },
          exec: (text) => tx.exec(text),
        } satisfies SqlTx),
      ),
  };
  const runs: OperatorSetRun[] = [];
  const hook = operatorSetHook({
    store: counted,
    mirror: createOperatorSetMirror({ config: fixture.config, ownKey: OWN }),
    depth: DEPTH,
    activity,
    publish: async (run) => {
      runs.push(run);
      await Effect.runPromise(
        publishOperatorMembership(run.membership).pipe(
          Effect.provideService(Globals, globals),
        ),
      );
    },
  });
  const step = async () => {
    const view = await store.transaction("read", (tx) =>
      currentViewIn(tx, store.dialect),
    );
    if (view === null) throw new Error("no follower view");
    rows = 0;
    const hold = await hook({ kind: "unchanged", view });
    const run = runs[runs.length - 1]!;
    const reason = (
      await Effect.runPromise(Ref.get(globals.LIVENESS_REASONS))
    ).get(HaltSource.operatorMembership);
    return { run, hold, reason, rows };
  };
  return { step };
};

/** Prunes the follower store to completion. */
export const pruneAll = async (store: FactStore): Promise<void> => {
  for (;;) {
    const pruned = await store.prune();
    if ("kind" in pruned) throw new Error(`prune: ${pruned.kind}`);
    if (pruned.done) return;
  }
};

/** A simulated operator-set chain over `store`, its lists at genesis. */
export const operatorSetChainOver = async (
  fixture: OperatorSetChainFixture,
  store: FactStore,
) => {
  const driver = new ChainDriver(store, operatorSetTrackedSet(fixture.config));
  await driver.init();
  const lists = new OperatorSetChain(fixture);
  const live = () => driver.chain.live();
  const land = (...parts: TxParts[]) =>
    driver.forward([txOf(parts, driver.chain.nonce())]);
  const unrelated = (): SimTx => ({
    inputs: [driver.chain.outsideInput()],
    outputs: [
      { address: simUniverse().untrackedAddress, lovelace: 2_000_000n },
    ],
    nonce: driver.chain.nonce(),
  });
  const idle = async (blocks: number) => {
    for (let i = 0; i < blocks; i += 1) await driver.forward([unrelated()]);
  };
  await land(lists.genesis(driver.chain.outsideInput()));
  return { store, driver, lists, live, land, idle };
};

export type OperatorSetChainOver = Awaited<
  ReturnType<typeof operatorSetChainOver>
>;
