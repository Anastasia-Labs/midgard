/**
 * The follower's operator-set wiring (NC14, D-N7): the driver hook that
 * keeps the operator set from the facts (only the rows that changed since
 * its last read) and publishes it, with the landed queue's tail, for the
 * watchdog, and this operator's membership as the liveness reason
 * `operator_removed`. Without a readable operator key there is no hook; the
 * follower runs without it.
 */
import type { DepthParameters, FactStore } from "@al-ft/midgard-l1-follower";
import { Effect, Ref, Runtime } from "effect";

import type { DriverHook } from "../l1-events/driver.js";
import {
  createOperatorSetMirror,
  databaseActivityRecord,
  memoryActivityRecord,
  type OperatorSetConfig,
  operatorSetHook,
  publishedOperatorSetOf,
  publishOperatorMembership,
  type stateQueueTailOf,
} from "../l1-operator-set/index.js";
import { resolveOwnOperatorKeyHashProgram } from "../transactions/operators/takeover.js";
import type { NodeConfig } from "./config.js";
import type { Database } from "./database.js";
import { Globals } from "./globals.globals.js";
import { Lucid } from "./lucid.js";
import { ContractDeploymentIdentity } from "./midgard-contracts.js";

export type StateQueueTail = ReturnType<typeof stateQueueTailOf>;

export type FollowerOperatorSet = Readonly<{
  /** The driver hook; undefined when the operator key is unreadable. */
  hook: DriverHook | undefined;
  /** Records the landed queue's tail of the current driver run. */
  setStateQueueTail: (tail: StateQueueTail) => void;
  /** This operator's key hash; undefined when it is unreadable. */
  ownKey: string | undefined;
}>;

export const followerOperatorSet = (options: {
  readonly store: FactStore;
  readonly config: OperatorSetConfig;
  readonly depth: DepthParameters;
}) =>
  Effect.gen(function* () {
    const { store, config } = options;
    const identity = yield* ContractDeploymentIdentity;
    const globals = yield* Globals;
    const lucid = yield* Lucid;
    const runtime = yield* Effect.runtime<never>();
    const dbRuntime = yield* Effect.runtime<Database | NodeConfig>();
    let stateQueueTail: StateQueueTail = null;
    const setStateQueueTail = (tail: StateQueueTail) => {
      stateQueueTail = tail;
    };
    const ownKey = yield* Effect.either(
      resolveOwnOperatorKeyHashProgram(lucid.operatorMainAddress),
    );
    if (ownKey._tag === "Left") {
      yield* Effect.logWarning(
        `L1 follower: no operator-set hook, the operator key is unreadable: ${ownKey.left.message}`,
      );
      return {
        hook: undefined,
        setStateQueueTail,
        ownKey: undefined,
      } as FollowerOperatorSet;
    }
    const hook = operatorSetHook({
      store,
      mirror: createOperatorSetMirror({ config, ownKey: ownKey.right }),
      depth: options.depth,
      activity:
        identity.manifestId === undefined
          ? memoryActivityRecord()
          : databaseActivityRecord({
              run: (effect) => Runtime.runPromise(dbRuntime)(effect),
              manifestId: identity.manifestId,
              ownKey: ownKey.right,
            }),
      publish: ({ set, membership }) =>
        Runtime.runPromise(runtime)(
          Effect.gen(function* () {
            yield* Ref.set(
              globals.OPERATOR_SET,
              publishedOperatorSetOf({
                set,
                membership,
                stateQueueTail,
                store,
                retired: config.retired,
              }),
            );
            yield* publishOperatorMembership(membership).pipe(
              Effect.provideService(Globals, globals),
            );
          }),
        ),
    });
    return {
      hook,
      setStateQueueTail,
      ownKey: ownKey.right,
    } as FollowerOperatorSet;
  });
