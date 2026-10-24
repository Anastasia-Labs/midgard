/**
 * The node's follower plan (`l1FollowerPlan`) for an always-succeeds
 * deployment, from the two origin inputs: the operator's `L1_ORIGIN` and the
 * manifest's recorded `l1Origin`. The local node settings are placeholders.
 */
import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import type { OutRef } from "@al-ft/midgard-l1-follower";
import { Effect } from "effect";

import { AlwaysSucceedsContract } from "../../src/services/always-succeeds.js";
import { l1FollowerPlan } from "../../src/services/l1-follower.plan.js";
import { withRealEventHistoryForTest } from "./event-history.js";

export const nodeFollowerPlanFor = (
  input: Readonly<{
    l1Origin: L1Origin | null;
    recordedOrigin: L1Origin | undefined;
    hubOracleOneShot: OutRef;
  }>,
) =>
  Effect.gen(function* () {
    const oneShot = {
      txHash: input.hubOracleOneShot.txHash.toString("hex"),
      outputIndex: input.hubOracleOneShot.index,
    };
    const contracts = withRealEventHistoryForTest(
      yield* AlwaysSucceedsContract,
      oneShot,
    );
    return l1FollowerPlan({
      config: {
        L1_NATIVE_LEDGER: {
          socketPath: "/run/node.socket",
          nodeConfigPath: "/etc/node/config.json",
          binaryPath: "/usr/bin/l1-node-transport",
        },
        L1_ORIGIN: input.l1Origin,
        HUB_ORACLE_ONE_SHOT_TX_HASH: oneShot.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: oneShot.outputIndex,
        L1_TX_CONTENT_SOURCES: [],
        NETWORK: "Preprod",
        POSTGRES_HOST: "localhost",
        POSTGRES_PORT: 5432,
        POSTGRES_USER: "node",
        POSTGRES_PASSWORD: "secret",
        POSTGRES_DB: "midgard",
      },
      contracts,
      manifest:
        input.recordedOrigin === undefined
          ? undefined
          : { l1Origin: input.recordedOrigin },
      securityParameter: 2_160,
    });
  }).pipe(Effect.provide(AlwaysSucceedsContract.Default));
