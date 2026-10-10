/**
 * Landed-block processing for the production lifecycle, as the production
 * driver runs it: the hook (`landedBlockHook`) after the driver's sink, at
 * the change's view under the driver's write capability (`withDriverView`),
 * over a follower store in the node's database, with the node's ports and
 * the driver's recompute as the rebase.
 */
import {
  openPostgresFactStore,
  projectionStoreOptions,
} from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventProjectionConfigFromContracts,
} from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import type { Effect } from "effect";

import {
  forcedOrderConfigFromContracts,
  forcedOrderProjection,
} from "../../src/forced-orders/index.js";
import type { DriverHold } from "../../src/l1-events/driver.js";
import { queueTerminalProjection } from "../../src/l1-queue-terminals/index.js";
import { stateQueueProjectionConfig } from "../../src/l1-state-queue/index.js";
import {
  landedBlockHook,
  nodeLandedBlockPorts,
} from "../../src/landed-blocks/index.js";
import type { NodeLandedBlockContext } from "../../src/landed-blocks/node-ports.js";
import type { NodeConfigDep } from "../../src/services/config.js";
import type { Database } from "../../src/services/database.js";
import { withDriverView } from "../../src/services/follower-write-gate.driver.js";
import type { Globals } from "../../src/services/globals.js";
import type { EmulatorLandedBlocks } from "./emulator-l1-follower.driver.js";

export const openLandedBlocks = async (args: {
  readonly contracts: SDK.MidgardValidators;
  readonly nodeConfig: NodeConfigDep;
  /** k, the follower's security parameter. */
  readonly securityParameter: number;
  readonly run: <A>(
    effect: Effect.Effect<
      A,
      never,
      NodeLandedBlockContext | Database | Globals
    >,
  ) => Promise<A>;
  /** The driver's recompute when the rebase is due (`rebaseIfDue`). */
  readonly rebase: (reason: string) => Effect.Effect<DriverHold | undefined>;
}) => {
  const { contracts, nodeConfig } = args;
  const stateQueue = stateQueueProjectionConfig(contracts.stateQueue);
  const events = eventProjectionConfigFromContracts(
    SDK.requireEventHistoryContracts(contracts),
    nodeConfig.NETWORK === "Mainnet" ? 1 : 0,
  );
  const forcedOrders = forcedOrderConfigFromContracts(contracts);
  // Never started: the emulator follower host (`follower-emulator.host.ts`)
  // writes the facts under the writer lease, and a start would check its
  // own tracked set against them (and reset them on a difference).
  // Landed-block processing only reads facts and takes its own
  // transactions, neither of which needs the writer lease.
  const store = openPostgresFactStore({
    ...projectionStoreOptions(
      [
        eventProjection(events),
        queueTerminalProjection(stateQueue),
        forcedOrderProjection(forcedOrders),
      ],
      {
        securityParameter: args.securityParameter,
        trackedSet: {
          addresses: new Set(),
          paymentCredentials: new Set(),
          policies: new Set(),
        },
      },
      "postgres",
    ),
    connection: {
      connectionString: `postgresql://${encodeURIComponent(nodeConfig.POSTGRES_USER)}:${encodeURIComponent(nodeConfig.POSTGRES_PASSWORD)}@${nodeConfig.POSTGRES_HOST}:${nodeConfig.POSTGRES_PORT.toString()}/${encodeURIComponent(nodeConfig.POSTGRES_DB)}`,
    },
  });
  const ports = nodeLandedBlockPorts(
    store,
    { projection: events, forcedOrders, stateQueue },
    args.rebase,
  );
  const hook: EmulatorLandedBlocks = (change) =>
    landedBlockHook({
      store,
      config: stateQueue,
      ports,
      run: (effect) => args.run(withDriverView(change.view)(effect)),
    })(change);
  return { hook, close: () => store.close() };
};
