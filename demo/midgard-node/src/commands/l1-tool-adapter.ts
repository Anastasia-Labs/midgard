/**
 * The tool adapter layer for the Lucid and contract services
 * (`services/l1-adapter.ts`): every CLI command that builds the node's Lucid
 * service or contract bundle provides this, so its clients (and a derived
 * bundle's slot-mapping read) go through the tool access `--l1` selects
 * (`l1-command-access.ts`) and never open a role's follower store.
 */
import { Layer } from "effect";

import { L1Adapter, openAdapterScoped } from "../services/l1-adapter.js";
import { Lucid } from "../services/lucid.js";
import { MidgardContractServices } from "../services/midgard-contracts.js";
import { openToolL1Access } from "./l1-command-access.js";

export const ToolL1AdapterLive = Layer.succeed(L1Adapter, {
  role: "tool",
  open: (config) =>
    openAdapterScoped(config, () =>
      openToolL1Access({
        network: config.NETWORK,
        ...(config.L1_NATIVE_LEDGER === undefined
          ? {}
          : { nativeLedger: config.L1_NATIVE_LEDGER }),
        nodeBehindMaxMs: config.L1_NODE_BEHIND_MAX_MS,
      }),
    ),
});

/** The Lucid service of a command: over the tool access `--l1` selects. */
export const ToolLucidLive = Lucid.Default.pipe(
  Layer.provide(ToolL1AdapterLive),
);

/** The contract services of a command: a derived bundle's slot mapping read
 * over the tool access `--l1` selects. */
export const ToolMidgardContractServices = MidgardContractServices.pipe(
  Layer.provide(ToolL1AdapterLive),
);
