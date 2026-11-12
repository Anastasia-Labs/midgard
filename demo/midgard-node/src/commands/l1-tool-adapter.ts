/**
 * The tool adapter layers for the Lucid and contract services
 * (`services/l1-adapter.ts`): every CLI command that builds the node's Lucid
 * service or contract bundle provides these, so its clients and a derived
 * bundle's slot-mapping read go through the tool access `--l1` selects
 * (`l1-command-access.ts`) and never open a role's follower store.
 */
import { Layer } from "effect";

import type { NodeConfigDep } from "../services/config.js";
import { nodeSeededAddresses } from "../services/intent-journal.tracked-set.js";
import {
  L1Adapter,
  L1ChainParameters,
  openAdapterScoped,
} from "../services/l1-adapter.js";
import { Lucid } from "../services/lucid.js";
import { MidgardContractServices } from "../services/midgard-contracts.js";
import { openToolL1Access } from "./l1-command-access.js";

const openTool = (config: NodeConfigDep) =>
  openAdapterScoped(config, () =>
    openToolL1Access({
      network: config.NETWORK,
      ...(config.L1_NATIVE_LEDGER === undefined
        ? {}
        : { nativeLedger: config.L1_NATIVE_LEDGER }),
      nodeBehindMaxMs: config.L1_NODE_BEHIND_MAX_MS,
      ownWallets: nodeSeededAddresses(config),
    }),
  );

export const ToolL1AdapterLive = Layer.succeed(L1Adapter, {
  role: "tool",
  open: openTool,
});

/** A command's chain parameters: over the tool access `--l1` selects. */
export const ToolL1ChainParametersLive = Layer.succeed(L1ChainParameters, {
  open: openTool,
});

/** The Lucid service of a command: over the tool access `--l1` selects. */
export const ToolLucidLive = Lucid.Default.pipe(
  Layer.provide(ToolL1AdapterLive),
);

/** The contract services of a command: a derived bundle's slot mapping read
 * over the tool access `--l1` selects. */
export const ToolMidgardContractServices = MidgardContractServices.pipe(
  Layer.provide(ToolL1ChainParametersLive),
);
