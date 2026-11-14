import type { ChainSyncEvent } from "@al-ft/l1-node-transport";

import type { OutRef } from "../types.js";
import { SIM_ORIGIN } from "./run-scenario.js";
import { SimChain, simUniverse } from "./sim-chain.js";

/**
 * A short simulated chain from `SIM_ORIGIN` holding a deployment's protocol
 * init: the first block creates the hub-oracle one-shot at a tracked
 * address, the second spends it, then `after` empty blocks follow. Every
 * event carries the chain's final tip, as a follower behind the node sees
 * it, so only the last event is at the tip.
 */
export const simProtocolInitChain = (
  after = 2,
): Readonly<{
  events: readonly ChainSyncEvent[];
  hubOracleOneShot: OutRef;
}> => {
  const universe = simUniverse();
  const chain = new SimChain(universe, SIM_ORIGIN);
  const prepare = chain.forward([
    {
      inputs: [chain.outsideInput()],
      outputs: [{ address: universe.trackedAddress, lovelace: 2_000_000n }],
      nonce: chain.nonce(),
    },
  ]);
  const hubOracleOneShot = { txHash: prepare.encoded.txHashes[0]!, index: 0 };
  const events: ChainSyncEvent[] = [
    prepare.event,
    chain.forward([
      {
        inputs: [hubOracleOneShot],
        outputs: [{ address: universe.untrackedAddress, lovelace: 2_000_000n }],
        nonce: chain.nonce(),
      },
    ]).event,
  ];
  for (let i = 0; i < after; i += 1) events.push(chain.forward([]).event);
  const tip = events[events.length - 1]!.tip;
  return {
    events: events.map((event) => ({ ...event, tip })),
    hubOracleOneShot,
  };
};
