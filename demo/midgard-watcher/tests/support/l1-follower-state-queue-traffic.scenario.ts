/**
 * The fork simulator's scenario traffic over the state-queue txs of
 * `l1-follower-state-queue-traffic.ts`: per block, the init tx, appends,
 * attestations and applies, merges, stray attestations and stranger
 * payments, each drawn with its configured chance.
 */
import type {
  ScenarioTraffic,
  SimTx,
  SimUtxo,
} from "@al-ft/midgard-l1-follower/testing";

import type { WatcherProjectionDeployment } from "../../src/l1-follower/projection.js";
import {
  applyTx,
  attestTx,
  commitTx,
  headerOf,
  initTx,
  mergeTx,
  outsideInput,
  queueState,
  scriptAddress,
  SIM_WATCHER_DEPLOYMENT,
  strayAttestTx,
} from "./l1-follower-state-queue-traffic.js";

/** Plain ADA paid to the queue or lock address by a stranger: never a queue output. */
const strangerTx = (
  n: number,
  deployment: WatcherProjectionDeployment,
): SimTx => ({
  inputs: [outsideInput(n % 64)],
  outputs: [
    {
      address: scriptAddress(
        n % 2 === 0
          ? deployment.stateQueueSpend
          : deployment.correctionLockSpend,
      ),
      lovelace: 3_000_000n,
    },
  ],
  nonce: 950_000 + n,
});

/**
 * Per block: the init tx while there is no queue, else (with probability
 * `commitChance`) one append on the live tail; sometimes a stranger payment.
 */
export const stateQueueTraffic = (
  options: Readonly<{
    deployment?: WatcherProjectionDeployment;
    commitChance?: number;
    strangerChance?: number;
    /** Attest the tail, or apply a live attestation. */
    attestChance?: number;
    /** Merge the oldest header while three or more are queued. */
    mergeChance?: number;
    /** Mint a DAAT naming a header that was never queued. */
    strayAttestChance?: number;
    /** Where the init tx puts the root (the state-queue address unless set). */
    rootAddress?: Buffer;
    /** Apply sets the node's DA status to Attested. */
    attestNodes?: boolean;
  }> = {},
): ScenarioTraffic => {
  const deployment = options.deployment ?? SIM_WATCHER_DEPLOYMENT;
  let strangers = 0;
  let strays = 0;
  return ({ chain, rng, claim }) => {
    const txs: SimTx[] = [];
    const state = queueState(chain, deployment, options.rootAddress);
    if (state === null) {
      if (rng.chance(0.5)) txs.push(initTx(deployment, options.rootAddress));
    } else {
      if (
        state.length >= 4 &&
        rng.chance(options.mergeChance ?? 0) &&
        claim((state.ordered[0] as SimUtxo).outRef) &&
        claim((state.ordered[1] as SimUtxo).outRef)
      )
        txs.push(mergeTx(state, deployment));
      if (rng.chance(options.attestChance ?? 0)) {
        const applicable = state.ordered.find((node) => {
          const header = headerOf(node, deployment);
          return header !== null && state.attestations.has(header);
        });
        if (applicable !== undefined) {
          const header = headerOf(applicable, deployment) as string;
          const attestation = state.attestations.get(header) as SimUtxo;
          if (claim(applicable.outRef) && claim(attestation.outRef))
            txs.push(
              applyTx(
                applicable,
                attestation,
                header,
                deployment,
                options.attestNodes === true,
              ),
            );
        } else if (state.tailHeaderHash !== null)
          txs.push(attestTx(state, deployment));
      }
      if (rng.chance(options.commitChance ?? 0.5) && claim(state.tail.outRef))
        txs.push(commitTx(state, deployment));
    }
    if (
      options.strayAttestChance !== undefined &&
      rng.chance(options.strayAttestChance)
    ) {
      txs.push(strayAttestTx(strays, deployment));
      strays += 1;
    }
    if (rng.chance(options.strangerChance ?? 0.1)) {
      txs.push(strangerTx(strangers, deployment));
      strangers += 1;
    }
    return txs;
  };
};
