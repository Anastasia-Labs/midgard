/**
 * Our computation threads on the follower's simulator chain: thread outputs
 * at a followed step script, each holding one token
 * `policy ++ categoryId ++ headerHash` and a step datum naming its fraud
 * prover, beside the state queue they dispute. A pure function of the
 * chain, as the other simulated traffic is; no Plutus evaluation is
 * claimed.
 */
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

import type { WatcherProjectionDeployment } from "../../src/l1-follower/projection.js";
import { D, harness, X } from "./l1-follower-raw-reads-fixture.js";
import { removeTailTx } from "./l1-follower-state-queue-removal.js";
import {
  commitTx,
  datum,
  headerOf,
  initTx,
  mergeTx,
  queueState,
  scriptAddress,
} from "./l1-follower-state-queue-traffic.js";

export const THREAD_POLICY = "7a".repeat(28);
export const STEP_SCRIPT = "7b".repeat(28);
export const OUR_KEY = "7c".repeat(28);
export const THEIR_KEY = "7d".repeat(28);
export const CATEGORY_ID = "00000001";
export const THREAD_DEPLOYMENT: WatcherProjectionDeployment = {
  ...D,
  followedScripts: [THREAD_POLICY, STEP_SCRIPT],
};
/** The removal depth the scenes reclaim at (the tip is depth 1). */
export const FINAL_DEPTH = 3;

const token = (name: string, quantity = 1n) =>
  new Map([[THREAD_POLICY, new Map([[name, quantity]])]]);

/** The scope a read of `OUR_KEY`'s threads takes. */
export const OUR_SCOPE = {
  policyId: THREAD_POLICY,
  proverKeyHash: OUR_KEY,
  finalDepth: FINAL_DEPTH,
} as const;

/** A queue with committed headers, and the thread traffic over it. */
export const threadScene = async (
  options: Readonly<{ headers?: number; retainedDepth?: number }> = {},
) => {
  const h = await harness(THREAD_DEPLOYMENT, undefined, options.retainedDepth);
  await h.forward([initTx(THREAD_DEPLOYMENT)]);
  const headers: string[] = [];
  for (let i = 0; i < (options.headers ?? 1); i += 1) {
    await h.forward([
      commitTx(queueState(h.chain, THREAD_DEPLOYMENT)!, THREAD_DEPLOYMENT),
    ]);
    headers.push(
      headerOf(
        queueState(h.chain, THREAD_DEPLOYMENT)!.tail,
        THREAD_DEPLOYMENT,
      )!,
    );
  }
  const state = () => queueState(h.chain, THREAD_DEPLOYMENT)!;
  /** A thread of `prover`'s on `headerHash` at the step script. */
  const threadTx = (
    headerHash: string,
    prover: string = OUR_KEY,
    lovelace = 3_000_000n,
    name = CATEGORY_ID + headerHash,
  ): SimTx => ({
    inputs: [h.chain.outsideInput()],
    outputs: [
      {
        address: scriptAddress(STEP_SCRIPT),
        lovelace,
        assets: token(name),
        datum: datum(
          Data.to(
            { fraud_prover: prover, data: null },
            SDK.FraudProofComputationThreadStepDatum,
          ),
        ),
      },
    ],
    mint: token(name),
    nonce: h.chain.nonce(),
  });
  /** Cancels the thread at `outRef` (its token burns, its ada leaves). */
  const cancelTx = (outRef: string): SimTx => {
    const [txHash, index] = outRef.split("#") as [string, string];
    const thread = h.chain
      .live()
      .find(
        ({ outRef: live }) =>
          live.txHash.toString("hex") === txHash &&
          live.index === Number(index),
      );
    const [name] = [
      ...(thread?.output.assets?.get(THREAD_POLICY)?.keys() ?? []),
    ];
    if (name === undefined) throw new Error(`no live thread at ${outRef}`);
    return {
      inputs: [{ txHash: Buffer.from(txHash, "hex"), index: Number(index) }],
      outputs: [{ address: X, lovelace: 2_000_000n }],
      mint: token(name, -1n),
      nonce: h.chain.nonce(),
    };
  };
  /** Lands one thread and returns its out-ref. */
  const open = async (
    ...args: Parameters<typeof threadTx>
  ): Promise<string> => {
    const [txHash] = (await h.forward([threadTx(...args)])).hashes as [string];
    return `${txHash}#0`;
  };
  const empty = async (blocks: number): Promise<void> => {
    for (let i = 0; i < blocks; i += 1) await h.forward([]);
  };
  return {
    ...h,
    headers,
    threadTx,
    cancelTx,
    open,
    empty,
    removeTail: async () =>
      await h.forward([removeTailTx(state(), THREAD_DEPLOYMENT)]),
    mergeOldest: async () =>
      await h.forward([mergeTx(state(), THREAD_DEPLOYMENT)]),
  };
};
