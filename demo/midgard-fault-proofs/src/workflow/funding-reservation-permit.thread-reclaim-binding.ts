/**
 * The narrow authority of a thread reclaim: the cancel of one thread of ours
 * on a header another record removed. Our wallet pays the fee; the
 * transaction spends that one thread and nothing else outside the wallet,
 * burns exactly the thread's one token, and pays every output back to our
 * wallet. The runtime bound checks lineage, fees and value conservation as
 * for any action; this adds what makes the transaction a reclaim and no
 * more.
 */
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { TxSigned, UTxO } from "@lucid-evolution/lucid";

import { addMintedAssets } from "./funding-reservation-permit.ledger-accounting.js";
import type { PermitState } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import {
  isThreadReclaimInput,
  RECLAIM_THREAD_ACTION_KIND,
} from "./own-thread-reclaim.js";

/** Refuses a reclaim action's transaction unless it is exactly the cancel
 * of the action's own thread back to our wallet. Any other action passes. */
export const assertThreadReclaimBound = ({
  state,
  action,
  signed,
  inputs,
  slashOrDispute,
}: {
  readonly state: PermitState;
  readonly action: FraudProofWorkflowAction;
  readonly signed: TxSigned;
  readonly inputs: ReadonlyMap<string, UTxO>;
  readonly slashOrDispute: boolean;
}): void => {
  if (
    action.input.actionKind !== RECLAIM_THREAD_ACTION_KIND &&
    state.currentActionKind !== RECLAIM_THREAD_ACTION_KIND
  )
    return;
  const refuse = (binding: string): never => {
    throw new Error(
      `thread reclaim differs from its exact binding: ${binding}`,
    );
  };
  if (!isThreadReclaimInput(action.input)) return refuse("action input");
  if (state.currentActionKind !== RECLAIM_THREAD_ACTION_KIND)
    return refuse("current action kind");
  if (state.currentActionDigest !== computeDeploymentManifestJsonDigest(action))
    return refuse("action digest");
  if (slashOrDispute) return refuse("slash or dispute authority");
  const wallet = state.snapshot.walletAddress;
  const foreign = [...inputs].filter(([, input]) => input.address !== wallet);
  if (foreign.length !== 1 || foreign[0]![0] !== action.input.threadOutRef)
    return refuse("spends exactly its own thread");
  const thread = foreign[0]![1];
  const tokens = Object.entries(thread.assets).filter(
    ([unit]) => unit !== "lovelace",
  );
  if (tokens.length !== 1 || tokens[0]![1] !== 1n)
    return refuse("thread holds exactly one token");
  const minted = new Map<string, bigint>();
  const body = signed.toTransaction().body();
  addMintedAssets(minted, body.mint());
  if (minted.size !== 1 || minted.get(tokens[0]![0]) !== -1n)
    return refuse("burns exactly the thread's token");
  const outputs = body.outputs();
  for (let index = 0; index < outputs.len(); index += 1)
    if (outputs.get(index).address().to_bech32() !== wallet)
      return refuse("pays only our wallet");
  const collateralReturn = body.collateral_return();
  if (
    collateralReturn !== undefined &&
    collateralReturn.address().to_bech32() !== wallet
  )
    return refuse("collateral returns only to our wallet");
};
