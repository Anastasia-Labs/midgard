/**
 * What a validation dispute's prover must hold for one thread move our
 * wallet pays, as the funded dispute tests and the devnet journey's prover
 * top-up read it. Nothing in the watcher checks it: a move our wallet cannot
 * fund does not build, and raises the operator's funding alarm.
 */
import type { DeploymentManifestCardanoProtocolParameters } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML } from "@lucid-evolution/lucid";

import { workflowRuntimeFundingMinimumFee } from "../../src/workflow/runtime-funding-fee.js";

/**
 * The reference-script bytes of the largest thread move of a validation-trace
 * dispute measured so far: a script-sources output proof finalize's hop
 * that attaches two descriptor yields
 * (tests/validation-trace-dispute-one-step-script-sources-68.test.ts).
 * Whether a move's thread or our wallet pays for it turns on the live
 * prices, so this bounds every measured move, not only those over the
 * per-move cap at today's prices.
 */
export const VALIDATION_DISPUTE_WALLET_MOVE = Object.freeze({
  referenceScriptBytes: 34_693n,
});

/**
 * What the prover's wallet must hold for one move it pays, under
 * `parameters`: the fee of a move of the protocol's largest signed size, our
 * wallet's inputs and change included, at the protocol's per-transaction
 * execution limit, reading the largest measured move's reference scripts,
 * and the minimum ada of its change, a plain coin returned to the funding
 * key. The builder selects our wallet's coins with the move's scripts
 * budgeted at that limit before it evaluates them, so a reserve priced at
 * the move's own execution units would not let it select. A funding-adequacy
 * check adds it to the bond and the case floor.
 */
export const validationDisputeWalletMoveReserve = ({
  parameters,
  fundingPaymentKeyHash,
}: {
  readonly parameters: DeploymentManifestCardanoProtocolParameters;
  readonly fundingPaymentKeyHash: string;
}): bigint => {
  const fee = workflowRuntimeFundingMinimumFee({
    parameters,
    transactionBytes: BigInt(parameters.maxTxSize),
    memory: BigInt(parameters.maxTxExUnits.memory),
    steps: BigInt(parameters.maxTxExUnits.steps),
    referenceScriptBytes: VALIDATION_DISPUTE_WALLET_MOVE.referenceScriptBytes,
  });
  // The largest plain change: a base address, the largest coin.
  const key = CML.Credential.new_pub_key(
    CML.Ed25519KeyHash.from_hex(fundingPaymentKeyHash),
  );
  const change = CML.TransactionOutput.new(
    CML.BaseAddress.new(0, key, key).to_address(),
    CML.Value.from_coin(0xffff_ffff_ffff_ffffn),
  );
  return (
    fee + CML.min_ada_required(change, BigInt(parameters.coinsPerUtxoByte))
  );
};
