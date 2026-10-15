import {
  credentialToAddress,
  type Network,
  type TxOutput,
} from "@lucid-evolution/lucid";

/**
 * Where a slash pays its bounty: the claimant's enterprise address and the
 * exact reward in lovelace.
 */
export type BountyRoutingPlan = {
  readonly claimantAddress: string;
  readonly lovelace: bigint;
};

/**
 * The address a bounty for `claimantKeyHash` is paid to: the enterprise
 * address of that key, as `operator_directory.bounty_routing_is_exact_v1`
 * requires.
 */
export const bountyClaimantAddress = (
  network: Network,
  claimantKeyHash: string,
): string =>
  credentialToAddress(network, { type: "Key", hash: claimantKeyHash });

/**
 * Off-chain mirror of `operator_directory.bounty_routing_is_exact_v1`, so a
 * builder that pays the wrong shape fails here rather than on-chain. Exactly
 * one output goes to the claimant's address, and it holds exactly the reward
 * in lovelace, nothing else in its value, no datum and no reference script.
 */
export const requireExactBountyRouting = (
  outputs: readonly TxOutput[],
  plan: BountyRoutingPlan,
  label: string,
): void => {
  const claimantOutputs = outputs.filter(
    (output) => output.address === plan.claimantAddress,
  );
  if (claimantOutputs.length !== 1) {
    throw new Error(
      `${label} must create exactly one output at the bounty claimant address; found ${claimantOutputs.length.toString()}`,
    );
  }
  const [output] = claimantOutputs;
  const exact =
    output!.assets["lovelace"] === plan.lovelace &&
    Object.keys(output!.assets).length === 1 &&
    output!.datum == null &&
    output!.datumHash == null &&
    (output!.scriptRef ?? null) === null;
  if (!exact) {
    throw new Error(
      `${label} must pay exactly ${plan.lovelace.toString()} lovelace, ADA only, with no datum and no reference script, to the bounty claimant`,
    );
  }
};
