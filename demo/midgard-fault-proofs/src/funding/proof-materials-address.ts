import {
  credentialToAddress,
  getAddressDetails,
  type Network,
} from "@lucid-evolution/lucid";

/**
 * Where the prover keeps its in-flight proof materials (proof chunks, field
 * carriage, staged script-decode bytes, staged operands, transition-trace
 * data carriage): the base address whose payment and stake credentials are
 * both the prover's payment key hash. Its money stays at the plain
 * enterprise address of that key (the funding address), so the two never
 * share an address and a read of one by its full address never returns an
 * output of the other. The same key signs for both. Nothing else constructs
 * this address.
 */
export const proofMaterialsAddress = (
  network: Network,
  paymentKeyHash: string,
): string =>
  credentialToAddress(
    network,
    { type: "Key", hash: paymentKeyHash },
    { type: "Key", hash: paymentKeyHash },
  );

/**
 * The proof-materials address that goes with `fundingAddress`, the plain
 * enterprise key address of the prover: same key, same network.
 */
export const proofMaterialsAddressOfFundingAddress = (
  fundingAddress: string,
): string => {
  const { networkId, paymentCredential, stakeCredential } =
    getAddressDetails(fundingAddress);
  if (paymentCredential?.type !== "Key" || stakeCredential !== undefined)
    throw new Error(
      "the funding address must be a plain enterprise key address",
    );
  // Every testnet shares network id 0 and its address encoding.
  return proofMaterialsAddress(
    networkId === 1 ? "Mainnet" : "Preprod",
    paymentCredential.hash,
  );
};

/**
 * Whether `address` is one of the prover's own two addresses: its funding
 * address `fundingAddress` or the proof-materials address of the same key.
 * A funding reservation tracks the prover's outputs at both; only those at
 * the funding address fund a transaction.
 */
export const isProverOwnAddress = (
  fundingAddress: string,
  address: string,
): boolean =>
  address === fundingAddress ||
  address === proofMaterialsAddressOfFundingAddress(fundingAddress);
