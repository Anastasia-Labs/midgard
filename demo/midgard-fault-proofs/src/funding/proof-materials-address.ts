import * as SDK from "@al-ft/midgard-sdk/fraud-proof";
import {
  getAddressDetails,
  type Network,
  type Script,
  type TxBuilder,
  validatorToAddress,
} from "@lucid-evolution/lucid";

/**
 * The native script `sig(paymentKeyHash)` that locks the prover's proof
 * materials: the prover's key alone spends them, by attaching this script
 * (no Plutus, no collateral) and that key's signature. It is the dispute's
 * operand staging script, so staged operands are proof materials too.
 */
export const proofMaterialsScript = (paymentKeyHash: string): Script =>
  SDK.midgardCekStagingScript(paymentKeyHash);

/**
 * Where the prover keeps its in-flight proof materials (proof chunks, field
 * carriage, staged script-decode bytes, transition-trace data carriage, and
 * the dispute's staged operands, whose staging address this is): the
 * enterprise address of `proofMaterialsScript(paymentKeyHash)`, a script
 * credential with no stake part. It is as long as the enterprise key
 * address (29 bytes), so a publication there is exactly as large as one at
 * the key address, and its payment credential differs from the funding
 * address's, so the two never share an address or a credential. Its money
 * stays at the plain enterprise key address (the funding address). Nothing
 * else constructs this address.
 */
export const proofMaterialsAddress = (
  network: Network,
  paymentKeyHash: string,
): string => validatorToAddress(network, proofMaterialsScript(paymentKeyHash));

/**
 * Lets `tx` spend outputs at the proof-materials address of
 * `paymentKeyHash`: attaches its `sig` script once and declares the key as a
 * signer, since Lucid's seed wallet signs only for inputs at its own (the
 * funding) address and for declared signers. Its only witness costs are the
 * 32-byte script and the key's signature: no Plutus, no collateral.
 */
export const spendProofMaterials = (
  tx: TxBuilder,
  paymentKeyHash: string,
): TxBuilder => {
  const script = proofMaterialsScript(paymentKeyHash);
  // eslint-disable-next-line midgard/fault-proof-reference-scripts-only -- our own sig(key hash) lock on our proof materials, not a fault-proof script.
  return tx.attach.SpendingValidator(script).addSignerKey(paymentKeyHash);
};

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

/** Whether `inputs` include an output at the proof-materials address of `fundingAddress`. */
export const spendsProofMaterial = (
  fundingAddress: string,
  inputs: Iterable<{ readonly address: string }>,
): boolean => {
  const materials = proofMaterialsAddressOfFundingAddress(fundingAddress);
  return [...inputs].some((input) => input.address === materials);
};
