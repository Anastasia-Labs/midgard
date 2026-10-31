import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";

import type { FraudSlashFundingAuthority } from "../remove-fraudulent-block.js";
import { exactResolvedOutputCbor } from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";

export const assertAuthenticatedFraudSlashReward = (
  slash: FraudSlashFundingAuthority,
  references: ReadonlyMap<string, UTxO>,
  contracts: ReadonlyMap<string, unknown>,
  walletAddress: string,
): void => {
  // The locally evaluated canonical removal authenticates this NFT source
  // under the finalized manifest. A datum alone never establishes identity.
  const proof = references.get(slash.fraudProofOutRef);
  if (
    proof === undefined ||
    proof.address !== slash.fraudProofAddress ||
    !contracts.has(proof.address) ||
    proof.assets[slash.fraudProofUnit] !== 1n ||
    proof.datum == null ||
    exactResolvedOutputCbor(proof) !== slash.fraudProofResolvedOutputCborHex
  )
    throw new Error("fraud slash authenticated fraud-proof reference changed");
  const prover = Data.from(proof.datum, FraudProofTokenDatum).fraud_prover;
  const address = CML.EnterpriseAddress.new(
    CML.Address.from_bech32(walletAddress).network_id(),
    CML.Credential.new_pub_key(CML.Ed25519KeyHash.from_hex(prover)),
  )
    .to_address()
    .to_bech32();
  if (slash.rewardAddress !== address)
    throw new Error(
      "fraud slash reward differs from authenticated fraud-proof prover",
    );
};

export const isExactFraudSlashRewardOutput = (
  output: CML.TransactionOutput,
  slash: FraudSlashFundingAuthority,
): boolean => {
  if (output.address().to_bech32() !== slash.rewardAddress) return false;
  if (
    output.amount().coin().toString() !== slash.rewardLovelace ||
    output.amount().has_multiassets() ||
    output.datum() !== undefined ||
    output.script_ref() !== undefined
  )
    throw new Error("fraud slash reward differs from exact release economics");
  return true;
};
