import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofRawL1Utxo,
  type VerifiedFraudProofReleaseEconomicsPolicy,
} from "../../src/workflow/index.js";

export const hash32 = (byte: string): string => byte.repeat(32);

export const policy = (byte: string): string => byte.repeat(28);

export const OPERATOR = policy("11");

export const PROVER = policy("12");

export const DEPLOYMENT = hash32("13");

export const RELEASE = hash32("14");

export const finalityPolicy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };

export const FINALITY =
  computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy);

export const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  blueprintHash: RELEASE,
  policyDigest: FINALITY,
  policy: finalityPolicy,
};

export const SOURCE = "local-kupmios-family-test";

export const economicsPolicy = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
} as const;

export const releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy = {
  schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  blueprintHash: RELEASE,
  policyDigest: computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
  policy: economicsPolicy,
};

export const scriptAddress = (byte: string): string =>
  credentialToAddress("Preview", scriptHashToCredential(policy(byte)));

export const value = (assets: Readonly<Record<string, bigint>>): CML.Value => {
  const multiasset = CML.MultiAsset.new();
  for (const [unit, quantity] of Object.entries(assets)) {
    if (unit === "lovelace") continue;
    multiasset.set(
      CML.ScriptHash.from_hex(unit.slice(0, 56)),
      CML.AssetName.from_hex(unit.slice(56)),
      quantity,
    );
  }
  return CML.Value.new(assets.lovelace ?? 0n, multiasset);
};

export const output = ({
  address,
  assets,
  datum,
}: {
  readonly address: string;
  readonly assets: Readonly<Record<string, bigint>>;
  readonly datum?: string;
}): CML.TransactionOutput =>
  CML.TransactionOutput.new(
    CML.Address.from_bech32(address),
    value(assets),
    datum === undefined
      ? undefined
      : CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
  );

export const raw = (
  outRef: string,
  transactionOutput: CML.TransactionOutput,
): FraudProofRawL1Utxo => ({
  outRef,
  outputCbor: transactionOutput.to_canonical_cbor_hex(),
  datumCbor:
    transactionOutput.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null,
  referenceScriptCbor: null,
});

export const input = (outRef: string): CML.TransactionInput => {
  const [txHash, index] = outRef.split("#");
  return CML.TransactionInput.new(
    CML.TransactionHash.from_hex(txHash!),
    BigInt(index!),
  );
};
