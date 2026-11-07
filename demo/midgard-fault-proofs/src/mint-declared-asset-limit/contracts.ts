import {
  buildMintDeclaredAssetLimitChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import {
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  MINT_DECLARED_ASSET_LIMIT_CATEGORY,
  MINT_DECLARED_ASSET_LIMIT_CATEGORY_ID,
} from "./family.js";

export const MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES = Object.freeze([
  "fraud_proofs/mint_declared_asset_limit/step_01.main.spend",
  "fraud_proofs/mint_declared_asset_limit/step_02.main.spend",
  "fraud_proofs/mint_declared_asset_limit/step_03.main.spend",
  "fraud_proofs/mint_declared_asset_limit/step_04.main.spend",
] as const);

export type MintDeclaredAssetLimitStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;

export type MintDeclaredAssetLimitContracts = Readonly<{
  steps: readonly [
    MintDeclaredAssetLimitStepContract,
    MintDeclaredAssetLimitStepContract,
    MintDeclaredAssetLimitStepContract,
    MintDeclaredAssetLimitStepContract,
  ];
  computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptAddress: string;
  };
  hubOraclePolicyId: string;
  stateQueuePolicyId: string;
  fieldPreimageCertificatePolicyId: string;
  fieldPreimageCertificateMintingScript: Script;
}>;

export type MintDeclaredAssetLimitBlueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

/** Applies the four scripts backwards, in their blueprint-declared order. */
export const applyMintDeclaredAssetLimitScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: MintDeclaredAssetLimitBlueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): MintDeclaredAssetLimitContracts["steps"] => {
  const { steps } = Effect.runSync(
    buildMintDeclaredAssetLimitChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId,
    }),
  );
  const titles = Object.values(MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES);
  const adapt = (step: SpendingValidator, index: number) =>
    Object.freeze({
      blueprintTitle: titles[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${"0".repeat(64)}#0`,
    });
  return [
    adapt(steps[0], 0),
    adapt(steps[1], 1),
    adapt(steps[2], 2),
    adapt(steps[3], 3),
  ];
};

export type MintDeclaredAssetLimitManifest = Readonly<{
  schemaVersion: "mint-declared-asset-limit-production-manifest-v1";
  category: typeof MINT_DECLARED_ASSET_LIMIT_CATEGORY;
  categoryId: typeof MINT_DECLARED_ASSET_LIMIT_CATEGORY_ID;
  network: Network;
  contracts: MintDeclaredAssetLimitContracts;
}>;

const hex = (value: string, bytes: number, label: string): void => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value))
    throw new Error(`mintDeclaredAssetLimit: ${label} is not canonical hex`);
};

export const loadMintDeclaredAssetLimitManifest = (
  manifest: MintDeclaredAssetLimitManifest,
): MintDeclaredAssetLimitManifest => {
  if (
    manifest.schemaVersion !==
      "mint-declared-asset-limit-production-manifest-v1" ||
    manifest.category !== MINT_DECLARED_ASSET_LIMIT_CATEGORY ||
    manifest.categoryId !== MINT_DECLARED_ASSET_LIMIT_CATEGORY_ID
  )
    throw new Error("mintDeclaredAssetLimit: manifest identity changed");
  manifest.contracts.steps.forEach((step, index) => {
    if (
      step.blueprintTitle !== MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES[index]
    )
      throw new Error(
        "mintDeclaredAssetLimit: ordered blueprint title changed",
      );
    if (validatorToScriptHash(step.spendingScript) !== step.spendingScriptHash)
      throw new Error("mintDeclaredAssetLimit: applied script hash changed");
    if (
      validatorToAddress(manifest.network, step.spendingScript) !==
      step.spendingScriptAddress
    )
      throw new Error("mintDeclaredAssetLimit: applied script address changed");
    if (!/^[0-9a-f]{64}#[0-9]+$/u.test(step.referenceOutRef))
      throw new Error(
        "mintDeclaredAssetLimit: reference out-ref is not canonical",
      );
  });
  hex(manifest.contracts.computationThread.policyId, 28, "thread policy");
  hex(manifest.contracts.fraudProof.policyId, 28, "proof policy");
  hex(manifest.contracts.hubOraclePolicyId, 28, "hub oracle policy");
  hex(manifest.contracts.stateQueuePolicyId, 28, "state queue policy");
  hex(
    manifest.contracts.fieldPreimageCertificatePolicyId,
    28,
    "field certificate policy",
  );
  return Object.freeze(manifest);
};
