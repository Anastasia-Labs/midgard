import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  OBSERVER_ORDER_INVALID_CATEGORY_ID_V1,
  OBSERVER_ORDER_INVALID_CATEGORY_V1,
} from "./family-v1.js";

export const OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES_V1 = Object.freeze([
  "fraud_proofs/observer_order_invalid/step_01.main.spend",
  "fraud_proofs/observer_order_invalid/step_02.main.spend",
  "fraud_proofs/observer_order_invalid/step_03.main.spend",
  "fraud_proofs/observer_order_invalid/step_04.main.spend",
] as const);

export type ObserverOrderInvalidStepContractV1 = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;

export type ObserverOrderInvalidContractsV1 = Readonly<{
  steps: readonly [
    ObserverOrderInvalidStepContractV1,
    ObserverOrderInvalidStepContractV1,
    ObserverOrderInvalidStepContractV1,
    ObserverOrderInvalidStepContractV1,
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

export type ObserverOrderInvalidBlueprintV1 = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

const applyExact = (
  blueprint: ObserverOrderInvalidBlueprintV1,
  title: string,
  parameters: readonly Data[],
): Script => {
  const validator = blueprint.validators.find((entry) => entry.title === title);
  if (validator === undefined)
    throw new Error(`observerOrderInvalid: blueprint omitted ${title}`);
  if ((validator.parameters?.length ?? 0) !== parameters.length)
    throw new Error(`observerOrderInvalid: ${title} parameter arity changed`);
  return {
    type: "PlutusV3",
    script: applyParamsToScript(validator.compiledCode, [...parameters]),
  };
};

/** Applies the four scripts backwards, in their blueprint-declared order. */
export const applyObserverOrderInvalidScriptsV1 = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: ObserverOrderInvalidBlueprintV1;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): ObserverOrderInvalidContractsV1["steps"] => {
  const applied = (
    index: number,
    parameters: readonly Data[],
  ): ObserverOrderInvalidStepContractV1 => {
    const blueprintTitle = OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES_V1[index]!;
    const spendingScript = applyExact(blueprint, blueprintTitle, parameters);
    return Object.freeze({
      blueprintTitle,
      spendingScript,
      spendingScriptHash: validatorToScriptHash(spendingScript),
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      referenceOutRef: "0".repeat(64).concat("#0"),
    });
  };
  const step04 = applied(3, [
    fraudProofPolicyId,
    fraudProofTokenAddressData,
    computationThreadPolicyId,
  ]);
  const step03 = applied(2, [
    step04.spendingScriptHash,
    computationThreadPolicyId,
    fieldPreimageCertificatePolicyId,
  ]);
  const step02 = applied(1, [
    step03.spendingScriptHash,
    computationThreadPolicyId,
    fieldPreimageCertificatePolicyId,
  ]);
  const step01 = applied(0, [
    step02.spendingScriptHash,
    computationThreadPolicyId,
    hubOracleScriptHash,
  ]);
  return [step01, step02, step03, step04];
};

export type ObserverOrderInvalidProductionManifestV1 = Readonly<{
  schemaVersion: "observer-order-invalid-production-manifest-v1";
  category: typeof OBSERVER_ORDER_INVALID_CATEGORY_V1;
  categoryId: typeof OBSERVER_ORDER_INVALID_CATEGORY_ID_V1;
  network: Network;
  contracts: ObserverOrderInvalidContractsV1;
}>;

const hex = (value: string, bytes: number, label: string): void => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value))
    throw new Error(`observerOrderInvalid: ${label} is not canonical hex`);
};

export const loadObserverOrderInvalidProductionManifestV1 = (
  manifest: ObserverOrderInvalidProductionManifestV1,
): ObserverOrderInvalidProductionManifestV1 => {
  if (
    manifest.schemaVersion !==
      "observer-order-invalid-production-manifest-v1" ||
    manifest.category !== OBSERVER_ORDER_INVALID_CATEGORY_V1 ||
    manifest.categoryId !== OBSERVER_ORDER_INVALID_CATEGORY_ID_V1
  )
    throw new Error("observerOrderInvalid: manifest identity changed");
  manifest.contracts.steps.forEach((step, index) => {
    if (
      step.blueprintTitle !== OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES_V1[index]
    )
      throw new Error("observerOrderInvalid: ordered blueprint title changed");
    if (validatorToScriptHash(step.spendingScript) !== step.spendingScriptHash)
      throw new Error("observerOrderInvalid: applied script hash changed");
    if (
      validatorToAddress(manifest.network, step.spendingScript) !==
      step.spendingScriptAddress
    )
      throw new Error("observerOrderInvalid: applied script address changed");
    if (!/^[0-9a-f]{64}#[0-9]+$/u.test(step.referenceOutRef))
      throw new Error(
        "observerOrderInvalid: reference out-ref is not canonical",
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
