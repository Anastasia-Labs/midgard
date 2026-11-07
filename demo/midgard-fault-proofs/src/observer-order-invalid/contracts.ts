import {
  buildObserverOrderInvalidChain,
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
  OBSERVER_ORDER_INVALID_CATEGORY,
  OBSERVER_ORDER_INVALID_CATEGORY_ID,
} from "./family.js";

export const OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES = Object.freeze([
  "fraud_proofs/observer_order_invalid/step_01.main.spend",
  "fraud_proofs/observer_order_invalid/step_02.main.spend",
  "fraud_proofs/observer_order_invalid/step_03.main.spend",
  "fraud_proofs/observer_order_invalid/step_04.main.spend",
] as const);

export type ObserverOrderInvalidStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;

export type ObserverOrderInvalidContracts = Readonly<{
  steps: readonly [
    ObserverOrderInvalidStepContract,
    ObserverOrderInvalidStepContract,
    ObserverOrderInvalidStepContract,
    ObserverOrderInvalidStepContract,
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

export type ObserverOrderInvalidBlueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

/** Applies the four scripts backwards, in their blueprint-declared order. */
export const applyObserverOrderInvalidScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: ObserverOrderInvalidBlueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): ObserverOrderInvalidContracts["steps"] => {
  const { steps } = Effect.runSync(
    buildObserverOrderInvalidChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId,
    }),
  );
  const titles = Object.values(OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES);
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

export type ObserverOrderInvalidManifest = Readonly<{
  schemaVersion: "observer-order-invalid-production-manifest-v1";
  category: typeof OBSERVER_ORDER_INVALID_CATEGORY;
  categoryId: typeof OBSERVER_ORDER_INVALID_CATEGORY_ID;
  network: Network;
  contracts: ObserverOrderInvalidContracts;
}>;

const hex = (value: string, bytes: number, label: string): void => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value))
    throw new Error(`observerOrderInvalid: ${label} is not canonical hex`);
};

export const loadObserverOrderInvalidManifest = (
  manifest: ObserverOrderInvalidManifest,
): ObserverOrderInvalidManifest => {
  if (
    manifest.schemaVersion !==
      "observer-order-invalid-production-manifest-v1" ||
    manifest.category !== OBSERVER_ORDER_INVALID_CATEGORY ||
    manifest.categoryId !== OBSERVER_ORDER_INVALID_CATEGORY_ID
  )
    throw new Error("observerOrderInvalid: manifest identity changed");
  manifest.contracts.steps.forEach((step, index) => {
    if (step.blueprintTitle !== OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES[index])
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
