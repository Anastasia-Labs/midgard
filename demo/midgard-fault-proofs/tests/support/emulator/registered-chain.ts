import {
  AddressData,
  addressDataFromBech32,
  type BuildFaultProofContractsParams,
  type FraudProofCatalogueCategoryDeploymentInfo,
  parseFaultProofBlueprint,
  type SpendingValidator as SdkSpendingValidator,
} from "@al-ft/midgard-sdk";
import { Data, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  type Blueprint,
  readBlueprint,
  realBlueprintPath,
} from "./blueprints.js";

/**
 * The step shape every family-local `contracts.ts` declares: the registered
 * SDK spending validator plus the blueprint title it was applied from and a
 * placeholder reference out-ref (the emulator suites publish their own
 * reference UTxOs).
 */
export type RegisteredFamilyStep = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;

type RegisteredStep = Pick<
  SdkSpendingValidator,
  "spendingScript" | "spendingScriptHash" | "spendingScriptAddress"
>;

/** Family-shaped steps taken from the registered chain, in blueprint order. */
export const familyStepsFromRegisteredChain = <
  const Steps extends readonly RegisteredStep[],
>(
  steps: Steps,
  titles: readonly string[],
): { readonly [Index in keyof Steps]: RegisteredFamilyStep } => {
  if (titles.length !== steps.length)
    throw new Error(
      `registered chain has ${steps.length.toString()} steps but ${titles.length.toString()} blueprint titles were declared`,
    );
  return steps.map((step, index) =>
    Object.freeze({
      blueprintTitle: titles[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${"00".repeat(32)}#${index.toString()}`,
    }),
  ) as unknown as { readonly [Index in keyof Steps]: RegisteredFamilyStep };
};

/**
 * The family-side application and the registered SDK chain are two paths to
 * one deployed identity. A lifecycle suite asserts they agree step for step
 * before driving the registered chain, and that the catalogue category the
 * harness folded names that chain's first step.
 */
export const expectRegisteredChainParity = ({
  registered,
  applied,
  category,
}: {
  readonly registered: {
    readonly firstStep: RegisteredStep;
    readonly steps: readonly RegisteredStep[];
  };
  readonly applied: readonly { readonly spendingScriptHash: string }[];
  readonly category: Pick<
    FraudProofCatalogueCategoryDeploymentInfo,
    "scriptHash"
  >;
}): void => {
  expect(applied.map((step) => step.spendingScriptHash)).toStrictEqual(
    registered.steps.map((step) => step.spendingScriptHash),
  );
  expect(registered.firstStep.spendingScriptHash).toBe(
    registered.steps[0]?.spendingScriptHash,
  );
  expect(category.scriptHash).toBe(registered.firstStep.spendingScriptHash);
};

/** Fixed shared policies for the blueprint-only parity checks. */
export const REGISTERED_CHAIN_FIXTURE_POLICIES = Object.freeze({
  hubOraclePolicyId: "11".repeat(28),
  fraudProofCataloguePolicyId: "22".repeat(28),
});

/**
 * Builds a family's registered chain from the real blueprint with fixed
 * hub-oracle and catalogue policies, and returns the exact shared values the
 * family-side application must be handed to reproduce it. Unit suites use it
 * to pin family application against the SDK chain without an emulator.
 */
export const buildRegisteredChainFixture = async <
  Contracts extends {
    readonly computationThread: { readonly policyId: string };
    readonly fraudProof: {
      readonly policyId: string;
      readonly spendingScriptAddress: string;
    };
    readonly fieldPreimageCertificate: { readonly policyId: string };
  },
>(
  build: (
    params: BuildFaultProofContractsParams,
  ) => Effect.Effect<Contracts, Error>,
): Promise<{
  readonly blueprint: Blueprint;
  readonly contracts: Contracts;
  readonly applyParams: {
    readonly blueprint: Blueprint;
    readonly network: "Custom";
    readonly computationThreadPolicyId: string;
    readonly fraudProofPolicyId: string;
    readonly fraudProofTokenAddressData: Data;
    readonly fieldPreimageCertificatePolicyId: string;
    readonly hubOracleScriptHash: string;
  };
}> => {
  const blueprint = readBlueprint(realBlueprintPath);
  const contracts = await Effect.runPromise(
    build({
      blueprint: parseFaultProofBlueprint(blueprint),
      network: "Custom",
      ...REGISTERED_CHAIN_FIXTURE_POLICIES,
    }),
  );
  const fraudProofTokenAddressData = await Effect.runPromise(
    addressDataFromBech32(contracts.fraudProof.spendingScriptAddress).pipe(
      Effect.map((address) => Data.from(Data.to(address, AddressData))),
    ),
  );
  return {
    blueprint,
    contracts,
    applyParams: {
      blueprint,
      network: "Custom",
      computationThreadPolicyId: contracts.computationThread.policyId,
      fraudProofPolicyId: contracts.fraudProof.policyId,
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId:
        contracts.fieldPreimageCertificate.policyId,
      // The hub oracle is one dual-purpose script: its minting policy id is
      // its spending script hash, which is the `hub_oracle: ScriptHash`
      // parameter every step-01 validator takes.
      hubOracleScriptHash: REGISTERED_CHAIN_FIXTURE_POLICIES.hubOraclePolicyId,
    },
  };
};
