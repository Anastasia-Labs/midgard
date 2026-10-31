import { readFileSync } from "node:fs";

import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  applyBlueprintParams,
  AVAILABILITY_CHALLENGE_SCRIPT_TITLES,
  availabilityResponseGeometry,
  buildAvailabilityChallengeValidator,
  buildDaAttestationValidator,
  buildDaParamsGovernorValidator,
  DA_ATTESTATION_SCRIPT_TITLES,
  DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  daAvailabilityParameters,
  encodeDaAvailabilityParameters,
  makeMintingPolicy,
  makeSpendingValidator,
  makeWithdrawalValidator,
  parseFaultProofBlueprint,
} from "../src/index.js";

const blueprint = parseFaultProofBlueprint(
  JSON.parse(
    readFileSync(
      new URL("../../../onchain/aiken/plutus.json", import.meta.url),
      "utf8",
    ),
  ) as unknown,
);

const NETWORK = "Preprod";
const HUB_ORACLE_POLICY_ID = "11".repeat(28);
const REFERENCE_SCRIPT_AUTH_POLICY_ID = "22".repeat(28);
const DA_PARAMS_POLICY_ID = "33".repeat(28);
const DA_BOND_POOL_POLICY_ID = "44".repeat(28);

const PARAMETERS = daAvailabilityParameters({
  responseGeometry: availabilityResponseGeometry(
    DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  ),
  ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  challengerBondLovelace:
    DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  maxOpenFeeLovelace: 500_000n,
  maxPublicationFeeLovelace: 500_000n,
  maxSettlementFeeLovelace: 500_000n,
  maxCloseFeeLovelace: 1_000_000n,
  maxTimeoutFeeLovelace: 1_200_000n,
});
const ENCODED_PARAMETERS = Data.from(
  encodeDaAvailabilityParameters(PARAMETERS),
);

const blueprintParameterTitles = (title: string): readonly string[] => {
  const validator = blueprint.validators.find(
    (candidate) => candidate.title === title,
  );
  expect(validator, title).toBeDefined();
  return validator!.parameters.map((parameter) => parameter.title);
};

// Applies the values in the order the BLUEPRINT declares its slots, so a
// builder that swaps, drops or reuses a slot produces a different script.
const applyByTitle = (
  title: string,
  valueByTitle: Readonly<Record<string, Data>>,
): string =>
  applyBlueprintParams(
    blueprint,
    title,
    blueprintParameterTitles(title).map((parameterTitle) => {
      const value = valueByTitle[parameterTitle];
      expect(value, `${title}: ${parameterTitle}`).toBeDefined();
      return value!;
    }),
  );

const attestation = () =>
  buildDaAttestationValidator(
    blueprint,
    NETWORK,
    {
      daParamsGovernor: { policyId: DA_PARAMS_POLICY_ID },
      hubOracle: { policyId: HUB_ORACLE_POLICY_ID },
      daBondPool: { policyId: DA_BOND_POOL_POLICY_ID },
    },
    REFERENCE_SCRIPT_AUTH_POLICY_ID,
    PARAMETERS,
  );

const availabilityChallenge = () =>
  buildAvailabilityChallengeValidator(
    blueprint,
    NETWORK,
    HUB_ORACLE_POLICY_ID,
    REFERENCE_SCRIPT_AUTH_POLICY_ID,
    DA_BOND_POOL_POLICY_ID,
    PARAMETERS,
  );

describe("public protocol deployment recipes", () => {
  it.each([
    { txHash: "ab", outputIndex: 0 },
    { txHash: "ab".repeat(32), outputIndex: -1 },
  ])("refuses malformed governor initialization references: %j", (outRef) => {
    expect(() =>
      buildDaParamsGovernorValidator(blueprint, "Preprod", outRef, 256, 16),
    ).toThrow();
  });

  it("builds the DA attestation with the hub and pool policies in the blueprint's slots", () => {
    for (const title of Object.values(DA_ATTESTATION_SCRIPT_TITLES)) {
      expect(blueprintParameterTitles(title)).toEqual([
        "da_params_policy_id",
        "reference_script_auth_policy_id",
        "hub_oracle_policy_id",
        "da_bond_pool_policy_id",
        "availability_parameters",
      ]);
    }
    const valueByTitle: Readonly<Record<string, Data>> = {
      da_params_policy_id: DA_PARAMS_POLICY_ID,
      reference_script_auth_policy_id: REFERENCE_SCRIPT_AUTH_POLICY_ID,
      hub_oracle_policy_id: HUB_ORACLE_POLICY_ID,
      da_bond_pool_policy_id: DA_BOND_POOL_POLICY_ID,
      availability_parameters: ENCODED_PARAMETERS,
    };
    const built = attestation();
    expect(built.policyId).toBe(
      makeMintingPolicy(
        applyByTitle(DA_ATTESTATION_SCRIPT_TITLES.mint, valueByTitle),
      ).policyId,
    );
    expect(built.spendingScriptHash).toBe(
      makeSpendingValidator(
        NETWORK,
        applyByTitle(DA_ATTESTATION_SCRIPT_TITLES.spend, valueByTitle),
      ).spendingScriptHash,
    );
  });

  it("builds exactly the open, settle, close and timeout yields, each in its blueprint slots", () => {
    const built = availabilityChallenge();
    expect(Object.keys(built.yields).sort()).toEqual([
      "close",
      "open",
      "settle",
      "timeout",
    ]);
    expect(Object.keys(AVAILABILITY_CHALLENGE_SCRIPT_TITLES).sort()).toEqual([
      "closeYield",
      "mint",
      "openYield",
      "settleYield",
      "spend",
      "timeoutYield",
    ]);

    const valueByTitle: Readonly<Record<string, Data>> = {
      hub_oracle_policy_id: HUB_ORACLE_POLICY_ID,
      reference_script_auth_policy_id: REFERENCE_SCRIPT_AUTH_POLICY_ID,
      availability_policy_id: built.policyId,
      da_bond_pool_policy_id: DA_BOND_POOL_POLICY_ID,
      parameters: ENCODED_PARAMETERS,
    };
    expect(built.policyId).toBe(
      makeMintingPolicy(
        applyByTitle(AVAILABILITY_CHALLENGE_SCRIPT_TITLES.mint, valueByTitle),
      ).policyId,
    );
    expect(built.spendingScriptHash).toBe(
      makeSpendingValidator(
        NETWORK,
        applyByTitle(AVAILABILITY_CHALLENGE_SCRIPT_TITLES.spend, valueByTitle),
      ).spendingScriptHash,
    );
    for (const [arm, title] of [
      ["open", AVAILABILITY_CHALLENGE_SCRIPT_TITLES.openYield],
      ["settle", AVAILABILITY_CHALLENGE_SCRIPT_TITLES.settleYield],
      ["close", AVAILABILITY_CHALLENGE_SCRIPT_TITLES.closeYield],
      ["timeout", AVAILABILITY_CHALLENGE_SCRIPT_TITLES.timeoutYield],
    ] as const) {
      expect(built.yields[arm].withdrawalScriptHash, arm).toBe(
        makeWithdrawalValidator(applyByTitle(title, valueByTitle))
          .withdrawalScriptHash,
      );
    }
    // Only the timeout yield slashes the pool, so only it takes the pool
    // policy (DECISIONS C8).
    expect(
      blueprintParameterTitles(
        AVAILABILITY_CHALLENGE_SCRIPT_TITLES.timeoutYield,
      ),
    ).toContain("da_bond_pool_policy_id");
    for (const title of [
      AVAILABILITY_CHALLENGE_SCRIPT_TITLES.mint,
      AVAILABILITY_CHALLENGE_SCRIPT_TITLES.spend,
      AVAILABILITY_CHALLENGE_SCRIPT_TITLES.openYield,
      AVAILABILITY_CHALLENGE_SCRIPT_TITLES.settleYield,
      AVAILABILITY_CHALLENGE_SCRIPT_TITLES.closeYield,
    ]) {
      expect(blueprintParameterTitles(title), title).not.toContain(
        "da_bond_pool_policy_id",
      );
    }
  });

  it("finds no per-block bond yield in the blueprint", () => {
    expect(
      blueprint.validators.filter((validator) =>
        validator.title.startsWith("availability_challenge_yields.bond."),
      ),
    ).toEqual([]);
  });
});
