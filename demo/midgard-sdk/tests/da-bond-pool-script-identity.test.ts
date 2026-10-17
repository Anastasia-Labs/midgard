import { readFileSync } from "node:fs";

import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  applyBlueprintParams,
  availabilityResponseGeometry,
  buildAvailabilityChallengeValidator,
  buildDaAttestationValidator,
  buildDaBondPoolValidator,
  buildDaParamsGovernorValidator,
  DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  DA_BOND_POOL_SCRIPT_TITLES,
  daAvailabilityParameters,
  encodeDaAvailabilityParameters,
  makeMintingPolicy,
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
const POOL_INIT_REF = { txHash: "22".repeat(32), outputIndex: 0 };
const GOVERNOR_INIT_REF = { txHash: "33".repeat(32), outputIndex: 1 };

type DaAvailabilityParametersInput = Parameters<
  typeof daAvailabilityParameters
>[0];

// The selected profile's DA amounts (including the pool top-up minimum and
// floor), the measured challenger bond and test fee ceilings.
const parameterInput = (
  overrides: Partial<DaAvailabilityParametersInput> = {},
): DaAvailabilityParametersInput => ({
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
  ...overrides,
});

const PARAMETERS = daAvailabilityParameters(parameterInput());

// The real DA params contract: its policy is the pool's `da_params_policy_id`.
const DA_PARAMS_POLICY_ID = buildDaParamsGovernorValidator(
  blueprint,
  NETWORK,
  GOVERNOR_INIT_REF,
  256,
  16,
).policyId;

const pool = (
  overrides: Partial<{
    initOutRef: typeof POOL_INIT_REF;
    hubOraclePolicyId: string;
    daParamsPolicyId: string;
    parameters: typeof PARAMETERS;
  }> = {},
) =>
  buildDaBondPoolValidator(
    blueprint,
    NETWORK,
    overrides.initOutRef ?? POOL_INIT_REF,
    overrides.hubOraclePolicyId ?? HUB_ORACLE_POLICY_ID,
    overrides.daParamsPolicyId ?? DA_PARAMS_POLICY_ID,
    overrides.parameters ?? PARAMETERS,
  );

const REFERENCE_SCRIPT_AUTH_POLICY_ID = "44".repeat(28);

const availabilityChallenge = (daBondPoolPolicyId: string) =>
  buildAvailabilityChallengeValidator(
    blueprint,
    NETWORK,
    HUB_ORACLE_POLICY_ID,
    REFERENCE_SCRIPT_AUTH_POLICY_ID,
    daBondPoolPolicyId,
    PARAMETERS,
  );

const daAttestation = (
  contracts: {
    readonly hubOraclePolicyId?: string;
    readonly daBondPoolPolicyId?: string;
  } = {},
) =>
  buildDaAttestationValidator(
    blueprint,
    NETWORK,
    {
      daParamsGovernor: { policyId: DA_PARAMS_POLICY_ID },
      hubOracle: {
        policyId: contracts.hubOraclePolicyId ?? HUB_ORACLE_POLICY_ID,
      },
      daBondPool: { policyId: contracts.daBondPoolPolicyId ?? pool().policyId },
    },
    REFERENCE_SCRIPT_AUTH_POLICY_ID,
    PARAMETERS,
  );

describe("DA bond pool script identity", () => {
  // This pin is the evidence that the pool's hash does not depend on the
  // availability-challenge or DA-attestation contracts: the pool has no slot
  // through which either policy id could reach it. Varying those contracts
  // cannot be observed in the pool's hash, so no build-and-compare test exists.
  it("takes exactly init_ref, the hub policy, the DA params policy and ParametersV1", () => {
    for (const title of Object.values(DA_BOND_POOL_SCRIPT_TITLES)) {
      const validator = blueprint.validators.find(
        (candidate) => candidate.title === title,
      );
      expect(validator, title).toBeDefined();
      const parameterTitles = validator!.parameters.map(
        (parameter) => parameter.title,
      );
      expect(parameterTitles).toEqual([
        "init_ref",
        "hub_oracle_policy_id",
        "da_params_policy_id",
        "parameters",
      ]);
      for (const parameterTitle of parameterTitles) {
        expect(parameterTitle).not.toMatch(/availab|attest|challenge/u);
      }
    }
  });

  it("applies each parameter to the slot the blueprint declares for it", () => {
    // The order comes from the blueprint, not from the builder, so a builder
    // that swaps or reuses a slot is caught here.
    const valueByTitle: Readonly<Record<string, Data>> = {
      init_ref: new Constr(0, [
        POOL_INIT_REF.txHash,
        BigInt(POOL_INIT_REF.outputIndex),
      ]),
      hub_oracle_policy_id: HUB_ORACLE_POLICY_ID,
      da_params_policy_id: DA_PARAMS_POLICY_ID,
      parameters: Data.from(encodeDaAvailabilityParameters(PARAMETERS)),
    };
    const mint = blueprint.validators.find(
      (candidate) => candidate.title === DA_BOND_POOL_SCRIPT_TITLES.mint,
    )!;
    const expected = makeMintingPolicy(
      applyBlueprintParams(
        blueprint,
        mint.title,
        mint.parameters.map((parameter) => valueByTitle[parameter.title]!),
      ),
    );
    expect(pool().policyId).toBe(expected.policyId);
  });

  it("is one script: the spend hash is the mint policy id", () => {
    const built = pool();
    expect(built.spendingScriptHash).toBe(built.policyId);
  });

  it.each([
    [
      "init_ref",
      { initOutRef: { txHash: POOL_INIT_REF.txHash, outputIndex: 1 } },
    ],
    ["hub_oracle_policy_id", { hubOraclePolicyId: "66".repeat(28) }],
    ["da_params_policy_id", { daParamsPolicyId: "77".repeat(28) }],
    [
      "parameters.max_open_fee_lovelace",
      {
        parameters: daAvailabilityParameters(
          parameterInput({ maxOpenFeeLovelace: 500_001n }),
        ),
      },
    ],
    [
      "parameters.challenger_bond_lovelace",
      {
        parameters: daAvailabilityParameters(
          parameterInput({
            challengerBondLovelace:
              DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE +
              1n,
          }),
        ),
      },
    ],
  ] as const)("changes its policy id when %s changes", (_field, override) => {
    expect(pool(override).policyId).not.toBe(pool().policyId);
  });

  it("binds only the timeout yield to the pool policy id", () => {
    const first = availabilityChallenge(pool().policyId);
    const second = availabilityChallenge("88".repeat(28));

    expect(second.yields.timeout.withdrawalScriptHash).not.toBe(
      first.yields.timeout.withdrawalScriptHash,
    );
    // The dispatcher and the other yields take no pool parameter (C8).
    expect(second.policyId).toBe(first.policyId);
    expect(second.spendingScriptHash).toBe(first.spendingScriptHash);
    for (const arm of ["open", "settle", "close"] as const) {
      expect(second.yields[arm].withdrawalScriptHash, arm).toBe(
        first.yields[arm].withdrawalScriptHash,
      );
    }
  });

  it.each([
    ["hub_oracle_policy_id", { hubOraclePolicyId: "99".repeat(28) }],
    ["da_bond_pool_policy_id", { daBondPoolPolicyId: "aa".repeat(28) }],
  ] as const)(
    "changes the DA attestation policy id when %s changes",
    (_field, override) => {
      const moved = daAttestation(override);
      const base = daAttestation();
      expect(moved.policyId).not.toBe(base.policyId);
      expect(moved.spendingScriptHash).not.toBe(base.spendingScriptHash);
    },
  );
});
