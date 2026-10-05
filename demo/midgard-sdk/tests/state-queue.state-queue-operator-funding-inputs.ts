import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  applyDoubleCborEncoding,
  type BuildTxWithRedeemer,
  type Data as LucidData,
  type MintingPolicy,
  mintingPolicyToId,
  type Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  type SpendingValidator,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  type AuthenticatedValidator,
  requireOperatorWalletInputs,
  resolveFraudProverRewardOutputIndex,
  type SpendingValidator as SdkSpendingValidator,
} from "../src/index.js";
import { applyExpectedScriptParams } from "./fault-proof.expected-parameter-application.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));

const repoRoot = resolve(moduleDir, "../../..");

export const realBlueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(repoRoot, "onchain/aiken/plutus.json");

export const alwaysSucceedsBlueprintPath = resolve(
  repoRoot,
  "demo/midgard-node/blueprints/always-succeeds/plutus.json",
);

export const network: Network = "Preprod";

export const outputReference = {
  transactionId: "44".repeat(32),
  outputIndex: 0n,
};

describe("fraud-prover reward builder exactness", () => {
  const proverAddress = "addr_test1vq53_prover";
  const reward = {
    proverEnterpriseAddress: proverAddress,
    lovelace: 400_000_000n,
  } as const;
  const output = (overrides: Record<string, unknown> = {}) => ({
    address: proverAddress,
    assets: { lovelace: reward.lovelace },
    datum: undefined,
    datumHash: undefined,
    scriptRef: undefined,
    ...overrides,
  });
  const context = (outputs: readonly unknown[]) =>
    ({ outputs }) as unknown as Parameters<BuildTxWithRedeemer>[0];

  it("accepts exactly one ADA-only NoDatum/no-reference-script prover output", () => {
    expect(
      resolveFraudProverRewardOutputIndex(
        context([output({ address: "addr_test1vother" }), output()]),
        reward,
        "Q53 reward",
      ),
    ).toBe(1n);
  });

  it.each([
    ["underpayment", output({ assets: { lovelace: reward.lovelace - 1n } })],
    ["overpayment", output({ assets: { lovelace: reward.lovelace + 1n } })],
    [
      "token",
      output({ assets: { lovelace: reward.lovelace, ["ab".repeat(28)]: 1n } }),
    ],
    ["inline datum", output({ datum: "d87980" })],
    ["datum hash", output({ datumHash: "ab".repeat(32) })],
    [
      "reference script",
      output({ scriptRef: { type: "PlutusV3", script: "00" } }),
    ],
  ])("rejects %s mutation", (_label, mutated) => {
    expect(() =>
      resolveFraudProverRewardOutputIndex(
        context([mutated]),
        reward,
        "Q53 reward",
      ),
    ).toThrow();
  });

  it("rejects any second output to the prover credential", () => {
    expect(() =>
      resolveFraudProverRewardOutputIndex(
        context([output(), output({ assets: { lovelace: 2_000_000n } })]),
        reward,
        "Q53 reward",
      ),
    ).toThrow(/exactly one output/);
  });
});

export const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

describe("state-queue operator funding inputs", () => {
  it("retains token-bearing preset wallet inputs", async () => {
    const tokenBearing = {
      txHash: "aa".repeat(32),
      outputIndex: 0,
      address: "addr_test1vr0dummy",
      assets: {
        lovelace: 10_000_000n,
        [`${"bb".repeat(28)}01`]: 1n,
      },
      datum: undefined,
      datumHash: undefined,
      scriptRef: undefined,
    } as UTxO;
    const pureAda = {
      ...tokenBearing,
      txHash: "cc".repeat(32),
      outputIndex: 1,
      assets: { lovelace: 5_000_000n },
    } as UTxO;

    await expect(
      Effect.runPromise(
        requireOperatorWalletInputs(
          [tokenBearing, pureAda],
          "state_queue commit tx",
        ),
      ),
    ).resolves.toEqual([tokenBearing, pureAda]);
  });

  it("accepts token-only operator wallet views for preset funding", async () => {
    const tokenBearing = {
      txHash: "dd".repeat(32),
      outputIndex: 0,
      address: "addr_test1vr0dummy",
      assets: {
        lovelace: 10_000_000n,
        [`${"ee".repeat(28)}01`]: 1n,
      },
      datum: undefined,
      datumHash: undefined,
      scriptRef: undefined,
    } as UTxO;

    await expect(
      Effect.runPromise(
        requireOperatorWalletInputs([tokenBearing], "state_queue commit tx"),
      ),
    ).resolves.toEqual([tokenBearing]);
  });

  it("rejects empty operator wallet views for preset funding", async () => {
    const result = await Effect.runPromise(
      Effect.either(requireOperatorWalletInputs([], "state_queue commit tx")),
    );

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toContain("operator wallet inputs");
    }
  });

  it("does not impose datum or script-ref filters on preset wallet inputs", async () => {
    const withDatum = {
      txHash: "12".repeat(32),
      outputIndex: 0,
      address: "addr_test1vr0dummy",
      assets: { lovelace: 10_000_000n },
      datum: "d87980",
      datumHash: undefined,
      scriptRef: undefined,
    } as UTxO;
    const withScriptRef = {
      ...withDatum,
      txHash: "13".repeat(32),
      datum: undefined,
      scriptRef: { type: "Native", script: "8200" },
    } as UTxO;

    await expect(
      Effect.runPromise(
        requireOperatorWalletInputs(
          [withDatum, withScriptRef],
          "state_queue commit tx",
        ),
      ),
    ).resolves.toEqual([withDatum, withScriptRef]);
  });
});

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
  readonly parameters?: readonly unknown[];
};

export type Blueprint = {
  readonly validators: readonly BlueprintValidator[];
};

export type StateQueueTestContracts = {
  readonly hubOracle: AuthenticatedValidator;
  readonly correctionLock: SdkSpendingValidator;
  readonly computationThread: AuthenticatedValidator;
  readonly daAttestation: AuthenticatedValidator;
  readonly stateQueue: AuthenticatedValidator;
  readonly commitYield: SdkSpendingValidator;
  readonly fraudRemovalYield: SdkSpendingValidator;
  readonly scheduler: AuthenticatedValidator;
  readonly activeOperators: AuthenticatedValidator;
  readonly retiredOperators: AuthenticatedValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly settlement: AuthenticatedValidator;
};

export const readBlueprint = (path: string): Blueprint =>
  JSON.parse(readFileSync(path, "utf8")) as Blueprint;

const getCompiledScript = (blueprint: Blueprint, title: string): string => {
  const found = blueprint.validators.find(
    (validator) => validator.title === title,
  );
  if (found === undefined) {
    throw new Error(`Validator with title "${title}" not found`);
  }
  return found.compiledCode;
};

export const applyAllBlueprintParamsToScript = (
  blueprint: Blueprint,
  title: string,
  params: LucidData[],
): string => {
  const validator = blueprint.validators.find(
    (candidate) => candidate.title === title,
  );
  if (validator === undefined) {
    throw new Error(`Validator with title "${title}" not found`);
  }
  if (validator.parameters === undefined) {
    throw new Error(`Validator "${title}" does not declare parameters`);
  }
  if (params.length !== validator.parameters.length) {
    throw new Error(
      `Validator "${title}" requires exactly ${validator.parameters.length.toString()} parameters, received ${params.length.toString()}`,
    );
  }
  return applyExpectedScriptParams(validator.compiledCode, params);
};

export const makeMintingValidator = (mintingScriptCBOR: string) => {
  const mintingScript: MintingPolicy = {
    type: "PlutusV3",
    script: mintingScriptCBOR,
  };
  return {
    mintingScriptCBOR,
    mintingScript,
    policyId: mintingPolicyToId(mintingScript),
  };
};

export const makeSpendingValidator = (
  spendingScriptCBOR: string,
): SdkSpendingValidator => {
  const spendingScript: SpendingValidator = {
    type: "PlutusV3",
    script: spendingScriptCBOR,
  };
  return {
    spendingScriptCBOR,
    spendingScript,
    spendingScriptAddress: validatorToAddress(network, spendingScript),
    spendingScriptHash: validatorToScriptHash(spendingScript),
  };
};

export const makeAuthenticatedValidator = (
  mintingScriptCBOR: string,
  spendingScriptCBOR: string,
): AuthenticatedValidator => ({
  ...makeMintingValidator(mintingScriptCBOR),
  ...makeSpendingValidator(spendingScriptCBOR),
});

const alwaysTitle = (baseName: string, purpose: "spend" | "mint"): string =>
  `midgard.${baseName}_${purpose}.else`;

export const alwaysScript = (
  blueprint: Blueprint,
  baseName: string,
  purpose: "spend" | "mint",
): string =>
  applyDoubleCborEncoding(
    getCompiledScript(blueprint, alwaysTitle(baseName, purpose)),
  );

export const alwaysAuthenticated = (
  blueprint: Blueprint,
  baseName: string,
): AuthenticatedValidator =>
  makeAuthenticatedValidator(
    alwaysScript(blueprint, baseName, "mint"),
    alwaysScript(blueprint, baseName, "spend"),
  );
