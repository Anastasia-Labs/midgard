import { createHash } from "node:crypto";

import {
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  canonicalFraudProofCatalogueFixture,
  positionalContractScriptCbor,
} from "../canonical-fraud-proof-catalogue.js";

export const h28 = (byte: string): string => byte.repeat(28);

export const h32 = (byte: string): string => byte.repeat(32);

export const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

export const asWireValue = <T>(value: T): T =>
  JSON.parse(JSON.stringify(value)) as T;

export const NATIVE_SCRIPT_CBOR = "820501";

export const NATIVE_SCRIPT_HASH = validatorToScriptHash({
  type: "Native",
  script: NATIVE_SCRIPT_CBOR,
});

export const DA_SIGNERS_HASH =
  "0395256ce5d90f07504b614b9e70e29a06fdd69cef6b01f6018615164125a5c5";

/** The default release/rule-bundle bytes. Suites that need their own pass
 * them through the options below. */
export const WATCHER_AUTHORITY_BLUEPRINT_HASH = h32("55");

export const WATCHER_AUTHORITY_RULE_BUNDLE_COMMITMENT = h32("44");

export const WATCHER_AUTHORITY_PROGRAM_COMMITMENTS = {
  "validation-machine-v1": h32("88"),
  "transition-order-v1": h32("99"),
};

export type MutableRecord = Record<string, any>;

export const deepFreezeFixture = <T>(value: T): T => {
  if (value !== null && typeof value === "object" && !Object.isFrozen(value)) {
    Object.freeze(value);
    for (const nested of Object.values(value)) {
      deepFreezeFixture(nested);
    }
  }
  return value;
};

export type AuthorityContractFixture = Readonly<{
  refScriptUTxO: Readonly<{ txHash: string; outputIndex: number }> | null;
  contract: Readonly<{ type: string; cborHex: string }>;
  scriptHash: string;
}> & {
  fraudProofCatalogue?: ReturnType<typeof canonicalFraudProofCatalogueFixture>;
};

/** Explicit unit-fixture parameters; these are not measured deployment defaults. */
export const WATCHER_HISTORY_FIXTURE_BOUNDS = Object.freeze({
  inlineLimitBytes: "1024",
  maxPayloadBytes: "8192",
  maxPayloadNodes: "512",
});

export type WatcherHistoryFixtureRecipe = Readonly<{
  protectionDurationMs: string;
  bounds: Readonly<{
    inlineLimitBytes: string;
    maxPayloadBytes: string;
    maxPayloadNodes: string;
  }>;
}>;

export const WATCHER_EMULATOR_HISTORY_RECIPE: WatcherHistoryFixtureRecipe =
  Object.freeze({
    protectionDurationMs: "2000",
    bounds: Object.freeze({
      inlineLimitBytes: "512",
      maxPayloadBytes: "5000",
      maxPayloadNodes: "512",
    }),
  });

export const addWatcherHistoryFixtureMetadata = (
  contracts: Record<string, AuthorityContractFixture>,
  initializationNonce: { txHash: string; outputIndex: number },
  recipe?: WatcherHistoryFixtureRecipe,
): void => {
  const bounds = recipe?.bounds ?? WATCHER_HISTORY_FIXTURE_BOUNDS;
  for (const [name, kind] of [
    ["deposit", "Deposit"],
    ["withdrawal", "Withdrawal"],
  ] as const) {
    contracts[name + "Mint"] = Object.assign({}, contracts[name + "Mint"], {
      eventHistoryRecipe: {
        kind,
        hubPolicyId: contracts.hubOracleMint!.scriptHash,
        initializationNonce,
        protectionDurationMs: recipe?.protectionDurationMs ?? "60000",
        bounds,
      },
    });
    const address = validatorToAddress("Preprod", {
      type: "PlutusV3",
      script: contracts[name + "HistoryRetentionSpend"]!.contract.cborHex,
    });
    const family =
      name === "deposit"
        ? "fraudProofFabricatedDeposit"
        : "fraudProofFabricatedWithdrawal";
    contracts[family] = Object.assign({}, contracts[family], {
      eventHistoryBounds: bounds,
      eventHistoryRetentionAddress: address,
    });
  }
  contracts.fraudProofTransitionTrace = Object.assign(
    {},
    contracts.fraudProofTransitionTrace,
    {
      eventHistoryBounds: bounds,
      eventHistoryRetentionAddresses: {
        deposit: validatorToAddress("Preprod", {
          type: "PlutusV3",
          script: contracts.depositHistoryRetentionSpend!.contract.cborHex,
        }),
        withdrawal: validatorToAddress("Preprod", {
          type: "PlutusV3",
          script: contracts.withdrawalHistoryRetentionSpend!.contract.cborHex,
        }),
      },
    },
  );
};

export type AuthorityReferenceScriptFixture = Readonly<{
  status: string;
  roleUnit: string;
  scriptHash: string;
  outRef: string;
}>;

export type WatcherAuthorityContractSet = Readonly<{
  contracts: Record<string, AuthorityContractFixture>;
  fraudProofCatalogue: ReturnType<typeof canonicalFraudProofCatalogueFixture>;
  referenceScripts: Record<string, AuthorityReferenceScriptFixture>;
}>;

/**
 * The applied contracts, catalogue and reference scripts, built without
 * committing to a release identity yet.
 *
 * Split out so a suite can inspect the contracts (for example, to derive a
 * program commitment from the catalogue's category script hashes) before it
 * says what the manifest commits to.
 */
const buildWatcherAuthorityContracts = (): WatcherAuthorityContractSet => {
  const referenceOutRefByContract = new Map<
    string,
    { txHash: string; outputIndex: number }
  >(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contractName, outputIndex) => [
        contractName,
        { txHash: h32("12"), outputIndex },
      ],
    ),
  );
  const contracts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((contractName) => {
      const native = contractName === "referenceScriptAuthMint";
      const script = native
        ? NATIVE_SCRIPT_CBOR
        : positionalContractScriptCbor(
            contractName === "depositSpend"
              ? "depositMint"
              : contractName === "withdrawalSpend"
                ? "withdrawalMint"
                : contractName,
          );
      return [
        contractName,
        {
          refScriptUTxO: referenceOutRefByContract.get(contractName) ?? null,
          contract: { type: native ? "Native" : "PlutusV3", cborHex: script },
          scriptHash: native
            ? NATIVE_SCRIPT_HASH
            : validatorToScriptHash({ type: "PlutusV3", script }),
        },
      ];
    }),
  ) as Record<string, AuthorityContractFixture>;
  const fraudProofCatalogue = canonicalFraudProofCatalogueFixture(contracts);
  const catalogueContract = contracts.fraudProofCatalogueMint;
  if (catalogueContract === undefined) {
    throw new Error("authority catalogue contract is missing");
  }
  catalogueContract.fraudProofCatalogue = fraudProofCatalogue;
  const referenceScripts = Object.fromEntries(
    Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      ([role, contractName]) => {
        const outRef = referenceOutRefByContract.get(contractName)!;
        const tokenName =
          DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
            role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
          ];
        return [
          role,
          {
            status: "confirmed",
            roleUnit:
              NATIVE_SCRIPT_HASH +
              Buffer.from(tokenName, "utf8").toString("hex"),
            scriptHash: contracts[contractName].scriptHash,
            outRef: `${outRef.txHash}#${outRef.outputIndex.toString()}`,
          },
        ];
      },
    ),
  ) as Record<string, AuthorityReferenceScriptFixture>;
  return { contracts, fraudProofCatalogue, referenceScripts };
};

let cachedWatcherAuthorityContracts: WatcherAuthorityContractSet | null = null;

export const makeWatcherAuthorityContracts =
  (): WatcherAuthorityContractSet => {
    cachedWatcherAuthorityContracts ??= deepFreezeFixture(
      buildWatcherAuthorityContracts(),
    );
    return structuredClone(cachedWatcherAuthorityContracts);
  };

export type WatcherDeploymentAuthorityFixtureOptions = Readonly<{
  network?: "Preprod" | "Custom";
  /** Reuse an already-built contract set, so the caller can derive program
   * commitments from the catalogue it is about to commit to. */
  contractSet?: WatcherAuthorityContractSet;
  fundingProfileBundleDigest?: string;
  blueprintHash?: string;
  /** Bind an existing ordinary initialization frame to its actual nonce. */
  hubOracleOneShotOutRef?: string;
  eventHistoryRecipe?: WatcherHistoryFixtureRecipe;
  ruleBundleCommitment?: string;
  programCommitments?: Readonly<Record<string, string>>;
}>;

export const WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS = Object.freeze({
  minFeeA: "44",
  minFeeB: "155381",
  priceMemory: Object.freeze({ numerator: "577", denominator: "10000" }),
  priceSteps: Object.freeze({ numerator: "721", denominator: "10000000" }),
  coinsPerUtxoByte: "4310",
  collateralPercentage: "150",
  maxCollateralInputs: "3",
  maxTxSize: "16384",
  maxValueSize: "5000",
  maxTxExUnits: Object.freeze({
    memory: "16500000",
    steps: "10000000000",
  }),
  referenceScriptFee: Object.freeze({
    base: Object.freeze({ numerator: "15", denominator: "1" }),
    range: "25600",
    multiplier: Object.freeze({ numerator: "6", denominator: "5" }),
    maximumSizeBytes: "204800",
  }),
});
