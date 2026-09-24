import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
} from "@al-ft/midgard-core/consensus-profile";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES as SHARED_DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS as SHARED_DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE as SHARED_DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueDeploymentInfo,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  referenceScriptAuthUnit,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  scriptFromNative,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES,
  DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  normalizeDeploymentManifestJsonValue,
  parseDeploymentManifestValue,
} from "../src/deployment-manifest.js";
import { buildFraudProofCatalogueDeploymentInfo } from "../src/transactions/initialization.js";
import { TEST_AVAILABILITY_CHALLENGE } from "./helpers/availability-challenge.js";

const NATIVE_SCRIPT_CBOR = "820501";
const NATIVE_SCRIPT_HASH = validatorToScriptHash({
  type: "Native",
  script: NATIVE_SCRIPT_CBOR,
});
const CONTRACT_SCRIPT_CBOR = "01";
const CONTRACT_SCRIPT_HASH = validatorToScriptHash({
  type: "PlutusV3",
  script: CONTRACT_SCRIPT_CBOR,
});
let CANONICAL_FRAUD_PROOF_CATALOGUE: FraudProofCatalogueDeploymentInfo;
beforeAll(async () => {
  CANONICAL_FRAUD_PROOF_CATALOGUE = await Effect.runPromise(
    buildFraudProofCatalogueDeploymentInfo(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map(
        (categoryName) =>
          [
            Buffer.from(
              FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[categoryName],
              "hex",
            ),
            { spendingScriptHash: CONTRACT_SCRIPT_HASH } as never,
            categoryName,
          ] as const,
      ),
    ),
  );
});
const DA_VKEY = "44".repeat(32);
const CARDANO_PARAMETERS = {
  minFeeA: "44",
  minFeeB: "155381",
  priceMemory: { numerator: "577", denominator: "10000" },
  priceSteps: { numerator: "721", denominator: "10000000" },
  coinsPerUtxoByte: "4310",
  collateralPercentage: "150",
  maxCollateralInputs: "3",
  maxTxSize: "16384",
  maxValueSize: "5000",
  maxTxExUnits: { memory: "16500000", steps: "10000000000" },
  referenceScriptFee: {
    base: { numerator: "15", denominator: "1" },
    range: "25600",
    multiplier: { numerator: "6", denominator: "5" },
    maximumSizeBytes: "204800",
  },
} as const;

const canonicalIdentity = (): Omit<DeploymentManifest, "manifestId"> => {
  const referenceOutRefByContract = new Map<
    string,
    { readonly txHash: string; readonly outputIndex: number }
  >(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contractName, outputIndex) => [
        contractName,
        { txHash: "22".repeat(32), outputIndex },
      ],
    ),
  );
  const contracts: Record<string, DeploymentManifest["contracts"][string]> =
    Object.fromEntries(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((contractName) => [
        contractName,
        {
          refScriptUTxO: referenceOutRefByContract.get(contractName) ?? null,
          ...(contractName === "depositMint" ||
          contractName === "withdrawalMint"
            ? {
                eventHistoryRecipe: {
                  kind:
                    contractName === "depositMint"
                      ? ("Deposit" as const)
                      : ("Withdrawal" as const),
                  hubPolicyId: CONTRACT_SCRIPT_HASH,
                  initializationNonce: {
                    txHash: "11".repeat(32),
                    outputIndex: 0,
                  },
                  protectionDurationMs: "2000",
                  bounds: {
                    inlineLimitBytes: "512",
                    maxPayloadBytes: "5000",
                    maxPayloadNodes: "512",
                  },
                },
              }
            : {}),
          ...(contractName === "fraudProofFabricatedDeposit" ||
          contractName === "fraudProofFabricatedWithdrawal"
            ? {
                eventHistoryBounds: {
                  inlineLimitBytes: "512",
                  maxPayloadBytes: "5000",
                  maxPayloadNodes: "512",
                },
                eventHistoryRetentionAddress: credentialToAddress("Preview", {
                  type: "Script",
                  hash: "ee".repeat(28),
                }),
              }
            : {}),
          ...(contractName === "fraudProofTransitionTrace"
            ? {
                eventHistoryBounds: {
                  inlineLimitBytes: "512",
                  maxPayloadBytes: "5000",
                  maxPayloadNodes: "512",
                },
                eventHistoryRetentionAddresses: {
                  deposit: credentialToAddress("Preview", {
                    type: "Script",
                    hash: "dd".repeat(28),
                  }),
                  withdrawal: credentialToAddress("Preview", {
                    type: "Script",
                    hash: "ee".repeat(28),
                  }),
                },
              }
            : {}),
          contract: {
            type:
              contractName === "referenceScriptAuthMint"
                ? ("Native" as const)
                : ("PlutusV3" as const),
            cborHex:
              contractName === "referenceScriptAuthMint"
                ? NATIVE_SCRIPT_CBOR
                : CONTRACT_SCRIPT_CBOR,
          },
          scriptHash:
            contractName === "referenceScriptAuthMint"
              ? NATIVE_SCRIPT_HASH
              : CONTRACT_SCRIPT_HASH,
        },
      ]),
    );
  contracts.fraudProofCatalogueMint = {
    ...contracts.fraudProofCatalogueMint,
    fraudProofCatalogue: CANONICAL_FRAUD_PROOF_CATALOGUE,
  };
  const referenceScripts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES.map((role) => {
      const contractName =
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
          role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
        ];
      return [
        role,
        {
          status: "confirmed" as const,
          roleUnit: referenceScriptAuthUnit(
            NATIVE_SCRIPT_HASH,
            role as keyof typeof REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
          ),
          scriptHash: contracts[contractName].scriptHash,
          outRef: `${referenceOutRefByContract.get(contractName)!.txHash}#${referenceOutRefByContract.get(contractName)!.outputIndex.toString()}`,
        },
      ];
    }),
  );
  const steps: DeploymentManifest["steps"] = {
    prepareHubOracleNonce: { status: "complete" },
    deployNodeRuntimeReferenceScripts: { status: "complete" },
    initProtocol: { status: "complete" },
    availabilityRegistration: { status: "complete" },
    phasRegistration: { status: "pending" },
    operatorRegistration: { status: "pending" },
    operatorActivation: { status: "pending" },
  };
  return {
    schemaVersion: DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    network: "Preview",
    cardanoProtocolParameters: {
      snapshot: CARDANO_PARAMETERS,
      digest: computeDeploymentManifestJsonDigest(CARDANO_PARAMETERS),
    },
    genesis: {
      headerHash: "00".repeat(28),
      utxoSetDigest: computeDeploymentManifestJsonDigest(
        normalizeDeploymentManifestJsonValue([]),
      ),
    },
    createdAt: "2026-07-24T00:00:00.000Z",
    updatedAt: "2026-07-24T00:00:00.000Z",
    referenceScriptDeployAddress: "addr_test1vcanonical",
    hubOracleOneShot: {
      txHash: "11".repeat(32),
      outputIndex: 0,
      outRef: `${"11".repeat(32)}#0`,
      status: "consumed_by_init",
    },
    referenceScriptAuthPolicy: {
      policyId: NATIVE_SCRIPT_HASH,
      nativeScript: {
        type: "Native",
        cborHex: NATIVE_SCRIPT_CBOR,
        expiresAtSlot: 1,
        expiresAtUnixTime: 1,
        timelockDurationMs: 1,
      },
      tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
      postTimelockAudit: {
        required: true,
        rule: "No authenticated reference-script output may change.",
      },
    },
    contracts,
    referenceScripts,
    da: {
      committeeVkeys: [DA_VKEY],
      committeeSignersHash: computeDeploymentManifestDaCommitteeSignersHash([
        DA_VKEY,
      ]),
      threshold: 1,
      transportProfile: {
        protocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
        runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
        envelopeEncoding: "identity",
        zstdLevel: 3,
        limits: DA_TRANSPORT_LIMITS,
        retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
      },
    },
    artifacts: {
      blueprintHash: "55".repeat(32),
    },
    validationDispute: {
      version: MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
    },
    l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
    economics:
      DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
    availabilityChallenge: TEST_AVAILABILITY_CHALLENGE,
    steps,
  };
};

const withId = (
  identity: Omit<DeploymentManifest, "manifestId">,
): DeploymentManifest => ({
  ...identity,
  manifestId: computeDeploymentManifestId(identity),
});

const canonicalManifest = (): DeploymentManifest => withId(canonicalIdentity());

describe("V1 deployment manifest", () => {
  it("keeps deployment registry mirrors aligned with core", () => {
    expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toEqual(
      SHARED_DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
    );
    expect(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).toEqual(
      SHARED_DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    );
    expect(FRAUD_PROOF_CATALOGUE_CATEGORY_IDS).toEqual(
      SHARED_DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
    );
  });

  it("names the network-id forced (wrongful-rejection) step and scan", () => {
    // The forced door and the resumable output scan it hands off to are
    // applied by `buildNetworkIdChain` but are not members of the chain's
    // `steps`, so each can only be deployed, published and restored through
    // its own canonical name and reference-script role.
    const auxiliaryContracts = [
      [
        "V1 fraud-proof network-id forced step",
        "fraudProofNetworkIdForcedStep",
      ],
      [
        "V1 fraud-proof network-id forced scan",
        "fraudProofNetworkIdForcedScan",
      ],
    ] as const;
    for (const [role, contractName] of auxiliaryContracts) {
      expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toContain(contractName);
      expect(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[role]).toBe(
        contractName,
      );
      expect(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES).toContain(role);
      expect(REFERENCE_SCRIPT_AUTH_TOKEN_NAMES).toHaveProperty(role);
    }
  });

  it("accepts a canonical authenticated V1 manifest", () => {
    expect(parseDeploymentManifestValue(canonicalManifest())).toEqual(
      canonicalManifest(),
    );
  });

  it("accepts publisher-signed authority with a distinct script reference recipient", () => {
    const identity = canonicalIdentity();
    const publisherKeyHash = "ab".repeat(28);
    const policy = scriptFromNative({
      type: "all",
      scripts: [
        { type: "sig", keyHash: publisherKeyHash },
        {
          type: "before",
          slot: identity.referenceScriptAuthPolicy.nativeScript.expiresAtSlot,
        },
      ],
    });
    const policyId = validatorToScriptHash(policy);
    const referenceScriptDeployAddress = credentialToAddress("Preview", {
      type: "Script",
      hash: "cd".repeat(28),
    });
    const manifest = withId({
      ...identity,
      referenceScriptDeployAddress,
      referenceScriptAuthPolicy: {
        ...identity.referenceScriptAuthPolicy,
        policyId,
        nativeScript: {
          ...identity.referenceScriptAuthPolicy.nativeScript,
          cborHex: policy.script,
        },
        postTimelockAudit: {
          required: false,
          rule: "Audit after publisher-authorized publication completes.",
        },
      },
      contracts: {
        ...identity.contracts,
        referenceScriptAuthMint: {
          ...identity.contracts.referenceScriptAuthMint,
          contract: { type: "Native", cborHex: policy.script },
          scriptHash: policyId,
        },
      },
      referenceScripts: Object.fromEntries(
        Object.entries(identity.referenceScripts).map(([role, reference]) => [
          role,
          {
            ...reference,
            scriptHash:
              role === "reference-script-auth minting"
                ? policyId
                : reference.scriptHash,
            roleUnit: referenceScriptAuthUnit(
              policyId,
              role as keyof typeof REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
            ),
          },
        ]),
      ),
    });
    expect(parseDeploymentManifestValue(manifest)).toEqual(manifest);
  });

  it("rejects finalization before availability reward registration completes", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    const incomplete = {
      ...identity,
      steps: {
        ...identity.steps,
        availabilityRegistration: { status: "pending" as const },
      },
    };
    expect(() =>
      parseDeploymentManifestValue({
        ...incomplete,
        manifestId: computeDeploymentManifestId(incomplete),
      }),
    ).toThrow("steps.availabilityRegistration.status must be complete");
  });

  it("rejects catalogue root, explicit ID, and membership-proof tampering", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    const catalogue =
      identity.contracts.fraudProofCatalogueMint.fraudProofCatalogue!;
    const withCatalogue = (
      fraudProofCatalogue: typeof catalogue,
    ): Omit<DeploymentManifest, "manifestId"> => ({
      ...identity,
      contracts: {
        ...identity.contracts,
        fraudProofCatalogueMint: {
          ...identity.contracts.fraudProofCatalogueMint,
          fraudProofCatalogue,
        },
      },
    });

    expect(() =>
      parseDeploymentManifestValue(
        withId(
          withCatalogue({
            ...catalogue,
            root: "33".repeat(32),
          }),
        ),
      ),
    ).toThrow(/catalogue root mismatch/u);

    expect(() =>
      parseDeploymentManifestValue(
        withId(
          withCatalogue({
            ...catalogue,
            categories: {
              ...catalogue.categories,
              nonExistentInputNoIndex: {
                ...catalogue.categories.nonExistentInputNoIndex,
                categoryId: "00000003",
              },
            },
          }),
        ),
      ),
    ).toThrow(/nonExistentInputNoIndex\.categoryId must be 00000002/u);

    expect(() =>
      parseDeploymentManifestValue(
        withId(
          withCatalogue({
            ...catalogue,
            categories: {
              ...catalogue.categories,
              zeroInput: {
                ...catalogue.categories.zeroInput,
                membershipProofCbor: "80",
              },
            },
          }),
        ),
      ),
    ).toThrow(/zeroInput\.membershipProofCbor does not prove membership/u);
  });

  it("rejects missing and unexpected root fields", () => {
    const {
      da: _da,
      manifestId: _manifestId,
      ...missingDa
    } = canonicalManifest();
    expect(() =>
      parseDeploymentManifestValue({
        ...missingDa,
        manifestId: computeDeploymentManifestId(
          missingDa as Omit<DeploymentManifest, "manifestId">,
        ),
      }),
    ).toThrow(/value\.da is required/u);

    expect(() =>
      parseDeploymentManifestValue({
        ...canonicalManifest(),
        historicalSchemaVersion: 9,
      }),
    ).toThrow(/value\.historicalSchemaVersion is unexpected/u);
  });

  it("rejects a manifest missing any compiled contract", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    const { fraudProofZeroInput: _zeroInput, ...withoutZeroInput } =
      identity.contracts;
    const missingContract = {
      ...identity,
      contracts: withoutZeroInput,
    } as Omit<DeploymentManifest, "manifestId">;
    expect(() => parseDeploymentManifestValue(withId(missingContract))).toThrow(
      /contracts\.fraudProofZeroInput is required/u,
    );
  });

  it("rejects a manifest missing a validation-dispute control contract", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    const {
      validationTraceDisputeSource: _validationTraceDisputeSource,
      ...withoutSource
    } = identity.contracts;
    const missingContract = {
      ...identity,
      contracts: withoutSource,
    } as Omit<DeploymentManifest, "manifestId">;
    expect(() => parseDeploymentManifestValue(withId(missingContract))).toThrow(
      /contracts\.validationTraceDisputeSource is required/u,
    );
  });

  it("rejects tampered script bytes even with a recomputed manifest ID", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    const tampered = {
      ...identity,
      contracts: {
        ...identity.contracts,
        txOrderSpend: {
          ...identity.contracts.txOrderSpend,
          contract: {
            ...identity.contracts.txOrderSpend.contract,
            cborHex: "02",
          },
        },
      },
    };
    expect(() =>
      parseDeploymentManifestValue(
        withId(tampered as Omit<DeploymentManifest, "manifestId">),
      ),
    ).toThrow(/contracts\.txOrderSpend\.scriptHash mismatch/u);
  });

  it("rejects tampered Cardano and DA identity fields", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    expect(() =>
      parseDeploymentManifestValue(
        withId({
          ...identity,
          cardanoProtocolParameters: {
            ...identity.cardanoProtocolParameters,
            digest: "66".repeat(32),
          },
        }),
      ),
    ).toThrow(/cardanoProtocolParameters\.digest mismatch/u);

    expect(() =>
      parseDeploymentManifestValue(
        withId({
          ...identity,
          da: {
            ...identity.da,
            threshold: 2,
          },
        }),
      ),
    ).toThrow(/threshold must not exceed committee size/u);
  });

  it("rejects a noncanonical dispute schedule", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    expect(() =>
      parseDeploymentManifestValue(
        withId({
          ...identity,
          validationDispute: {
            ...identity.validationDispute,
            maturityMs: 39_600_000,
          },
        }),
      ),
    ).toThrow(/canonical V1 maturity/u);
  });

  it("rejects noncanonical release finality even with a recomputed identity", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    for (const l1Finality of [
      { ...identity.l1Finality, confirmationDepth: 29 },
      { ...identity.l1Finality, automaticRecoveryMaxDepth: 2159 },
      { ...identity.l1Finality, deepRollbackPolicy: "manual-v1" },
    ]) {
      const tampered = { ...identity, l1Finality };
      expect(() =>
        parseDeploymentManifestValue({
          ...tampered,
          manifestId: computeDeploymentManifestId(
            tampered as Omit<DeploymentManifest, "manifestId">,
          ),
        }),
      ).toThrow(/l1Finality/u);
    }
  });

  it("rejects mutated or cross-profile economics with a recomputed identity", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    for (const economics of [
      {
        ...identity.economics,
        fraudProverRewardLovelace:
          identity.economics.fraudProverRewardLovelace + 1,
      },
      {
        ...identity.economics,
        profile: "public-preprod-launch-v1",
      },
    ]) {
      const tampered = { ...identity, economics };
      expect(() =>
        parseDeploymentManifestValue({
          ...tampered,
          manifestId: computeDeploymentManifestId(
            tampered as Omit<DeploymentManifest, "manifestId">,
          ),
        }),
      ).toThrow(/economics/u);
    }
  });

  it("rejects an unsupported profile and tuple digest", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    expect(() =>
      parseDeploymentManifestValue({
        ...withId({
          ...identity,
          consensusProfile: {
            ...MIDGARD_CONSENSUS_PROFILE,
            profileId: "unsupported-profile-99",
          } as never,
        }),
      }),
    ).toThrow(/consensusProfile must exactly match canonical V1/u);

    expect(() =>
      parseDeploymentManifestValue(
        withId({
          ...identity,
          consensusProfileDigest: "77".repeat(32),
        }),
      ),
    ).toThrow(/consensusProfileDigest must exactly match canonical V1/u);
  });
});
