import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  computeDeploymentManifestV1JsonDigest as computeSharedDeploymentManifestV1JsonDigest,
  normalizeDeploymentManifestV1JsonValue as normalizeSharedDeploymentManifestV1JsonValue,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  referenceScriptAuthUnit,
} from "@al-ft/midgard-sdk";
import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  computeDeploymentManifestId,
  computeDeploymentManifestV1DaCommitteeSignersHash,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_ROLES,
  DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
  DEPLOYMENT_MANIFEST_V1_STEP_NAMES,
  type DeploymentManifestV1Value,
  normalizeDeploymentManifestV1JsonValue,
  parseDeploymentManifestV1Value,
} from "@/deployment-manifest-v1.js";

const NATIVE_SCRIPT_CBOR = `8200581c${"00".repeat(28)}`;
const NATIVE_SCRIPT_HASH = validatorToScriptHash({
  type: "Native",
  script: NATIVE_SCRIPT_CBOR,
});
const CONTRACT_SCRIPT_CBOR = "01";
const CONTRACT_SCRIPT_HASH = validatorToScriptHash({
  type: "PlutusV3",
  script: CONTRACT_SCRIPT_CBOR,
});
const DA_VKEY = "44".repeat(32);
const CARDANO_PARAMETERS = normalizeDeploymentManifestV1JsonValue({
  maxTxSize: 16_384,
  maxValueSize: 5_000,
  maxTxExUnits: { memory: "16500000", steps: "10000000000" },
});

type FraudProofCatalogueFixture = NonNullable<
  DeploymentManifestV1Value["contracts"][string]["fraudProofCatalogue"]
>;

const buildFraudProofCatalogueFixture =
  async (): Promise<FraudProofCatalogueFixture> => {
    const categoryIdSchema = Data.Bytes({
      minLength: 4,
      maxLength: 4,
    });
    const scriptHashSchema = Data.Bytes({
      minLength: 28,
      maxLength: 28,
    });
    const categories = Object.fromEntries(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((categoryName, index) => [
        categoryName,
        {
          categoryId: index.toString(16).padStart(8, "0"),
          scriptHash: CONTRACT_SCRIPT_HASH,
          membershipProofCbor: "",
        },
      ]),
    ) as unknown as FraudProofCatalogueFixture["categories"];
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    for (const categoryName of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
      const category = categories[categoryName];
      await trie.insert(
        Buffer.from(
          Data.to(category.categoryId as never, categoryIdSchema),
          "hex",
        ),
        Buffer.from(
          Data.to(category.scriptHash as never, scriptHashSchema),
          "hex",
        ),
      );
    }
    for (const categoryName of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
      const category = categories[categoryName];
      const proof = await trie.prove(
        Buffer.from(
          Data.to(category.categoryId as never, categoryIdSchema),
          "hex",
        ),
      );
      (
        categories as Record<
          string,
          {
            categoryId: string;
            scriptHash: string;
            membershipProofCbor: string;
          }
        >
      )[categoryName] = {
        ...category,
        membershipProofCbor: proof.toCBOR().toString("hex"),
      };
    }
    return {
      root: Buffer.from(trie.hash).toString("hex"),
      categories,
    };
  };

let fraudProofCatalogueFixture: FraudProofCatalogueFixture;

beforeAll(async () => {
  fraudProofCatalogueFixture = await buildFraudProofCatalogueFixture();
});

const canonicalIdentity = (): Omit<DeploymentManifestV1Value, "manifestId"> => {
  const referenceOutRefByContract = new Map<
    string,
    { readonly txHash: string; readonly outputIndex: number }
  >(
    Object.values(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contractName, outputIndex) => [
        contractName,
        { txHash: "22".repeat(32), outputIndex },
      ],
    ),
  );
  const contracts: Record<
    string,
    DeploymentManifestV1Value["contracts"][string]
  > = Object.fromEntries(
    DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.map((contractName) => [
      contractName,
      {
        refScriptUTxO: referenceOutRefByContract.get(contractName) ?? null,
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
    fraudProofCatalogue: fraudProofCatalogueFixture,
  };
  const referenceScripts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_ROLES.map((role) => {
      const contractName =
        DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
          role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
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
  const steps = Object.fromEntries(
    DEPLOYMENT_MANIFEST_V1_STEP_NAMES.map((stepName) => [
      stepName,
      {
        status:
          stepName === "prepareHubOracleNonce" ||
          stepName === "deployNodeRuntimeReferenceScripts" ||
          stepName === "initProtocol"
            ? ("complete" as const)
            : ("pending" as const),
      },
    ]),
  );
  return {
    schemaVersion: DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
    network: "Preview",
    cardanoProtocolParameters: {
      snapshot: CARDANO_PARAMETERS,
      digest: computeDeploymentManifestV1JsonDigest(CARDANO_PARAMETERS),
    },
    genesis: {
      headerHash: "00".repeat(28),
      utxoSetDigest: computeDeploymentManifestV1JsonDigest(
        normalizeDeploymentManifestV1JsonValue([]),
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
      committeeSignersHash: computeDeploymentManifestV1DaCommitteeSignersHash([
        DA_VKEY,
      ]),
      threshold: 1,
      transportProfile: {
        protocolVersion: DA_TRANSPORT_V1_PROTOCOL_VERSION,
        runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
        envelopeEncoding: "identity",
        zstdLevel: 3,
        limits: DA_TRANSPORT_LIMITS_V1,
        retentionDays: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
      },
    },
    proofEvidence: {
      digest: null,
      blueprintHash: "55".repeat(32),
    },
    validationDispute: {
      version: MIDGARD_CONSENSUS_PROFILE_V1.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs,
    },
    steps,
  };
};

const withId = (
  identity: Omit<DeploymentManifestV1Value, "manifestId">,
): DeploymentManifestV1Value => ({
  ...identity,
  manifestId: computeDeploymentManifestId(identity),
});

const canonicalManifest = (): DeploymentManifestV1Value =>
  withId(canonicalIdentity());

const withFraudProofCatalogue = (
  identity: Omit<DeploymentManifestV1Value, "manifestId">,
  fraudProofCatalogue: FraudProofCatalogueFixture,
): Omit<DeploymentManifestV1Value, "manifestId"> => ({
  ...identity,
  contracts: {
    ...identity.contracts,
    fraudProofCatalogueMint: {
      ...identity.contracts.fraudProofCatalogueMint,
      fraudProofCatalogue,
    },
  },
});

const mutateHexByte = (hex: string, byteIndex: number): string => {
  const offset = byteIndex * 2;
  const current = Number.parseInt(hex.slice(offset, offset + 2), 16);
  return `${hex.slice(0, offset)}${(current ^ 1).toString(16).padStart(2, "0")}${hex.slice(offset + 2)}`;
};

describe("V1 deployment manifest", () => {
  it("delegates canonical JSON normalization and digesting to core", () => {
    expect(normalizeDeploymentManifestV1JsonValue).toBe(
      normalizeSharedDeploymentManifestV1JsonValue,
    );
    expect(computeDeploymentManifestV1JsonDigest).toBe(
      computeSharedDeploymentManifestV1JsonDigest,
    );
  });

  it("accepts the sole exact authenticated V1 manifest", () => {
    expect(parseDeploymentManifestV1Value(canonicalManifest())).toEqual(
      canonicalManifest(),
    );
  });

  it("rejects missing and unexpected root fields", () => {
    const {
      da: _da,
      manifestId: _manifestId,
      ...missingDa
    } = canonicalManifest();
    expect(() =>
      parseDeploymentManifestV1Value({
        ...missingDa,
        manifestId: computeDeploymentManifestId(
          missingDa as Omit<DeploymentManifestV1Value, "manifestId">,
        ),
      }),
    ).toThrow(/value\.da is required/u);

    expect(() =>
      parseDeploymentManifestV1Value({
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
    } as Omit<DeploymentManifestV1Value, "manifestId">;
    expect(() =>
      parseDeploymentManifestV1Value(withId(missingContract)),
    ).toThrow(/contracts\.fraudProofZeroInput is required/u);
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
    } as Omit<DeploymentManifestV1Value, "manifestId">;
    expect(() =>
      parseDeploymentManifestV1Value(withId(missingContract)),
    ).toThrow(/contracts\.validationTraceDisputeSource is required/u);
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
      parseDeploymentManifestV1Value(
        withId(tampered as Omit<DeploymentManifestV1Value, "manifestId">),
      ),
    ).toThrow(/contracts\.txOrderSpend\.scriptHash mismatch/u);
  });

  it("rejects swapped fraud-proof catalogue IDs with a recomputed manifest ID", () => {
    const identity = canonicalIdentity();
    const catalogue =
      identity.contracts.fraudProofCatalogueMint.fraudProofCatalogue!;
    const doubleSpend = catalogue.categories.doubleSpend;
    const nonExistentInput = catalogue.categories.nonExistentInput;
    const tampered = withFraudProofCatalogue(identity, {
      ...catalogue,
      categories: {
        ...catalogue.categories,
        doubleSpend: {
          ...doubleSpend,
          categoryId: nonExistentInput.categoryId,
        },
        nonExistentInput: {
          ...nonExistentInput,
          categoryId: doubleSpend.categoryId,
        },
      },
    });
    expect(() => parseDeploymentManifestV1Value(withId(tampered))).toThrow(
      /categories\.doubleSpend\.categoryId must be 00000000/u,
    );
  });

  it("rejects duplicate fraud-proof catalogue IDs with a recomputed manifest ID", () => {
    const identity = canonicalIdentity();
    const catalogue =
      identity.contracts.fraudProofCatalogueMint.fraudProofCatalogue!;
    const tampered = withFraudProofCatalogue(identity, {
      ...catalogue,
      categories: {
        ...catalogue.categories,
        nonExistentInput: {
          ...catalogue.categories.nonExistentInput,
          categoryId: catalogue.categories.doubleSpend.categoryId,
        },
      },
    });
    expect(() => parseDeploymentManifestV1Value(withId(tampered))).toThrow(
      /categories\.nonExistentInput\.categoryId must be 00000001/u,
    );
  });

  it("rejects an altered fraud-proof catalogue root with a recomputed manifest ID", () => {
    const identity = canonicalIdentity();
    const catalogue =
      identity.contracts.fraudProofCatalogueMint.fraudProofCatalogue!;
    const tampered = withFraudProofCatalogue(identity, {
      ...catalogue,
      root: "44".repeat(32),
    });
    expect(() => parseDeploymentManifestV1Value(withId(tampered))).toThrow(
      /fraudProofCatalogue\.root mismatch/u,
    );
  });

  it("rejects an altered fraud-proof membership proof with a recomputed manifest ID", () => {
    const identity = canonicalIdentity();
    const catalogue =
      identity.contracts.fraudProofCatalogueMint.fraudProofCatalogue!;
    const tampered = withFraudProofCatalogue(identity, {
      ...catalogue,
      categories: {
        ...catalogue.categories,
        doubleSpend: {
          ...catalogue.categories.doubleSpend,
          membershipProofCbor: mutateHexByte(
            catalogue.categories.doubleSpend.membershipProofCbor,
            20,
          ),
        },
      },
    });
    expect(() => parseDeploymentManifestV1Value(withId(tampered))).toThrow(
      /categories\.doubleSpend\.membershipProofCbor/u,
    );
  });

  it("rejects tampered Cardano and DA identity fields", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    expect(() =>
      parseDeploymentManifestV1Value(
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
      parseDeploymentManifestV1Value(
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
      parseDeploymentManifestV1Value(
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

  it("rejects an unsupported profile and tuple digest", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    expect(() =>
      parseDeploymentManifestV1Value({
        ...withId({
          ...identity,
          consensusProfile: {
            ...MIDGARD_CONSENSUS_PROFILE_V1,
            profileId: "unsupported-profile-99",
          } as never,
        }),
      }),
    ).toThrow(/consensusProfile must exactly match canonical V1/u);

    expect(() =>
      parseDeploymentManifestV1Value(
        withId({
          ...identity,
          consensusProfileDigest: "77".repeat(32),
        }),
      ),
    ).toThrow(/consensusProfileDigest must exactly match canonical V1/u);
  });
});
