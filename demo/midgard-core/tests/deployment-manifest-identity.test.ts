import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { bytesToHex, hexToBytes } from "@noble/hashes/utils.js";
import { beforeAll, describe, expect, expectTypeOf, it } from "vitest";

import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
} from "../src/consensus-profile.js";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "../src/da-transport.js";
import {
  assertDeploymentMarkerMatches,
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
  type DeploymentManifestFraudProofCatalogueIdentity,
  makeDeploymentMarker,
  MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
  normalizeDeploymentManifestJsonValue,
  parseDeploymentManifestAvailabilityChallenge,
  parseDeploymentManifestEconomics,
  parseDeploymentMarker,
  verifyDeploymentManifestFraudProofCatalogueIdentity,
  verifyDeploymentManifestIdentity,
  verifyFinalizedDeploymentManifest,
} from "../src/deployment-manifest-identity.js";

let generatedCatalogueFixture: DeploymentManifestFraudProofCatalogueIdentity;

const CATALOGUE_FIXTURE_SCRIPT_HASH =
  "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0";

const catalogueFixtureKey = (categoryId: string): Buffer =>
  Buffer.concat([Buffer.from([0x44]), Buffer.from(categoryId, "hex")]);

beforeAll(async () => {
  const value = Buffer.from(`581c${CATALOGUE_FIXTURE_SCRIPT_HASH}`, "hex");
  const trie = await Trie.fromList(
    DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map(
      (categoryName) => ({
        key: catalogueFixtureKey(
          DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[categoryName],
        ),
        value,
      }),
    ),
  );
  const categories: Record<
    string,
    DeploymentManifestFraudProofCatalogueIdentity["categories"][keyof DeploymentManifestFraudProofCatalogueIdentity["categories"]]
  > = {};
  for (const categoryName of DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const categoryId =
      DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[categoryName];
    const proof = await trie.prove(catalogueFixtureKey(categoryId));
    categories[categoryName] = {
      categoryId,
      scriptHash: CATALOGUE_FIXTURE_SCRIPT_HASH,
      membershipProofCbor: proof.toCBOR().toString("hex"),
    };
  }
  generatedCatalogueFixture = {
    root: Buffer.from(trie.hash).toString("hex"),
    categories:
      categories as DeploymentManifestFraudProofCatalogueIdentity["categories"],
  };
});

const catalogueFixture = (): DeploymentManifestFraudProofCatalogueIdentity =>
  generatedCatalogueFixture;

const identityInput = () => ({
  schemaVersion: MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
  network: "Preprod",
  cardanoProtocolParameters: {},
  genesis: {},
  createdAt: "2026-07-24T00:00:00.000Z",
  updatedAt: "2026-07-24T00:00:00.000Z",
  referenceScriptDeployAddress: "addr_test1reference",
  hubOracleOneShot: {},
  referenceScriptAuthPolicy: {},
  contracts: {},
  referenceScripts: {},
  da: {},
  artifacts: {},
  steps: {},
  validationDispute: {},
  l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
  economics: DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
  availabilityChallenge: {
    responseClasses: {
      smallPayloadMaxBytes: 65_536,
      smallResponseWindowMs: 3_600_000,
      fullPayloadMaxBytes: 67_108_864,
      fullResponseWindowMs: 172_800_000,
    },
    responseGeometry: {
      chunkByteLength: 14_020,
      trancheByteLength: 4_194_304,
      maxTrancheCount: 16,
    },
    daBondLovelace: 10_000_000_000,
    challengerBondLovelace: 10_000_000_000,
    maxOpenFeeLovelace: 500_000,
    maxPublicationFeeLovelace: 500_000,
    maxSettlementFeeLovelace: 500_000,
    maxCloseFeeLovelace: 1_000_000,
    maxTimeoutFeeLovelace: 1_200_000,
    bondOwnerCredential: "77".repeat(28),
  },
});

// A complete document built independently of the node parser and SDK builders.
const finalizedManifest = () => {
  const referenceOutRefs = new Map<
    string,
    { txHash: string; outputIndex: number }
  >(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (name, outputIndex) => [name, { txHash: "22".repeat(32), outputIndex }],
    ),
  );
  const policy = { type: "Native" as const, script: "820501" };
  const policyId = validatorToScriptHash(policy);
  const snapshot = {
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
  };
  const document = {
    ...identityInput(),
    cardanoProtocolParameters: {
      snapshot,
      digest: computeDeploymentManifestJsonDigest(snapshot),
    },
    genesis: { headerHash: "00".repeat(28), utxoSetDigest: "33".repeat(32) },
    hubOracleOneShot: {
      txHash: "11".repeat(32),
      outputIndex: 0,
      outRef: `${"11".repeat(32)}#0`,
      status: "consumed_by_init",
    },
    referenceScriptAuthPolicy: {
      policyId,
      nativeScript: {
        type: policy.type,
        cborHex: policy.script,
        expiresAtSlot: 1,
        expiresAtUnixTime: 1,
        timelockDurationMs: 1,
      },
      tokenNames: DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
      postTimelockAudit: {
        required: true,
        rule: "No authenticated reference-script output may change.",
      },
    },
    contracts: Object.fromEntries(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((name) => [
        name,
        {
          refScriptUTxO: referenceOutRefs.get(name) ?? null,
          contract:
            name === "referenceScriptAuthMint"
              ? { type: policy.type, cborHex: policy.script }
              : { type: "PlutusV3", cborHex: "01" },
          scriptHash:
            name === "referenceScriptAuthMint"
              ? policyId
              : CATALOGUE_FIXTURE_SCRIPT_HASH,
          ...(name === "fraudProofCatalogueMint"
            ? { fraudProofCatalogue: catalogueFixture() }
            : {}),
        },
      ]),
    ),
    referenceScripts: Object.fromEntries(
      Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
        ([role, name]) => {
          const outRef = referenceOutRefs.get(name)!;
          const tokenName =
            DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
              role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
            ];
          return [
            role,
            {
              status: "confirmed",
              roleUnit: policyId + Buffer.from(tokenName).toString("hex"),
              scriptHash:
                name === "referenceScriptAuthMint"
                  ? policyId
                  : CATALOGUE_FIXTURE_SCRIPT_HASH,
              outRef: `${outRef.txHash}#${outRef.outputIndex}`,
            },
          ];
        },
      ),
    ),
    da: {
      committeeVkeys: ["44".repeat(32)],
      committeeSignersHash: bytesToHex(
        blake2b(hexToBytes("44".repeat(32)), { dkLen: 32 }),
      ),
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
    artifacts: { blueprintHash: "55".repeat(32) },
    steps: {
      prepareHubOracleNonce: { status: "complete" },
      deployNodeRuntimeReferenceScripts: { status: "complete" },
      initProtocol: { status: "complete" },
      availabilityRegistration: { status: "complete" },
      phasRegistration: { status: "pending" },
      operatorRegistration: { status: "pending" },
      operatorActivation: { status: "pending" },
    },
    validationDispute: {
      version: MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
    },
  };
  return { ...document, manifestId: computeDeploymentManifestId(document) };
};

describe("finalized deployment manifest", () => {
  it("returns a typed view of the original document, including on repeated verification", () => {
    const document = finalizedManifest();
    const verified = verifyFinalizedDeploymentManifest(document);
    expectTypeOf(verified.network).toEqualTypeOf<
      "Mainnet" | "Preprod" | "Preview" | "Custom"
    >();
    expectTypeOf(verified.artifacts.blueprintHash).toEqualTypeOf<string>();
    expectTypeOf(
      verified.cardanoProtocolParameters.snapshot.maxTxSize,
    ).toEqualTypeOf<string>();
    expect(verified).toBe(document);
    expect(verifyFinalizedDeploymentManifest(document)).toBe(document);
    expect(Object.isFrozen(document)).toBe(false);
    expect(Object.isFrozen(document.artifacts)).toBe(false);
  });

  it.each([
    [
      "network",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.network = "unsupported";
      },
      /network is unsupported/,
    ],
    [
      "blueprint identity",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.artifacts.blueprintHash = "00";
      },
      /blueprintHash/,
    ],
    [
      "protocol parameter snapshot",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.cardanoProtocolParameters.snapshot.maxTxSize = "0";
        manifest.cardanoProtocolParameters.digest =
          computeDeploymentManifestJsonDigest(
            manifest.cardanoProtocolParameters.snapshot,
          );
      },
      /bounds must be positive/,
    ],
    [
      "contract hash",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.contracts.hubOracleMint.scriptHash = "00".repeat(28);
      },
      /scriptHash mismatch/,
    ],
    [
      "reference publication",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.referenceScripts["hub-oracle minting"].status = "pending";
      },
      /status must be confirmed/,
    ],
    [
      "required initialization",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.steps.initProtocol.status = "pending";
      },
      /initProtocol.status must be complete/,
    ],
    [
      "optional step status",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.steps.operatorActivation.status = "unsupported";
      },
      /operatorActivation.status is unsupported/,
    ],
  ] as const)(
    "rejects invalid %s even with a matching document ID",
    (_name, mutate, error) => {
      const manifest = finalizedManifest();
      mutate(manifest);
      const { manifestId: _manifestId, ...identity } = manifest;
      manifest.manifestId = computeDeploymentManifestId(identity);
      // Identity-only verification deliberately does not claim finalization.
      expect(verifyDeploymentManifestIdentity(manifest)).toBe(manifest);
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(error);
    },
  );

  it("rechecks document identity after a previously verified caller-owned value changes", () => {
    const manifest = finalizedManifest();
    verifyFinalizedDeploymentManifest(manifest);
    manifest.artifacts.blueprintHash = "66".repeat(32);
    expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
      /id mismatch/,
    );
  });
});

describe("DeploymentManifestV1 shared identity", () => {
  it("includes every registered fraud-proof validator in the canonical registry", () => {
    expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toContain("fraudProofZeroInput");
    // #547 appended the Q18/Q31/Q15 first-step validators. The registry is
    // append-only, so each must be present and the catalogue order must name
    // exactly the same set of categories in the same positions.
    expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toContain(
      "fraudProofNoReferenceInput",
    );
    expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toContain(
      "fraudProofReferenceInputNoIdx",
    );
    expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toContain(
      "fraudProofInvalidSignature",
    );
    // The registry is append-only, so its size is not a contract and pinning
    // it only forces a re-pin on every legitimate append. What is a contract
    // is that the roster stays internally consistent: a validator that is
    // registered but not published under a reference-script role is applied on
    // every deployment and reachable from none, and two roles that share an
    // auth-token name or a contract collide on chain.
    const contractNames =
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES as readonly string[];
    const contractByRole =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE as Readonly<
        Record<string, string>
      >;
    const tokenNameByRole =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES as Readonly<
        Record<string, string>
      >;
    const publishedRoles = Object.keys(contractByRole);
    expect(new Set(contractNames).size).toBe(contractNames.length);
    expect(publishedRoles.filter((role) => !(role in tokenNameByRole))).toEqual(
      [],
    );
    expect(
      publishedRoles.filter(
        (role) => !contractNames.includes(contractByRole[role]!),
      ),
    ).toEqual([]);
    const publishedContracts = publishedRoles.map(
      (role) => contractByRole[role]!,
    );
    expect(new Set(publishedContracts).size).toBe(publishedContracts.length);
    const tokenNames = Object.values(tokenNameByRole);
    expect(new Set(tokenNames).size).toBe(tokenNames.length);
    // The only registered contracts without a reference-script role are the
    // seven core validators the deployment applies directly; every fraud-proof
    // validator must be published.
    expect(
      contractNames.filter((name) => !publishedContracts.includes(name)),
    ).toEqual([
      "escapeHatchSpend",
      "escapeHatchMint",
      "fraudProofCatalogueSpend",
      "fraudProofSpend",
      "txOrderSpend",
      "txOrderMint",
      "settlementSpend",
    ]);
    // One role is token-only: the CEK direct resolver is referenced by its
    // auth token and never applied as its own reference script.
    expect(
      Object.keys(tokenNameByRole).filter((role) => !(role in contractByRole)),
    ).toEqual(["V1 validation-trace CEK direct resolver"]);
    expect(
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        "V1 fraud-proof min-ada step-02 tx yield"
      ],
    ).toBe("fraudProofMinAdaStep02TxWithdraw");
    expect(
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
        "V1 fraud-proof min-ada step-02 UTxO yield"
      ],
    ).toBe("V1FpMinAdaS02UtxoYield");
    expect(
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        "V1 fraud-proof withdrawn-input step-03"
      ],
    ).toBe("fraudProofWithdrawnInputStep03");
    expect(
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        "V1 fraud-proof transition-trace final-7"
      ],
    ).toBe("fraudProofTransitionTraceDuplicate");

    const appendedLinearFamilies = [
      ["FabricatedDeposit", "fabricated-deposit", 4],
      ["FabricatedWithdrawal", "fabricated-withdrawal", 4],
      ["MissingSignature", "missing-signature", 4],
      ["MissingNativeScriptTx", "missing-native-script-tx", 8],
      ["WithdrawnReferenceInput", "withdrawn-reference-input", 3],
      ["CanonicalDecodability", "canonical-decodability", 2],
      ["CommittedFieldShape", "committed-field-shape", 2],
      ["MinFee", "min-fee", 2],
      ["WithdrawalMistag", "withdrawal-mistag", 5],
      ["DoubleWithdraw", "double-withdraw", 2],
      ["CrossBlockDuplicateEvent", "cross-block-duplicate-event", 2],
      ["L2TxMistag", "l2-tx-mistag", 2],
      ["WithdrawnInput", "withdrawn-input", 3],
      ["ValueNotPreserved", "value-not-preserved", 4],
      ["InputSetUniqueness", "input-set-uniqueness", 4],
      ["MintAuthorization", "mint-authorization", 5],
      ["MissingNativeScriptUtxo", "missing-native-script-utxo", 5],
      ["NativeScriptInvalid", "native-script-invalid", 3],
      ["MinAda", "min-ada", 2],
      ["TransactionOutputNonCanonical", "transaction-output-non-canonical", 4],
      ["ResolvedOutputNonCanonical", "resolved-output-non-canonical", 5],
      ["MintDeclaredAssetLimit", "mint-declared-asset-limit", 4],
    ] as const;
    for (const [contractStem, roleStem, stepCount] of appendedLinearFamilies) {
      for (let step = 1; step <= stepCount; step += 1) {
        const stepSuffix =
          step === 1 ? "" : `Step${step.toString().padStart(2, "0")}`;
        const contractName = `fraudProof${contractStem}${stepSuffix}`;
        const role = `V1 fraud-proof ${roleStem} step-${step.toString().padStart(2, "0")}`;
        expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toContain(contractName);
        expect(
          DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
            role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
          ],
        ).toBe(contractName);
      }
    }

    const executionNativeScriptInvalidContracts = [
      ["step-01", "fraudProofExecutionNativeScriptInvalid"],
      ["step-02", "fraudProofExecutionNativeScriptInvalidStep02"],
      ["step-03", "fraudProofExecutionNativeScriptInvalidStep03"],
      ["step-04", "fraudProofExecutionNativeScriptInvalidStep04"],
      ["step-05", "fraudProofExecutionNativeScriptInvalidStep05"],
      ["step-06", "fraudProofExecutionNativeScriptInvalidStep06"],
      [
        "accepted-reconstruction-init",
        "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
      ],
      [
        "accepted-spend-prefix",
        "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
      ],
      [
        "accepted-mint-prefix",
        "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
      ],
      [
        "accepted-observer-prefix",
        "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
      ],
      [
        "accepted-receive-prefix",
        "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
      ],
      [
        "accepted-inline-source",
        "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
      ],
      [
        "accepted-reference-source",
        "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
      ],
    ] as const;
    for (const [
      roleSuffix,
      contractName,
    ] of executionNativeScriptInvalidContracts) {
      expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toContain(contractName);
      expect(
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
          `V1 fraud-proof execution-native-script-invalid ${roleSuffix}` as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
        ],
      ).toBe(contractName);
    }

    const nativeScriptDecodingContracts = [
      [
        "V1 fraud-proof native-script-decoding step-01",
        "fraudProofNativeScriptDecoding",
      ],
      [
        "V1 fraud-proof native-script-decoding step-02",
        "fraudProofNativeScriptDecodingStep02",
      ],
      [
        "V1 fraud-proof native-script-decoding step-03 open-subject",
        "fraudProofNativeScriptDecodingStep03OpenSubject",
      ],
      [
        "V1 fraud-proof native-script-decoding step-03 bind-descriptor",
        "fraudProofNativeScriptDecodingStep03BindDescriptor",
      ],
      [
        "V1 fraud-proof native-script-decoding step-03 advance-or-close",
        "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
      ],
      [
        "V1 fraud-proof native-script-decoding step-04",
        "fraudProofNativeScriptDecodingStep04",
      ],
    ] as const;
    for (const [role, contractName] of nativeScriptDecodingContracts) {
      expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toContain(contractName);
      expect(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[role]).toBe(
        contractName,
      );
    }

    const transitionFinalContracts = [
      "fraudProofTransitionTraceControl",
      "fraudProofTransitionTraceSource",
      "fraudProofTransitionTraceWithdrawal",
      "fraudProofTransitionTraceForced",
      "fraudProofTransitionTraceAcceptedTransaction",
      "fraudProofTransitionTraceDeposit",
      "fraudProofTransitionTraceL1Event",
      "fraudProofTransitionTraceDuplicate",
    ] as const;
    transitionFinalContracts.forEach((contractName, index) => {
      expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toContain(contractName);
      expect(
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
          `V1 fraud-proof transition-trace final-${index.toString()}` as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
        ],
      ).toBe(contractName);
    });
  });

  it("names the network-id auxiliary forced door and forced scan", () => {
    // Neither is a member of the network-id chain's `steps` — the forced door
    // is a side entrance into step 02 and the forced scan is the resumable
    // output walk between them — so a step-indexed roster reaches neither.
    // Each has to carry its own contract name, reference-script role and
    // auth-token name or it is applied on every deployment and published
    // nowhere.
    const networkIdAuxiliaryContracts = [
      [
        "V1 fraud-proof network-id forced step",
        "fraudProofNetworkIdForcedStep",
        "V1FpNetworkIdForced",
      ],
      [
        "V1 fraud-proof network-id forced scan",
        "fraudProofNetworkIdForcedScan",
        "V1FpNetworkIdForcedScan",
      ],
    ] as const;
    for (const [role, contractName, tokenName] of networkIdAuxiliaryContracts) {
      expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toContain(contractName);
      expect(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[role]).toBe(
        contractName,
      );
      expect(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[role]).toBe(
        tokenName,
      );
    }
    expect(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.indexOf(
        "fraudProofNetworkIdForcedScan",
      ),
    ).toBe(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.indexOf(
        "fraudProofNetworkIdForcedStep",
      ) + 1,
    );
  });

  it("authenticates the exact 54-entry fraud-proof catalogue root and proofs", () => {
    const catalogue = catalogueFixture();
    expect(
      verifyDeploymentManifestFraudProofCatalogueIdentity(catalogue),
    ).toEqual(catalogue);
  });

  it("rejects catalogue root, explicit ID, value, proof, and category-set tampering", () => {
    const catalogue = catalogueFixture();

    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity({
        ...catalogue,
        root: "ff".repeat(32),
      }),
    ).toThrow(/catalogue root mismatch/u);

    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          nonExistentInputNoIndex: {
            ...catalogue.categories.nonExistentInputNoIndex,
            categoryId: "00000003",
          },
        },
      }),
    ).toThrow(/nonExistentInputNoIndex\.categoryId must be 00000002/u);

    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          zeroInput: {
            ...catalogue.categories.zeroInput,
            scriptHash: "aa".repeat(28),
          },
        },
      }),
    ).toThrow(/catalogue root mismatch/u);

    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          invalidRange: {
            ...catalogue.categories.invalidRange,
            membershipProofCbor:
              catalogue.categories.doubleSpend.membershipProofCbor,
          },
        },
      }),
    ).toThrow(/invalidRange\.membershipProofCbor does not prove membership/u);

    const { validationTraceDispute: _missing, ...missingCategory } =
      catalogue.categories;
    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity({
        ...catalogue,
        categories:
          missingCategory as DeploymentManifestFraudProofCatalogueIdentity["categories"],
      }),
    ).toThrow(/validationTraceDispute is required/u);

    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          historicalCategory: catalogue.categories.doubleSpend,
        } as DeploymentManifestFraudProofCatalogueIdentity["categories"],
      }),
    ).toThrow(/historicalCategory is unexpected/u);
  });

  it("rejects malformed categories at the exported catalogue boundary", () => {
    const catalogue = catalogueFixture();
    const { membershipProofCbor: _proof, ...missingProof } =
      catalogue.categories.doubleSpend;
    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          doubleSpend:
            missingProof as DeploymentManifestFraudProofCatalogueIdentity["categories"]["doubleSpend"],
        },
      }),
    ).toThrow(/doubleSpend\.membershipProofCbor is required/u);

    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          doubleSpend: {
            ...catalogue.categories.doubleSpend,
            scriptHash: "AA".repeat(28),
          },
        },
      }),
    ).toThrow(/doubleSpend\.scriptHash must be lowercase canonical hex/u);

    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          doubleSpend: {
            ...catalogue.categories.doubleSpend,
            membershipProofCbor: "f",
          },
        },
      }),
    ).toThrow(
      /doubleSpend\.membershipProofCbor must be lowercase canonical hex/u,
    );
  });

  it("owns canonical JSON normalization and digest vectors", () => {
    const normalized = normalizeDeploymentManifestJsonValue({
      z: [1, 2n],
      a: { y: true, x: null },
    });
    expect(normalized).toEqual({
      z: [1, "2"],
      a: { y: true, x: null },
    });
    expect(computeDeploymentManifestJsonDigest(normalized)).toBe(
      "ccff47a9e0ebd42629b30db95fa7988b032093e903958b916820987a100d7cb4",
    );
    expect(
      computeDeploymentManifestJsonDigest({
        a: { x: null, y: true },
        z: [1, "2"],
      }),
    ).toBe("ccff47a9e0ebd42629b30db95fa7988b032093e903958b916820987a100d7cb4");
    expect(
      computeDeploymentManifestJsonDigest({
        a: { x: null, y: false },
        z: [1, "2"],
      }),
    ).not.toBe(
      "ccff47a9e0ebd42629b30db95fa7988b032093e903958b916820987a100d7cb4",
    );
  });

  it("rejects values outside the canonical JSON boundary", () => {
    expect(() =>
      normalizeDeploymentManifestJsonValue({ missing: undefined }),
    ).toThrow(/value\.missing must not be undefined/u);
    expect(() =>
      normalizeDeploymentManifestJsonValue({ invalid: Number.NaN }),
    ).toThrow(/must contain only finite numbers/u);
    expect(() => computeDeploymentManifestJsonDigest({ raw: 2n })).toThrow(
      /must contain only JSON-safe values/u,
    );
  });

  it("recomputes the exact full-manifest identity", () => {
    const identity = identityInput();
    const manifest = {
      ...identity,
      manifestId: computeDeploymentManifestId(identity),
    };
    expect(verifyDeploymentManifestIdentity(manifest)).toEqual(manifest);
  });

  it("accepts only exact release-bound economics profiles", () => {
    const bounded =
      DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"];
    const publicPreprod =
      DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["public-preprod-launch-v1"];
    expect(parseDeploymentManifestEconomics(bounded)).toEqual(bounded);
    expect(parseDeploymentManifestEconomics(publicPreprod)).toEqual(
      publicPreprod,
    );
    expect(() =>
      parseDeploymentManifestEconomics({
        ...bounded,
        slashingPenaltyLovelace: bounded.slashingPenaltyLovelace + 1,
      }),
    ).toThrow(/slashingPenaltyLovelace must equal/u);
    expect(() =>
      parseDeploymentManifestEconomics({
        ...bounded,
        profile: "public-preprod-launch-v1",
      }),
    ).toThrow(/requiredBondLovelace must equal/u);
    expect(() =>
      parseDeploymentManifestEconomics({ ...bounded, extra: true }),
    ).toThrow(/must contain exactly/u);
    const { proverCollateralFloorLovelace: _omitted, ...legacy } = bounded;
    expect(() => parseDeploymentManifestEconomics(legacy)).toThrow(
      /must contain exactly/u,
    );
    expect(() =>
      parseDeploymentManifestEconomics({
        ...bounded,
        proverCollateralFloorLovelace:
          bounded.proverCollateralFloorLovelace + 1,
      }),
    ).toThrow(/proverCollateralFloorLovelace must equal/u);
  });

  it("keeps activated Q58 chunk geometry separate from the 4,095-byte proof-field limit", () => {
    const availability = identityInput().availabilityChallenge;
    expect(
      parseDeploymentManifestAvailabilityChallenge({
        ...availability,
        responseGeometry: {
          ...availability.responseGeometry,
          chunkByteLength: 8_192,
        },
      }).responseGeometry.chunkByteLength,
    ).toBe(8_192);
    expect(() =>
      parseDeploymentManifestAvailabilityChallenge({
        ...availability,
        responseGeometry: {
          ...availability.responseGeometry,
          chunkByteLength:
            MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES + 1,
        },
      }),
    ).toThrow(/safety\/coverage bounds/u);
  });

  it("owns the sole exact DeploymentMarkerV1 boundary", () => {
    const manifestId = computeDeploymentManifestId(identityInput());
    const marker = makeDeploymentMarker(manifestId);
    expect(marker).toEqual({
      schemaVersion: MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
      manifestId,
    });
    expect(parseDeploymentMarker(marker)).toEqual(marker);
    expect(assertDeploymentMarkerMatches(marker, marker, "Postgres")).toEqual(
      marker,
    );
    expect(() =>
      parseDeploymentMarker({ ...marker, historicalVersion: 9 }),
    ).toThrow(/exactly schemaVersion and manifestId/u);
    expect(() =>
      parseDeploymentMarker({ manifestId: marker.manifestId }),
    ).toThrow(/exactly schemaVersion and manifestId/u);
    expect(() =>
      assertDeploymentMarkerMatches(
        marker,
        makeDeploymentMarker("ff".repeat(32)),
        "DA store",
      ),
    ).toThrow(
      `DA store deployment marker mismatch: expected ${marker.manifestId}, found ${"ff".repeat(32)}`,
    );
  });

  it("rejects tampering, missing fields, and extra fields", () => {
    const identity = identityInput();
    const manifest = {
      ...identity,
      manifestId: computeDeploymentManifestId(identity),
    };
    expect(() =>
      verifyDeploymentManifestIdentity({
        ...manifest,
        network: "Preview",
      }),
    ).toThrow(/id mismatch/u);

    const { da: _da, ...missingDa } = manifest;
    expect(() => verifyDeploymentManifestIdentity(missingDa)).toThrow(
      /value\.da is required/u,
    );
    expect(() =>
      verifyDeploymentManifestIdentity({
        ...manifest,
        historicalVersion: 9,
      }),
    ).toThrow(/value\.historicalVersion is unexpected/u);
  });
});
