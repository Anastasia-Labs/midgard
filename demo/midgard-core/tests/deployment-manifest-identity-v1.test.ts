import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { beforeAll, describe, expect, it } from "vitest";

import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
} from "../src/consensus-profile-v1.js";
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
} from "../src/deployment-manifest-identity-v1.js";

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
  proofEvidence: {},
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
    expect(
      DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.slice(-43),
    ).toEqual([
      "fabricatedDeposit",
      "fabricatedWithdrawal",
      "nativeScriptDecoding",
      "missingSignature",
      "missingNativeScriptTx",
      "withdrawnReferenceInput",
      "canonicalDecodability",
      "committedFieldShape",
      "minFee",
      "withdrawalMistag",
      "doubleWithdraw",
      "crossBlockDuplicateEvent",
      "l2TxMistag",
      "withdrawnInput",
      "valueNotPreserved",
      "inputSetUniqueness",
      "mintAuthorization",
      "networkId",
      "missingNativeScriptUtxo",
      "nativeScriptInvalid",
      "minAda",
      "fieldPreimageLengthMismatch",
      "fieldItemWidthIllegal",
      "witnessScriptDecoding",
      "scriptIntegrityHashMissing",
      "transactionOutputNonCanonical",
      "resolvedOutputNonCanonical",
      "mintDeclaredAssetLimit",
      "spendInputSignerMissing",
      "protectedOutputSignerMissing",
      "observersForbiddenOnUntaggedNetwork",
      "observerOrderInvalid",
      "redeemerCanonicity",
      "outputReferenceScriptDecoding",
      "executionSourceScriptDecoding",
      "receivePurposeLanguage",
      "unusedScriptWitness",
      "missingScriptSource",
      "missingRedeemer",
      "unusedRedeemer",
      "executionNativeScriptInvalid",
      "scriptIntegrityHashMismatch",
      "distinctAssetAccumulationLimit",
    ]);
    expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toHaveLength(287);
    expect(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
    ).toHaveLength(280);
    expect(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES),
    ).toHaveLength(281);
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

  it("authenticates the exact 54-entry fraud-proof catalogue root and proofs", () => {
    const catalogue = catalogueFixture();
    expect(catalogue.root).toBe(
      "6cbb4f733ce82c426c88652348b4f04c04975251222c10526b46d728f58ee351",
    );
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
    // Rebound 2026-08-30: Q58's exact response classes and release-selected
    // geometry/bond/all lifecycle fee ceilings became authenticated deployment
    // identity after the exact 14,020-byte signed-transaction measurement.
    // The same rebound adds F04's exact 5 ADA prover collateral floor as an
    // authenticated release-economics term. Rebound 2026-08-29: exact release economics became an authenticated
    // root field, so manifest identity distinguishes public launch from the
    // bounded acceptance profile without consulting `network`. The preceding
    // rebound made the 30/2160 rollback policy release-bound.
    // Previously rebound 2026-08-23: the identity input embeds
    // MIDGARD_CONSENSUS_PROFILE, whose committed constants changed in
    // 2c7fd3bb (E_MIN_ADA at the ValueAndMint descriptor step, #618/#627);
    // the old pin predated that commit. Previously rebound 2026-08-01 for
    // 4a4bc660 on the same basis.
    expect(manifest.manifestId).toBe(
      "c5f43f5d6a805779f7f86d79b5186bd50e9435fee14c5ec7b444efc0f349c673",
    );
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
