import "./deployment-manifest-identity.finalized-deployment-manifest.js";

import { describe, expect, it } from "vitest";

import {
  assertDeploymentMarkerMatches,
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
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
import {
  daBondManifestAmounts,
  SELECTED_DEPLOYMENT_PROFILE,
} from "../src/deployment-profile.js";
import {
  catalogueFixture,
  finalizedManifest,
  identityInput,
} from "./deployment-manifest-identity.finalized-manifest.js";

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
    // Token-only roles: the CEK direct resolver is referenced by its auth
    // token and never applied as its own reference script.
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

  it("records the DA bond pool after the DA params governor, with no per-header bond yield", () => {
    // The pool is parameterised by the DA params policy and every later DA
    // contract by the pool policy, so its entries follow the governor's.
    const governorMint = DEPLOYMENT_MANIFEST_CONTRACT_NAMES.indexOf(
      "daParamsGovernorMint",
    );
    expect(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.slice(
        governorMint + 1,
        governorMint + 3,
      ),
    ).toEqual(["daBondPoolSpend", "daBondPoolMint"]);
    for (const [role, contractName, tokenName] of [
      ["da-bond-pool spending", "daBondPoolSpend", "DaBondPoolSpend"],
      ["da-bond-pool minting", "daBondPoolMint", "DaBondPoolMint"],
    ] as const) {
      expect(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[role]).toBe(
        contractName,
      );
      expect(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[role]).toBe(
        tokenName,
      );
    }
    // The availability family is its spend, its mint and the four arm
    // withdrawals: nothing else, so no per-header bond script survives.
    const availabilityArms = ["Open", "Settle", "Close", "Timeout"] as const;
    expect(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.filter((name) =>
        name.startsWith("availabilityChallenge"),
      ),
    ).toEqual([
      "availabilityChallengeSpend",
      "availabilityChallengeMint",
      ...availabilityArms.map((arm) => `availabilityChallenge${arm}Withdraw`),
    ]);
    expect(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES).filter(
        (role) => role.startsWith("availability-challenge"),
      ),
    ).toEqual([
      "availability-challenge spending",
      "availability-challenge minting",
      ...availabilityArms.map(
        (arm) => `availability-challenge ${arm.toLowerCase()} withdrawal`,
      ),
    ]);
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

  it("rejects a rehashed manifest that overrides the profile confirmation policy", () => {
    const { manifestId: _manifestId, ...identity } = finalizedManifest();
    const changed = {
      ...identity,
      l1Finality: {
        ...identity.l1Finality,
        confirmationDepth: identity.l1Finality.confirmationDepth + 1,
      },
    };
    expect(() =>
      verifyFinalizedDeploymentManifest({
        ...changed,
        manifestId: computeDeploymentManifestId(changed),
      }),
    ).toThrow(/l1Finality/u);
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

  it("500 tADA DA bond + 10k challenger bond passes on preprod-testing", () => {
    expect(SELECTED_DEPLOYMENT_PROFILE.name).toBe("preprod-testing");
    const identity = identityInput();
    expect(identity.availabilityChallenge.daBondLovelace).toBe(500_000_000);
    expect(identity.availabilityChallenge.challengerBondLovelace).toBe(
      10_000_000_000,
    );
    const parsed = parseDeploymentManifestAvailabilityChallenge(
      identity.availabilityChallenge,
    );
    expect(parsed).toMatchObject({
      daBondLovelace: 500_000_000,
      daSlashPenaltyLovelace: 100_000_000,
      daBondMinTopUpLovelace: 5_000_000,
      daBondPoolFloorLovelace: 5_000_000,
      challengeRecordLovelace: 27_000_000,
      challengerBondLovelace: 10_000_000_000,
    });
    const manifest = {
      ...identity,
      manifestId: computeDeploymentManifestId(identity),
    };
    expect(() => verifyDeploymentManifestIdentity(manifest)).not.toThrow();
  });

  it("unequal bonds accepted", () => {
    const availability = identityInput().availabilityChallenge;
    // Any challenger bond that covers the fee reserve, none equal to the
    // profile's 500 tADA DA bond.
    for (const challengerBondLovelace of [
      2_409_200_001, 9_999_999_999, 12_000_000_000,
    ]) {
      expect(
        parseDeploymentManifestAvailabilityChallenge({
          ...availability,
          challengerBondLovelace,
        }).challengerBondLovelace,
      ).toBe(challengerBondLovelace);
    }
  });

  it("coverage floor binds the challenger bond only", () => {
    const availability = identityInput().availabilityChallenge;
    // 16 tranches x ceil(4 MiB / 14,020) = 4,800 publications at 500,000,
    // plus 16 settlements at 500,000, plus max(close, timeout) = 1,200,000.
    const reserve = 4_800 * 500_000 + 16 * 500_000 + 1_200_000;
    expect(reserve).toBe(2_409_200_000);
    // The 500 tADA DA bond sits far below the reserve and is still accepted.
    expect(availability.daBondLovelace).toBeLessThan(reserve);
    expect(
      parseDeploymentManifestAvailabilityChallenge({
        ...availability,
        challengerBondLovelace: reserve + 1,
      }).challengerBondLovelace,
    ).toBe(reserve + 1);
    expect(() =>
      parseDeploymentManifestAvailabilityChallenge({
        ...availability,
        challengerBondLovelace: reserve,
      }),
    ).toThrow(/challenger bond must cover every maximum-size publication/u);
    // With one-lovelace fee ceilings the reserve drops below the DA bond. A
    // challenger bond equal to that reserve is still refused: a DA bond above
    // the reserve never stands in for the challenger bond.
    const oneLovelaceFees = {
      maxOpenFeeLovelace: 1,
      maxPublicationFeeLovelace: 1,
      maxSettlementFeeLovelace: 1,
      maxCloseFeeLovelace: 1,
      maxTimeoutFeeLovelace: 1,
    };
    const smallReserve = 4_800 + 16 + 1;
    expect(availability.daBondLovelace).toBeGreaterThan(smallReserve);
    expect(
      parseDeploymentManifestAvailabilityChallenge({
        ...availability,
        ...oneLovelaceFees,
        challengerBondLovelace: smallReserve + 1,
      }).challengerBondLovelace,
    ).toBe(smallReserve + 1);
    expect(() =>
      parseDeploymentManifestAvailabilityChallenge({
        ...availability,
        ...oneLovelaceFees,
        challengerBondLovelace: smallReserve,
      }),
    ).toThrow(/challenger bond must cover every maximum-size publication/u);
  });

  it("admits exactly the pooled-bond availability fields", () => {
    // The per-header bond and its owner credential are gone: the section is
    // the response shape, the profile's pool amounts, the deploy-time
    // challenger bond and the fee ceilings, and nothing else.
    const expectedKeys = [
      "responseClasses",
      "responseGeometry",
      "daBondLovelace",
      "challengerBondLovelace",
      "maxOpenFeeLovelace",
      "maxPublicationFeeLovelace",
      "maxSettlementFeeLovelace",
      "maxCloseFeeLovelace",
      "maxTimeoutFeeLovelace",
      "daSlashPenaltyLovelace",
      "daBondMinTopUpLovelace",
      "daBondPoolFloorLovelace",
      "challengeRecordLovelace",
    ].sort();
    const availability = identityInput().availabilityChallenge;
    expect(Object.keys(availability).sort()).toEqual(expectedKeys);
    expect(
      Object.keys(
        parseDeploymentManifestAvailabilityChallenge(availability),
      ).sort(),
    ).toEqual(expectedKeys);
    expect(() =>
      parseDeploymentManifestAvailabilityChallenge({
        ...availability,
        ownerCredential: "77".repeat(28),
      }),
    ).toThrow(/availabilityChallenge must contain exactly/u);
  });

  it("DA amounts must equal the selected profile", () => {
    const availability = identityInput().availabilityChallenge;
    for (const [key, expected] of Object.entries(daBondManifestAmounts())) {
      for (const wrong of [expected - 1, expected + 1]) {
        expect(() =>
          parseDeploymentManifestAvailabilityChallenge({
            ...availability,
            [key]: wrong,
          }),
        ).toThrow(
          new RegExp(
            `availabilityChallenge\\.${key} must equal the selected deployment profile's value ${expected.toString()}`,
            "u",
          ),
        );
      }
      const { [key as keyof typeof availability]: _omitted, ...missing } =
        availability;
      expect(() =>
        parseDeploymentManifestAvailabilityChallenge(missing),
      ).toThrow(/availabilityChallenge must contain exactly/u);
    }
    // A manifest written before the pooled bond carries the old DA bond and
    // none of the pool amounts.
    const {
      daSlashPenaltyLovelace: _penalty,
      daBondMinTopUpLovelace: _topUp,
      daBondPoolFloorLovelace: _floor,
      challengeRecordLovelace: _record,
      ...legacy
    } = availability;
    expect(() =>
      parseDeploymentManifestAvailabilityChallenge({
        ...legacy,
        daBondLovelace: 10_000_000_000,
      }),
    ).toThrow(/availabilityChallenge must contain exactly/u);
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
    ).toThrow(/compiled profile/u);

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
