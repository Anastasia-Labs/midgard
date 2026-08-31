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
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES as SHARED_DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_V1_L1_FINALITY,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE as SHARED_DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  normalizeDeploymentManifestV1JsonValue as normalizeSharedDeploymentManifestV1JsonValue,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueDeploymentInfo,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  referenceScriptAuthUnit,
} from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
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
import {
  buildFraudProofCatalogueDeploymentInfo,
  uint32ToFraudProofID,
} from "@/transactions/initialization.js";

import { TEST_AVAILABILITY_CHALLENGE_V1 } from "./helpers/availability-challenge-v1.js";

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
let CANONICAL_FRAUD_PROOF_CATALOGUE: FraudProofCatalogueDeploymentInfo;
beforeAll(async () => {
  CANONICAL_FRAUD_PROOF_CATALOGUE = await Effect.runPromise(
    buildFraudProofCatalogueDeploymentInfo(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map(
        (categoryName, index) =>
          [
            uint32ToFraudProofID(index),
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
    fraudProofCatalogue: CANONICAL_FRAUD_PROOF_CATALOGUE,
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
    l1Finality: DEPLOYMENT_MANIFEST_V1_L1_FINALITY,
    economics:
      DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
    availabilityChallenge: TEST_AVAILABILITY_CHALLENGE_V1,
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

describe("V1 deployment manifest", () => {
  it("delegates canonical JSON normalization and digesting to core", () => {
    expect(normalizeDeploymentManifestV1JsonValue).toBe(
      normalizeSharedDeploymentManifestV1JsonValue,
    );
    expect(computeDeploymentManifestV1JsonDigest).toBe(
      computeSharedDeploymentManifestV1JsonDigest,
    );
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toEqual(
      SHARED_DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
    );
    expect(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).toEqual(
      SHARED_DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    );
    // Re-derived from the wave-current `midgard-core` roster.
    // `midgard-core/tests/deployment-manifest-identity-v1.test.ts` pins the
    // same three numbers (163 contracts, 156 reference-script roles, 157 auth
    // token names) and is green, and the two assertions above make this
    // package's copies fail closed against it. The state-correction wave is
    // what moved them: `correctionLockSpend`, `claimRegistrySpend`,
    // `availabilityChallengeSpend` and `availabilityChallengeMint` joined the
    // contract vector, and `availability-challenge spending`/`minting` joined
    // the role and token vocabularies.
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toHaveLength(163);
    expect(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_ROLES).toHaveLength(156);
    expect(Object.keys(REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)).toHaveLength(157);
    expect(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER).toHaveLength(32);
  });

  // The golden manifest ID is `sha256` over the canonical stable JSON of the
  // identity above, so it moves whenever the identity's field set moves. The
  // previous value (`58fa9eb4...`, written at `0cecf536`) was reproduced
  // bit-for-bit by reverting exactly three additions, each of which is a
  // deliberate V1 addition and none of which drops or reshapes an existing
  // field:
  //
  //  1. `4a4bc660` added `limits.maxSinglePublicationCompleteItemBytes` (14396)
  //     to `MIDGARD_CONSENSUS_LIMITS_V1` (midgard-core
  //     `src/consensus-profile-v1.ts`), moving `consensusProfile` and
  //     `consensusProfileDigest` (`e81d6fdc...` -> `181730d3...`). This landed
  //     BEFORE `0cecf536`, so `58fa9eb4...` was already stale when written;
  //     `docs/consensus-profile-v1.md` was left stale in the same way and was
  //     only re-synced later at `72dc8ead`.
  //  2. `fraudProofZeroInput` was added to
  //     `DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES`, taking `contracts` from 50 to
  //     51 entries.
  //  3. `zeroInput` was inserted at index 5 of
  //     `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` (midgard-sdk
  //     `src/fraud-proof/catalogue.ts`), which adds a 7th catalogue category
  //     and shifts `validationTraceDispute` from `00000005` to `00000006`.
  //     Merge `baa7e937` also replaced this fixture's hand-written catalogue
  //     (fake root `"33".repeat(32)`, `membershipProofCbor: "80"`) with the
  //     real one derived by `buildFraudProofCatalogueDeploymentInfo`, which is
  //     now mandatory: `verifyDeploymentManifestV1FraudProofCatalogueIdentity`
  //     reconstructs the catalogue MPF root from the categories and verifies
  //     every membership proof, so a fake root no longer parses.
  //  4. Goal task `Q44` appended `fraudProofDaHashPreimage` to
  //     `DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES` (51 -> 52 `contracts` entries)
  //     and `daHashPreimage` to `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` as the
  //     8th category (`00000007`), which also moves the derived catalogue root
  //     and every membership proof. Both are append-only, so no existing
  //     category ID shifts. `68c2a3ae...` -> `a9219993...`.
  //  5. Goal task `C28` appended the `V1 validation-trace CEK direct resolver`
  //     role token (`V1ValidationTraceCekResolver0`) to
  //     `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` so the ~156 KiB applied `cek_v1`
  //     direct resolver is consumed only through an authenticated reference
  //     script. Token names feed the manifest identity, so the pin moved
  //     `a9219993...` -> `c9cb35df...`. The role deliberately does NOT join
  //     `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE`: its
  //     publication exceeds the L1 envelope and stays a fault-proofs
  //     deployment entry (`validationTraceDisputeCekDirectResolver`).
  //  6. Issue #547 registered the three harvested families in one change:
  //     `fraudProofNoReferenceInput`, `fraudProofReferenceInputNoIdx`, and
  //     `fraudProofInvalidSignature` were appended to
  //     `DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES` (52 -> 55 `contracts` entries)
  //     and `noReferenceInput` / `referenceInputNoIdx` / `invalidSignature` to
  //     `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` as categories 9, 10 and 11
  //     (`00000008`, `00000009`, `0000000a`), which also moves the derived
  //     catalogue root and every membership proof. All six additions are
  //     append-only, so no existing category ID shifts.
  //     `c9cb35df...` -> `28ac3909...`.
  //  7. Issue #579 registered the §8.6 field-preimage certificate as a
  //     deployment role. `fieldPreimageCertificateSpend` and
  //     `fieldPreimageCertificateMint` were appended to
  //     `DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES` (55 -> 57 `contracts`
  //     entries) and the roles `V1 field-preimage certificate` and
  //     `V1 field-preimage certificate minting` were added to
  //     `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE` (37 -> 39)
  //     and `..._TOKEN_NAMES` (38 -> 40). #594 retired the receipt family and
  //     gave the tx-order mint and every field-opening step a
  //     `field_preimage_certificate_policy_id` parameter; one deployed
  //     certificate policy serves both readers (§8.6, _Consumption_), so it
  //     takes a role rather than being rederived per load site. Both
  //     additions are append-only, so no existing contract index shifts, and
  //     the catalogue category order is untouched — this family is not a
  //     catalogue category.
  //     `28ac3909...` -> `a5f80592...`.
  //  8. Issue #579, owner rulings of 2026-08-14, REMOVED the three retired
  //     tx-field names in one movement: `txOrderFieldPreimageSpend`,
  //     `txOrderFieldReceiptSpend` and `txOrderFieldReceiptMint` (57 -> 54
  //     `contracts` entries), with their reference-script roles (39 -> 36) and
  //     token names (40 -> 37). This is the first REMOVAL in this chain and so
  //     the first entry that is NOT append-only: every contract after the
  //     removed positions shifts down, which is why the watcher's
  //     position-derived synthetic catalogue moved with it.
  //     Cause: commit df53dc6a7 (#587), executing the owner's 2026-08-10 ruling
  //     that the tx-field receipt chain dies before #579's single blueprint
  //     regeneration, deleted `tx-field-preimage-v1.ak`,
  //     `tx-field-receipt-v1.ak` and its spend twin together; #594 replaced the
  //     tx-order mint's receipt parameters with the §8.6 certificate policy id.
  //     The regenerated blueprint declares none of the three titles, so removing
  //     them here completes off-chain what #587 did on-chain. The catalogue
  //     category order is again untouched — none of the three is a category.
  //     `a5f80592...` -> `d9c811ab...`.
  //  9. Issue #627 (with #618) added ONE compiled constant. 2c7fd3bb put
  //     `coinsPerUtxoByte: 4_310` into `MIDGARD_CONSENSUS_LIMITS_V1` as the
  //     free parameter of the minimum-Ada floor, under the owner's 2026-08-23
  //     option-B ruling that the rate which convicts a block is a redeploy
  //     event rather than a declared block field. The identity embeds
  //     `MIDGARD_CONSENSUS_PROFILE_V1`, so the digest moves with it. Nothing
  //     positional changed: `contracts` stays at 54 entries, the reference
  //     script roles at 36 and the token names at 37, no catalogue category
  //     was added, removed or renumbered, and the derived catalogue root in
  //     this fixture is built from synthetic script bytes and so is untouched
  //     by the blueprint regeneration that carries this re-pin. 2c7fd3bb left
  //     every pin to the single #617 regeneration by design, which is why
  //     this row was already red at a8a2c6c5.
  //     `d9c811ab...` -> `29a6a7e6...`.
  // 10. The complete fault-proof registration wave appended fourteen
  //     catalogue categories (`0000000b` through `00000018`), 53 compiled
  //     contract entries (45 validators across the appended linear families
  //     plus eight transition-trace finals), and 54 authenticated
  //     reference-script roles/tokens (those 53 entries plus the existing
  //     transition-trace route). Existing positions remain unchanged. The
  //     canonical catalogue root/proofs and full manifest identity move with
  //     those append-only ABI vectors.
  //     `29a6a7e6...` -> `b6dfe4d7...`.
  // 11. The 2026-08-28 registration wave appended three catalogue categories
  //     (`valueNotPreserved` `00000019`, `inputSetUniqueness` `0000001a`,
  //     `mintAuthorization` `0000001b`), eleven compiled contract entries
  //     (4 + 2 + 5 step validators), and eleven authenticated
  //     reference-script roles/tokens. Existing positions remain unchanged.
  //     `b6dfe4d7...` -> `acc508eb...`.
  // 12. Native-script-decoding replaced its oversized step-03 identity with
  //     open-subject, bind-descriptor, and advance-or-close identities: two
  //     net-new compiled contracts and authenticated reference-script roles.
  //     `acc508eb...` -> `c598ec8e...`.
  // 13. Q35 appended `networkId` as category `0000001c`, its two compiled
  //     step validators, and their authenticated reference-script roles.
  //     The catalogue root/proofs and manifest identity move while
  //     every previously assigned category and contract position is retained.
  //     `c598ec8e...` -> `4eaa7ee8...`.
  // 14. Q57 made the source-neutral L1 finality/recovery policy an
  //     authenticated manifest root field. `4eaa7ee8...` -> `c6250e32...`.
  // 15. Q50 appended the shared fraud-proof verifier scripts and every missing
  //     legacy family step, and made each one an authenticated reference-script
  //     role. Existing contract positions remain unchanged.
  //     `c6250e32...` -> `a210f82f...`.
  // 16. Release economics is now an exact authenticated root block whose
  //     profile distinguishes public Preprod launch from bounded acceptance.
  //     `a210f82f...` -> `88503dda...`.
  // 17. The state-correction wave registered four new compiled contracts —
  //     `correctionLockSpend` and `claimRegistrySpend` (slotted after
  //     `fraudProofMinAdaStep02`), and `availabilityChallengeSpend` /
  //     `availabilityChallengeMint` (appended) — taking `contracts` from 159 to
  //     163, and gave the availability challenge a reference-script role and
  //     auth token (roles 154 -> 156, token names 155 -> 157). The same wave
  //     appended `missingNativeScriptUtxo`, `nativeScriptInvalid` and `minAda`
  //     to `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` (29 -> 32), which moves the
  //     derived catalogue root and every membership proof. All of that is
  //     already pinned independently by
  //     `midgard-core/tests/deployment-manifest-identity-v1.test.ts`, which is
  //     green; this row was red only because this package's mirrors lagged it.
  //     `88503dda...` -> `919b2d39...`.
  it("accepts the sole exact authenticated V1 manifest", () => {
    expect(canonicalManifest().manifestId).toBe(
      "919b2d3948dca2737e1e8ed51a54ba973fb8fe0f593e1a4c9a5f6613354c3bee",
    );
    expect(parseDeploymentManifestV1Value(canonicalManifest())).toEqual(
      canonicalManifest(),
    );
  });

  it("rejects catalogue root, positional ID, and membership-proof tampering", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    const catalogue =
      identity.contracts.fraudProofCatalogueMint.fraudProofCatalogue!;
    const withCatalogue = (
      fraudProofCatalogue: typeof catalogue,
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

    expect(() =>
      parseDeploymentManifestV1Value(
        withId(
          withCatalogue({
            ...catalogue,
            root: "33".repeat(32),
          }),
        ),
      ),
    ).toThrow(/catalogue root mismatch/u);

    expect(() =>
      parseDeploymentManifestV1Value(
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
      parseDeploymentManifestV1Value(
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

  it("rejects noncanonical release finality even with a recomputed identity", () => {
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    for (const l1Finality of [
      { ...identity.l1Finality, confirmationDepth: 29 },
      { ...identity.l1Finality, automaticRecoveryMaxDepth: 2159 },
      { ...identity.l1Finality, deepRollbackPolicy: "manual-v1" },
    ]) {
      const tampered = { ...identity, l1Finality };
      expect(() =>
        parseDeploymentManifestV1Value({
          ...tampered,
          manifestId: computeDeploymentManifestId(
            tampered as Omit<DeploymentManifestV1Value, "manifestId">,
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
        parseDeploymentManifestV1Value({
          ...tampered,
          manifestId: computeDeploymentManifestId(
            tampered as Omit<DeploymentManifestV1Value, "manifestId">,
          ),
        }),
      ).toThrow(/economics/u);
    }
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
