import { dirname, resolve as resolvePath } from "node:path";
import { fileURLToPath } from "node:url";

import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import { DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import type {
  MidgardValidators,
  ReferenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  referenceScriptAuthTokenName,
} from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import {
  toUnit,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import {
  buildContractDeploymentInfoFromContracts,
  buildContractDeploymentInfoProgram,
  buildDeploymentManifestV1,
  buildReferenceScriptOutRefMap,
  cardanoProtocolParametersIdentityV1FromProvider,
  defaultContractDeploymentInfoOutputPath,
  DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
  type DeploymentManifestBuildContext,
  type DeploymentManifestV1IdentityContext,
  parseDeploymentManifestV1,
  verifyDeploymentManifestAgainstConfig,
} from "../src/commands/contract-deployment-info.js";
import {
  computeDeploymentManifestId,
  computeDeploymentManifestV1DaCommitteeSignersHash,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  normalizeDeploymentManifestV1JsonValue,
} from "../src/deployment-manifest-v1.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  midgardContractsFromDeploymentManifest,
  parseRuntimeDeploymentManifest,
} from "../src/services/midgard-contracts.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "../src/transactions/initialization.js";
import { TEST_AVAILABILITY_CHALLENGE_V1 } from "./helpers/availability-challenge-v1.js";

const testReferenceScriptAuthPolicy = (
  _policyId: string,
  _cborHex: string,
): ReferenceScriptAuthPolicyDeploymentInfo => ({
  policyId: validatorToScriptHash({
    type: "Native",
    script: `8200581c${"00".repeat(28)}`,
  }),
  nativeScript: {
    type: "Native",
    cborHex: `8200581c${"00".repeat(28)}`,
    expiresAtSlot: 0,
    expiresAtUnixTime: 0,
    timelockDurationMs: 1,
  },
  tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  postTimelockAudit: {
    required: true,
    rule: "test fixture",
  },
});

const ONE_SHOT_TX_HASH = "ab".repeat(32);
const TEST_DA_VKEY = "11".repeat(32);
const TEST_CARDANO_PARAMETERS = {
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
const TEST_MANIFEST_IDENTITY_CONTEXT: DeploymentManifestV1IdentityContext = {
  availabilityChallenge: TEST_AVAILABILITY_CHALLENGE_V1,
  economics:
    DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
  cardanoProtocolParameters: {
    snapshot: TEST_CARDANO_PARAMETERS,
    digest: computeDeploymentManifestV1JsonDigest(TEST_CARDANO_PARAMETERS),
  },
  genesis: {
    headerHash: "00".repeat(28),
    utxoSetDigest: computeDeploymentManifestV1JsonDigest(
      normalizeDeploymentManifestV1JsonValue([]),
    ),
  },
  da: {
    committeeVkeys: [TEST_DA_VKEY],
    committeeSignersHash: computeDeploymentManifestV1DaCommitteeSignersHash([
      TEST_DA_VKEY,
    ]),
    threshold: 1,
    transportProfile: {
      protocolVersion: DA_TRANSPORT_V1_PROTOCOL_VERSION,
      runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
      envelopeEncoding: "identity" as const,
      zstdLevel: 3,
      limits: DA_TRANSPORT_LIMITS_V1,
      retentionDays: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
    },
  },
  proofEvidence: {
    digest: null,
    blueprintHash: "22".repeat(32),
  },
};

const testFraudProofCatalogue = (contracts: MidgardValidators) =>
  buildFraudProofCatalogueDeploymentInfo(
    fraudProofsToIndexedValidators(contracts.fraudProofs),
  );

const buildFinalizedContractDeploymentInfo = (
  contracts: MidgardValidators,
  authPolicy: ReferenceScriptAuthPolicyDeploymentInfo,
) =>
  Effect.map(testFraudProofCatalogue(contracts), (fraudProofCatalogue) =>
    buildContractDeploymentInfoFromContracts(
      contracts,
      authPolicy,
      new Map(
        Object.values(
          DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
        ).map((contractName, outputIndex) => [
          contractName,
          { txHash: "33".repeat(32), outputIndex },
        ]),
      ),
      fraudProofCatalogue,
    ),
  );

const TEST_FINALIZED_MANIFEST_BUILD_CONTEXT = {
  network: "Preprod",
  ...TEST_MANIFEST_IDENTITY_CONTEXT,
  referenceScriptDeployAddress: "addr_test1reference",
  hubOracleOneShotTxHash: ONE_SHOT_TX_HASH,
  hubOracleOneShotOutputIndex: 0,
  hubOracleOneShotStatus: "consumed_by_init",
  steps: {
    initProtocol: { status: "complete", txHash: "cd".repeat(32) },
  },
} satisfies DeploymentManifestBuildContext;

describe("contract deployment info", () => {
  it("derives the exact Cardano parameter snapshot and digest from the configured provider", async () => {
    let calls = 0;
    const identity = await cardanoProtocolParametersIdentityV1FromProvider(
      {
        getProtocolParameters: async () => {
          calls += 1;
          return {
            minFeeA: 44,
            minFeeB: 155_381,
            priceMem: 0.0577,
            priceStep: 0.0000721,
            coinsPerUtxoByte: 4_310n,
            collateralPercentage: 150,
            maxCollateralInputs: 3,
            maxTxSize: 16_384,
            maxValSize: 5_000,
            maxTxExMem: 16_500_000n,
            maxTxExSteps: 10_000_000_000n,
            minFeeRefScriptCostPerByte: 15,
          };
        },
      },
      {
        jsonrpc: "2.0",
        id: "fixture",
        result: {
          minFeeCoefficient: 44,
          minFeeConstant: { ada: { lovelace: 155_381 } },
          scriptExecutionPrices: {
            memory: "577/10000",
            cpu: "721/10000000",
          },
          minUtxoDepositCoefficient: 4_310,
          collateralPercentage: 150,
          maxCollateralInputs: 3,
          maxTransactionSize: { bytes: 16_384 },
          maxValueSize: { bytes: 5_000 },
          maxExecutionUnitsPerTransaction: {
            memory: 16_500_000,
            cpu: 10_000_000_000,
          },
          minFeeReferenceScripts: { base: 15, range: 25_600, multiplier: 1.2 },
          maxReferenceScriptsSizePerTransaction: { bytes: 204_800 },
        },
      },
    );
    expect(calls).toBe(1);
    expect(identity.snapshot).toEqual(TEST_CARDANO_PARAMETERS);
    expect(identity.digest).toBe(
      computeDeploymentManifestV1JsonDigest(identity.snapshot),
    );
  });

  it.effect(
    "builds explicit script entries for the current validator bundle",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;
        const authPolicy = testReferenceScriptAuthPolicy(
          contracts.referenceScriptAuth.policyId,
          contracts.referenceScriptAuth.mintingScriptCBOR,
        );
        const manifest = buildContractDeploymentInfoFromContracts(
          contracts,
          authPolicy,
        );

        expect(manifest.referenceScriptAuthPolicy.policyId).toEqual(
          authPolicy.policyId,
        );
        expect(manifest.contracts.hubOracleMint.contract.type).toEqual(
          "PlutusV3",
        );
        expect(manifest.contracts.hubOracleMint.scriptHash).toEqual(
          contracts.hubOracle.policyId,
        );
        expect(manifest.contracts.daAttestationMint.scriptHash).toEqual(
          contracts.daAttestation.policyId,
        );
        expect(manifest.contracts.daParamsGovernorMint.scriptHash).toEqual(
          contracts.daParamsGovernor.policyId,
        );
        expect(manifest.contracts.depositMint.scriptHash).toEqual(
          contracts.deposit.policyId,
        );
        expect(manifest.contracts.depositSpend.scriptHash).toEqual(
          contracts.deposit.spendingScriptHash,
        );
        expect(manifest.contracts.reserveWithdraw.scriptHash).toEqual(
          contracts.reserve.withdrawalScriptHash,
        );
        expect(manifest.contracts.reserveSpend.scriptHash).toEqual(
          contracts.reserve.spendingScriptHash,
        );
        expect(manifest.contracts.payoutSpend.scriptHash).toEqual(
          contracts.payout.spendingScriptHash,
        );
        expect(manifest.contracts.payoutMint.scriptHash).toEqual(
          contracts.payout.policyId,
        );
        expect(manifest.contracts.depositMint.refScriptUTxO).toBeNull();
        expect(
          manifest.contracts.fraudProofInvalidRange.refScriptUTxO,
        ).toBeNull();
        expect(
          manifest.contracts.validationTraceDisputeSource.scriptHash,
        ).toEqual(
          contracts.fraudProofs.validationTraceDispute.source
            .spendingScriptHash,
        );
        expect(
          manifest.contracts.validationTraceDisputeAward.scriptHash,
        ).toEqual(
          contracts.fraudProofs.validationTraceDispute.award.spendingScriptHash,
        );
        expect(
          manifest.contracts.fraudProofMissingSignatureStep04.scriptHash,
        ).toEqual(
          contracts.fraudProofContracts.missingSignature.steps[3]
            .spendingScriptHash,
        );
        expect(
          manifest.contracts.fraudProofMissingNativeScriptTxStep06.scriptHash,
        ).toEqual(
          contracts.fraudProofContracts.missingNativeScriptTx.steps[5]
            .spendingScriptHash,
        );
        expect(
          manifest.contracts.fraudProofMissingNativeScriptTxStep07.scriptHash,
        ).toEqual(
          contracts.fraudProofContracts.missingNativeScriptTx.steps[6]
            .spendingScriptHash,
        );
        expect(
          manifest.contracts.fraudProofMissingNativeScriptTxStep08.scriptHash,
        ).toEqual(
          contracts.fraudProofContracts.missingNativeScriptTx.steps[7]
            .spendingScriptHash,
        );
        expect(
          manifest.contracts.fraudProofTransitionTraceDuplicate.scriptHash,
        ).toEqual(
          contracts.fraudProofContracts.transitionTrace.finals[7]
            .spendingScriptHash,
        );
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("can attach fraud-proof catalogue deployment metadata", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const fraudProofCatalogue = yield* testFraudProofCatalogue(contracts);
      const manifest = buildContractDeploymentInfoFromContracts(
        contracts,
        authPolicy,
        new Map(),
        fraudProofCatalogue,
      );

      expect(
        manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue?.root,
      ).toBe(fraudProofCatalogue.root);
      expect(
        manifest.contracts.fraudProofCatalogueSpend.fraudProofCatalogue,
      ).toBeUndefined();
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect(
    "requires the matching role token before recording a ref script",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;
        const authPolicy = testReferenceScriptAuthPolicy(
          contracts.referenceScriptAuth.policyId,
          contracts.referenceScriptAuth.mintingScriptCBOR,
        );
        const stateQueueMintRef: UTxO = {
          txHash: "01".repeat(32),
          outputIndex: 0,
          address: "addr_test1reference",
          assets: {
            lovelace: 4_000_000n,
            [toUnit(
              authPolicy.policyId,
              referenceScriptAuthTokenName("state-queue minting"),
            )]: 1n,
          },
          scriptRef: contracts.stateQueue.mintingScript,
        };
        const manifest = yield* buildContractDeploymentInfoProgram(
          contracts,
          [stateQueueMintRef],
          authPolicy,
        );

        expect(manifest.contracts.stateQueueMint.refScriptUTxO).toEqual({
          txHash: stateQueueMintRef.txHash,
          outputIndex: stateQueueMintRef.outputIndex,
        });
        expect(manifest.contracts.daAttestationMint.refScriptUTxO).toBeNull();
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("rejects duplicate live UTxOs for one reference-script role", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const roleUnit = toUnit(
        authPolicy.policyId,
        referenceScriptAuthTokenName("state-queue minting"),
      );
      const ref = (txByte: string): UTxO => ({
        txHash: txByte.repeat(32),
        outputIndex: 0,
        address: "addr_test1reference",
        assets: { lovelace: 4_000_000n, [roleUnit]: 1n },
        scriptRef: contracts.stateQueue.mintingScript,
      });
      expect(() =>
        buildReferenceScriptOutRefMap(
          [ref("01"), ref("ff")],
          [
            {
              name: "stateQueueMint",
              script: contracts.stateQueue.mintingScript,
              scriptHash: contracts.stateQueue.policyId,
              contract: {
                type: contracts.stateQueue.mintingScript.type,
                cborHex: contracts.stateQueue.mintingScript.script,
              },
              referenceScriptTargetName: "state-queue minting",
            },
          ],
          authPolicy,
        ),
      ).toThrow(/ambiguous.*exactly one live/u);
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("rejects a role token attached to the wrong reference script", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const roleUnit = toUnit(
        authPolicy.policyId,
        referenceScriptAuthTokenName("state-queue minting"),
      );
      expect(() =>
        buildReferenceScriptOutRefMap(
          [
            {
              txHash: "ff".repeat(32),
              outputIndex: 0,
              address: "addr_test1reference",
              assets: { lovelace: 4_000_000n, [roleUnit]: 1n },
              scriptRef: {
                type: "Native",
                script: `8200581c${"01".repeat(28)}`,
              },
            },
          ],
          [
            {
              name: "stateQueueMint",
              script: contracts.stateQueue.mintingScript,
              scriptHash: contracts.stateQueue.policyId,
              contract: {
                type: contracts.stateQueue.mintingScript.type,
                cborHex: contracts.stateQueue.mintingScript.script,
              },
              referenceScriptTargetName: "state-queue minting",
            },
          ],
          authPolicy,
        ),
      ).toThrow(/script hash mismatch/u);
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("rejects a non-unit reference-script role-token quantity", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const roleUnit = toUnit(
        authPolicy.policyId,
        referenceScriptAuthTokenName("state-queue minting"),
      );
      expect(() =>
        buildReferenceScriptOutRefMap(
          [
            {
              txHash: "02".repeat(32),
              outputIndex: 0,
              address: "addr_test1reference",
              assets: { lovelace: 4_000_000n, [roleUnit]: 2n },
              scriptRef: contracts.stateQueue.mintingScript,
            },
          ],
          [
            {
              name: "stateQueueMint",
              script: contracts.stateQueue.mintingScript,
              scriptHash: contracts.stateQueue.policyId,
              contract: {
                type: contracts.stateQueue.mintingScript.type,
                cborHex: contracts.stateQueue.mintingScript.script,
              },
              referenceScriptTargetName: "state-queue minting",
            },
          ],
          authPolicy,
        ),
      ).toThrow(/must carry exactly one/u);
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("rejects a role token without an attached reference script", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const roleUnit = toUnit(
        authPolicy.policyId,
        referenceScriptAuthTokenName("state-queue minting"),
      );
      expect(() =>
        buildReferenceScriptOutRefMap(
          [
            {
              txHash: "03".repeat(32),
              outputIndex: 0,
              address: "addr_test1reference",
              assets: { lovelace: 4_000_000n, [roleUnit]: 1n },
              scriptRef: undefined,
            },
          ],
          [
            {
              name: "stateQueueMint",
              script: contracts.stateQueue.mintingScript,
              scriptHash: contracts.stateQueue.policyId,
              contract: {
                type: contracts.stateQueue.mintingScript.type,
                cborHex: contracts.stateQueue.mintingScript.script,
              },
              referenceScriptTargetName: "state-queue minting",
            },
          ],
          authPolicy,
        ),
      ).toThrow(/not attached to a reference script/u);
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("rejects a reference UTxO bundling multiple auth-role tokens", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const roleUnit = toUnit(
        authPolicy.policyId,
        referenceScriptAuthTokenName("state-queue minting"),
      );
      const otherRoleUnit = toUnit(
        authPolicy.policyId,
        referenceScriptAuthTokenName("scheduler minting"),
      );
      expect(() =>
        buildReferenceScriptOutRefMap(
          [
            {
              txHash: "01".repeat(32),
              outputIndex: 0,
              address: "addr_test1reference",
              assets: {
                lovelace: 4_000_000n,
                [roleUnit]: 1n,
                [otherRoleUnit]: 1n,
              },
              scriptRef: contracts.stateQueue.mintingScript,
            },
          ],
          [
            {
              name: "stateQueueMint",
              script: contracts.stateQueue.mintingScript,
              scriptHash: contracts.stateQueue.policyId,
              contract: {
                type: contracts.stateQueue.mintingScript.type,
                cborHex: contracts.stateQueue.mintingScript.script,
              },
              referenceScriptTargetName: "state-queue minting",
            },
          ],
          authPolicy,
        ),
      ).toThrow(/must carry no other/u);
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it("defaults init manifest output to the package-root deploymentInfo", () => {
    const packageRoot = resolvePath(
      dirname(fileURLToPath(import.meta.url)),
      "..",
    );
    expect(defaultContractDeploymentInfoOutputPath()).toEqual(
      resolvePath(
        packageRoot,
        "deploymentInfo",
        "contract-deployment-info.json",
      ),
    );
  });

  it.effect(
    "authenticates every canonical V1 manifest field in its identity",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;
        const authPolicy = testReferenceScriptAuthPolicy(
          contracts.referenceScriptAuth.policyId,
          contracts.referenceScriptAuth.mintingScriptCBOR,
        );
        const deploymentInfo = yield* buildFinalizedContractDeploymentInfo(
          contracts,
          authPolicy,
        );

        const first = buildDeploymentManifestV1(deploymentInfo, {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
          now: new Date("2026-06-18T00:00:00.000Z"),
        });
        const second = buildDeploymentManifestV1(deploymentInfo, {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
          now: new Date("2026-06-19T00:00:00.000Z"),
          existingManifest: first,
        });

        expect(first.schemaVersion).toEqual(
          DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
        );
        expect(first.consensusProfile).toEqual(MIDGARD_CONSENSUS_PROFILE_V1);
        expect(first.manifestId).toEqual(second.manifestId);
        expect(second.createdAt).toEqual(first.createdAt);
        expect(second.updatedAt).toEqual(first.updatedAt);
        expect(second.hubOracleOneShot).toMatchObject({
          txHash: ONE_SHOT_TX_HASH,
          outputIndex: 0,
          outRef: `${ONE_SHOT_TX_HASH}#0`,
          status: "consumed_by_init",
        });
        expect(second.steps.initProtocol).toEqual({
          status: "complete",
          txHash: "cd".repeat(32),
        });
        expect(
          second.referenceScripts["state-queue minting"]?.roleUnit,
        ).toContain(authPolicy.policyId);
        expect(parseDeploymentManifestV1(second).manifestId).toEqual(
          second.manifestId,
        );
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect(
    "builds the V1 manifest with the exact contracts and dispute schedule",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;
        const authPolicy = testReferenceScriptAuthPolicy(
          contracts.referenceScriptAuth.policyId,
          contracts.referenceScriptAuth.mintingScriptCBOR,
        );
        const deploymentInfo = yield* buildFinalizedContractDeploymentInfo(
          contracts,
          authPolicy,
        );
        const manifest = buildDeploymentManifestV1(deploymentInfo, {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
          now: new Date("2026-07-23T00:00:00.000Z"),
        });

        expect(manifest.schemaVersion).toEqual(
          MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
        );
        expect(manifest.consensusProfile).toEqual(MIDGARD_CONSENSUS_PROFILE_V1);
        expect(manifest.validationDispute).toEqual({
          version: MIDGARD_CONSENSUS_PROFILE_V1.validationDisputeVersion,
          responseWindowMs:
            MIDGARD_CONSENSUS_PROFILE_V1.limits
              .validationDisputeResponseWindowMs,
          maxBisectionRounds:
            MIDGARD_CONSENSUS_PROFILE_V1.limits.maxValidationBisectionRounds,
          maturityMs: MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs,
        });
        expect(manifest.contracts.validationTraceDispute.scriptHash).toMatch(
          /^[0-9a-f]{56}$/u,
        );
        expect(parseDeploymentManifestV1(manifest).manifestId).toEqual(
          manifest.manifestId,
        );

        const {
          validationTraceDispute: _validationTraceDispute,
          ...withoutValidationDispute
        } = deploymentInfo.contracts;
        expect(() =>
          buildDeploymentManifestV1(
            {
              ...deploymentInfo,
              contracts: withoutValidationDispute,
            },
            {
              ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
            },
          ),
        ).toThrow(/contracts\.validationTraceDispute/);
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("reports manifest/config drift", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const deploymentInfo = yield* buildFinalizedContractDeploymentInfo(
        contracts,
        authPolicy,
      );
      const manifest = buildDeploymentManifestV1(deploymentInfo, {
        ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
      });

      const report = verifyDeploymentManifestAgainstConfig(manifest, {
        network: "Preview",
        referenceScriptDeployAddress: "addr_test1other",
        hubOracleOneShotTxHash: "cd".repeat(32),
        hubOracleOneShotOutputIndex: 1,
        economicsProfile: "public-preprod-launch-v1",
        path: "/tmp/contract-deployment-info.json",
      });

      expect(report.ok).toEqual(false);
      expect(report.recommendation).toEqual("correct_attach_config");
      expect(report.mismatches.join("\n")).toContain("network");
      expect(report.mismatches.join("\n")).toContain(
        "referenceScriptDeployAddress",
      );
      expect(report.mismatches.join("\n")).toContain("hubOracleOneShot.txHash");
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("rejects manifests with a tampered identity hash or profile", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const manifest = buildDeploymentManifestV1(
        yield* buildFinalizedContractDeploymentInfo(contracts, authPolicy),
        {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
        },
      );

      expect(() =>
        parseDeploymentManifestV1({
          ...manifest,
          hubOracleOneShot: {
            ...manifest.hubOracleOneShot,
            outputIndex: 1,
            outRef: `${ONE_SHOT_TX_HASH}#1`,
          },
        }),
      ).toThrow(/Deployment manifest id mismatch/);
      expect(() =>
        parseRuntimeDeploymentManifest({
          ...manifest,
          hubOracleOneShot: {
            ...manifest.hubOracleOneShot,
            outputIndex: 1,
            outRef: `${ONE_SHOT_TX_HASH}#1`,
          },
        }),
      ).toThrow(/Deployment manifest id mismatch/);
      expect(() =>
        parseDeploymentManifestV1({
          ...manifest,
          consensusProfile: {
            ...manifest.consensusProfile,
            protocolVersion: 2,
          },
        }),
      ).toThrow(/consensusProfile/);
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it("rejects unsupported schemas in the runtime contract source", () => {
    expect(() =>
      parseRuntimeDeploymentManifest({
        schemaVersion: "unsupported-contract-deployment-info",
        contracts: {},
      }),
    ).toThrow(/schemaVersion must be midgard-deployment-manifest-v1/);
  });

  it.effect("rejects a deployment manifest with an unknown network", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const manifest = buildDeploymentManifestV1(
        yield* buildFinalizedContractDeploymentInfo(contracts, authPolicy),
        {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
        },
      );

      const { manifestId: _manifestId, ...identityInput } = manifest;
      const invalidNetwork = { ...identityInput, network: "Bogus" };
      expect(() =>
        parseDeploymentManifestV1({
          ...invalidNetwork,
          manifestId: computeDeploymentManifestId(invalidNetwork),
        }),
      ).toThrow(/Mainnet, Preprod, Preview, or Custom/);
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect(
    "reconstructs validators from deployment manifest contract bytes",
    () =>
      Effect.gen(function* () {
        const contracts = yield* AlwaysSucceedsContract;
        const authPolicy = testReferenceScriptAuthPolicy(
          contracts.referenceScriptAuth.policyId,
          contracts.referenceScriptAuth.mintingScriptCBOR,
        );
        const manifest = buildDeploymentManifestV1(
          yield* buildFinalizedContractDeploymentInfo(contracts, authPolicy),
          {
            ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
          },
        );

        const reconstructed = midgardContractsFromDeploymentManifest(
          "Preprod",
          manifest,
          "fixture-contract-deployment-info.json",
          contracts,
        );

        expect(reconstructed.scheduler.spendingScriptHash).toEqual(
          manifest.contracts.schedulerSpend.scriptHash,
        );
        expect(reconstructed.scheduler.spendingScript.script).toEqual(
          manifest.contracts.schedulerSpend.contract.cborHex,
        );
        expect(reconstructed.scheduler.policyId).toEqual(
          manifest.contracts.schedulerMint.scriptHash,
        );
        expect(reconstructed.activeOperators.spendingScriptHash).toEqual(
          manifest.contracts.activeOperatorsSpend.scriptHash,
        );
        expect(reconstructed.fraudProofCatalogue.policyId).toEqual(
          manifest.contracts.fraudProofCatalogueMint.scriptHash,
        );
        expect(
          reconstructed.fraudProofs.transitionTrace.spendingScriptHash,
        ).toEqual(manifest.contracts.fraudProofTransitionTrace.scriptHash);
        expect(
          reconstructed.fraudProofContracts.missingNativeScriptTx.steps.map(
            ({ spendingScriptHash }) => spendingScriptHash,
          ),
        ).toEqual([
          manifest.contracts.fraudProofMissingNativeScriptTx.scriptHash,
          manifest.contracts.fraudProofMissingNativeScriptTxStep02.scriptHash,
          manifest.contracts.fraudProofMissingNativeScriptTxStep03.scriptHash,
          manifest.contracts.fraudProofMissingNativeScriptTxStep04.scriptHash,
          manifest.contracts.fraudProofMissingNativeScriptTxStep05.scriptHash,
          manifest.contracts.fraudProofMissingNativeScriptTxStep06.scriptHash,
          manifest.contracts.fraudProofMissingNativeScriptTxStep07.scriptHash,
          manifest.contracts.fraudProofMissingNativeScriptTxStep08.scriptHash,
        ]);
        expect(
          reconstructed.fraudProofContracts.transitionTrace.finals.map(
            ({ spendingScriptHash }) => spendingScriptHash,
          ),
        ).toEqual([
          manifest.contracts.fraudProofTransitionTraceControl.scriptHash,
          manifest.contracts.fraudProofTransitionTraceSource.scriptHash,
          manifest.contracts.fraudProofTransitionTraceWithdrawal.scriptHash,
          manifest.contracts.fraudProofTransitionTraceForced.scriptHash,
          manifest.contracts.fraudProofTransitionTraceAcceptedTransaction
            .scriptHash,
          manifest.contracts.fraudProofTransitionTraceDeposit.scriptHash,
          manifest.contracts.fraudProofTransitionTraceL1Event.scriptHash,
          manifest.contracts.fraudProofTransitionTraceDuplicate.scriptHash,
        ]);
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("rejects deployment manifest contract hash drift", () =>
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const manifest = buildDeploymentManifestV1(
        yield* buildFinalizedContractDeploymentInfo(contracts, authPolicy),
        {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
        },
      );

      expect(() =>
        midgardContractsFromDeploymentManifest(
          "Preprod",
          {
            ...manifest,
            contracts: {
              ...manifest.contracts,
              schedulerSpend: {
                ...manifest.contracts.schedulerSpend,
                scriptHash: "aa".repeat(28),
              },
            },
          },
          "fixture-contract-deployment-info.json",
          contracts,
        ),
      ).toThrow(/contracts\.schedulerSpend\.scriptHash/);
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );
});
