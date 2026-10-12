import { dirname, resolve as resolvePath } from "node:path";
import { fileURLToPath } from "node:url";

import { type CborInput, CborTag, encodeCbor } from "@al-ft/l1-node-transport";
import { MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION } from "@al-ft/midgard-core/consensus-profile";
import { referenceScriptAuthTokenName } from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import {
  toUnit,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it as unitIt } from "vitest";

import {
  buildContractDeploymentInfoFromContracts,
  buildContractDeploymentInfoProgram,
  buildDeploymentManifest,
  buildReferenceScriptOutRefMap,
  cardanoProtocolParametersIdentity,
  cardanoProtocolParametersIdentityFromLedger,
  defaultContractDeploymentInfoOutputPath,
  DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  ledgerProtocolParametersReader,
  parseDeploymentManifest,
  verifyDeploymentManifestAgainstConfig,
} from "../src/commands/contract-deployment-info.js";
import {
  isRecordedValidationTraceSemantic,
  VALIDATION_TRACE_SEMANTIC_KEYS,
} from "../src/deployable-scripts.js";
import {
  computeDeploymentManifestId,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
} from "../src/deployment-manifest.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  midgardContractsFromDeploymentManifest,
  parseRuntimeDeploymentManifest,
} from "../src/services/midgard-contracts.js";
import { nodeRuntimeReferenceScriptTargets } from "../src/transactions/reference-scripts.js";
import {
  buildFinalizedContractDeploymentInfo,
  ONE_SHOT_TX_HASH,
  TEST_CARDANO_PARAMETERS,
  TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
  testFraudProofCatalogue,
  testReferenceScriptAuthPolicy,
  validatorHashesByPath,
} from "./contract-deployment-info.validator-hashes-by-path.js";
import { withRealEventHistoryForTest } from "./helpers/event-history.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

describe("contract deployment info", () => {
  unitIt(
    "derives the exact Cardano parameter snapshot and digest from the ledger's protocol parameters",
    async () => {
      const rational = (numerator: bigint, denominator: bigint) =>
        new CborTag(30n, [numerator, denominator]);
      const fields: CborInput[] = Array.from({ length: 31 }, () => 0n);
      fields[0] = 44n;
      fields[1] = 155_381n;
      fields[3] = 16_384n;
      fields[14] = 4_310n;
      fields[16] = [rational(577n, 10_000n), rational(721n, 10_000_000n)];
      fields[17] = [16_500_000n, 10_000_000_000n];
      fields[19] = 5_000n;
      fields[20] = 150n;
      fields[21] = 3n;
      fields[30] = rational(15n, 1n);
      let calls = 0;
      const identity = await cardanoProtocolParametersIdentityFromLedger(
        async () => {
          calls += 1;
          return encodeCbor(fields);
        },
      );
      expect(calls).toBe(1);
      expect(identity.snapshot).toEqual(TEST_CARDANO_PARAMETERS);
      expect(identity).toEqual(
        cardanoProtocolParametersIdentity(TEST_CARDANO_PARAMETERS),
      );
    },
  );

  unitIt(
    "refuses a Lucid service without the ledger's protocol-parameter reader",
    () => {
      expect(() => ledgerProtocolParametersReader({})).toThrow(
        /no reader of the local node's protocol parameters/,
      );
    },
  );

  it.effect(
    "builds explicit script entries for the current validator bundle",
    () =>
      Effect.gen(function* () {
        const contracts = withRealEventHistoryForTest(
          yield* AlwaysSucceedsContract,
          { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
        );
        const authPolicy = testReferenceScriptAuthPolicy(
          contracts.referenceScriptAuth.policyId,
          contracts.referenceScriptAuth.mintingScriptCBOR,
        );
        const manifest = buildContractDeploymentInfoFromContracts(
          contracts,
          authPolicy,
        );

        for (const [category, entry] of [
          ["fabricatedDeposit", "fraudProofFabricatedDeposit"],
          ["fabricatedWithdrawal", "fraudProofFabricatedWithdrawal"],
        ] as const) {
          expect(manifest.contracts[entry].eventHistoryBounds).toEqual({
            inlineLimitBytes: "512",
            maxPayloadBytes: "5000",
            maxPayloadNodes: "512",
          });
          expect(
            manifest.contracts[entry].eventHistoryRetentionAddress,
          ).toEqual(
            contracts.fraudProofContracts[category].history.retentionAddress,
          );
        }

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
          manifest.contracts.fraudProofWithdrawalMistagStep05.scriptHash,
        ).toEqual(
          contracts.fraudProofContracts.withdrawalMistag.steps[4]
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
      const contracts = withRealEventHistoryForTest(
        yield* AlwaysSucceedsContract,
        { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
      );
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
        const contracts = withRealEventHistoryForTest(
          yield* AlwaysSucceedsContract,
          { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
        );
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
      const contracts = withRealEventHistoryForTest(
        yield* AlwaysSucceedsContract,
        { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
      );
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
      const contracts = withRealEventHistoryForTest(
        yield* AlwaysSucceedsContract,
        { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
      );
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
      const contracts = withRealEventHistoryForTest(
        yield* AlwaysSucceedsContract,
        { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
      );
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
      const contracts = withRealEventHistoryForTest(
        yield* AlwaysSucceedsContract,
        { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
      );
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
      const contracts = withRealEventHistoryForTest(
        yield* AlwaysSucceedsContract,
        { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
      );
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
        const contracts = withRealEventHistoryForTest(
          yield* AlwaysSucceedsContract,
          { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
        );
        const authPolicy = testReferenceScriptAuthPolicy(
          contracts.referenceScriptAuth.policyId,
          contracts.referenceScriptAuth.mintingScriptCBOR,
        );
        const deploymentInfo = yield* buildFinalizedContractDeploymentInfo(
          contracts,
          authPolicy,
        );

        const first = buildDeploymentManifest(deploymentInfo, {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
          now: new Date("2026-06-18T00:00:00.000Z"),
        });
        const second = buildDeploymentManifest(deploymentInfo, {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
          now: new Date("2026-06-19T00:00:00.000Z"),
          existingManifest: first,
        });

        expect(first.schemaVersion).toEqual(DEPLOYMENT_MANIFEST_SCHEMA_VERSION);
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
        expect(parseDeploymentManifest(second).manifestId).toEqual(
          second.manifestId,
        );
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect(
    "builds the V1 manifest with the exact contracts and dispute schedule",
    () =>
      Effect.gen(function* () {
        const contracts = withRealEventHistoryForTest(
          yield* AlwaysSucceedsContract,
          { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
        );
        const authPolicy = testReferenceScriptAuthPolicy(
          contracts.referenceScriptAuth.policyId,
          contracts.referenceScriptAuth.mintingScriptCBOR,
        );
        const deploymentInfo = yield* buildFinalizedContractDeploymentInfo(
          contracts,
          authPolicy,
        );
        const manifest = buildDeploymentManifest(deploymentInfo, {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
          now: new Date("2026-07-23T00:00:00.000Z"),
        });

        expect(manifest.schemaVersion).toEqual(
          MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
        );
        expect(manifest.contracts.validationTraceDispute.scriptHash).toMatch(
          /^[0-9a-f]{56}$/u,
        );
        expect(parseDeploymentManifest(manifest).manifestId).toEqual(
          manifest.manifestId,
        );

        const {
          validationTraceDispute: _validationTraceDispute,
          ...withoutValidationDispute
        } = deploymentInfo.contracts;
        expect(() =>
          buildDeploymentManifest(
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
      const contracts = withRealEventHistoryForTest(
        yield* AlwaysSucceedsContract,
        { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
      );
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const deploymentInfo = yield* buildFinalizedContractDeploymentInfo(
        contracts,
        authPolicy,
      );
      const manifest = buildDeploymentManifest(deploymentInfo, {
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
      const contracts = withRealEventHistoryForTest(
        yield* AlwaysSucceedsContract,
        { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
      );
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const manifest = buildDeploymentManifest(
        yield* buildFinalizedContractDeploymentInfo(contracts, authPolicy),
        {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
        },
      );

      expect(() =>
        parseDeploymentManifest({
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
        parseDeploymentManifest({
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
      const contracts = withRealEventHistoryForTest(
        yield* AlwaysSucceedsContract,
        { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
      );
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const manifest = buildDeploymentManifest(
        yield* buildFinalizedContractDeploymentInfo(contracts, authPolicy),
        {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
        },
      );

      const { manifestId: _manifestId, ...identityInput } = manifest;
      const invalidNetwork = { ...identityInput, network: "Bogus" };
      expect(() =>
        parseDeploymentManifest({
          ...invalidNetwork,
          manifestId: computeDeploymentManifestId(invalidNetwork),
        }),
      ).toThrow(
        "Deployment profile, digest, and network must match the compiled profile",
      );
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect(
    "reconstructs validators from deployment manifest contract bytes",
    () =>
      Effect.gen(function* () {
        const contracts = withRealEventHistoryForTest(
          yield* AlwaysSucceedsContract,
          { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
        );
        const authPolicy = testReferenceScriptAuthPolicy(
          contracts.referenceScriptAuth.policyId,
          contracts.referenceScriptAuth.mintingScriptCBOR,
        );
        const manifest = buildDeploymentManifest(
          yield* buildFinalizedContractDeploymentInfo(contracts, authPolicy),
          {
            ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
          },
        );

        const reconstructed = midgardContractsFromDeploymentManifest(
          "Preprod",
          manifest,
          "fixture-contract-deployment-info.json",
        );

        for (const category of [
          "fabricatedDeposit",
          "fabricatedWithdrawal",
        ] as const) {
          expect(reconstructed.fraudProofContracts[category].history).toEqual(
            contracts.fraudProofContracts[category].history,
          );
        }

        for (const [field, name] of [
          ["forcedStep", "fraudProofMissingSignatureForcedStep"],
          ["forcedSigner", "fraudProofMissingSignatureForcedSigner"],
          ["forcedWitness", "fraudProofMissingSignatureForcedWitness"],
        ] as const) {
          expect(
            reconstructed.fraudProofContracts.missingSignature[field]
              .spendingScriptHash,
          ).toEqual(manifest.contracts[name].scriptHash);
          expect(
            reconstructed.fraudProofContracts.missingSignature[field]
              .spendingScript.script,
          ).toEqual(manifest.contracts[name].contract.cborHex);
        }

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
          reconstructed.fraudProofContracts.withdrawalMistag.steps.map(
            ({ spendingScriptHash }) => spendingScriptHash,
          ),
        ).toEqual([
          manifest.contracts.fraudProofWithdrawalMistag.scriptHash,
          manifest.contracts.fraudProofWithdrawalMistagStep02.scriptHash,
          manifest.contracts.fraudProofWithdrawalMistagStep03.scriptHash,
          manifest.contracts.fraudProofWithdrawalMistagStep04.scriptHash,
          manifest.contracts.fraudProofWithdrawalMistagStep05.scriptHash,
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

  unitIt(
    "restores every applied validator the real bundle carries",
    async () => {
      const contracts = await loadRealMidgardContractsForTest({
        txHash: ONE_SHOT_TX_HASH,
        outputIndex: 0,
      });
      const manifest = buildDeploymentManifest(
        await Effect.runPromise(
          buildFinalizedContractDeploymentInfo(
            contracts,
            testReferenceScriptAuthPolicy(
              contracts.referenceScriptAuth.policyId,
              contracts.referenceScriptAuth.mintingScriptCBOR,
            ),
          ),
        ),
        { ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT },
      );
      const reconstructed = midgardContractsFromDeploymentManifest(
        "Preprod",
        manifest,
        "fixture-contract-deployment-info.json",
      );

      const restored = validatorHashesByPath(reconstructed);
      const real = validatorHashesByPath(contracts);
      expect(real.failClosed).toEqual([]);

      // The validation-trace members the manifest records no bytes for fail
      // closed on read rather than borrowing a locally built script.
      const vtd = "$.fraudProofContracts.validationTraceDispute";
      const unrecorded = [
        ...VALIDATION_TRACE_SEMANTIC_KEYS.flatMap((key, index) =>
          isRecordedValidationTraceSemantic(key)
            ? []
            : [`${vtd}.semanticResolvers.${index.toString()}`],
        ),
        `${vtd}.steps`,
        `${vtd}.prepareResolvers`,
        `${vtd}.resolvers`,
      ];
      expect([...restored.failClosed].sort()).toEqual([...unrecorded].sort());

      // The SDK builder spreads untyped copies of the top-level shared
      // validators into the chain record. Each copy must equal its top-level
      // path, which is compared below.
      const sharedCopy =
        /^\$\.fraudProofContracts\.(computationThread|fieldPreimageCertificate|fraudProof)(\..+)$/u;
      for (const [path, hash] of Object.entries(real.hashes)) {
        const [, shared, field] = sharedCopy.exec(path) ?? [];
        if (shared !== undefined && field !== undefined)
          expect({ path, hash: real.hashes[`$.${shared}${field}`] }).toEqual({
            path,
            hash,
          });
      }

      // A validator the manifest loader forgets leaves a hole at its path, and
      // one it takes from anywhere but the manifest carries another hash.
      const comparable = (hashes: Record<string, string>) =>
        Object.fromEntries(
          Object.entries(hashes).filter(
            ([path]) =>
              !sharedCopy.test(path) &&
              !unrecorded.some(
                (member) => path === member || path.startsWith(`${member}.`),
              ),
          ),
        );
      expect(comparable(restored.hashes)).toEqual({
        ...comparable(real.hashes),
        // The fixture publishes under a substitute native auth policy.
        "$.referenceScriptAuth.policyId":
          manifest.referenceScriptAuthPolicy.policyId,
      });
    },
  );

  unitIt(
    "restores every node-runtime reference script from the manifest, never from the base bundle",
    async () => {
      const contracts = await loadRealMidgardContractsForTest({
        txHash: ONE_SHOT_TX_HASH,
        outputIndex: 0,
      });
      const manifest = buildDeploymentManifest(
        await Effect.runPromise(
          buildFinalizedContractDeploymentInfo(
            contracts,
            testReferenceScriptAuthPolicy(
              contracts.referenceScriptAuth.policyId,
              contracts.referenceScriptAuth.mintingScriptCBOR,
            ),
          ),
        ),
        { ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT },
      );
      // The loader once filled every validation-trace role outside the six
      // control scripts from a caller-supplied base bundle. The node runtime
      // supplies the always-succeeds stubs there, so 193 published roles
      // resolved to scripts the deployment never published and the
      // reference-script reconcile reported them all missing. Every target
      // the node publishes or reconciles must hash to what the manifest
      // records for its role.
      const restored = midgardContractsFromDeploymentManifest(
        "Preprod",
        manifest,
        "fixture-contract-deployment-info.json",
      );

      const targets = nodeRuntimeReferenceScriptTargets(restored);
      expect(targets.map(({ name }) => name).sort()).toEqual(
        Object.keys(manifest.referenceScripts).sort(),
      );
      const divergent = targets.flatMap(({ name, script }) => {
        const recorded = manifest.referenceScripts[name]?.scriptHash;
        const contract =
          DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
            name as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
          ];
        const restoredHash = validatorToScriptHash(script);
        return recorded !== undefined &&
          restoredHash === recorded &&
          manifest.contracts[contract]?.scriptHash === recorded
          ? []
          : [name];
      });
      expect(divergent).toEqual([]);

      // The hub oracle witness is the one-shot mint policy's own bytes, never
      // a spend script borrowed from elsewhere.
      expect(validatorToScriptHash(restored.hubOracle.spendingScript)).toBe(
        restored.hubOracle.spendingScriptHash,
      );
      expect(restored.hubOracle.spendingScriptHash).toBe(
        manifest.contracts.hubOracleMint.scriptHash,
      );
    },
  );

  it.effect(
    "publishes and restores the network-id forced (wrongful-rejection) step and scan",
    () =>
      Effect.gen(function* () {
        const contracts = withRealEventHistoryForTest(
          yield* AlwaysSucceedsContract,
          { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
        );
        const authPolicy = testReferenceScriptAuthPolicy(
          contracts.referenceScriptAuth.policyId,
          contracts.referenceScriptAuth.mintingScriptCBOR,
        );
        const deploymentInfo = yield* buildFinalizedContractDeploymentInfo(
          contracts,
          authPolicy,
        );
        const manifest = buildDeploymentManifest(deploymentInfo, {
          ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
        });
        const reconstructed = midgardContractsFromDeploymentManifest(
          "Preprod",
          manifest,
          "fixture-contract-deployment-info.json",
        );

        // `buildNetworkIdChain` keeps the forced door and the resumable output
        // scan out of `steps`, so each can only reach the manifest through its
        // own canonical name. Without those entries the deployment publishes
        // no reference script for the forced leg and the wrongful-rejection
        // proof cannot be submitted at all.
        const auxiliaryContracts = [
          [
            "fraudProofNetworkIdForcedStep",
            "V1 fraud-proof network-id forced step",
            contracts.fraudProofContracts.networkId.forcedStep,
            reconstructed.fraudProofContracts.networkId.forcedStep,
          ],
          [
            "fraudProofNetworkIdForcedScan",
            "V1 fraud-proof network-id forced scan",
            contracts.fraudProofContracts.networkId.forcedScan,
            reconstructed.fraudProofContracts.networkId.forcedScan,
          ],
        ] as const;
        for (const [
          contractName,
          role,
          applied,
          restored,
        ] of auxiliaryContracts) {
          const record = manifest.contracts[contractName];
          expect(record).toBeDefined();
          expect(record.scriptHash).toEqual(applied.spendingScriptHash);
          expect(record.contract.cborHex).toEqual(
            applied.spendingScript.script,
          );
          expect(record.refScriptUTxO).not.toBeNull();
          expect(manifest.referenceScripts[role]).toMatchObject({
            status: "confirmed",
          });
          expect(restored.spendingScriptHash).toEqual(record.scriptHash);
          expect(restored.spendingScript.script).toEqual(
            record.contract.cborHex,
          );
        }

        expect(
          reconstructed.fraudProofContracts.networkId.steps.map(
            ({ spendingScriptHash }) => spendingScriptHash,
          ),
        ).toEqual([
          manifest.contracts.fraudProofNetworkId.scriptHash,
          manifest.contracts.fraudProofNetworkIdStep02.scriptHash,
        ]);
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  /**
   * Everything above runs on the AlwaysSucceeds stand-in, where every
   * fault-proof step shares one script: an ordered `[step01, step02, ...]`
   * comparison against manifest roles is satisfied by ANY permutation there,
   * so the step-index-to-role wiring is not actually discriminated. This case
   * repeats those comparisons against the real applied blueprint, where each
   * step has its own hash, and asserts the distinctness that makes the
   * comparison meaningful in the first place.
   */
  unitIt(
    "discriminates step-index-to-role wiring against the real blueprint",
    async () => {
      const contracts = await loadRealMidgardContractsForTest({
        txHash: ONE_SHOT_TX_HASH,
        outputIndex: 0,
      });
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const manifest = buildDeploymentManifest(
        await Effect.runPromise(
          buildFinalizedContractDeploymentInfo(contracts, authPolicy),
        ),
        { ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT },
      );
      const reconstructed = midgardContractsFromDeploymentManifest(
        "Preprod",
        manifest,
        "fixture-contract-deployment-info.json",
      );

      const expectOrderedDistinctWiring = (
        label: string,
        appliedHashes: readonly string[],
        manifestHashes: readonly string[],
      ) => {
        // Without this, the ordered comparison below cannot tell a correct
        // wiring from a permuted one.
        expect(
          new Set(appliedHashes).size,
          `${label}: step scripts must be pairwise distinct`,
        ).toEqual(appliedHashes.length);
        expect(appliedHashes, label).toEqual(manifestHashes);
      };

      expectOrderedDistinctWiring(
        "withdrawalMistag.steps",
        reconstructed.fraudProofContracts.withdrawalMistag.steps.map(
          ({ spendingScriptHash }) => spendingScriptHash,
        ),
        [
          manifest.contracts.fraudProofWithdrawalMistag.scriptHash,
          manifest.contracts.fraudProofWithdrawalMistagStep02.scriptHash,
          manifest.contracts.fraudProofWithdrawalMistagStep03.scriptHash,
          manifest.contracts.fraudProofWithdrawalMistagStep04.scriptHash,
          manifest.contracts.fraudProofWithdrawalMistagStep05.scriptHash,
        ],
      );

      expectOrderedDistinctWiring(
        "transitionTrace.finals",
        reconstructed.fraudProofContracts.transitionTrace.finals.map(
          ({ spendingScriptHash }) => spendingScriptHash,
        ),
        [
          manifest.contracts.fraudProofTransitionTraceControl.scriptHash,
          manifest.contracts.fraudProofTransitionTraceSource.scriptHash,
          manifest.contracts.fraudProofTransitionTraceWithdrawal.scriptHash,
          manifest.contracts.fraudProofTransitionTraceForced.scriptHash,
          manifest.contracts.fraudProofTransitionTraceAcceptedTransaction
            .scriptHash,
          manifest.contracts.fraudProofTransitionTraceDeposit.scriptHash,
          manifest.contracts.fraudProofTransitionTraceL1Event.scriptHash,
          manifest.contracts.fraudProofTransitionTraceDuplicate.scriptHash,
        ],
      );

      expectOrderedDistinctWiring(
        "networkId.steps",
        reconstructed.fraudProofContracts.networkId.steps.map(
          ({ spendingScriptHash }) => spendingScriptHash,
        ),
        [
          manifest.contracts.fraudProofNetworkId.scriptHash,
          manifest.contracts.fraudProofNetworkIdStep02.scriptHash,
        ],
      );
    },
    600_000,
  );

  it.effect("rejects deployment manifest contract hash drift", () =>
    Effect.gen(function* () {
      const contracts = withRealEventHistoryForTest(
        yield* AlwaysSucceedsContract,
        { txHash: ONE_SHOT_TX_HASH, outputIndex: 0 },
      );
      const authPolicy = testReferenceScriptAuthPolicy(
        contracts.referenceScriptAuth.policyId,
        contracts.referenceScriptAuth.mintingScriptCBOR,
      );
      const manifest = buildDeploymentManifest(
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
        ),
      ).toThrow(/contracts\.schedulerSpend\.scriptHash/);
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );
});
