import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import { DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { REFERENCE_SCRIPT_AUTH_TOKEN_NAMES } from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import {
  mintingPolicyToId,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it as unitIt } from "vitest";

import {
  buildContractDeploymentInfoFromContracts,
  buildDeploymentManifestV1,
  type DeploymentManifestV1IdentityContext,
} from "@/commands/contract-deployment-info.js";
import {
  computeDeploymentManifestV1DaCommitteeSignersHash,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  normalizeDeploymentManifestV1JsonValue,
} from "@/deployment-manifest-v1.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import {
  assertDeploymentManifestMatchesConfig,
  buildRealTxOrderContracts,
  readRuntimeDeploymentManifestFile,
  REAL_ACTIVE_OPERATORS_SCRIPT_TITLES,
  REAL_DEPOSIT_SCRIPT_TITLES,
  REAL_HUB_ORACLE_SCRIPT_TITLES,
  REAL_PAYOUT_SCRIPT_TITLES,
  REAL_REGISTERED_OPERATORS_SCRIPT_TITLES,
  REAL_RESERVE_SCRIPT_TITLES,
  REAL_RETIRED_OPERATORS_SCRIPT_TITLES,
  REAL_SETTLEMENT_SCRIPT_TITLES,
  REAL_STATE_QUEUE_SCRIPT_TITLES,
  REAL_TX_ORDER_SCRIPT_TITLES,
  REAL_WITHDRAWAL_SCRIPT_TITLES,
  withRealStateQueueAndOperatorContracts,
} from "@/services/midgard-contracts.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "@/transactions/initialization.js";

import {
  TEST_AVAILABILITY_CHALLENGE_V1,
  TEST_AVAILABILITY_PARAMETERS_V1,
} from "./helpers/availability-challenge-v1.js";
import { TEST_CARDANO_PROTOCOL_PARAMETERS_V1 } from "./helpers/cardano-protocol-parameters-v1.js";

describe("midgard contracts registry", () => {
  const oneShotOutRef = {
    txHash: "00".repeat(32),
    outputIndex: 0,
  } as const;
  const cardanoSnapshot = TEST_CARDANO_PROTOCOL_PARAMETERS_V1;
  const daVkey = "11".repeat(32);
  const manifestIdentityContext: DeploymentManifestV1IdentityContext = {
    availabilityChallenge: TEST_AVAILABILITY_CHALLENGE_V1,
    economics:
      DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
    cardanoProtocolParameters: {
      snapshot: cardanoSnapshot,
      digest: computeDeploymentManifestV1JsonDigest(cardanoSnapshot),
    },
    genesis: {
      headerHash: "00".repeat(28),
      utxoSetDigest: computeDeploymentManifestV1JsonDigest(
        normalizeDeploymentManifestV1JsonValue([]),
      ),
    },
    da: {
      committeeVkeys: [daVkey],
      committeeSignersHash: computeDeploymentManifestV1DaCommitteeSignersHash([
        daVkey,
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

  it.effect("resolves real state_queue and hub_oracle scripts", () =>
    Effect.gen(function* () {
      const placeholderContracts = yield* AlwaysSucceedsContract;
      const resolved = yield* withRealStateQueueAndOperatorContracts(
        "Preprod",
        placeholderContracts,
        { ...oneShotOutRef },
        {
          referenceScriptAuth: placeholderContracts.referenceScriptAuth,
          availabilityChallengeParameters: TEST_AVAILABILITY_PARAMETERS_V1,
        },
      );

      expect(REAL_HUB_ORACLE_SCRIPT_TITLES.mint).toBe("hub_oracle.mint.mint");
      expect(REAL_STATE_QUEUE_SCRIPT_TITLES.spend).toBe(
        "state_queue.spend.spend",
      );
      expect(REAL_STATE_QUEUE_SCRIPT_TITLES.mint).toBe("state_queue.mint.mint");
      expect(REAL_DEPOSIT_SCRIPT_TITLES.mint).toBe(
        "user_events/deposit.mint.mint",
      );
      expect(REAL_REGISTERED_OPERATORS_SCRIPT_TITLES.mint).toBe(
        "operator_directory/registered_operators.mint.mint",
      );
      expect(REAL_ACTIVE_OPERATORS_SCRIPT_TITLES.mint).toBe(
        "operator_directory/active_operators.mint.mint",
      );
      expect(REAL_RETIRED_OPERATORS_SCRIPT_TITLES.mint).toBe(
        "operator_directory/retired_operators.mint.mint",
      );
      expect(REAL_TX_ORDER_SCRIPT_TITLES.mint).toBe(
        "user_events/tx_order_v1.mint.mint",
      );
      // #579 ruling A: the receipt titles are gone. The certificate titles are
      // what the tx-order family now resolves alongside the mint, so they are
      // what this pin has to hold.
      expect(REAL_TX_ORDER_SCRIPT_TITLES.fieldPreimageCertificateMint).toBe(
        "field_preimage_certificate.field_preimage_certificate.mint",
      );
      expect(REAL_WITHDRAWAL_SCRIPT_TITLES.mint).toBe(
        "user_events/withdrawal.mint.mint",
      );
      expect(REAL_SETTLEMENT_SCRIPT_TITLES.mint).toBe("settlement.mint.mint");
      expect(REAL_RESERVE_SCRIPT_TITLES.spend).toBe("reserve.spend.spend");
      expect(REAL_RESERVE_SCRIPT_TITLES.withdraw).toBe("reserve.withdraw.else");
      expect(REAL_PAYOUT_SCRIPT_TITLES.mint).toBe("payout.mint.mint");
      expect(REAL_PAYOUT_SCRIPT_TITLES.spend).toBe("payout.spend.spend");

      expect(resolved.hubOracle.mintingScriptCBOR).not.toEqual(
        placeholderContracts.hubOracle.mintingScriptCBOR,
      );
      expect(resolved.hubOracle.policyId).toEqual(
        mintingPolicyToId(resolved.hubOracle.mintingScript),
      );

      expect(resolved.stateQueue.spendingScriptCBOR).not.toEqual(
        placeholderContracts.stateQueue.spendingScriptCBOR,
      );
      expect(resolved.stateQueue.mintingScriptCBOR).not.toEqual(
        placeholderContracts.stateQueue.mintingScriptCBOR,
      );
      expect(resolved.stateQueue.policyId).toEqual(
        mintingPolicyToId(resolved.stateQueue.mintingScript),
      );

      expect(resolved.registeredOperators.policyId).not.toEqual(
        placeholderContracts.registeredOperators.policyId,
      );
      expect(resolved.activeOperators.policyId).not.toEqual(
        placeholderContracts.activeOperators.policyId,
      );
      expect(resolved.retiredOperators.policyId).not.toEqual(
        placeholderContracts.retiredOperators.policyId,
      );

      expect(resolved.deposit.policyId).not.toEqual(
        placeholderContracts.deposit.policyId,
      );
      expect(resolved.txOrder.policyId).not.toEqual(
        placeholderContracts.txOrder.policyId,
      );
      expect(resolved.withdrawal.policyId).not.toEqual(
        placeholderContracts.withdrawal.policyId,
      );
      expect(resolved.settlement.policyId).not.toEqual(
        placeholderContracts.settlement.policyId,
      );
      expect(resolved.scheduler.policyId).not.toEqual(
        placeholderContracts.scheduler.policyId,
      );
      expect(resolved.payout.policyId).not.toEqual(
        placeholderContracts.payout.policyId,
      );
      expect(resolved.payout.spendingScriptCBOR).not.toEqual(
        placeholderContracts.payout.spendingScriptCBOR,
      );
      expect(resolved.reserve.spendingScriptCBOR).not.toEqual(
        placeholderContracts.reserve.spendingScriptCBOR,
      );
      expect(resolved.reserve.withdrawalScriptCBOR).not.toEqual(
        placeholderContracts.reserve.withdrawalScriptCBOR,
      );
      expect(resolved.fraudProofs.doubleSpend.spendingScriptCBOR).not.toEqual(
        placeholderContracts.fraudProofs.doubleSpend.spendingScriptCBOR,
      );
      expect(
        resolved.fraudProofs.transitionTrace.spendingScriptCBOR,
      ).not.toEqual(
        placeholderContracts.fraudProofs.transitionTrace.spendingScriptCBOR,
      );
      expect(
        resolved.fraudProofs.nonExistentInput.spendingScriptCBOR,
      ).not.toEqual(
        placeholderContracts.fraudProofs.nonExistentInput.spendingScriptCBOR,
      );
      expect(resolved.fraudProofs.zeroInput.spendingScriptCBOR).not.toEqual(
        placeholderContracts.fraudProofs.zeroInput.spendingScriptCBOR,
      );
      expect(resolved.fraudProofContracts.missingSignature.steps).toHaveLength(
        4,
      );
      // Eight, matching the eight `V1 fraud-proof missing-native-script-tx
      // step-0N` roles the canonical manifest role map declares (step-07 and
      // step-08 are the later additions this pin had not caught up with).
      expect(
        resolved.fraudProofContracts.missingNativeScriptTx.steps,
      ).toHaveLength(8);
      expect(resolved.fraudProofContracts.withdrawalMistag.steps).toHaveLength(
        5,
      );
      expect(resolved.fraudProofContracts.withdrawnInput.steps).toHaveLength(3);
      expect(resolved.fraudProofContracts.transitionTrace.finals).toHaveLength(
        8,
      );
      expect(
        resolved.fraudProofs.missingSignature.spendingScriptCBOR,
      ).not.toEqual(
        placeholderContracts.fraudProofs.missingSignature.spendingScriptCBOR,
      );
      expect(
        resolved.fraudProofs.crossBlockDuplicateEvent.spendingScriptCBOR,
      ).not.toEqual(
        placeholderContracts.fraudProofs.crossBlockDuplicateEvent
          .spendingScriptCBOR,
      );
      const validationControlHashes = [
        resolved.fraudProofs.validationTraceDispute.spendingScriptHash,
        resolved.fraudProofs.validationTraceDispute.source.spendingScriptHash,
        resolved.fraudProofs.validationTraceDispute.game.spendingScriptHash,
        resolved.fraudProofs.validationTraceDispute.boundary.spendingScriptHash,
        resolved.fraudProofs.validationTraceDispute.timeout.spendingScriptHash,
        resolved.fraudProofs.validationTraceDispute.award.spendingScriptHash,
      ];
      expect(new Set(validationControlHashes).size).toEqual(6);

      const txOrderContracts = yield* buildRealTxOrderContracts(
        "Preprod",
        resolved.hubOracle.policyId,
      );
      expect(txOrderContracts.txOrder.policyId).toEqual(
        mintingPolicyToId(txOrderContracts.txOrder.mintingScript),
      );
      expect(txOrderContracts.fieldPreimageCertificate.policyId).toEqual(
        mintingPolicyToId(
          txOrderContracts.fieldPreimageCertificate.mintingScript,
        ),
      );
      expect(
        txOrderContracts.fieldPreimageCertificate.spendingScriptHash,
      ).toHaveLength(56);
      expect(
        txOrderContracts.cekProgramMaterial.spendingScriptHash,
      ).toHaveLength(56);
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("rejects invalid one-shot hub-oracle outref configuration", () =>
    Effect.gen(function* () {
      const placeholderContracts = yield* AlwaysSucceedsContract;
      const result = yield* Effect.either(
        withRealStateQueueAndOperatorContracts(
          "Preprod",
          placeholderContracts,
          {
            txHash: "zz",
            outputIndex: -1,
          },
          {
            referenceScriptAuth: placeholderContracts.referenceScriptAuth,
            availabilityChallengeParameters: TEST_AVAILABILITY_PARAMETERS_V1,
          },
        ),
      );
      expect(result._tag).toEqual("Left");
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  unitIt(
    "fails closed for every existing non-V1 or tampered manifest",
    async () => {
      const dir = await mkdtemp(join(tmpdir(), "midgard-runtime-manifest-"));
      const missingPath = join(dir, "missing.json");
      const unsupportedPath = join(dir, "unsupported.json");
      const tamperedPath = join(dir, "tampered.json");
      try {
        expect(() =>
          readRuntimeDeploymentManifestFile(missingPath, true),
        ).toThrow(/does not exist/);
        await writeFile(
          unsupportedPath,
          JSON.stringify({ schemaVersion: "unsupported", contracts: {} }),
        );
        expect(() =>
          readRuntimeDeploymentManifestFile(unsupportedPath, false),
        ).toThrow(/schemaVersion must be midgard-deployment-manifest-v1/);
        expect(() =>
          readRuntimeDeploymentManifestFile(unsupportedPath, true),
        ).toThrow(/schemaVersion must be midgard-deployment-manifest-v1/);

        const contracts = await Effect.runPromise(
          AlwaysSucceedsContract.pipe(
            Effect.provide(AlwaysSucceedsContract.Default),
          ),
        );
        const nativeScriptCbor = `8200581c${"00".repeat(28)}`;
        const authPolicy = {
          policyId: validatorToScriptHash({
            type: "Native" as const,
            script: nativeScriptCbor,
          }),
          nativeScript: {
            type: "Native" as const,
            cborHex: nativeScriptCbor,
            expiresAtSlot: 1,
            expiresAtUnixTime: 1,
            timelockDurationMs: 1,
          },
          tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
          postTimelockAudit: {
            required: true as const,
            rule: "test fixture",
          },
        };
        const referenceScriptOutRefs = new Map(
          Object.values(
            DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
          ).map((contractName, outputIndex) => [
            contractName,
            { txHash: "33".repeat(32), outputIndex },
          ]),
        );
        const fraudProofCatalogue = await Effect.runPromise(
          buildFraudProofCatalogueDeploymentInfo(
            fraudProofsToIndexedValidators(contracts.fraudProofs),
          ),
        );
        const deploymentInfo = buildContractDeploymentInfoFromContracts(
          contracts,
          authPolicy,
          referenceScriptOutRefs,
          fraudProofCatalogue,
        );
        const manifest = buildDeploymentManifestV1(deploymentInfo, {
          network: "Preprod",
          ...manifestIdentityContext,
          referenceScriptDeployAddress: "addr_test1reference",
          hubOracleOneShotTxHash: "ab".repeat(32),
          hubOracleOneShotOutputIndex: 0,
          hubOracleOneShotStatus: "consumed_by_init",
          now: new Date("2026-07-24T00:00:00.000Z"),
          steps: {
            initProtocol: { status: "complete" },
          },
        });
        const commonConfig = {
          NETWORK: "Preprod" as const,
          L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: "addr_test1reference",
          HUB_ORACLE_ONE_SHOT_TX_HASH: "ab".repeat(32),
          HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: 0,
        };
        expect(() =>
          assertDeploymentManifestMatchesConfig(manifest, tamperedPath, {
            ...commonConfig,
            MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE: "public-preprod-launch-v1",
            OPERATOR_REQUIRED_BOND_LOVELACE: 100_000_000_000n,
            OPERATOR_SLASHING_PENALTY_LOVELACE: 25_000_000_000n,
          }),
        ).toThrow(
          /economics\.profile manifest=bounded-acceptance-v1 config=public-preprod-launch-v1; economics\.requiredBondLovelace manifest=900000000 config=100000000000; economics\.slashingPenaltyLovelace manifest=500000000 config=25000000000/u,
        );

        const publicManifest = buildDeploymentManifestV1(deploymentInfo, {
          network: "Preprod",
          ...manifestIdentityContext,
          economics:
            DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE[
              "public-preprod-launch-v1"
            ],
          referenceScriptDeployAddress: "addr_test1reference",
          hubOracleOneShotTxHash: "ab".repeat(32),
          hubOracleOneShotOutputIndex: 0,
          hubOracleOneShotStatus: "consumed_by_init",
          now: new Date("2026-07-24T00:00:00.000Z"),
          steps: { initProtocol: { status: "complete" } },
        });
        expect(() =>
          assertDeploymentManifestMatchesConfig(publicManifest, tamperedPath, {
            ...commonConfig,
            MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE: "bounded-acceptance-v1",
            OPERATOR_REQUIRED_BOND_LOVELACE: 900_000_000n,
            OPERATOR_SLASHING_PENALTY_LOVELACE: 500_000_000n,
          }),
        ).toThrow(
          /economics\.profile manifest=public-preprod-launch-v1 config=bounded-acceptance-v1; economics\.requiredBondLovelace manifest=100000000000 config=900000000; economics\.slashingPenaltyLovelace manifest=25000000000 config=500000000/u,
        );
        await writeFile(
          tamperedPath,
          JSON.stringify({
            ...manifest,
            network: "Preview",
          }),
        );
        expect(() =>
          readRuntimeDeploymentManifestFile(tamperedPath, false),
        ).toThrow(/Deployment manifest id mismatch/);
      } finally {
        await rm(dir, { recursive: true });
      }
    },
  );
});
