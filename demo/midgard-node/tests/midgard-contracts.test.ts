import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { it } from "@effect/vitest";
import { mintingPolicyToId } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it as unitIt } from "vitest";

import {
  computeDeploymentManifestId,
  DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  type DeploymentManifestV2Value,
} from "@/deployment-manifest-v2.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import {
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
  readRuntimeDeploymentManifestFile,
  withRealStateQueueAndOperatorContracts,
} from "@/services/midgard-contracts.js";

describe("midgard contracts registry", () => {
  const oneShotOutRef = {
    txHash: "00".repeat(32),
    outputIndex: 0,
  } as const;

  it.effect("resolves real state_queue and hub_oracle scripts", () =>
    Effect.gen(function* () {
      const placeholderContracts = yield* AlwaysSucceedsContract;
      const resolved = yield* withRealStateQueueAndOperatorContracts(
        "Preprod",
        placeholderContracts,
        { ...oneShotOutRef },
        { referenceScriptAuth: placeholderContracts.referenceScriptAuth },
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
        "user_events/tx_order.mint.mint",
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
          { referenceScriptAuth: placeholderContracts.referenceScriptAuth },
        ),
      );
      expect(result._tag).toEqual("Left");
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  unitIt(
    "fails closed for explicit manifests and only permits auto-discovered legacy fallback",
    async () => {
      const dir = await mkdtemp(join(tmpdir(), "midgard-runtime-manifest-"));
      const missingPath = join(dir, "missing.json");
      const legacyPath = join(dir, "legacy.json");
      const tamperedPath = join(dir, "tampered.json");
      try {
        expect(() =>
          readRuntimeDeploymentManifestFile(missingPath, true),
        ).toThrow(/does not exist/);
        await writeFile(
          legacyPath,
          JSON.stringify({ schemaVersion: "legacy", contracts: {} }),
        );
        expect(readRuntimeDeploymentManifestFile(legacyPath, false)).toBe(
          undefined,
        );
        expect(() =>
          readRuntimeDeploymentManifestFile(legacyPath, true),
        ).toThrow(/schemaVersion must be midgard-deployment-manifest-v2/);

        const identityInput: Omit<DeploymentManifestV2Value, "manifestId"> = {
          schemaVersion: DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
          network: "Preprod",
          referenceScriptDeployAddress: "addr_test1reference",
          hubOracleOneShot: {
            txHash: "ab".repeat(32),
            outputIndex: 0,
            outRef: `${"ab".repeat(32)}#0`,
          },
          referenceScriptAuthPolicy: { policyId: "cd".repeat(28) },
          contracts: {},
          referenceScripts: {},
          steps: {},
        };
        await writeFile(
          tamperedPath,
          JSON.stringify({
            ...identityInput,
            manifestId: computeDeploymentManifestId(identityInput),
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
