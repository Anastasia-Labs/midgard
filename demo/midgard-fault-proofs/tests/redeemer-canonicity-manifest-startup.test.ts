import { readFile } from "node:fs/promises";
import { join } from "node:path";

import {
  credentialToAddress,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { parseContractDeploymentInfo } from "../src/inspect-contracts.js";
import { REDEEMER_CANONICITY_BLUEPRINT_TITLES } from "../src/redeemer-canonicity/contracts.js";
import {
  createManifestBoundRedeemerCanonicityWorkflow,
  type ManifestBoundRedeemerCanonicityWorkflowConfig,
} from "../src/redeemer-canonicity/runtime.js";

// This gate consumes a real finalized deployment. Manifest verification and
// applied contracts are never mocked; startup cannot perform live I/O.
const runDirectory = process.env.MIDGARD_FINALIZED_RUN_DIR;

describe.skipIf(runDirectory === undefined)(
  "redeemerCanonicity actual finalized-manifest startup",
  () => {
    it("binds three applied steps and refuses substituted reference identities", async () => {
      const [manifestJson, blueprintJson, deploymentInfoJson] =
        await Promise.all([
          readFile(join(runDirectory!, "deploymentInfo/manifest.json"), "utf8"),
          readFile(join(runDirectory!, "deploymentInfo/plutus.json"), "utf8"),
          readFile(
            join(
              runDirectory!,
              "work/journeys/transition-trace/contract-deployment-info.json",
            ),
            "utf8",
          ),
        ]);
      const manifest = JSON.parse(manifestJson) as {
        network: Network;
        manifestId: string;
        referenceScriptDeployAddress: string;
      };
      const deploymentInfo: unknown = JSON.parse(deploymentInfoJson);
      const entries = parseContractDeploymentInfo(deploymentInfo);
      const reference = (name: string): UTxO => {
        const entry = entries[name]!;
        if (entry.refScriptUTxO == null || entry.contract == null)
          throw new Error(`retained deployment is missing ${name}`);
        return {
          ...entry.refScriptUTxO,
          address: manifest.referenceScriptDeployAddress,
          assets: {},
          scriptRef: {
            type: entry.contract.type,
            script: entry.contract.cborHex,
          },
        };
      };
      const forbiddenIo = vi.fn((): never => {
        throw new Error("startup attempted live I/O");
      });
      const paymentKeyHash = "12".repeat(28);
      const config: ManifestBoundRedeemerCanonicityWorkflowConfig = {
        manifest,
        blueprintJson,
        deploymentInfo,
        headerHash: "34".repeat(28),
        decisionDigest: "56".repeat(32),
        lucid: new Proxy({} as LucidEvolution, { get: forbiddenIo }),
        signer: {
          source: "startup-test-public-credential",
          paymentKeyHash,
          address: credentialToAddress(manifest.network, {
            type: "Key",
            hash: paymentKeyHash,
          }),
          selectWallet: forbiddenIo,
        },
        source: {
          sourceId: "startup-test-no-network",
          kupoHttpUrl: "http://127.0.0.1:1",
          ogmiosUrl: "ws://127.0.0.1:1",
          fetchImpl: forbiddenIo,
          webSocketFactory: forbiddenIo,
        },
        stateQueueMutationLeaseCoordinator: { acquire: forbiddenIo },
        referenceScripts: {
          steps: [
            reference("fraudProofRedeemerCanonicity"),
            reference("fraudProofRedeemerCanonicityStep02"),
            reference("fraudProofRedeemerCanonicityStep03"),
          ],
          witnesses: {
            computationThreadMint: reference("computationThreadMint"),
            fraudProofMint: reference("fraudProofMint"),
            phasMembershipWithdraw: reference("phasMembershipWithdraw"),
            chunkedVerifyWithdraw: reference("chunkedVerifyWithdraw"),
            pexcludesWithdraw: reference("pexcludesWithdraw"),
          },
          fieldPreimageCertificateMint: reference(
            "fieldPreimageCertificateMint",
          ),
          removal: {
            correctionLockSpend: reference("correctionLockSpend"),
            stateQueueSpend: reference("stateQueueSpend"),
            stateQueueMint: reference("stateQueueMint"),
            stateQueueFraudRemovalWithdraw: reference(
              "stateQueueFraudRemovalWithdraw",
            ),
            activeOperatorsSpend: reference("activeOperatorsSpend"),
            activeOperatorsMint: reference("activeOperatorsMint"),
            retiredOperatorsSpend: reference("retiredOperatorsSpend"),
            retiredOperatorsMint: reference("retiredOperatorsMint"),
            schedulerSpend: reference("schedulerSpend"),
          },
        },
      };
      const workflow =
        await createManifestBoundRedeemerCanonicityWorkflow(config);
      expect(workflow.binding.deploymentFingerprint).toBe(manifest.manifestId);
      expect(workflow.binding.definition.computationThread.steps).toHaveLength(
        3,
      );
      expect(
        workflow.contracts.steps.map((step) => step.blueprintTitle),
      ).toEqual(REDEEMER_CANONICITY_BLUEPRINT_TITLES);
      expect(
        workflow.contracts.steps.map((step) => step.spendingScriptHash),
      ).toEqual([
        entries.fraudProofRedeemerCanonicity!.scriptHash,
        entries.fraudProofRedeemerCanonicityStep02!.scriptHash,
        entries.fraudProofRedeemerCanonicityStep03!.scriptHash,
      ]);
      const [first, second, third] = config.referenceScripts.steps;
      await expect(
        createManifestBoundRedeemerCanonicityWorkflow({
          ...config,
          referenceScripts: {
            ...config.referenceScripts,
            steps: [second, first, third],
          },
        }),
      ).rejects.toThrow(
        /reference UTxO differs from finalized manifest identity/u,
      );
      await expect(
        createManifestBoundRedeemerCanonicityWorkflow({
          ...config,
          referenceScripts: {
            ...config.referenceScripts,
            steps: [{ ...first, scriptRef: second.scriptRef }, second, third],
          },
        }),
      ).rejects.toThrow(
        /reference UTxO script differs from finalized manifest identity/u,
      );
      expect(forbiddenIo).not.toHaveBeenCalled();
    }, 120_000);
  },
);
