/**
 * The control a third-party caller's slash runs through before it is
 * submitted: the slash's own fraud-slash authority names the removed header
 * and the authenticated prover's reward, the canonical pair it removes is
 * admitted as a double spend, the slash pays the reward and nothing to the
 * caller, and the caller's wallet holds only plain coins, from which the
 * slash posts its collateral. Once the slash has landed, its reward output
 * stands on chain and a phase-2-valid slash has spent none of the caller's
 * coins.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import { deploymentManifestHubOraclePolicyId } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import {
  credentialToAddress,
  type LucidEvolution,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  type FraudSlashFundingAuthority,
  readFraudSlashFundingAuthority,
} from "../../src/remove-fraudulent-block.js";
import { DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY } from "../../src/workflow/complete-replay.detect-double-withdraws.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "../../src/workflow/header-classifier.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-finality-policy.js";
import {
  type LocallyEvaluatedTransaction,
  workflowTransactionCollateralInputOutRefs,
} from "../../src/workflow/transaction-boundary.js";
import { authenticatedHeaderObservation } from "../helpers/canonical-block-evidence-fixture.js";
import type { bindCanonicalFixtureHeader } from "./submit-init/deployment.js";

type Evidence = Awaited<ReturnType<typeof bindCanonicalFixtureHeader>>;

const assertRemovedHeader = async ({
  authority,
  manifest,
  removedHeader,
  callerLucid,
}: {
  readonly authority: FraudSlashFundingAuthority;
  readonly manifest: Evidence["manifest"];
  readonly removedHeader: SDK.Header;
  readonly callerLucid: LucidEvolution;
}) => {
  const [txHash, index] = authority.removedStateQueueOutRef.split("#");
  const removed = await callerLucid.utxosByOutRef([
    { txHash: txHash!, outputIndex: Number(index) },
  ]);
  expect(removed).toHaveLength(1);
  const policyId = manifest.contracts.stateQueueMint!.scriptHash;
  expect(removed[0]!.address).toBe(
    credentialToAddress(manifest.network, {
      type: "Script",
      hash: manifest.contracts.stateQueueSpend!.scriptHash,
    }),
  );
  const parsed = await Effect.runPromise(
    SDKStateQueue.utxoToStateQueueUTxO(removed[0]!, policyId),
  );
  const actualHeader = await Effect.runPromise(
    SDK.getHeaderFromStateQueueDatum(parsed.datum),
  );
  expect(actualHeader).toEqual(removedHeader);
  const expectedHash = await Effect.runPromise(
    SDK.hashBlockHeader(removedHeader),
  );
  const actualHash = await Effect.runPromise(SDK.hashBlockHeader(actualHeader));
  expect(actualHash).toBe(expectedHash);
  expect(
    removed[0]!.assets[
      toUnit(policyId, SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + expectedHash)
    ],
  ).toBe(1n);
};

/** Checks the signed slash `boundary` before it is submitted; `confirm`
 * checks it once it has landed. */
export const preparePermissionlessSlashControl = async ({
  evidence,
  removedHeader,
  callerLucid,
  boundary,
}: {
  readonly evidence: Evidence;
  readonly removedHeader: SDK.Header;
  readonly callerLucid: LucidEvolution;
  readonly boundary: LocallyEvaluatedTransaction;
}) => {
  const authority = readFraudSlashFundingAuthority(boundary.signed);
  if (authority === null)
    throw new Error("Canonical builder did not mint its live slash authority");
  const { manifest } = evidence;
  expect(authority.headerHash).toBe(evidence.headerHash);
  expect(authority.hubOraclePolicyId).toBe(
    deploymentManifestHubOraclePolicyId(manifest),
  );
  await assertRemovedHeader({
    authority,
    manifest,
    removedHeader,
    callerLucid,
  });
  const observation = authenticatedHeaderObservation(evidence);
  const classifier = await createHeaderClassifier({
    hubOraclePolicyId: deploymentManifestHubOraclePolicyId(manifest),
    replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority: {
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      verifyForWorkflow: async () => ({
        schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
        hubOraclePolicyId: deploymentManifestHubOraclePolicyId(manifest),
        blueprintHash: manifest.artifacts.blueprintHash,
        policyDigest: computeFraudProofReleaseFinalityPolicyDigest(
          manifest.l1Finality,
        ),
        policy: manifest.l1Finality,
      }),
    },
  });
  const decision = await classifyHeader({
    classifier,
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: manifest.l1Finality.confirmationDepth,
      }),
    sources: [
      {
        sourceId: "permissionless-retained-canonical-pair",
        fetchPayloadByHeaderHash: async () => ({
          ok: true as const,
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: "permissionless-retained-canonical-pair/fixture",
            grade: "security" as const,
          },
          sourceId: "permissionless-retained-canonical-pair",
          sourcePeerId: "fixture",
          payloadEnvelopeCbor: evidence.payloadEnvelopeCbor,
          attempts: [],
        }),
      },
    ],
  });
  if (
    decision.decision !== "fault_detected" ||
    decision.category !== "doubleSpend"
  )
    throw new Error("Actual canonical pair was not admitted as doubleSpend");
  expect(decision.headerHash).toBe(evidence.headerHash);
  const collateral = workflowTransactionCollateralInputOutRefs(boundary.signed);
  const walletAddress = await callerLucid.wallet().address();
  expect(authority.rewardAddress).not.toBe(walletAddress);
  const outputs = boundary.signed.toTransaction().body().outputs();
  const outputList = Array.from({ length: outputs.len() }, (_, index) =>
    outputs.get(index),
  );
  const rewardIndices = outputList.flatMap((output, index) =>
    output.address().to_bech32() === authority.rewardAddress ? [index] : [],
  );
  expect(rewardIndices).toHaveLength(1);
  const rewardIndex = rewardIndices[0]!;
  expect(outputList[rewardIndex]!.amount().coin().toString()).toBe(
    authority.rewardLovelace,
  );
  expect(
    outputList.some((output) => output.address().to_bech32() === walletAddress),
  ).toBe(false);
  const walletCoins = (await callerLucid.wallet().getUtxos())
    .map((utxo) => {
      expect(utxo.address).toBe(walletAddress);
      expect(Object.keys(utxo.assets)).toEqual(["lovelace"]);
      expect(utxo.datum).toBeUndefined();
      expect(utxo.scriptRef).toBeUndefined();
      return outRefLabel(utxo);
    })
    .sort((a, b) => a.localeCompare(b, "en"));
  // Every coin the slash posts as collateral is one of the caller's coins.
  expect(collateral.filter((outRef) => !walletCoins.includes(outRef))).toEqual(
    [],
  );
  return {
    confirm: async () => {
      const rewardOutRef = {
        txHash: boundary.signed.toHash(),
        outputIndex: rewardIndex,
      };
      const [reward] = await callerLucid.utxosByOutRef([rewardOutRef]);
      expect(reward?.address).toBe(authority.rewardAddress);
      expect(reward?.assets).toEqual({
        lovelace: BigInt(authority.rewardLovelace),
      });
      // A phase-2-valid slash spends none of the caller's coins: the ones it
      // posted as collateral are still there.
      expect(
        (
          await callerLucid.utxosByOutRef(
            walletCoins.map((outRef) => {
              const [txHash, index] = outRef.split("#");
              return { txHash: txHash!, outputIndex: Number(index) };
            }),
          )
        )
          .map(outRefLabel)
          .sort((a, b) => a.localeCompare(b, "en")),
      ).toEqual(walletCoins);
    },
  };
};
