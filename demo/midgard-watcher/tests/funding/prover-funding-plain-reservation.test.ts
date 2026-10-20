/**
 * The prover funding plan reserves plain-ADA coins only: a coin holding a
 * token is never spent, so it is never reserved as funding either. A wallet
 * with no plain funding beside its collateral is unavailable funding, which
 * the watcher resumes on a later observation. The one exception is an input
 * of a superseded attempt, which its replacement must spend.
 */
import {
  createWorkflowRuntimeFundingPolicy,
  WORKFLOW_RUNNER_FACTORIES,
} from "@al-ft/midgard-fault-proofs";
import { credentialToAddress } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { calculateWatcherRuntimeProverFunding } from "../../src/funding/prover-funding-calculation.js";
import {
  planWatcherProverFundingReservation,
  WatcherProverFundingUnavailableError,
} from "../../src/funding/prover-funding-reservation.js";
import { watcherDeploymentReleaseEconomicsAuthority } from "../../src/runtime/deployment-identity.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import { runtimeAuthority } from "./prover-funding-calculation.runtime-authority.js";
import {
  fundingPaymentKeyHash,
  tokenUnit,
  walletAddress,
} from "./prover-funding-calculation.transaction-cbor.js";

const coin = (fill: string, assets: Record<string, bigint>) => ({
  txHash: fill.repeat(32),
  outputIndex: 0,
  address: walletAddress,
  assets,
});

const TOKEN_COIN = coin("03", { lovelace: 1_100_000_000n, [tokenUnit]: 1n });
const COLLATERAL = coin("02", { lovelace: 900_000_000n });
const PLAIN = coin("01", { lovelace: 6_000_000n });

it("reserves only plain-ADA coins as funding, save a superseded attempt's input", async () => {
  const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
  const protocolParameters = await runtimeAuthority(deploymentIdentity);
  const economics = await watcherDeploymentReleaseEconomicsAuthority(
    deploymentIdentity,
  ).verifyForWorkflow({ deploymentFingerprint: deploymentIdentity.manifestId });
  const calculation = await calculateWatcherRuntimeProverFunding({
    deploymentIdentity,
    protocolParameters,
    policy: createWorkflowRuntimeFundingPolicy({
      category: "doubleSpend",
      runner: WORKFLOW_RUNNER_FACTORIES.doubleSpend(async () => {
        throw new Error("planner must not execute a workflow");
      }),
      deploymentFingerprint: deploymentIdentity.manifestId,
      fundingPaymentKeyHash,
      protocolParameters: protocolParameters.snapshot,
      economics,
      referenceScripts: [],
      contracts: [
        {
          address: credentialToAddress("Preprod", {
            type: "Script",
            hash: "55".repeat(28),
          }),
          scriptHash: "55".repeat(28),
          role: "proof_thread",
        },
      ],
    }),
  });
  const plan = (
    utxos: readonly ReturnType<typeof coin>[],
    avoidCollateralOutRefs?: readonly string[],
  ) =>
    planWatcherProverFundingReservation({
      deploymentIdentity,
      calculation,
      decisionDigest: "77".repeat(32),
      walletAddress,
      utxos,
      ...(avoidCollateralOutRefs === undefined
        ? {}
        : { avoidCollateralOutRefs }),
    });
  const tokenOutRef = `${TOKEN_COIN.txHash}#0`;

  // The larger coin holds a token: only the plain coin funds.
  const mixed = plan([TOKEN_COIN, COLLATERAL, PLAIN]);
  expect(mixed.inputs.map(({ outRef, role }) => [outRef, role])).toEqual([
    [`${PLAIN.txHash}#0`, "funding"],
    [`${COLLATERAL.txHash}#0`, "collateral"],
  ]);
  expect(mixed.fundingLovelace).toBe("6000000");
  expect(mixed.assets).toEqual([]);

  // No plain coin beside the collateral: funding is unavailable until a
  // top-up, rather than a reservation of a coin no transaction spends.
  expect(() => plan([TOKEN_COIN, COLLATERAL])).toThrow(
    WatcherProverFundingUnavailableError,
  );

  // A superseded attempt spent the token coin: its replacement must spend
  // it too, so it stays reserved as funding.
  expect(
    plan([TOKEN_COIN, COLLATERAL, PLAIN], [tokenOutRef]).inputs,
  ).toContainEqual(
    expect.objectContaining({ outRef: tokenOutRef, role: "funding" }),
  );
});
