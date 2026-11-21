/**
 * The prover funding plan reserves the funding address's pure-ADA coins
 * without a reference script, with or without a datum: a tagged refund the
 * chain pays to the funding address is money. A coin holding a token or a
 * reference script is never spent, so it is never reserved as funding
 * either, and neither is anything at the proof-materials address. A wallet
 * with no plain funding beside its collateral is unavailable funding, which
 * the watcher resumes on a later observation. The one exception is an input
 * of a superseded attempt, which its replacement must spend.
 */
import { proofMaterialsAddressOfFundingAddress } from "@al-ft/midgard-fault-proofs/funding/proof-materials-address";
import { WORKFLOW_RUNNER_FACTORIES } from "@al-ft/midgard-fault-proofs/workflow/runtime";
import { createWorkflowRuntimeFundingPolicy } from "@al-ft/midgard-fault-proofs/workflow/runtime-funding-policy";
import { Constr, credentialToAddress, Data } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { calculateWatcherRuntimeProverFunding } from "../../src/funding/prover-funding-calculation.aggregate-watcher-prover-funding-sweep.js";
import { planWatcherProverFundingReservation } from "../../src/funding/prover-funding-reservation.plan-watcher-prover-funding-reservation.js";
import { WatcherProverFundingUnavailableError } from "../../src/funding/prover-funding-reservation.watcher-prover-funding-reservation-store.js";
import { watcherDeploymentReleaseEconomicsAuthority } from "../../src/runtime/deployment-identity.watcher-deployment-availability-challenge-authority.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.build-watcher-deployment-authority-fixture.js";
import { runtimeAuthority } from "./prover-funding-calculation.runtime-authority.js";
import {
  fundingPaymentKeyHash,
  tokenUnit,
  walletAddress,
} from "./prover-funding-calculation.transaction-cbor.js";

const coin = (
  fill: string,
  assets: Record<string, bigint>,
  output: {
    readonly address?: string;
    readonly datum?: string;
    readonly datumHash?: string;
    readonly scriptRef?: { readonly type: "PlutusV3"; readonly script: string };
  } = {},
) => ({
  txHash: fill.repeat(32),
  outputIndex: 0,
  address: walletAddress,
  assets,
  ...output,
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
    supersededInputOutRefs?: readonly string[],
  ) =>
    planWatcherProverFundingReservation({
      deploymentIdentity,
      calculation,
      decisionDigest: "77".repeat(32),
      walletAddress,
      utxos,
      ...(supersededInputOutRefs === undefined
        ? {}
        : { supersededInputOutRefs }),
    });
  const tokenOutRef = `${TOKEN_COIN.txHash}#0`;

  // The largest coin holds a token: only the plain coins are reserved.
  const mixed = plan([TOKEN_COIN, COLLATERAL, PLAIN]);
  expect(mixed.inputs.map(({ outRef }) => outRef)).toEqual([
    `${PLAIN.txHash}#0`,
    `${COLLATERAL.txHash}#0`,
  ]);
  expect(mixed.fundingLovelace).toBe("906000000");
  expect(mixed.assets).toEqual([]);

  // The coin that covers the collateral funds the action too.
  expect(
    plan([TOKEN_COIN, COLLATERAL]).inputs.map(({ outRef }) => outRef),
  ).toEqual([`${COLLATERAL.txHash}#0`]);

  // The token coin never counts toward collateral: without a plain coin that
  // covers it, funding is unavailable until a top-up.
  expect(() => plan([TOKEN_COIN, PLAIN])).toThrow(
    WatcherProverFundingUnavailableError,
  );

  // A Close refund tagged with an inline OutputReference datum and a coin
  // under a datum hash are money of the funding address; a reference-script
  // coin and a proof-materials coin of the same key are not funding.
  const tag = Data.to(new Constr(0, ["ab".repeat(32), 0n]));
  const TAGGED_REFUND = coin("04", { lovelace: 900_000_000n }, { datum: tag });
  const HASHED = coin(
    "05",
    { lovelace: 7_000_000n },
    { datumHash: "cd".repeat(32) },
  );
  const REFERENCE_SCRIPT = coin(
    "06",
    { lovelace: 2_000_000_000n },
    { scriptRef: { type: "PlutusV3", script: "46010000222601" } },
  );
  const MATERIALS = coin(
    "07",
    { lovelace: 3_000_000_000n },
    {
      address: proofMaterialsAddressOfFundingAddress(walletAddress),
      datum: tag,
    },
  );
  const shaped = plan([REFERENCE_SCRIPT, MATERIALS, TAGGED_REFUND, HASHED]);
  expect(shaped.inputs.map(({ outRef }) => outRef)).toEqual([
    `${TAGGED_REFUND.txHash}#0`,
    `${HASHED.txHash}#0`,
  ]);
  expect(shaped.fundingLovelace).toBe("907000000");
  // Without the tagged refund the collateral is not covered.
  expect(() => plan([REFERENCE_SCRIPT, MATERIALS, HASHED])).toThrow(
    WatcherProverFundingUnavailableError,
  );

  // A superseded attempt spent the token coin: its replacement must spend
  // it too, so it stays reserved as funding.
  expect(
    plan([TOKEN_COIN, COLLATERAL, PLAIN], [tokenOutRef]).inputs,
  ).toContainEqual(expect.objectContaining({ outRef: tokenOutRef }));
});
