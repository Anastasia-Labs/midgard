/** A validation-trace dispute's measured funding profile sizes the
 * reservation for VerifySource's challenger bond, while the funding permit
 * admits only the deployment-bound bond: the calculation admits a profile
 * whose measured bond is the signed deployment's and refuses one that
 * measured any other. The profile here is synthetic; the category has no
 * signed measured profile yet. */
import {
  createWorkflowFundingRequirements,
  unsafeCreateMeasuredWorkflowRunnerForTest,
  workflowFundingRequirementsForRunner,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { calculateWatcherProverFunding } from "../../src/funding/prover-funding-calculation.js";
import {
  watcherDeploymentAvailabilityChallengeAuthority,
  watcherDeploymentReleaseEconomicsAuthority,
} from "../../src/runtime/deployment-identity.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import { runtimeAuthority } from "./prover-funding-calculation.runtime-authority.js";

const CATEGORY = "validationTraceDispute" as const;
const FEE = 200_000n;
const CHANGE = 3_000_000n;

const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x46));
const fundingPaymentKeyHash = key.to_public().hash().to_hex();
const walletAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(key.to_public().hash()),
)
  .to_address()
  .to_bech32();
/** The dispute-bond contract the measured VerifySource locked its bond at. */
const bondAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_script(CML.ScriptHash.from_hex("b5".repeat(28))),
)
  .to_address()
  .to_bech32();
const FUNDING_OUT_REF = `${"66".repeat(32)}#0`;

/** A signed VerifySource-shaped transaction: change and a `bond` output. */
const measuredVerifySource = (bond: bigint) => {
  const inputLovelace = FEE + CHANGE + bond;
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("66".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(walletAddress),
      CML.Value.from_coin(CHANGE),
    ),
  );
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(bondAddress),
      CML.Value.from_coin(bond),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, FEE);
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      key.to_public(),
      key.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  return {
    actionKind: "verify_source",
    signedTransactionCborHex: CML.Transaction.new(
      body,
      witnesses,
      true,
      undefined,
    ).to_canonical_cbor_hex(),
    fundingControlledInputs: [
      {
        outRef: FUNDING_OUT_REF,
        resolvedOutputCborHex: CML.TransactionOutput.new(
          CML.Address.from_bech32(walletAddress),
          CML.Value.from_coin(inputLovelace),
        ).to_canonical_cbor_hex(),
        role: "wallet_funding" as const,
        semanticRole: "wallet_funding" as const,
        contractAddress: walletAddress,
        identityAssets: [],
        fundingLovelace: inputLovelace.toString(),
        fundingAssets: [],
        sourceActionKind: null,
        sourceOutputIndex: null,
      },
    ],
    fundingControlledOutputs: [
      {
        outputIndex: 0,
        role: "wallet_change" as const,
        custodyRole: "none" as const,
        semanticRole: "wallet_change" as const,
        contractAddress: walletAddress,
        fundingLovelace: CHANGE.toString(),
        fundingAssets: [],
      },
      {
        outputIndex: 1,
        role: "locked_permanent" as const,
        custodyRole: "bond" as const,
        semanticRole: "challenger_bond" as const,
        contractAddress: bondAddress,
        fundingLovelace: bond.toString(),
        fundingAssets: [],
      },
    ],
    referenceInputs: [],
    referenceScriptBytes: 0,
    requiredBondLovelace: bond.toString(),
    requiredRewardCustodyLovelace: "0",
    requiredNativeAssets: [],
    collateralRequired: false,
    conflictRetryCount: 0,
  };
};

/** The calculation over a profile whose VerifySource measured `bond`. */
const calculate = async (bond: (deployment: bigint) => bigint) => {
  const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
  const deployment = BigInt(
    watcherDeploymentAvailabilityChallengeAuthority(deploymentIdentity)
      .parameters.challengerBondLovelace,
  );
  const protocolParameters = await runtimeAuthority(deploymentIdentity);
  const economics = await watcherDeploymentReleaseEconomicsAuthority(
    deploymentIdentity,
  ).verifyForWorkflow({
    deploymentFingerprint: deploymentIdentity.manifestId,
  });
  const runner = unsafeCreateMeasuredWorkflowRunnerForTest({
    category: CATEGORY,
    fundingRequirements: createWorkflowFundingRequirements({
      scope: { kind: "fraud_proof_category", category: CATEGORY },
      deploymentFingerprint: deploymentIdentity.manifestId,
      blueprintSha256: "22".repeat(32),
      protocolParametersDigest: protocolParameters.snapshotDigest,
      economicsPolicyDigest: economics.policyDigest,
      fundingPaymentKeyHash,
      measurementToolVersion: "midgard-cardano-transaction-measurer-v1",
      measurementArtifactSha256: "55".repeat(32),
      actions: [measuredVerifySource(bond(deployment))],
    }),
  });
  return {
    deployment,
    calculation: calculateWatcherProverFunding({
      deploymentIdentity,
      protocolParameters,
      requirements: workflowFundingRequirementsForRunner({
        category: CATEGORY,
        runner,
      }),
    }),
  };
};

describe("validation-trace dispute measured challenger bond", () => {
  it("admits a measured VerifySource bond equal to the deployment-bound bond", async () => {
    const { deployment, calculation } = await calculate((bound) => bound);
    expect((await calculation).totals.requiredBondLovelace).toBe(
      deployment.toString(),
    );
  });

  it("refuses a measured VerifySource bond one lovelace over the deployment-bound bond", async () => {
    const { deployment, calculation } = await calculate((bound) => bound + 1n);
    await expect(calculation).rejects.toThrow(
      `verify_source measured validation dispute bond differs from the deployment-bound bond: measured=${(deployment + 1n).toString()} deployment=${deployment.toString()}`,
    );
  });
});
