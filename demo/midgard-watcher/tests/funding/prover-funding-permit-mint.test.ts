import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import {
  createWorkflowFundingRequirements,
  unsafeCreateMeasuredWorkflowRunnerForTest,
  type WorkflowActuationPermit,
  type WorkflowAdapterRunner,
} from "@al-ft/midgard-fault-proofs";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import { unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest } from "../../src/funding/prover-funding.js";
import {
  createWatcherProverFundingAuthorityFactory,
  type WatcherProverFundingAuthorityFactory,
} from "../../src/funding/prover-funding-authority.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "../../src/funding/prover-funding-reservation.js";
import { openWatcherSqliteProverFundingReservationStore } from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import { watcherDeploymentReleaseEconomicsAuthority } from "../../src/runtime/deployment-identity.js";
import {
  mintWatcherProverFundingReservationPermit,
  type WatcherProverFundingUtxoProvider,
} from "../../src/runtime/watcher-runtime.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

const directories: string[] = [];

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (directory) =>
        rm(directory, { recursive: true, force: true }),
      ),
  );
});

const ogmiosParameters = () => ({
  minFeeCoefficient: 44,
  minFeeConstant: { ada: { lovelace: 155381 } },
  scriptExecutionPrices: { memory: "577/10000", cpu: "721/10000000" },
  minUtxoDepositCoefficient: 4310,
  collateralPercentage: 150,
  maxCollateralInputs: 3,
  maxTransactionSize: { bytes: 16384 },
  maxValueSize: { bytes: 5000 },
  maxExecutionUnitsPerTransaction: {
    memory: 16_500_000,
    cpu: 10_000_000_000,
  },
  minFeeReferenceScripts: {
    base: 15,
    range: 25_600,
    multiplier: 1.2,
  },
  maxReferenceScriptsSizePerTransaction: { bytes: 204_800 },
});

const fetchImpl = vi.fn(
  async (_url: string | URL | Request, init?: RequestInit) => {
    const request = JSON.parse(String(init?.body)) as { readonly id: string };
    return new Response(
      JSON.stringify({
        jsonrpc: "2.0",
        id: request.id,
        result: ogmiosParameters(),
      }),
      { status: 200, headers: { "content-type": "application/json" } },
    );
  },
) as unknown as typeof fetch;

const structuralRunner = Object.freeze({
  runnerVersion: "midgard-production-workflow-adapter-runner-v1",
  runOrResume: async () => {
    throw new Error("test runner must not execute");
  },
}) as unknown as WorkflowAdapterRunner;

const structuralActuationPermit = Object.freeze({
  permitVersion: "midgard-production-workflow-actuation-permit-v1",
}) as WorkflowActuationPermit;

const provider: WatcherProverFundingUtxoProvider = Object.freeze({
  getUtxos: async () => [],
  getUtxosByOutRef: async () => [],
});

const DECISION_DIGEST = "cd".repeat(32);

const mint = (input: {
  readonly factory: WatcherProverFundingAuthorityFactory;
  readonly runner?: WorkflowAdapterRunner;
  readonly walletAddress?: string;
  readonly provider?: WatcherProverFundingUtxoProvider;
}) =>
  mintWatcherProverFundingReservationPermit({
    category: "doubleSpend",
    runner: input.runner ?? structuralRunner,
    factory: input.factory,
    actuationPermit: structuralActuationPermit,
    rollbackGeneration: "0",
    decisionDigest: DECISION_DIGEST,
    walletAddress:
      input.walletAddress ??
      "addr_test1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg57c2qv",
    provider: input.provider ?? provider,
  });

// A wallet whose payment key hash is the one the measured funding profile is
// bound to, so the profile, the policy and the reservation all agree.
const signingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x44));
const fundingPaymentKeyHash = signingKey.to_public().hash().to_hex();
const fundedWalletAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(signingKey.to_public().hash()),
)
  .to_address()
  .to_bech32();
const protocolAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(
    CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x45)).to_public().hash(),
  ),
)
  .to_address()
  .to_bech32();

/** One protocol-funded removal transaction with a collateral requirement. */
const removalTransactionCbor = (): string => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("66".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(fundedWalletAddress),
      CML.Value.from_coin(3_000_000n),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  const collateralInputs = CML.TransactionInputList.new();
  collateralInputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("88".repeat(32)), 0n),
  );
  body.set_collateral_inputs(collateralInputs);
  body.set_total_collateral(5_000_000n);
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      signingKey.to_public(),
      signingKey.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  return CML.Transaction.new(
    body,
    witnesses,
    true,
    undefined,
  ).to_canonical_cbor_hex();
};

const measuredDoubleSpendRunner = async (
  deploymentIdentity: ReturnType<
    typeof makeWatcherDeploymentAuthorityFixture
  >["result"],
  protocolParametersDigest: string,
): Promise<WorkflowAdapterRunner> => {
  const economics = await watcherDeploymentReleaseEconomicsAuthority(
    deploymentIdentity,
  ).verifyForWorkflow({
    deploymentFingerprint: deploymentIdentity.manifestId,
  });
  return unsafeCreateMeasuredWorkflowRunnerForTest({
    category: "doubleSpend",
    fundingRequirements: createWorkflowFundingRequirements({
      scope: { kind: "fraud_proof_category", category: "doubleSpend" },
      deploymentFingerprint: deploymentIdentity.manifestId,
      blueprintSha256: "22".repeat(32),
      protocolParametersDigest,
      economicsPolicyDigest: economics.policyDigest,
      fundingPaymentKeyHash,
      measurementToolVersion: "midgard-cardano-transaction-measurer-v1",
      measurementArtifactSha256: "55".repeat(32),
      actions: [
        {
          actionKind: "remove",
          signedTransactionCborHex: removalTransactionCbor(),
          fundingControlledInputs: [
            {
              outRef: `${"66".repeat(32)}#0`,
              resolvedOutputCborHex: CML.TransactionOutput.new(
                CML.Address.from_bech32(protocolAddress),
                CML.Value.from_coin(3_200_000n),
              ).to_canonical_cbor_hex(),
              role: "protocol",
              semanticRole: "protocol_state",
              contractAddress: protocolAddress,
              identityAssets: [],
              fundingLovelace: "0",
              fundingAssets: [],
              sourceActionKind: null,
              sourceOutputIndex: null,
            },
          ],
          fundingControlledOutputs: [
            {
              outputIndex: 0,
              role: "protocol_reward",
              custodyRole: "none",
              semanticRole: "prover_reward",
              contractAddress: fundedWalletAddress,
              fundingLovelace: "0",
              fundingAssets: [],
            },
          ],
          referenceInputs: [],
          referenceScriptBytes: 0,
          requiredBondLovelace: "0",
          requiredRewardCustodyLovelace: "0",
          requiredNativeAssets: [],
          collateralRequired: true,
          conflictRetryCount: 0,
        },
      ],
    }),
  });
};

const walletUtxo = (index: number, lovelace: bigint): UTxO => ({
  txHash: "99".repeat(32),
  outputIndex: index,
  address: fundedWalletAddress,
  assets: { lovelace },
});

const reservationRecords = async (
  store: WatcherProverFundingReservationStore,
) => (await store.readAll()).map(parseWatcherProverFundingReservationRecord);

describe("watcher production prover funding permit mint V1", () => {
  it("refuses a structural funding authority factory without reserving", async () => {
    const { store } = await openStore();
    const admitted = createWatcherProverFundingAuthorityFactory({
      journalRoot: process.cwd(),
      deploymentIdentity: makeWatcherDeploymentAuthorityFixture().result,
      protocolParameters:
        await unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest({
          deploymentIdentity: makeWatcherDeploymentAuthorityFixture().result,
          ogmiosUrl: "http://127.0.0.1:1337",
          timeoutMs: 10_000,
          fetchImpl,
        }),
      store,
    });
    await expect(
      mint({
        factory: {
          ...admitted,
        } as WatcherProverFundingAuthorityFactory,
      }),
    ).rejects.toThrow("prover funding authority factory is not admitted");
    // A refused mint must not lease any wallet input.
    expect(await reservationRecords(store)).toEqual([]);
  });

  it("refuses a runner that was not installed by its canonical workflow factory without reserving", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const { store } = await openStore();
    const factory = createWatcherProverFundingAuthorityFactory({
      journalRoot: process.cwd(),
      deploymentIdentity,
      protocolParameters:
        await unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest({
          deploymentIdentity,
          ogmiosUrl: "http://127.0.0.1:1337",
          timeoutMs: 10_000,
          fetchImpl,
        }),
      store,
    });
    await expect(mint({ factory })).rejects.toThrow(/fixed category runner/u);
    expect(await reservationRecords(store)).toEqual([]);
  });

  it("admits a measured runner and rejects an invalid actuation permit before reserving", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const { store } = await openStore();
    const protocolParameters =
      await unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest({
        deploymentIdentity,
        ogmiosUrl: "http://127.0.0.1:1337",
        timeoutMs: 10_000,
        fetchImpl,
      });
    const factory = createWatcherProverFundingAuthorityFactory({
      journalRoot: process.cwd(),
      deploymentIdentity,
      protocolParameters,
      store,
    });
    const runner = await measuredDoubleSpendRunner(
      deploymentIdentity,
      protocolParameters.snapshotDigest,
    );
    const walletUtxos = [
      walletUtxo(0, 2_000_000_000n),
      walletUtxo(1, 2_000_000_000n),
      walletUtxo(2, 2_000_000_000n),
      walletUtxo(3, 2_000_000_000n),
    ];
    const getUtxos = vi.fn(async () => walletUtxos);
    const fundedProvider: WatcherProverFundingUtxoProvider = Object.freeze({
      getUtxos,
      getUtxosByOutRef: async () => [],
    });

    // A refused live authority must not create a reservation.
    await expect(
      mint({
        factory,
        runner,
        walletAddress: fundedWalletAddress,
        provider: fundedProvider,
      }),
    ).rejects.toThrow("actuation permit was not admitted");
    expect(getUtxos).toHaveBeenCalledWith(fundedWalletAddress);

    expect(await reservationRecords(store)).toEqual([]);
  });
});

const openStore = async () => {
  const directory = await mkdtemp(
    join(process.cwd(), ".watcher-funding-permit-mint-test-"),
  );
  directories.push(directory);
  return await openWatcherSqliteProverFundingReservationStore({
    path: join(directory, "watcher.sqlite"),
  });
};
