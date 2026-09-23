import {
  encodeLinkedListNodeView,
  REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
} from "@al-ft/midgard-sdk";
import { credentialToAddress, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createLocalStateQueueMutationLeaseCoordinator,
  fraudRemovalUsesWalletCoinSelection,
  LOCAL_STATE_QUEUE_MUTATION_LEASE_SOURCE,
  LOCAL_STATE_QUEUE_MUTATION_LEASE_TOKEN,
  fraudSlashEconomicsFromDeploymentManifest,
  RegisteredOperatorActivationRequiredError,
  resolveFraudSlashEconomics,
  resolveRegisteredOperatorRemovalWitness,
} from "../src/remove-fraudulent-block.js";

describe("registered-operator scheduler rewind witness", () => {
  const policyId = "ab".repeat(28);
  const address = credentialToAddress("Custom", {
    type: "Script",
    hash: "cd".repeat(28),
  });
  const key = (time: bigint) => time.toString(16).padStart(12, "0");
  const element = (activation: bigint | null, next: bigint | null): UTxO => ({
    txHash: (activation === null
      ? "11"
      : activation % 2n === 0n
        ? "22"
        : "33"
    ).repeat(32),
    outputIndex: activation === null ? 0 : 1,
    address,
    assets: {
      lovelace: 2_000_000n,
      [toUnit(
        policyId,
        activation === null
          ? REGISTERED_OPERATORS_ROOT_ASSET_NAME
          : REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + key(activation),
      )]: 1n,
    },
    datum: encodeLinkedListNodeView({
      key: activation === null ? "Empty" : { Key: { key: key(activation) } },
      next: next === null ? "Empty" : { Key: { key: key(next) } },
      data: "",
    }),
  });
  const resolve = (utxos: UTxO[], upper = 999n) =>
    resolveRegisteredOperatorRemovalWitness({
      utxos,
      address,
      policyId,
      inclusiveValidityUpperBound: upper,
    });

  it("uses the authentic empty root", async () => {
    const root = element(null, null);
    expect(await resolve([root])).toBe(root);
  });
  it("uses the final future registration instead of a nonempty root, regardless of provider order", async () => {
    const root = element(null, 2001n);
    const latest = element(2001n, 1000n);
    const earliest = element(1000n, null);
    expect(await resolve([earliest, root, latest])).toBe(earliest);
  });
  it.each([1000n, 1001n])(
    "requires activation when the removal interval reaches the final registration at %s",
    async (upper) => {
      const pending = element(1000n, null);
      await expect(
        resolve([element(null, 1000n), pending], upper),
      ).rejects.toMatchObject({
        name: RegisteredOperatorActivationRequiredError.name,
        registeredOperatorOutRef: `${pending.txHash}#1`,
        activationTime: 1000n,
      });
    },
  );
  it("refuses a missing final node", async () => {
    await expect(resolve([element(null, 1000n)])).rejects.toThrow(
      /missing node/u,
    );
  });
  it("refuses an unreachable registration", async () => {
    await expect(
      resolve([element(null, null), element(1000n, null)]),
    ).rejects.toThrow(/unreachable/u);
  });
  it("refuses descending-order violations instead of treating a later node as the earliest activation", async () => {
    await expect(
      resolve([
        element(null, 1000n),
        element(1000n, 2001n),
        element(2001n, null),
      ]),
    ).rejects.toThrow(/descending activation/u);
  });
  it("refuses a cycle", async () => {
    await expect(
      resolve([element(null, 1000n), element(1000n, 1000n)]),
    ).rejects.toThrow(/cycle/u);
  });
  it("ignores unauthenticated root lookalikes", async () => {
    const root = element(null, null);
    await expect(
      resolve([{ ...root, assets: { lovelace: 2_000_000n } }]),
    ).rejects.toThrow(/authentic root/u);
    await expect(
      resolve([
        {
          ...root,
          address: credentialToAddress("Custom", {
            type: "Key",
            hash: "ef".repeat(28),
          }),
        },
      ]),
    ).rejects.toThrow(/authentic root/u);
  });
});

const publicEconomics = {
  profile: "public-preprod-launch-v1",
  requiredBondLovelace: 100_000_000_000n,
  slashingPenaltyLovelace: 25_000_000_000n,
  inactivitySlashingPenaltyLovelace: 10_000_000_000n,
  fraudProverRewardLovelace: 75_000_000_000n,
  proverCollateralFloorLovelace: 5_000_000n,
} as const;
const boundedEconomics = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: 900_000_000n,
  slashingPenaltyLovelace: 500_000_000n,
  inactivitySlashingPenaltyLovelace: 100_000_000n,
  fraudProverRewardLovelace: 400_000_000n,
  proverCollateralFloorLovelace: 5_000_000n,
} as const;

describe("Q53 exact fraud-slash economics", () => {
  it("disables wallet coin selection only for exact bond-backed slash branches", () => {
    expect(fraudRemovalUsesWalletCoinSelection("SlashActiveOperator")).toBe(
      false,
    );
    expect(fraudRemovalUsesWalletCoinSelection("SlashRetiredOperator")).toBe(
      false,
    );
    expect(fraudRemovalUsesWalletCoinSelection("OperatorAlreadySlashed")).toBe(
      true,
    );
  });

  it("binds the public and testnet full/partially-inactivity-slashed tranches", () => {
    expect(
      resolveFraudSlashEconomics(publicEconomics, 100_000_000_000n),
    ).toEqual({
      requiredBondLovelace: 100_000_000_000n,
      fraudProverRewardLovelace: 75_000_000_000n,
      exactFeeLovelace: 25_000_000_000n,
      tranche: "full",
    });
    expect(
      resolveFraudSlashEconomics(publicEconomics, 90_000_000_000n),
    ).toEqual({
      requiredBondLovelace: 100_000_000_000n,
      fraudProverRewardLovelace: 75_000_000_000n,
      exactFeeLovelace: 15_000_000_000n,
      tranche: "partially-inactivity-slashed",
    });
    expect(resolveFraudSlashEconomics(boundedEconomics, 900_000_000n)).toEqual({
      requiredBondLovelace: 900_000_000n,
      fraudProverRewardLovelace: 400_000_000n,
      exactFeeLovelace: 500_000_000n,
      tranche: "full",
    });
    expect(resolveFraudSlashEconomics(boundedEconomics, 800_000_000n)).toEqual({
      requiredBondLovelace: 900_000_000n,
      fraudProverRewardLovelace: 400_000_000n,
      exactFeeLovelace: 400_000_000n,
      tranche: "partially-inactivity-slashed",
    });
  });

  it.each([
    899_999_999n,
    900_000_001n,
    799_999_999n,
    800_000_001n,
    700_000_000n,
  ])("rejects illegal testnet bond tranche %s", (lovelace) => {
    expect(() =>
      resolveFraudSlashEconomics(boundedEconomics, lovelace),
    ).toThrow(/must be exactly/);
  });

  it("rejects a manifest economics tuple with inconsistent slash relations", () => {
    expect(() =>
      resolveFraudSlashEconomics(
        { ...boundedEconomics, fraudProverRewardLovelace: 399_999_999n },
        900_000_000n,
      ),
    ).toThrow(/violate F04 slash relations/u);
    expect(() =>
      resolveFraudSlashEconomics(
        {
          ...boundedEconomics,
          inactivitySlashingPenaltyLovelace: 500_000_000n,
        },
        900_000_000n,
      ),
    ).toThrow(/violate F04 slash relations/u);
  });

  it("selects economics from the release manifest, never the Cardano network label", () => {
    expect(
      fraudSlashEconomicsFromDeploymentManifest({
        economics: {
          ...publicEconomics,
          requiredBondLovelace: Number(publicEconomics.requiredBondLovelace),
          slashingPenaltyLovelace: Number(
            publicEconomics.slashingPenaltyLovelace,
          ),
          inactivitySlashingPenaltyLovelace: Number(
            publicEconomics.inactivitySlashingPenaltyLovelace,
          ),
          fraudProverRewardLovelace: Number(
            publicEconomics.fraudProverRewardLovelace,
          ),
          proverCollateralFloorLovelace: Number(
            publicEconomics.proverCollateralFloorLovelace,
          ),
        },
      }),
    ).toEqual(publicEconomics);
    expect(() =>
      fraudSlashEconomicsFromDeploymentManifest({
        economics: {
          profile: "public-preprod-launch-v1",
          requiredBondLovelace: 900_000_000,
          slashingPenaltyLovelace: 500_000_000,
          inactivitySlashingPenaltyLovelace: 100_000_000,
          fraudProverRewardLovelace: 400_000_000,
          proverCollateralFloorLovelace: 5_000_000,
        },
      }),
    ).toThrow(/requiredBondLovelace must equal/u);
  });
});

describe("remove-fraudulent-block local lease coordinator", () => {
  it("acquires and resumes the one frozen local lease identity", async () => {
    const coordinator = createLocalStateQueueMutationLeaseCoordinator();
    const acquired = await coordinator.acquire();
    expect({ token: acquired.token, source: acquired.source }).toEqual({
      token: "local-retry-until-confirmed",
      source: "local",
    });
    expect(acquired.token).toBe(LOCAL_STATE_QUEUE_MUTATION_LEASE_TOKEN);
    expect(acquired.source).toBe(LOCAL_STATE_QUEUE_MUTATION_LEASE_SOURCE);
    expect(Object.isFrozen(acquired)).toBe(true);
    await expect(acquired.renew()).resolves.toBeUndefined();
    await expect(acquired.release()).resolves.toBeUndefined();
    await expect(acquired.fail("lost race")).resolves.toBeUndefined();

    // A fresh coordinator (a restarted process) resumes the journaled identity.
    const resumed =
      await createLocalStateQueueMutationLeaseCoordinator().resume?.({
        token: acquired.token,
        source: acquired.source,
      });
    expect(resumed).toMatchObject({
      token: LOCAL_STATE_QUEUE_MUTATION_LEASE_TOKEN,
      source: LOCAL_STATE_QUEUE_MUTATION_LEASE_SOURCE,
    });
  });

  it("refuses a journaled Midgard node lease as a different coordinator", async () => {
    const coordinator = createLocalStateQueueMutationLeaseCoordinator();
    await expect(
      coordinator.resume?.({
        token: "lease-token",
        source: "http://midgard-node.test",
      }),
    ).rejects.toThrow("different coordinator");
    await expect(
      coordinator.resume?.({
        token: LOCAL_STATE_QUEUE_MUTATION_LEASE_TOKEN,
        source: "http://midgard-node.test",
      }),
    ).rejects.toThrow("cannot switch coordination mode");
  });

  it("refuses an unknown local lease token", async () => {
    await expect(
      createLocalStateQueueMutationLeaseCoordinator().resume?.({
        token: "lease-token",
        source: LOCAL_STATE_QUEUE_MUTATION_LEASE_SOURCE,
      }),
    ).rejects.toThrow("unknown local token");
  });
});
