/**
 * A proven-record removal spends prover-wallet coins (its fee input and
 * collateral) outside any funding reservation. From its pre-submit boundary
 * until it confirms or its validity ends, the funding store holds them, so a
 * reservation plan never selects one and the two transactions never race
 * for a coin.
 */
import { rm } from "node:fs/promises";

import {
  createWorkflowRuntimeFundingPolicy,
  WORKFLOW_RUNNER_FACTORIES,
} from "@al-ft/midgard-fault-proofs";
import { credentialToAddress } from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import { calculateWatcherRuntimeProverFunding } from "../../src/funding/prover-funding-calculation.js";
import { planWatcherProverFundingReservation } from "../../src/funding/prover-funding-reservation.js";
import { isWatcherProverFundingReservationConflict } from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import { watcherDeploymentReleaseEconomicsAuthority } from "../../src/runtime/deployment-identity.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import { runtimeAuthority } from "./prover-funding-calculation.runtime-authority.js";
import {
  fundingPaymentKeyHash,
  walletAddress,
} from "./prover-funding-calculation.transaction-cbor.js";
import {
  openStore,
  plan as storedPlan,
  temporaryDirectories,
} from "./sqlite-prover-funding-reservation-store.signed-transition.js";

afterEach(async () => {
  await Promise.all(
    temporaryDirectories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

const coin = (fill: string, lovelace: bigint) => ({
  txHash: fill.repeat(32),
  outputIndex: 0,
  address: walletAddress,
  assets: { lovelace },
});
const outRef = ({ txHash, outputIndex }: ReturnType<typeof coin>) =>
  `${txHash}#${outputIndex.toString()}`;

// The removal's fee input and collateral, and two coins it left alone.
const REMOVAL_FEE = coin("01", 6_000_000n);
const REMOVAL_COLLATERAL = coin("02", 900_000_000n);
const FREE_FEE = coin("04", 6_000_000n);
const FREE_COLLATERAL = coin("05", 900_000_000n);

it("plans no reservation over a coin an unconfirmed removal holds", async () => {
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
  const plan = (utxos: readonly ReturnType<typeof coin>[], decision = "77") =>
    planWatcherProverFundingReservation({
      deploymentIdentity,
      calculation,
      decisionDigest: decision.repeat(32),
      walletAddress,
      utxos,
    });
  const { runtime } = await openStore();
  try {
    const { store } = runtime;
    const wallet = [REMOVAL_FEE, REMOVAL_COLLATERAL, FREE_FEE, FREE_COLLATERAL];
    const held = [outRef(REMOVAL_FEE), outRef(REMOVAL_COLLATERAL)];
    store.holdWalletInputs({ outRefs: held, untilMs: Date.now() + 300_000 });

    // The authority plans from the wallet less every reserved out-ref.
    const reserved = new Set(await store.readReservedOutRefs({}));
    const planned = plan(wallet.filter((utxo) => !reserved.has(outRef(utxo))));
    expect(planned.inputs.map(({ outRef, role }) => [outRef, role])).toEqual([
      [outRef(FREE_FEE), "funding"],
      [outRef(FREE_COLLATERAL), "collateral"],
    ]);
    await expect(store.reserve(planned)).resolves.toBe("reserved");

    // A plan read before the hold still cannot reserve a held coin.
    const stale = plan([REMOVAL_FEE, REMOVAL_COLLATERAL], "78");
    const refused = await store.reserve(stale).catch((error: unknown) => error);
    expect(isWatcherProverFundingReservationConflict(refused)).toBe(true);
    expect(refused).toMatchObject({ conflict: { outRef: held[0] } });
  } finally {
    runtime.close();
  }
});

it("holds wallet coins in memory until release or expiry, never over a lease", async () => {
  const { path, runtime } = await openStore();
  let open = runtime;
  const leased = storedPlan("aa", "66");
  const [funding, collateral] = leased.inputs.map(({ outRef }) => outRef);
  const free = `${"13".repeat(32)}#1`;
  const other = `${"14".repeat(32)}#0`;
  const now = Date.now();
  const clock = vi.spyOn(Date, "now").mockReturnValue(now);
  try {
    const { store } = runtime;
    await store.reserve(leased);
    const conflict = (hold: () => unknown) => {
      try {
        hold();
      } catch (error) {
        return isWatcherProverFundingReservationConflict(error);
      }
      return false;
    };
    // A coin a reservation leases is never held.
    expect(
      conflict(() =>
        store.holdWalletInputs({
          outRefs: [free, collateral!],
          untilMs: now + 10,
        }),
      ),
    ).toBe(true);
    // The refused hold took nothing.
    expect(await store.readReservedOutRefs({})).not.toContain(free);

    const release = store.holdWalletInputs({
      outRefs: [free],
      untilMs: now + 10,
    });
    expect(await store.readReservedOutRefs({})).toEqual(
      expect.arrayContaining([funding, collateral, free]),
    );
    // Holds belong to no reservation, so the lease holder sees them too.
    expect(
      await store.readReservedOutRefs({
        excludingReservationId: leased.reservationId,
      }),
    ).toEqual([free]);
    // Nor is a coin held twice, or reserved while held.
    expect(
      conflict(() =>
        store.holdWalletInputs({ outRefs: [free], untilMs: now + 10 }),
      ),
    ).toBe(true);
    const competitor = storedPlan("bb", "77", free);
    const refused = await store
      .reserve({
        ...competitor,
        inputs: competitor.inputs.map((input) =>
          input.role === "collateral"
            ? { ...input, outRef: `${"15".repeat(32)}#0` }
            : input,
        ),
      })
      .catch((error: unknown) => error);
    expect(isWatcherProverFundingReservationConflict(refused)).toBe(true);
    expect(refused).toMatchObject({ conflict: { outRef: free } });

    release();
    expect(await store.readReservedOutRefs({})).not.toContain(free);

    // Expiry ends a hold with no release; a restart drops every hold.
    store.holdWalletInputs({ outRefs: [free], untilMs: now + 10 });
    store.holdWalletInputs({ outRefs: [other], untilMs: now + 20 });
    clock.mockReturnValue(now + 10);
    expect(await store.readReservedOutRefs({})).not.toContain(free);
    expect(await store.readReservedOutRefs({})).toContain(other);
    store.holdWalletInputs({ outRefs: [free], untilMs: now + 20 });
    runtime.close();
    open = (await openStore(path)).runtime;
    expect(
      await open.store.readReservedOutRefs({
        excludingReservationId: leased.reservationId,
      }),
    ).toEqual([]);
  } finally {
    clock.mockRestore();
    open.close();
  }
});
