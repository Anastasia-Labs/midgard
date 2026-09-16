import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import { createWatcherProverFundingAuthorityFactory } from "../../src/funding/prover-funding-authority.js";
import * as calculations from "../../src/funding/prover-funding-calculation.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

const setup = async (priorRoster = false) => {
  const fixture = await setupFundingRecoveryFixture(
    false,
    false,
    false,
    false,
    priorRoster,
  );
  let changedBasis = false;
  const readAll = vi.fn(async () => {
    const records = await fixture.records();
    return records.map((record) => {
      if (!changedBasis) return record;
      const { recordDigest: _digest, ...fields } = record;
      const changed = { ...fields, reservationBasisDigest: "fe".repeat(32) };
      return {
        ...changed,
        recordDigest: computeDeploymentManifestJsonDigest(changed),
      };
    });
  });
  const factory = createWatcherProverFundingAuthorityFactory({
    journalRoot: fixture.journalRoot,
    launchScope: fixture.old.launchScope,
    deploymentIdentity,
    protocolParameters: fixture.protocolParameters,
    store: { ...fixture.store, readAll },
  });
  const calculate = vi.spyOn(
    calculations,
    "calculateWatcherRuntimeProverFunding",
  );
  const create = (generation: string) => {
    const controller = createWorkflowActuationPermitController({
      decision: fixture.fresh,
      rollbackGeneration: generation,
    });
    return {
      controller,
      run: () =>
        factory.create({
          category: "doubleSpend",
          runner: fixture.runner,
          actuationPermit: controller.permit,
          rollbackGeneration: generation,
          decisionDigest: fixture.fresh.decisionDigest,
          walletAddress,
          readWalletUtxos: fixture.readWalletUtxos,
          resolveInputs: fixture.resolveInputs,
          reservationMode: "resume_only",
          resolveProtocolInputAuthority: async () => {
            throw new Error("unexpected protocol read");
          },
        }),
    };
  };
  return {
    fixture,
    calculate,
    create,
    readAll,
    changeBasis: () => {
      changedBasis = true;
    },
  };
};

it.each([false, true])(
  "reuses one admitted calculation across authority generations (prior policy: %s)",
  async (priorRoster) => {
    const test = await setup(priorRoster);
    const before = await test.fixture.records();
    for (const generation of ["2", "3", "4"])
      await test.create(generation).run();
    expect(test.calculate).toHaveBeenCalledTimes(1);
    expect(test.readAll.mock.calls.length).toBeGreaterThanOrEqual(3);
    expect(await test.fixture.records()).toEqual(before);
  },
);

it("rejects changed original reservation basis even after a calculation cache hit", async () => {
  const test = await setup();
  await test.create("2").run();
  test.changeBasis();
  await expect(test.create("3").run()).rejects.toThrow(
    /reservation identity mismatch/u,
  );
  expect(test.calculate).toHaveBeenCalledTimes(1);
});

it("still checks current actuation before reusing a prior generation's calculation", async () => {
  const test = await setup();
  await test.create("2").run();
  const next = test.create("3");
  next.controller.revoke("rollback");
  await expect(next.run()).rejects.toThrow(/revoked/u);
  expect(test.calculate).toHaveBeenCalledTimes(1);
});
