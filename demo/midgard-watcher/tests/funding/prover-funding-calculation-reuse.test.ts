import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  beginWorkflowFundingReservationAction,
  createWorkflowActuationPermitController,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import * as parameterAuthorities from "../../src/funding/prover-funding.js";
import { unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest } from "../../src/funding/prover-funding.js";
import * as authorityCreation from "../../src/funding/prover-funding-authority.create-watcher-prover-funding-authority.js";
import { createWatcherProverFundingAuthorityFactory } from "../../src/funding/prover-funding-authority.js";
import * as calculations from "../../src/funding/prover-funding-calculation.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { ogmiosParameters } from "./prover-funding-calculation.transaction-cbor.js";

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

const updatedParameters = async (collateralPercentage = 150) =>
  await unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest({
    deploymentIdentity,
    ogmiosUrl: "http://127.0.0.1:1337",
    timeoutMs: 10_000,
    fetchImpl: vi.fn(async (_url, init) => {
      const { id } = JSON.parse(String(init?.body)) as { id: string };
      return new Response(
        JSON.stringify({
          jsonrpc: "2.0",
          id,
          result: {
            ...ogmiosParameters(),
            minFeeCoefficient: 45,
            collateralPercentage,
          },
        }),
      );
    }) as unknown as typeof fetch,
  });

it("resumes an exact deployed reservation after a live fee update and restart", async () => {
  const fixture = await setupFundingRecoveryFixture();
  const current = await updatedParameters();
  const calculate = vi.spyOn(
    calculations,
    "calculateWatcherRuntimeProverFunding",
  );
  const before = await fixture.records();
  const factory = createWatcherProverFundingAuthorityFactory({
    journalRoot: fixture.journalRoot,
    launchScope: fixture.old.launchScope,
    deploymentIdentity,
    protocolParameters: current,
    store: fixture.store,
  });
  const controller = createWorkflowActuationPermitController({
    decision: fixture.fresh,
    rollbackGeneration: "2",
  });
  await factory.create({
    category: "doubleSpend",
    runner: fixture.runner,
    actuationPermit: controller.permit,
    rollbackGeneration: "2",
    decisionDigest: fixture.fresh.decisionDigest,
    walletAddress,
    readWalletUtxos: fixture.readWalletUtxos,
    resolveInputs: fixture.resolveInputs,
    reservationMode: "resume_only",
    resolveProtocolInputAuthority: async () => {
      throw new Error("unexpected protocol read");
    },
  });
  expect(current.snapshot.minFeeA).toBe("45");
  expect(calculate.mock.calls[0]![0].protocolParameters.snapshot.minFeeA).toBe(
    "44",
  );
  expect(await fixture.records()).toEqual(before);
});

it("uses updated live parameters for new decisions instead of its cached startup basis", async () => {
  const fixture = await setupFundingRecoveryFixture();
  const current = await updatedParameters();
  vi.spyOn(
    parameterAuthorities,
    "refreshWatcherProtocolParameterRuntimeAuthority",
  ).mockResolvedValue(current);
  const calculate = vi.spyOn(
    calculations,
    "calculateWatcherRuntimeProverFunding",
  );
  const create = vi
    .spyOn(authorityCreation, "createWatcherProverFundingAuthority")
    .mockResolvedValue({ permit: {} } as Awaited<
      ReturnType<typeof authorityCreation.createWatcherProverFundingAuthority>
    >);
  const readAll = vi.fn(async () => []);
  const factory = createWatcherProverFundingAuthorityFactory({
    journalRoot: fixture.journalRoot + "/new",
    launchScope: fixture.old.launchScope,
    deploymentIdentity,
    protocolParameters: fixture.protocolParameters,
    store: { ...fixture.store, readAll },
  });
  const controller = createWorkflowActuationPermitController({
    decision: fixture.fresh,
    rollbackGeneration: "2",
  });
  await factory.create({
    category: "doubleSpend",
    runner: fixture.runner,
    actuationPermit: controller.permit,
    rollbackGeneration: "2",
    decisionDigest: fixture.fresh.decisionDigest,
    walletAddress,
    readWalletUtxos: fixture.readWalletUtxos,
    resolveInputs: fixture.resolveInputs,
    resolveProtocolInputAuthority: async () => {
      throw new Error("unexpected protocol read");
    },
  });
  expect(calculate.mock.calls[0]![0].protocolParameters.snapshot.minFeeA).toBe(
    "45",
  );
  expect(create.mock.calls[0]![0].calculation.protocolParametersDigest).toBe(
    current.snapshotDigest,
  );
});

it("authenticates and resumes a nondeployment parameter basis after restart", async () => {
  const fixture = await setupFundingRecoveryFixture(
    false,
    false,
    false,
    false,
    false,
    20n,
    45,
  );
  await fixture.restartStore();
  const calculate = vi.spyOn(
    calculations,
    "calculateWatcherRuntimeProverFunding",
  );
  // The new live query has returned to a different fee; the exact signed
  // reservation still requires the 45-coefficient snapshot it was born under.
  const factory = createWatcherProverFundingAuthorityFactory({
    journalRoot: fixture.journalRoot,
    launchScope: fixture.old.launchScope,
    deploymentIdentity,
    protocolParameters:
      await parameterAuthorities.unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest(
        {
          deploymentIdentity,
          ogmiosUrl: "http://127.0.0.1:1337",
          timeoutMs: 10_000,
          fetchImpl: vi.fn(async (_url, init) => {
            const { id } = JSON.parse(String(init?.body)) as { id: string };
            return new Response(
              JSON.stringify({
                jsonrpc: "2.0",
                id,
                result: ogmiosParameters(),
              }),
            );
          }) as unknown as typeof fetch,
        },
      ),
    protocolParameterHistory: fixture.protocolParameterHistory,
    store: fixture.store,
  });
  const before = await fixture.records();
  const controller = createWorkflowActuationPermitController({
    decision: fixture.fresh,
    rollbackGeneration: "2",
  });
  const permit = await factory.create({
    category: "doubleSpend",
    runner: fixture.runner,
    actuationPermit: controller.permit,
    rollbackGeneration: "2",
    decisionDigest: fixture.fresh.decisionDigest,
    walletAddress,
    readWalletUtxos: fixture.readWalletUtxos,
    resolveInputs: fixture.resolveInputs,
    reservationMode: "resume_only",
    resolveProtocolInputAuthority: async () => {
      throw new Error("unexpected protocol read");
    },
  });
  expect(calculate.mock.calls[0]![0].protocolParameters.snapshot.minFeeA).toBe(
    "45",
  );
  expect(await fixture.records()).toEqual(before);
  await fixture.run(
    fixture.bind(fixture.fresh, {
      permit,
      controller,
      releaseUnused: async () => undefined,
    }),
  );
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
  const entries = await fixture.journal.load(fixture.initial.workflowId);
  expect(entries.slice(0, fixture.originalEntries.length)).toEqual(
    fixture.originalEntries,
  );
});

it("reprices the next installed step's idle collateral after reconciling its original signed intent", async () => {
  const fixture = await setupFundingRecoveryFixture();
  const current = await updatedParameters(600);
  fixture.walletUtxos.push({
    txHash: "cd".repeat(32),
    outputIndex: 0,
    address: walletAddress,
    assets: { lovelace: 4_000_000_000n },
  });
  const create = vi.spyOn(
    authorityCreation,
    "createWatcherProverFundingAuthority",
  );
  const before = await fixture.records();
  const factory = createWatcherProverFundingAuthorityFactory({
    journalRoot: fixture.journalRoot,
    launchScope: fixture.old.launchScope,
    deploymentIdentity,
    protocolParameters: current,
    protocolParameterHistory: fixture.protocolParameterHistory,
    store: fixture.store,
  });
  const controller = createWorkflowActuationPermitController({
    decision: fixture.fresh,
    rollbackGeneration: "2",
  });
  const permit = await factory.create({
    category: "doubleSpend",
    runner: fixture.runner,
    actuationPermit: controller.permit,
    rollbackGeneration: "2",
    decisionDigest: fixture.fresh.decisionDigest,
    walletAddress,
    readWalletUtxos: fixture.readWalletUtxos,
    resolveInputs: fixture.resolveInputs,
    reservationMode: "resume_only",
    resolveProtocolInputAuthority: async () => {
      throw new Error("unexpected protocol read");
    },
  });
  expect(
    BigInt(
      create.mock.calls[0]![0].selectionCalculation!
        .maximumSlashCollateralLovelace,
    ),
  ).toBe(3_000_000_000n);
  const journal = fixture.bind(fixture.fresh, {
    permit,
    controller,
    releaseUnused: async () =>
      factory.releaseUnused({ actuationPermit: controller.permit }),
  });
  await fixture.run(journal);
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
  const signedPrefix = await journal.load(fixture.initial.workflowId);
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: "next", input: { actionKind: "step-one" } },
  });
  const after = (await fixture.records())[0]!;
  expect(
    after.activeInputs
      .filter(({ role }) => role === "collateral")
      .map(({ outRef }) => outRef),
  ).toEqual([`${"cd".repeat(32)}#0`]);
  expect(after).toMatchObject({
    reservationId: before[0]!.reservationId,
    policyDigest: before[0]!.policyDigest,
    reservationBasisDigest: before[0]!.reservationBasisDigest,
  });
  expect(await journal.load(fixture.initial.workflowId)).toEqual(signedPrefix);
});
