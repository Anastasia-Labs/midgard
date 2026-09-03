import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import type {
  ProductionWorkflowActuationPermitV1,
  ProductionWorkflowAdapterRunnerV1,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  createWatcherProductionProverFundingAuthorityFactoryV1,
  type WatcherProductionProverFundingAuthorityFactoryV1,
} from "../../src/funding/production-prover-funding-authority-v1.js";
import { unsafeCreateWatcherProductionProtocolParameterRuntimeAuthorityForTestV1 } from "../../src/funding/production-prover-funding-v1.js";
import type { WatcherProductionWorkflowFundingProfileOverlayV1 } from "../../src/funding/production-workflow-funding-profile-overlay-v1.js";
import { openWatcherSqliteProverFundingReservationStoreV1 } from "../../src/funding/sqlite-prover-funding-reservation-store-v1.js";
import { mintWatcherProductionProverFundingReservationPermitV1 } from "../../src/runtime/production-watcher-runtime-v1.js";
import { makeWatcherDeploymentAuthorityFixtureV1 } from "../support/deployment-authority-fixture.js";

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

const structuralOverlay = Object.freeze({
  schemaVersion:
    "midgard-watcher-production-workflow-funding-profile-overlay-v1",
  deploymentFingerprint: "11".repeat(32),
  releaseEvidenceDigest: "22".repeat(32),
  bundlePath: "/etc/midgard/funding-profiles.json",
  profiles: Object.freeze({}),
}) as unknown as WatcherProductionWorkflowFundingProfileOverlayV1;

const structuralRunner = Object.freeze({
  runnerVersion: "midgard-production-workflow-adapter-runner-v1",
  runOrResume: async () => {
    throw new Error("test runner must not execute");
  },
}) as unknown as ProductionWorkflowAdapterRunnerV1;

const structuralActuationPermit = Object.freeze({
  permitVersion: "midgard-production-workflow-actuation-permit-v1",
}) as ProductionWorkflowActuationPermitV1;

const provider = Object.freeze({
  getUtxos: async () => [],
  getUtxosByOutRef: async () => [],
});

const mint = (input: {
  readonly factory: WatcherProductionProverFundingAuthorityFactoryV1;
  readonly overlay?: WatcherProductionWorkflowFundingProfileOverlayV1;
}) =>
  mintWatcherProductionProverFundingReservationPermitV1({
    category: "doubleSpend",
    runner: structuralRunner,
    fundingProfileOverlay: input.overlay ?? structuralOverlay,
    factory: input.factory,
    actuationPermit: structuralActuationPermit,
    rollbackGeneration: "0",
    decisionDigest: "cd".repeat(32),
    walletAddress:
      "addr_test1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg57c2qv",
    provider,
  });

describe("watcher production prover funding permit mint V1", () => {
  it("refuses a structural funding authority factory", async () => {
    const admitted = createWatcherProductionProverFundingAuthorityFactoryV1({
      deploymentIdentity: makeWatcherDeploymentAuthorityFixtureV1().result,
      protocolParameters:
        await unsafeCreateWatcherProductionProtocolParameterRuntimeAuthorityForTestV1(
          {
            deploymentIdentity:
              makeWatcherDeploymentAuthorityFixtureV1().result,
            ogmiosUrl: "http://127.0.0.1:1337",
            timeoutMs: 10_000,
            fetchImpl,
          },
        ),
      store: (await openStore()).store,
    });
    await expect(
      mint({
        factory: {
          ...admitted,
        } as WatcherProductionProverFundingAuthorityFactoryV1,
      }),
    ).rejects.toThrow("prover funding authority factory is not admitted");
  });

  it("refuses a funding profile overlay that was not admitted from signed release evidence", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixtureV1().result;
    const factory = createWatcherProductionProverFundingAuthorityFactoryV1({
      deploymentIdentity,
      protocolParameters:
        await unsafeCreateWatcherProductionProtocolParameterRuntimeAuthorityForTestV1(
          {
            deploymentIdentity,
            ogmiosUrl: "http://127.0.0.1:1337",
            timeoutMs: 10_000,
            fetchImpl,
          },
        ),
      store: (await openStore()).store,
    });
    await expect(mint({ factory })).rejects.toThrow(
      "production workflow funding profile overlay was not admitted from signed release evidence",
    );
  });
});

const openStore = async () => {
  const directory = await mkdtemp(
    join(process.cwd(), ".watcher-funding-permit-mint-test-"),
  );
  directories.push(directory);
  return await openWatcherSqliteProverFundingReservationStoreV1({
    path: join(directory, "watcher.sqlite"),
  });
};
