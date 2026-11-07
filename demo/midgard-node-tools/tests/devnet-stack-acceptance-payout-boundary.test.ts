import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, beforeEach, expect, it, vi } from "vitest";

import { verifyAcceptancePayoutLineage } from "../src/devnet-stack/acceptance-payout-lineage.js";
import { sampleAcceptancePayouts } from "../src/devnet-stack/acceptance-payout-sample.js";
import { makeLayout } from "../src/devnet-stack/layout.js";
import { collectorFixture } from "./devnet-stack-acceptance-payout-collector.fixtures.js";

const controls = vi.hoisted(() => ({
  capture: vi.fn(),
  sql: vi.fn(),
  manifest: vi.fn(),
}));
vi.mock("../src/devnet-stack/acceptance-native-boundary.js", () => ({
  captureAcceptanceNativeBoundary: controls.capture,
}));
vi.mock("../src/devnet-stack/acceptance-payout-sql.js", () => ({
  readAcceptanceSettlements: controls.sql,
}));
vi.mock("../src/devnet-stack/watcher-release.js", () => ({
  readFinalizedManifest: controls.manifest,
}));

const limits = {
  timeoutMs: 10000,
  maxTransactionBytes: 16384,
  maxLineageTransactions: 8,
  maxSettlementRows: 16,
  maxKupoResponseBytes: 16384,
  maxUtxoResponseBytes: 16384,
  maxReferenceInputs: 4,
  blockScanLimit: 1,
  maxDrillEvidenceBytes: 16384,
};
const directories: string[] = [];
afterEach(() => {
  for (const path of directories.splice(0))
    rmSync(path, { recursive: true, force: true });
});
beforeEach(() => vi.resetAllMocks());

const setup = () => {
  const fixture = collectorFixture();
  const directory = mkdtempSync(join(tmpdir(), "midgard-payout-capture-"));
  directories.push(directory);
  const layout = makeLayout(directory);
  mkdirSync(layout.journeyDir, { recursive: true });
  writeFileSync(
    layout.runEnv,
    [
      `MIDGARD_PHASE4_RUN_DIR=${directory}`,
      "MIDGARD_PHASE4_RUN_ID=fixture",
      "MIDGARD_PHASE4_COMPOSE_PROJECT=fixture",
      "MIDGARD_PHASE4_NETWORK_MAGIC=42",
      "MIDGARD_PHASE4_OGMIOS_PORT=2337",
      "MIDGARD_PHASE4_KUPO_PORT=12345",
      "MIDGARD_PHASE4_POSTGRES_PORT=5433",
      "MIDGARD_PHASE4_POSTGRES_USER=postgres",
      "MIDGARD_PHASE4_POSTGRES_PASSWORD=postgres",
      "MIDGARD_PHASE4_POSTGRES_DATABASE=fixture",
      "MIDGARD_PHASE4_CARDANO_NODE_IMAGE=fixture",
      "MIDGARD_PHASE4_POSTGRES_IMAGE=fixture",
    ].join("\n"),
  );
  mkdirSync(layout.logs, { recursive: true });
  const drillNames = [
    "restart-cardano-node",
    "stop-kupo",
    "stop-ogmios",
    "kill-public-retained-da",
    "kill-node-on-admission",
    "kill-node-on-block-submitted",
    "kill-node-on-settlement",
    "kill-node",
    "kill-da-member-0",
    "kill-da-member-1",
    "kill-watcher",
    "pause-postgres",
  ];
  const targets = [
    "cardano-node",
    "kupo",
    "ogmios",
    "public-retained-da",
    "node",
    "node",
    "node",
    "node",
    "da-committee-0",
    "da-committee-1",
    "watcher",
    "postgres",
  ];
  writeFileSync(
    layout.drillsLog,
    drillNames
      .map((drill, index) =>
        JSON.stringify({
          drill,
          target: targets[index],
          injectedAt: "2020-01-01T00:00:00.000Z",
          recoveredAt: "2020-01-01T00:00:01.000Z",
          ok: true,
          detail: "fixture",
        }),
      )
      .join("\n") + "\n",
  );
  const journey = join(layout.journeyDir, "journey.json");
  writeFileSync(
    journey,
    JSON.stringify({
      schemaVersion: "midgard-devnet-journal-v1",
      entries: {
        "phase:holdings": "done",
        ...Object.fromEntries(
          fixture.records.map((row, index) => [
            `withdrawal:W${index + 1}`,
            row,
          ]),
        ),
      },
    }),
  );
  controls.manifest.mockReturnValue({
    manifestId: "33".repeat(32),
    network: "Custom",
    contracts: {
      withdrawalMint: { scriptHash: fixture.config.withdrawalPolicyId },
      withdrawalSpend: { scriptHash: fixture.config.withdrawalPolicyId },
      payoutMint: { scriptHash: fixture.config.payoutPolicyId },
      payoutSpend: { scriptHash: fixture.config.payoutPolicyId },
    },
    l1Finality: { confirmationDepth: 2160 },
  });
  controls.sql.mockResolvedValue(fixture.snapshot);
  // The real collection helper and pure codecs run. Native source ownership is tested separately by its adapter unit.
  controls.capture.mockImplementation(async (_options, use) => ({
    value: await use({ ...fixture.scope }),
    boundary: {
      deploymentManifestId: "33".repeat(32),
      point: fixture.scope.point,
      generation: "7",
    },
  }));
  const proofs = fixture.inputs.map((input) =>
    verifyAcceptancePayoutLineage(input, fixture.config),
  );
  const frame = JSON.stringify({
    jsonrpc: "2.0",
    id: "owned",
    result: proofs.map((proof) => ({
      transaction: { id: proof.beneficiary.txHash },
      index: proof.beneficiary.outputIndex,
      address: proof.address,
      value: { ada: { lovelace: "ADA" }, ["55".repeat(28)]: { ab: "ASSET" } },
    })),
  })
    .replaceAll('"ADA"', "9007199254740993")
    .replaceAll('"ASSET"', "9007199254740995");
  vi.mocked(fixture.scope.queryExactOutRefs).mockResolvedValue(frame);
  // Replace only the owned Kupo transport with the source fixture, keeping actual readers/codecs.
  return { fixture, layout, journey, frame, proofs };
};

vi.mock(
  "../src/devnet-stack/acceptance-payout-sources.js",
  async (original) => {
    const actual =
      await original<
        typeof import("../src/devnet-stack/acceptance-payout-sources.js")
      >();
    return { ...actual, openAcceptanceKupoReads: vi.fn() };
  },
);
import { openAcceptanceKupoReads } from "../src/devnet-stack/acceptance-payout-sources.js";
const wire = (fixture: ReturnType<typeof collectorFixture>) =>
  vi.mocked(openAcceptanceKupoReads).mockReturnValue({
    fetchImpl: fixture.fetchImpl,
    close: vi.fn(async () => {}),
  });

it("publishes all four full-value receipts only after two exact queries and refreshed history", async () => {
  const { fixture, layout } = setup();
  wire(fixture);
  const receipt = await sampleAcceptancePayouts(layout, limits);
  expect(receipt.current).toHaveLength(4);
  expect(
    receipt.current.every((row) => row.assets.lovelace === "9007199254740993"),
  ).toBe(true);
  expect(fixture.scope.queryExactOutRefs).toHaveBeenCalledTimes(2);
  expect(controls.sql).toHaveBeenCalledTimes(2);
  expect(fixture.scope.queryExactOutRefs).toHaveBeenLastCalledWith(
    receipt.lineages.map((row) => row.beneficiary),
  );
});
it("holds if final current outref is spent or changes despite an earlier positive query", async () => {
  const { fixture, layout, frame, proofs } = setup();
  wire(fixture);
  vi.mocked(fixture.scope.queryExactOutRefs)
    .mockResolvedValueOnce(frame)
    .mockResolvedValueOnce(
      frame.replace(proofs[0]!.beneficiary.txHash, "00".repeat(32)),
    );
  await expect(sampleAcceptancePayouts(layout, limits)).rejects.toThrow(
    /current exact beneficiary/,
  );
});
it("holds on a fresh history generation change and never makes the second payout query", async () => {
  const { fixture, layout } = setup();
  wire(fixture);
  controls.sql
    .mockResolvedValueOnce(fixture.snapshot)
    .mockResolvedValueOnce({ ...fixture.snapshot, generation: "10" });
  await expect(sampleAcceptancePayouts(layout, limits)).rejects.toThrow(
    /evidence changed/,
  );
  expect(fixture.scope.queryExactOutRefs).toHaveBeenCalledTimes(1);
});
it("rejects final native revocation and changed journal before receipt publication", async () => {
  const { fixture, layout, frame, journey } = setup();
  wire(fixture);
  vi.mocked(fixture.scope.queryExactOutRefs)
    .mockResolvedValueOnce(frame)
    .mockImplementationOnce(async () => {
      fixture.controller.abort();
      return frame;
    });
  await expect(sampleAcceptancePayouts(layout, limits)).rejects.toThrow(
    /native revoked/,
  );
  const second = setup();
  wire(second.fixture);
  vi.mocked(second.fixture.scope.queryExactOutRefs)
    .mockResolvedValueOnce(second.frame)
    .mockImplementationOnce(async () => {
      writeFileSync(join(second.layout.journeyDir, "journey.json"), "{}");
      return second.frame;
    });
  await expect(sampleAcceptancePayouts(second.layout, limits)).rejects.toThrow(
    /journal changed/,
  );
  expect(journey).not.toBe(second.journey);
});
it("refuses mismatched final deployment authority and unfinished journey", async () => {
  const { fixture, layout, journey } = setup();
  wire(fixture);
  controls.capture.mockImplementation(async (_options, use) => ({
    value: await use(fixture.scope),
    boundary: { deploymentManifestId: "44".repeat(32) },
  }));
  await expect(sampleAcceptancePayouts(layout, limits)).rejects.toThrow(
    /different deployment/,
  );
  controls.capture.mockClear();
  writeFileSync(
    journey,
    JSON.stringify({ schemaVersion: "midgard-devnet-journal-v1", entries: {} }),
  );
  await expect(sampleAcceptancePayouts(layout, limits)).rejects.toThrow(
    /full journey/,
  );
  expect(controls.capture).not.toHaveBeenCalled();
});
it("requires all twelve actual successful drill records, bounded evidence and no owed restore", async () => {
  const { fixture, layout } = setup();
  wire(fixture);
  await expect(
    sampleAcceptancePayouts(layout, { ...limits, maxDrillEvidenceBytes: 1 }),
  ).rejects.toThrow(/byte bound/);
  expect(controls.capture).not.toHaveBeenCalled();
  writeFileSync(layout.drillsLog, "[]\n");
  await expect(sampleAcceptancePayouts(layout, limits)).rejects.toThrow(
    /expected 12 drills/,
  );
  expect(controls.capture).not.toHaveBeenCalled();
  const second = setup();
  wire(second.fixture);
  writeFileSync(`${second.layout.drillsLog}.restore.json`, "{}");
  await expect(sampleAcceptancePayouts(second.layout, limits)).rejects.toThrow(
    /owed restore/,
  );
  expect(controls.capture).not.toHaveBeenCalled();
});
