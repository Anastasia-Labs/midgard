/**
 * `runUserCli` against a deployed emulator scene: an identity-valid deployment
 * manifest describing the scene, the user's seed in a mode-0600 file, a
 * record root in a fresh directory, and deps whose L1 view is the emulator
 * and whose node is a stub the test describes.
 */
import { mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
} from "@al-ft/midgard-core/consensus-profile";
import {
  computeDeploymentManifestId,
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  daBondManifestAmounts,
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "@al-ft/midgard-core/deployment-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  loadUserHistoryBlueprint,
  setupUserHistoryScene,
  userHistoryManifestSections,
  type UserHistoryScene,
} from "@al-ft/midgard-sdk/test-support/user-history";
import { Lucid } from "@lucid-evolution/lucid";

import { type UserCliDeps } from "../../src/deps.js";
import { type UserNode } from "../../src/node.js";
import { runUserCli } from "../../src/run.js";

export const SCENE_TIMEOUT_MS = 300_000;

/** Everything in a manifest the identity check reads, around the scene's
 * user-history sections; on the selected profile's network. */
export const manifestIdentityInput = (
  sections: Readonly<Record<string, unknown>> = {},
): Record<string, unknown> => {
  const network = SELECTED_DEPLOYMENT_PROFILE.network;
  return {
    schemaVersion: MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    deploymentProfile: SELECTED_DEPLOYMENT_PROFILE,
    deploymentProfileDigest: SELECTED_DEPLOYMENT_PROFILE_DIGEST,
    l1SlotConfig:
      NAMED_NETWORK_L1_SLOT_CONFIGS[
        network as keyof typeof NAMED_NETWORK_L1_SLOT_CONFIGS
      ],
    cardanoProtocolParameters: {},
    genesis: {},
    createdAt: "2026-11-03T00:00:00.000Z",
    updatedAt: "2026-11-03T00:00:00.000Z",
    referenceScriptDeployAddress: "addr_test1reference",
    hubOracleOneShot: {},
    referenceScriptAuthPolicy: {},
    contracts: {},
    referenceScripts: {},
    da: {},
    artifacts: {},
    steps: {},
    validationDispute: {},
    l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
    economics:
      DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
    availabilityChallenge: {
      responseClasses: {
        smallPayloadMaxBytes: 65_536,
        smallResponseWindowMs:
          SELECTED_DEPLOYMENT_PROFILE.timing.da_small_response_window_ms,
        fullPayloadMaxBytes: 67_108_864,
        fullResponseWindowMs:
          SELECTED_DEPLOYMENT_PROFILE.timing.da_full_response_window_ms,
      },
      responseGeometry: {
        chunkByteLength: 14_020,
        trancheByteLength: 4_194_304,
        maxTrancheCount: 16,
      },
      ...daBondManifestAmounts(),
      challengerBondLovelace: 10_000_000_000,
      maxOpenFeeLovelace: 500_000,
      maxPublicationFeeLovelace: 500_000,
      maxSettlementFeeLovelace: 500_000,
      maxCloseFeeLovelace: 1_000_000,
      maxTimeoutFeeLovelace: 1_200_000,
    },
    ...sections,
    network,
  };
};

export const withManifestId = (input: Record<string, unknown>) => ({
  ...input,
  manifestId: computeDeploymentManifestId(input),
});

export const tempDirectory = (prefix: string): string =>
  mkdtempSync(join(tmpdir(), `midgard-user-cli-${prefix}-`));

export const writeSeedFile = (directory: string, seedPhrase: string) => {
  const path = join(directory, "wallet-seed");
  writeFileSync(path, seedPhrase, { mode: 0o600 });
  return path;
};

/** What `--json` prints for a landed submission. */
export type PrintedOutcome = {
  readonly kind: string;
  readonly submissionId: string;
  readonly eventId: string;
  readonly admissionTxHash: string;
  readonly publicationTxHash?: string;
  readonly slot: number;
  readonly attempts: number;
};

/** One `attempt-NNNN.json` file as written. */
export type AttemptFileJson = {
  readonly version: number;
  readonly attempt: number;
  readonly phase: string;
  readonly nonce: string;
  readonly eventId: string;
  readonly publication: string | null;
  readonly signedTxCbor: string;
  readonly txHash: string;
  readonly ttlSlot: number;
};

export const printedOutcome = (stdout: string) =>
  JSON.parse(stdout) as PrintedOutcome;

export const printedJson = (stdout: string) => JSON.parse(stdout) as unknown;

export const jsonFile = (path: string) =>
  JSON.parse(readFileSync(path, "utf8")) as unknown;

export const attemptFile = (path: string) =>
  JSON.parse(readFileSync(path, "utf8")) as AttemptFileJson;

export type CliRun = {
  readonly code: number;
  readonly stdout: string;
  readonly stderr: string;
};

export type UserCliHarness = Awaited<ReturnType<typeof setupUserCliHarness>>;

/** The stub node serves this deployment and the given output, committed. */
export const stubNode = (
  manifestId: string,
  output: Awaited<ReturnType<UserNode["utxoByOutRef"]>>,
  producerStatus = "committed",
): UserNode => ({
  deploymentManifestId: async () => manifestId,
  utxoByOutRef: async () => output,
  txStatus: async () => producerStatus,
});

export const setupUserCliHarness = async () => {
  const scene: UserHistoryScene = await setupUserHistoryScene({
    blueprint: loadUserHistoryBlueprint(),
  });
  const directory = tempDirectory("scene");
  const manifest = withManifestId(
    manifestIdentityInput(userHistoryManifestSections(scene)),
  );
  const manifestPath = join(directory, "manifest.json");
  writeFileSync(manifestPath, JSON.stringify(manifest));
  const seedFile = writeSeedFile(directory, scene.user.seedPhrase);
  const slotConfig = {
    zeroTime: scene.emulator.now() - scene.emulator.slot * 1000,
    zeroSlot: 0,
    slotLength: 1000,
  };

  const deps = (overrides: Partial<UserCliDeps> = {}) => {
    const out: string[] = [];
    const err: string[] = [];
    const value: UserCliDeps = {
      env: {},
      stdout: (text) => out.push(text),
      stderr: (text) => err.push(text),
      homeDir: directory,
      now: scene.clock.now,
      sleep: scene.clock.sleep,
      fetch: async () => {
        throw new Error("This test reaches no HTTP endpoint");
      },
      // As the real view: Lucid on the manifest's network, the emulator
      // standing in for the indexer.
      openL1View: async ({ network, seedPhrase }) => {
        const lucid = await Lucid(scene.emulator, network, { slotConfig });
        if (seedPhrase !== undefined) lucid.selectWallet.fromSeed(seedPhrase);
        return { lucid, indexedSlot: async () => scene.emulator.slot };
      },
      connectNode: async () => {
        throw new Error("This test reaches no node");
      },
      connectL2: async () => {
        throw new Error("This test reaches no L2 node");
      },
      ...overrides,
    };
    return {
      deps: value,
      output: () => ({ stdout: out.join(""), stderr: err.join("") }),
    };
  };

  /** Runs `midgard-user` with the scene's manifest, seed and a Blockfrost
   * access the injected view stands in for; a record directory only for the
   * commands that keep records. */
  const run = async (
    argv: readonly string[],
    {
      recordDir,
      overrides,
      access = [
        "--l1",
        "blockfrost",
        "--blockfrost-url",
        "http://127.0.0.1:9/unused",
        "--blockfrost-project-id",
        "unused",
      ],
    }: {
      readonly recordDir?: string;
      readonly overrides?: Partial<UserCliDeps>;
      readonly access?: readonly string[];
    },
  ): Promise<CliRun> => {
    const { deps: value, output } = deps(overrides);
    const [command, ...rest] = argv;
    const code = await runUserCli(
      [
        command!,
        ...rest,
        "--manifest",
        manifestPath,
        ...(recordDir === undefined ? [] : ["--record-dir", recordDir]),
        ...access,
        ...(command === "status"
          ? []
          : ["--wallet-seed-phrase-file", seedFile]),
      ],
      value,
    );
    return { code, ...output() };
  };

  return {
    scene,
    manifest,
    manifestPath,
    seedFile,
    directory,
    slotConfig,
    deps,
    run,
  };
};
