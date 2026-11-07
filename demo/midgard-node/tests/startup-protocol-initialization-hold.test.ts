/**
 * The startup protocol check (`ensureProtocolInitializedOnStartup`) never
 * deploys: with the deployment's finalized manifest it only checks the
 * availability-challenge reward accounts, by keyed reward-account queries,
 * reading no address and submitting nothing; without the manifest it fails
 * under `deployment_manifest_missing`, reading nothing. A Lucid slot mapping
 * other than the deployment's fails it at once under
 * `l1_slot_config_mismatch`. An unregistered reward account is a verdict and
 * fails it at once; a reward-account read that stays unavailable is waited
 * out under `STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS`. The node's ledger
 * is a double that records every query and submission.
 */
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  CborMap,
  encodeCbor,
  type LedgerQuery,
} from "@al-ft/l1-node-transport";
import { fakeL1NodeTransport } from "@al-ft/l1-node-transport/testing";
import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type L1SlotConfig,
  NAMED_NETWORK_L1_SLOT_CONFIGS,
} from "@al-ft/midgard-core/l1-slot-config";
import { LedgerProvider } from "@al-ft/midgard-l1-follower/provider";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  Lucid as makeLucid,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";
import { Effect, Exit } from "effect";
import {
  afterAll,
  afterEach,
  beforeAll,
  describe,
  expect,
  it,
  vi,
} from "vitest";

import { ensureProtocolInitializedOnStartup } from "../src/commands/listen-startup.js";
import {
  ContractDeploymentIdentity,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import {
  AVAILABILITY_REWARD_ACCOUNT_UNREGISTERED,
  DEPLOYMENT_MANIFEST_MISSING,
  findStartupStepFailure,
  L1_SLOT_CONFIG_MISMATCH,
  REWARD_ACCOUNT_STATUS_UNAVAILABLE,
  StartupWaitingReporter,
} from "../src/services/startup-waiting.js";
import { makeFinalizedDeploymentManifestFixture } from "./helpers/finalized-deployment-manifest.js";
import { lucidServiceStub } from "./helpers/lucid-service-stub.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const MAX_ATTEMPTS = 4;
const PREPROD = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;
const NONCE = { txHash: "ab".repeat(32), outputIndex: 0 };
const REWARD_QUERIES = [
  "stake_deleg_deposits",
  "filtered_delegations_and_rewards",
];

let contracts: SDK.MidgardValidators;
let manifest: DeploymentManifest;
let manifestDirectory: string;

beforeAll(async () => {
  contracts = await loadRealMidgardContractsForTest(NONCE);
  manifest = await makeFinalizedDeploymentManifestFixture({
    contracts,
    nonce: NONCE,
  });
  manifestDirectory = await mkdtemp(join(tmpdir(), "midgard-startup-"));
  await writeFile(
    join(manifestDirectory, "contract-deployment-info.json"),
    JSON.stringify(manifest),
  );
});

afterEach(() => {
  vi.unstubAllEnvs();
});

afterAll(async () => {
  await rm(manifestDirectory, { recursive: true, force: true });
});

/**
 * The node's ledger: it answers the tip and the reward-account queries
 * (every account registered or none, the first `unreachableReads` reads
 * refused as a dropped connection) and refuses everything else. `queries`
 * lists every query it was asked; `submitted` counts submissions.
 */
const ledgerDouble = (rewards: {
  readonly registered: boolean;
  readonly unreachableReads?: number;
}) => {
  const queries: string[] = [];
  let submitted = 0;
  let rewardReads = 0;
  const key = ({ type, hash }: { type: "Key" | "Script"; hash: string }) => [
    type === "Key" ? 0n : 1n,
    Buffer.from(hash, "hex"),
  ];
  const answer = async (query: LedgerQuery): Promise<Uint8Array> => {
    queries.push(query.query);
    switch (query.query) {
      case "chain_point":
        return encodeCbor([1n, Buffer.alloc(32)]);
      case "chain_block_no":
        return encodeCbor([1n, 1n]);
      case "stake_deleg_deposits":
        rewardReads += 1;
        if (rewardReads <= (rewards.unreachableReads ?? 0))
          throw Object.assign(new Error("connect ECONNREFUSED"), {
            code: "ECONNREFUSED",
          });
        return encodeCbor(
          new CborMap(
            rewards.registered
              ? query.credentials.map((credential) => [
                  key(credential),
                  2_000_000n,
                ])
              : [],
          ),
        );
      case "filtered_delegations_and_rewards":
        return encodeCbor([
          new CborMap([]),
          new CborMap(
            rewards.registered
              ? query.credentials.map((credential) => [key(credential), 0n])
              : [],
          ),
        ]);
      case "system_start":
      case "current_era":
      case "era_history":
      case "protocol_params":
      case "utxo_by_address":
      case "utxo_by_txin":
        throw new Error(`the ledger double refuses ${query.query}`);
    }
  };
  const transport = fakeL1NodeTransport({
    query: answer,
    withLedgerState: async (_at, use) => await use({ query: answer }),
    submit: async () => {
      submitted += 1;
      return {
        accepted: false as const,
        rejection: Buffer.from("the ledger double refuses submissions"),
      };
    },
  });
  return {
    transport,
    queries,
    submitted: () => submitted,
  };
};

/** Runs the check over `node`, with every startup report recorded. */
const runCheck = async (
  node: ReturnType<typeof ledgerDouble>,
  options: {
    /** Whether the deployment's manifest is at the configured path. */
    readonly manifest?: boolean;
    /** The mapping the node's Lucid holds; absent is Preprod's. */
    readonly lucidSlots?: L1SlotConfig;
  } = {},
) => {
  vi.stubEnv(
    "MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH",
    join(
      manifestDirectory,
      options.manifest === false
        ? "absent.json"
        : "contract-deployment-info.json",
    ),
  );
  const provider = new LedgerProvider({
    transport: node.transport,
    pinPointMs: 0,
  });
  const lucid =
    options.lucidSlots === undefined
      ? await makeLucid(provider, "Preprod", {
          presetProtocolParameters: PROTOCOL_PARAMETERS_DEFAULT,
        })
      : await makeLucid(provider, "Custom", {
          slotConfig: options.lucidSlots,
          presetProtocolParameters: PROTOCOL_PARAMETERS_DEFAULT,
        });
  const reported: [string, readonly string[]][] = [];
  const exit = await Effect.runPromiseExit(
    ensureProtocolInitializedOnStartup.pipe(
      Effect.provideService(NodeConfig, {
        NETWORK: "Preprod",
        L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS:
          manifest.referenceScriptDeployAddress,
        HUB_ORACLE_ONE_SHOT_TX_HASH: NONCE.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: NONCE.outputIndex,
        DEPLOYMENT_ECONOMICS_PROFILE: manifest.economics.profile,
        // A bootstrapping node: it still never deploys.
        RUN_GENESIS_ON_STARTUP: true,
        STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS: MAX_ATTEMPTS,
        STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS: 0,
      } as unknown as NodeConfig["Type"]),
      Effect.provideService(Lucid, lucidServiceStub({ api: lucid })),
      Effect.provideService(ContractDeploymentIdentity, {
        kind: "manifest",
        manifest,
      } as unknown as ContractDeploymentIdentity),
      Effect.provideService(
        MidgardContracts,
        contracts as unknown as MidgardContracts,
      ),
      Effect.locally(StartupWaitingReporter, (key, reasons) =>
        Effect.sync(() => {
          reported.push([key, reasons]);
        }),
      ),
    ),
  );
  return {
    exit,
    reported,
    failure: Exit.isFailure(exit)
      ? findStartupStepFailure(exit.cause)
      : undefined,
  };
};

describe("the startup protocol check", () => {
  it("with the deployment's manifest, reads only the reward accounts by key and submits nothing", async () => {
    const node = ledgerDouble({ registered: true });
    const { exit } = await runCheck(node);
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(new Set(node.queries)).toEqual(
      new Set(["chain_point", "chain_block_no", ...REWARD_QUERIES]),
    );
    expect(node.submitted()).toBe(0);
  });

  it("without a manifest, fails under deployment_manifest_missing, reading and submitting nothing", async () => {
    const node = ledgerDouble({ registered: true });
    const { failure } = await runCheck(node, { manifest: false });
    expect(failure).toMatchObject({
      step: "protocol_initialization",
      reason: DEPLOYMENT_MANIFEST_MISSING,
      exhausted: false,
    });
    expect(node.queries).toEqual([]);
    expect(node.submitted()).toBe(0);
  });

  it("fails at once under l1_slot_config_mismatch when Lucid's slot mapping differs from the manifest's, reading nothing", async () => {
    const node = ledgerDouble({ registered: true });
    const { failure, reported } = await runCheck(node, {
      lucidSlots: { ...PREPROD, zeroTime: PREPROD.zeroTime + 1_000 },
    });
    expect(failure).toMatchObject({
      step: "protocol_initialization",
      reason: L1_SLOT_CONFIG_MISMATCH,
      exhausted: false,
    });
    expect(failure?.message).toMatch(/l1_slot_config_mismatch: runtime/u);
    expect(node.queries).toEqual([]);
    expect(reported).toEqual([]);
  });

  it("fails at once on an unregistered reward account, reading it once", async () => {
    const node = ledgerDouble({ registered: false });
    const { failure, reported } = await runCheck(node);
    expect(failure).toMatchObject({
      step: "availability_reward_accounts",
      reason: AVAILABILITY_REWARD_ACCOUNT_UNREGISTERED,
      exhausted: false,
      attempts: 1,
    });
    expect(
      node.queries.filter((query) => query === "stake_deleg_deposits"),
    ).toHaveLength(1);
    expect(reported).toEqual([]);
  });

  it("waits out an unreachable reward-account read within its budget", async () => {
    const node = ledgerDouble({
      registered: true,
      unreachableReads: MAX_ATTEMPTS - 1,
    });
    const { exit, reported } = await runCheck(node);
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(reported).toEqual([
      ...Array.from({ length: MAX_ATTEMPTS - 1 }, () => [
        "availability_reward_accounts",
        [REWARD_ACCOUNT_STATUS_UNAVAILABLE],
      ]),
      ["availability_reward_accounts", []],
    ]);
    expect(node.submitted()).toBe(0);
  });

  it("fails under reward_account_status_unavailable once the read outlives its budget", async () => {
    const node = ledgerDouble({
      registered: true,
      unreachableReads: Number.MAX_SAFE_INTEGER,
    });
    const { failure } = await runCheck(node);
    expect(failure).toMatchObject({
      step: "availability_reward_accounts",
      reason: REWARD_ACCOUNT_STATUS_UNAVAILABLE,
      exhausted: true,
      attempts: MAX_ATTEMPTS,
    });
  });
});
