/**
 * Everyday Day-1 user commands (`address`, `balance`, `send`) and automatic
 * Blockfrost URL defaulting per Cardano network.
 */
import {
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
} from "@al-ft/lucid-midgard";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import {
  generateSeedPhrase,
  getAddressDetails,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { type UserCliDeps } from "../src/deps.js";
import { DEFAULT_BLOCKFROST_URLS, resolveL1Access } from "../src/l1-view.js";
import { runUserCli } from "../src/run.js";
import { inProcessL2Node, l2Output } from "./support/l2-node.js";
import { tempDirectory, writeSeedFile } from "./support/user-cli.js";

const SENDER_SEED = generateSeedPhrase();
const RECIPIENT_SEED = generateSeedPhrase();
const PREPROD_SENDER = walletFromSeed(SENDER_SEED, { network: "Preprod" });
const MAINNET_SENDER = walletFromSeed(SENDER_SEED, { network: "Mainnet" });
const PREPROD_RECIPIENT = walletFromSeed(RECIPIENT_SEED, {
  network: "Preprod",
});
const HUB_ORACLE_POLICY_ID = "11".repeat(28);
const ASSET_POLICY = "ab".repeat(28);
const ASSET_NAME = "746f6b656e";
const ASSET_UNIT = `${ASSET_POLICY}${ASSET_NAME}`;

const PREPROD_SLOT_CONFIG = {
  zeroTime: 1_654_041_600_000,
  zeroSlot: 0,
  slotLength: 1000,
} as const;

const l2OutputWithAssets = (
  address: string,
  producer: number,
  assets: Readonly<Record<string, bigint>>,
): MidgardUtxo => {
  const outRef: OutRef = {
    txHash: producer.toString(16).padStart(2, "0").repeat(32),
    outputIndex: 1,
  };
  return decodeMidgardUtxo({
    outRef,
    outRefCbor: outRefToCbor(outRef),
    outputCbor: encodeMidgardTxOutput(address, assets),
  });
};

const runCli = async (
  argv: readonly string[],
  options: {
    readonly env?: Readonly<Record<string, string | undefined>>;
    readonly connectL2?: (endpoint: string) => Promise<MidgardProvider>;
  } = {},
) => {
  const out: string[] = [];
  const err: string[] = [];
  const unreachable = async (): Promise<never> => {
    throw new Error("unreachable in day1-commands test");
  };
  const deps: UserCliDeps = {
    env: options.env ?? { USER_SEED: SENDER_SEED },
    stdout: (text) => out.push(text),
    stderr: (text) => err.push(text),
    homeDir: tempDirectory("day1-home"),
    now: Date.now,
    sleep: async () => undefined,
    fetch: unreachable,
    openL1View: unreachable,
    connectNode: unreachable,
    connectL2: options.connectL2 ?? unreachable,
  };
  const code = await runUserCli(argv, deps);
  return { code, stdout: out.join(""), stderr: err.join("") };
};

describe("Blockfrost URL defaulting", () => {
  it.each([
    ["Preprod", DEFAULT_BLOCKFROST_URLS.Preprod],
    ["Mainnet", DEFAULT_BLOCKFROST_URLS.Mainnet],
    ["Preview", DEFAULT_BLOCKFROST_URLS.Preview],
  ] as const)(
    "defaults --blockfrost-url from %s when omitted",
    (network, expectedUrl) => {
      const access = resolveL1Access(
        { l1: "blockfrost", blockfrostProjectId: "proj-123" },
        {},
        network,
      );
      expect(access).toEqual({
        kind: "blockfrost",
        url: expectedUrl,
        projectId: "proj-123",
      });
    },
  );

  it("lets explicit --blockfrost-url and L1_BLOCKFROST_URL override the default", () => {
    expect(
      resolveL1Access(
        {
          l1: "blockfrost",
          blockfrostUrl: "https://custom.example/api/v0/",
          blockfrostProjectId: "proj-123",
        },
        { L1_BLOCKFROST_URL: "https://env.example/api/v0" },
        "Preprod",
      ),
    ).toEqual({
      kind: "blockfrost",
      url: "https://custom.example/api/v0",
      projectId: "proj-123",
    });

    expect(
      resolveL1Access(
        { l1: "blockfrost", blockfrostProjectId: "proj-123" },
        { L1_BLOCKFROST_URL: "https://env.example/api/v0/" },
        "Preprod",
      ),
    ).toEqual({
      kind: "blockfrost",
      url: "https://env.example/api/v0",
      projectId: "proj-123",
    });
  });

  it("refuses blockfrost on Custom network and refuses blank explicit --blockfrost-url", () => {
    expect(() =>
      resolveL1Access(
        { l1: "blockfrost", blockfrostProjectId: "proj-123" },
        {},
        "Custom",
      ),
    ).toThrow(/named networks only/u);
    expect(() =>
      resolveL1Access(
        {
          l1: "blockfrost",
          blockfrostUrl: "   ",
          blockfrostProjectId: "proj-123",
        },
        {},
        "Preprod",
      ),
    ).toThrow(/--blockfrost-url must not be empty/u);
  });
});

describe("midgard-user address", () => {
  it("derives Preprod and Mainnet addresses from --seed-env and --seed-file", async () => {
    const preprod = await runCli([
      "address",
      "--seed-env",
      "USER_SEED",
      "--network",
      "preprod",
    ]);
    expect(preprod.code).toBe(0);
    expect(preprod.stderr).toBe("");
    expect(preprod.stdout.trim()).toBe(PREPROD_SENDER.address);

    const seedDir = tempDirectory("addr-seed");
    const seedFile = writeSeedFile(seedDir, SENDER_SEED);
    const mainnetJson = await runCli([
      "address",
      "--seed-file",
      seedFile,
      "--network",
      "Mainnet",
      "--json",
    ]);
    expect(mainnetJson.code).toBe(0);
    const parsed = JSON.parse(mainnetJson.stdout) as {
      readonly network: string;
      readonly address: string;
      readonly paymentKeyHash: string;
    };
    expect(parsed).toEqual({
      network: "Mainnet",
      address: MAINNET_SENDER.address,
      paymentKeyHash: getAddressDetails(MAINNET_SENDER.address)
        .paymentCredential?.hash,
    });
  });

  it("refuses missing network/manifest and conflicting seed flags", async () => {
    const noNetwork = await runCli(["address", "--seed-env", "USER_SEED"]);
    expect(noNetwork.code).toBe(1);
    expect(noNetwork.stderr).toContain("--network");

    const both = await runCli([
      "address",
      "--seed-env",
      "USER_SEED",
      "--wallet-seed-phrase-env",
      "USER_SEED",
      "--network",
      "Preprod",
    ]);
    expect(both.code).toBe(1);
    expect(both.stderr).toContain("not both");
  });
});

describe("midgard-user balance", () => {
  it("prints total lovelace, native assets, and txHash#index outRefs for --address and --seed-env", async () => {
    const utxos = [
      l2Output(PREPROD_SENDER.address, 0xa1, 5_000_000n),
      l2OutputWithAssets(PREPROD_SENDER.address, 0xa2, {
        lovelace: 2_500_000n,
        [ASSET_UNIT]: 42n,
      }),
    ];
    const l2 = inProcessL2Node({
      hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
      outputs: utxos,
      l1SlotConfig: PREPROD_SLOT_CONFIG,
    });

    const plain = await runCli(
      [
        "balance",
        "--node-endpoint",
        "http://127.0.0.1:9/unused",
        "--address",
        PREPROD_SENDER.address,
      ],
      { connectL2: async () => l2.provider },
    );
    expect(plain.code).toBe(0);
    expect(plain.stderr).toBe("");
    expect(plain.stdout).toContain(`address: ${PREPROD_SENDER.address}`);
    expect(plain.stdout).toContain("lovelace: 7500000 (7.500000 ADA)");
    expect(plain.stdout).toContain(`${ASSET_UNIT}: 42`);
    expect(plain.stdout).toContain(`${"a1".repeat(32)}#0`);
    expect(plain.stdout).toContain(`${"a2".repeat(32)}#1`);

    const fromSeedJson = await runCli(
      [
        "balance",
        "--node-endpoint",
        "http://127.0.0.1:9/unused",
        "--seed-env",
        "USER_SEED",
        "--json",
      ],
      { connectL2: async () => l2.provider },
    );
    expect(fromSeedJson.code).toBe(0);
    expect(JSON.parse(fromSeedJson.stdout)).toEqual({
      address: PREPROD_SENDER.address,
      totalLovelace: "7500000",
      assets: { [ASSET_UNIT]: "42" },
      utxoCount: 2,
      utxos: [
        {
          outRef: `${"a1".repeat(32)}#0`,
          txHash: "a1".repeat(32),
          outputIndex: 0,
          lovelace: "5000000",
          assets: {},
        },
        {
          outRef: `${"a2".repeat(32)}#1`,
          txHash: "a2".repeat(32),
          outputIndex: 1,
          lovelace: "2500000",
          assets: { [ASSET_UNIT]: "42" },
        },
      ],
    });
  });

  it("refuses when neither or both --address and seed source are provided", async () => {
    const neither = await runCli([
      "balance",
      "--node-endpoint",
      "http://127.0.0.1:9/unused",
    ]);
    expect(neither.code).toBe(1);
    expect(neither.stderr).toContain("--address");

    const both = await runCli([
      "balance",
      "--node-endpoint",
      "http://127.0.0.1:9/unused",
      "--address",
      PREPROD_SENDER.address,
      "--seed-env",
      "USER_SEED",
    ]);
    expect(both.code).toBe(1);
    expect(both.stderr).toContain("not both");
  });
});

describe("midgard-user send", () => {
  it("builds, signs, and submits an ordinary L2 transfer (with and without --wait)", async () => {
    const outputs = [l2Output(PREPROD_SENDER.address, 0xb1, 10_000_000n)];
    const l2NoWait = inProcessL2Node({
      hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
      outputs,
      l1SlotConfig: PREPROD_SLOT_CONFIG,
    });

    const submitted = await runCli(
      [
        "send",
        "--node-endpoint",
        "http://127.0.0.1:9/unused",
        "--seed-env",
        "USER_SEED",
        "--to",
        PREPROD_RECIPIENT.address,
        "--lovelace",
        "2000000",
        "--json",
      ],
      { connectL2: async () => l2NoWait.provider },
    );
    expect(submitted.stderr).toBe("");
    expect(submitted.code).toBe(0);
    const printed = JSON.parse(submitted.stdout) as {
      readonly txHash: string;
      readonly status: string;
      readonly from: string;
      readonly to: string;
      readonly lovelace: string;
    };
    expect(printed.txHash).toMatch(/^[0-9a-f]{64}$/u);
    expect(printed.status).toBe("queued");
    expect(printed.from).toBe(PREPROD_SENDER.address);
    expect(printed.to).toBe(PREPROD_RECIPIENT.address);
    expect(printed.lovelace).toBe("2000000");
    expect(l2NoWait.submitted()).toHaveLength(1);

    const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(l2NoWait.submitted()[0]!, "hex"),
    );
    const decodedOutputs = decodeMidgardNativeByteListPreimage(
      decoded.body.outputsPreimageCbor,
      "outputs",
    ).map((raw) => decodeMidgardTxOutput(raw));
    const recipientBytes = midgardAddressFromText(PREPROD_RECIPIENT.address);
    expect(
      decodedOutputs.some(
        (output) =>
          output.address.equals(recipientBytes) &&
          output.value.lovelace === 2_000_000n,
      ),
    ).toBe(true);

    const l2Wait = inProcessL2Node({
      hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
      outputs,
      l1SlotConfig: PREPROD_SLOT_CONFIG,
    });
    const waited = await runCli(
      [
        "send",
        "--node-endpoint",
        "http://127.0.0.1:9/unused",
        "--seed-env",
        "USER_SEED",
        "--to",
        PREPROD_RECIPIENT.address,
        "--lovelace",
        "2000000",
        "--wait",
      ],
      { connectL2: async () => l2Wait.provider },
    );
    expect(waited.stderr).toBe("");
    expect(waited.code).toBe(0);
    expect(waited.stdout).toMatch(
      /L2 transfer accepted: tx [0-9a-f]{64}, 2000000 lovelace -> /u,
    );
  });

  it("refuses when the node rejects on --wait or when the wallet holds no L2 UTxOs", async () => {
    const outputs = [l2Output(PREPROD_SENDER.address, 0xc1, 10_000_000n)];
    const spentLedger = inProcessL2Node({
      hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
      outputs,
      ledger: [],
      l1SlotConfig: PREPROD_SLOT_CONFIG,
    });
    const rejected = await runCli(
      [
        "send",
        "--node-endpoint",
        "http://127.0.0.1:9/unused",
        "--seed-env",
        "USER_SEED",
        "--to",
        PREPROD_RECIPIENT.address,
        "--lovelace",
        "2000000",
        "--wait",
      ],
      { connectL2: async () => spentLedger.provider },
    );
    expect(rejected.code).toBe(1);
    expect(rejected.stderr).toMatch(
      /The node rejected L2 transaction [0-9a-f]{64}: /u,
    );

    const empty = inProcessL2Node({
      hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
      outputs: [],
      l1SlotConfig: PREPROD_SLOT_CONFIG,
    });
    const noUtxos = await runCli(
      [
        "send",
        "--node-endpoint",
        "http://127.0.0.1:9/unused",
        "--seed-env",
        "USER_SEED",
        "--to",
        PREPROD_RECIPIENT.address,
        "--lovelace",
        "2000000",
      ],
      { connectL2: async () => empty.provider },
    );
    expect(noUtxos.code).toBe(1);
    expect(noUtxos.stderr).toContain("holds no L2 output");
  });
});
