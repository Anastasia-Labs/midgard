import { chmod, realpath, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { NativeLedgerKupmios } from "@al-ft/midgard-core/native-reward-account";
import {
  credentialToRewardAddress,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import type { NativeLedgerConfig } from "../src/config.js";
import { lucidFromProviderUrl } from "../src/l1/lucid.js";
import { tempDir } from "./helpers.js";

const KUPMIOS_URL = "kupmios:http://127.0.0.1:1442|ws://127.0.0.1:1337";
const SCRIPT_HASH = "ab".repeat(28);
const rewardAddress = credentialToRewardAddress("Preprod", {
  type: "Script",
  hash: SCRIPT_HASH,
});

// Answers the helper's startup line the way the native chain-sync helper
// does for a registered, undelegated script reward account.
const HELPER_SOURCE = `#!/usr/bin/env node
const { createHash } = require("node:crypto");
let input = "";
process.stdin.on("data", (chunk) => (input += chunk));
process.stdin.on("end", () => {
  const startup = input.replace(/\\n$/u, "");
  const request = JSON.parse(startup);
  process.stdout.write(
    JSON.stringify({
      credential: request.operation.credential,
      depositLovelace: "2000000",
      kind: "reward_account",
      point: { blockHash: "cd".repeat(32), blockNo: "7", slot: "42" },
      poolIdHash: null,
      registered: true,
      rewardsLovelace: "0",
      schemaVersion: request.schemaVersion,
      startupDigest: createHash("sha256").update(startup, "utf8").digest("hex"),
    }) + "\\n",
  );
});
`;

const writeLocalLedger = async (): Promise<NativeLedgerConfig> => {
  const dir = await realpath(await tempDir());
  const nodeConfigPath = join(dir, "config.json");
  const socketPath = join(dir, "node.socket");
  const binaryPath = join(dir, "midgard-chain-sync.cjs");
  await writeFile(join(dir, "shelley-genesis.json"), '{"networkMagic":1}');
  await writeFile(
    nodeConfigPath,
    JSON.stringify({ ShelleyGenesisFile: "shelley-genesis.json" }),
  );
  await writeFile(socketPath, "");
  await writeFile(binaryPath, HELPER_SOURCE);
  await chmod(binaryPath, 0o755);
  return {
    authorityNodeId: "local-cardano-node",
    socketPath,
    nodeConfigPath,
    binaryPath,
  };
};

describe("lucidFromProviderUrl", () => {
  beforeEach(() => {
    // Lucid reads protocol parameters at construction; no Ogmios is running.
    vi.spyOn(
      NativeLedgerKupmios.prototype,
      "getProtocolParameters",
    ).mockResolvedValue(PROTOCOL_PARAMETERS_DEFAULT);
  });
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("builds Kupmios providers whose reward-account state comes from the local ledger", async () => {
    const { lucid, providerSource } = await lucidFromProviderUrl(
      KUPMIOS_URL,
      "Preprod",
      undefined,
    );
    expect(lucid.config().provider).toBeInstanceOf(NativeLedgerKupmios);
    expect(providerSource).toBe(
      "kupmios:http://127.0.0.1:1442|ws://127.0.0.1:1337",
    );
  });

  it("refuses reward-account reads when no local node ledger is configured", async () => {
    const { lucid } = await lucidFromProviderUrl(
      KUPMIOS_URL,
      "Preprod",
      undefined,
    );
    await expect(
      lucid.config().provider!.getRewardAccount!(rewardAddress),
    ).rejects.toThrow(/Reward-account state requires a local node ledger/u);
    await expect(lucid.rewardAccountAt(rewardAddress)).rejects.toThrow(
      /Reward-account state requires a local node ledger/u,
    );
  });

  it("reads registration of an undelegated script account from the configured ledger", async () => {
    const nativeLedger = await writeLocalLedger();
    const { lucid } = await lucidFromProviderUrl(
      KUPMIOS_URL,
      "Preprod",
      nativeLedger,
    );
    await expect(lucid.rewardAccountAt(rewardAddress)).resolves.toEqual({
      registered: true,
      rewards: 0n,
      poolId: null,
    });
    // The authority is resolved once: later reads do not re-read the node
    // configuration.
    await rm(nativeLedger.nodeConfigPath);
    await expect(lucid.rewardAccountAt(rewardAddress)).resolves.toMatchObject({
      registered: true,
    });
  });

  it("retries authority resolution after a failure instead of caching it", async () => {
    const nativeLedger = await writeLocalLedger();
    const nodeConfig = nativeLedger.nodeConfigPath;
    const moved = `${nodeConfig}.absent`;
    const { lucid } = await lucidFromProviderUrl(KUPMIOS_URL, "Preprod", {
      ...nativeLedger,
      nodeConfigPath: moved,
    });
    await expect(lucid.rewardAccountAt(rewardAddress)).rejects.toThrow(
      /ENOENT/u,
    );
    await writeFile(
      moved,
      JSON.stringify({ ShelleyGenesisFile: "shelley-genesis.json" }),
    );
    await expect(lucid.rewardAccountAt(rewardAddress)).resolves.toMatchObject({
      registered: true,
    });
  });

  it("refuses a local ledger whose genesis belongs to another network", async () => {
    const nativeLedger = await writeLocalLedger();
    const { lucid } = await lucidFromProviderUrl(
      KUPMIOS_URL,
      "Preview",
      nativeLedger,
    );
    await expect(
      lucid.rewardAccountAt(
        credentialToRewardAddress("Preview", {
          type: "Script",
          hash: SCRIPT_HASH,
        }),
      ),
    ).rejects.toThrow(/differs from network Preview/u);
  });
});
