import { realpath, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { closeSharedL1NodeTransports } from "@al-ft/l1-node-transport";
import {
  LEDGER_HANDLER,
  writeFakeSidecar,
} from "@al-ft/l1-node-transport/testing/fake-sidecar";
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
const PREPROD_MAGIC = 1;
const PREVIEW_MAGIC = 2;
const SCRIPT_HASH = "ab".repeat(28);
const rewardAddress = credentialToRewardAddress("Preprod", {
  type: "Script",
  hash: SCRIPT_HASH,
});

const writeLocalLedger = async (): Promise<NativeLedgerConfig> => {
  const dir = await realpath(await tempDir());
  const nodeConfigPath = join(dir, "config.json");
  const socketPath = join(dir, "node.socket");
  const binaryPath = join(dir, "node-transport");
  await writeFile(join(dir, "shelley-genesis.json"), '{"networkMagic":1}');
  await writeFile(
    nodeConfigPath,
    JSON.stringify({ ShelleyGenesisFile: "shelley-genesis.json" }),
  );
  await writeFile(socketPath, "");
  // The node ledger registers the script account, undelegated.
  await writeFakeSidecar({
    path: binaryPath,
    handlerModule: LEDGER_HANDLER,
    options: { magic: PREPROD_MAGIC, scriptHash: SCRIPT_HASH },
  });
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
  afterEach(async () => {
    vi.restoreAllMocks();
    await closeSharedL1NodeTransports();
  });

  it("builds Kupmios providers whose reward-account state comes from the local ledger", async () => {
    const { lucid, providerSource } = await lucidFromProviderUrl(
      KUPMIOS_URL,
      "Preprod",
      undefined,
      PREPROD_MAGIC,
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
      PREPROD_MAGIC,
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
      PREPROD_MAGIC,
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
    const { lucid } = await lucidFromProviderUrl(
      KUPMIOS_URL,
      "Preprod",
      { ...nativeLedger, nodeConfigPath: moved },
      PREPROD_MAGIC,
    );
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
      PREVIEW_MAGIC,
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
