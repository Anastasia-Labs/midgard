import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  Emulator,
  generateEmulatorAccountFromPrivateKey,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  availabilityResponderCollateral,
  availabilityResponderFromConfig,
} from "../src/availability/factory.js";
import type { CommitteeConfig } from "../src/config.js";
import { JsonFileCommitteeStore } from "../src/store.js";
import { minimalConfig, tempDir } from "./helpers.js";

const configFor = (dir: string): CommitteeConfig => ({
  ...minimalConfig({
    dir,
    manifestPath: join(dir, "manifest.json"),
    deploymentInfoPath: join(dir, "deployment.json"),
    signerSeed: "00".repeat(32),
    signerPublicKey: "11".repeat(32),
  }),
  l1SubmissionEnabled: true,
  availabilityJournalPath: join(dir, "availability.sqlite"),
  availabilitySubmitterKeySource: "private-key:test-responder-key",
  l1SubmitterKeySource: "private-key:test-attestation-key",
  cardanoProviderUrls: ["kupmios:http://kupo|http://ogmios"],
  l1Source: {
    sourceMode: "local_node",
    authorityNodeId: "test-authority",
    chainSyncProviderUrl: "ogmios:http://ogmios",
    queryProviderUrls: ["kupmios:http://kupo|http://ogmios"],
  },
});

const chainProvider = {
  fetchStateQueueNodes: async () => [],
  currentChainSyncCursor: async () => {
    throw new Error(
      "Canonical queries must not run before startup guard checks",
    );
  },
};

describe("availability responder production factory", () => {
  it("requires explicit durable journal and independent wallet configuration", async () => {
    const dir = await tempDir();
    const config = configFor(dir);
    const store = await JsonFileCommitteeStore.open(join(dir, "store.json"));
    await expect(
      availabilityResponderFromConfig(
        { ...config, availabilityJournalPath: undefined },
        store,
        chainProvider,
      ),
    ).rejects.toThrow(/DA_AVAILABILITY_JOURNAL_PATH/);
    await expect(
      availabilityResponderFromConfig(
        { ...config, availabilitySubmitterKeySource: undefined },
        store,
        chainProvider,
      ),
    ).rejects.toThrow(/dedicated DA_AVAILABILITY_SUBMITTER_KEY_SOURCE/);
  });

  it("requires a canonical local node before any signing work", async () => {
    const dir = await tempDir();
    const store = await JsonFileCommitteeStore.open(join(dir, "store.json"));
    await expect(
      availabilityResponderFromConfig(
        {
          ...configFor(dir),
          l1Source: { sourceMode: "external_providers", providers: [] },
        },
        store,
        chainProvider,
      ),
    ).rejects.toThrow(/local_node canonical/);
  });

  it("rejects the attestation payment key even under a different file-backed key source", async () => {
    const dir = await tempDir();
    const account = generateEmulatorAccountFromPrivateKey({
      lovelace: 5_000_000n,
    });
    const lucid = await Lucid(new Emulator([account]), "Custom");
    const keyFile = join(dir, "responder.key");
    await writeFile(keyFile, `private-key:${account.privateKey}`);
    const store = await JsonFileCommitteeStore.open(join(dir, "store.json"));
    await expect(
      availabilityResponderFromConfig(
        {
          ...configFor(dir),
          availabilitySubmitterKeySource: `file:${keyFile}`,
          l1SubmitterKeySource: `private-key:${account.privateKey}`,
        },
        store,
        chainProvider,
        {
          lucidFromProviderUrl: async () => ({
            lucid,
            providerSource: "emulator",
          }),
        },
      ),
    ).rejects.toThrow(/different payment credentials/);
  });

  it("selects sufficient isolated plain ADA collateral and refuses insufficient funding", async () => {
    const account = generateEmulatorAccountFromPrivateKey({
      lovelace: 3_000_000n,
    });
    const lucid = await Lucid(new Emulator([account]), "Custom");
    lucid.selectWallet.fromPrivateKey(account.privateKey);
    expect(
      await availabilityResponderCollateral(lucid, 1_000_000n),
    ).toHaveLength(1);
    await expect(
      availabilityResponderCollateral(lucid, 3_000_000n),
    ).rejects.toThrow(/lacks separate plain-ADA collateral/);
  });
});
