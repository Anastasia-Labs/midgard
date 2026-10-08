import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  Emulator,
  generateEmulatorAccountFromPrivateKey,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  availabilityResponderCollateral,
  availabilityResponderFromConfig,
} from "../src/availability/factory.js";
import type { CommitteeL1ClientConfig } from "../src/config.js";
import { minimalConfig, tempDir } from "./helpers.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";

const configFor = (dir: string): CommitteeL1ClientConfig => ({
  ...minimalConfig({
    manifestPath: join(dir, "manifest.json"),
    deploymentInfoPath: join(dir, "deployment.json"),
    signerSeed: "00".repeat(32),
    signerPublicKey: "11".repeat(32),
  }),
  l1SubmissionEnabled: true,
  availabilityJournalPath: join(dir, "availability.sqlite"),
  availabilitySubmitterKeySource: "private-key:test-responder-key",
  l1SubmitterKeySource: "private-key:test-attestation-key",
  cardanoL1Source: { networkMagic: 1 },
});

type FollowerArg = Parameters<typeof availabilityResponderFromConfig>[2];

/** A follower whose reads must not run before the startup guard checks. */
const followerFor = (lucid?: LucidEvolution): FollowerArg => ({
  source: { readiness: () => [] } as unknown as FollowerArg["source"],
  store: {} as NonNullable<FollowerArg["store"]>,
  provider: {} as NonNullable<FollowerArg["provider"]>,
  lucid: async () => {
    if (lucid === undefined)
      throw new Error("Follower reads must not run before the guard checks");
    return lucid;
  },
});

describe("availability responder production factory", () => {
  it("requires explicit durable journal and independent wallet configuration", async () => {
    const dir = await tempDir();
    const config = configFor(dir);
    const store = await openTestCommitteeStore();
    await expect(
      availabilityResponderFromConfig(
        { ...config, availabilityJournalPath: undefined },
        store,
        followerFor(),
      ),
    ).rejects.toThrow(/DA_AVAILABILITY_JOURNAL_PATH/);
    await expect(
      availabilityResponderFromConfig(
        { ...config, availabilitySubmitterKeySource: undefined },
        store,
        followerFor(),
      ),
    ).rejects.toThrow(/dedicated DA_AVAILABILITY_SUBMITTER_KEY_SOURCE/);
  });

  it("requires the committee's L1 follower before any signing work", async () => {
    const dir = await tempDir();
    const store = await openTestCommitteeStore();
    await expect(
      availabilityResponderFromConfig(configFor(dir), store, {
        ...followerFor(),
        store: null,
        provider: null,
      }),
    ).rejects.toThrow(/requires the committee's L1 follower/);
  });

  it("rejects the attestation payment key even under a different file-backed key source", async () => {
    const dir = await tempDir();
    const account = generateEmulatorAccountFromPrivateKey({
      lovelace: 5_000_000n,
    });
    const lucid = await Lucid(new Emulator([account]), "Custom");
    const keyFile = join(dir, "responder.key");
    await writeFile(keyFile, `private-key:${account.privateKey}`);
    const store = await openTestCommitteeStore();
    await expect(
      availabilityResponderFromConfig(
        {
          ...configFor(dir),
          availabilitySubmitterKeySource: `file:${keyFile}`,
          l1SubmitterKeySource: `private-key:${account.privateKey}`,
        },
        store,
        followerFor(lucid),
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
