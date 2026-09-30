import { copyFile, mkdir, mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { entropyToMnemonic } from "bip39";
import { loadCommitteeConfig } from "da-committee-node/config";
import { parse } from "dotenv";
import { computeDeploymentManifestDaCommitteeSignersHash } from "midgard-node/deployment-manifest";
import { parseNativeLedgerSettings } from "midgard-node/services/native-ledger";
import { writeFinalizedDeploymentInfo } from "midgard-node/tests/da-libp2p-runtime-manifest.write-finalized-deployment-info";
import { expect, it } from "vitest";

import { configureStackCommittee } from "../src/full-stack/committee.js";
import { parseStackConfig } from "../src/full-stack/config.js";
import { generateDaServices } from "../src/full-stack/da-services.js";
import {
  configureHostNativeLedger,
  containerNativeLedger,
} from "../src/full-stack/native-ledger.js";
import { StackProcesses } from "../src/full-stack/process.js";

it("generates committee configurations accepted by the real loader with persistent local chain authority", async () => {
  const directory = await mkdtemp(join(tmpdir(), "midgard-stack-services-"));
  try {
    const sample = JSON.parse(
      await readFile(
        new URL("../config/preprod-stack.example.json", import.meta.url),
        "utf8",
      ),
    );
    sample.nodeRoot = directory;
    sample.runDirectory = join(directory, "run");
    sample.watcher.configDirectory = join(directory, "run/watcher");
    const config = parseStackConfig(sample);
    const processes = new StackProcesses(config, {});
    const hostLedger = parseNativeLedgerSettings(
      configureHostNativeLedger(processes),
    );
    const containerLedger = containerNativeLedger(processes);
    expect(hostLedger?.socketPath).toBe(
      join(directory, "cardano/ipc/node.socket"),
    );
    expect(parseNativeLedgerSettings(containerLedger.env)?.binaryPath).toBe(
      "/app/native-ledger/midgard-chain-sync",
    );
    expect(containerLedger.volumes).toContain(
      `${hostLedger!.binaryPath}:/app/native-ledger/midgard-chain-sync:ro`,
    );
    const env: Record<string, string> = {
      DA_THRESHOLD: "2",
      L1_KUPO_KEY: "http://127.0.0.1:1442",
      L1_OGMIOS_KEY: "http://127.0.0.1:1337",
      L1_OPERATOR_SEED_PHRASE: entropyToMnemonic("00".repeat(16)),
      DA_COSIGNER_SEED_PHRASE: entropyToMnemonic("ff".repeat(16)),
    };
    for (const [index, member] of config.da.members.entries()) {
      env[member.seedEnv] = entropyToMnemonic(
        (index + 10).toString(16).padStart(32, "0"),
      );
      env[member.transportEnv] =
        `seed:${(index + 10).toString(16).padStart(64, "0")}`;
    }
    env[config.da.producerTransportEnv] = `seed:${"01".repeat(32)}`;
    env[config.da.retainedTransportEnv] = `seed:${"02".repeat(32)}`;
    env[config.da.submitterSeedEnv] = entropyToMnemonic("03".repeat(16));
    env[config.da.databasePasswordEnv] = "writer-test-password";
    env[config.da.readerPasswordEnv] = "reader-test-password";
    const committee = await configureStackCommittee(config, env);
    const fixture = await writeFinalizedDeploymentInfo((raw) => {
      const da = raw.da as {
        committeeVkeys: string[];
        committeeSignersHash: string;
        threshold: number;
      };
      da.committeeVkeys = committee.map((member) => member.daVkey);
      da.committeeSignersHash = computeDeploymentManifestDaCommitteeSignersHash(
        da.committeeVkeys,
      );
      da.threshold = 2;
    });
    await mkdir(join(directory, "deploymentInfo"));
    const manifest = join(
      directory,
      "deploymentInfo/contract-deployment-info.json",
    );
    await copyFile(fixture.path, manifest);
    const generated = await generateDaServices(new StackProcesses(config, env));
    for (const member of committee) {
      const serviceEnv = parse(
        await readFile(
          join(generated.directory, `committee-${member.signerIndex}.env`),
        ),
      );
      const loaded = await loadCommitteeConfig({
        ...serviceEnv,
        MIDGARD_DEPLOYMENT_MANIFEST_PATH: join(
          generated.directory,
          `committee-${member.signerIndex}.json`,
        ),
        MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: manifest,
      });
      expect(loaded.l1Source.sourceMode).toBe("local_node");
      expect(loaded.nativeLedger?.binaryPath).toBe(
        "/usr/local/bin/midgard-chain-sync",
      );
      expect(loaded.signerIndex).toBe(member.signerIndex);
      expect(loaded.signerKeySource).toBe(
        `cardano-seed:${env[member.seedEnv]}`,
      );
      expect(loaded.l1SubmissionEnabled).toBe(member.signerIndex === 0);
      expect(generated.volumes).toHaveProperty(
        `da-committee-${member.signerIndex}-state`,
      );
    }
    const publicEnv = parse(
      await readFile(join(generated.directory, "retained.env")),
    );
    expect(publicEnv).not.toHaveProperty("L1_SUBMITTER_KEY_SOURCE");
    expect(publicEnv).not.toHaveProperty("DA_SIGNER_KEY_SOURCE");
    expect(publicEnv.DA_PUBLIC_RETAINED_DA_DATABASE_ROLE).toBe(
      "midgard_da_reader",
    );
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});
