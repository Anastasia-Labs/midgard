import {
  copyFile,
  mkdir,
  mkdtemp,
  readFile,
  rm,
  stat,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { entropyToMnemonic } from "bip39";
import { loadCommitteeConfig } from "da-committee-node/config";
import { parse } from "dotenv";
import { computeDeploymentManifestDaCommitteeSignersHash } from "midgard-node/deployment-manifest";
import { parseNativeLedgerSettings } from "midgard-node/services/native-ledger";
import { writeFinalizedDeploymentInfo } from "midgard-node/tests/da-libp2p-runtime-manifest.write-finalized-deployment-info";
import { afterEach, expect, it } from "vitest";

import { configureStackCommittee } from "../src/full-stack/committee.js";
import { parseStackConfig } from "../src/full-stack/config.js";
import { generateDaServices } from "../src/full-stack/da-services.js";
import {
  configureHostNativeLedger,
  containerNativeLedger,
  exportLocalCardanoConfig,
  nativeLedgerPaths,
} from "../src/full-stack/native-ledger.js";
import { StackProcesses } from "../src/full-stack/process.js";
import { L1OriginUndeterminedError } from "../src/l1-origin.js";
import {
  RecordingProcesses,
  removeStackFixtures,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";

afterEach(removeStackFixtures);

it("leaves the exported Cardano configuration readable to container users", async () => {
  const { config } = await stackFixture();
  const processes = new RecordingProcesses(config, stackEnvironment(config));
  const { directory } = nativeLedgerPaths(processes);
  processes.responses["cardano-config-export"] = async () => {
    // docker cp extracts in the controller process, under its umask.
    await mkdir(join(directory, "genesis"), { recursive: true, mode: 0o700 });
    await writeFile(join(directory, "genesis/shelley.json"), "{}", {
      mode: 0o600,
    });
  };
  const umask = process.umask(0o077);
  await exportLocalCardanoConfig(processes).finally(() => process.umask(umask));
  for (const [path, mode] of [
    [directory, 0o755],
    [join(directory, "genesis"), 0o755],
    [join(directory, "genesis/shelley.json"), 0o644],
  ] as const)
    expect((await stat(path)).mode & 0o777).toBe(mode);
});

it("generates committee configurations accepted by the real loader: the run's L1 origin and local node, no chain index", async () => {
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
      "/app/native-ledger/midgard-l1-node-transport",
    );
    expect(containerLedger.volumes).toContain(
      `${hostLedger!.binaryPath}:/app/native-ledger/midgard-l1-node-transport:ro`,
    );
    const env: Record<string, string> = {
      DA_THRESHOLD: "2",
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
    // The controller's umask; files the containers read must still be readable.
    const umask = process.umask(0o077);
    await mkdir(join(directory, "deploymentInfo"));
    const manifest = join(
      directory,
      "deploymentInfo/contract-deployment-info.json",
    );
    await copyFile(fixture.path, manifest);
    // Before the origin step restored the run's origin, no committee
    // environment is written.
    await expect(
      generateDaServices(new StackProcesses(config, env)),
    ).rejects.toBeInstanceOf(L1OriginUndeterminedError);
    env.L1_ORIGIN = `100.${"ab".repeat(32)}`;
    const generated = await generateDaServices(
      new StackProcesses(config, env),
    ).finally(() => process.umask(umask));
    // The init script runs before any table exists, so it grants the reader
    // no table: no default privilege that would reach every future table.
    const init = await readFile(
      join(generated.directory, "da-postgres-init.sh"),
      "utf8",
    );
    expect(init).toContain("CREATE ROLE midgard_da_reader");
    expect(init).not.toMatch(/DEFAULT PRIVILEGES|GRANT SELECT/u);
    for (const name of ["da-postgres-init.sh", "committee-0.json"])
      expect((await stat(join(generated.directory, name))).mode & 0o044).toBe(
        0o044,
      );
    expect((await stat(manifest)).mode & 0o044).toBe(0o044);
    const services = generated.services as Record<
      string,
      { image?: string; volumes: string[] }
    >;
    for (const service of ["da-committee-0", "public-retained-da"])
      expect(services[service]!.image).toBe(
        "midgard-stack-da:${COMPOSE_PROJECT_NAME}",
      );
    // Readiness compares the committee's configured fingerprint with this id.
    const { manifestId } = JSON.parse(await readFile(manifest, "utf8"));
    const committeeManifest = JSON.parse(
      await readFile(join(generated.directory, "committee-0.json"), "utf8"),
    );
    expect(committeeManifest.deployment.fingerprint).toBe(manifestId);
    expect(
      committeeManifest.da_committee.members.map(
        (member: { signer_index: number }) => member.signer_index,
      ),
    ).toEqual(committee.map((member) => member.signerIndex));
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
      expect(loaded.l1Origin).toEqual({
        slot: 100,
        blockHash: "ab".repeat(32),
      });
      expect(
        Object.keys(serviceEnv).filter((name) =>
          /KUPO|OGMIOS|PROVIDER_URL|L1_SOURCE_MODE|CHAIN_SYNC_(URL|CURSOR)/u.test(
            name,
          ),
        ),
      ).toEqual([]);
      expect(loaded.nativeLedger?.binaryPath).toBe(
        "/app/native-ledger/midgard-l1-node-transport",
      );
      // The host's pinned build, the same binary the node container mounts.
      expect(services[`da-committee-${member.signerIndex}`]!.volumes).toContain(
        `${hostLedger!.binaryPath}:/app/native-ledger/midgard-l1-node-transport:ro`,
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
