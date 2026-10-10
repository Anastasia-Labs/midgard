import { chmod, mkdir } from "node:fs/promises";
import { join, resolve } from "node:path";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { formatL1Origin, parseL1Origin } from "@al-ft/midgard-core/l1-origin";
import {
  generateDaLibp2pRuntimeManifest,
  writeDaLibp2pRuntimeManifest,
} from "midgard-node/da/libp2p-runtime-manifest";
import { writeTextFileAtomic } from "midgard-node/files/atomic-write";

import { L1OriginUndeterminedError } from "../l1-origin.js";
import { configureStackCommittee } from "./committee.js";
import { stackPaths } from "./deployment.js";
import { readJsonIfPresent } from "./journal.js";
import { containerChainSync, shareWithContainers } from "./native-ledger.js";
import type { StackProcesses } from "./process.js";

/** Project-scoped, so two stacks on one host never overwrite each other's image. */
const STACK_DA_IMAGE = "midgard-stack-da:${COMPOSE_PROJECT_NAME}";

/** The public retained-DA reader's login and the only tables it may read. */
export const STACK_DA_READER_ROLE = "midgard_da_reader";
export const STACK_DA_READER_TABLES = [
  "committee_da_payloads",
  "committee_state_queue_headers",
] as const;

/**
 * The reader's table grants in one member's database, run once the committee
 * node has created its tables. It first removes every other table privilege,
 * including the default privileges earlier stacks granted on every table, so
 * the role never reaches a follower or private committee table. Only member
 * 0's database is served; the others keep no table grant at all.
 */
export const stackDaReaderGrantSql = (signerIndex: number): string =>
  [
    `ALTER DEFAULT PRIVILEGES FOR ROLE midgard_da_writer IN SCHEMA public REVOKE ALL ON TABLES FROM ${STACK_DA_READER_ROLE};`,
    `REVOKE ALL ON ALL TABLES IN SCHEMA public FROM ${STACK_DA_READER_ROLE};`,
    ...(signerIndex === 0
      ? [
          `GRANT SELECT ON ${STACK_DA_READER_TABLES.join(", ")} TO ${STACK_DA_READER_ROLE};`,
        ]
      : []),
  ].join(" ");

export async function writePrivateEnv(
  path: string,
  env: Record<string, string>,
) {
  const body = Object.entries(env)
    .map(([key, value]) => {
      if (/\r|\n/.test(value))
        throw new Error(
          `Multiline ${key} is not supported in a Compose environment file`,
        );
      return `${key}=${JSON.stringify(value.replaceAll("$", "$$"))}`;
    })
    .join("\n");
  await writeTextFileAtomic(path, `${body}\n`, { mode: 0o600 });
}
export async function generateDaServices(processes: StackProcesses) {
  const { config, env } = processes;
  const directory = join(config.runDirectory, "services");
  await mkdir(directory, { recursive: true, mode: 0o700 });
  const manifestPath = stackPaths(processes).manifest;
  const manifest = verifyFinalizedDeploymentManifest(
    await readJsonIfPresent(manifestPath),
  );
  // A public contract manifest the DA containers read as their own user.
  await shareWithContainers(manifestPath);
  const threshold = Number(env.DA_THRESHOLD);
  if (
    !Number.isSafeInteger(threshold) ||
    threshold < 1 ||
    threshold > config.da.members.length
  )
    throw new Error(
      "Set the exact configured DA_THRESHOLD before running the stack",
    );
  // Each committee follower starts from the run's origin, which the origin
  // step restored; without it no committee environment is written.
  const committeeL1Origin = (() => {
    const text = env.L1_ORIGIN ?? "";
    if (text === "")
      throw new L1OriginUndeterminedError(
        "the run has no L1 origin yet, so no committee environment is written",
      );
    try {
      return formatL1Origin(parseL1Origin(text, "L1_ORIGIN"));
    } catch (error) {
      throw new L1OriginUndeterminedError((error as Error).message);
    }
  })();
  const members = (await configureStackCommittee(config, env)).map((member) => {
    const { signerIndex } = member;
    return {
      signerIndex,
      daVkey: member.daVkey,
      seedEnv: member.seedEnv,
      libp2pPrivateKeySource: env[member.transportEnv]!,
      roles: [
        "committee",
        "retrieval",
        ...(signerIndex === 0 ? ["coordinator"] : []),
      ],
      endpoint: { port: config.da.ports.committeeTransportBase + signerIndex },
    };
  });
  const common = {
    contractDeploymentInfoPath: manifestPath,
    network: "Preprod",
    producerPrivateKeySource: env[config.da.producerTransportEnv]!,
    publicRetainedDaPrivateKeySource: env[config.da.retainedTransportEnv]!,
    committeeMembers: members,
    threshold,
    publicRetainedDaPort: config.da.ports.retainedTransport,
  };
  const producer = join(directory, "producer.json");
  await writeDaLibp2pRuntimeManifest(
    producer,
    await generateDaLibp2pRuntimeManifest({
      ...common,
      target: "producer",
      profile: "producer-container-committee-host",
    }),
  );
  await writeDaLibp2pRuntimeManifest(
    join(directory, "producer-host.json"),
    await generateDaLibp2pRuntimeManifest({
      ...common,
      target: "producer",
      producerPort: Number(env.MIDGARD_NODE_DA_HOST_PORT ?? 39002),
      profile: "host",
    }),
  );
  const mount = (source: string, target: string) => `${source}:${target}:ro`;
  const build = {
    context: resolve(config.nodeRoot, ".."),
    dockerfile: "da-committee-node/Dockerfile",
  };
  const services: Record<string, unknown> = {};
  const writerPassword = env[config.da.databasePasswordEnv]!;
  const readerPassword = env[config.da.readerPasswordEnv]!;
  if (writerPassword === readerPassword)
    throw new Error("DA reader and writer passwords must differ");
  const pgEnv = join(directory, "da-postgres.env");
  await writePrivateEnv(pgEnv, {
    POSTGRES_USER: "midgard_da_writer",
    POSTGRES_PASSWORD: writerPassword,
    POSTGRES_DB: "midgard_da_0",
    STACK_DA_READER_PASSWORD: readerPassword,
  });
  const init = join(directory, "da-postgres-init.sh");
  // Container users (postgres, node) read these; an explicit mode is not masked by the umask.
  await writeTextFileAtomic(
    init,
    `#!/bin/sh\nset -eu\nreader_password_sql=$(printf '%s' "$STACK_DA_READER_PASSWORD" | sed "s/'/''/g")\nprintf "CREATE ROLE midgard_da_reader LOGIN PASSWORD '%s';\\n" "$reader_password_sql" | psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB"\nfor index in ${members.map((member) => member.signerIndex).join(" ")}; do\n  database="midgard_da_$index"\n  if [ "$index" != 0 ]; then createdb --username "$POSTGRES_USER" "$database"; fi\n  printf '%s\\n' 'GRANT CONNECT ON DATABASE '"$database"' TO midgard_da_reader;' 'GRANT USAGE ON SCHEMA public TO midgard_da_reader;' | psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$database"\ndone\n`,
    { mode: 0o644 },
  );
  services["da-postgres"] = {
    image: "postgres:15.15-alpine",
    restart: "unless-stopped",
    env_file: [pgEnv],
    ports: [`127.0.0.1:${config.da.ports.database}:5432`],
    volumes: [
      "da-postgres-data:/var/lib/postgresql/data",
      mount(init, "/docker-entrypoint-initdb.d/10-reader.sh"),
    ],
    healthcheck: {
      test: ["CMD-SHELL", "pg_isready -U midgard_da_writer -d midgard_da_0"],
      interval: "2s",
      timeout: "3s",
      retries: 60,
    },
  };
  for (const member of members) {
    const memberManifest = join(
      directory,
      `committee-${member.signerIndex}.json`,
    );
    await writeDaLibp2pRuntimeManifest(
      memberManifest,
      await generateDaLibp2pRuntimeManifest({
        ...common,
        target: "committee",
        profile: "host",
        localSignerIndex: member.signerIndex,
        producerPort: Number(env.MIDGARD_NODE_DA_HOST_PORT ?? 39002),
      }),
    );
    // Public peer identities and addresses only; the node user in the image reads it.
    await chmod(memberManifest, 0o644);
    const envFile = join(directory, `committee-${member.signerIndex}.env`);
    await writePrivateEnv(envFile, {
      MIDGARD_NETWORK: "Preprod",
      MIDGARD_DEPLOYMENT_MANIFEST_PATH: "/config/committee.json",
      MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: "/config/manifest.json",
      CARDANO_LOCAL_NODE_AUTHORITY_ID: "local-cardano-node",
      L1_ORIGIN: committeeL1Origin,
      CARDANO_LOCAL_NODE_SOCKET_PATH: "/ipc/node.socket",
      CARDANO_LOCAL_NODE_CONFIG_PATH: "/cardano-config/config.json",
      CARDANO_L1_NODE_TRANSPORT_BINARY_PATH: containerChainSync(processes).path,
      CARDANO_FINALITY_DEPTH: String(manifest.l1Finality.confirmationDepth),
      DA_LIBP2P_PRIVATE_KEY_SOURCE: member.libp2pPrivateKeySource,
      DA_SIGNER_INDEX: String(member.signerIndex),
      DA_SIGNER_KEY_SOURCE: `cardano-seed:${env[member.seedEnv]}`,
      DA_THRESHOLD: String(threshold),
      DA_L1_SUBMISSION_ENABLED: String(member.signerIndex === 0),
      ...(member.signerIndex === 0
        ? { L1_SUBMITTER_KEY_SOURCE: `seed:${env[config.da.submitterSeedEnv]}` }
        : {}),
      DA_COMMITTEE_DATABASE_URL: `postgresql://midgard_da_writer:${encodeURIComponent(writerPassword)}@127.0.0.1:${config.da.ports.database}/midgard_da_${member.signerIndex}`,
      DA_COMMITTEE_API_HOST: "127.0.0.1",
      DA_COMMITTEE_API_PORT: String(
        config.da.ports.committeeApiBase + member.signerIndex,
      ),
    });
    services[`da-committee-${member.signerIndex}`] = {
      build,
      image: STACK_DA_IMAGE,
      command: ["dist/index.js"],
      network_mode: "host",
      restart: "unless-stopped",
      env_file: [envFile],
      volumes: [
        mount(memberManifest, "/config/committee.json"),
        mount(manifestPath, "/config/manifest.json"),
        mount(
          join(config.watcher.configDirectory, "cardano"),
          "/cardano-config",
        ),
        `${join(config.nodeRoot, "cardano/ipc")}:/ipc`,
        containerChainSync(processes).volume,
        `da-committee-${member.signerIndex}-state:/var/lib/midgard-da`,
      ],
      depends_on: { "da-postgres": { condition: "service_healthy" } },
    };
  }
  const publicEnv = join(directory, "retained.env");
  await writePrivateEnv(publicEnv, {
    MIDGARD_NETWORK: "Preprod",
    MIDGARD_DEPLOYMENT_MANIFEST_PATH: "/config/committee.json",
    MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: "/config/manifest.json",
    DA_PUBLIC_RETAINED_DA_ENABLED: "true",
    DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE:
      env[config.da.retainedTransportEnv]!,
    DA_PUBLIC_RETAINED_DA_DATABASE_ROLE: "midgard_da_reader",
    DA_PUBLIC_RETAINED_DA_DATABASE_URL: `postgresql://midgard_da_reader:${encodeURIComponent(readerPassword)}@127.0.0.1:${config.da.ports.database}/midgard_da_0`,
  });
  services["public-retained-da"] = {
    build,
    image: STACK_DA_IMAGE,
    command: ["dist/public-retained-da.js"],
    network_mode: "host",
    restart: "unless-stopped",
    env_file: [publicEnv],
    volumes: [
      mount(join(directory, "committee-0.json"), "/config/committee.json"),
      mount(manifestPath, "/config/manifest.json"),
    ],
    depends_on: { "da-postgres": { condition: "service_healthy" } },
  };
  return {
    directory,
    services,
    producer,
    volumes: Object.fromEntries(
      members.map((member) => [`da-committee-${member.signerIndex}-state`, {}]),
    ),
  };
}
