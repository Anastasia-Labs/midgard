import "./index.registration-4.js";

import {
  collectStringOption,
  failCli,
  parseE2EEnvInheritanceOption,
  parseNonNegativeIntegerOption,
  parsePositiveIntegerOption,
  parseStringListOption,
  writeJson,
} from "./commands/cli-runtime.js";
import { runDaLibp2pPreflightFromEnv } from "./da/libp2p-producer.js";
import {
  DA_LIBP2P_RUNTIME_PROFILES,
  generateDaLibp2pRuntimeManifest,
  writeDaLibp2pRuntimeManifest,
} from "./da/libp2p-runtime-manifest.js";
import { buildE2EProcessEnv, parseEnvOverrides } from "./e2e/env.js";
import {
  parseDaLibp2pCommitteeMembers,
  parseDaLibp2pPreflightMode,
  parseDaLibp2pRuntimeProfile,
  parseDaLibp2pRuntimeTarget,
  program,
} from "./index.registration.js";

program
  .command("da-libp2p-generate-manifest")
  .description("Generate a target-specific libp2p DA runtime manifest")
  .requiredOption("--target <target>", "Runtime target: producer or committee")
  .requiredOption(
    "--profile <profile>",
    `Address profile: ${DA_LIBP2P_RUNTIME_PROFILES.join(", ")}`,
  )
  .requiredOption(
    "--contract-deployment-info <path>",
    "Finalized V1 contract deployment info path; deployment.fingerprint is derived from manifestId",
  )
  .requiredOption(
    "--producer-libp2p-key-source <source>",
    "Producer DA_LIBP2P_PRIVATE_KEY_SOURCE",
  )
  .requiredOption(
    "--public-retained-da-libp2p-key-source <source>",
    "Dedicated non-signer DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE",
  )
  .requiredOption("--threshold <n>", "DA committee threshold")
  .option(
    "--committee-member <spec>",
    "Committee member as signerIndex,daVkey,libp2pKeySource,role+role[,[host:]port]; the endpoint overrides the shared committee address; repeatable",
    collectStringOption,
    [],
  )
  .requiredOption("--network <network>", "Exact Cardano network label")
  .option("--local-signer-index <n>", "Local committee signer index")
  .option("--producer-port <port>", "Producer retrieval libp2p port")
  .option("--committee-port <port>", "Committee node libp2p port")
  .option("--public-retained-da-port <port>", "Public retained-DA libp2p port")
  .option("--producer-service-name <name>", "Compose producer service DNS name")
  .option(
    "--committee-service-name <name>",
    "Compose committee node service DNS name",
  )
  .option("--producer-public-host <host>", "Public producer DNS/IP")
  .option("--committee-public-host <host>", "Public committee node DNS/IP")
  .option(
    "--public-retained-da-public-host <host>",
    "Public retained-DA DNS/IP (defaults to --committee-public-host)",
  )
  .option("--out <path>", "Write manifest JSON to this path")
  .action(async (options) => {
    const opts = typeof options.opts === "function" ? options.opts() : options;
    try {
      const committeeMembers = parseDaLibp2pCommitteeMembers(
        opts.committeeMember,
      );
      if (committeeMembers.length === 0) {
        throw new Error("at least one --committee-member is required");
      }
      const manifest = await generateDaLibp2pRuntimeManifest({
        target: parseDaLibp2pRuntimeTarget(opts.target),
        profile: parseDaLibp2pRuntimeProfile(opts.profile),
        contractDeploymentInfoPath: opts.contractDeploymentInfo,
        producerPrivateKeySource: opts.producerLibp2pKeySource,
        publicRetainedDaPrivateKeySource: opts.publicRetainedDaLibp2pKeySource,
        committeeMembers,
        threshold: parsePositiveIntegerOption(opts.threshold, "--threshold"),
        network: opts.network,
        ...(typeof opts.localSignerIndex === "string"
          ? {
              localSignerIndex: parseNonNegativeIntegerOption(
                opts.localSignerIndex,
                "--local-signer-index",
              ),
            }
          : {}),
        ...(typeof opts.producerPort === "string"
          ? {
              producerPort: parsePositiveIntegerOption(
                opts.producerPort,
                "--producer-port",
              ),
            }
          : {}),
        ...(typeof opts.committeePort === "string"
          ? {
              committeePort: parsePositiveIntegerOption(
                opts.committeePort,
                "--committee-port",
              ),
            }
          : {}),
        ...(typeof opts.publicRetainedDaPort === "string"
          ? {
              publicRetainedDaPort: parsePositiveIntegerOption(
                opts.publicRetainedDaPort,
                "--public-retained-da-port",
              ),
            }
          : {}),
        ...(typeof opts.producerServiceName === "string"
          ? { producerServiceName: opts.producerServiceName }
          : {}),
        ...(typeof opts.committeeServiceName === "string"
          ? { committeeServiceName: opts.committeeServiceName }
          : {}),
        ...(typeof opts.producerPublicHost === "string"
          ? { producerPublicHost: opts.producerPublicHost }
          : {}),
        ...(typeof opts.committeePublicHost === "string"
          ? { committeePublicHost: opts.committeePublicHost }
          : {}),
        ...(typeof opts.publicRetainedDaPublicHost === "string"
          ? { publicRetainedDaPublicHost: opts.publicRetainedDaPublicHost }
          : {}),
      });
      if (typeof opts.out === "string" && opts.out.length > 0) {
        await writeDaLibp2pRuntimeManifest(opts.out, manifest);
      }
      writeJson(manifest);
    } catch (error) {
      failCli("da-libp2p-generate-manifest", error);
    }
  });

program
  .command("da-libp2p-preflight")
  .description("Probe libp2p DA committee reachability from the producer")
  .option("--json", "Print machine-readable JSON", true)
  .option(
    "--mode <mode>",
    "Preflight mode: bind-listen validates producer listener binding before startup; dial-only probes peers without binding after startup",
    "bind-listen",
  )
  .option(
    "--env-file <path>",
    "Dotenv-compatible env file to apply before explicit --env overrides; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--env <KEY=VALUE>",
    "Explicit environment override; repeatable and applied after --env-file",
    collectStringOption,
    [],
  )
  .option(
    "--env-inheritance <mode>",
    "Environment inheritance mode: process or none",
    "process",
  )
  .action(async (options) => {
    const opts = typeof options.opts === "function" ? options.opts() : options;
    try {
      const { env } = await buildE2EProcessEnv({
        cwd: process.cwd(),
        envFiles: parseStringListOption(opts.envFile, "--env-file"),
        overrides: parseEnvOverrides(parseStringListOption(opts.env, "--env")),
        inherit: parseE2EEnvInheritanceOption(opts.envInheritance),
      });
      const report = await runDaLibp2pPreflightFromEnv(env, {
        mode: parseDaLibp2pPreflightMode(opts.mode),
      });
      writeJson(report);
      if (!report.passed) {
        process.exitCode = 1;
      }
    } catch (error) {
      failCli("da-libp2p-preflight", error);
    }
  });
