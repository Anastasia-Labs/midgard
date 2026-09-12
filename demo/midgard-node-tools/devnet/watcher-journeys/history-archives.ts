import { execFileSync } from "node:child_process";
import { createHash, randomUUID, X509Certificate } from "node:crypto";
import { existsSync } from "node:fs";
import {
  mkdir,
  open,
  readdir,
  readFile,
  rename,
  rm,
  writeFile,
} from "node:fs/promises";
import { isIPv4 } from "node:net";
import { join } from "node:path";

import {
  computeFraudProofRawL1PointId,
  validateVerifiedFraudProofReleaseFinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs";
import { CML, validatorToScriptHash } from "@lucid-evolution/lucid";
import type {
  WatcherNativeBlockAdmission,
  WatcherNativeChainSyncEvent,
} from "midgard-watcher";

import { writeJourneyFile } from "./artifacts.js";

const writeArchiveRecord = async (
  path: string,
  value: unknown,
  immutable = false,
) => {
  const bytes = JSON.stringify(value);
  if (immutable && existsSync(path)) {
    if ((await readFile(path, "utf8")) !== bytes)
      throw new Error("Archive record changed immutable authenticated bytes");
    return;
  }
  await writeJourneyFile(path, bytes);
};

/** Persistence fed only by the independently admitted native recorder. */
export const createJourneyNativeScriptArchive = (
  directories: readonly string[],
) => {
  const updateCanonicalArchive = async (action: () => Promise<void>) => {
    for (const directory of directories)
      await rm(join(directory, "canonical-ready"), { force: true });
    await action();
    const generation = randomUUID();
    for (const directory of directories)
      await writeArchiveRecord(join(directory, "canonical-ready"), generation);
  };
  return {
    retainNativeBlock: async (block: WatcherNativeBlockAdmission) =>
      updateCanonicalArchive(async () => {
        const point = {
          blockHash: block.blockHash,
          blockNo: block.blockNo,
          slot: block.slot,
        };
        const inclusionPoint = {
          ...point,
          pointId: computeFraudProofRawL1PointId(point),
        };
        for (const directory of directories) {
          await writeArchiveRecord(
            join(directory, "canonical", `${block.blockNo}.json`),
            { point: inclusionPoint, prevHash: block.prevHash },
          );
        }
        for (const [
          transactionIndex,
          bytes,
        ] of block.transactionCbors.entries()) {
          const body = CML.Transaction.from_cbor_hex(bytes).body();
          const txHash = CML.hash_transaction(body).to_hex();
          if (block.transactionIds[transactionIndex] !== txHash)
            throw new Error("Native archive transaction order changed");
          const outputs = body.outputs();
          for (
            let outputIndex = 0;
            outputIndex < outputs.len();
            outputIndex++
          ) {
            const output = outputs.get(outputIndex);
            const script = output.script_ref()?.as_native();
            if (script === undefined) continue;
            const scriptBytesHex = script.to_canonical_cbor_hex();
            const expectedScriptHash = validatorToScriptHash({
              type: "Native",
              script: scriptBytesHex,
            });
            const record = {
              expectedScriptHash,
              scriptBytesHex,
              publicationOutRef: `${txHash}#${outputIndex}`,
              publicationOutputCbor: output.to_canonical_cbor_hex(),
              publicationTransactionBodyCbor: body.to_canonical_cbor_hex(),
              publicationTransactionIndex: transactionIndex,
              inclusionBlockTransactionIds: block.transactionIds,
              inclusionPoint,
              rawBlockCbor: block.rawBlockCbor,
            };
            for (const directory of directories) {
              const records = join(
                directory,
                "native-scripts",
                expectedScriptHash,
              );
              await mkdir(records, { recursive: true, mode: 0o700 });
              await writeArchiveRecord(
                join(
                  records,
                  `${txHash}_${outputIndex}_${block.blockHash}.json`,
                ),
                record,
                true,
              );
            }
          }
        }
      }),
    rollbackNativeBlocks: async (
      point: Extract<
        WatcherNativeChainSyncEvent,
        { kind: "roll_backward" }
      >["point"],
    ) =>
      updateCanonicalArchive(async () => {
        for (const directory of directories) {
          const files = await readdir(join(directory, "canonical"));
          for (const file of files.filter((name) =>
            /^[0-9]+\.json$/.test(name),
          )) {
            const path = join(directory, "canonical", file);
            const retained = JSON.parse(await readFile(path, "utf8")) as {
              point: { slot: string; blockHash: string };
            };
            if (
              point.kind === "origin" ||
              BigInt(retained.point.slot) > BigInt(point.slot) ||
              (retained.point.slot === point.slot &&
                retained.point.blockHash !== point.blockHash)
            )
              await rm(path);
          }
          const canonicalDirectory = await open(
            join(directory, "canonical"),
            "r",
          );
          try {
            await canonicalDirectory.sync();
          } finally {
            await canonicalDirectory.close();
          }
        }
      }),
  };
};

const ARCHIVE_NODE_IMAGE =
  "node:22.22.2-bookworm-slim@sha256:9f6d5975c7dca860947d3915877f85607946403fc55349f39b4bc3688448bb6e";

// Each archive is a distinct HTTPS process, certificate and retained directory
// on the run's container network. Its records contain real node inclusion points.
export const startJourneyHistoryArchives = async (input: {
  runDirectory: string;
  composeProject: string;
  deploymentFingerprint: string;
  releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}) => {
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy(
    input.releaseFinality,
  );
  if (releaseFinality.deploymentIdentityDigest !== input.deploymentFingerprint)
    throw new Error("Archive release differs from deployment identity");
  const providers: {
    sourceId: string;
    operatorIdentitySha256: string;
    authorityEndpoint: string;
  }[] = [];
  const certificates: string[] = [];
  const directories: string[] = [];
  const containers: string[] = [];
  const docker = (...args: string[]) =>
    execFileSync("docker", args, { encoding: "utf8", timeout: 30_000 }).trim();
  const close = async () => {
    for (const container of containers.splice(0))
      docker("stop", "--time", "5", container);
  };
  try {
    for (const role of ["a", "b"]) {
      const directory = join(input.runDirectory, "history", role);
      await mkdir(join(directory, "records"), { recursive: true, mode: 0o700 });
      const keyPath = join(directory, "key.pem");
      const certificatePath = join(directory, "certificate.pem");
      await rm(join(directory, "ready"), { force: true });
      await rm(join(directory, "canonical-ready"), { force: true });
      // Starting the process before certificate creation lets Docker assign
      // its actual IP, which is then bound in the certificate's subjectAltName.
      await writeFile(
        join(directory, "server.mjs"),
        await readFile(
          new URL("./history-archive-server.mjs", import.meta.url),
        ),
      );
      await mkdir(join(directory, "canonical"), {
        recursive: true,
        mode: 0o700,
      });
      await mkdir(join(directory, "native-scripts"), {
        recursive: true,
        mode: 0o700,
      });
      const container = `${input.composeProject}-history-${role}`;
      const existing = docker(
        "ps",
        "--all",
        "--filter",
        `name=^/${container}$`,
        "--format",
        "{{.Names}}",
      );
      if (existing === container) {
        const details = JSON.parse(docker("inspect", container))[0];
        if (
          details.State.Running ||
          !details.Mounts.some(
            (mount: { Source: string; Destination: string }) =>
              mount.Source === directory && mount.Destination === "/service",
          )
        )
          throw new Error(
            "Archive container is already running or belongs to a different directory",
          );
        docker("start", container);
      } else {
        docker(
          "run",
          "--detach",
          "--name",
          container,
          "--network",
          `${input.composeProject}_default`,
          "--user",
          `${process.getuid!()}:${process.getgid!()}`,
          "--volume",
          `${directory}:/service`,
          ARCHIVE_NODE_IMAGE,
          "node",
          "/service/server.mjs",
        );
      }
      containers.push(container);
      const network = JSON.parse(docker("inspect", container))[0]
        .NetworkSettings.Networks[`${input.composeProject}_default`];
      const ip = network.IPAddress;
      if (typeof ip !== "string" || !isIPv4(ip))
        throw new Error("Archive has no isolated container IP");
      const pendingCertificate = join(directory, "certificate.pending.pem");
      execFileSync(
        "openssl",
        [
          "req",
          "-x509",
          ...(existsSync(keyPath)
            ? ["-key", keyPath]
            : ["-newkey", "ed25519", "-noenc", "-keyout", keyPath]),
          "-out",
          pendingCertificate,
          "-days",
          "7",
          "-subj",
          `/CN=watcher-history-${role}`,
          "-addext",
          `subjectAltName=IP:${ip}`,
        ],
        { stdio: "pipe", timeout: 10_000 },
      );
      await rename(pendingCertificate, certificatePath);
      const certificate = await readFile(certificatePath, "utf8");
      const publicKey = new X509Certificate(certificate).publicKey.export({
        type: "spki",
        format: "der",
      });
      const provider = {
        sourceId: `journey-history-${role}`,
        operatorIdentitySha256: createHash("sha256")
          .update(publicKey)
          .digest("hex"),
        authorityEndpoint: `https://${ip}:8443`,
      };
      await writeArchiveRecord(join(directory, "authority.json"), {
        releaseFinality,
        sourceId: provider.sourceId,
        operatorIdentitySha256: provider.operatorIdentitySha256,
      });
      await writeFile(join(directory, "ready"), "ready\n");
      providers.push(provider);
      certificates.push(certificate);
      directories.push(directory);
    }
    const caPath = join(input.runDirectory, "history/archive-ca.pem");
    await writeFile(caPath, certificates.join("\n"));
    return {
      caPath,
      configuration: {
        sourceMode: "external_provider_quorum",
        consistencyPolicy: "exact_bytes_all_providers_v1",
        providers,
      },
      ...createJourneyNativeScriptArchive(directories),
      retain: async (
        block: { headerHash: string; payloadEnvelopeCbor: Uint8Array },
        point: { blockHash: string; blockNo: string; slot: string },
      ) => {
        const bytes = JSON.stringify({
          schemaVersion:
            "midgard-production-historical-native-script-history-record-v1",
          deploymentFingerprint: input.deploymentFingerprint,
          headerHash: block.headerHash,
          payloadEnvelopeCborHex: Buffer.from(
            block.payloadEnvelopeCbor,
          ).toString("hex"),
          inclusionPoint: {
            ...point,
            pointId: computeFraudProofRawL1PointId(point),
          },
        });
        for (const directory of directories) {
          const path = join(directory, "records", `${block.headerHash}.json`);
          if (existsSync(path)) {
            if ((await readFile(path, "utf8")) !== bytes)
              throw new Error(
                "Historical archive already retained different bytes for this header",
              );
          } else await writeFile(path, bytes, { flag: "wx" });
        }
      },
      close,
    };
  } catch (cause) {
    await close();
    throw cause;
  }
};
