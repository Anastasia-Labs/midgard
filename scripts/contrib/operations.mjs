import { existsSync, realpathSync } from "node:fs";
import { resolve } from "node:path";
import { createServer } from "node:net";

import { buildPackage, checkBuild, runDirectory } from "./build.mjs";
import {
  atomicJson,
  inputIdentity,
  json,
  packageByName,
  outputIdentity,
  runtimeBuildClosure,
  sha256,
} from "./files.mjs";
import { runProcess } from "./process.mjs";
import { redact } from "./diagnostics.mjs";
import { writeReceipt } from "./receipts.mjs";
import { withResource } from "./resources.mjs";

export const devnetPlan = (root, runId) => {
  if (!/^[a-z0-9][a-z0-9-]{0,47}$/u.test(runId ?? ""))
    throw new Error(
      "--run-id must be a lowercase identifier of 1..48 characters",
    );
  const identity = sha256(`${realpathSync(root)}:${runId}`).slice(0, 8);
  const base = 20000 + 8 * (Number.parseInt(identity, 16) % 5000);
  return {
    schema: "midgard-devnet-allocation/v1",
    root: realpathSync(root),
    runId,
    identity,
    env: {
      MIDGARD_PHASE4_RUN_ID: `${identity}-${runId}`,
      MIDGARD_PHASE4_OGMIOS_PORT: String(base),
      MIDGARD_PHASE4_KUPO_PORT: String(base + 1),
      MIDGARD_PHASE4_POSTGRES_PORT: String(base + 2),
    },
    scope: "isolated phase4 generator only; no reset, deployment or bootstrap",
  };
};

const portAvailable = (port) =>
  new Promise((done, reject) => {
    const server = createServer();
    server.once("error", (error) =>
      reject(
        new Error(
          `port ${port} is unavailable: ${error.code}; choose another run id and inspect its allocation`,
        ),
      ),
    );
    server.listen(port, "127.0.0.1", () => server.close(done));
  });

export const withPorts = async (
  ports,
  action,
  { signal, env = process.env } = {},
) => {
  const unique = [...new Set(ports)].sort((a, b) => a - b);
  if (
    unique.length !== ports.length ||
    unique.some(
      (port) =>
        !Number.isSafeInteger(port) ||
        port < 1024 ||
        port > 65535 ||
        port === 5433 ||
        port === 55433,
    )
  )
    throw new Error(
      "ports must be distinct nonprivileged host ports, excluding shared test/acceptance databases",
    );
  const acquire = (index, ownedEnv) =>
    index === unique.length
      ? action(ownedEnv)
      : withResource(
          `host-port:${unique[index]}`,
          async (portEnv) => {
            await portAvailable(unique[index]);
            return acquire(index + 1, portEnv);
          },
          { signal, env: ownedEnv },
        );
  return acquire(0, env);
};

export const generateDevnet = async (
  root,
  runId,
  { signal, env = process.env } = {},
) => {
  const plan = devnetPlan(root, runId);
  const ports = Object.values(plan.env).slice(1).map(Number);
  const pkg = packageByName(root, "midgard-node-tools");
  return withPorts(
    ports,
    async (ownedEnv) => {
      const directory = runDirectory();
      const allocation = {
        ...plan,
        env: {
          ...plan.env,
          MIDGARD_PHASE4_RUN_DIR: resolve(directory, "devnet"),
        },
      };
      const allocationPath = resolve(directory, "allocation.json");
      atomicJson(allocationPath, allocation);
      const before = inputIdentity(root, pkg.name);
      const step = await runProcess({
        argv: [
          "bash",
          "demo/midgard-node-tools/devnet/phase4-process/scripts/generate.sh",
        ],
        cwd: root,
        env: { ...ownedEnv, ...allocation.env },
        signal,
        logPath: resolve(directory, "generate.log"),
      });
      const receipt = writeReceipt({
        root,
        pkg,
        directory,
        kind: "devnet-generation",
        before,
        after: inputIdentity(root, pkg.name),
        steps: [step],
      });
      receipt.allocation = allocation;
      receipt.evidenceFiles = [
        {
          path: allocationPath,
          sha256: sha256(JSON.stringify(allocation, null, 2) + "\n"),
        },
      ];
      if (receipt.exitCode === 0) {
        const outputs = outputIdentity(directory, "devnet");
        if (!Object.keys(outputs.files).length) {
          receipt.status = "failed";
          receipt.exitCode = 1;
          receipt.reason = "devnet generator produced no assets";
        } else
          receipt.retainedArtifacts = [
            {
              name: "devnet-assets",
              root: directory,
              directory: "devnet",
              outputs,
            },
          ];
      }
      atomicJson(receipt.path, receipt);
      return receipt;
    },
    { signal, env },
  );
};

export const acceptancePlan = (root, path) => {
  const config = json(path);
  if (
    !config.runDirectory ||
    !config.nodeRoot ||
    !config.da?.ports ||
    !Array.isArray(config.da.members)
  )
    throw new Error(
      "acceptance requires the existing e2e-stack config schema; read midgard-node-tools/docs/PREPROD_STACK.md",
    );
  if (
    realpathSync(config.nodeRoot) !==
    realpathSync(resolve(root, "demo/midgard-node"))
  )
    throw new Error(
      "acceptance nodeRoot must belong to the selected checkout; select that checkout with --root",
    );
  const ports = [config.da.ports.database, config.da.ports.retainedTransport];
  for (let index = 0; index < config.da.members.length; index += 1)
    ports.push(
      config.da.ports.committeeApiBase + index,
      config.da.ports.committeeTransportBase + index,
    );
  if (
    !config.da.members.length ||
    new Set(ports).size !== ports.length ||
    ports.some(
      (port) =>
        !Number.isSafeInteger(port) ||
        port < 1024 ||
        port > 65535 ||
        port === 5433,
    )
  )
    throw new Error(
      "acceptance ports must be distinct nonprivileged ports outside the shared test database",
    );
  return {
    configSha256: sha256(JSON.stringify(config)),
    runDirectory: resolve(config.runDirectory),
    ports,
    argv: [
      process.execPath,
      resolve(root, "demo/midgard-node-tools/dist/index.js"),
      "e2e-stack",
      "--config",
      resolve(path),
    ],
    scope:
      "execution provenance around the existing stack; acceptance is determined by its saved evidence, never by this wrapper alone",
  };
};

export const runAcceptance = async (
  root,
  path,
  { signal, env = process.env } = {},
) => {
  const plan = acceptancePlan(root, path);
  const pkg = packageByName(root, "midgard-node-tools");
  return withResource(
    `workspace:${realpathSync(root)}`,
    async (workspaceEnv) =>
      withResource(
        `acceptance-run:${plan.runDirectory}`,
        async (ownedEnv) => {
          // Existing runs may legitimately have their ports open when attaching.
          // Claim their keys without probing/freeing them; the stack verifies its
          // recorded process and deployment identities before reuse.
          const ports = [...new Set(plan.ports)].sort((a, b) => a - b);
          const acquire = (index, runEnv) =>
            index === ports.length
              ? execute(runEnv)
              : withResource(
                  `host-port:${ports[index]}`,
                  (next) => acquire(index + 1, next),
                  { signal, env: runEnv },
                );
          const execute = async (runEnv) => {
            for (const name of [
              "midgard-core",
              "lucid-midgard",
              "midgard-sdk",
              "midgard-node",
              "midgard-node-tools",
            ]) {
              if (checkBuild(root, name).status !== "fresh") {
                const built = await buildPackage(root, name, {
                  signal,
                  env: runEnv,
                });
                if (built.exitCode)
                  throw new Error(
                    `acceptance prerequisite failed: ${built.path}`,
                  );
              }
            }
            const directory = runDirectory();
            const before = inputIdentity(root, pkg.name);
            const artifacts = runtimeBuildClosure(root, pkg.name).map(
              (entry) => ({
                name: entry.name,
                outputs: outputIdentity(root, `${entry.directory}/dist`),
              }),
            );
            const step = await runProcess({
              argv: plan.argv,
              cwd: root,
              env: runEnv,
              signal,
              logPath: resolve(directory, "stack.log"),
              timeoutMs: 12 * 60 * 60 * 1000,
            });
            const receipt = writeReceipt({
              root,
              pkg,
              directory,
              kind: "acceptance-execution",
              before,
              after: inputIdentity(root, pkg.name),
              steps: [step],
              proofKind: "live-execution",
            });
            receipt.artifacts = artifacts;
            if (
              artifacts.some(
                (entry) =>
                  entry.outputs.sha256 !==
                  outputIdentity(
                    root,
                    `${packageByName(root, entry.name).directory}/dist`,
                  ).sha256,
              )
            ) {
              receipt.status = "failed";
              receipt.exitCode = 1;
              receipt.reason =
                "compiled artifacts changed during acceptance execution";
            }
            if (acceptancePlan(root, path).configSha256 !== plan.configSha256) {
              receipt.status = "failed";
              receipt.exitCode = 1;
              receipt.reason = "acceptance config changed during execution";
            }
            const summary = resolve(plan.runDirectory, "summary.json");
            receipt.acceptanceEvidence = existsSync(summary)
              ? { path: summary, value: redact(json(summary)) }
              : { status: "absent" };
            receipt.configSha256 = plan.configSha256;
            atomicJson(receipt.path, receipt);
            return receipt;
          };
          return acquire(0, ownedEnv);
        },
        { signal, env: workspaceEnv },
      ),
    { signal, env },
  );
};
