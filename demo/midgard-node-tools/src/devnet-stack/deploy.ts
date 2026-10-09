import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";

import { ensureL1Origin } from "./deployment-origin.js";
import { readJsonIfPresent, writeDurableFile } from "./durable.js";
import { execLogged, lastJsonValue, requireSuccess } from "./exec.js";
import type { Identities } from "./identities.js";
import type { Layout, RunEnv } from "./layout.js";
import {
  type Artifacts,
  type HubOracleOneShot,
  nodeEnvironment,
} from "./node-env.js";

const sha256File = (path: string) =>
  createHash("sha256").update(readFileSync(path)).digest("hex");

/**
 * Snapshots what the deployment is bound to into the run: the blueprint with
 * its build record and the native binaries. A later rebuild of the checkout
 * cannot then change what a running or restarted service executes.
 */
export const ensureArtifacts = (layout: Layout): Artifacts => {
  const copyOnce = (source: string, target: string, mode: number) => {
    if (existsSync(target)) return;
    if (!existsSync(source))
      throw new Error(`${source} is missing; run the build step first`);
    writeDurableFile(target, readFileSync(source), mode);
  };
  const aiken = join(layout.repoRoot, "onchain/aiken");
  copyOnce(join(aiken, "plutus.json"), layout.blueprint, 0o644);
  copyOnce(
    join(aiken, "plutus.json.deployment.json"),
    `${layout.blueprint}.deployment.json`,
    0o644,
  );
  const nativeOwnerBinary = join(layout.bin, "architecture-g-owner");
  copyOnce(
    join(
      layout.nodeRoot,
      "native/mpf-event-flat-wasm/target/release/architecture-g-owner",
    ),
    nativeOwnerBinary,
    0o755,
  );
  const transportBinary = join(layout.bin, "midgard-l1-node-transport");
  copyOnce(
    join(layout.transportRoot, "dist/native/midgard-l1-node-transport"),
    transportBinary,
    0o755,
  );
  return {
    nativeOwnerBinary,
    nativeOwnerSha256: sha256File(nativeOwnerBinary),
    transportBinary,
  };
};

export type DeployContext = {
  readonly layout: Layout;
  readonly run: RunEnv;
  readonly identities: Identities;
  readonly artifacts: Artifacts;
};

const runNodeCommand = (
  context: DeployContext,
  args: readonly string[],
  label: string,
  options: {
    readonly oneShot?: HubOracleOneShot;
    readonly timeoutMs: number;
  },
) =>
  execLogged(process.execPath, ["dist/index.js", ...args], {
    cwd: context.layout.nodeRoot,
    env: nodeEnvironment({
      ...context,
      oneShot: options.oneShot,
      role: "command",
    }),
    logDir: context.layout.stepLogs,
    label,
    timeoutMs: options.timeoutMs,
  });

/** A node command in the run's node environment. */
export const nodeCli = async (
  context: DeployContext,
  args: readonly string[],
  label: string,
  oneShot?: HubOracleOneShot,
  timeoutMs = 1_800_000,
) =>
  runNodeCommand(context, args, label, {
    oneShot,
    timeoutMs,
  });

type RunState = {
  identity?: {
    hubOracleOneShot?: HubOracleOneShot;
    referenceScriptAuthPolicy?: {
      nativeScript?: { expiresAtUnixTime?: number };
    };
  };
  steps?: Record<string, { status?: string }>;
};

/** The node's REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS default. */
const AUTH_MIN_REMAINING_MS = 5_400_000;

/**
 * The reference-script auth policy is a time-locked native script. A
 * publication interrupted for longer than its window cannot resume under it;
 * before `init` spends the hub-oracle nonce nothing on chain is bound to that
 * policy yet, so a replacement policy is the recovery, not a new deployment.
 */
export const needsFreshAuthPolicy = (
  runState: RunState | undefined,
  nowMs: number,
): boolean => {
  const expiresAt =
    runState?.identity?.referenceScriptAuthPolicy?.nativeScript
      ?.expiresAtUnixTime;
  return expiresAt !== undefined && expiresAt - nowMs < AUTH_MIN_REMAINING_MS;
};

const nonceUnspent = async (run: RunEnv, oneShot: HubOracleOneShot) => {
  const response = await fetch(
    `http://127.0.0.1:${run.kupoPort}/matches/${oneShot.outputIndex}@${oneShot.txHash}?unspent`,
    { signal: AbortSignal.timeout(10_000) },
  );
  if (!response.ok)
    throw new Error(
      `Kupo answered ${response.status} for the hub-oracle nonce`,
    );
  return ((await response.json()) as unknown[]).length > 0;
};

/** Attempts before a publication failure is reported instead of retried. */
const PUBLICATION_ATTEMPTS = 6;

/**
 * Publishes the node-runtime reference scripts. The command resumes from its
 * run-state, so a failure (a stalled provider on a loaded host) is retried
 * from where it stopped; each attempt re-decides whether the auth policy
 * window still allows resuming under the recorded policy.
 */
const publishReferenceScripts = async (
  context: DeployContext,
  oneShot: HubOracleOneShot,
  runState: () => RunState | undefined,
) => {
  const { layout } = context;
  for (let attempt = 1; ; attempt += 1) {
    const fresh =
      needsFreshAuthPolicy(runState(), Date.now()) &&
      (await nonceUnspent(context.run, oneShot))
        ? [
            "--fresh-redeploy",
            "--fresh-redeploy-reason",
            "reference-script auth policy expired before publication completed; protocol not initialized",
          ]
        : [];
    if (fresh.length > 0)
      console.log(
        "deploy: auth policy window spent before init; publishing under a replacement policy",
      );
    const result = await nodeCli(
      context,
      [
        "deploy-reference-script-node-runtime",
        "--run-state",
        layout.deploymentRunState,
        "--contract-deployment-info-output",
        layout.contractManifest,
        ...fresh,
      ],
      "reference-scripts",
      oneShot,
      // Publishing the full roster takes well over 30 minutes on a devnet.
      4 * 3_600_000,
    );
    if (result.code === 0) return;
    if (attempt >= PUBLICATION_ATTEMPTS)
      requireSuccess(result, "reference-scripts");
    console.log(
      `deploy: reference-script publication failed (attempt ${attempt}/${PUBLICATION_ATTEMPTS}, see ${result.log}); resuming in 60 s`,
    );
    await new Promise((resolve) => setTimeout(resolve, 60_000));
  }
};

type ContractManifest = {
  steps?: { initProtocol?: { status?: string; txHash?: string } };
  hubOracleOneShot?: { txHash?: string; outputIndex?: number };
};

/**
 * The fresh-deployment sequence of operator commands. Each command is
 * idempotent or resumes from the run-state it records; completion is read
 * from those records and the chain, never assumed from a previous run.
 */
export const ensureDeployed = async (
  context: DeployContext,
): Promise<HubOracleOneShot> => {
  const { layout } = context;
  const step = async (
    args: readonly string[],
    label: string,
    oneShot?: HubOracleOneShot,
    timeoutMs?: number,
  ) =>
    requireSuccess(
      await nodeCli(context, args, label, oneShot, timeoutMs),
      label,
    );

  await step(["db:migrate"], "db-migrate");

  const runState = () => readJsonIfPresent<RunState>(layout.deploymentRunState);
  if (runState()?.steps?.hubOracleNonce?.status !== "complete")
    await step(
      [
        "prepare-hub-oracle-one-shot-nonce",
        "--run-state",
        layout.deploymentRunState,
        "--json",
      ],
      "hub-oracle-nonce",
    );
  const oneShot = runState()?.identity?.hubOracleOneShot;
  if (
    oneShot === undefined ||
    runState()?.steps?.hubOracleNonce?.status !== "complete"
  )
    throw new Error(
      `${layout.deploymentRunState} records no completed hub-oracle nonce`,
    );
  // The node's follower starts at the point before the nonce block; a run
  // whose origin cannot be found never starts a node.
  const origin = await ensureL1Origin(context, oneShot);
  console.log(`deploy: L1 origin ${origin.slot}.${origin.blockHash}`);

  const manifest = () =>
    readJsonIfPresent<ContractManifest>(layout.contractManifest);
  if (manifest()?.steps?.initProtocol?.status !== "complete") {
    await publishReferenceScripts(context, oneShot, runState);
    await step(
      [
        "reconcile",
        "reference-scripts-complete",
        "--scope",
        "node-runtime",
        "--json",
      ],
      "reference-scripts-reconcile",
      oneShot,
    );
    await step(
      ["init", "--contract-deployment-info-output", layout.contractManifest],
      "init",
      oneShot,
    );
  }
  if (manifest()?.steps?.initProtocol?.status !== "complete")
    throw new Error(
      "init returned without recording initProtocol in the manifest",
    );
  await step(
    ["reconcile", "phas-registered", "--json"],
    "phas-reconcile",
    oneShot,
  );
  await step(["register-active-operator"], "register-active-operator", oneShot);
  return oneShot;
};

export const deploymentStatus = async (
  context: DeployContext,
  oneShot: HubOracleOneShot,
) =>
  lastJsonValue(
    requireSuccess(
      await nodeCli(
        context,
        ["deployment-status"],
        "deployment-status",
        oneShot,
      ),
      "deployment-status",
    ).stdout,
  );
