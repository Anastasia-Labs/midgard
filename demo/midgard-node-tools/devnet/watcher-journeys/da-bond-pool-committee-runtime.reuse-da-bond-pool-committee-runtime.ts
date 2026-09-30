import { spawnSync } from "node:child_process";
import { createHash, randomBytes } from "node:crypto";
import { existsSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { isAbsolute } from "node:path";

import {
  identityFromSeedHex,
  loadDaLibp2pIdentity,
} from "@al-ft/midgard-core/da-libp2p-identity";
import { loadCommitteeConfig } from "da-committee-node/config";
import { DaPeerRegistry } from "da-committee-node/da/libp2p";

import {
  daBondPoolCommitteeRuntimeArgv,
  DaBondPoolCommitteeRuntimeError,
  type DaBondPoolCommitteeRuntimePlan,
} from "./da-bond-pool-committee-runtime.plan-da-bond-pool-committee-runtime.js";

// ---------------------------------------------------------------------------
// Keys
// ---------------------------------------------------------------------------

/**
 * A fresh libp2p identity from 32 random bytes, written as the key file
 * `DA_LIBP2P_PRIVATE_KEY_SOURCE=file:` reads (its protobuf private key, hex),
 * mode 0600. Refuses to overwrite an existing file.
 */
export const writeFreshDaLibp2pKey = async (
  path: string,
): Promise<Readonly<{ source: string; peerId: string }>> => {
  if (!isAbsolute(path))
    throw new DaBondPoolCommitteeRuntimeError(
      `the libp2p key path ${path} is not absolute`,
    );
  const identity = await identityFromSeedHex(randomBytes(32).toString("hex"));
  try {
    writeFileSync(path, `${identity.privateKeyProtobufHex}\n`, {
      mode: 0o600,
      flag: "wx",
    });
  } catch (cause) {
    if ((cause as NodeJS.ErrnoException).code === "EEXIST")
      throw new DaBondPoolCommitteeRuntimeError(
        `the libp2p key ${path} already exists; the adapter never overwrites one, so run the journey on a freshly deployed run directory`,
        { cause },
      );
    throw cause;
  }
  return { source: `file:${path}`, peerId: identity.peerId };
};

// ---------------------------------------------------------------------------
// The generator process
// ---------------------------------------------------------------------------

export type DaBondPoolRuntimeProcessResult = Readonly<{
  exitCode: number | null;
  signal: string | null;
  stdout: string;
  stderr: string;
}>;

export type DaBondPoolRuntimeProcessRunner = (input: {
  readonly argv: readonly string[];
  readonly env: Readonly<Record<string, string>>;
  readonly cwd: string;
}) => DaBondPoolRuntimeProcessResult;

/** Runs one process to completion, bounded by `timeoutMs`. */
export const spawnDaBondPoolRuntimeProcess =
  (timeoutMs: number): DaBondPoolRuntimeProcessRunner =>
  ({ argv, env, cwd }) => {
    const result = spawnSync(argv[0]!, argv.slice(1), {
      env,
      cwd,
      encoding: "utf8",
      timeout: timeoutMs,
      stdio: ["ignore", "pipe", "pipe"],
    });
    if (result.error !== undefined)
      throw new DaBondPoolCommitteeRuntimeError(
        `could not run ${argv.slice(0, 3).join(" ")}: ${result.error.message}`,
        { cause: result.error },
      );
    return {
      exitCode: result.status,
      signal: result.signal,
      stdout: result.stdout,
      stderr: result.stderr,
    };
  };

/** What the adapter records about the runtime it produced. */
export type DaBondPoolCommitteeRuntimeEvidence = Readonly<{
  /** The generator's argument vector: key sources, never key bytes. */
  argv: readonly string[];
  exitCode: number;
  outPath: string;
  outputSha256: string;
  observer: Readonly<{
    signerIndex: number;
    libp2pKeySource: string;
    peerId: string;
  }>;
  /** Every written key's source and peer id; no key bytes. */
  keys: readonly Readonly<{ source: string; peerId: string }>[];
  ports: DaBondPoolCommitteeRuntimePlan["ports"];
}>;

const tail = (text: string): string =>
  text.trim().split("\n").slice(-20).join("\n");

/**
 * Writes the plan's keys, then runs the real generator (`command` is the
 * built `midgard-node` entry, for example `[node, dist/index.js]`). Refuses a
 * non-zero exit, a missing output, and an output whose local member is not the
 * observer. Refuses before any key is written if the output already exists.
 */
export const produceDaBondPoolCommitteeRuntime = async (input: {
  readonly plan: DaBondPoolCommitteeRuntimePlan;
  readonly command: readonly string[];
  readonly env: Readonly<Record<string, string>>;
  readonly cwd: string;
  readonly run: DaBondPoolRuntimeProcessRunner;
}): Promise<DaBondPoolCommitteeRuntimeEvidence> => {
  const { plan } = input;
  if (existsSync(plan.outPath))
    throw new DaBondPoolCommitteeRuntimeError(
      `the runtime manifest ${plan.outPath} already exists; the adapter never overwrites one, so run the journey on a freshly deployed run directory`,
    );
  const keys: { source: string; peerId: string }[] = [];
  for (const path of plan.keyPaths)
    keys.push(await writeFreshDaLibp2pKey(path));
  const argv = [...input.command, ...daBondPoolCommitteeRuntimeArgv(plan)];
  const result = input.run({ argv, env: input.env, cwd: input.cwd });
  if (result.exitCode !== 0)
    throw new DaBondPoolCommitteeRuntimeError(
      `da-libp2p-generate-manifest exited ${String(result.exitCode)}${result.signal === null ? "" : ` (${result.signal})`}: ${tail(result.stderr)}`,
    );
  if (!existsSync(plan.outPath))
    throw new DaBondPoolCommitteeRuntimeError(
      `da-libp2p-generate-manifest exited 0 but wrote no ${plan.outPath}`,
    );
  const raw = readFileSync(plan.outPath);
  const written = JSON.parse(raw.toString("utf8")) as {
    runtime_topology?: { target?: unknown; local_signer_index?: unknown };
  };
  if (
    written.runtime_topology?.target !== "committee" ||
    written.runtime_topology.local_signer_index !== plan.observer.signerIndex
  )
    throw new DaBondPoolCommitteeRuntimeError(
      `the runtime manifest's topology ${JSON.stringify(written.runtime_topology)} is not the committee target with local signer index ${plan.observer.signerIndex.toString()}`,
    );
  const observerKey = keys.find(
    (key) => key.source === plan.observer.libp2pKeySource,
  );
  if (observerKey === undefined)
    throw new DaBondPoolCommitteeRuntimeError(
      "the observer's libp2p key is not among the written keys",
    );
  return {
    argv,
    exitCode: result.exitCode,
    outPath: plan.outPath,
    outputSha256: createHash("sha256").update(raw).digest("hex"),
    observer: { ...plan.observer, peerId: observerKey.peerId },
    keys,
    ports: plan.ports,
  };
};

/**
 * The runtime an earlier run of this run directory produced, for a resumed
 * journey (a smoke on a kept devnet). Never runs the generator and never
 * writes a key. Refuses unless the recorded evidence (`runtime.json`) names
 * the plan's manifest, observer and keys, the manifest still hashes to the
 * recorded digest and is the committee target for the observer, and every
 * key file exists and is readable by its owner only.
 */
export const reuseDaBondPoolCommitteeRuntime = (input: {
  readonly plan: DaBondPoolCommitteeRuntimePlan;
  readonly recordedEvidencePath: string;
}): DaBondPoolCommitteeRuntimeEvidence => {
  const { plan, recordedEvidencePath } = input;
  if (!existsSync(recordedEvidencePath))
    throw new DaBondPoolCommitteeRuntimeError(
      `a resumed journey needs the earlier run's ${recordedEvidencePath}`,
    );
  const evidence = JSON.parse(
    readFileSync(recordedEvidencePath, "utf8"),
  ) as DaBondPoolCommitteeRuntimeEvidence;
  const expectedSources = plan.keyPaths.map((path) => `file:${path}`);
  const recordedSources = evidence.keys.map((key) => key.source);
  if (
    evidence.outPath !== plan.outPath ||
    evidence.observer.signerIndex !== plan.observer.signerIndex ||
    evidence.observer.libp2pKeySource !== plan.observer.libp2pKeySource ||
    JSON.stringify(recordedSources) !== JSON.stringify(expectedSources)
  )
    throw new DaBondPoolCommitteeRuntimeError(
      `${recordedEvidencePath} records the runtime ${evidence.outPath} with observer ${evidence.observer.signerIndex.toString()} (${evidence.observer.libp2pKeySource}) and keys [${recordedSources.join(", ")}], not this plan's ${plan.outPath} with observer ${plan.observer.signerIndex.toString()} (${plan.observer.libp2pKeySource}) and keys [${expectedSources.join(", ")}]`,
    );
  if (!existsSync(plan.outPath))
    throw new DaBondPoolCommitteeRuntimeError(
      `the recorded runtime manifest ${plan.outPath} is missing`,
    );
  const raw = readFileSync(plan.outPath);
  const sha256 = createHash("sha256").update(raw).digest("hex");
  if (sha256 !== evidence.outputSha256)
    throw new DaBondPoolCommitteeRuntimeError(
      `the runtime manifest ${plan.outPath} hashes to ${sha256}, not the recorded ${evidence.outputSha256}`,
    );
  const written = JSON.parse(raw.toString("utf8")) as {
    runtime_topology?: { target?: unknown; local_signer_index?: unknown };
  };
  if (
    written.runtime_topology?.target !== "committee" ||
    written.runtime_topology.local_signer_index !== plan.observer.signerIndex
  )
    throw new DaBondPoolCommitteeRuntimeError(
      `the runtime manifest's topology ${JSON.stringify(written.runtime_topology)} is not the committee target with local signer index ${plan.observer.signerIndex.toString()}`,
    );
  for (const path of plan.keyPaths) {
    if (!existsSync(path))
      throw new DaBondPoolCommitteeRuntimeError(
        `the recorded libp2p key ${path} is missing`,
      );
    const mode = statSync(path).mode & 0o777;
    if ((mode & 0o077) !== 0)
      throw new DaBondPoolCommitteeRuntimeError(
        `the libp2p key ${path} is readable by others (mode ${mode.toString(8)}); it must be 0600`,
      );
  }
  return evidence;
};

// ---------------------------------------------------------------------------
// The committee node's own acceptance
// ---------------------------------------------------------------------------

/**
 * The deployment, network and L1 source variables of the committee node's
 * environment, from the pair of manifests it loads.
 */
export const daBondPoolCommitteeSettings = (input: {
  readonly runtimeManifestPath: string;
  readonly deploymentManifestPath: string;
  readonly network: string;
  /** Required for `Custom`; omitted for a named network. */
  readonly networkMagic?: number;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly chainSyncCursorPath: string;
  readonly finalityDepth: number;
  readonly nativeLedger?: Readonly<{
    socket: string;
    config: string;
    binary: string;
  }>;
}): Readonly<Record<string, string>> => ({
  MIDGARD_DEPLOYMENT_MANIFEST_PATH: input.runtimeManifestPath,
  MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: input.deploymentManifestPath,
  MIDGARD_NETWORK: input.network,
  ...(input.networkMagic === undefined
    ? {}
    : { CARDANO_NETWORK_MAGIC: input.networkMagic.toString() }),
  CARDANO_PROVIDER_URLS: `kupmios:${input.kupoUrl}|${input.ogmiosUrl}`,
  CARDANO_L1_SOURCE_MODE: "local_node",
  CARDANO_LOCAL_NODE_AUTHORITY_ID: "local-cardano-node",
  CARDANO_LOCAL_NODE_CHAIN_SYNC_URL: `chain-sync:kupmios:${input.kupoUrl}|${input.ogmiosUrl}`,
  CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH: input.chainSyncCursorPath,
  CARDANO_FINALITY_DEPTH: input.finalityDepth.toString(),
  ...(input.nativeLedger === undefined
    ? {}
    : {
        CARDANO_LOCAL_NODE_SOCKET_PATH: input.nativeLedger.socket,
        CARDANO_LOCAL_NODE_CONFIG_PATH: input.nativeLedger.config,
        CARDANO_NATIVE_CHAIN_SYNC_BINARY_PATH: input.nativeLedger.binary,
      }),
});

/**
 * The start-up path of `da-committee-node` main() up to its peer check: the
 * node's own configuration loader over `env`, libp2p DA mode, no DA signer,
 * and a libp2p identity the runtime manifest's committee peer set admits
 * (`requireKnownPeer`). Returns the admitted peer id.
 */
export const verifyDaBondPoolCommitteeRuntime = async (
  env: Readonly<Record<string, string | undefined>>,
): Promise<Readonly<{ peerId: string }>> => {
  const config = await loadCommitteeConfig({ ...env });
  if (config.daTransport.kind !== "libp2p")
    throw new Error("the runtime manifest is not in libp2p DA mode");
  if (config.signerIndex !== undefined || config.signerKeySource !== undefined)
    throw new Error("the observer must hold no DA signer");
  if (config.libp2pPrivateKeySource === undefined)
    throw new Error("DA_LIBP2P_PRIVATE_KEY_SOURCE is unset");
  const { peerId } = await loadDaLibp2pIdentity(config.libp2pPrivateKeySource);
  DaPeerRegistry.fromConfig(config.daTransport).requireKnownPeer(peerId);
  return { peerId };
};
