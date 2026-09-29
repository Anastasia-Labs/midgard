/**
 * The DA libp2p runtime of the pooled DA bond journey's committee node
 * (ruling P31): the libp2p keys and the committee-target runtime manifest
 * that the observer of ruling P27 loads.
 *
 * The journey deployment writes neither, so the live adapter produces both
 * after the deploy and before the node's first spawn, through the same path
 * an operator uses after init: fresh libp2p keys, then the real
 * `midgard-node da-libp2p-generate-manifest --target committee` process. The
 * members' signer indexes and DA verification keys and the threshold come
 * from the finalized deployment manifest, never from hand-written JSON.
 *
 * The observer loads one member's libp2p identity, so the manifest's peer set
 * admits it. It never loads that member's DA signing key: libp2p identity is
 * transport-only, and `DA_SIGNER_INDEX` stays unset (P27(1)).
 *
 * Planning and the evidence checks are pure; the key writer, the process run
 * and the configuration check take their inputs explicitly, so both
 * polarities are testable without a devnet.
 */
import { execFileSync, spawnSync } from "node:child_process";
import { createHash, randomBytes } from "node:crypto";
import { existsSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { isAbsolute, join } from "node:path";

import {
  identityFromSeedHex,
  loadDaLibp2pIdentity,
} from "@al-ft/midgard-core/da-libp2p-identity";
import { loadCommitteeConfig } from "da-committee-node/config";
import { DaPeerRegistry } from "da-committee-node/da/libp2p";

// ---------------------------------------------------------------------------
// Paths, ports and roles
// ---------------------------------------------------------------------------

/** The DA runtime manifest the committee node loads, relative to the run directory. */
export const DA_BOND_POOL_COMMITTEE_RUNTIME_MANIFEST =
  "deploymentInfo/da-runtime-manifest.json";

/** The libp2p key files the adapter writes, relative to the run directory. */
export const DA_BOND_POOL_LIBP2P_SECRETS = Object.freeze({
  member: (signerIndex: number): string =>
    `secrets/da-bond-pool-libp2p-committee-${signerIndex.toString()}.key`,
  producer: "secrets/da-bond-pool-libp2p-producer.key",
  publicRetainedDa: "secrets/da-bond-pool-libp2p-public-retained-da.key",
});

/**
 * The generator's default libp2p ports, which the main checkout keeps. A
 * linked worktree adds its `portOffset` from `scripts/lib/worktree-identity.mjs`,
 * the offset the phase 4 devnet's `generate.sh` adds to its default ports.
 * The offset is a multiple of 10, so each checkout owns the block of ten ports
 * from `committee + offset`.
 */
export const DA_BOND_POOL_LIBP2P_DEFAULT_PORTS = Object.freeze({
  committee: 39_001,
  producer: 39_002,
  publicRetainedDa: 39_003,
});
const PORT_BLOCK_SIZE = 10;

/**
 * Every member's roles: the committee role, the L1 coordinator role every
 * member's own submitter has, and retrieval, as in the operator bring-up.
 */
export const DA_BOND_POOL_COMMITTEE_MEMBER_ROLES = Object.freeze([
  "committee",
  "coordinator",
  "retrieval",
]);

/** How the committee runtime cannot be produced or is refused. */
export class DaBondPoolCommitteeRuntimeError extends Error {
  constructor(problem: string, options?: ErrorOptions) {
    super(
      `The DA bond pool journey cannot produce its committee runtime (ruling P31): ${problem}`,
      options,
    );
    this.name = "DaBondPoolCommitteeRuntimeError";
  }
}

/**
 * This checkout's host-port offset, read from `scripts/lib/worktree-identity.mjs`
 * exactly as the phase 4 devnet's `generate.sh` reads it.
 */
export const readWorktreePortOffset = (repositoryRoot: string): number => {
  const raw = execFileSync(
    process.execPath,
    [
      join(repositoryRoot, "scripts/lib/worktree-identity.mjs"),
      "portOffset",
      "--root",
      repositoryRoot,
    ],
    { encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] },
  ).trim();
  const offset = Number(raw);
  if (!/^\d+$/u.test(raw) || !Number.isSafeInteger(offset))
    throw new DaBondPoolCommitteeRuntimeError(
      `the worktree port offset is not a natural number: ${JSON.stringify(raw)}`,
    );
  return offset;
};

// ---------------------------------------------------------------------------
// The plan
// ---------------------------------------------------------------------------

/** The parts of a finalized deployment manifest the plan reads. */
export type DaBondPoolCommitteeDeployment = Readonly<{
  network: string;
  da: Readonly<{ committeeVkeys: readonly string[]; threshold: number }>;
}>;

export type DaBondPoolCommitteeRuntimeMember = Readonly<{
  signerIndex: number;
  daVkey: string;
  libp2pPrivateKeySource: string;
  roles: readonly string[];
  /** Absent for the observer, which listens on the committee port. */
  endpoint?: Readonly<{ port: number }>;
}>;

export type DaBondPoolCommitteeRuntimePlan = Readonly<{
  network: string;
  threshold: number;
  contractDeploymentInfoPath: string;
  outPath: string;
  /** Absolute paths of every key file the adapter writes, in write order. */
  keyPaths: readonly string[];
  producerPrivateKeySource: string;
  publicRetainedDaPrivateKeySource: string;
  members: readonly DaBondPoolCommitteeRuntimeMember[];
  ports: Readonly<{
    committee: number;
    producer: number;
    publicRetainedDa: number;
  }>;
  observer: Readonly<{ signerIndex: number; libp2pKeySource: string }>;
}>;

/**
 * The committee runtime for a run directory: one fresh libp2p key per DA
 * committee member of the deployment, a producer key and a public retained-DA
 * key, the ports, and the observer member. The observer listens on the
 * committee port; every other member gets its own port after the public
 * retained-DA port, inside this checkout's block of ten.
 */
export const planDaBondPoolCommitteeRuntime = (input: {
  readonly runDirectory: string;
  readonly deployment: DaBondPoolCommitteeDeployment;
  readonly portOffset: number;
  /** Default: the member with signer index 0. */
  readonly observerSignerIndex?: number;
}): DaBondPoolCommitteeRuntimePlan => {
  const { runDirectory, deployment, portOffset } = input;
  if (!isAbsolute(runDirectory))
    throw new DaBondPoolCommitteeRuntimeError(
      `the run directory ${runDirectory} is not absolute`,
    );
  if (
    !Number.isSafeInteger(portOffset) ||
    portOffset < 0 ||
    portOffset % PORT_BLOCK_SIZE !== 0
  )
    throw new DaBondPoolCommitteeRuntimeError(
      `the port offset ${String(portOffset)} is not a natural multiple of ${PORT_BLOCK_SIZE.toString()}`,
    );
  const vkeys = deployment.da.committeeVkeys;
  if (vkeys.length === 0)
    throw new DaBondPoolCommitteeRuntimeError(
      "the deployment manifest names no DA committee member",
    );
  const { threshold } = deployment.da;
  if (
    !Number.isSafeInteger(threshold) ||
    threshold <= 0 ||
    threshold > vkeys.length
  )
    throw new DaBondPoolCommitteeRuntimeError(
      `the deployment's DA threshold ${String(threshold)} does not fit its ${vkeys.length.toString()} member(s)`,
    );
  const observerSignerIndex = input.observerSignerIndex ?? 0;
  if (
    !Number.isSafeInteger(observerSignerIndex) ||
    observerSignerIndex < 0 ||
    observerSignerIndex >= vkeys.length
  )
    throw new DaBondPoolCommitteeRuntimeError(
      `the observer signer index ${String(observerSignerIndex)} names no member of the deployment's DA committee`,
    );
  const ports = {
    committee: DA_BOND_POOL_LIBP2P_DEFAULT_PORTS.committee + portOffset,
    producer: DA_BOND_POOL_LIBP2P_DEFAULT_PORTS.producer + portOffset,
    publicRetainedDa:
      DA_BOND_POOL_LIBP2P_DEFAULT_PORTS.publicRetainedDa + portOffset,
  };
  const blockEnd = ports.committee + PORT_BLOCK_SIZE - 1;
  const keyPath = (relative: string) => join(runDirectory, relative);
  let nextPort = ports.publicRetainedDa;
  const members = vkeys.map((daVkey, signerIndex) => {
    const libp2pPrivateKeySource = `file:${keyPath(DA_BOND_POOL_LIBP2P_SECRETS.member(signerIndex))}`;
    if (signerIndex === observerSignerIndex)
      return {
        signerIndex,
        daVkey,
        libp2pPrivateKeySource,
        roles: DA_BOND_POOL_COMMITTEE_MEMBER_ROLES,
      };
    nextPort += 1;
    if (nextPort > blockEnd)
      throw new DaBondPoolCommitteeRuntimeError(
        `${vkeys.length.toString()} committee members do not fit this checkout's libp2p ports ${ports.committee.toString()}-${blockEnd.toString()}`,
      );
    return {
      signerIndex,
      daVkey,
      libp2pPrivateKeySource,
      roles: DA_BOND_POOL_COMMITTEE_MEMBER_ROLES,
      endpoint: { port: nextPort },
    };
  });
  const producer = keyPath(DA_BOND_POOL_LIBP2P_SECRETS.producer);
  const publicRetainedDa = keyPath(
    DA_BOND_POOL_LIBP2P_SECRETS.publicRetainedDa,
  );
  return {
    network: deployment.network,
    threshold,
    contractDeploymentInfoPath: join(
      runDirectory,
      "deploymentInfo/manifest.json",
    ),
    outPath: keyPath(DA_BOND_POOL_COMMITTEE_RUNTIME_MANIFEST),
    keyPaths: [
      ...members.map((member) =>
        member.libp2pPrivateKeySource.slice("file:".length),
      ),
      producer,
      publicRetainedDa,
    ],
    producerPrivateKeySource: `file:${producer}`,
    publicRetainedDaPrivateKeySource: `file:${publicRetainedDa}`,
    members,
    ports,
    observer: {
      signerIndex: observerSignerIndex,
      libp2pKeySource: members[observerSignerIndex]!.libp2pPrivateKeySource,
    },
  };
};

/**
 * The generator's options for a plan: the committee target on the `host`
 * profile, with the observer as the local member. The live adapter passes
 * exactly these as `da-libp2p-generate-manifest` arguments.
 */
export const daBondPoolCommitteeRuntimeOptions = (
  plan: DaBondPoolCommitteeRuntimePlan,
) =>
  ({
    target: "committee",
    profile: "host",
    contractDeploymentInfoPath: plan.contractDeploymentInfoPath,
    network: plan.network,
    producerPrivateKeySource: plan.producerPrivateKeySource,
    publicRetainedDaPrivateKeySource: plan.publicRetainedDaPrivateKeySource,
    committeeMembers: plan.members,
    threshold: plan.threshold,
    localSignerIndex: plan.observer.signerIndex,
    producerPort: plan.ports.producer,
    committeePort: plan.ports.committee,
    publicRetainedDaPort: plan.ports.publicRetainedDa,
  }) as const;

/**
 * The `da-libp2p-generate-manifest` arguments for a plan. They carry key
 * sources (`file:` paths), never key bytes.
 */
export const daBondPoolCommitteeRuntimeArgv = (
  plan: DaBondPoolCommitteeRuntimePlan,
): readonly string[] => {
  const options = daBondPoolCommitteeRuntimeOptions(plan);
  return [
    "da-libp2p-generate-manifest",
    "--target",
    options.target,
    "--profile",
    options.profile,
    "--contract-deployment-info",
    options.contractDeploymentInfoPath,
    "--network",
    options.network,
    "--threshold",
    options.threshold.toString(),
    "--producer-libp2p-key-source",
    options.producerPrivateKeySource,
    "--public-retained-da-libp2p-key-source",
    options.publicRetainedDaPrivateKeySource,
    ...options.committeeMembers.flatMap((member) => [
      "--committee-member",
      [
        member.signerIndex.toString(),
        member.daVkey,
        member.libp2pPrivateKeySource,
        member.roles.join("+"),
        ...(member.endpoint === undefined
          ? []
          : [member.endpoint.port.toString()]),
      ].join(","),
    ]),
    "--local-signer-index",
    options.localSignerIndex.toString(),
    "--producer-port",
    options.producerPort.toString(),
    "--committee-port",
    options.committeePort.toString(),
    "--public-retained-da-port",
    options.publicRetainedDaPort.toString(),
    "--out",
    plan.outPath,
  ];
};

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
