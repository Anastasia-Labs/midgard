import { execFileSync } from "node:child_process";
import { isAbsolute, join } from "node:path";

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
