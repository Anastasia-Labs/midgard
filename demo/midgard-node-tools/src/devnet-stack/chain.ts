import { existsSync, readFileSync, renameSync } from "node:fs";
import { basename, join } from "node:path";

import { writeDurableFile, writeDurableJson } from "./durable.js";
import { execLogged, type ExecResult, requireSuccess } from "./exec.js";
import { type Layout, readRunEnv, type RunEnv } from "./layout.js";

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

/**
 * Generates the private chain once. A directory left behind by a generation
 * that died before writing run.env never started a chain, so it is moved
 * aside (not deleted) and generation starts again.
 */
export const ensureChainGenerated = async (layout: Layout): Promise<RunEnv> => {
  if (!existsSync(layout.runEnv)) {
    if (existsSync(layout.runDir)) {
      const aside = `${layout.runDir}.partial-${Date.now()}`;
      if (existsSync(join(layout.runDir, "cardano/db/immutable")))
        throw new Error(
          `${layout.runDir} has chain data but no run.env; preserve it and choose another run directory`,
        );
      renameSync(layout.runDir, aside);
      console.log(`moved an incomplete generation aside to ${aside}`);
    }
    requireSuccess(
      await execLogged("sh", [join(layout.phase4Scripts, "generate.sh")], {
        env: {
          MIDGARD_PHASE4_RUN_DIR: layout.runDir,
          MIDGARD_PHASE4_RUN_ID: basename(layout.runDir),
        },
        logDir: join(layout.runDir, "..", `.${basename(layout.runDir)}-logs`),
        label: "generate",
        timeoutMs: 600_000,
      }),
      "devnet chain generation",
    );
  }
  return readRunEnv(layout);
};

/**
 * Docker's json-file driver keeps a container's whole stdout unless capped:
 * on a 1-second-slot chain the four L1 containers wrote 24 MB a minute, which
 * fills the host within days and then fails every database and the chain at
 * once. Nothing reads these logs (the controller reads its own step logs), so
 * each service keeps at most three 100 MB files.
 */
export const L1_CONTAINER_LOGGING = {
  driver: "json-file",
  options: { "max-size": "100m", "max-file": "3" },
} as const;

export const L1_SERVICES = [
  "cardano-node",
  "ogmios",
  "kupo",
  "postgres",
] as const;

/**
 * The phase4 compose file runs the L1 containers with `restart: "no"` for its
 * crash matrix. The long-running stack needs them supervised, and the node
 * socket must stay reachable by host processes after every restart, which a
 * one-off chmod does not survive: the node re-creates the socket with the
 * container's umask. Written as JSON, which compose reads as YAML; the next
 * `up` of an existing run recreates its containers with these settings.
 */
export const SUPERVISED_COMPOSE = `${JSON.stringify(
  {
    services: Object.fromEntries(
      L1_SERVICES.map((service) => [
        service,
        {
          ...(service === "cardano-node"
            ? {
                entrypoint: [
                  "/bin/sh",
                  "-c",
                  'umask 0000 && exec /usr/local/bin/entrypoint "$$@"',
                  "entrypoint",
                ],
              }
            : {}),
          restart: "unless-stopped",
          logging: L1_CONTAINER_LOGGING,
        },
      ]),
    ),
  },
  null,
  2,
)}\n`;

export const compose = (
  layout: Layout,
  run: RunEnv,
  args: readonly string[],
  label: string,
  input?: string,
): Promise<ExecResult> =>
  execLogged(
    "docker",
    [
      "compose",
      "--project-name",
      run.composeProject,
      "--file",
      layout.composeFile,
      "--file",
      layout.supervisedComposeFile,
      ...args,
    ],
    {
      env: { MIDGARD_PHASE4_RUN_DIR: layout.runDir, ...composeVariables(run) },
      logDir: layout.stepLogs,
      label,
      timeoutMs: 600_000,
      input,
    },
  );

const composeVariables = (run: RunEnv) => ({
  MIDGARD_PHASE4_COMPOSE_PROJECT: run.composeProject,
  MIDGARD_PHASE4_OGMIOS_PORT: String(run.ogmiosPort),
  MIDGARD_PHASE4_KUPO_PORT: String(run.kupoPort),
  MIDGARD_PHASE4_POSTGRES_PORT: String(run.postgresPort),
  MIDGARD_PHASE4_POSTGRES_USER: run.postgresUser,
  MIDGARD_PHASE4_POSTGRES_PASSWORD: run.postgresPassword,
  MIDGARD_PHASE4_POSTGRES_DATABASE: run.postgresDatabase,
});

export const startL1 = async (layout: Layout, run: RunEnv): Promise<void> => {
  writeDurableFile(layout.supervisedComposeFile, SUPERVISED_COMPOSE);
  writeHostCardanoConfig(layout);
  requireSuccess(
    await compose(layout, run, ["up", "--detach", "--wait"], "l1-up"),
    "starting the L1 containers",
  );
  await waitL1Ready(run, 600_000);
  await reportL1Liveness(layout, run);
};

/**
 * Host processes (native ledger reads, chain-sync helpers) read the node
 * config with host paths; the container config names /genesis/.
 */
const writeHostCardanoConfig = (layout: Layout) => {
  const config = JSON.parse(
    readFileSync(join(layout.runDir, "config/config.json"), "utf8"),
  ) as Record<string, unknown>;
  const genesis = join(layout.runDir, "genesis");
  const rewritten = Object.fromEntries(
    Object.entries(config).map(([key, value]) => [
      key,
      typeof value === "string" && value.startsWith("/genesis/")
        ? join(genesis, value.slice("/genesis/".length))
        : value,
    ]),
  );
  writeDurableJson(layout.hostCardanoConfig, rewritten);
};

export type OgmiosHealth = {
  readonly networkSynchronization: number;
  readonly lastKnownTip?: { readonly slot: number };
  readonly connectionStatus: string;
};

export const ogmiosHealth = async (run: RunEnv): Promise<OgmiosHealth> => {
  const response = await fetch(`http://127.0.0.1:${run.ogmiosPort}/health`, {
    signal: AbortSignal.timeout(5_000),
  });
  return (await response.json()) as OgmiosHealth;
};

const kupoHealthy = async (run: RunEnv): Promise<boolean> => {
  const response = await fetch(`http://127.0.0.1:${run.kupoPort}/health`, {
    headers: { accept: "application/json" },
    signal: AbortSignal.timeout(5_000),
  });
  if (response.status !== 200) return false;
  const body = (await response.json()) as { connection_status?: string };
  return body.connection_status === "connected";
};

/** Ready = Ogmios synced and connected, Kupo caught up, blocks advancing. */
export const waitL1Ready = async (run: RunEnv, timeoutMs: number) => {
  const deadline = Date.now() + timeoutMs;
  let firstSlot: number | undefined;
  let last = "no answer yet";
  while (Date.now() < deadline) {
    try {
      const ogmios = await ogmiosHealth(run);
      const slot = ogmios.lastKnownTip?.slot;
      last = `ogmios ${ogmios.connectionStatus} sync=${ogmios.networkSynchronization} slot=${slot}`;
      if (
        ogmios.connectionStatus === "connected" &&
        ogmios.networkSynchronization >= 0.999 &&
        slot !== undefined &&
        (await kupoHealthy(run))
      ) {
        if (firstSlot === undefined) firstSlot = slot;
        else if (slot > firstSlot) return;
      }
    } catch (error) {
      last = error instanceof Error ? error.message : String(error);
    }
    await sleep(2_000);
  }
  throw new Error(`L1 did not become ready: ${last}`);
};

/** The Shelley genesis fields that bound how long the pool can forge. */
export type KesGenesis = {
  readonly systemStart: string;
  readonly slotLength: number;
  readonly slotsPerKESPeriod: number;
  readonly maxKESEvolutions: number;
};

/** The first slot at which the pool's one KES key can no longer forge. */
export type KesHorizon = KesGenesis & { readonly endSlot: number };

/** A run must forge for at least this long without an opcert reissue. */
export const KES_HORIZON_MINIMUM_DAYS = 3_650;
/** Below this much forging time left, the run reports its KES horizon. */
export const KES_WARNING_SECONDS = 30 * 86_400;
/**
 * A tip this many slots behind the wall clock means the L1 stopped: with one
 * pool at active-slot coefficient 0.05, ten blockless minutes have odds of
 * about e^-30.
 */
export const L1_TIP_STALL_SLOTS = 600;

const readCborUint = (bytes: Buffer, at: number): [number, number] => {
  const head = bytes[at]!;
  if (head >> 5 !== 0)
    throw new Error(`expected a CBOR unsigned integer at byte ${at}`);
  const info = head & 0x1f;
  if (info < 24) return [info, at + 1];
  const width = { 24: 1, 25: 2, 26: 4, 27: 8 }[info];
  if (width === undefined)
    throw new Error(`unsupported CBOR integer width at byte ${at}`);
  const value =
    width === 8
      ? Number(bytes.readBigUInt64BE(at + 1))
      : bytes.readUIntBE(at + 1, width);
  return [value, at + 1 + width];
};

/**
 * The KES period an operational certificate starts at. Its CBOR is
 * `[[hot vkey (bytes 32), counter, kes period, sigma], cold vkey]`.
 */
export const opcertKesPeriod = (cborHex: string): number => {
  const bytes = Buffer.from(cborHex, "hex");
  if (
    bytes[0] !== 0x82 ||
    bytes[1] !== 0x84 ||
    bytes[2] !== 0x58 ||
    bytes[3] !== 0x20
  )
    throw new Error("not an operational certificate");
  const [, afterCounter] = readCborUint(bytes, 4 + 32);
  return readCborUint(bytes, afterCounter)[0];
};

export const kesHorizon = (
  genesis: KesGenesis,
  opcertPeriod: number,
): KesHorizon => ({
  systemStart: genesis.systemStart,
  slotLength: genesis.slotLength,
  slotsPerKESPeriod: genesis.slotsPerKESPeriod,
  maxKESEvolutions: genesis.maxKESEvolutions,
  endSlot:
    (opcertPeriod + genesis.maxKESEvolutions) * genesis.slotsPerKESPeriod,
});

/** The run's horizon, from its generated genesis and the pool's opcert. */
export const readKesHorizon = (layout: Layout): KesHorizon => {
  const genesis = JSON.parse(
    readFileSync(layout.shelleyGenesis, "utf8"),
  ) as KesGenesis;
  const opcert = JSON.parse(
    readFileSync(
      join(layout.runDir, "genesis/pools-keys/pool1/opcert.cert"),
      "utf8",
    ),
  ) as { cborHex: string };
  return kesHorizon(genesis, opcertKesPeriod(opcert.cborHex));
};

export type L1Liveness = {
  readonly tipSlot: number;
  readonly wallSlot: number;
  readonly kesEndSlot: number;
  readonly kesSlotsRemaining: number;
  /** One line per condition that ends or will end block production. */
  readonly reasons: readonly string[];
};

/**
 * Whether the L1 is still producing blocks, and for how long the pool's KES
 * key can keep doing so. Both are judged against the wall-clock slot, the one
 * the pool forges at.
 */
export const l1Liveness = (
  horizon: KesHorizon,
  tipSlot: number,
  nowMs: number,
): L1Liveness => {
  const wallSlot = Math.floor(
    (nowMs - Date.parse(horizon.systemStart)) / (horizon.slotLength * 1_000),
  );
  const kesSlotsRemaining = horizon.endSlot - wallSlot;
  const lag = wallSlot - tipSlot;
  return {
    tipSlot,
    wallSlot,
    kesEndSlot: horizon.endSlot,
    kesSlotsRemaining,
    reasons: [
      ...(lag > L1_TIP_STALL_SLOTS
        ? [
            `l1_tip_stalled: tipSlot=${tipSlot}, wallSlot=${wallSlot}, lagSlots=${lag}`,
          ]
        : []),
      ...(kesSlotsRemaining <= 0
        ? [
            `l1_kes_exhausted: kesEndSlot=${horizon.endSlot}, wallSlot=${wallSlot}`,
          ]
        : kesSlotsRemaining * horizon.slotLength < KES_WARNING_SECONDS
          ? [
              `l1_kes_horizon_near: kesEndSlot=${horizon.endSlot}, slotsRemaining=${kesSlotsRemaining}`,
            ]
          : []),
    ],
  };
};

/** l1Liveness for a running chain, its tip read from Ogmios. */
export const readL1Liveness = async (
  layout: Layout,
  run: RunEnv,
  now: () => number = Date.now,
): Promise<L1Liveness> => {
  const tipSlot = (await ogmiosHealth(run)).lastKnownTip?.slot;
  if (tipSlot === undefined) throw new Error("Ogmios reports no tip yet");
  return l1Liveness(readKesHorizon(layout), tipSlot, now());
};

/** Reports, never refuses: a chain close to its horizon still runs until it. */
const reportL1Liveness = async (layout: Layout, run: RunEnv) => {
  try {
    const liveness = await readL1Liveness(layout, run);
    const horizon = readKesHorizon(layout);
    const days = (liveness.kesSlotsRemaining * horizon.slotLength) / 86_400;
    console.log(
      `l1: tip slot ${liveness.tipSlot}; the pool's KES key forges until slot ${liveness.kesEndSlot} (${days.toFixed(1)} days left)`,
    );
    for (const reason of liveness.reasons) console.log(`l1: WARNING ${reason}`);
  } catch (error) {
    console.log(
      `l1: could not read the KES horizon: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
};

/**
 * cardano-cli from the pinned node image, with the run directory at /run.
 * Callers pass the era group (`latest`) where the subcommand needs one.
 */
export const cardanoCli = (
  layout: Layout,
  run: RunEnv,
  args: readonly string[],
  label: string,
) =>
  execLogged(
    "docker",
    [
      "run",
      "--rm",
      "--user",
      `${process.getuid?.() ?? 1000}:${process.getgid?.() ?? 1000}`,
      "--volume",
      `${layout.runDir}:/run`,
      "--entrypoint",
      "cardano-cli",
      run.cardanoImage,
      ...args,
    ],
    { logDir: layout.stepLogs, label, timeoutMs: 180_000 },
  );

export const psql = async (
  layout: Layout,
  run: RunEnv,
  database: string,
  sql: string,
  label: string,
) =>
  requireSuccess(
    await compose(
      layout,
      run,
      [
        "exec",
        "-T",
        "postgres",
        "psql",
        "-v",
        "ON_ERROR_STOP=1",
        "-At",
        "-U",
        run.postgresUser,
        "-d",
        database,
      ],
      label,
      sql,
    ),
    `psql ${label}`,
  ).stdout.trim();
