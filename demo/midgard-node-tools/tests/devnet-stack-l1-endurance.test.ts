import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import preprod from "../devnet/preprod/configuration.json" with { type: "json" };
import {
  KES_HORIZON_MINIMUM_DAYS,
  KES_WARNING_SECONDS,
  kesHorizon,
  L1_CONTAINER_LOGGING,
  L1_SERVICES,
  L1_TIP_STALL_SLOTS,
  l1Liveness,
  opcertKesPeriod,
  readKesHorizon,
  SUPERVISED_COMPOSE,
} from "../src/devnet-stack/chain.js";
import { makeLayout } from "../src/devnet-stack/layout.js";

const phase4 = join(import.meta.dirname, "../devnet/phase4-process");
const composeFile = join(phase4, "compose.yaml");
const CARDANO_IMAGE =
  /PHASE4_CARDANO_NODE_IMAGE="([^"]+)"/.exec(
    readFileSync(join(phase4, "scripts/common.sh"), "utf8"),
  )?.[1] ?? "";

const docker = (args: readonly string[], env: Record<string, string> = {}) =>
  spawnSync("docker", args, {
    encoding: "utf8",
    env: { ...process.env, ...env },
    timeout: 300_000,
  });
const composeAvailable = docker(["compose", "version"]).status === 0;
const imageAvailable =
  composeAvailable && docker(["image", "inspect", CARDANO_IMAGE]).status === 0;

type ComposeConfig = {
  services: Record<
    string,
    { logging?: unknown; restart?: string; entrypoint?: string[] }
  >;
};

/** The configuration compose applies, as it resolves it. */
const resolvedCompose = (files: readonly string[], runDir = "/tmp/run") => {
  const result = docker(
    [
      "compose",
      ...files.flatMap((file) => ["--file", file]),
      "config",
      "--format",
      "json",
    ],
    {
      MIDGARD_PHASE4_COMPOSE_PROJECT: "endurance_test",
      MIDGARD_PHASE4_RUN_DIR: runDir,
      MIDGARD_PHASE4_POSTGRES_PASSWORD: "test",
      MIDGARD_PHASE4_POSTGRES_DATABASE: "test",
    },
  );
  expect(result.status, result.stderr).toBe(0);
  return JSON.parse(result.stdout) as ComposeConfig;
};

const DAY = 86_400;
const days = (horizonSlots: number, slotLength: number) =>
  (horizonSlots * slotLength) / DAY;

/** `[[hot vkey, counter, kes period, sigma], cold vkey]` with the given ints. */
const opcertHex = (counter: string, period: string) =>
  `8284` +
  `5820${"11".repeat(32)}` +
  counter +
  period +
  `5840${"22".repeat(64)}` +
  `5820${"33".repeat(32)}`;

describe("L1 container logs", () => {
  it("caps every supervised L1 container's log and keeps the supervision", () => {
    const overlay = JSON.parse(SUPERVISED_COMPOSE) as ComposeConfig;
    expect(Object.keys(overlay.services).sort()).toEqual(
      [...L1_SERVICES].sort(),
    );
    for (const service of L1_SERVICES) {
      expect(overlay.services[service]).toMatchObject({
        restart: "unless-stopped",
        logging: {
          driver: "json-file",
          options: { "max-size": "100m", "max-file": "3" },
        },
      });
    }
    expect(overlay.services["cardano-node"]!.entrypoint?.[2]).toBe(
      'umask 0000 && exec /usr/local/bin/entrypoint "$$@"',
    );
  });

  it.skipIf(!composeAvailable)(
    "compose applies the cap to all four services, alone and under the overlay",
    () => {
      const dir = mkdtempSync(join(tmpdir(), "devnet-stack-endurance-"));
      try {
        const overlay = join(dir, "compose.supervised.yaml");
        spawnSync("sh", ["-c", `cat > "${overlay}"`], {
          input: SUPERVISED_COMPOSE,
        });
        for (const files of [[composeFile], [composeFile, overlay]]) {
          const resolved = resolvedCompose(files);
          expect(Object.keys(resolved.services).sort()).toEqual(
            [...L1_SERVICES].sort(),
          );
          for (const service of L1_SERVICES)
            expect(resolved.services[service]!.logging).toEqual(
              L1_CONTAINER_LOGGING,
            );
        }
        const supervised = resolvedCompose([composeFile, overlay]);
        expect(supervised.services["cardano-node"]!.entrypoint?.[2]).toBe(
          'umask 0000 && exec /usr/local/bin/entrypoint "$$@"',
        );
        for (const service of L1_SERVICES)
          expect(supervised.services[service]!.restart).toBe("unless-stopped");
      } finally {
        rmSync(dir, { recursive: true, force: true });
      }
    },
  );
});

describe("pool KES horizon", () => {
  const isolated = { ...preprod.consensus, ...preprod.isolatedChain.consensus };
  const genesis = (consensus: typeof preprod.consensus) => ({
    systemStart: "2026-09-30T08:00:58Z",
    slotLength: consensus.slotLength,
    slotsPerKESPeriod: consensus.slotsPerKESPeriod,
    maxKESEvolutions: consensus.maxKESEvolutions,
  });

  it("outlives the minimum unattended run on the isolated chain's consensus", () => {
    const horizon = kesHorizon(genesis(isolated), 0);
    expect(days(horizon.endSlot, horizon.slotLength)).toBeGreaterThanOrEqual(
      KES_HORIZON_MINIMUM_DAYS,
    );
    expect(isolated.maxKESEvolutions).toBeLessThanOrEqual(64);
  });

  it("is the 93-day cliff on Preprod's own consensus", () => {
    const horizon = kesHorizon(genesis(preprod.consensus), 0);
    expect(horizon.endSlot).toBe(62 * 129_600);
    expect(days(horizon.endSlot, horizon.slotLength)).toBe(93);
    expect(days(horizon.endSlot, horizon.slotLength)).toBeLessThan(
      KES_HORIZON_MINIMUM_DAYS,
    );
  });

  it("reads the opcert's start period, after its counter", () => {
    expect(opcertKesPeriod(opcertHex("00", "00"))).toBe(0);
    expect(opcertKesPeriod(opcertHex("03", "1819"))).toBe(25);
    expect(opcertKesPeriod(opcertHex("190100", "1a00010000"))).toBe(65_536);
    expect(kesHorizon(genesis(isolated), 2).endSlot).toBe((2 + 62) * 5_184_000);
    expect(() => opcertKesPeriod("8200")).toThrow(/operational certificate/);
  });
});

describe("l1Liveness", () => {
  const horizon = kesHorizon(
    {
      systemStart: "2026-01-01T00:00:00Z",
      slotLength: 1,
      slotsPerKESPeriod: DAY,
      maxKESEvolutions: 62,
    },
    0,
  );
  const at = (slot: number) =>
    Date.parse("2026-01-01T00:00:00Z") + slot * 1_000;

  it("reports nothing for a chain whose tip keeps up and whose key has time", () => {
    const liveness = l1Liveness(
      horizon,
      10_000,
      at(10_000 + L1_TIP_STALL_SLOTS),
    );
    expect(liveness.reasons).toEqual([]);
    expect(liveness.kesSlotsRemaining).toBe(
      horizon.endSlot - 10_000 - L1_TIP_STALL_SLOTS,
    );
  });

  it("reports a tip that stopped advancing", () => {
    const liveness = l1Liveness(
      horizon,
      10_000,
      at(10_001 + L1_TIP_STALL_SLOTS),
    );
    expect(liveness.reasons).toEqual([
      `l1_tip_stalled: tipSlot=10000, wallSlot=${10_001 + L1_TIP_STALL_SLOTS}, lagSlots=${L1_TIP_STALL_SLOTS + 1}`,
    ]);
  });

  it("reports a key near its end, then one past it", () => {
    const near = horizon.endSlot - KES_WARNING_SECONDS + 1;
    expect(l1Liveness(horizon, near, at(near)).reasons).toEqual([
      `l1_kes_horizon_near: kesEndSlot=${horizon.endSlot}, slotsRemaining=${KES_WARNING_SECONDS - 1}`,
    ]);
    expect(l1Liveness(horizon, near - 1, at(near - 1)).reasons).toEqual([]);
    expect(
      l1Liveness(horizon, horizon.endSlot, at(horizon.endSlot)).reasons,
    ).toEqual([
      `l1_kes_exhausted: kesEndSlot=${horizon.endSlot}, wallSlot=${horizon.endSlot}`,
    ]);
  });
});

/**
 * The real generator, up to the point where it would start services: the
 * genesis it writes must carry the isolated chain's KES consensus, the node
 * config must trace at Warning, and compose must cap the run's logs.
 */
describe.skipIf(!imageAvailable)("a generated chain", () => {
  it("forges past the minimum run, traces at Warning and caps its logs", () => {
    const parent = mkdtempSync(join(tmpdir(), "devnet-stack-generate-"));
    const runDir = join(parent, "endurance");
    try {
      const generated = spawnSync("sh", [join(phase4, "scripts/generate.sh")], {
        encoding: "utf8",
        env: {
          ...process.env,
          MIDGARD_PHASE4_RUN_DIR: runDir,
          MIDGARD_PHASE4_RUN_ID: "endurance-test",
          MIDGARD_PHASE4_OGMIOS_PORT: "1",
          MIDGARD_PHASE4_KUPO_PORT: "2",
          MIDGARD_PHASE4_POSTGRES_PORT: "3",
        },
        timeout: 300_000,
      });
      expect(generated.status, generated.stderr).toBe(0);
      const horizon = readKesHorizon(makeLayout(runDir));
      expect(horizon.slotsPerKESPeriod).toBe(
        preprod.isolatedChain.consensus.slotsPerKESPeriod,
      );
      expect(horizon.maxKESEvolutions).toBe(preprod.consensus.maxKESEvolutions);
      expect(days(horizon.endSlot, horizon.slotLength)).toBeGreaterThanOrEqual(
        KES_HORIZON_MINIMUM_DAYS,
      );
      const config = JSON.parse(
        readFileSync(join(runDir, "config/config.json"), "utf8"),
      ) as { TraceOptions: Record<string, { severity: string }> };
      expect(config.TraceOptions[""]!.severity).toBe("Warning");
      for (const service of L1_SERVICES)
        expect(
          resolvedCompose([composeFile], runDir).services[service]!.logging,
        ).toEqual(L1_CONTAINER_LOGGING);
    } finally {
      rmSync(parent, { recursive: true, force: true });
    }
  });
});
