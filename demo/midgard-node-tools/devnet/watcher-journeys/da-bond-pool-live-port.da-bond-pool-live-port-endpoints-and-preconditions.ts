import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { TEST_AVAILABILITY_PARAMETERS } from "midgard-node/tests/helpers/availability-challenge";
import { describe, expect, it } from "vitest";

import {
  daBondPoolJourneyDirectory,
  findJourneyDaemons,
  journeyEndpointsFromRunEnv,
  kupoMatchesEverything,
} from "./da-bond-pool-live-port.js";

export const ADA = 1_000_000n;

export const parameters: SDK.DaAvailabilityParameters = {
  ...TEST_AVAILABILITY_PARAMETERS,
  da_slash_penalty_lovelace: 100n * ADA,
  max_timeout_fee_lovelace: 3n * ADA,
};

export const challenger = "addr_test1_challenger";

export const tx = (index: number) =>
  `${index.toString(16).padStart(2, "0")}`.repeat(32);

export const coin = (
  index: number,
  lovelace: bigint,
  extra: Partial<UTxO> = {},
): UTxO => ({
  txHash: tx(index),
  outputIndex: 0,
  address: challenger,
  assets: { lovelace },
  datum: null,
  datumHash: null,
  scriptRef: null,
  ...extra,
});

describe("DA bond pool live port: endpoints and preconditions", () => {
  it("builds Kupo and Ogmios URLs from run.env ports and rejects bad ports", () => {
    expect(
      journeyEndpointsFromRunEnv({
        MIDGARD_PHASE4_KUPO_PORT: "31442",
        MIDGARD_PHASE4_OGMIOS_PORT: "31337",
      }),
    ).toEqual({
      kupoUrl: "http://127.0.0.1:31442",
      ogmiosUrl: "http://127.0.0.1:31337",
    });
    expect(() =>
      journeyEndpointsFromRunEnv({ MIDGARD_PHASE4_OGMIOS_PORT: "31337" }),
    ).toThrow("MIDGARD_PHASE4_KUPO_PORT must be a TCP port");
    expect(() =>
      journeyEndpointsFromRunEnv({
        MIDGARD_PHASE4_KUPO_PORT: "70000",
        MIDGARD_PHASE4_OGMIOS_PORT: "31337",
      }),
    ).toThrow("MIDGARD_PHASE4_KUPO_PORT must be a TCP port");
  });

  it("accepts Kupo only when it matches every address", () => {
    expect(kupoMatchesEverything(["*"])).toBe(true);
    expect(kupoMatchesEverything(["addr_test1*", "*"])).toBe(true);
    expect(kupoMatchesEverything(["addr_test1vz*"])).toBe(false);
    expect(kupoMatchesEverything({ patterns: ["*"] })).toBe(false);
  });

  it("finds a watcher or committee daemon bound to the run directory", () => {
    const run = "/runs/journey-a";
    const processes = [
      {
        pid: 10,
        argv: [
          "/usr/bin/node",
          "/repo/demo/midgard-watcher/dist/cli.js",
          "start",
          "--config",
          `${run}/work/session/watcher.json`,
        ],
      },
      {
        pid: 11,
        argv: [
          "node",
          "/repo/demo/da-committee-node/dist/cli.js",
          `--config=${run}/committee.json`,
        ],
      },
      // Another run's watcher is not this journey's concern.
      {
        pid: 12,
        argv: [
          "node",
          "/repo/demo/midgard-watcher/dist/cli.js",
          "start",
          "--config",
          "/runs/journey-b/watcher.json",
        ],
      },
      // The journey's own vitest process names the run but is no daemon.
      { pid: 13, argv: ["node", "vitest", "run", `${run}/x`] },
    ];
    const found = findJourneyDaemons(processes, `${run}/`, new Set(), 99);
    expect(found).toHaveLength(2);
    expect(found[0]).toMatch(/^pid 10: /u);
    expect(found[1]).toMatch(/^pid 11: /u);
    expect(findJourneyDaemons(processes, run, new Set(), 10)).toHaveLength(1);
  });

  describe("admits only the adapter's own committee node, by pid (P27(5))", () => {
    const run = "/runs/journey-a";
    const committeeArgv = [
      "/usr/bin/node",
      "/repo/demo/da-committee-node/dist/index.js",
    ];
    const committeeEnviron = [
      "PATH=/usr/bin",
      `DA_AVAILABILITY_JOURNAL_PATH=${run}/work/journeys/da-bond-pool/committee/availability-journal.sqlite`,
      `L1_SUBMITTER_KEY_SOURCE=file:${run}/secrets/da-bond-pool-committee-l1-submitter.seed`,
    ];
    const own = { pid: 500, argv: committeeArgv, environ: committeeEnviron };
    const find = (
      processes: Parameters<typeof findJourneyDaemons>[0],
      admitted: readonly number[],
    ) => findJourneyDaemons(processes, run, new Set(admitted), 1);

    it("passes with the admitted pid and nothing else", () => {
      expect(find([own], [500])).toEqual([]);
      expect(find([], [])).toEqual([]);
    });

    it("refuses the same command line under another pid", () => {
      const found = find([own, { ...own, pid: 501 }], [500]);
      expect(found).toEqual([expect.stringMatching(/^pid 501: /u)]);
    });

    it("refuses a watcher while the committee pid is admitted", () => {
      const watcher = {
        pid: 600,
        argv: [
          "node",
          "/repo/demo/midgard-watcher/dist/cli.js",
          "start",
          "--config",
          `${run}/work/session/watcher.json`,
        ],
        environ: ["PATH=/usr/bin"],
      };
      expect(find([own, watcher], [500])).toEqual([
        expect.stringMatching(/^pid 600: /u),
      ]);
    });

    it("refuses a committee node that names the run only in its environment", () => {
      expect(find([{ ...own, pid: 700 }], [])).toEqual([
        expect.stringMatching(/^pid 700: /u),
      ]);
      // Another run's node, by environment, is not this journey's concern.
      expect(
        find(
          [
            {
              pid: 701,
              argv: committeeArgv,
              environ: [
                "DA_AVAILABILITY_JOURNAL_PATH=/runs/journey-b/j.sqlite",
              ],
            },
          ],
          [],
        ),
      ).toEqual([]);
    });

    it("fails closed on a daemon whose environment is unreadable", () => {
      expect(
        find([{ pid: 800, argv: committeeArgv, environ: "unreadable" }], []),
      ).toEqual([expect.stringMatching(/^pid 800: .*environment unreadable/u)]);
      // A non-daemon with an unreadable environment is not a concern.
      expect(
        find([{ pid: 801, argv: ["/sbin/init"], environ: "unreadable" }], []),
      ).toEqual([]);
    });

    it("refuses an admitted pid that is not a committee node or is gone", () => {
      expect(
        find([{ pid: 900, argv: ["node", "vitest"], environ: [] }], [900]),
      ).toEqual([expect.stringMatching(/pid 900: .*not a da-committee-node/u)]);
      expect(find([], [500])).toEqual([
        expect.stringMatching(/pid 500 \(admitted, but not running\)/u),
      ]);
    });
  });

  it("keeps the journey's files under the run's work directory", () => {
    expect(daBondPoolJourneyDirectory("/runs/a")).toBe(
      "/runs/a/work/journeys/da-bond-pool",
    );
  });
});
