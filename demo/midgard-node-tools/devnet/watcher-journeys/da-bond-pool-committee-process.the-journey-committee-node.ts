import { describe, expect, it } from "vitest";

import {
  buildDaBondPoolCommitteeEnv,
  DaBondPoolCommitteeEnvError,
  redactDaBondPoolEnv,
  worktreeDerivedPort,
} from "./da-bond-pool-committee-process.js";

export const T0 = Date.parse("2026-09-28T12:00:00.000Z");

const iso = (ms: number) => new Date(ms).toISOString();

export const shortReason = (backing: bigint, at: number) =>
  `da_bond_pool_backing_short: backing=${backing}, required=100, checkedAt=${iso(at)}`;

export const withdrawingReason = (unlockAt: bigint, at: number) =>
  `da_bond_pool_withdrawing: unlockAt=${unlockAt}, checkedAt=${iso(at)}`;

export type L1Source = { status: string; intervention?: string };

const body = (
  reasons: readonly string[],
  lastStartedAt?: number,
  l1Source?: L1Source,
) =>
  JSON.stringify({
    ready: reasons.length === 0,
    reasons,
    ...(lastStartedAt === undefined
      ? {}
      : { scanner: { lastStartedAt: iso(lastStartedAt) } }),
    ...(l1Source === undefined ? {} : { l1Source }),
  });

export const answer = (
  reasons: readonly string[],
  lastStartedAt?: number,
  l1Source?: L1Source,
) => ({
  httpStatus: reasons.length === 0 ? 200 : 503,
  body: body(reasons, lastStartedAt, l1Source),
});

export const ROLLBACK_BEYOND_K =
  "rollback_beyond_k: the node rolled back 2161 blocks";

export const intervention: L1Source = {
  status: "intervention",
  intervention: ROLLBACK_BEYOND_K,
};

/** A clock that `sleep` advances. */
export const fakeClock = (start = T0) => {
  let t = start;
  return {
    now: () => t,
    sleep: async (ms: number) => {
      t += ms;
    },
    advance: (ms: number) => {
      t += ms;
    },
  };
};

describe("the journey committee node's environment (P27(1))", () => {
  const base = {
    settings: {
      MIDGARD_NETWORK: "Custom",
      CARDANO_NETWORK_MAGIC: "42",
    },
    l1Submitter: { source: "file:/run/secrets/l1.key", keyHash: "aa" },
    availabilitySubmitter: {
      source: "file:/run/secrets/availability.key",
      keyHash: "bb",
    },
    operationalKeyHashes: { operator: "cc", challenger: "dd" },
    libp2pKeySource: "file:/run/secrets/libp2p.key",
    journalPath: "/run/committee/journal.jsonl",
    databaseUrl: "postgres://user:secret@127.0.0.1:5433/committee",
    apiHost: "127.0.0.1",
    apiPort: 23_456,
    pollIntervalMs: 1_000,
    inherited: {
      PATH: "/usr/bin",
      HOME: "/home/journey",
      DA_SIGNER_INDEX: "0",
      SECRET_TOKEN: "leak",
    },
  } as const;

  it("submits to L1 with preflight on, holds no signer, and drops every other inherited variable", () => {
    const { env, recorded } = buildDaBondPoolCommitteeEnv(base);
    expect(env).toMatchObject({
      DA_L1_SUBMISSION_ENABLED: "true",
      DA_L1_PREFLIGHT_ENABLED: "true",
      L1_SUBMITTER_KEY_SOURCE: "file:/run/secrets/l1.key",
      DA_AVAILABILITY_SUBMITTER_KEY_SOURCE:
        "file:/run/secrets/availability.key",
      DA_AVAILABILITY_JOURNAL_PATH: "/run/committee/journal.jsonl",
      DA_COMMITTEE_API_PORT: "23456",
      DA_COMMITTEE_POLL_INTERVAL_MS: "1000",
      MIDGARD_CONFIG_MODE: "disabled",
      MIDGARD_DOTENV_MODE: "disabled",
      PATH: "/usr/bin",
      HOME: "/home/journey",
      MIDGARD_NETWORK: "Custom",
    });
    expect(
      Object.keys(env).filter((name) =>
        /SIGNER_INDEX|SIGNER_KEY|AUTO_FUND|SECRET_TOKEN/u.test(name),
      ),
    ).toEqual([]);
    expect(recorded.DA_COMMITTEE_DATABASE_URL).toBe("<redacted>");
    expect(recorded.PATH).toBeUndefined();
  });

  it.each([
    [
      "a signer index",
      { settings: { DA_SIGNER_INDEX: "0" } },
      /DA_SIGNER_INDEX is set/u,
    ],
    [
      "a signer key source",
      { settings: { DA_SIGNER_KEY_SOURCE_0: "file:/k" } },
      /DA_SIGNER_KEY_SOURCE_0 is set/u,
    ],
    [
      "an auto-fund key",
      { settings: { DA_L1_AUTO_FUND_KEY_SOURCE: "file:/k" } },
      /DA_L1_AUTO_FUND_KEY_SOURCE is set/u,
    ],
    [
      "a port-owned variable in settings",
      { settings: { DA_L1_PREFLIGHT_ENABLED: "false" } },
      /DA_L1_PREFLIGHT_ENABLED is set by the port/u,
    ],
    [
      "equal submitter key hashes",
      {
        availabilitySubmitter: {
          source: "file:/run/secrets/availability.key",
          keyHash: "aa",
        },
      },
      /are the same key/u,
    ],
    [
      "one key file for both submitters",
      {
        availabilitySubmitter: {
          source: "file:/run/secrets/l1.key",
          keyHash: "bb",
        },
      },
      /are the same key/u,
    ],
    [
      "a submitter key that is an operational key",
      { l1Submitter: { source: "file:/run/secrets/l1.key", keyHash: "cc" } },
      /the L1 submitter key is the operator key/u,
    ],
    [
      "a relative journal path",
      { journalPath: "journal.jsonl" },
      /is not absolute/u,
    ],
  ] as const)("refuses %s", (_label, override, message) => {
    expect(() => buildDaBondPoolCommitteeEnv({ ...base, ...override })).toThrow(
      DaBondPoolCommitteeEnvError,
    );
    expect(() => buildDaBondPoolCommitteeEnv({ ...base, ...override })).toThrow(
      message,
    );
  });

  it("redacts inline key sources and named secrets, and keeps file sources", () => {
    expect(
      redactDaBondPoolEnv({
        A: "seed:abandon abandon",
        B: "file:/k",
        DA_COMMITTEE_DATABASE_URL: "postgres://x",
      }),
    ).toEqual({
      A: "<redacted>",
      B: "file:/k",
      DA_COMMITTEE_DATABASE_URL: "<redacted>",
    });
  });

  it("derives a stable port per worktree and purpose", () => {
    const a = worktreeDerivedPort("/w/one", "committee-api");
    expect(a).toBe(worktreeDerivedPort("/w/one", "committee-api"));
    expect(a).toBeGreaterThanOrEqual(20_000);
    expect(a).toBeLessThan(40_000);
    expect(
      new Set([
        a,
        worktreeDerivedPort("/w/two", "committee-api"),
        worktreeDerivedPort("/w/one", "other"),
      ]).size,
    ).toBe(3);
  });
});
