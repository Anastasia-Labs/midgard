import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { TEST_AVAILABILITY_PARAMETERS } from "midgard-node/tests/helpers/availability-challenge";
import { describe, expect, it } from "vitest";

import {
  absentBlockStatus,
  assertDistinctChallengerKey,
  attestRefusalResult,
  awaitTimeBudgetMs,
  DA_BOND_POOL_AWAIT_TIME_SLACK_MS,
  DA_BOND_POOL_COLLATERAL_MARGIN_LOVELACE,
  DaBondJourneySigningMaterialError,
  daBondPoolApplyRefusal,
  daBondPoolChallengerFundingShortfall,
  daBondPoolJourneyDirectory,
  daBondPoolJourneyParamsOf,
  findJourneyDaemons,
  isTransientCanonicalError,
  journeyEndpointsFromRunEnv,
  kupoMatchesEverything,
  nextJourneyBlockInterval,
  planDaBondOwnerQuorum,
  planDaBondPoolChallengerFunding,
  requireJourneySeed,
  selectDaBondPoolChallengerCoins,
  summarizeDaBondPoolTimeout,
} from "./da-bond-pool-live-port.js";

const ADA = 1_000_000n;
const parameters: SDK.DaAvailabilityParameters = {
  ...TEST_AVAILABILITY_PARAMETERS,
  da_slash_penalty_lovelace: 100n * ADA,
  max_timeout_fee_lovelace: 3n * ADA,
};
const challenger = "addr_test1_challenger";
const tx = (index: number) =>
  `${index.toString(16).padStart(2, "0")}`.repeat(32);
const coin = (
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

describe("DA bond pool live port: parameters and funding", () => {
  it("maps the manifest amounts and timing to the journey parameters", () => {
    expect(
      daBondPoolJourneyParamsOf(parameters, {
        da_bond_withdraw_delay_ms: 2_340_000,
        da_attestation_timeout_ms: "600000",
      }),
    ).toEqual({
      daBond: parameters.da_bond_lovelace,
      penalty: 100n * ADA,
      floor: parameters.da_bond_pool_floor_lovelace,
      minTopUp: parameters.da_bond_min_top_up_lovelace,
      maxTimeoutFee: 3n * ADA,
      challengeRecordLovelace: parameters.challenge_record_lovelace,
      withdrawDelayMs: 2_340_000,
      attestationTimeoutMs: 600_000,
    });
    expect(() =>
      daBondPoolJourneyParamsOf(parameters, {
        da_bond_withdraw_delay_ms: 0,
        da_attestation_timeout_ms: 600_000,
      }),
    ).toThrow("da_bond_withdraw_delay_ms must be a positive integer");
  });

  it("plans exact Open coins and a G9 collateral coin", () => {
    const plan = planDaBondPoolChallengerFunding({
      parameters,
      collateralPercentage: 150,
    });
    expect(plan.openCoinLovelace).toBe(
      parameters.challenger_bond_lovelace +
        parameters.challenge_record_lovelace +
        parameters.max_open_fee_lovelace,
    );
    expect(plan.openCoins).toBe(2);
    // 150% of (100 + 3) ADA, the worst Timeout fee.
    expect(plan.timeoutCollateralLovelace).toBe(154_500_000n);
    expect(plan.collateralLovelace).toBe(
      154_500_000n + DA_BOND_POOL_COLLATERAL_MARGIN_LOVELACE,
    );
    expect(() =>
      planDaBondPoolChallengerFunding({
        parameters,
        collateralPercentage: 150,
        operatingLovelace: 10n * ADA,
      }),
    ).toThrow("operating coin must hold at least 17000000 lovelace");
  });

  it("funds only what the challenger wallet lacks", () => {
    const plan = planDaBondPoolChallengerFunding({
      parameters,
      collateralPercentage: 150,
    });
    expect(
      daBondPoolChallengerFundingShortfall({
        utxos: [],
        address: challenger,
        plan,
      }),
    ).toEqual([
      plan.openCoinLovelace,
      plan.openCoinLovelace,
      plan.collateralLovelace,
      plan.operatingLovelace,
    ]);
    const funded = [
      coin(1, plan.openCoinLovelace),
      coin(2, plan.openCoinLovelace),
      coin(3, plan.collateralLovelace),
      coin(4, plan.operatingLovelace),
    ];
    expect(
      daBondPoolChallengerFundingShortfall({
        utxos: funded,
        address: challenger,
        plan,
      }),
    ).toEqual([]);
    // A 10 ADA coin is below G9 and cannot collateralize a Timeout; a coin
    // at another address or carrying a token is not the challenger's cash.
    expect(
      daBondPoolChallengerFundingShortfall({
        utxos: [
          coin(1, plan.openCoinLovelace),
          coin(2, plan.openCoinLovelace, { address: "addr_test1_other" }),
          coin(3, 10n * ADA),
          coin(4, plan.operatingLovelace, {
            assets: { lovelace: plan.operatingLovelace, ["ab".repeat(28)]: 1n },
          }),
        ],
        address: challenger,
        plan,
      }),
    ).toEqual([
      plan.openCoinLovelace,
      plan.collateralLovelace,
      plan.operatingLovelace,
    ]);
  });

  it("selects coins by role and never picks a reserved coin", () => {
    const plan = planDaBondPoolChallengerFunding({
      parameters,
      collateralPercentage: 150,
    });
    const open1 = coin(1, plan.openCoinLovelace);
    const open2 = coin(2, plan.openCoinLovelace);
    const collateral = coin(3, plan.collateralLovelace);
    const operating = coin(4, 400n * ADA);
    const utxos = [operating, open2, collateral, open1];
    expect(
      selectDaBondPoolChallengerCoins({ utxos, address: challenger, plan }),
    ).toEqual({ collateral, openFunding: open1, operating });
    // An unconfirmed intent reserves its inputs and its collateral; the
    // planned collateral coin may back the next intent too.
    expect(
      selectDaBondPoolChallengerCoins({
        utxos,
        address: challenger,
        plan,
        reserved: new Set([`${open1.txHash}#0`, `${collateral.txHash}#0`]),
      }),
    ).toEqual({ collateral, openFunding: open2, operating });
    // Without the planned coin, a reserved coin is never posted or spent.
    expect(
      selectDaBondPoolChallengerCoins({
        utxos: [open1, operating, coin(5, 300n * ADA)],
        address: challenger,
        plan,
        reserved: new Set([`${open1.txHash}#0`, `${operating.txHash}#0`]),
      }),
    ).toEqual({ collateral: coin(5, 300n * ADA) });
  });

  it("refuses a challenger key another role already uses", () => {
    expect(() =>
      assertDistinctChallengerKey("aa", { operator: "bb", availability: "cc" }),
    ).not.toThrow();
    expect(() =>
      assertDistinctChallengerKey("aa", {
        operator: "bb",
        availability: "aa",
        cosigner: "aa",
      }),
    ).toThrow("also the availability, cosigner key");
  });
});

describe("DA bond pool live port: signing material", () => {
  const operator = { role: "operator", keyHash: "01".repeat(28) };
  const cosigner = { role: "cosigner", keyHash: "02".repeat(28) };
  const stranger = "03".repeat(28);

  it("takes the threshold of held owners, in owner order", () => {
    expect(
      planDaBondOwnerQuorum({
        owners: [cosigner.keyHash, operator.keyHash],
        updateThreshold: 2n,
        held: [operator, cosigner],
        source: "accounts",
      }),
    ).toEqual([cosigner, operator]);
    expect(
      planDaBondOwnerQuorum({
        owners: [stranger, operator.keyHash],
        updateThreshold: 1n,
        held: [operator, cosigner],
        source: "accounts",
      }),
    ).toEqual([operator]);
  });

  it("names every owner key the run lacks", () => {
    const attempt = () =>
      planDaBondOwnerQuorum({
        owners: [operator.keyHash, stranger],
        updateThreshold: 2n,
        held: [operator, cosigner],
        source: "secrets/journey-accounts.json",
      });
    expect(attempt).toThrow(DaBondJourneySigningMaterialError);
    try {
      attempt();
    } catch (error) {
      expect((error as DaBondJourneySigningMaterialError).missing).toEqual([
        `the signing key of DA params owner ${stranger}`,
      ]);
      expect((error as Error).message).toContain(
        "needs 2 of 2 owners; secrets/journey-accounts.json holds 1",
      );
    }
    expect(() =>
      planDaBondOwnerQuorum({
        owners: [operator.keyHash],
        updateThreshold: 2n,
        held: [operator],
        source: "accounts",
      }),
    ).toThrow("update_threshold 2 cannot be met by 1 owner(s)");
  });

  it("names a missing seed phrase", () => {
    expect(
      requireJourneySeed(
        { cosigner: { seedPhrase: " a b " } },
        "cosigner",
        "s",
      ),
    ).toBe("a b");
    expect(() =>
      requireJourneySeed({ operator: {} }, "cosigner", "secrets/x.json"),
    ).toThrow(
      "missing signing material: the cosigner seedPhrase in secrets/x.json",
    );
  });
});

describe("DA bond pool live port: Apply refusals", () => {
  const poolError = (reason: SDK.DaAttestationBuildFailureReason) =>
    new SDK.DaAttestationBuildError({
      message: `Apply refused: ${reason}`,
      cause: undefined,
      reason,
    });

  it("reads the pool refusal out of the Effect failure Effect.runPromise rejects with", async () => {
    const rejected = await Effect.runPromise(
      Effect.fail(poolError("pool-under-backed")),
    ).catch((error: unknown) => error);
    expect(daBondPoolApplyRefusal(rejected)).toEqual({
      reason: "pool-under-backed",
      message: "Apply refused: pool-under-backed",
    });
    expect(attestRefusalResult(rejected)).toEqual({
      kind: "refused",
      reason: "pool-under-backed: Apply refused: pool-under-backed",
    });
  });

  it("finds it through a cause chain or an aggregate", () => {
    expect(
      daBondPoolApplyRefusal(
        new Error("attest failed", { cause: poolError("pool-withdrawing") }),
      )?.reason,
    ).toBe("pool-withdrawing");
    expect(
      daBondPoolApplyRefusal(
        new AggregateError([new Error("x"), poolError("pool-unavailable")]),
      )?.reason,
    ).toBe("pool-unavailable");
  });

  it("does not turn any other failure into a refusal", async () => {
    const other = await Effect.runPromise(
      Effect.fail(poolError("invalid_validity_range")),
    ).catch((error: unknown) => error);
    expect(daBondPoolApplyRefusal(other)).toBeUndefined();
    expect(attestRefusalResult(new Error("pool-under-backed"))).toBeUndefined();
    const cyclic = new Error("loop");
    (cyclic as { cause?: unknown }).cause = cyclic;
    expect(daBondPoolApplyRefusal(cyclic)).toBeUndefined();
  });
});

describe("DA bond pool live port: chain reads", () => {
  it("tells a merged header from a removed one", () => {
    expect(absentBlockStatus("aa", "aa")).toBe("merged");
    expect(absentBlockStatus("aa", "bb")).toBe("removed");
  });

  it("starts a block at its predecessor's end and never ends it first", () => {
    expect(
      nextJourneyBlockInterval({ predecessorEndTime: 1_000n, nowMs: 5_000 }),
    ).toEqual({ startTime: 1_000n, endTime: 64_999n });
    expect(
      nextJourneyBlockInterval({
        predecessorEndTime: 200_000n,
        nowMs: 5_000,
      }),
    ).toEqual({ startTime: 200_000n, endTime: 200_001n });
  });

  it("bounds awaitTime by the wait plus slack", () => {
    expect(awaitTimeBudgetMs(100_000, 40_000)).toBe(
      60_000 + DA_BOND_POOL_AWAIT_TIME_SLACK_MS,
    );
    expect(awaitTimeBudgetMs(1_000, 40_000, 5)).toBe(5);
  });

  it("retries only canonical-alignment errors", () => {
    expect(
      isTransientCanonicalError(
        new Error(
          "Availability command requires Kupo and Ogmios aligned at the same canonical tip",
        ),
      ),
    ).toBe(true);
    expect(
      isTransientCanonicalError(
        new Error(
          "Availability state changed during canonical discovery; rerun the command",
        ),
      ),
    ).toBe(true);
    expect(isTransientCanonicalError(new Error("ScriptFailure"))).toBe(false);
  });
});

describe("DA bond pool live port: Timeout evidence", () => {
  const poolAddress = "addr_test1_pool";
  const poolUnit = `${"cd".repeat(28)}`;
  const pool = {
    outRef: `${tx(9)}#1`,
    address: poolAddress,
    unit: poolUnit,
    lovelace: 700n * ADA,
    datum: "d87980",
  };
  const base = {
    txId: tx(10),
    fee: 100n * ADA,
    inputs: [`${tx(8)}#0`, pool.outRef],
    pool,
    challengerAddress: challenger,
  };

  it("reads the slash from the landed body", () => {
    expect(
      summarizeDaBondPoolTimeout({
        ...base,
        outputs: [
          {
            address: poolAddress,
            assets: { lovelace: 200n * ADA, [poolUnit]: 1n },
            datum: "d87980",
          },
          { address: challenger, assets: { lovelace: 12_427n * ADA } },
          { address: "addr_test1_actor_base", assets: { lovelace: 2n * ADA } },
        ],
        challengerRemainingLovelace: 12_000n * ADA,
      }),
    ).toEqual({
      txId: base.txId,
      fee: 100n * ADA,
      challengerOutputLovelace: 12_427n * ADA,
      challengerOutputCount: 1,
      poolBefore: 700n * ADA,
      poolAfter: 200n * ADA,
      poolDatumAndNftKept: true,
      challengerRemainingLovelace: 12_000n * ADA,
    });
  });

  it("flags a pool output that changed its datum or gained a token", () => {
    const summary = summarizeDaBondPoolTimeout({
      ...base,
      outputs: [
        {
          address: poolAddress,
          assets: {
            lovelace: 200n * ADA,
            [poolUnit]: 1n,
            ["ef".repeat(28)]: 1n,
          },
          datum: "d87980",
        },
        { address: challenger, assets: { lovelace: 1n } },
        { address: challenger, assets: { lovelace: 2n } },
      ],
    });
    expect(summary.poolDatumAndNftKept).toBe(false);
    expect(summary.challengerOutputCount).toBe(2);
    expect(summary.challengerOutputLovelace).toBe(3n);
    expect(summary).not.toHaveProperty("challengerRemainingLovelace");
    expect(
      summarizeDaBondPoolTimeout({
        ...base,
        outputs: [
          {
            address: poolAddress,
            assets: { lovelace: 200n * ADA, [poolUnit]: 1n },
            datum: "d87a80",
          },
        ],
      }).poolDatumAndNftKept,
    ).toBe(false);
  });

  it("refuses a Timeout that does not spend the pool or continues it twice", () => {
    const poolOutput = {
      address: poolAddress,
      assets: { lovelace: 200n * ADA, [poolUnit]: 1n },
      datum: "d87980",
    };
    expect(() =>
      summarizeDaBondPoolTimeout({
        ...base,
        inputs: [`${tx(8)}#0`],
        outputs: [poolOutput],
      }),
    ).toThrow("does not spend the observed pool");
    expect(() =>
      summarizeDaBondPoolTimeout({
        ...base,
        outputs: [poolOutput, poolOutput],
      }),
    ).toThrow("has 2 pool outputs");
  });
});
