import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  OgmiosJsonRpcError,
  paymentCredentialOf,
  TxSubmitError,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { TEST_AVAILABILITY_PARAMETERS } from "midgard-node/tests/helpers/availability-challenge";
import {
  PublishedTransactionExpiredError,
  PublishedTransactionSubmissionError,
} from "midgard-watcher/tests/support/published-block-actor";
import { describe, expect, it } from "vitest";

import {
  absentBlockStatus,
  assertDistinctChallengerKey,
  attestRefusalResult,
  attestWithinLedgerValidity,
  availabilityAttemptRecovery,
  availabilityEndingError,
  AvailabilityIntentLapsedError,
  availabilitySubmissionToAwait,
  awaitAvailabilityInclusion,
  awaitQuietJournal,
  awaitTimeBudgetMs,
  commitWithinLedgerValidity,
  DA_BOND_POOL_AWAIT_TIME_SLACK_MS,
  DA_BOND_POOL_COLLATERAL_MARGIN_LOVELACE,
  DaBondJourneySigningMaterialError,
  daBondPoolApplyRefusal,
  daBondPoolChallengerFundingShortfall,
  daBondPoolJourneyDirectory,
  daBondPoolJourneyParamsOf,
  DaBondPoolJourneyResumeMismatchError,
  errorChainTexts,
  findJourneyDaemons,
  isSpentInputsRebroadcastRefusal,
  isTransientCanonicalError,
  journeyEndpointsFromRunEnv,
  kupoMatchesEverything,
  landAvailabilitySubmission,
  ledgerValidityRefusal,
  MAX_LAPSED_REPLANS,
  nextJourneyBlockInterval,
  planDaBondOwnerQuorum,
  planDaBondPoolChallengerFunding,
  prepareAvailabilityAttempt,
  requireJourneySeed,
  requireResumableQueue,
  selectDaBondPoolChallengerCoins,
  settleExpiredCommitReads,
  summarizeDaBondPoolTimeout,
  unsettledReconciliationError,
  validityIntervalRefusal,
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
    ).toEqual({ startTime: 200_000n, endTime: 201_999n });
  });

  it("ends a block on the last millisecond of a slot even when its predecessor ends within a minute", () => {
    // A slot-aligned predecessor end (x999) less than 60 s ahead of now.
    const { startTime, endTime } = nextJourneyBlockInterval({
      predecessorEndTime: 100_999n,
      nowMs: 30_000,
    });
    expect(endTime).toBeGreaterThan(startTime);
    expect((endTime + 1n) % 1000n).toBe(0n);
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
    expect(
      isTransientCanonicalError(
        new Error(
          "Availability transaction inclusion changed during its canonical read",
        ),
      ),
    ).toBe(true);
    // The foreign-spend read's twin: a block landed during that read.
    const foreignSpendRace = new Error(
      "Availability input spend changed during its canonical read",
    );
    expect(isTransientCanonicalError(foreignSpendRace)).toBe(true);
    expect(unsettledReconciliationError(foreignSpendRace)).toBe(
      "the canonical view is catching up",
    );
    // Not a timing race: the spend sits above the boundary it was read at.
    expect(
      isTransientCanonicalError(
        new Error("Availability input spend lies above the canonical boundary"),
      ),
    ).toBe(false);
    expect(isTransientCanonicalError(new Error("ScriptFailure"))).toBe(false);
  });
});

describe("DA bond pool live port: availability inclusion wait", () => {
  const txId = "ab".repeat(32);
  // The Ogmios refusal the live run hit when reconciliation rebroadcast an
  // Open that was already in the mempool.
  const spentInputs = Object.assign(
    new Error(
      'Ogmios JSON-RPC error 3997: The transaction couldn\'t be added to the mempool. A justification is given as \'data.error\'.: {"error":"All inputs are spent. Transaction has probably already been included"}',
    ),
    {
      code: 3997,
      data: {
        error:
          "All inputs are spent. Transaction has probably already been included",
      },
    },
  );
  const result = (
    status: SDK.DaAvailabilityOperationResult["status"],
  ): SDK.DaAvailabilityOperationResult =>
    ({ txHash: txId, status }) as SDK.DaAvailabilityOperationResult;
  const scripted = (
    steps: readonly (SDK.DaAvailabilityOperationResult["status"] | Error)[],
  ) => {
    let index = 0;
    return async () => {
      const step = steps[Math.min(index, steps.length - 1)]!;
      index += 1;
      if (step instanceof Error) throw step;
      return [result(step)];
    };
  };
  const clock = () => {
    let time = 0;
    return {
      now: () => time,
      wait: async (ms: number) => {
        time += ms;
      },
    };
  };

  it("classifies only a spent-input refusal as a rebroadcast refusal", () => {
    expect(isSpentInputsRebroadcastRefusal(spentInputs)).toBe(true);
    expect(
      isSpentInputsRebroadcastRefusal(
        Object.assign(new Error("submitTransaction failed"), {
          data: { error: { BadInputsUTxO: ["a#0"] } },
        }),
      ),
    ).toBe(true);
    expect(isSpentInputsRebroadcastRefusal(new Error("ScriptFailure"))).toBe(
      false,
    );
    expect(
      isSpentInputsRebroadcastRefusal(
        new Error(
          "Provider returned a different availability transaction hash",
        ),
      ),
    ).toBe(false);
  });

  it("keeps reconciling after a spent-input refusal until the transaction is included", async () => {
    const { now, wait } = clock();
    const lines: string[] = [];
    await expect(
      awaitAvailabilityInclusion({
        txId,
        reconcile: scripted([spentInputs, spentInputs, "included"]),
        journalRecord: () => ({ state: "pending" }),
        timeoutMs: 60_000,
        pollMs: 2_000,
        wait,
        now,
        log: (line) => lines.push(line),
      }),
    ).resolves.toBeUndefined();
    expect(lines).toHaveLength(1);
    expect(lines[0]).toContain("rebroadcast refused with spent inputs");
  });

  it("fails at once on any other reconciliation error", async () => {
    const { now, wait } = clock();
    await expect(
      awaitAvailabilityInclusion({
        txId,
        reconcile: scripted([new Error("ScriptFailure"), "included"]),
        journalRecord: () => ({ state: "pending" }),
        timeoutMs: 60_000,
        pollMs: 2_000,
        wait,
        now,
        log: () => undefined,
      }),
    ).rejects.toThrow("ScriptFailure");
  });

  it("fails when the refused transaction's inputs turn out spent by another", async () => {
    const { now, wait } = clock();
    await expect(
      awaitAvailabilityInclusion({
        txId,
        reconcile: scripted([spentInputs, "expired"]),
        journalRecord: () => ({ state: "pending" }),
        timeoutMs: 60_000,
        pollMs: 2_000,
        wait,
        now,
        log: () => undefined,
      }),
    ).rejects.toThrow(`Availability transaction ${txId} ended expired`);
  });

  it("names the last refusal when the wait times out", async () => {
    const { now, wait } = clock();
    await expect(
      awaitAvailabilityInclusion({
        txId,
        reconcile: scripted([spentInputs]),
        journalRecord: () => ({ state: "pending" }),
        timeoutMs: 10_000,
        pollMs: 2_000,
        wait,
        now,
        log: () => undefined,
      }),
    ).rejects.toThrow(
      /was not included in time \(pending\); \d+ unsettled reconciliation\(s\), last: .*All inputs are spent/u,
    );
  });
});

describe("DA bond pool live port: header commit validity", () => {
  // What `signed.submit()` rejects with: Lucid's TxSubmitError around the
  // provider's error, inside the FiberFailure of Effect.runPromise. The data
  // is the refusal the devnet's Ogmios logged for the failed B3 commit.
  const submitFailure = (code: number, message: string, data: unknown) =>
    Effect.runPromise(
      Effect.fail(
        new TxSubmitError({
          cause: new OgmiosJsonRpcError({
            code,
            message,
            data,
            method: "submitTransaction",
            id: null,
          }),
        }),
      ),
    ).catch((error: unknown) => error);
  const outsideValidity = () =>
    submitFailure(
      3118,
      "The transaction is outside of its validity interval.",
      {
        validityInterval: { invalidBefore: 3210, invalidAfter: 3330 },
        currentSlot: 3193,
      },
    );
  const spentInputs = () =>
    submitFailure(3997, "The transaction couldn't be added to the mempool.", {
      error: "All inputs are spent.",
    });
  const refused = async (cause: Promise<unknown>) =>
    new PublishedTransactionSubmissionError("e1bb026a", await cause);

  it("finds the refusal through the FiberFailure and the cause chain", async () => {
    const error = await refused(outsideValidity());
    expect(error.message).toBe("Header submission e1bb026a is unresolved");
    expect(errorChainTexts(error).join(" | ")).toContain('"currentSlot":3193');
    expect(validityIntervalRefusal(error)).toContain('"invalidBefore":3210');
    expect(validityIntervalRefusal(await refused(spentInputs()))).toBe(
      undefined,
    );
    expect(validityIntervalRefusal(new Error("ScriptFailure"))).toBe(undefined);
  });

  const expired = (hash = "e1bb026a") =>
    new PublishedTransactionExpiredError("header commit B3", hash, 1_000);
  const run = (
    outcomes: readonly (() => Promise<unknown>)[],
    maxAttempts = 3,
    settle: (
      error: PublishedTransactionExpiredError,
    ) => Promise<unknown> = async () => undefined,
  ) => {
    const calls: string[] = [];
    const lines: string[] = [];
    const result = commitWithinLedgerValidity({
      label: "commit B3",
      awaitFreshTip: async () => {
        calls.push("fresh");
      },
      settleExpired: async (error) => {
        calls.push(`settle ${error.txHash}`);
        return settle(error);
      },
      refreshWallet: async () => {
        calls.push("refresh");
      },
      submit: async (attempt) => {
        calls.push(`submit ${attempt}`);
        const outcome = await outcomes[attempt - 1]!();
        if (outcome instanceof Error) throw outcome;
        return outcome;
      },
      maxAttempts,
      log: (line) => lines.push(line),
    });
    return { result, calls, lines };
  };

  it("rebuilds on a fresh tip after a validity-interval refusal", async () => {
    const { result, calls, lines } = run([
      () => refused(outsideValidity()),
      async () => "landed",
    ]);
    await expect(result).resolves.toBe("landed");
    expect(calls).toEqual(["fresh", "submit 1", "fresh", "submit 2"]);
    expect(lines).toHaveLength(1);
    expect(lines[0]).toContain(
      "commit B3: submission e1bb026a refused outside its validity interval",
    );
  });

  it("fails at once, naming the reason, on any other submission refusal", async () => {
    const { result, calls, lines } = run([() => refused(spentInputs())]);
    await expect(result).rejects.toBeInstanceOf(
      PublishedTransactionSubmissionError,
    );
    expect(calls).toEqual(["fresh", "submit 1"]);
    expect(lines[0]).toContain("All inputs are spent");
    expect(calls).not.toContain(expect.stringMatching(/^settle/u));
  });

  it("does not retry an error raised outside the submission", async () => {
    const { result, calls } = run([
      async () =>
        new Error(
          'Header awaited past its bound: {"validityInterval":{"invalidBefore":1}}',
        ),
    ]);
    await expect(result).rejects.toThrow("Header awaited past its bound");
    expect(calls).toEqual(["fresh", "submit 1"]);
  });

  it("rebuilds a commit that expired unminted", async () => {
    const { result, calls, lines } = run([
      async () => expired(),
      async () => "landed",
    ]);
    await expect(result).resolves.toBe("landed");
    expect(calls).toEqual([
      "fresh",
      "submit 1",
      "settle e1bb026a",
      "fresh",
      "submit 2",
    ]);
    expect(lines[0]).toContain("expired unminted past its validity bound");
  });

  it("adopts an expired commit that landed after all", async () => {
    const { result, calls, lines } = run(
      [async () => expired(), async () => "rebuilt"],
      3,
      async () => "adopted",
    );
    await expect(result).resolves.toBe("adopted");
    // The actor's wallet pin predates the landed commit: drop it once.
    expect(calls).toEqual(["fresh", "submit 1", "settle e1bb026a", "refresh"]);
    expect(lines[0]).toContain("adopting it");
  });

  it("fails when an expired commit cannot be settled", async () => {
    const failure = expired();
    const { result, calls } = run(
      [async () => failure, async () => "rebuilt"],
      3,
      async (error) => {
        throw error;
      },
    );
    await expect(result).rejects.toBe(failure);
    expect(calls).toEqual(["fresh", "submit 1", "settle e1bb026a"]);
  });

  it("fails when the last attempt expires unminted", async () => {
    const { result, calls } = run(
      [async () => expired(), async () => expired(), async () => expired()],
      3,
    );
    await expect(result).rejects.toBeInstanceOf(
      PublishedTransactionExpiredError,
    );
    expect(calls.filter((call) => call.startsWith("submit"))).toHaveLength(3);
    expect(calls.filter((call) => call.startsWith("settle"))).toHaveLength(3);
  });

  it("stops after the last attempt", async () => {
    const { result, calls, lines } = run(
      [() => refused(outsideValidity()), () => refused(outsideValidity())],
      2,
    );
    await expect(result).rejects.toBeInstanceOf(
      PublishedTransactionSubmissionError,
    );
    expect(calls).toEqual(["fresh", "submit 1", "fresh", "submit 2"]);
    expect(lines).toHaveLength(2);
    expect(lines[1]).toContain('"currentSlot":3193');
  });

  describe("settles an expired commit from its reads", () => {
    const commit = "c0".repeat(32);
    const apply = "a0".repeat(32);
    const other = "0f".repeat(32);
    const settle = (
      reads: Partial<Parameters<typeof settleExpiredCommitReads>[0]>,
    ) =>
      settleExpiredCommitReads({
        txId: commit,
        stable: true,
        anchorSpentBy: null,
        headerHolders: [],
        read: 1,
        maxReads: 3,
        ...reads,
      });

    it("adopts a commit whose own transaction spent the anchor", () => {
      expect(settle({ anchorSpentBy: commit, headerHolders: [commit] })).toBe(
        "adopt",
      );
      // A DA Apply spent the header output and recreated it.
      expect(settle({ anchorSpentBy: commit, headerHolders: [apply] })).toBe(
        "adopt",
      );
      expect(settle({ anchorSpentBy: commit, headerHolders: [] })).toBe(
        "adopt",
      );
    });

    it("rebuilds only when the anchor is unspent and no output holds the header", () => {
      expect(settle({})).toBe("absent");
    });

    it("refuses every other read as a conflict", () => {
      expect(settle({ anchorSpentBy: other })).toBe("conflict");
      expect(settle({ anchorSpentBy: other, headerHolders: [other] })).toBe(
        "conflict",
      );
      expect(settle({ headerHolders: [commit] })).toBe("conflict");
      expect(
        settle({ anchorSpentBy: commit, headerHolders: [commit, apply] }),
      ).toBe("conflict");
    });

    it("rereads a moving boundary up to the cap", () => {
      expect(settle({ stable: false, anchorSpentBy: commit, read: 2 })).toBe(
        "reread",
      );
      expect(settle({ stable: false, read: 3 })).toBe("unsettled");
    });
  });
});

describe("DA bond pool live port: availability validity and lapses", () => {
  const txId = "cd".repeat(32);
  // What the provider's submit rejects with, inside Lucid's TxSubmitError and
  // the FiberFailure of Effect.runPromise.
  const ogmiosFailure = (code: number, message: string, data: unknown) =>
    Effect.runPromise(
      Effect.fail(
        new TxSubmitError({
          cause: new OgmiosJsonRpcError({
            code,
            message,
            data,
            method: "submitTransaction",
            id: null,
          }),
        }),
      ),
    ).catch((error: unknown) => error as Error);
  const OUTSIDE = "The transaction is outside of its validity interval.";
  const lowerBound = () =>
    ogmiosFailure(3118, OUTSIDE, {
      validityInterval: { invalidBefore: 3210, invalidAfter: 3330 },
      currentSlot: 3193,
    });
  const upperBound = () =>
    ogmiosFailure(3118, OUTSIDE, {
      validityInterval: { invalidBefore: 3210, invalidAfter: 3330 },
      currentSlot: 3331,
    });
  const scriptFailure = () =>
    ogmiosFailure(
      3010,
      "Some scripts of the transactions terminated with error(s).",
      {
        validationError: "ValueNotConserved",
      },
    );
  const LAPSED = "Expired with every normal input canonically unspent";
  const result = (
    status: SDK.DaAvailabilityOperationResult["status"],
  ): SDK.DaAvailabilityOperationResult =>
    ({ txHash: txId, status }) as SDK.DaAvailabilityOperationResult;
  const scripted = (
    steps: readonly (
      | SDK.DaAvailabilityOperationResult["status"]
      | (() => Promise<Error>)
      | Error
    )[],
  ) => {
    let index = 0;
    return async () => {
      const step = steps[Math.min(index, steps.length - 1)]!;
      index += 1;
      if (typeof step === "function") throw await step();
      if (step instanceof Error) throw step;
      return [result(step)];
    };
  };
  const wait = (
    reconcile: () => Promise<readonly SDK.DaAvailabilityOperationResult[]>,
    record: { state?: string; detail?: string | null } | undefined = {
      state: "pending",
    },
    log: (line: string) => void = () => undefined,
  ) => {
    let time = 0;
    return awaitAvailabilityInclusion({
      txId,
      reconcile,
      journalRecord: () => record,
      timeoutMs: 60_000,
      pollMs: 2_000,
      wait: async (ms) => {
        time += ms;
      },
      now: () => time,
      log,
    });
  };

  it("reads the ledger's validity refusal and which bound it failed", async () => {
    expect(ledgerValidityRefusal(await lowerBound())).toMatchObject({
      bound: "lower",
    });
    expect(ledgerValidityRefusal(await upperBound())).toMatchObject({
      bound: "upper",
    });
    expect(ledgerValidityRefusal(await lowerBound())?.text).toContain(
      '"currentSlot":3193',
    );
    // A plain error carrying the same structured data, as a provider may.
    expect(
      ledgerValidityRefusal(
        Object.assign(new Error("RejectTx"), {
          data: { validityInterval: { invalidAfter: 3330 }, currentSlot: 3331 },
        }),
      ),
    ).toMatchObject({ bound: "upper" });
    expect(ledgerValidityRefusal(await scriptFailure())).toBeUndefined();
    expect(
      ledgerValidityRefusal(
        new Error('refused: {"validityInterval":{"invalidBefore":3210}}'),
      ),
    ).toBeUndefined();
    expect(
      ledgerValidityRefusal(
        Object.assign(new Error("other"), {
          code: 3010,
          data: { validityInterval: {}, currentSlot: 1 },
        }),
      ),
    ).toBeUndefined();
  });

  it("names only an unspent-input expiry as lapsed", () => {
    expect(
      availabilityEndingError(txId, "expired", {
        state: "expired",
        detail: LAPSED,
      }),
    ).toBeInstanceOf(AvailabilityIntentLapsedError);
    const spent = availabilityEndingError(txId, "expired", {
      state: "expired",
      detail:
        "Expired with a normal input spent elsewhere and another still unspent",
    });
    expect(spent).not.toBeInstanceOf(AvailabilityIntentLapsedError);
    expect(spent.message).toContain("spent elsewhere");
    expect(
      availabilityEndingError(txId, "conflict", {
        state: "conflict",
        detail: LAPSED,
      }),
    ).not.toBeInstanceOf(AvailabilityIntentLapsedError);
  });

  it("ends the wait with the lapsed error only for a lapsed expiry", async () => {
    await expect(
      wait(scripted(["expired"]), { state: "expired", detail: LAPSED }),
    ).rejects.toBeInstanceOf(AvailabilityIntentLapsedError);
    const spent = await wait(scripted(["expired"]), {
      state: "expired",
      detail: "Expired with a normal input spent elsewhere",
    }).catch((error: unknown) => error);
    expect(spent).toBeInstanceOf(Error);
    expect(spent).not.toBeInstanceOf(AvailabilityIntentLapsedError);
    const conflict = await wait(scripted(["conflict"]), {
      state: "conflict",
    }).catch((error: unknown) => error);
    expect(conflict).not.toBeInstanceOf(AvailabilityIntentLapsedError);
    expect((conflict as Error).message).toContain("ended conflict");
  });

  it("keeps reconciling through a rebroadcast refused on either validity bound", async () => {
    const lines: string[] = [];
    await expect(
      wait(scripted([lowerBound, upperBound, "included"]), undefined, (line) =>
        lines.push(line),
      ),
    ).resolves.toBeUndefined();
    expect(lines).toHaveLength(2);
    expect(lines[0]).toContain("before its validity interval's start");
    expect(lines[1]).toContain("past its validity interval's end");
    expect(lines[1]).toContain('"currentSlot":3331');
  });

  it("keeps reconciling through a transient canonical-view error until the transaction is included", async () => {
    await expect(
      wait(
        scripted([
          new Error(
            "Availability command requires Kupo and Ogmios aligned at the same canonical tip",
          ),
          new Error(
            "Availability transaction inclusion changed during its canonical read",
          ),
          "included",
        ]),
      ),
    ).resolves.toBeUndefined();
    await expect(
      wait(
        scripted([
          new Error(
            "Availability command requires Kupo and Ogmios aligned at the same canonical tip",
          ),
          "expired",
        ]),
      ),
    ).rejects.toThrow(`Availability transaction ${txId} ended expired`);
  });

  it("still fails at once on a script failure", async () => {
    let calls = 0;
    const failure = await wait(async () => {
      calls += 1;
      throw await scriptFailure();
    }).catch((error: unknown) => error);
    expect(calls).toBe(1);
    expect(ledgerValidityRefusal(failure)).toBeUndefined();
  });

  it("awaits a journaled first broadcast the ledger refused, and nothing else", async () => {
    const pending = () => "pending";
    expect(
      availabilitySubmissionToAwait(await lowerBound(), txId, pending),
    ).toBe(txId);
    expect(
      availabilitySubmissionToAwait(await upperBound(), txId, pending),
    ).toBe(txId);
    expect(
      availabilitySubmissionToAwait(
        new Error(
          "Availability command requires Kupo and Ogmios aligned at the same canonical tip",
        ),
        txId,
        pending,
      ),
    ).toBe(txId);
    // Not journaled, not built, or not a validity or canonical error.
    expect(
      availabilitySubmissionToAwait(await lowerBound(), txId, () => undefined),
    ).toBeUndefined();
    expect(
      availabilitySubmissionToAwait(await lowerBound(), undefined, pending),
    ).toBeUndefined();
    expect(
      availabilitySubmissionToAwait(await scriptFailure(), txId, pending),
    ).toBeUndefined();
    expect(
      availabilitySubmissionToAwait(
        new Error("value not conserved"),
        txId,
        pending,
      ),
    ).toBeUndefined();
  });

  it("re-plans a lapsed transaction up to the cap and retries only an unjournaled transient error", async () => {
    const lapsed = new AvailabilityIntentLapsedError(txId);
    const transient = new Error(
      "Availability command requires Kupo and Ogmios aligned at the same canonical tip",
    );
    const state = {
      journaled: true,
      lapses: 0,
      attempt: 1,
      maxTransientAttempts: 5,
    };
    for (let lapses = 0; lapses < MAX_LAPSED_REPLANS; lapses += 1)
      expect(availabilityAttemptRecovery(lapsed, { ...state, lapses })).toBe(
        "replan",
      );
    expect(
      availabilityAttemptRecovery(lapsed, {
        ...state,
        lapses: MAX_LAPSED_REPLANS,
      }),
    ).toBe("throw");
    expect(
      availabilityAttemptRecovery(transient, { ...state, journaled: false }),
    ).toBe("retry");
    // Once journaled, the transaction may be in flight: never re-plan it.
    expect(availabilityAttemptRecovery(transient, state)).toBe("throw");
    expect(
      availabilityAttemptRecovery(transient, {
        ...state,
        journaled: false,
        attempt: 5,
      }),
    ).toBe("throw");
    for (const error of [
      new Error(`Availability transaction ${txId} ended conflict`),
      await scriptFailure(),
      new Error("Availability open of aa plans close, expected open"),
    ])
      expect(
        availabilityAttemptRecovery(error, { ...state, journaled: false }),
      ).toBe("throw");
  });

  it("waits for a fresh ledger tip after settling the journal and before reading the boundary", async () => {
    const calls: string[] = [];
    await expect(
      prepareAvailabilityAttempt({
        quietJournal: async () => {
          calls.push("quiet");
        },
        awaitFreshTip: async () => {
          await Promise.resolve();
          calls.push("fresh");
        },
        readBoundary: async () => {
          calls.push("boundary");
          return "point";
        },
      }),
    ).resolves.toBe("point");
    expect(calls).toEqual(["quiet", "fresh", "boundary"]);
  });

  describe("settles the journal before planning", () => {
    // What the SDK's reconcile rejects with when its rebroadcast is refused:
    // the provider's raw Ogmios error, outside any Effect.
    const rawOgmios = (code: number, message: string, data: unknown) =>
      new OgmiosJsonRpcError({
        code,
        message,
        data,
        method: "submitTransaction",
        id: null,
      });
    const allSpent = () =>
      rawOgmios(
        3997,
        "The transaction couldn't be added to the mempool. A justification is given as 'data.error'.",
        {
          error:
            "All inputs are spent. Transaction has probably already been included",
        },
      );
    const quiet = (
      steps: readonly (
        | readonly SDK.DaAvailabilityOperationResult["status"][]
        | (() => Error)
      )[],
      timeoutMs = 60_000,
    ) => {
      let calls = 0;
      let time = 0;
      const thrown: Error[] = [];
      const done = awaitQuietJournal({
        reconcile: async () => {
          const step = steps[Math.min(calls, steps.length - 1)]!;
          calls += 1;
          if (typeof step === "function") {
            const error = step();
            thrown.push(error);
            throw error;
          }
          return step.map(result);
        },
        timeoutMs,
        pollMs: 2_000,
        wait: async (ms) => {
          time += ms;
        },
        now: () => time,
      });
      return { done, calls: () => calls, thrown };
    };

    it("reconciles through a raw spent-input or validity refusal until the journal settles", async () => {
      const spent = quiet([allSpent, ["included"]]);
      await expect(spent.done).resolves.toBeUndefined();
      expect(spent.calls()).toBe(2);
      const bounds = quiet([
        () =>
          rawOgmios(3118, OUTSIDE, {
            validityInterval: { invalidBefore: 3210, invalidAfter: 3330 },
            currentSlot: 3193,
          }),
        () =>
          rawOgmios(3118, OUTSIDE, {
            validityInterval: { invalidBefore: 3210, invalidAfter: 3330 },
            currentSlot: 3331,
          }),
        ["submitted"],
        ["expired", "confirmed"],
      ]);
      await expect(bounds.done).resolves.toBeUndefined();
      expect(bounds.calls()).toBe(4);
    });

    it("fails at once on a script failure", async () => {
      const failure = await scriptFailure();
      const run = quiet([() => failure, []]);
      await expect(run.done).rejects.toBe(failure);
      expect(run.calls()).toBe(1);
    });

    it("rethrows the last unsettled refusal once the deadline passes", async () => {
      const run = quiet([allSpent], 10_000);
      const error = await run.done.catch((caught: unknown) => caught);
      expect(run.calls()).toBeGreaterThan(1);
      expect(error).toBe(run.thrown.at(-1));
    });

    it("fails on a conflicting intent and on an intent still open at the deadline", async () => {
      await expect(quiet([allSpent, ["conflict"]]).done).rejects.toThrow(
        "Availability journal holds a conflicting intent",
      );
      await expect(quiet([["waiting"]], 10_000).done).rejects.toThrow(
        "Availability journal did not settle",
      );
    });
  });

  describe("lands one availability submission", () => {
    const submission = (
      execute: (
        journal: (txId: string) => void,
      ) => Promise<SDK.DaAvailabilityOperationResult>,
      records: Record<string, { state?: string; detail?: string }> = {},
    ) => {
      const awaited: string[] = [];
      const lines: string[] = [];
      let built: string | undefined;
      const landed = landAvailabilitySubmission({
        label: `timeout ${txId}`,
        // The executor builds, journals the intent as pending, then
        // broadcasts it.
        execute: () =>
          execute((id) => {
            built = id;
            records[id] = { state: "pending", ...records[id] };
          }),
        builtTxId: () => built,
        journalRecord: (id) => records[id],
        awaitIncluded: async (id) => {
          awaited.push(id);
        },
        log: (line) => lines.push(line),
      });
      return { landed, awaited, lines };
    };

    it("awaits a journaled transaction whose first broadcast the ledger refused", async () => {
      const { landed, awaited, lines } = submission(async (journal) => {
        journal(txId);
        throw await lowerBound();
      });
      await expect(landed).resolves.toEqual({ kind: "included", txId });
      expect(awaited).toEqual([txId]);
      expect(lines[0]).toContain(`journaled ${txId}`);
      expect(lines[0]).toContain('"currentSlot":3193');
    });

    it("rethrows a script failure, or a refusal before anything was journaled", async () => {
      const failure = await scriptFailure();
      const script = submission(async (journal) => {
        journal(txId);
        throw failure;
      });
      await expect(script.landed).rejects.toBe(failure);
      expect(script.awaited).toEqual([]);
      const refusal = await lowerBound();
      const early = submission(async () => {
        throw refusal;
      });
      await expect(early.landed).rejects.toBe(refusal);
      expect(early.awaited).toEqual([]);
    });

    it("awaits a submitted transaction and fails on another hash or an ending", async () => {
      const submitted = submission(async (journal) => {
        journal(txId);
        return result("submitted");
      });
      await expect(submitted.landed).resolves.toEqual({
        kind: "included",
        txId,
      });
      expect(submitted.awaited).toEqual([txId]);
      await expect(
        submission(async (journal) => {
          journal("ef".repeat(32));
          return result("submitted");
        }).landed,
      ).rejects.toThrow(`Availability executor returned ${txId}`);
      await expect(
        submission(
          async (journal) => {
            journal(txId);
            return result("expired");
          },
          { [txId]: { state: "expired", detail: LAPSED } },
        ).landed,
      ).rejects.toBeInstanceOf(AvailabilityIntentLapsedError);
    });

    it("returns the reconciled result when nothing was built", async () => {
      const reconciled = result("waiting");
      const { landed, awaited } = submission(async () => reconciled);
      await expect(landed).resolves.toEqual({
        kind: "reconciled",
        result: reconciled,
      });
      expect(awaited).toEqual([]);
    });
  });

  it("re-plans the expiry the SDK journals for an intent whose inputs stayed unspent", async () => {
    const account = generateEmulatorAccount({ lovelace: 100_000_000n });
    const emulator = new Emulator([account]);
    emulator.awaitBlock(5);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const directory = mkdtempSync(join(tmpdir(), "da-bond-pool-lapse-"));
    const journal = openAvailabilityOperationJournal(
      join(directory, "journal.sqlite"),
    );
    try {
      const deploymentIdentity = "aa".repeat(32);
      const actor = paymentCredentialOf(account.address).hash;
      const headerHash = "bb".repeat(28);
      const context: SDK.DaAvailabilityOperationContext = {
        deploymentIdentity,
        actor,
        journal,
        stateQueuePolicyId: "cc".repeat(28),
        minimumConfirmationDepth: 30,
        transactionLimits: {
          maxTxSize: 16384,
          maxTxExMem: 16500000n,
          maxTxExSteps: 10000000000n,
          coinsPerUtxoByte: 4310n,
          feeCeilings: { prepare: 1000000n },
        },
        assertActuationCurrent: () => {},
        observe: async () => ({ status: "unspent", currentSlot: 0 }),
        submit: async (signedCbor) =>
          SDK.inspectDaAvailabilitySignedIntent({
            deploymentIdentity,
            actor,
            headerHash,
            action: "prepare",
            signedCbor,
          }).txHash,
      };
      const { txHash } = await SDK.runDaAvailabilityOperation(context, {
        action: "prepare",
        headerHash,
        build: async () =>
          SDK.buildDaAvailabilityFundingPreparationTx(lucid, {
            fundingInput: (await lucid.wallet().getUtxos())[0]!,
            outputLovelace: 50_000_000n,
            feeLovelace: 1_000_000n,
            validFrom: BigInt(emulator.now() - 60_000),
            validTo: BigInt(emulator.now() + 60_000),
          }),
      });
      // No block took it before its validity ended, and nothing spent its
      // inputs: the SDK's own reconcile journals that ending.
      await expect(
        awaitAvailabilityInclusion({
          txId: txHash,
          reconcile: () =>
            SDK.reconcileDaAvailabilityOperations({
              ...context,
              observe: async (intent) => ({
                status: "unspent",
                currentSlot: intent.validUntilSlot,
              }),
            }),
          journalRecord: (id) => journal.findTransaction(id) ?? undefined,
          timeoutMs: 0,
          pollMs: 0,
          wait: async () => undefined,
          log: () => undefined,
        }),
      ).rejects.toBeInstanceOf(AvailabilityIntentLapsedError);
    } finally {
      journal.close();
      rmSync(directory, { recursive: true, force: true });
    }
  });
});

describe("DA bond pool live port: Apply validity", () => {
  const validityRefusal = () =>
    Effect.runPromise(
      Effect.fail(
        Object.assign(new Error("Ogmios JSON-RPC error 3118"), {
          data: {
            validityInterval: { invalidBefore: 3210 },
            currentSlot: 3193,
          },
        }),
      ),
    ).catch((error: unknown) => error);
  const run = (
    outcomes: readonly (() => Promise<unknown>)[],
    maxAttempts = 3,
  ) => {
    const calls: string[] = [];
    const lines: string[] = [];
    let index = 0;
    const result = attestWithinLedgerValidity({
      label: "attest B2",
      attest: async () => {
        calls.push("attest");
        const outcome = await outcomes[Math.min(index, outcomes.length - 1)]!();
        index += 1;
        if (outcome instanceof Error || typeof outcome !== "object")
          throw outcome;
        return outcome as { kind: "attested" };
      },
      refusal: (error) =>
        error instanceof Error && error.message.includes("pool-under-backed")
          ? { kind: "refused" as const, reason: "pool-under-backed" }
          : undefined,
      awaitFreshTip: async () => {
        calls.push("fresh");
      },
      refreshWallet: async () => {
        calls.push("wallet");
      },
      maxAttempts,
      log: (line) => lines.push(line),
    });
    return { result, calls, lines };
  };

  it("attests again on a fresh tip after a validity refusal", async () => {
    const { result, calls, lines } = run([
      validityRefusal,
      async () => ({ kind: "attested" }),
    ]);
    await expect(result).resolves.toEqual({ kind: "attested" });
    expect(calls).toEqual(["fresh", "attest", "fresh", "wallet", "attest"]);
    expect(lines[0]).toContain('"currentSlot":3193');
  });

  it("returns a pool refusal at once", async () => {
    const { result, calls } = run([
      async () => new Error("Apply refused: pool-under-backed"),
    ]);
    await expect(result).resolves.toEqual({
      kind: "refused",
      reason: "pool-under-backed",
    });
    expect(calls).toEqual(["fresh", "attest"]);
  });

  it("fails at once, naming the chain, on a script failure", async () => {
    const { result, calls, lines } = run([
      async () =>
        new Error("Apply failed", {
          cause: Object.assign(new Error("ValidatorFailed"), {
            code: 3010,
            data: { validationError: "ValueNotConserved" },
          }),
        }),
    ]);
    await expect(result).rejects.toThrow("Apply failed");
    expect(calls).toEqual(["fresh", "attest"]);
    expect(lines[0]).toContain("ValueNotConserved");
  });

  it("stops after the last attempt", async () => {
    const { result, calls } = run([validityRefusal], 3);
    await expect(result).rejects.toBeDefined();
    expect(calls.filter((call) => call === "attest")).toHaveLength(3);
  });
});

describe("DA bond pool live port: resuming after step 5", () => {
  const b2 = "b2".repeat(28);
  it("accepts a queue that holds only the recorded B2 behind its root", () => {
    expect(() => requireResumableQueue([b2], b2)).not.toThrow();
  });

  it("refuses an empty queue, another block, and a block after B2", () => {
    for (const headers of [[], ["b1".repeat(28)], [b2, "b3".repeat(28)]]) {
      expect(() => requireResumableQueue(headers, b2)).toThrow(
        DaBondPoolJourneyResumeMismatchError,
      );
      expect(() => requireResumableQueue(headers, b2)).toThrow(
        `holds [${headers.join(", ")}], not only the recorded B2 ${b2}`,
      );
    }
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
