import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  assertDaAvailabilityOpenWithinChallengeWindow,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@al-ft/midgard-sdk";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import type { WatcherDaBondPoolObservation } from "../../src/availability/pool-observation.js";
import type { WatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  createWatcherOperationsObservability,
  watcherDaBondPoolReadFailureReporter,
  watcherDaBondPoolReporter,
} from "../../src/runtime/operations-observability.js";
import {
  daBondPoolUtxo,
  fixture,
  utxo,
} from "../support/availability-challenge-fixture.js";
import {
  actions,
  ACTOR,
  ADA,
  answered,
  DEPLOYMENT,
  io,
  liveChallenge,
  observation,
  OPENING,
  openLanded,
  PARAMETERS,
  runtime,
  timedOut,
  TIMEOUT_COLLATERAL,
  type TimeoutInput,
  withheld,
  withJournal,
  workflowLive,
  workflowRows,
} from "./concurrent-challenges.fixture.js";

describe("concurrent availability challenges on one wallet (spec #685 E3, P9)", () => {
  it("prepares and Opens a second withheld header while the first header's challenge is live", async () => {
    const first = liveChallenge("44");
    const second = withheld("45");
    openLanded(first.challenged.headerHash);
    const collateral = utxo(0, TIMEOUT_COLLATERAL, "a1");
    io.utxos = [collateral, utxo(1, OPENING + 100n * ADA, "a2")];
    const watcher = await runtime();
    try {
      const state = observation([first.challenged, second.attested]);
      await watcher.reconcile(state, true);
      expect(watcher.status()).toMatchObject({ action: "prepare" });
      // The preparation produced the exact challenger coin.
      io.utxos = [
        collateral,
        utxo(0, OPENING, "a3"),
        utxo(1, 100n * ADA, "a3"),
      ];
      await watcher.reconcile(state, true);
      expect(watcher.status()).toMatchObject({ action: "open" });
      expect(watcher.status().openRefused).toBeUndefined();
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([
      [second.attested.headerHash, "prepare"],
      [second.attested.headerHash, "open"],
    ]);
    // Both challenges now hold their own workflow in the one journal.
    expect(workflowLive(first.challenged.headerHash)).toBe(true);
    expect(workflowLive(second.attested.headerHash)).toBe(true);
  });

  it("refuses an unfundable Open with a typed reason and still closes the live challenge in the same reconciliation", async () => {
    const first = liveChallenge("44");
    const second = withheld("45");
    openLanded(first.challenged.headerHash);
    // Enough for a close's collateral, never for a Timeout's.
    io.utxos = [utxo(0, 5n * ADA, "b1"), utxo(1, 5n * ADA, "b2")];
    const watcher = await runtime();
    try {
      await watcher.reconcile(
        observation([answered(first.challenged), second.attested]),
        true,
      );
      expect(watcher.status()).toMatchObject({
        phase: "waiting",
        action: "close",
        openRefused: [
          {
            headerHash: second.attested.headerHash,
            reason: "insufficient-availability-capital",
            requiredLovelace: TIMEOUT_COLLATERAL.toString(),
            availableLovelace: (10n * ADA).toString(),
          },
        ],
      });
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([[first.challenged.headerHash, "close"]]);
  });

  it("refuses an Open the wallet cannot bond and still times out the expired challenge in the same reconciliation", async () => {
    const first = fixture("44", BigInt(Date.now()));
    const second = withheld("45");
    openLanded(first.challenged.headerHash);
    io.utxos = [utxo(0, TIMEOUT_COLLATERAL, "c1"), utxo(1, 10n * ADA, "c2")];
    // Someone topped the shared pool up after the finalized snapshot.
    io.tipPool = { ...utxo(9, 100_005n * ADA, "c9"), address: "pool" };
    const watcher = await runtime();
    try {
      await watcher.reconcile(
        observation([timedOut(first.challenged), second.attested]),
        true,
      );
      expect(watcher.status()).toMatchObject({
        phase: "waiting",
        action: "timeout",
        openRefused: [
          {
            headerHash: second.attested.headerHash,
            reason: "insufficient-availability-capital",
            availableLovelace: (10n * ADA).toString(),
          },
        ],
      });
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([[first.challenged.headerHash, "timeout"]]);
    // The Timeout spends the pool as it stands at the tip, not the finalized
    // snapshot's outref, which the TopUp already spent.
    expect(io.timeouts.map(({ pool }) => pool)).toEqual([io.tipPool]);
  });

  it("defers a Timeout whose pool is not found at the tip and still closes another live challenge in the same reconciliation (P13(2))", async () => {
    const expired = fixture("44", BigInt(Date.now()));
    const complete = liveChallenge("45");
    openLanded(expired.challenged.headerHash);
    openLanded(complete.challenged.headerHash);
    io.utxos = [utxo(0, TIMEOUT_COLLATERAL, "f1"), utxo(1, 10n * ADA, "f2")];
    // No authentic pool at the tip: the Timeout is not built this tick, and
    // never against the finalized snapshot's pool.
    io.tipPool = undefined;
    const watcher = await runtime();
    try {
      // Non-Open steps keep snapshot order, so the Timeout is tried first.
      await watcher.reconcile(
        observation([
          timedOut(expired.challenged),
          answered(complete.challenged),
        ]),
        true,
      );
      expect(watcher.status()).toMatchObject({
        phase: "waiting",
        action: "close",
        timeoutsDeferred: [
          {
            headerHash: expired.challenged.headerHash,
            reason: "tip-pool-unavailable",
            detail: "no DA bond pool at tip",
          },
        ],
      });
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([[complete.challenged.headerHash, "close"]]);
    expect(io.timeouts).toEqual([]);
  });

  it("reports every step the journal refuses while another deployment's challenge is live, staying ready", async () => {
    const header = withheld("48");
    openLanded("49".repeat(28), "other-deployment");
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL, "g1"),
      utxo(1, OPENING, "g2"),
      utxo(2, 100n * ADA, "g2"),
    ];
    const watcher = await runtime();
    try {
      await watcher.reconcile(observation([header.attested]), true);
      expect(watcher.status()).toMatchObject({
        phase: "ready",
        workflowRefused: [
          {
            headerHash: header.attested.headerHash,
            action: "open",
            detail: `Availability wallet capital belongs to an unresolved challenge workflow in another deployment (deployment other-deployment, header ${"49".repeat(28)})`,
          },
        ],
      });
      expect(watcher.status().workflowReleased).toBeUndefined();
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([]);
    // The release check ran on the other deployment's Open and found no
    // terminal step, so the row stays.
    expect(io.workflowRelease).toHaveBeenCalledWith(
      expect.anything(),
      expect.objectContaining({
        state: "confirmed",
        intent: expect.objectContaining({ id: `open-${"49".repeat(28)}` }),
      }),
      "49".repeat(28),
    );
    expect(workflowRows()).toEqual([["other-deployment", "49".repeat(28)]]);
  });

  describe("releases a workflow someone else's terminal step ended (P20)", () => {
    const OTHER = "49".repeat(28);
    const CLOSE = {
      reason: "challenge-closed" as const,
      txHash: "ce".repeat(32),
      spendPoint: "10:ab",
      confirmationDepth: 30,
    };
    const walletForOpen = () => [
      utxo(0, TIMEOUT_COLLATERAL, "g1"),
      utxo(1, OPENING, "g2"),
      utxo(2, 100n * ADA, "g2"),
    ];

    it("releases capital after the other deployment's committee Close exceeds recovery depth", async () => {
      const header = withheld("48");
      openLanded(OTHER, "other-deployment");
      io.utxos = walletForOpen();
      io.workflowRelease.mockImplementation(
        async (_observation, _open, headerHash: string) =>
          headerHash === OTHER
            ? {
                ...CLOSE,
                confirmationDepth:
                  DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth + 1,
              }
            : undefined,
      );
      const watcher = await runtime();
      try {
        await watcher.reconcile(observation([header.attested]), true);
        expect(watcher.status()).toMatchObject({
          action: "open",
          workflowReleased: [
            {
              deployment: "other-deployment",
              headerHash: OTHER,
              reason: "challenge-closed",
              txHash: CLOSE.txHash,
              spendPoint: CLOSE.spendPoint,
            },
          ],
        });
        expect(watcher.status().workflowRefused).toBeUndefined();
      } finally {
        await watcher.close();
      }
      expect(actions()).toEqual([[header.attested.headerHash, "open"]]);
      // Only the released row went; this deployment's new Open holds its own.
      expect(workflowRows()).toEqual([
        [DEPLOYMENT, header.attested.headerHash],
      ]);
    });

    it("keeps the row and reports it when the release check fails, without aborting the reconciliation", async () => {
      const header = withheld("48");
      openLanded(OTHER, "other-deployment");
      io.utxos = walletForOpen();
      io.workflowRelease.mockRejectedValue(new Error("Ogmios unreachable"));
      const watcher = await runtime();
      try {
        await watcher.reconcile(observation([header.attested]), true);
        expect(watcher.status()).toMatchObject({
          phase: "ready",
          workflowReleaseDeferred: [
            {
              deployment: "other-deployment",
              headerHash: OTHER,
              detail: "Ogmios unreachable",
            },
          ],
          workflowRefused: [
            { headerHash: header.attested.headerHash, action: "open" },
          ],
        });
        expect(watcher.status().workflowReleased).toBeUndefined();
      } finally {
        await watcher.close();
      }
      expect(actions()).toEqual([]);
      expect(workflowRows()).toEqual([["other-deployment", OTHER]]);
    });

    it("keeps the row while this actor's own step for that header is unresolved", async () => {
      const header = withheld("48");
      openLanded(OTHER, "other-deployment");
      withJournal((journal) => {
        const lease = journal.acquire(ACTOR, "earlier", Date.now(), 60_000);
        const id = `close-${OTHER}`;
        journal.persist(
          lease,
          {
            id,
            deploymentIdentity: "other-deployment",
            actor: ACTOR,
            headerHash: OTHER,
            action: "close",
            signedCbor: id,
            txHash: id,
            spentOutRefs: [`${id}#0`],
            collateralOutRefs: [],
            expectedOutRefs: [],
            validUntilSlot: 1,
            completesWorkflow: true,
          },
          Date.now(),
        );
        journal.release(lease);
      });
      io.utxos = walletForOpen();
      io.workflowRelease.mockResolvedValue(CLOSE);
      const watcher = await runtime();
      try {
        await watcher.reconcile(observation([header.attested]), true);
        expect(watcher.status()).toMatchObject({
          workflowReleaseDeferred: [
            {
              deployment: "other-deployment",
              headerHash: OTHER,
              detail:
                "Availability workflow release requires no unresolved intent for the header",
            },
          ],
          workflowRefused: [
            { headerHash: header.attested.headerHash, action: "open" },
          ],
        });
      } finally {
        await watcher.close();
      }
      expect(actions()).toEqual([]);
      expect(workflowRows()).toEqual([["other-deployment", OTHER]]);
    });
  });

  it("caps an Open taken in the last minute of the window at the header's Open deadline", async () => {
    const window = BigInt(
      SELECTED_DEPLOYMENT_PROFILE.timing.da_challenge_window_ms,
    );
    // The deadline is 30 s away: inside the default 60 s validity horizon.
    const endTime = BigInt(Date.now()) - window + 30_000n;
    const header = fixture("47", endTime);
    io.attestedCommitment.mockImplementation(async () => header.commitment);
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL, "e1"),
      utxo(1, OPENING, "e2"),
      utxo(2, 100n * ADA, "e2"),
    ];
    const watcher = await runtime();
    try {
      await watcher.reconcile(observation([header.attested]), true);
      expect(watcher.status()).toMatchObject({ action: "open" });
    } finally {
      await watcher.close();
    }
    const deadline = endTime + window;
    expect(io.opens.map(({ validTo }) => validTo)).toEqual([deadline - 1n]);
    // The SDK's window check admits the capped bound and refuses the uncapped
    // one the Open would otherwise carry.
    const accepts = (validTo: bigint) => () =>
      assertDaAvailabilityOpenWithinChallengeWindow({
        validTo,
        nodeEndTime: endTime,
        daChallengeWindowMs: window,
      });
    expect(accepts(deadline)).not.toThrow();
    expect(accepts(deadline + 30_000n)).toThrow("Challenge window closed");
  });
});

/**
 * A signed Timeout as the SDK runner persists it: the pool, record, terminal
 * and queue node as normal inputs, the wallet collateral, the target-node burn
 * and a redeemer, valid from `slot` to `slot + 1000`.
 */
const timeoutTransaction = (input: TimeoutInput, slot: number): string => {
  const outRef = (utxo: UTxO) =>
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(utxo.txHash),
      BigInt(utxo.outputIndex),
    );
  const inputs = CML.TransactionInputList.new();
  [input.pool, input.record, input.terminal, input.queue]
    .filter(
      (utxo, index, all) =>
        all.findIndex(
          (other) =>
            other.txHash === utxo.txHash &&
            other.outputIndex === utxo.outputIndex,
        ) === index,
    )
    .forEach((utxo) => inputs.add(outRef(utxo)));
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    200_000n,
  );
  body.set_validity_interval_start(BigInt(slot));
  body.set_ttl(BigInt(slot + 1_000));
  const collateral = CML.TransactionInputList.new();
  input.collateralInputs.forEach((utxo) => collateral.add(outRef(utxo)));
  body.set_collateral_inputs(collateral);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(io.queuePolicy),
    CML.AssetName.from_hex(
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + TIMED_OUT_HEADER,
    ),
    -1n,
  );
  body.set_mint(mint);
  const redeemers = CML.LegacyRedeemerList.new();
  redeemers.add(
    CML.LegacyRedeemer.new(
      CML.RedeemerTag.Spend,
      0n,
      CML.PlutusData.new_integer(CML.BigInteger.from_str("0")),
      CML.ExUnits.new(1_000_000n, 100_000_000n),
    ),
  );
  const witnesses = CML.TransactionWitnessSet.new();
  witnesses.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  return CML.Transaction.new(body, witnesses, true, undefined).to_cbor_hex();
};
/** The builder the SDK runner signs with the wallet's key. */
const signable = (cbor: string, key: CML.PrivateKey) => ({
  toTransaction: () => CML.Transaction.from_cbor_hex(cbor),
  sign: {
    withWallet: () => ({
      complete: async () => {
        const unsigned = CML.Transaction.from_cbor_hex(cbor);
        const vkeys = CML.VkeywitnessList.new();
        vkeys.add(
          CML.make_vkey_witness(CML.hash_transaction(unsigned.body()), key),
        );
        const witnesses = unsigned.witness_set();
        witnesses.set_vkeywitnesses(vkeys);
        const signed = CML.Transaction.new(
          unsigned.body(),
          witnesses,
          true,
          undefined,
        ).to_cbor_hex();
        return { toCBOR: () => signed };
      },
    }),
  },
});
const txHashOf = (cbor: string) =>
  CML.hash_transaction(CML.Transaction.from_cbor_hex(cbor).body()).to_hex();
const inputsOf = (cbor: string) => {
  const inputs = CML.Transaction.from_cbor_hex(cbor).body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};
const TIMED_OUT_HEADER = "44".repeat(28);

describe("a Timeout against a pool whose datum another spend re-encoded (P14(5))", () => {
  // TopUp keeps the pool datum by Data value only, so anyone paying the
  // minimum may re-store it in another encoding. The Timeout's tip read goes
  // through the SDK's value-level decoder, so a withholding committee cannot
  // block its own slash this way.
  const tipPool = (datum: string): UTxO => ({
    ...daBondPoolUtxo(100_005n * ADA),
    address: "pool",
    datum,
  });
  const expiredAtHead = () => {
    const expired = fixture("44", BigInt(Date.now()));
    openLanded(expired.challenged.headerHash);
    io.utxos = [utxo(0, TIMEOUT_COLLATERAL, "c1"), utxo(1, 10n * ADA, "c2")];
    io.tipPoolRead = "sdk";
    return expired;
  };

  it.each([
    ["the indefinite-length form", "d8799fff"],
    ["the tag-102 constructor form", "d866820080"],
    ["the tag-102 form with an indefinite field list", "d86682009fff"],
  ])(
    "builds the Timeout against a Bonded pool stored in %s",
    async (_label, datum) => {
      const expired = expiredAtHead();
      io.tipPool = tipPool(datum);
      const watcher = await runtime();
      try {
        await watcher.reconcile(
          observation([timedOut(expired.challenged)]),
          true,
        );
        expect(watcher.status()).toMatchObject({
          phase: "waiting",
          action: "timeout",
        });
        expect(watcher.status().timeoutsDeferred ?? []).toEqual([]);
      } finally {
        await watcher.close();
      }
      expect(actions()).toEqual([[expired.challenged.headerHash, "timeout"]]);
      expect(io.timeouts.map(({ pool }) => pool)).toEqual([io.tipPool]);
    },
  );

  it.each([
    ["trailing bytes after the datum", "d8798000", "not valid Plutus Data"],
    ["a foreign constructor", "d87b80", "not valid Plutus Data"],
    ["a negative unlock_at", "d87a8120", "non-negative unlock_at"],
  ])(
    "defers the Timeout when the tip pool stores %s",
    async (_label, datum, detail) => {
      const expired = expiredAtHead();
      io.tipPool = tipPool(datum);
      const watcher = await runtime();
      try {
        await watcher.reconcile(
          observation([timedOut(expired.challenged)]),
          true,
        );
        expect(watcher.status().timeoutsDeferred).toEqual([
          expect.objectContaining({
            headerHash: expired.challenged.headerHash,
            reason: "tip-pool-unavailable",
            detail: expect.stringContaining(detail),
          }),
        ]);
      } finally {
        await watcher.close();
      }
      expect(io.timeouts).toEqual([]);
    },
  );
});

describe("a Timeout whose pool moved after it was built (P13(4)(b))", () => {
  it("expires the stranded Timeout through the partial-missing arm, then rebuilds it against the new tip pool and lands it", async () => {
    // The SDK's own runner and reconciliation, on real signed bytes.
    const actual =
      await vi.importActual<typeof import("@al-ft/midgard-sdk")>(
        "@al-ft/midgard-sdk",
      );
    io.run.mockImplementation(actual.runDaAvailabilityOperation);
    io.reconcile.mockImplementation(actual.reconcileDaAvailabilityOperations);
    const deployment = "d0".repeat(32);
    const key = CML.PrivateKey.generate_ed25519();
    // The mocked wallet's payment credential is its address.
    io.walletAddress = key.to_public().hash().to_hex();
    io.queuePolicy = "a8".repeat(28);
    io.limits = {
      maxTxSize: 16_384,
      maxTxExMem: 14_000_000n,
      maxTxExSteps: 10_000_000_000n,
      coinsPerUtxoByte: 4_310n,
      feeCeilings: { timeout: PARAMETERS.max_timeout_fee_lovelace },
      timeoutFeePartCeiling: PARAMETERS.da_slash_penalty_lovelace,
    };
    // The finalized point's slot, which reconciliation compares with each
    // intent's validity.
    let slot = 100;
    io.timeoutTx = (input) => signable(timeoutTransaction(input, slot), key);
    const broadcasts: string[] = [];
    io.submitTx.mockImplementation(async (cbor: string) => {
      broadcasts.push(cbor);
      return txHashOf(cbor);
    });
    // Canonical input state at the finalized point, as the source reports it.
    const spentElsewhere = new Set<string>();
    const landed = new Set<string>();
    io.operation.mockImplementation(
      async (
        _observation: unknown,
        intent: Readonly<{
          txHash: string;
          spentOutRefs: readonly string[];
        }>,
      ): Promise<SDK.DaAvailabilityOperationObservation> => {
        if (landed.has(intent.txHash))
          return {
            status: "included",
            txHash: intent.txHash,
            inclusionPoint: "block",
            confirmationDepth: 1,
          };
        const missingOutRefs = intent.spentOutRefs.filter((ref) =>
          spentElsewhere.has(ref),
        );
        return missingOutRefs.length === 0
          ? { status: "unspent", currentSlot: slot }
          : { status: "inputs_missing", currentSlot: slot, missingOutRefs };
      },
    );
    const expired = fixture("44", BigInt(Date.now()));
    expect(expired.challenged.headerHash).toBe(TIMED_OUT_HEADER);
    io.utxos = [utxo(0, TIMEOUT_COLLATERAL, "c1"), utxo(1, 10n * ADA, "c2")];
    const firstPool = { ...utxo(9, 100_005n * ADA, "c9"), address: "pool" };
    const toppedUp = { ...utxo(0, 101_005n * ADA, "ca"), address: "pool" };
    const poolRef = (pool: UTxO) =>
      `${pool.txHash}#${pool.outputIndex.toString()}`;
    io.tipPool = firstPool;
    const state = observation([timedOut(expired.challenged)]);
    const watcher = await runtime(deployment);
    try {
      // Tick 1 builds and broadcasts the Timeout against the tip pool.
      await watcher.reconcile(state, true);
      expect(watcher.status()).toMatchObject({ action: "timeout" });
      expect(broadcasts).toHaveLength(1);
      expect(inputsOf(broadcasts[0]!)).toContain(poolRef(firstPool));

      // A TopUp spends that pool before the Timeout lands: the Timeout's
      // only missing normal input is the pool, and its validity runs out.
      spentElsewhere.add(poolRef(firstPool));
      io.tipPool = toppedUp;
      slot = 1_100;

      // Tick 2 expires it and rebuilds against the topped-up pool.
      await watcher.reconcile(state, true);
      expect(watcher.status()).toMatchObject({
        phase: "waiting",
        action: "timeout",
      });
      expect(io.timeouts.map(({ pool }) => pool)).toEqual([
        firstPool,
        toppedUp,
      ]);
      expect(broadcasts).toHaveLength(2);
      expect(inputsOf(broadcasts[1]!)).toContain(poolRef(toppedUp));
      expect(inputsOf(broadcasts[1]!)).not.toContain(poolRef(firstPool));

      // Tick 3 (observing only) finds the rebuilt Timeout finalized.
      landed.add(txHashOf(broadcasts[1]!));
      await watcher.reconcile(state, false);
      expect(watcher.status()).toMatchObject({ phase: "ready" });
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([
      [TIMED_OUT_HEADER, "timeout"],
      [TIMED_OUT_HEADER, "timeout"],
    ]);
    withJournal((journal) => {
      expect(journal.findTransaction(txHashOf(broadcasts[0]!))).toMatchObject({
        state: "expired",
        detail:
          "Expired with a normal input spent elsewhere and another still unspent",
      });
      const rebuilt = journal.findTransaction(txHashOf(broadcasts[1]!))!;
      expect(rebuilt.state).toBe("confirmed");
      expect(journal.pending(deployment, io.walletAddress)).toEqual([]);
      // Confirmed inside its validity: it still holds exactly what it spends.
      expect(journal.reservedOutRefs(io.walletAddress)).toEqual(
        [
          ...rebuilt.intent.spentOutRefs,
          ...rebuilt.intent.collateralOutRefs,
        ].sort(),
      );
    });
  });
});

describe("the DA bond pool is read every reconciliation and never blocks (spec #685 E5, #691)", () => {
  const backed =
    PARAMETERS.da_bond_pool_floor_lovelace + PARAMETERS.da_bond_lovelace;
  const withdrawing: SDK.DaBondPoolDatum = {
    Withdrawing: { unlock_at: 1_900_000_000_000n },
  };
  /** A 64-hex deployment id, as the operations sink requires. */
  const MANIFEST = "ab".repeat(32);
  /** The watcher's operations status with every other readiness input healthy. */
  const operationsFixture = () => {
    const operations = createWatcherOperationsObservability({
      deploymentFingerprint: MANIFEST,
      supervisor: {
        status: () => ({
          phase: "accepting",
          recovered: true,
          unfinishedObjectiveCount: 0,
          queuedJobCount: 0,
          activeJob: null,
          blockedJob: null,
          deadlineHealth: "safe",
          earliestDeadlineJob: null,
          remainingSafeStartMs: null,
          journalIntegrity: null,
          journalUnavailable: null,
          journalDecisionMissing: [],
          journalBusy: null,
        }),
      } as unknown as WatcherFaultProofSupervisor,
      launchScopeStatus: () => ({
        installedCategoryCount: 1,
        requiredCategoryCount: 1,
      }),
      durableProofQueueStatus: () => ({
        queuedJobCount: 0,
        oldestQueuedAtMs: null,
      }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
    });
    operations.sink.recordL1Source({
      sourceIdentityDigest: "22".repeat(32),
      sourceMode: "local_node",
      status: "consistent",
      blockHash: "33".repeat(32),
      blockNo: "1",
      slot: "1",
      observedAtMs: Date.now().toString(),
    });
    return operations;
  };
  /**
   * The runtime wired to the operations sink through the reporter the watcher
   * runtime passes as its `onDaBondPool` and `onDaBondPoolReadFailure`.
   */
  const wired = async () => {
    const operations = operationsFixture();
    const readouts: WatcherDaBondPoolObservation[] = [];
    const report = watcherDaBondPoolReporter(operations.sink, MANIFEST);
    const watcher = await runtime(DEPLOYMENT, {
      onDaBondPool: (pool) => {
        readouts.push(pool);
        report(pool);
      },
      onDaBondPoolReadFailure: watcherDaBondPoolReadFailureReporter(
        operations.sink,
      ),
    });
    return { operations, readouts, watcher };
  };
  const poolCodes = (operations: ReturnType<typeof operationsFixture>) =>
    operations.api
      .status()
      .activeAlerts.map(({ code }) => code)
      .filter((code) => code.startsWith("da_bond_pool_"));

  it("reports the pool on a reconciliation with no pending header", async () => {
    io.pool.mockResolvedValue(daBondPoolUtxo(backed));
    const { operations, readouts, watcher } = await wired();
    try {
      await watcher.reconcile(observation([]), false);
      expect(io.pool).toHaveBeenCalledTimes(1);
      expect(watcher.status()).toMatchObject({
        phase: "ready",
        pendingHeaders: [],
        pool: {
          state: "bonded",
          backing: PARAMETERS.da_bond_lovelace.toString(),
          alerts: { underBacked: false, withdrawing: false },
        },
      });
      expect(readouts).toHaveLength(1);
      expect(operations.api.status()).toMatchObject({
        readiness: "ready",
        daBondPool: { state: "bonded", belowBond: false },
      });
    } finally {
      await watcher.close();
    }
    expect(io.snapshot).not.toHaveBeenCalled();
  });

  it("fires under-backed on a slash-drained pool and clears it after a top-up, never touching phase or readiness", async () => {
    const drained = SDK.planDaBondPoolSlash({
      poolLovelace: backed,
      parameters: PARAMETERS,
    }).poolOutputLovelace;
    const { operations, readouts, watcher } = await wired();
    try {
      io.pool.mockResolvedValue(daBondPoolUtxo(backed));
      await watcher.reconcile(observation([]), false);
      expect(poolCodes(operations)).toEqual([]);
      io.pool.mockResolvedValue(daBondPoolUtxo(drained));
      await watcher.reconcile(observation([]), false);
      expect(watcher.status()).toMatchObject({
        phase: "ready",
        pool: { belowBond: true, alerts: { underBacked: true } },
      });
      expect(poolCodes(operations)).toEqual(["da_bond_pool_under_backed"]);
      expect(operations.api.status()).toMatchObject({
        readiness: "ready",
        readinessReasons: [],
      });
      io.pool.mockResolvedValue(daBondPoolUtxo(backed));
      await watcher.reconcile(observation([]), false);
      expect(watcher.status()).toMatchObject({
        phase: "ready",
        pool: { belowBond: false, alerts: { underBacked: false } },
      });
      expect(poolCodes(operations)).toEqual([]);
    } finally {
      await watcher.close();
    }
    expect(readouts.map(({ alerts }) => alerts.underBacked)).toEqual([
      false,
      true,
      false,
    ]);
  });

  it("fires withdrawing on BeginWithdraw and clears it after a cancel, never touching phase or readiness", async () => {
    const { operations, readouts, watcher } = await wired();
    try {
      io.pool.mockResolvedValue(daBondPoolUtxo(backed, withdrawing));
      await watcher.reconcile(observation([]), false);
      expect(watcher.status()).toMatchObject({
        phase: "ready",
        pool: {
          state: "withdrawing",
          unlockAt: "1900000000000",
          alerts: { withdrawing: true, underBacked: false },
        },
      });
      expect(poolCodes(operations)).toEqual(["da_bond_pool_withdrawing"]);
      expect(operations.api.status().readiness).toBe("ready");
      io.pool.mockResolvedValue(daBondPoolUtxo(backed, "Bonded"));
      await watcher.reconcile(observation([]), false);
      expect(watcher.status()).toMatchObject({
        phase: "ready",
        pool: { state: "bonded", alerts: { withdrawing: false } },
      });
      expect(poolCodes(operations)).toEqual([]);
    } finally {
      await watcher.close();
    }
    expect(readouts.map(({ state }) => state)).toEqual([
      "withdrawing",
      "bonded",
    ]);
  });

  it.each([
    ["missing", (): UTxO | undefined => undefined],
    ["withdrawing", () => daBondPoolUtxo(backed, withdrawing)],
    ["under-backed", () => daBondPoolUtxo(backed - 1n)],
    // The local Kupmios source re-encodes outputs canonically, so lucid's
    // indefinite-length Withdrawing datum comes back definite-length.
    [
      "canonically re-encoded withdrawing",
      () => ({
        ...daBondPoolUtxo(backed, withdrawing),
        datum: "d87a811b000001ba60d33800",
      }),
    ],
    // TopUp pins the datum by Data value, so anyone may re-store it this way.
    [
      "re-encoded under-backed",
      () => ({ ...daBondPoolUtxo(backed - 1n), datum: "d8799fff" }),
    ],
  ] as const)(
    "reports a %s pool while staying ready and still Opening a withheld header",
    async (_label, poolOf) => {
      const header = withheld("46");
      io.pool.mockResolvedValue(poolOf());
      io.utxos = [
        utxo(0, TIMEOUT_COLLATERAL, "d1"),
        utxo(1, OPENING, "d2"),
        utxo(2, 100n * ADA, "d2"),
      ];
      const watcher = await runtime();
      try {
        const state = observation([header.attested]);
        // Readiness is `phase !== "blocked"` (watcher-runtime status).
        await watcher.reconcile(state, false);
        expect(watcher.status().phase).toBe("ready");
        expect(Object.values(watcher.status().pool!.alerts).some(Boolean)).toBe(
          true,
        );
        await watcher.reconcile(state, true);
        expect(watcher.status()).toMatchObject({
          phase: "waiting",
          action: "open",
        });
      } finally {
        await watcher.close();
      }
      expect(actions()).toEqual([[header.attested.headerHash, "open"]]);
    },
  );

  it("reports a failed pool read without blocking, still Opening a withheld header", async () => {
    const header = withheld("48");
    io.pool.mockRejectedValue(new Error("pool read failed"));
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL, "d1"),
      utxo(1, OPENING, "d2"),
      utxo(2, 100n * ADA, "d2"),
    ];
    const watcher = await runtime();
    try {
      const state = observation([header.attested]);
      await watcher.reconcile(state, false);
      expect(watcher.status()).toMatchObject({
        phase: "ready",
        poolReadFailure: "pool read failed",
      });
      expect(watcher.status().pool).toBeUndefined();
      await watcher.reconcile(state, true);
      expect(watcher.status()).toMatchObject({
        phase: "waiting",
        action: "open",
      });
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([[header.attested.headerHash, "open"]]);
  });

  it("keeps the last good readout when a later reconciliation fails", async () => {
    const { operations, readouts, watcher } = await wired();
    try {
      io.pool.mockResolvedValue(daBondPoolUtxo(backed, withdrawing));
      await watcher.reconcile(observation([]), false);
      const good = watcher.status().pool;
      expect(good?.state).toBe("withdrawing");
      io.pool.mockRejectedValue(new Error("pool read failed"));
      await watcher.reconcile(observation([]), false);
      expect(watcher.status()).toMatchObject({
        phase: "ready",
        poolReadFailure: "pool read failed",
      });
      expect(watcher.status().pool).toEqual(good);
      // The failure is served on /v1/status next to the last good readout,
      // and the watcher stays ready.
      expect(operations.api.status()).toMatchObject({
        readiness: "ready",
        daBondPool: { state: "withdrawing" },
        daBondPoolReadFailure: { error: "pool read failed" },
      });
      // A failure after a good pool read keeps that read too.
      const pendingHeader = observation([withheld("47").attested]);
      io.pool.mockResolvedValue(daBondPoolUtxo(backed));
      io.snapshot.mockRejectedValue(new Error("snapshot failed"));
      await watcher.reconcile(pendingHeader, false);
      expect(watcher.status()).toMatchObject({
        phase: "blocked",
        detail: "snapshot failed",
        pool: { state: "bonded" },
      });
      // The good read cleared the earlier read failure.
      expect(watcher.status().poolReadFailure).toBeUndefined();
      expect(operations.api.status().daBondPoolReadFailure).toBeNull();
    } finally {
      await watcher.close();
    }
    expect(readouts.map(({ state }) => state)).toEqual([
      "withdrawing",
      "bonded",
    ]);
  });
});
