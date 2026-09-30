/**
 * Pooled DA bond (#689): the availability challenge lifecycle against the ONE
 * pooled committee bond, from attestation through close or timeout, with the
 * pool's `Slash` taking `min(da_bond, backing)` in the timeout itself.
 *
 * Every refusal below is paired with its honest control and names the script,
 * purpose (and, where the ledger order is fixed, the index) that refuses it.
 */
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "./helpers/availability-challenge.js";
import "./helpers/availability-challenge-emulator.js";
import "./availability-challenge-pool-slash-lifecycle.submit-built.js";
import "./availability-challenge-pool-slash-lifecycle.complete-timeout-mirror.js";
import "./availability-challenge-pool-slash-lifecycle.build-empty-block-merge.js";

import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  advanceToMaturity,
  buildEmptyBlockMerge,
  expectTimeoutArithmetic,
} from "./availability-challenge-pool-slash-lifecycle.build-empty-block-merge.js";
import {
  buildLockedHeadResume,
  completeTimeoutMirror,
} from "./availability-challenge-pool-slash-lifecycle.complete-timeout-mirror.js";
import {
  attestAndOpen,
  backingOf,
  expireAndSettle,
  MAX_MEMORY,
  MAX_SIGNED_BYTES,
  MAX_STEPS,
  nodeOf,
  plainOutputMinAda,
  position,
  recordOf,
  refusalOf,
  resources,
  snapshot,
  submitBuilt,
  timeoutParams,
  timeoutYieldWithdrawIndex,
} from "./availability-challenge-pool-slash-lifecycle.submit-built.js";
import { TEST_AVAILABILITY_PARAMETERS as parameters } from "./helpers/availability-challenge.js";
import {
  assertAvailabilityRefusal,
  attestAvailability,
  AVAILABILITY_DEFAULT_POOL_LOVELACE,
  availabilityDeployment,
  createAvailabilityFixture,
  openAvailability,
} from "./helpers/availability-challenge-emulator.js";

// ---------------------------------------------------------------------------

describe("pooled DA bond challenge and slash lifecycle", () => {
  it("merges an honestly attested block into the confirmed state", async () => {
    const f = await createAvailabilityFixture(1);
    const attested = await attestAvailability(f);
    const node = nodeOf(attested.queue);
    expect(node.da_attestation).toEqual({
      Attested: { commitment_hash: attested.commitmentHash },
    });
    expect(
      SDK.daAvailabilityStateQueueStatusPermitsMerge(node.da_attestation),
    ).toBe(true);
    // No L2 material: the merge binds no settlement.
    for (const root of [
      node.header.transactionsRoot,
      node.header.depositsRoot,
      node.header.withdrawalsRoot,
      node.header.forcedTransactionsRoot,
    ])
      expect(root).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    advanceToMaturity(f, attested.queue);
    const merge = await buildEmptyBlockMerge(f, attested.queue);
    const [root] = await f.submit("merge the attested block", merge.tx);
    expect(root!.address).toBe(merge.root.address);
    expect(root!.assets).toEqual(merge.root.assets);
    expect(root!.datum).toBe(merge.continuedDatum);
    const rootView = Effect.runSync(SDK.getLinkedListNodeViewFromUTxO(root!));
    expect(rootView.next).toBe("Empty");
    expect(Data.castFrom(rootView.data, SDK.ConfirmedState)).toEqual(
      merge.continued,
    );
    expect(
      await f.lucid.utxosAtWithUnit(
        f.contracts.stateQueue.spendingScriptAddress,
        f.queueUnit,
      ),
    ).toHaveLength(0);
  }, 180_000);

  /**
   * The negative of the merge above: the same hand-built merge of a mature
   * block whose status is `Challenged` is refused by the merge yield's DA
   * gate (`merge_to_confirmed_state` admits only `Attested` and `Published`).
   */
  it("refuses to merge a block whose availability challenge is open", async () => {
    const f = await createAvailabilityFixture(1);
    const d = availabilityDeployment(f);
    const s = await attestAndOpen(f, d);
    const queue = s.queue!.utxo;
    expect(nodeOf(queue).da_attestation).toEqual({
      Challenged: {
        commitment_hash: SDK.daAvailabilityCommitmentHash(
          recordOf(s).commitment,
        ),
        challenge_asset_name: recordOf(s).challenge_asset_name,
      },
    });
    advanceToMaturity(f, queue);
    await assertAvailabilityRefusal(
      (await buildEmptyBlockMerge(f, queue)).tx,
      {
        purpose: "withdraw",
        script: "state-queue merge withdrawal",
        // The merge's only withdrawal.
        index: 0,
      },
      f.scriptNames,
    );
    expect(
      await f.lucid.utxosAtWithUnit(
        f.contracts.stateQueue.spendingScriptAddress,
        f.queueUnit,
      ),
    ).toHaveLength(1);
  }, 180_000);

  it("closes a challenge the responder answers and refunds the challenger", async () => {
    const f = await createAvailabilityFixture(1);
    const d = availabilityDeployment(f);
    const poolBefore = await f.getPool();
    let s = await attestAndOpen(f, d);
    const b = recordOf(s);
    expect(
      SDK.daAvailabilityStateQueueStatusPermitsMerge(
        nodeOf(s.queue!.utxo).da_attestation,
      ),
    ).toBe(false);
    let thread = s.tranches[0]!.utxo;
    let carrier: UTxO | undefined;
    for (const publication of SDK.planDaAvailabilityPublications({
      commitment: b.commitment,
      payload: f.payload,
      challengeAssetName: b.challenge_asset_name,
    })[0]!.publications) {
      const { outputs } = await submitBuilt(
        f,
        await Effect.runPromise(
          SDK.buildPublishDaAvailabilityChunkTxProgram(f.lucid, d, {
            ...(await resources(
              f,
              parameters.max_publication_fee_lovelace,
              b.response_deadline,
            )),
            thread,
            previousCarrier: carrier,
            publication,
          }),
        ),
      );
      thread = outputs[0]!;
      carrier = outputs[1]!;
    }
    s = await snapshot(f, d);
    const {
      outputs: [terminal],
    } = await submitBuilt(
      f,
      await Effect.runPromise(
        SDK.buildSettleDaAvailabilityTrancheTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_settlement_fee_lovelace)),
          record: s.record!,
          terminal: s.terminal!,
          thread,
          carrier,
        }),
      ),
    );
    const { outputs } = await submitBuilt(
      f,
      await Effect.runPromise(
        SDK.buildCloseDaAvailabilityChallengeTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_close_fee_lovelace)),
          record: s.record!,
          terminal: terminal!,
          queue: s.queue!.utxo,
        }),
      ),
    );
    expect(outputs).toHaveLength(2);
    const published = nodeOf(outputs[0]!);
    expect(published.da_attestation).toEqual({
      Published: {
        terminal_commitment: SDK.daAvailabilityPublishedTerminalCommitment(
          b.commitment,
        ),
      },
    });
    expect(
      SDK.daAvailabilityStateQueueStatusPermitsMerge(published.da_attestation),
    ).toBe(true);
    expect(outputs[1]!.address).toBe(f.challenger.address);
    expect(outputs[1]!.assets).toEqual({
      lovelace:
        parameters.challenger_bond_lovelace -
        parameters.max_publication_fee_lovelace -
        parameters.max_settlement_fee_lovelace -
        parameters.max_close_fee_lovelace +
        parameters.challenge_record_lovelace,
    });
    // The pooled bond is untouched by an answered challenge.
    expect((await f.getPool()).assets).toEqual(poolBefore.assets);
    const closed = await snapshot(f, d);
    expect(closed.record).toBeUndefined();
    expect(closed.tranches).toHaveLength(0);
  }, 180_000);

  it("times out a withheld block with a full slash of the pooled bond", async () => {
    const f = await createAvailabilityFixture(1);
    const d = availabilityDeployment(f);
    const s = await attestAndOpen(f, d);
    const terminal = await expireAndSettle(f, d, s);
    const pool = await f.getPool();
    expect(backingOf(pool)).toBeGreaterThanOrEqual(parameters.da_bond_lovelace);
    const remaining = Data.from(
      terminal.datum!,
      SDK.DaAvailabilityTerminalAccumulatorDatum,
    ).remaining_challenger_lovelace;
    const slash = SDK.planDaBondPoolSlash({
      poolLovelace: pool.assets.lovelace,
      parameters,
    });
    const cap = parameters.max_timeout_fee_lovelace;
    const timeoutYieldIndex = timeoutYieldWithdrawIndex(f);
    const mirror = (c: bigint) =>
      completeTimeoutMirror(f, s, terminal, {
        pool,
        poolOutputLovelace: slash.poolOutputLovelace,
        feeLovelace: slash.feePart + c,
        challengerOutputLovelace:
          remaining - c + parameters.challenge_record_lovelace + slash.payout,
      });
    // Honest controls: c = 0 and c = max_timeout_fee both evaluate.
    await mirror(0n);
    await mirror(cap);
    // The fee burns less than fee_part (c = -1): `challenger_fee >= 0`.
    await assertAvailabilityRefusal(
      mirror(-1n),
      {
        purpose: "withdraw",
        script: "availability-challenge timeout withdrawal",
        index: timeoutYieldIndex,
      },
      f.scriptNames,
    );
    // c above the cap: `challenger_fee <= max_timeout_fee_lovelace`.
    await assertAvailabilityRefusal(
      mirror(cap + 1n),
      {
        purpose: "withdraw",
        script: "availability-challenge timeout withdrawal",
        index: timeoutYieldIndex,
      },
      f.scriptNames,
    );
    // ... which the production builder refuses before building.
    expect(
      (
        await refusalOf(
          SDK.buildTimeoutDaAvailabilityChallengeTxProgram(f.lucid, d, {
            ...(await timeoutParams(f, s, terminal, pool)),
            challengerFeeLovelace: cap + 1n,
          }),
        )
      ).reason,
    ).toBe("timeout-challenger-fee-cap");
    // Without the pool input the timeout yield finds no authentic pool.
    await assertAvailabilityRefusal(
      completeTimeoutMirror(f, s, terminal, {
        feeLovelace: 2_000_000n,
        challengerOutputLovelace:
          remaining - 2_000_000n + parameters.challenge_record_lovelace,
      }),
      {
        purpose: "withdraw",
        script: "availability-challenge timeout withdrawal",
        index: timeoutYieldIndex,
      },
      f.scriptNames,
    );

    const built = await Effect.runPromise(
      SDK.buildTimeoutDaAvailabilityChallengeTxProgram(
        f.lucid,
        d,
        await timeoutParams(f, s, terminal, pool),
      ),
    );
    const landed = await submitBuilt(f, built);
    const c = expectTimeoutArithmetic(
      f,
      built,
      landed,
      { pool, terminal },
      {
        taken: parameters.da_bond_lovelace,
        feePart: parameters.da_slash_penalty_lovelace,
        payout:
          parameters.da_bond_lovelace - parameters.da_slash_penalty_lovelace,
      },
    );
    // A fully backed pool's penalty pays the whole fee.
    expect(c).toBe(0n);
    expect(landed.measurement.fee).toBe(parameters.da_slash_penalty_lovelace);
    // H12: the timeout's size and aggregate budget.
    console.info(
      "pooled timeout H12",
      JSON.stringify({
        signedBytes: landed.measurement.signedBytes,
        memory: landed.measurement.memory.toString(),
        steps: landed.measurement.steps.toString(),
        fee: landed.measurement.fee.toString(),
      }),
    );
    expect(landed.measurement.signedBytes).toBeLessThanOrEqual(
      MAX_SIGNED_BYTES,
    );
    expect(landed.measurement.memory).toBeLessThanOrEqual(MAX_MEMORY);
    expect(landed.measurement.steps).toBeLessThanOrEqual(MAX_STEPS);
    expect((await snapshot(f, d)).queue).toBeUndefined();
  }, 240_000);

  /**
   * A LATE timeout against a pool the owners' completed withdrawal drew below
   * one bond (`taken = backing < da_bond`).
   *
   * Only the owners' withdrawal can leave a withheld block facing a partly
   * funded pool. Both timeout removal arms are head-anchored:
   * `state-queue.ak` `remove_unavailable_head_v1` requires
   * `removed_link == None`, and `prune_unavailable_block_descendant_v1`
   * requires `head_link == unavailable_header_hash`. The pool's `Slash` runs
   * only in the Idle-lock first step, and descendants are pruned in Locked
   * resume steps with no slash. Apply requires a `Bonded` pool with
   * `backing >= da_bond`. So no block applied while the pool was full
   * survives a slash to face the reduced pool. A `Slash` continuation and a
   * `CompleteWithdraw` output carry the same datum bytes (`Bonded`) and the
   * same pool NFT, so the partial arithmetic is the same on either route.
   *
   * The withdrawal takes effect only at `unlock_at` = the BeginWithdraw upper
   * bound + `da_bond_withdraw_delay`, and that delay covers validity +
   * max(window + full response, maturity) + slash grace. A timely challenger,
   * whose timeout lands by `response_deadline + da_slash_grace`, therefore
   * always meets a full bond (I3). The timeout has no on-chain upper bound,
   * so a late challenger can still meet the drawn-down pool, which is the
   * case here: `fee_part = penalty` first, `payout = backing - penalty`.
   *
   * The first row leaves `0 < payout < minUTxO`: the reward cannot stand as
   * an output of its own and lands merged into the one challenger output
   * (D3).
   */
  it.each([
    {
      label: "payout below min-UTxO",
      backing: parameters.da_slash_penalty_lovelace + 500_000n,
    },
    {
      label: "penalty + 1 ADA",
      backing: parameters.da_slash_penalty_lovelace + 1_000_000n,
    },
    { label: "250 ADA", backing: 250_000_000n },
  ])(
    "times out a withheld block late, against a pool a withdrawal drew below one bond ($label)",
    async ({ backing }) => {
      const f = await createAvailabilityFixture(1);
      const d = availabilityDeployment(f);
      const s = await attestAndOpen(f, d);
      expect(backingOf(await f.getPool())).toBe(
        AVAILABILITY_DEFAULT_POOL_LOVELACE -
          parameters.da_bond_pool_floor_lovelace,
      );
      expect(backing).toBeLessThan(parameters.da_bond_lovelace);
      expect(backing).toBeGreaterThan(parameters.da_slash_penalty_lovelace);
      const { unlockAt } = await f.beginPoolWithdraw();
      // A6: the drawn-down pool is out of a timely challenger's reach.
      expect(unlockAt).toBeGreaterThanOrEqual(
        recordOf(s).response_deadline + f.timing.daSlashGraceMs,
      );
      f.advanceToMs(unlockAt);
      await f.completePoolWithdraw(backingOf(await f.getPool()) - backing);
      const pool = await f.getPool();
      expect(backingOf(pool)).toBe(backing);
      expect(pool.datum).toBe(Data.to("Bonded", SDK.DaBondPoolDatum));
      const terminal = await expireAndSettle(f, d, s);
      const payout = backing - parameters.da_slash_penalty_lovelace;
      const built = await Effect.runPromise(
        SDK.buildTimeoutDaAvailabilityChallengeTxProgram(
          f.lucid,
          d,
          await timeoutParams(f, s, terminal, pool),
        ),
      );
      const landed = await submitBuilt(f, built);
      const c = expectTimeoutArithmetic(
        f,
        built,
        landed,
        { pool, terminal },
        {
          taken: backing,
          feePart: parameters.da_slash_penalty_lovelace,
          payout,
        },
      );
      expect(c).toBe(0n);
      // The pool keeps exactly its floor.
      expect(landed.outputs[3]!.assets.lovelace).toBe(
        parameters.da_bond_pool_floor_lovelace,
      );
      if (backing === parameters.da_slash_penalty_lovelace + 500_000n) {
        expect(payout).toBeGreaterThan(0n);
        expect(payout).toBeLessThan(plainOutputMinAda(f.challenger.address));
      }
    },
    240_000,
  );

  /**
   * The owners race a withdrawal against a challenge: BeginWithdraw lands
   * after the Open, and the timeout lands after `response_deadline` and
   * before `unlock_at`. The `Withdrawing` pool still pays the full bond, its
   * datum continues byte for byte (same `unlock_at`) with the NFT, and the
   * later CompleteWithdraw can draw only what the slash left.
   */
  it("times out a withheld block against a Withdrawing pool at full slash", async () => {
    const f = await createAvailabilityFixture(1);
    const d = availabilityDeployment(f);
    const s = await attestAndOpen(f, d);
    const { pool: withdrawing, unlockAt } = await f.beginPoolWithdraw();
    expect(withdrawing.datum).toBe(
      Data.to({ Withdrawing: { unlock_at: unlockAt } }, SDK.DaBondPoolDatum),
    );
    const terminal = await expireAndSettle(f, d, s);
    const pool = await f.getPool();
    expect(pool.datum).toBe(withdrawing.datum);
    expect(BigInt(f.emulator.now())).toBeGreaterThan(
      recordOf(s).response_deadline,
    );
    const built = await Effect.runPromise(
      SDK.buildTimeoutDaAvailabilityChallengeTxProgram(
        f.lucid,
        d,
        await timeoutParams(f, s, terminal, pool),
      ),
    );
    const landed = await submitBuilt(f, built);
    // The timeout's upper bound is before unlock_at.
    expect(BigInt(f.emulator.now())).toBeLessThan(unlockAt);
    const c = expectTimeoutArithmetic(
      f,
      built,
      landed,
      { pool, terminal },
      {
        taken: parameters.da_bond_lovelace,
        feePart: parameters.da_slash_penalty_lovelace,
        payout:
          parameters.da_bond_lovelace - parameters.da_slash_penalty_lovelace,
      },
    );
    expect(c).toBe(0n);
    const slashed = landed.outputs[3]!;
    expect(slashed.datum).toBe(withdrawing.datum);
    expect(slashed.assets[f.poolUnit]).toBe(1n);
    const remaining = backingOf(slashed);
    expect(remaining).toBe(
      backingOf(withdrawing) - parameters.da_bond_lovelace,
    );
    f.advanceToMs(unlockAt);
    // CompleteWithdraw draws at most the backing the slash left.
    const overdraw = await Effect.runPromise(
      Effect.either(
        SDK.buildCompleteDaBondPoolWithdrawTxProgram(f.lucid, {
          poolValidator: f.contracts.daBondPool,
          parameters,
          pool: { utxo: await f.getPool() },
          daParamsUtxo: f.daParamsUtxo,
          signerKeyHashes: f.daParamsDatum.owners,
          referenceScripts: {
            daBondPoolSpending: f.poolReferences.daBondPoolSpending,
          },
          amount: remaining + 1n,
          destination: f.responder.address,
          validity: { validFrom: BigInt(f.emulator.now()) },
        }),
      ),
    );
    expect(overdraw._tag).toBe("Left");
    if (overdraw._tag === "Left")
      expect(overdraw.left.reason).toBe("amount_exceeds_backing");
    const drained = await f.completePoolWithdraw(remaining);
    expect(drained.assets.lovelace).toBe(
      parameters.da_bond_pool_floor_lovelace,
    );
    expect(drained.datum).toBe(Data.to("Bonded", SDK.DaBondPoolDatum));
  }, 240_000);

  it("times out a withheld block against an empty pool, the challenger paying the fee", async () => {
    const f = await createAvailabilityFixture(1);
    const d = availabilityDeployment(f);
    const s = await attestAndOpen(f, d);
    const { unlockAt } = await f.beginPoolWithdraw();
    f.advanceToMs(unlockAt);
    await f.completePoolWithdraw(backingOf(await f.getPool()));
    const pool = await f.getPool();
    expect(backingOf(pool)).toBe(0n);
    const terminal = await expireAndSettle(f, d, s);
    const built = await Effect.runPromise(
      SDK.buildTimeoutDaAvailabilityChallengeTxProgram(
        f.lucid,
        d,
        await timeoutParams(f, s, terminal, pool),
      ),
    );
    const landed = await submitBuilt(f, built);
    const c = expectTimeoutArithmetic(
      f,
      built,
      landed,
      { pool, terminal },
      { taken: 0n, feePart: 0n, payout: 0n },
    );
    // Nothing is slashed: the fee is the challenger's c alone, and the pool
    // continues unchanged.
    expect(c).toBeGreaterThan(0n);
    expect(landed.measurement.fee).toBe(c);
    expect(landed.outputs[3]!.assets).toEqual(pool.assets);
  }, 240_000);

  it("refuses an Open whose upper bound reaches end_time + da_challenge_window", async () => {
    const f = await createAvailabilityFixture(1);
    const attested = await attestAvailability(f);
    const endTime = nodeOf(attested.queue).header.endTime;
    const closesAt = endTime + f.timing.daChallengeWindowMs;
    // Validity bounds are whole slots, and the header's end time need not sit
    // on the slot grid. `lastInTime` is the last slot whose inclusive upper
    // `validTo - 1` is before the deadline, `firstLate` the next slot.
    const grid = BigInt(f.emulator.now());
    const lastInTime =
      closesAt - ((((closesAt - grid) % 1_000n) + 1_000n) % 1_000n);
    const firstLate = lastInTime + 1_000n;
    expect(lastInTime - 1n).toBeLessThan(closesAt);
    expect(firstLate - 1n).toBeGreaterThanOrEqual(closesAt);
    // Two exact fundings (20 s each) and the builds fit before the window.
    expect(grid).toBeLessThan(lastInTime - 100_000n);
    f.advanceToMs(lastInTime - 100_000n);
    const late = await openAvailability(f, attested, { validTo: firstLate });
    await assertAvailabilityRefusal(
      late.build(),
      {
        purpose: "withdraw",
        script: "availability-challenge open withdrawal",
        // The Open's only withdrawal.
        index: 0,
      },
      f.scriptNames,
    );
    // The SDK refuses the same range before building.
    expect(() =>
      SDK.assertDaAvailabilityOpenWithinChallengeWindow({
        validTo: firstLate,
        nodeEndTime: endTime,
        daChallengeWindowMs: f.timing.daChallengeWindowMs,
      }),
    ).toThrow(expect.objectContaining({ reason: "challenge-window-closed" }));
    const d = availabilityDeployment(f);
    expect(
      (
        await refusalOf(
          SDK.buildOpenDaAvailabilityChallengeTxProgram(f.lucid, d, {
            collateralInputs: await f.collateralInputs(),
            feeLovelace: parameters.max_open_fee_lovelace,
            validFrom: BigInt(f.emulator.now()),
            validTo: firstLate,
            commitment: attested.commitment,
            queue: attested.queue,
            challengerFunding: late.funding,
            challenger: f.challengerKey,
            daChallengeWindowMs: f.timing.daChallengeWindowMs,
          }),
        )
      ).reason,
    ).toBe("challenge-window-closed");
    // Control: the last slot before the deadline lands.
    const inTime = await openAvailability(f, attested, {
      validTo: lastInTime,
    });
    const opened = await inTime.submit();
    expect(nodeOf(opened.queue).da_attestation).toEqual({
      Challenged: {
        commitment_hash: attested.commitmentHash,
        challenge_asset_name: inTime.plan.challengeAssetName,
      },
    });
  }, 180_000);

  it("refuses an Open whose commitment does not hash to the node's commitment_hash", async () => {
    const f = await createAvailabilityFixture(1);
    const d = availabilityDeployment(f);
    const attested = await attestAvailability(f);
    const [first, ...rest] = attested.commitment.tranche_descriptors;
    const substituted: SDK.DaAvailabilityCommitment = {
      ...attested.commitment,
      tranche_descriptors: [
        { ...first!, chunk_commitment: "00".repeat(32) },
        ...rest,
      ],
    };
    expect(SDK.daAvailabilityCommitmentHash(substituted)).not.toBe(
      attested.commitmentHash,
    );
    const open = await openAvailability(f, attested);
    // Low level: the record and the Challenged node carry the substituted
    // commitment, bypassing the SDK pre-check.
    await assertAvailabilityRefusal(
      open.build({ commitment: substituted }),
      {
        purpose: "withdraw",
        script: "availability-challenge open withdrawal",
        // The Open's only withdrawal.
        index: 0,
      },
      f.scriptNames,
    );
    // The SDK refuses the same commitment before building.
    expect(
      (
        await refusalOf(
          SDK.buildOpenDaAvailabilityChallengeTxProgram(f.lucid, d, {
            ...(await resources(f, parameters.max_open_fee_lovelace)),
            commitment: substituted,
            queue: attested.queue,
            challengerFunding: open.funding,
            challenger: f.challengerKey,
            daChallengeWindowMs: f.timing.daChallengeWindowMs,
          }),
        )
      ).reason,
    ).toBe("commitment-hash-mismatch");
    // Control: the attested preimage lands.
    const opened = await open.submit();
    expect(nodeOf(opened.queue).da_attestation).toEqual({
      Challenged: {
        commitment_hash: attested.commitmentHash,
        challenge_asset_name: open.plan.challengeAssetName,
      },
    });
  }, 180_000);

  /**
   * B4/G4: the pool's Slash binds to the timeout step alone. A descendant-
   * first timeout leaves the correction lock Locked; the head-removal resume
   * step then carries the same state-queue redeemer
   * (`RemoveUnavailableBlockAfterTimeout`), so only the lock's `Idle` check
   * keeps a second slash out of it.
   */
  it("refuses a pool Slash inside a Locked correction-lock resume step", async () => {
    const f = await createAvailabilityFixture(1, 1);
    const d = availabilityDeployment(f);
    let s = await attestAndOpen(f, d);
    const b = recordOf(s);
    const terminal = await expireAndSettle(f, d, s);
    await submitBuilt(
      f,
      await Effect.runPromise(
        SDK.buildTimeoutDaAvailabilityChallengeTxProgram(f.lucid, d, {
          ...(await timeoutParams(f, s, terminal, await f.getPool())),
          descendant: s.descendant!.utxo,
        }),
      ),
    );
    s = await snapshot(f, d);
    expect(s.descendant).toBeUndefined();
    expect(s.queue).toBeDefined();
    const lock = Data.from(s.correctionLock.datum!, SDK.CorrectionLockDatum);
    expect(lock).toEqual({
      Locked: {
        target_header_hash: f.target.headerHash,
        correction_identity: {
          AvailabilityChallenge: {
            challenge_asset_name: b.challenge_asset_name,
          },
        },
      },
    });
    const pool = await f.getPool();
    expect(backingOf(pool)).toBeGreaterThan(0n);
    const collateral = await f.collateralInputs();
    const feeFunding = (await f.lucid.wallet().getUtxos()).find(
      (u) =>
        Object.keys(u.assets).length === 1 &&
        u.assets.lovelace > 10_000_000n &&
        !collateral.some(
          (c) => c.txHash === u.txHash && c.outputIndex === u.outputIndex,
        ),
    )!;
    await assertAvailabilityRefusal(
      buildLockedHeadResume(f, s, b.challenge_asset_name, feeFunding, pool),
      {
        purpose: "spend",
        script: "da-bond-pool",
        // Spend redeemers follow the sorted inputs.
        index: Number(
          position(
            [
              s.queue!.utxo,
              s.confirmedState.utxo,
              s.correctionLock,
              feeFunding,
              pool,
            ],
            pool,
          ),
        ),
      },
      f.scriptNames,
    );
    // Control: the same resume step without the pool lands.
    const outputs = await f.submit(
      "resume head removal on the Locked lock",
      buildLockedHeadResume(f, s, b.challenge_asset_name, feeFunding),
    );
    expect(outputs[1]!.datum).toBe(Data.to("Idle", SDK.CorrectionLockDatum));
    const removed = await snapshot(f, d);
    expect(removed.queue).toBeUndefined();
    // The pool kept what the timeout left.
    expect((await f.getPool()).assets).toEqual(pool.assets);
  }, 240_000);
});
