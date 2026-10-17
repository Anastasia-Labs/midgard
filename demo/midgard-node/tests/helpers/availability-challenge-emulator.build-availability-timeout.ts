import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { coordinate } from "./availability-challenge-emulator.attest-availability.js";
import { type AvailabilityFixtureOptions } from "./availability-challenge-emulator.availability-redeemer-script.js";
import { createFixture } from "./availability-challenge-emulator.create-fixture.js";
import {
  type AvailabilityLayout,
  index,
  inline,
  mintIndex,
  outRef,
  refIndex,
} from "./availability-challenge-emulator.measure-availability-transaction.js";
import {
  type AvailabilityFixture,
  type OpenAvailability,
  yieldTx,
} from "./availability-challenge-emulator.open-availability.js";

/**
 * `TimeoutChallenge` with the head removal and the pool's `Slash` in one
 * transaction (hand-built mirror of the SDK builder). Outputs: the continued
 * root (0), the Idle correction lock (1), the ONE challenger output
 * `remaining - c + challenge_record + payout` (2), the pool with
 * `pool - taken` beside its NFT and its datum unchanged (3), the removed
 * node's rent (4). The fee is exactly `feePart + c`, `feePart =
 * min(penalty, taken)`.
 */
export const buildAvailabilityTimeout = async (
  f: AvailabilityFixture,
  open: OpenAvailability,
  record: UTxO,
  queue: UTxO,
  terminal: UTxO,
  options: {
    early?: boolean;
    /** Pays the challenger output to the responder instead. */
    redirectPayout?: boolean;
    /** The challenger's fee contribution `c` (default 0). */
    challengerFeeLovelace?: bigint;
    pool?: UTxO;
  } = {},
) => {
  const pool = options.pool ?? (await f.getPool());
  const slash = SDK.planDaBondPoolSlash({
    poolLovelace: pool.assets.lovelace,
    parameters: f.parameters,
  });
  const terminalDatum = Data.from(
    terminal.datum!,
    SDK.DaAvailabilityTerminalAccumulatorDatum,
  );
  const plan = SDK.planDaAvailabilityTimeout({
    poolLovelace: pool.assets.lovelace,
    remainingChallengerLovelace: terminalDatum.remaining_challenger_lovelace,
    challengerFeeLovelace: options.challengerFeeLovelace ?? 0n,
    parameters: f.parameters,
  });
  expect(plan.feePart).toBe(slash.feePart);
  // Exact: the inputs pay the outputs and `feePart + c`, completed without
  // coin selection by `f.submit`, so no change output exists.
  const fee = plan.feeLovelace;
  const lower = options.early
    ? open.plan.responseDeadline - 1_000n
    : BigInt(f.emulator.now());
  const queuePolicy = f.contracts.stateQueue.policyId;
  const ctx: AvailabilityLayout = {
    inputs: [record, terminal, queue, f.rootUtxo, f.correctionLockUtxo, pool],
    policies: [open.policy, queuePolicy],
    references: [
      f.hubOracleRefInput,
      f.reference("state-queue spending"),
      f.reference("state-queue minting"),
      f.reference("state-queue unavailable-timeout withdrawal"),
      f.reference("correction-lock spending"),
      f.reference("availability-challenge spending"),
      f.reference("availability-challenge minting"),
      f.reference("availability-challenge timeout withdrawal"),
      f.reference("da-bond-pool spending"),
    ],
  };
  const rootDatum = SDK.encodeLinkedListNodeView({
    ...f.rootDatum,
    next: "Empty",
  });
  let tx = f.lucid
    .newTx()
    .setMinFee(fee)
    .validFrom(Number(lower))
    .validTo(Number(lower + 60_000n))
    .collectFrom([record, terminal], coordinate(ctx, open.policy))
    .collectFrom(
      [queue, f.rootUtxo],
      Data.to("LinkedListMutation", SDK.StateQueueSpendRedeemer),
    )
    .collectFrom(
      [f.correctionLockUtxo],
      Data.to(
        {
          Correct: {
            hub_oracle_ref_input_index: refIndex(ctx, f.hubOracleRefInput),
          },
        },
        SDK.CorrectionLockRedeemer,
      ),
    )
    .collectFrom(
      [pool],
      Data.to(
        {
          Slash: {
            hub_oracle_ref_input_index: refIndex(ctx, f.hubOracleRefInput),
            state_queue_mint_redeemer_index: mintIndex(ctx, queuePolicy),
            correction_lock_input_index: index(ctx, f.correctionLockUtxo),
            output_index: 3n,
          },
        },
        SDK.DaBondPoolSpendRedeemer,
      ),
    )
    .readFrom([...ctx.references])
    .mintAssets(
      {
        [open.policy + open.plan.challengeAssetName]: -1n,
        [open.terminalUnit]: -1n,
      },
      Data.to(
        {
          TimeoutChallenge: {
            yield_to_ref_input_index: refIndex(
              ctx,
              f.reference("availability-challenge timeout withdrawal"),
            ),
            hub_oracle_ref_input_index: refIndex(ctx, f.hubOracleRefInput),
            record_input_index: index(ctx, record),
            terminal_accumulator_input_index: index(ctx, terminal),
            state_queue_mint_redeemer_index: mintIndex(ctx, queuePolicy),
            pool_input_index: index(ctx, pool),
            pool_output_index: 3n,
            challenger_refund_output_index: 2n,
          },
        },
        SDK.DaAvailabilityMintRedeemer,
      ),
    )
    .mintAssets(
      { [f.queueUnit]: -1n },
      Data.to(
        {
          RemoveUnavailableBlockAfterTimeout: {
            yield_to_ref_input_index: refIndex(
              ctx,
              f.reference("state-queue unavailable-timeout withdrawal"),
            ),
            unavailable_header_hash: f.target.headerHash,
            challenge_asset_name: open.plan.challengeAssetName,
            removal_approach: {
              RemoveTimedOutHead: {
                confirmed_state_input_outref: outRef(f.rootUtxo),
                confirmed_state_output_index: 0n,
              },
            },
          },
        },
        SDK.StateQueueRedeemer,
      ),
    )
    .pay.ToContract(f.rootUtxo.address, inline(rootDatum), f.rootUtxo.assets)
    .pay.ToContract(
      f.correctionLockUtxo.address,
      inline(Data.to("Idle", SDK.CorrectionLockDatum)),
      f.correctionLockUtxo.assets,
    )
    .pay.ToAddress(
      options.redirectPayout ? f.responder.address : f.challenger.address,
      { lovelace: plan.challengerOutputLovelace },
    )
    .pay.ToContract(pool.address, inline(pool.datum!), {
      lovelace: plan.poolOutputLovelace,
      [f.poolUnit]: 1n,
    })
    .pay.ToAddress(f.responder.address, { lovelace: queue.assets.lovelace })
    .withdraw(
      SDK.scriptRewardAddress(
        "Preprod",
        f.contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
      ),
      0n,
      Data.void(),
    );
  tx = yieldTx(f, tx, "timeout");
  return { tx, plan, pool };
};

export const advanceAvailabilityDeadline = (
  f: AvailabilityFixture,
  open: OpenAvailability,
) => {
  const slots = Math.max(
    1,
    Math.ceil((Number(open.plan.responseDeadline) - f.emulator.now()) / 1_000) +
      1,
  );
  f.emulator.awaitSlot(slots);
};

// ---------------------------------------------------------------------------
// Commit fixture: an empty queue that real `CommitBlockHeader`s fill
// ---------------------------------------------------------------------------

/**
 * A fixture whose state queue starts empty. It has no `target`: every block
 * is committed by `commitAvailabilityBlock`, which returns an
 * `AvailabilityFixture` for that block, so the attestation, challenge and
 * timeout helpers above run against it unchanged.
 */
export type AvailabilityCommitFixture = Omit<
  AvailabilityFixture,
  "target" | "payload" | "commitment" | "queueUnit"
>;

/**
 * The genesis fixture with an empty state queue, plus what a real commit
 * needs: the scheduler naming the responder as the active operator, the
 * responder's active-operator node, the `state-queue commit withdrawal` and
 * `active-operators spending` reference scripts, and the registered commit
 * yield.
 */
export const createAvailabilityCommitFixture = async (
  options: AvailabilityFixtureOptions = {},
): Promise<AvailabilityCommitFixture> => {
  const {
    target: _target,
    payload: _payload,
    commitment: _commitment,
    queueUnit: _queueUnit,
    ...fixture
  } = await createFixture(14_021, 0, 0, options, true);
  return fixture;
};

/** One state-queue element, as the queue's linked list holds it. */
type AvailabilityQueueElement = {
  readonly utxo: UTxO;
  readonly view: SDK.LinkedListNodeView;
  readonly assetName: string;
};

export const availabilityQueueElements = async (
  f: AvailabilityCommitFixture,
): Promise<AvailabilityQueueElement[]> => {
  const policy = f.contracts.stateQueue.policyId;
  const elements: AvailabilityQueueElement[] = [];
  for (const utxo of await f.lucid.utxosAt(
    f.contracts.stateQueue.spendingScriptAddress,
  )) {
    const unit = Object.keys(utxo.assets).find(
      (candidate) => candidate !== "lovelace" && candidate.startsWith(policy),
    );
    if (unit === undefined) continue;
    elements.push({
      utxo,
      view: await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(utxo)),
      assetName: unit.slice(policy.length),
    });
  }
  return elements;
};

/** The live root and correction lock. */
export const liveAvailabilityAnchors = async (f: AvailabilityCommitFixture) => {
  const [rootUtxo] = await f.lucid.utxosAtWithUnit(
    f.contracts.stateQueue.spendingScriptAddress,
    f.rootUnit,
  );
  const [correctionLockUtxo] = await f.lucid.utxosAtWithUnit(
    f.contracts.correctionLock.spendingScriptAddress,
    SDK.correctionLockUnit(f.contracts.hubOracle.policyId),
  );
  if (!rootUtxo || !correctionLockUtxo)
    throw new Error("The state-queue root or the correction lock is missing");
  return {
    rootUtxo,
    rootDatum: await Effect.runPromise(
      SDK.getLinkedListNodeViewFromUTxO(rootUtxo),
    ),
    correctionLockUtxo,
  };
};

/** The block's live queue node as an attestation target, if still queued. */
export const liveAvailabilityTarget = async (
  f: AvailabilityFixture,
): Promise<SDK.DaAttestationStateQueueTarget | undefined> => {
  const [utxo] = await f.lucid.utxosAtWithUnit(
    f.contracts.stateQueue.spendingScriptAddress,
    f.queueUnit,
  );
  if (utxo === undefined) return undefined;
  const datum = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(utxo),
  );
  return {
    headerHash: f.target.headerHash,
    stateQueueNode: Data.castFrom(datum.data, SDK.StateQueueNode),
    stateQueueUtxo: {
      utxo,
      datum,
      assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + f.target.headerHash,
    },
  };
};
