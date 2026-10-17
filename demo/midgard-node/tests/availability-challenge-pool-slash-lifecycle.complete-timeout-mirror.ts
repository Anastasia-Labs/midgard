import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type RedeemerContext,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  inline,
  recordOf,
  timeoutYield,
  unavailableTimeoutYield,
} from "./availability-challenge-pool-slash-lifecycle.submit-built.js";
import { TEST_AVAILABILITY_PARAMETERS as parameters } from "./helpers/availability-challenge.js";
import {
  type AvailabilityFixture,
  inputAt,
  mintAt,
  referenceAt,
} from "./helpers/availability-challenge-emulator.js";

/** The challenger output and, when present, the pool's continuing output. */
export type MirrorArithmetic = {
  readonly challengerOutputLovelace: bigint;
} & (
  | { readonly pool: UTxO; readonly poolOutputLovelace: bigint }
  | { readonly pool?: undefined }
);

/** The Idle lock's `Correct` spend, reading the hub oracle. */
const correct =
  (f: AvailabilityFixture): BuildTxWithRedeemer =>
  (ctx) =>
    Data.to(
      {
        Correct: {
          hub_oracle_ref_input_index: referenceAt(ctx, f.hubOracleRefInput),
        },
      },
      SDK.CorrectionLockRedeemer,
    );

/** The head removal: the continued root is output 0. */
const removeHead =
  (
    f: AvailabilityFixture,
    challengeAssetName: string,
    root: UTxO,
  ): BuildTxWithRedeemer =>
  (ctx) =>
    Data.to(
      {
        RemoveUnavailableBlockAfterTimeout: {
          yield_to_ref_input_index: referenceAt(
            ctx,
            f.reference("state-queue unavailable-timeout withdrawal"),
          ),
          unavailable_header_hash: f.target.headerHash,
          challenge_asset_name: challengeAssetName,
          removal_approach: {
            RemoveTimedOutHead: {
              confirmed_state_input_outref: SDK.outputReferenceFromUTxO(root),
              confirmed_state_output_index: 0n,
            },
          },
        },
      },
      SDK.StateQueueRedeemer,
    );

/** The pool's `Slash` beside the head removal: the pool continues at 3. */
const slash =
  (f: AvailabilityFixture, lock: UTxO): BuildTxWithRedeemer =>
  (ctx) =>
    Data.to(
      {
        Slash: {
          hub_oracle_ref_input_index: referenceAt(ctx, f.hubOracleRefInput),
          state_queue_mint_redeemer_index: mintAt(
            ctx,
            f.contracts.stateQueue.policyId,
          ),
          correction_lock_input_index: inputAt(ctx, lock),
          output_index: 3n,
        },
      },
      SDK.DaBondPoolSpendRedeemer,
    );

/**
 * A hand-built mirror of the timeout (record, terminal, head, root, Idle lock,
 * pool) with free arithmetic, so a negative can break exactly one relation
 * the production builder never lets through. Outputs: root (0), Idle lock
 * (1), the ONE challenger output (2), the pool (3, when present), rent (4),
 * then whatever a composed transaction pays. Its redeemer indices resolve
 * against the final transaction, so another builder can be composed after
 * it. The caller pins the fee where it completes.
 */
export const buildTimeoutMirror = (
  f: AvailabilityFixture,
  s: SDK.DaAvailabilityChallengeSnapshot,
  terminal: UTxO,
  arithmetic: MirrorArithmetic,
): TxBuilder => {
  const record = s.record!;
  const queue = s.queue!.utxo;
  const root = s.confirmedState.utxo;
  const lock = s.correctionLock;
  const { pool } = arithmetic;
  const challengeAssetName = recordOf(s).challenge_asset_name;
  const ap = f.contracts.availabilityChallenge.policyId;
  const qp = f.contracts.stateQueue.policyId;
  const references = [
    f.hubOracleRefInput,
    f.reference("state-queue spending"),
    f.reference("state-queue minting"),
    f.reference("state-queue unavailable-timeout withdrawal"),
    f.reference("correction-lock spending"),
    f.reference("availability-challenge spending"),
    f.reference("availability-challenge minting"),
    f.reference("availability-challenge timeout withdrawal"),
    ...(pool ? [f.reference("da-bond-pool spending")] : []),
  ];
  const lower = BigInt(f.emulator.now());
  const coordinate = (ctx: RedeemerContext) =>
    Data.to(
      { Coordinate: { mint_redeemer_index: mintAt(ctx, ap) } },
      SDK.DaAvailabilitySpendRedeemer,
    );
  let tx = f.lucid
    .newTx()
    .validFrom(Number(lower))
    .validTo(Number(lower + 60_000n))
    .collectFrom([record, terminal], coordinate)
    .collectFrom(
      [queue, root],
      Data.to("LinkedListMutation", SDK.StateQueueSpendRedeemer),
    )
    .collectFrom([lock], correct(f))
    .readFrom(references)
    .mintAssets(
      {
        [ap + challengeAssetName]: -1n,
        [ap +
        SDK.daAvailabilityTerminalAccumulatorAssetName(challengeAssetName)]:
          -1n,
      },
      (ctx) =>
        Data.to(
          {
            TimeoutChallenge: {
              yield_to_ref_input_index: referenceAt(
                ctx,
                f.reference("availability-challenge timeout withdrawal"),
              ),
              hub_oracle_ref_input_index: referenceAt(ctx, f.hubOracleRefInput),
              record_input_index: inputAt(ctx, record),
              terminal_accumulator_input_index: inputAt(ctx, terminal),
              state_queue_mint_redeemer_index: mintAt(ctx, qp),
              // Without a pool the index names the (unauthentic) lock input.
              pool_input_index: inputAt(ctx, pool ?? lock),
              pool_output_index: 3n,
              challenger_refund_output_index: 2n,
            },
          },
          SDK.DaAvailabilityMintRedeemer,
        ),
    )
    .mintAssets({ [f.queueUnit]: -1n }, removeHead(f, challengeAssetName, root))
    .pay.ToContract(
      root.address,
      inline(
        SDK.encodeLinkedListNodeView({
          ...s.confirmedState.datum,
          next: s.queue!.datum.next,
        }),
      ),
      root.assets,
    )
    .pay.ToContract(
      lock.address,
      inline(Data.to("Idle", SDK.CorrectionLockDatum)),
      lock.assets,
    )
    .pay.ToAddress(f.challenger.address, {
      lovelace: arithmetic.challengerOutputLovelace,
    });
  if (arithmetic.pool)
    tx = tx
      .collectFrom([arithmetic.pool], slash(f, lock))
      .pay.ToContract(arithmetic.pool.address, inline(arithmetic.pool.datum!), {
        lovelace: arithmetic.poolOutputLovelace,
        [f.poolUnit]: 1n,
      });
  return tx.pay
    .ToAddress(f.responder.address, { lovelace: queue.assets.lovelace })
    .withdraw(unavailableTimeoutYield(f), 0n, Data.void())
    .withdraw(timeoutYield(f), 0n, Data.void());
};

/**
 * The mirror alone. The fee is exactly `feeLovelace`: the inputs pay the
 * outputs and it, and no change output exists, so it completes here with coin
 * selection off.
 */
export const completeTimeoutMirror = (
  f: AvailabilityFixture,
  s: SDK.DaAvailabilityChallengeSnapshot,
  terminal: UTxO,
  arithmetic: MirrorArithmetic & { readonly feeLovelace: bigint },
): ReturnType<TxBuilder["complete"]> =>
  buildTimeoutMirror(f, s, terminal, arithmetic)
    .setMinFee(arithmetic.feeLovelace)
    .complete({ coinSelection: false, localUPLCEval: true });

/**
 * A hand-built resume step that removes the head on the `Locked` lock a
 * descendant-first timeout left behind (the SDK's `RemoveTimedOutHead`
 * continuation), optionally with the pool's `Slash` smuggled in. Outputs:
 * root (0), Idle lock (1), rent (2), the slashed pool (3, when present), then
 * the fee input's change.
 */
export const buildLockedHeadResume = (
  f: AvailabilityFixture,
  s: SDK.DaAvailabilityChallengeSnapshot,
  challengeAssetName: string,
  feeFunding: UTxO,
  pool?: UTxO,
): TxBuilder => {
  const queue = s.queue!.utxo;
  const root = s.confirmedState.utxo;
  const lock = s.correctionLock;
  const references = [
    f.hubOracleRefInput,
    f.reference("state-queue spending"),
    f.reference("state-queue minting"),
    f.reference("state-queue unavailable-timeout withdrawal"),
    f.reference("correction-lock spending"),
    ...(pool ? [f.reference("da-bond-pool spending")] : []),
  ];
  const lower = BigInt(f.emulator.now());
  let tx = f.lucid
    .newTx()
    .validFrom(Number(lower))
    .validTo(Number(lower + 60_000n))
    .collectFrom([feeFunding])
    .collectFrom(
      [queue, root],
      Data.to("LinkedListMutation", SDK.StateQueueSpendRedeemer),
    )
    .collectFrom([lock], correct(f))
    .readFrom(references)
    .mintAssets({ [f.queueUnit]: -1n }, removeHead(f, challengeAssetName, root))
    .pay.ToContract(
      root.address,
      inline(
        SDK.encodeLinkedListNodeView({
          ...s.confirmedState.datum,
          next: s.queue!.datum.next,
        }),
      ),
      root.assets,
    )
    .pay.ToContract(
      lock.address,
      inline(Data.to("Idle", SDK.CorrectionLockDatum)),
      lock.assets,
    )
    .pay.ToAddress(f.responder.address, { lovelace: queue.assets.lovelace });
  if (pool) {
    const { poolOutputLovelace } = SDK.planDaBondPoolSlash({
      poolLovelace: pool.assets.lovelace,
      parameters,
    });
    tx = tx
      .collectFrom([pool], slash(f, lock))
      .pay.ToContract(pool.address, inline(pool.datum!), {
        lovelace: poolOutputLovelace,
        [f.poolUnit]: 1n,
      });
  }
  return tx.withdraw(unavailableTimeoutYield(f), 0n, Data.void());
};
