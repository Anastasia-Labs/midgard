import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  inline,
  type Layout,
  type Measurement,
  nodeOf,
  refIndex,
} from "./availability-challenge-pool-slash-lifecycle.submit-built.js";
import { TEST_AVAILABILITY_PARAMETERS as parameters } from "./helpers/availability-challenge.js";
import {
  AVAILABILITY_PROFILE,
  type AvailabilityFixture,
} from "./helpers/availability-challenge-emulator.js";

/**
 * A hand-built merge of the queue head into the confirmed state. The fixture
 * block carries no L2 material, so `merge_to_confirmed_state` requires
 * `m_settlement_redeemer_index == None` and spawns no settlement (the SDK
 * merge builder always spawns one). Outputs: the continued root (0), then the
 * wallet's change, which takes the merged node's lovelace.
 */
export const buildEmptyBlockMerge = async (
  f: AvailabilityFixture,
  queue: UTxO,
) => {
  const [root] = await f.lucid.utxosAtWithUnit(
    f.contracts.stateQueue.spendingScriptAddress,
    f.rootUnit,
  );
  if (!root) throw new Error("Missing confirmed-state root");
  const rootView = Effect.runSync(SDK.getLinkedListNodeViewFromUTxO(root));
  const queueView = Effect.runSync(SDK.getLinkedListNodeViewFromUTxO(queue));
  const header = nodeOf(queue).header;
  const confirmed = Data.castFrom(rootView.data, SDK.ConfirmedState);
  const continued: SDK.ConfirmedState = {
    headerHash: f.target.headerHash,
    prevHeaderHash: confirmed.headerHash,
    utxoRoot: header.utxosRoot,
    startTime: confirmed.startTime,
    endTime: header.endTime,
    protocolVersion: header.protocolVersion,
  };
  const continuedDatum = SDK.encodeLinkedListNodeView({
    ...rootView,
    next: queueView.next,
    data: SDK.castConfirmedStateToData(
      continued,
    ) as SDK.LinkedListNodeView["data"],
  });
  const mergeYield = f.reference("state-queue merge withdrawal");
  const layout: Layout = {
    inputs: [root, queue],
    policies: [f.contracts.stateQueue.policyId],
    references: [
      f.hubOracleRefInput,
      f.correctionLockUtxo,
      mergeYield,
      f.reference("state-queue spending"),
      f.reference("state-queue minting"),
    ],
  };
  const tx = f.lucid
    .newTx()
    .validFrom(f.emulator.now())
    .collectFrom(
      [root, queue],
      Data.to("LinkedListMutation", SDK.StateQueueSpendRedeemer),
    )
    .readFrom([...layout.references])
    .pay.ToContract(root.address, inline(continuedDatum), root.assets)
    .mintAssets(
      { [f.queueUnit]: -1n },
      Data.to(
        {
          MergeToConfirmedStateV1: {
            yield_to_ref_input_index: refIndex(layout, mergeYield),
            header_node_key: f.target.headerHash,
            confirmed_state_input_outref: SDK.outputReferenceFromUTxO(root),
            confirmed_state_output_index: 0n,
            m_settlement_redeemer_index: null,
            merged_block_withdrawals_root: header.withdrawalsRoot,
            merged_block_forced_transactions_root:
              header.forcedTransactionsRoot,
            merged_block_transactions_root: header.transactionsRoot,
            merged_block_deposits_root: header.depositsRoot,
            merged_block_transition_trace_root: header.transitionTraceRoot,
            merged_block_event_to_step_root: header.eventToStepRoot,
            merged_block_validation_traces_root: header.validationTracesRoot,
            merged_block_withdrawal_count: header.withdrawalCount,
            merged_block_forced_transaction_count:
              header.forcedTransactionCount,
            merged_block_l2_transaction_count: header.l2TransactionCount,
            merged_block_deposit_count: header.depositCount,
            merged_block_total_event_count: header.totalEventCount,
            merged_block_transition_step_count: header.transitionStepCount,
            merged_block_validation_trace_count: header.validationTraceCount,
          },
        },
        SDK.StateQueueRedeemer,
      ),
    )
    .withdraw(
      SDK.scriptRewardAddress(
        "Preprod",
        f.contracts.stateQueue.yields.merge.withdrawalScript,
      ),
      0n,
      Data.void(),
    );
  return { tx, root, continued, continuedDatum };
};

/** Advances the emulator to the block's merge maturity. */
export const advanceToMaturity = (f: AvailabilityFixture, queue: UTxO) => {
  const matureAt =
    nodeOf(queue).header.endTime +
    BigInt(AVAILABILITY_PROFILE.timing.block_maturity_ms);
  f.advanceToMs(matureAt);
  expect(BigInt(f.emulator.now())).toBeGreaterThanOrEqual(matureAt);
};

/**
 * Checks a landed production timeout: the exact fee `feePart + c`, the ONE
 * merged challenger output, the pool continuing with `pool_in - taken` and its
 * datum, and no change output.
 */
export const expectTimeoutArithmetic = (
  f: AvailabilityFixture,
  built: SDK.BuiltDaAvailabilityTransaction,
  landed: { outputs: UTxO[]; measurement: Measurement },
  legs: { pool: UTxO; terminal: UTxO },
  expected: { taken: bigint; feePart: bigint; payout: bigint },
) => {
  const remaining = Data.from(
    legs.terminal.datum!,
    SDK.DaAvailabilityTerminalAccumulatorDatum,
  ).remaining_challenger_lovelace;
  const slash = SDK.planDaBondPoolSlash({
    poolLovelace: legs.pool.assets.lovelace,
    parameters,
  });
  expect({
    taken: slash.taken,
    feePart: slash.feePart,
    payout: slash.payout,
  }).toEqual(expected);
  // The builder's planned challenger fee `c`, from its own fee split.
  const c = built.feeLovelace - built.timeoutFeePartLovelace!;
  expect(landed.measurement.fee).toBe(expected.feePart + c);
  expect(built.timeoutFeePartLovelace).toBe(expected.feePart);
  expect(c).toBeGreaterThanOrEqual(0n);
  expect(c).toBeLessThanOrEqual(parameters.max_timeout_fee_lovelace);
  // No change output: root, lock, challenger, pool, rent and nothing else.
  expect(landed.measurement.outputs).toBe(5);
  expect(built.expectedOutputs).toHaveLength(5);
  const [root, lock, challenger, pool, rent] = landed.outputs;
  expect(root!.assets).toEqual(f.rootUtxo.assets);
  expect(lock!.datum).toBe(Data.to("Idle", SDK.CorrectionLockDatum));
  expect(challenger!.address).toBe(f.challenger.address);
  expect(challenger!.assets).toEqual({
    lovelace:
      remaining - c + parameters.challenge_record_lovelace + expected.payout,
  });
  expect(
    landed.outputs.filter((u) => u.address === f.challenger.address),
  ).toHaveLength(1);
  expect(pool!.address).toBe(legs.pool.address);
  expect(pool!.assets).toEqual({
    ...legs.pool.assets,
    lovelace: legs.pool.assets.lovelace - expected.taken,
  });
  expect(pool!.datum).toBe(legs.pool.datum);
  expect(rent!.address).toBe(f.responder.address);
  return c;
};
