/**
 * Pooled DA bond (#689): the availability challenge lifecycle against the ONE
 * pooled committee bond, from attestation through close or timeout, with the
 * pool's `Slash` taking `min(da_bond, backing)` in the timeout itself.
 *
 * Every refusal below is paired with its honest control and names the script,
 * purpose (and, where the ledger order is fixed, the index) that refuses it.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type TxBuilder, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { TEST_AVAILABILITY_PARAMETERS as parameters } from "./helpers/availability-challenge.js";
import {
  assertAvailabilityRefusal,
  attestAvailability,
  AVAILABILITY_DEFAULT_POOL_LOVELACE,
  AVAILABILITY_EMULATOR_PARAMETERS,
  AVAILABILITY_PROFILE,
  availabilityDeployment,
  type AvailabilityFixture,
  createAvailabilityFixture,
  openAvailability,
} from "./helpers/availability-challenge-emulator.js";

// ---------------------------------------------------------------------------
// File-local helpers.
// ---------------------------------------------------------------------------

/** H12: the ledger and budget bounds every submitted transaction meets. */
const MAX_SIGNED_BYTES = 15_872;
const MAX_MEMORY = 13_200_000n;
const MAX_STEPS = 8_000_000_000n;

type Measurement = {
  readonly signedBytes: number;
  readonly memory: bigint;
  readonly steps: bigint;
  readonly fee: bigint;
  readonly outputs: number;
};

const measure = (cbor: string): Measurement => {
  const tx = CML.Transaction.from_cbor_hex(cbor);
  const redeemers = tx.witness_set().redeemers()?.to_flat_format();
  let memory = 0n;
  let steps = 0n;
  for (let i = 0; i < (redeemers?.len() ?? 0); i++) {
    memory += redeemers!.get(i).ex_units().mem();
    steps += redeemers!.get(i).ex_units().steps();
  }
  return {
    signedBytes: cbor.length / 2,
    memory,
    steps,
    fee: tx.body().fee(),
    outputs: tx.body().outputs().len(),
  };
};

/**
 * Signs and submits an SDK-built availability transaction after checking the
 * H12 bounds and the G9/H1 collateral rule: one to three plain-ADA coins
 * covering 150% of the exact fee.
 */
const submitBuilt = async (
  f: AvailabilityFixture,
  built: SDK.BuiltDaAvailabilityTransaction,
) => {
  const signed = await built.tx.sign.withWallet().complete();
  const measurement = measure(signed.toCBOR());
  expect(measurement.signedBytes).toBeLessThanOrEqual(MAX_SIGNED_BYTES);
  expect(measurement.memory).toBeGreaterThan(0n);
  expect(measurement.memory).toBeLessThanOrEqual(MAX_MEMORY);
  expect(measurement.steps).toBeLessThanOrEqual(MAX_STEPS);
  expect(measurement.fee).toBe(built.feeLovelace);
  expect(signed.toHash()).toBe(built.txId);
  expect(built.collateralOutRefs.length).toBeGreaterThanOrEqual(1);
  expect(built.collateralOutRefs.length).toBeLessThanOrEqual(3);
  expect(
    built.collateralOutRefs.reduce((t, u) => t + u.assets.lovelace, 0n),
  ).toBeGreaterThanOrEqual((built.feeLovelace * 150n + 99n) / 100n);
  for (const c of built.collateralOutRefs)
    expect(Object.keys(c.assets)).toEqual(["lovelace"]);
  const id = await signed.submit();
  f.emulator.awaitBlock(1);
  const outputs = await f.lucid.utxosByOutRef(
    built.expectedOutputs.map((_, outputIndex) => ({
      txHash: id,
      outputIndex,
    })),
  );
  return { outputs, measurement };
};

const resources = async (
  f: AvailabilityFixture,
  feeLovelace: bigint,
  /** A publication's range must close by the challenge's response deadline. */
  responseDeadline?: bigint,
): Promise<SDK.DaAvailabilityTransactionResources> => {
  const collateralInputs = await f.collateralInputs();
  const validFrom = BigInt(f.emulator.now());
  const validTo =
    responseDeadline !== undefined &&
    responseDeadline + 1n < validFrom + 60_000n
      ? responseDeadline + 1n
      : validFrom + 60_000n;
  return { collateralInputs, feeLovelace, validFrom, validTo };
};

/** The timeout derives its own exact fee: resources without one. */
const timeoutResources = async (f: AvailabilityFixture) => {
  const { feeLovelace: _fee, ...rest } = await resources(f, 1n);
  return rest;
};

const OPEN_FUNDING_LOVELACE =
  parameters.challenger_bond_lovelace +
  parameters.challenge_record_lovelace +
  parameters.max_open_fee_lovelace;

/** Selects the challenger's wallet and funds one exact Open input. */
const fundChallenger = async (f: AvailabilityFixture) => {
  f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
  const funding = await f.submit(
    "prepare challenger funding",
    f.lucid.newTx().pay.ToAddress(f.challenger.address, {
      lovelace: OPEN_FUNDING_LOVELACE,
    }),
    true,
  );
  return funding.find((u) => u.assets.lovelace === OPEN_FUNDING_LOVELACE)!;
};

const snapshot = (f: AvailabilityFixture, d: SDK.DaAvailabilityDeployment) =>
  SDK.fetchDaAvailabilityChallengeSnapshot(f.lucid, d, f.target.headerHash);

const recordOf = (s: SDK.DaAvailabilityChallengeSnapshot) => {
  if (!s.recordDatum) throw new Error("Expected a challenge record");
  return s.recordDatum;
};

const nodeOf = (queue: UTxO) =>
  Data.castFrom(
    Effect.runSync(SDK.getLinkedListNodeViewFromUTxO(queue)).data,
    SDK.StateQueueNode,
  );

/** Attests the fixture block and opens a challenge with the SDK builder. */
const attestAndOpen = async (
  f: AvailabilityFixture,
  d: SDK.DaAvailabilityDeployment,
) => {
  const attested = await attestAvailability(f);
  const challengerFunding = await fundChallenger(f);
  await submitBuilt(
    f,
    await Effect.runPromise(
      SDK.buildOpenDaAvailabilityChallengeTxProgram(f.lucid, d, {
        ...(await resources(f, parameters.max_open_fee_lovelace)),
        commitment: attested.commitment,
        queue: attested.queue,
        challengerFunding,
        challenger: f.challengerKey,
        daChallengeWindowMs: f.timing.daChallengeWindowMs,
      }),
    ),
  );
  return snapshot(f, d);
};

/** Lets every tranche expire unanswered and settles it: `has_timed_out`. */
const expireAndSettle = async (
  f: AvailabilityFixture,
  d: SDK.DaAvailabilityDeployment,
  s: SDK.DaAvailabilityChallengeSnapshot,
) => {
  f.advanceToMs(recordOf(s).response_deadline + 1_000n);
  f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
  const { outputs } = await submitBuilt(
    f,
    await Effect.runPromise(
      SDK.buildSettleDaAvailabilityTrancheTxProgram(f.lucid, d, {
        ...(await resources(f, parameters.max_settlement_fee_lovelace)),
        record: s.record!,
        terminal: s.terminal!,
        thread: s.tranches[0]!.utxo,
      }),
    ),
  );
  return outputs[0]!;
};

/** The production timeout builder's parameters for the head-only fixture. */
const timeoutParams = async (
  f: AvailabilityFixture,
  s: SDK.DaAvailabilityChallengeSnapshot,
  terminal: UTxO,
  pool: UTxO,
): Promise<SDK.TimeoutDaAvailabilityChallengeParams> => ({
  ...(await timeoutResources(f)),
  record: s.record!,
  terminal,
  pool,
  queue: s.queue!.utxo,
  confirmedState: s.confirmedState.utxo,
  correctionLock: s.correctionLock,
  headerHash: f.target.headerHash,
  challengeAssetName: recordOf(s).challenge_asset_name,
  rentRefundAddress: f.responder.address,
});

/** The SDK error a program fails with (never a success). */
const refusalOf = async <A>(
  program: Effect.Effect<A, SDK.DaAvailabilityTransactionError>,
): Promise<SDK.DaAvailabilityTransactionError> => {
  const result = await Effect.runPromise(Effect.either(program));
  if (result._tag === "Right")
    throw new Error("Expected the SDK builder to refuse, but it built");
  return result.left;
};

const backingOf = (pool: UTxO) =>
  SDK.daBondPoolBacking({ lovelace: pool.assets.lovelace, parameters });

/** The min-UTxO of a lone plain-ADA output to `address` (stabilised). */
const plainOutputMinAda = (address: string) => {
  let lovelace = 0n;
  for (let attempt = 0; attempt < 4; attempt++) {
    const required = CML.min_ada_required(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(address),
        CML.Value.from_coin(lovelace),
        undefined,
        undefined,
      ),
      AVAILABILITY_EMULATOR_PARAMETERS.coinsPerUtxoByte,
    );
    if (required <= lovelace) return lovelace;
    lovelace = required;
  }
  throw new Error("min-UTxO did not stabilise");
};

// --- Ledger-layout helpers for the hand-built mirrors ----------------------

type Layout = {
  readonly inputs: readonly UTxO[];
  readonly references: readonly UTxO[];
  readonly policies: readonly string[];
};
const compareOutRef = (a: UTxO, b: UTxO) =>
  a.txHash < b.txHash
    ? -1
    : a.txHash > b.txHash
      ? 1
      : a.outputIndex - b.outputIndex;
const position = (utxos: readonly UTxO[], utxo: UTxO) => {
  const found = [...utxos]
    .sort(compareOutRef)
    .findIndex(
      (u) => u.txHash === utxo.txHash && u.outputIndex === utxo.outputIndex,
    );
  if (found < 0) throw new Error("Missing authored input");
  return BigInt(found);
};
const inputIndex = (layout: Layout, utxo: UTxO) =>
  position(layout.inputs, utxo);
const refIndex = (layout: Layout, utxo: UTxO) =>
  position(layout.references, utxo);
const inline = (value: string) => ({ kind: "inline" as const, value });

const isScriptAddress = (address: string) =>
  CML.Address.from_bech32(address).payment_cred()?.as_script() !== undefined;
/** Redeemers sort spends first, then mints by policy. */
const scriptSpendCount = (layout: Layout) =>
  layout.inputs.filter((u) => isScriptAddress(u.address)).length;
const mintIndex = (layout: Layout, policy: string) =>
  BigInt(
    scriptSpendCount(layout) + [...layout.policies].sort().indexOf(policy),
  );

const unavailableTimeoutYield = (f: AvailabilityFixture) =>
  SDK.scriptRewardAddress(
    "Preprod",
    f.contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
  );
const timeoutYield = (f: AvailabilityFixture) =>
  SDK.scriptRewardAddress(
    "Preprod",
    f.contracts.availabilityChallenge.yields.timeout.withdrawalScript,
  );

/**
 * The ledger index of the timeout yield's withdrawal redeemer in the mirror:
 * withdrawals sort by reward-account bytes, and the yield hashes depend on the
 * fixture's reference-script auth policy.
 */
const timeoutYieldWithdrawIndex = (f: AvailabilityFixture) =>
  [unavailableTimeoutYield(f), timeoutYield(f)]
    .map((address) => CML.Address.from_bech32(address).to_hex())
    .sort()
    .indexOf(CML.Address.from_bech32(timeoutYield(f)).to_hex());

/**
 * A hand-built mirror of the timeout (record, terminal, head, root, Idle lock,
 * pool) with free arithmetic, so a negative can break exactly one relation
 * the production builder never lets through. Outputs: root (0), Idle lock
 * (1), the ONE challenger output (2), the pool (3, when present), rent (last).
 * The fee is exactly `feeLovelace`: the inputs pay the outputs and it, and no
 * change output exists, so it completes here with coin selection off.
 */
const completeTimeoutMirror = (
  f: AvailabilityFixture,
  s: SDK.DaAvailabilityChallengeSnapshot,
  terminal: UTxO,
  arithmetic: {
    readonly feeLovelace: bigint;
    readonly challengerOutputLovelace: bigint;
  } & (
    | { readonly pool: UTxO; readonly poolOutputLovelace: bigint }
    | { readonly pool?: undefined }
  ),
): ReturnType<TxBuilder["complete"]> => {
  const record = s.record!;
  const queue = s.queue!.utxo;
  const root = s.confirmedState.utxo;
  const lock = s.correctionLock;
  const { pool } = arithmetic;
  const challengeAssetName = recordOf(s).challenge_asset_name;
  const ap = f.contracts.availabilityChallenge.policyId;
  const qp = f.contracts.stateQueue.policyId;
  const layout: Layout = {
    inputs: [record, terminal, queue, root, lock, ...(pool ? [pool] : [])],
    policies: [ap, qp],
    references: [
      f.hubOracleRefInput,
      f.reference("state-queue spending"),
      f.reference("state-queue minting"),
      f.reference("state-queue unavailable-timeout withdrawal"),
      f.reference("correction-lock spending"),
      f.reference("availability-challenge spending"),
      f.reference("availability-challenge minting"),
      f.reference("availability-challenge timeout withdrawal"),
      ...(pool ? [f.reference("da-bond-pool spending")] : []),
    ],
  };
  const lower = BigInt(f.emulator.now());
  const coordinate = Data.to(
    { Coordinate: { mint_redeemer_index: mintIndex(layout, ap) } },
    SDK.DaAvailabilitySpendRedeemer,
  );
  let tx = f.lucid
    .newTx()
    .setMinFee(arithmetic.feeLovelace)
    .validFrom(Number(lower))
    .validTo(Number(lower + 60_000n))
    .collectFrom([record, terminal], coordinate)
    .collectFrom(
      [queue, root],
      Data.to("LinkedListMutation", SDK.StateQueueSpendRedeemer),
    )
    .collectFrom(
      [lock],
      Data.to(
        {
          Correct: {
            hub_oracle_ref_input_index: refIndex(layout, f.hubOracleRefInput),
          },
        },
        SDK.CorrectionLockRedeemer,
      ),
    )
    .readFrom([...layout.references])
    .mintAssets(
      {
        [ap + challengeAssetName]: -1n,
        [ap +
        SDK.daAvailabilityTerminalAccumulatorAssetName(challengeAssetName)]:
          -1n,
      },
      Data.to(
        {
          TimeoutChallenge: {
            yield_to_ref_input_index: refIndex(
              layout,
              f.reference("availability-challenge timeout withdrawal"),
            ),
            hub_oracle_ref_input_index: refIndex(layout, f.hubOracleRefInput),
            record_input_index: inputIndex(layout, record),
            terminal_accumulator_input_index: inputIndex(layout, terminal),
            state_queue_mint_redeemer_index: mintIndex(layout, qp),
            // Without a pool the index names the (unauthentic) lock input.
            pool_input_index: inputIndex(layout, pool ?? lock),
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
              layout,
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
      ),
    )
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
      .collectFrom(
        [arithmetic.pool],
        Data.to(
          {
            Slash: {
              hub_oracle_ref_input_index: refIndex(layout, f.hubOracleRefInput),
              state_queue_mint_redeemer_index: mintIndex(layout, qp),
              correction_lock_input_index: inputIndex(layout, lock),
              output_index: 3n,
            },
          },
          SDK.DaBondPoolSpendRedeemer,
        ),
      )
      .pay.ToContract(arithmetic.pool.address, inline(arithmetic.pool.datum!), {
        lovelace: arithmetic.poolOutputLovelace,
        [f.poolUnit]: 1n,
      });
  return tx.pay
    .ToAddress(f.responder.address, { lovelace: queue.assets.lovelace })
    .withdraw(unavailableTimeoutYield(f), 0n, Data.void())
    .withdraw(timeoutYield(f), 0n, Data.void())
    .complete({ coinSelection: false, localUPLCEval: true });
};

/**
 * A hand-built resume step that removes the head on the `Locked` lock a
 * descendant-first timeout left behind (the SDK's `RemoveTimedOutHead`
 * continuation), optionally with the pool's `Slash` smuggled in. Outputs:
 * root (0), Idle lock (1), rent (2), the slashed pool (3, when present), then
 * the fee input's change.
 */
const buildLockedHeadResume = (
  f: AvailabilityFixture,
  s: SDK.DaAvailabilityChallengeSnapshot,
  challengeAssetName: string,
  feeFunding: UTxO,
  pool?: UTxO,
): TxBuilder => {
  const queue = s.queue!.utxo;
  const root = s.confirmedState.utxo;
  const lock = s.correctionLock;
  const qp = f.contracts.stateQueue.policyId;
  const layout: Layout = {
    inputs: [queue, root, lock, feeFunding, ...(pool ? [pool] : [])],
    policies: [qp],
    references: [
      f.hubOracleRefInput,
      f.reference("state-queue spending"),
      f.reference("state-queue minting"),
      f.reference("state-queue unavailable-timeout withdrawal"),
      f.reference("correction-lock spending"),
      ...(pool ? [f.reference("da-bond-pool spending")] : []),
    ],
  };
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
    .collectFrom(
      [lock],
      Data.to(
        {
          Correct: {
            hub_oracle_ref_input_index: refIndex(layout, f.hubOracleRefInput),
          },
        },
        SDK.CorrectionLockRedeemer,
      ),
    )
    .readFrom([...layout.references])
    .mintAssets(
      { [f.queueUnit]: -1n },
      Data.to(
        {
          RemoveUnavailableBlockAfterTimeout: {
            yield_to_ref_input_index: refIndex(
              layout,
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
      ),
    )
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
      .collectFrom(
        [pool],
        Data.to(
          {
            Slash: {
              hub_oracle_ref_input_index: refIndex(layout, f.hubOracleRefInput),
              state_queue_mint_redeemer_index: mintIndex(layout, qp),
              correction_lock_input_index: inputIndex(layout, lock),
              output_index: 3n,
            },
          },
          SDK.DaBondPoolSpendRedeemer,
        ),
      )
      .pay.ToContract(pool.address, inline(pool.datum!), {
        lovelace: poolOutputLovelace,
        [f.poolUnit]: 1n,
      });
  }
  return tx.withdraw(unavailableTimeoutYield(f), 0n, Data.void());
};

/**
 * A hand-built merge of the queue head into the confirmed state. The fixture
 * block carries no L2 material, so `merge_to_confirmed_state` requires
 * `m_settlement_redeemer_index == None` and spawns no settlement (the SDK
 * merge builder always spawns one). Outputs: the continued root (0), then the
 * wallet's change, which takes the merged node's lovelace.
 */
const buildEmptyBlockMerge = async (f: AvailabilityFixture, queue: UTxO) => {
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
const advanceToMaturity = (f: AvailabilityFixture, queue: UTxO) => {
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
const expectTimeoutArithmetic = (
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
