import * as SDK from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { coordinate } from "./availability-challenge-emulator.attest-availability.js";
import {
  AVAILABILITY_EMULATOR_PARAMETERS,
  type AvailabilityLayout,
  index,
  inline,
  position,
  refIndex,
  spendingInputs,
} from "./availability-challenge-emulator.measure-availability-transaction.js";
import {
  type AvailabilityFixture,
  type OpenAvailability,
  queueUpdate,
  yieldTx,
} from "./availability-challenge-emulator.open-availability.js";

export const buildAvailabilityPublication = (
  f: AvailabilityFixture,
  thread: UTxO,
  publication: SDK.DaAvailabilityPublicationDatum,
  previousCarrier?: UTxO,
  options: { badChunk?: boolean } = {},
) => {
  const datum = Data.from(thread.datum!, SDK.DaAvailabilityTrancheDatum);
  const parameters = f.parameters;
  if (!("Active" in datum))
    throw new Error("Only an active tranche accepts a publication");
  // A publication may not stay valid past the response deadline, which the
  // selected profile's response window can place inside the default range.
  const validFrom = BigInt(f.emulator.now());
  // At or past the deadline the clamped range is empty or inverted, so fail
  // here with the cause instead of submitting a transaction that cannot land.
  if (validFrom >= datum.Active.response_deadline)
    throw new Error(
      `Publication built at or after the response deadline (now=${validFrom}, deadline=${datum.Active.response_deadline})`,
    );
  const deadlineUpper = datum.Active.response_deadline + 1n;
  const validTo =
    validFrom + 60_000n < deadlineUpper ? validFrom + 60_000n : deadlineUpper;
  const geometry = SDK.availabilityResponseGeometry({
    chunkByteLength: Number(parameters.response_geometry.chunk_byte_length),
    trancheByteLength: Number(parameters.response_geometry.tranche_byte_length),
    maxTrancheCount: Number(parameters.response_geometry.max_tranche_count),
  });
  const next = SDK.advanceDaAvailabilityTranche({
    active: datum,
    publication,
    responseGeometry: geometry,
    inclusiveValidityUpper: validTo - 1n,
    carrierOutputIndex: 1n,
  });
  const fee = parameters.max_publication_fee_lovelace;
  const carrierLovelace =
    calculateMinLovelaceFromUTxO(
      AVAILABILITY_EMULATOR_PARAMETERS.coinsPerUtxoByte,
      {
        txHash: "00".repeat(32),
        outputIndex: 1,
        address: thread.address,
        assets: { lovelace: 0n },
        datum: Data.to(publication, SDK.DaAvailabilityPublicationDatum),
      },
    ) + 100_000n;
  const nextLovelace =
    thread.assets.lovelace +
    (previousCarrier?.assets.lovelace ?? 0n) -
    carrierLovelace -
    fee;
  const mutated = options.badChunk
    ? { ...publication, chunk_hash: "00".repeat(32) }
    : publication;
  const ctx: AvailabilityLayout = {
    inputs: [thread, ...(previousCarrier ? [previousCarrier] : [])],
    references: [f.reference("availability-challenge spending")],
    policies: [],
  };
  let tx = f.lucid
    .newTx()
    .setMinFee(fee)
    .validFrom(Number(validFrom))
    .validTo(Number(validTo))
    .readFrom([f.reference("availability-challenge spending")])
    .collectFrom(
      [thread],
      Data.to(
        {
          AdvanceTranche: {
            thread_output_index: 0n,
            carrier_output_index: 1n,
            m_previous_carrier_input_index: previousCarrier
              ? index(ctx, previousCarrier)
              : null,
          },
        },
        SDK.DaAvailabilitySpendRedeemer,
      ),
    )
    .pay.ToContract(
      thread.address,
      inline(SDK.encodeDaAvailabilityTrancheDatum(next)),
      { ...thread.assets, lovelace: nextLovelace },
    )
    .pay.ToContract(
      thread.address,
      inline(Data.to(mutated, SDK.DaAvailabilityPublicationDatum)),
      { lovelace: carrierLovelace },
    );
  if (previousCarrier)
    tx = tx.collectFrom(
      [previousCarrier],
      Data.to(
        {
          ConsumeCarrier: {
            thread_input_index: index(ctx, thread),
            thread_spend_redeemer_index: position(spendingInputs(ctx), thread),
          },
        },
        SDK.DaAvailabilitySpendRedeemer,
      ),
    );
  return tx;
};

/** `SettleTranche`, reading the challenge record as a reference input. */
export const buildAvailabilitySettlement = (
  f: AvailabilityFixture,
  open: OpenAvailability,
  record: UTxO,
  terminal: UTxO,
  thread: UTxO,
  carrier?: UTxO,
  options: { validityLower?: bigint; bypassDeadlinePlanner?: boolean } = {},
) => {
  const terminalDatum = Data.from(
    terminal.datum!,
    SDK.DaAvailabilityTerminalAccumulatorDatum,
  );
  const threadDatum = Data.from(thread.datum!, SDK.DaAvailabilityTrancheDatum);
  const lower = options.validityLower ?? BigInt(f.emulator.now());
  const fee = f.parameters.max_settlement_fee_lovelace;
  const settlement = SDK.planDaAvailabilitySettlement({
    commitment: open.attested.commitment,
    terminalAccumulator: terminalDatum,
    tranche: threadDatum,
    threadLovelace: thread.assets.lovelace,
    carrierLovelace: carrier?.assets.lovelace ?? 0n,
    transactionFeeLovelace: fee,
    inclusiveValidityLower: options.bypassDeadlinePlanner
      ? open.plan.responseDeadline
      : lower,
    parameters: f.parameters,
  });
  const trancheIndex = Number(terminalDatum.next_tranche_index);
  const ctx: AvailabilityLayout = {
    inputs: [terminal, thread, ...(carrier ? [carrier] : [])],
    policies: [open.policy],
    references: [
      record,
      f.reference("availability-challenge spending"),
      f.reference("availability-challenge minting"),
      f.reference("availability-challenge settle withdrawal"),
    ],
  };
  let tx = f.lucid
    .newTx()
    .setMinFee(fee)
    .validFrom(Number(lower))
    .validTo(Number(lower + 60_000n))
    .collectFrom([...ctx.inputs], coordinate(ctx, open.policy))
    .readFrom([...ctx.references])
    .mintAssets(
      {
        [open.policy +
        SDK.daAvailabilityTrancheAssetName({
          challengeAssetName: open.plan.challengeAssetName,
          trancheIndex,
        })]: -1n,
      },
      Data.to(
        {
          SettleTranche: {
            yield_to_ref_input_index: refIndex(
              ctx,
              f.reference("availability-challenge settle withdrawal"),
            ),
            record_ref_input_index: refIndex(ctx, record),
            terminal_accumulator_input_index: index(ctx, terminal),
            terminal_accumulator_output_index: 0n,
            tranche_input_index: index(ctx, thread),
            carrier_input_index: carrier ? index(ctx, carrier) : null,
          },
        },
        SDK.DaAvailabilityMintRedeemer,
      ),
    )
    .pay.ToContract(
      open.address,
      inline(
        SDK.encodeDaAvailabilityTerminalAccumulatorDatum(
          settlement.nextTerminalAccumulator,
        ),
      ),
      { lovelace: settlement.nextTerminalLovelace, [open.terminalUnit]: 1n },
    );
  tx = yieldTx(f, tx, "settle");
  return tx;
};

/**
 * `CloseChallenge`: burns the record and terminal, marks the node Published
 * (0) and refunds the challenger `remaining - fee + challenge_record` (1).
 * The pooled bond is not touched.
 */
export const buildAvailabilityClose = (
  f: AvailabilityFixture,
  open: OpenAvailability,
  record: UTxO,
  queue: UTxO,
  terminal: UTxO,
  options: { redirectRefund?: boolean } = {},
) => {
  const fee = f.parameters.max_close_fee_lovelace;
  const ctx: AvailabilityLayout = {
    inputs: [record, terminal, queue],
    policies: [open.policy],
    references: [
      f.hubOracleRefInput,
      f.reference("state-queue spending"),
      f.reference("availability-challenge spending"),
      f.reference("availability-challenge minting"),
      f.reference("availability-challenge close withdrawal"),
    ],
  };
  const queueView = SDK.getLinkedListNodeViewFromUTxO(queue);
  const view = Effect.runSync(queueView);
  const node = Data.castFrom(view.data, SDK.StateQueueNode);
  const queueDatum = SDK.encodeLinkedListNodeView({
    ...view,
    data: SDK.castStateQueueNodeToData({
      ...node,
      da_attestation: {
        Published: {
          terminal_commitment: SDK.daAvailabilityPublishedTerminalCommitment(
            open.attested.commitment,
          ),
        },
      },
    }) as SDK.LinkedListNodeView["data"],
  });
  let tx = f.lucid
    .newTx()
    .setMinFee(fee)
    .collectFrom([record, terminal], coordinate(ctx, open.policy))
    .collectFrom([queue], queueUpdate(ctx, open.policy, queue, 0n))
    .readFrom([...ctx.references])
    .mintAssets(
      {
        [open.policy + open.plan.challengeAssetName]: -1n,
        [open.terminalUnit]: -1n,
      },
      Data.to(
        {
          CloseChallenge: {
            yield_to_ref_input_index: refIndex(
              ctx,
              f.reference("availability-challenge close withdrawal"),
            ),
            hub_oracle_ref_input_index: refIndex(ctx, f.hubOracleRefInput),
            record_input_index: index(ctx, record),
            terminal_accumulator_input_index: index(ctx, terminal),
            state_queue_input_index: index(ctx, queue),
            state_queue_output_index: 0n,
            challenger_refund_output_index: 1n,
          },
        },
        SDK.DaAvailabilityMintRedeemer,
      ),
    )
    .pay.ToContract(queue.address, inline(queueDatum), queue.assets)
    .pay.ToAddress(
      options.redirectRefund ? f.responder.address : f.challenger.address,
      {
        lovelace:
          terminal.assets.lovelace -
          fee +
          f.parameters.challenge_record_lovelace,
      },
    );
  tx = yieldTx(f, tx, "close");
  return tx;
};
