import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import * as Availability from "./availability-challenge.js";
import {
  at,
  auth,
  coordinate,
  type DaAvailabilityDeployment,
  type DaAvailabilityExpectedOutput,
  effect,
  fail,
  type PublishDaAvailabilityChunkParams,
  recordDatum,
  type SettleDaAvailabilityTrancheParams,
  spend,
  terminalDatum,
  trancheDatum,
} from "./availability-challenge-transactions.at.js";
import { authenticateCarrier } from "./availability-challenge-transactions.build-open-da-availability-challenge-tx-program.js";
import {
  alignResources,
  complete,
} from "./availability-challenge-transactions.complete.js";
import {
  baseRefs,
  minAda,
  mint,
  mintRefs,
  pay,
  withYield,
} from "./availability-challenge-transactions.plan-da-availability-timeout.js";
import { StateQueueNode } from "./ledger-state.js";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "./linked-list.js";
import { type StateQueueUTxO } from "./state-queue.js";
import {
  requireInputIndex,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
} from "./tx-context-redeemer.js";

export const buildPublishDaAvailabilityChunkTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: PublishDaAvailabilityChunkParams,
) =>
  effect(async () => {
    p = alignResources(lucid, p);
    const t = trancheDatum(p.thread);
    authenticateCarrier(d, p.thread, t, p.previousCarrier);
    if (!("Active" in t)) fail("Only an active tranche accepts publication");
    const active = (
      t as Extract<Availability.DaAvailabilityTrancheDatum, { Active: unknown }>
    ).Active;
    const carrier: DaAvailabilityExpectedOutput = {
      address: p.thread.address,
      assets: { lovelace: 0n },
      datum: Availability.encodeDaAvailabilityPublicationDatum(
        p.publication,
        d.parameters.response_geometry,
        active.descriptor,
      ),
    };
    carrier.assets.lovelace = minAda(lucid, carrier);
    const next = Availability.advanceDaAvailabilityTranche({
      active: t,
      publication: p.publication,
      responseGeometry: Availability.availabilityResponseGeometry({
        chunkByteLength: Number(
          d.parameters.response_geometry.chunk_byte_length,
        ),
        trancheByteLength: Number(
          d.parameters.response_geometry.tranche_byte_length,
        ),
        maxTrancheCount: Number(
          d.parameters.response_geometry.max_tranche_count,
        ),
      }),
      inclusiveValidityUpper: p.validTo - 1n,
      carrierOutputIndex: 1n,
    });
    const output: DaAvailabilityExpectedOutput = {
      address: p.thread.address,
      assets: { ...p.thread.assets },
      datum: Availability.encodeDaAvailabilityTrancheDatum(next),
    };
    const transition =
      Availability.planDaAvailabilityPublicationValueTransition({
        threadInputLovelace: p.thread.assets.lovelace,
        previousCarrierInputLovelace: p.previousCarrier?.assets.lovelace ?? 0n,
        nextCarrierOutputLovelace: carrier.assets.lovelace,
        transactionFeeLovelace: p.feeLovelace,
        minimumThreadOutputLovelace: minAda(lucid, output),
        isFirstPublication: active.latest_carrier_output_index === null,
        parameters: d.parameters,
      });
    output.assets.lovelace = transition;
    const refs = baseRefs(d);
    let tx = lucid
      .newTx()
      .readFrom(refs)
      .collectFrom(
        [p.thread],
        spend(p.thread, (ctx) => ({
          AdvanceTranche: {
            thread_output_index: at(ctx, 0, output, "thread"),
            carrier_output_index: at(ctx, 1, carrier, "carrier"),
            m_previous_carrier_input_index: p.previousCarrier
              ? requireInputIndex(ctx, p.previousCarrier, "previous carrier")
              : null,
          },
        })),
      );
    if (p.previousCarrier)
      tx = tx.collectFrom(
        [p.previousCarrier],
        spend(p.previousCarrier, (ctx) => ({
          ConsumeCarrier: {
            thread_input_index: requireInputIndex(ctx, p.thread, "thread"),
            thread_spend_redeemer_index: requireSpendRedeemerIndex(
              ctx,
              p.thread,
              "thread",
            ),
          },
        })),
      );
    const outputs = [output, carrier];
    return complete(lucid, d, p, pay(tx, outputs), {
      action: "publish",
      headerHash: active.header_hash,
      challengeAssetName: active.challenge_asset_name,
      inputs: [p.thread, ...(p.previousCarrier ? [p.previousCarrier] : [])],
      refs,
      outputs,
    });
  });

/**
 * Authenticates a challenge record (the DACH token and exactly
 * `challenge_record_lovelace` at the availability address, with a canonical
 * record datum for this deployment) and, when given, the terminal accumulator
 * it binds.
 */
export const challenged = (
  d: DaAvailabilityDeployment,
  record: UTxO,
  terminal?: UTxO,
) => {
  const r = recordDatum(record, d);
  if (r.commitment.deployment_identity !== d.hubOraclePolicyId)
    fail("Challenge record deployment identity mismatch");
  auth(record, d.contracts.availabilityChallenge.spendingScriptAddress, [
    d.contracts.availabilityChallenge.policyId + r.challenge_asset_name,
  ]);
  if (record.assets.lovelace !== d.parameters.challenge_record_lovelace)
    fail("Challenge record must hold exactly challenge_record_lovelace");
  if (terminal) {
    const t = terminalDatum(terminal);
    auth(terminal, record.address, [
      d.contracts.availabilityChallenge.policyId +
        Availability.daAvailabilityTerminalAccumulatorAssetName(
          r.challenge_asset_name,
        ),
    ]);
    if (
      t.challenge_asset_name !== r.challenge_asset_name ||
      t.header_hash !== r.commitment.header_hash ||
      t.deployment_identity !== r.commitment.deployment_identity ||
      t.challenger !== r.challenger ||
      t.response_deadline !== r.response_deadline ||
      t.remaining_challenger_lovelace !== terminal.assets.lovelace
    )
      fail("Terminal accumulator does not authenticate the challenge record");
  }
  return r;
};

/** The queue node's status is exactly the `Challenged` the record implies. */
export const assertChallengedNode = (
  q: StateQueueUTxO,
  r: Availability.DaAvailabilityChallengeRecord,
) => {
  const node = Data.castFrom(q.datum.data, StateQueueNode);
  const status = node.da_attestation;
  if (
    q.assetName !==
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + r.commitment.header_hash ||
    typeof status !== "object" ||
    !("Challenged" in status) ||
    status.Challenged.challenge_asset_name !== r.challenge_asset_name ||
    status.Challenged.commitment_hash !==
      Availability.daAvailabilityCommitmentHash(r.commitment)
  )
    fail("Queue does not authenticate the challenge");
  return node;
};

export const buildSettleDaAvailabilityTrancheTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: SettleDaAvailabilityTrancheParams,
) =>
  effect(async () => {
    p = alignResources(lucid, p);
    const r = challenged(d, p.record, p.terminal),
      t = trancheDatum(p.thread);
    authenticateCarrier(d, p.thread, t, p.carrier);
    const term = terminalDatum(p.terminal);
    const plan = Availability.planDaAvailabilitySettlement({
      commitment: r.commitment,
      terminalAccumulator: term,
      tranche: t,
      threadLovelace: p.thread.assets.lovelace,
      carrierLovelace: p.carrier?.assets.lovelace ?? 0n,
      transactionFeeLovelace: p.feeLovelace,
      inclusiveValidityLower: p.validFrom,
      parameters: d.parameters,
    });
    const policy = d.contracts.availabilityChallenge.policyId;
    const output = {
      address: p.terminal.address,
      assets: { ...p.terminal.assets, lovelace: plan.nextTerminalLovelace },
      datum: Availability.encodeDaAvailabilityTerminalAccumulatorDatum(
        plan.nextTerminalAccumulator,
      ),
    };
    const refs = [...mintRefs(d, "settle"), p.record];
    const inputs = [p.terminal, p.thread, ...(p.carrier ? [p.carrier] : [])];
    let tx = lucid.newTx().readFrom(refs);
    for (const u of inputs) tx = tx.collectFrom([u], coordinate(u, policy));
    tx = tx.mintAssets(
      {
        [policy +
        Availability.daAvailabilityTrancheAssetName({
          challengeAssetName: r.challenge_asset_name,
          trancheIndex: Number(term.next_tranche_index),
        })]: -1n,
      },
      mint(policy, (ctx) => ({
        SettleTranche: {
          yield_to_ref_input_index: requireReferenceInputIndex(
            ctx,
            refs[2]!,
            "settle yield",
          ),
          record_ref_input_index: requireReferenceInputIndex(
            ctx,
            p.record,
            "record",
          ),
          terminal_accumulator_input_index: requireInputIndex(
            ctx,
            p.terminal,
            "terminal",
          ),
          terminal_accumulator_output_index: at(ctx, 0, output, "terminal"),
          tranche_input_index: requireInputIndex(ctx, p.thread, "thread"),
          carrier_input_index: p.carrier
            ? requireInputIndex(ctx, p.carrier, "carrier")
            : null,
        },
      })),
    );
    return complete(
      lucid,
      d,
      p,
      withYield(lucid, d, pay(tx, [output]), "settle"),
      {
        action: "settle",
        headerHash: r.commitment.header_hash,
        challengeAssetName: r.challenge_asset_name,
        inputs,
        refs,
        outputs: [output],
      },
    );
  });
