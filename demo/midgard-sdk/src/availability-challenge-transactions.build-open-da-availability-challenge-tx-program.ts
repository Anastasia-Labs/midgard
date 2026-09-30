import {
  type Assets,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import * as Availability from "./availability-challenge.js";
import {
  at,
  auth,
  type DaAvailabilityDeployment,
  type DaAvailabilityExpectedOutput,
  datum,
  effect,
  fail,
  keyAddress,
  type OpenDaAvailabilityChallengeParams,
  state,
} from "./availability-challenge-transactions.at.js";
import {
  alignResources,
  complete,
} from "./availability-challenge-transactions.complete.js";
import {
  assertDaAvailabilityChallengeRecordMinAda,
  assertDaAvailabilityOpenCommitment,
  assertDaAvailabilityOpenWithinChallengeWindow,
  hub,
  minAda,
  mint,
  mintRefs,
  pay,
  protocolParameters,
  queueUpdate,
  role,
  withYield,
} from "./availability-challenge-transactions.plan-da-availability-timeout.js";
import { outputReferenceFromUTxO } from "./common.js";
import { castStateQueueNodeToData, StateQueueNode } from "./ledger-state.js";
import {
  encodeLinkedListNodeView,
  type LinkedListNodeView,
} from "./linked-list.js";
import {
  requireInputIndex,
  requireReferenceInputIndex,
} from "./tx-context-redeemer.js";
import { isPlainPositiveAdaOnlyUtxo } from "./tx-output-utils.js";

/** Conservatively reserves a full-ledger-size carrier and thread at live rent. */
export const assertDaAvailabilityOpeningWorkingCapital = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  plan: Availability.DaAvailabilityChallengeDatumPlan,
): void => {
  const protocol = protocolParameters(lucid);
  // No accepted carrier can be larger than its entire transaction. Encoding this
  // bound as a bytes datum also covers the datum-envelope overhead conservatively.
  const carrierFloor = minAda(lucid, {
    address: d.contracts.availabilityChallenge.spendingScriptAddress,
    assets: { lovelace: 100_000_000n },
    datum: Data.to("00".repeat(protocol.maxTxSize)),
  });
  for (let i = 0; i < plan.trancheThreads.length; i++) {
    const thread = plan.trancheThreads[i]!;
    if (!("Active" in thread)) fail("Opening must create active tranches");
    const active = (
      thread as Extract<
        Availability.DaAvailabilityTrancheDatum,
        { Active: unknown }
      >
    ).Active;
    const funded = plan.trancheFunding[i]!;
    const worstThread = {
      Active: {
        ...active,
        next_offset:
          active.descriptor.start_offset + active.descriptor.byte_length,
        latest_carrier_output_index: 1n,
      },
    };
    const threadFloor = minAda(lucid, {
      address: d.contracts.availabilityChallenge.spendingScriptAddress,
      assets: {
        lovelace: funded.initialLovelace,
        [d.contracts.availabilityChallenge.policyId +
        Availability.daAvailabilityTrancheAssetName({
          challengeAssetName: plan.challengeAssetName,
          trancheIndex: i,
        })]: 1n,
      },
      datum: Data.to(worstThread, Availability.DaAvailabilityTrancheDatum),
    });
    if (
      funded.initialLovelace -
        funded.maximumPublicationFeeReserveLovelace -
        funded.maximumSettlementFeeReserveLovelace <
      carrierFloor + threadFloor
    )
      fail(
        "challenger_bond_lovelace cannot fund all publication fees and live carrier/thread working capital",
      );
  }
};

/**
 * Opens a challenge (spec #685 E1). Inputs: the challenger's exact funding
 * coin and the Attested queue node. Outputs, in this order: the challenge
 * record (0), the queue node now `Challenged` (1), one thread per tranche
 * (2..), the terminal accumulator (last). The DACH identity derives from the
 * funding coin's out-reference and `opened_at` is `validTo - 1`.
 */
export const buildOpenDaAvailabilityChallengeTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: OpenDaAvailabilityChallengeParams,
) =>
  effect(async () => {
    p = alignResources(lucid, p);
    const policy = d.contracts.availabilityChallenge.policyId,
      address = d.contracts.availabilityChallenge.spendingScriptAddress;
    const q = await state(p.queue, d);
    const node = Data.castFrom(q.datum.data, StateQueueNode);
    const commitmentHash = assertDaAvailabilityOpenCommitment({
      commitment: p.commitment,
      deploymentIdentity: d.hubOraclePolicyId,
      queueAssetName: q.assetName,
      status: node.da_attestation,
      parameters: d.parameters,
    });
    assertDaAvailabilityOpenWithinChallengeWindow({
      validTo: p.validTo,
      nodeEndTime: node.header.endTime,
      daChallengeWindowMs: p.daChallengeWindowMs,
    });
    if (
      !isPlainPositiveAdaOnlyUtxo(p.challengerFunding) ||
      p.challengerFunding.address !== keyAddress(lucid, p.challenger) ||
      p.challengerFunding.assets.lovelace !==
        d.parameters.challenger_bond_lovelace +
          d.parameters.challenge_record_lovelace +
          p.feeLovelace
    )
      fail(
        "Opening requires an exact isolated challenger_bond_lovelace + challenge_record_lovelace + fee input",
      );
    const plan = Availability.buildDaAvailabilityChallengeDatumPlan({
      commitment: p.commitment,
      challengerFundingOutRef: outputReferenceFromUTxO(p.challengerFunding),
      challenger: p.challenger,
      // The validator anchors the response window at the inclusive upper
      // validity bound; the ledger's upper end is exclusive.
      openedAt: p.validTo - 1n,
      parameters: d.parameters,
    });
    assertDaAvailabilityOpeningWorkingCapital(lucid, d, plan);
    const record: DaAvailabilityExpectedOutput = {
      address,
      assets: {
        lovelace: plan.recordLovelace,
        [policy + plan.challengeAssetName]: 1n,
      },
      datum: Availability.encodeDaAvailabilityChallengeRecord(
        plan.record,
        d.parameters,
      ),
    };
    assertDaAvailabilityChallengeRecordMinAda({
      coinsPerUtxoByte: protocolParameters(lucid).coinsPerUtxoByte,
      record,
      challengeRecordLovelace: d.parameters.challenge_record_lovelace,
    });
    const outputs: DaAvailabilityExpectedOutput[] = [
      record,
      {
        address: p.queue.address,
        assets: p.queue.assets,
        datum: encodeLinkedListNodeView({
          ...q.datum,
          data: castStateQueueNodeToData({
            ...node,
            da_attestation: {
              Challenged: {
                commitment_hash: commitmentHash,
                challenge_asset_name: plan.challengeAssetName,
              },
            },
          }) as LinkedListNodeView["data"],
        }),
      },
    ];
    const minted: Assets = { [policy + plan.challengeAssetName]: 1n };
    for (let i = 0; i < plan.trancheThreads.length; i++) {
      const unit =
        policy +
        Availability.daAvailabilityTrancheAssetName({
          challengeAssetName: plan.challengeAssetName,
          trancheIndex: i,
        });
      minted[unit] = 1n;
      outputs.push({
        address,
        assets: {
          lovelace: plan.trancheFunding[i]!.initialLovelace,
          [unit]: 1n,
        },
        datum: Availability.encodeDaAvailabilityTrancheDatum(
          plan.trancheThreads[i]!,
        ),
      });
    }
    const terminalUnit =
      policy +
      Availability.daAvailabilityTerminalAccumulatorAssetName(
        plan.challengeAssetName,
      );
    minted[terminalUnit] = 1n;
    outputs.push({
      address,
      assets: {
        lovelace: plan.terminalAccumulatorFundingLovelace,
        [terminalUnit]: 1n,
      },
      datum: Availability.encodeDaAvailabilityTerminalAccumulatorDatum(
        plan.terminalAccumulator,
      ),
    });
    const terminalIndex = outputs.length - 1;
    const refs = [
      ...mintRefs(d, "open"),
      hub(d),
      role(d, "state-queue spending", d.contracts.stateQueue.spendingScript),
    ];
    const yieldRef = refs[2]!;
    let tx = lucid
      .newTx()
      .collectFrom([p.challengerFunding])
      .collectFrom([p.queue], queueUpdate(p.queue, policy, outputs[1]!, 1))
      .readFrom(refs)
      .mintAssets(
        minted,
        mint(policy, (ctx) => {
          for (let i = 0; i < plan.trancheThreads.length; i++)
            at(ctx, 2 + i, outputs[2 + i]!, "tranche");
          return {
            OpenChallenge: {
              yield_to_ref_input_index: requireReferenceInputIndex(
                ctx,
                yieldRef,
                "open yield",
              ),
              hub_oracle_ref_input_index: requireReferenceInputIndex(
                ctx,
                d.hubOracleRefInput,
                "hub",
              ),
              record_output_index: at(ctx, 0, record, "record"),
              challenger_input_index: requireInputIndex(
                ctx,
                p.challengerFunding,
                "challenger",
              ),
              state_queue_input_index: requireInputIndex(ctx, p.queue, "queue"),
              state_queue_output_index: at(ctx, 1, outputs[1]!, "queue"),
              first_tranche_output_index: 2n,
              terminal_accumulator_output_index: at(
                ctx,
                terminalIndex,
                outputs[terminalIndex]!,
                "terminal",
              ),
              challenger: p.challenger,
            },
          };
        }),
      )
      .addSignerKey(p.challenger);
    tx = withYield(lucid, d, pay(tx, outputs), "open");
    return complete(lucid, d, p, tx, {
      action: "open",
      headerHash: p.commitment.header_hash,
      challengeAssetName: plan.challengeAssetName,
      inputs: [p.challengerFunding, p.queue],
      refs,
      outputs,
    });
  });

export const authenticateCarrier = (
  d: DaAvailabilityDeployment,
  thread: UTxO,
  t: Availability.DaAvailabilityTrancheDatum,
  carrier?: UTxO,
) => {
  const v = "Active" in t ? t.Active : t.Receipt;
  const index =
    "Active" in t
      ? t.Active.latest_carrier_output_index
      : t.Receipt.terminal_carrier_output_index;
  auth(thread, d.contracts.availabilityChallenge.spendingScriptAddress, [
    d.contracts.availabilityChallenge.policyId +
      Availability.daAvailabilityTrancheAssetName({
        challengeAssetName: v.challenge_asset_name,
        trancheIndex: Number(v.descriptor.tranche_index),
      }),
  ]);
  if (index === null) {
    if (carrier) fail("Unexpected carrier for a fresh tranche");
    return;
  }
  if (
    !carrier ||
    carrier.txHash !== thread.txHash ||
    BigInt(carrier.outputIndex) !== index
  )
    fail("Missing exact latest carrier out-reference");
  const c = carrier!;
  auth(c, thread.address, []);
  const publication = Availability.parseDaAvailabilityPublicationDatumCbor(
    Data.to(
      Data.from(datum(c), Availability.DaAvailabilityPublicationDatum),
      Availability.DaAvailabilityPublicationDatum,
    ),
    d.parameters.response_geometry,
    v.descriptor,
  );
  const accumulator =
    "Active" in t ? t.Active.accumulator : t.Receipt.terminal_accumulator;
  if (
    publication.challenge_asset_name !== v.challenge_asset_name ||
    publication.header_hash !== v.header_hash ||
    publication.deployment_identity !== v.deployment_identity ||
    publication.tranche_index !== v.descriptor.tranche_index ||
    publication.next_accumulator !== accumulator ||
    publication.chunk_offset + publication.chunk_byte_length !==
      ("Active" in t
        ? t.Active.next_offset
        : t.Receipt.descriptor.start_offset + t.Receipt.descriptor.byte_length)
  )
    fail("Carrier does not authenticate tranche continuation");
};
