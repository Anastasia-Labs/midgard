import {
  Data,
  getAddressDetails,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import * as Availability from "./availability-challenge.js";
import {
  at,
  type CloseDaAvailabilityChallengeParams,
  coordinate,
  type DaAvailabilityDeployment,
  type DaAvailabilityExpectedOutput,
  datum,
  effect,
  fail,
  keyAddress,
  refKey,
  state,
  terminalDatum,
} from "./availability-challenge-transactions.at.js";
import {
  assertChallengedNode,
  challenged,
} from "./availability-challenge-transactions.build-publish-da-availability-chunk-tx-program.js";
import {
  alignResources,
  complete,
} from "./availability-challenge-transactions.complete.js";
import {
  hub,
  mint,
  mintRefs,
  pay,
  queueUpdate,
  role,
  withYield,
} from "./availability-challenge-transactions.plan-da-availability-timeout.js";
import {
  assertCanonicalDaBondPoolDatum,
  DaBondPoolDatum,
  daBondPoolUnit,
} from "./da-bond-pool.js";
import { castStateQueueNodeToData } from "./ledger-state.js";
import {
  encodeLinkedListNodeView,
  type LinkedListNodeView,
} from "./linked-list.js";
import {
  requireInputIndex,
  requireReferenceInputIndex,
} from "./tx-context-redeemer.js";

/**
 * Closes a fully published challenge. Inputs: record, terminal, queue node.
 * Outputs: the queue node now `Published` (0) and the challenger's refund of
 * `remaining - fee + challenge_record_lovelace` (1). The committee's pooled
 * bond is not touched.
 */
export const buildCloseDaAvailabilityChallengeTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: CloseDaAvailabilityChallengeParams,
) =>
  effect(async () => {
    p = alignResources(lucid, p);
    const r = challenged(d, p.record, p.terminal),
      term = terminalDatum(p.terminal);
    if (
      term.has_timed_out_tranche ||
      term.next_tranche_index !==
        BigInt(r.commitment.tranche_descriptors.length)
    )
      fail("Challenge is not completely published and settled");
    if (term.remaining_challenger_lovelace <= p.feeLovelace)
      fail("Close fee must leave challenger reserve");
    const q = await state(p.queue, d);
    const node = assertChallengedNode(q, r);
    const outputs: DaAvailabilityExpectedOutput[] = [
      {
        address: p.queue.address,
        assets: p.queue.assets,
        datum: encodeLinkedListNodeView({
          ...q.datum,
          data: castStateQueueNodeToData({
            ...node,
            da_attestation: {
              Published: {
                terminal_commitment:
                  Availability.daAvailabilityPublishedTerminalCommitment(
                    r.commitment,
                  ),
              },
            },
          }) as LinkedListNodeView["data"],
        }),
      },
      {
        address: keyAddress(lucid, r.challenger),
        assets: {
          lovelace:
            term.remaining_challenger_lovelace -
            p.feeLovelace +
            d.parameters.challenge_record_lovelace,
        },
      },
    ];
    const policy = d.contracts.availabilityChallenge.policyId,
      refs = [
        ...mintRefs(d, "close"),
        hub(d),
        role(d, "state-queue spending", d.contracts.stateQueue.spendingScript),
      ];
    const tx = lucid
      .newTx()
      .collectFrom([p.record], coordinate(p.record, policy))
      .collectFrom([p.terminal], coordinate(p.terminal, policy))
      .collectFrom([p.queue], queueUpdate(p.queue, policy, outputs[0]!, 0))
      .readFrom(refs)
      .mintAssets(
        {
          [policy + r.challenge_asset_name]: -1n,
          [policy +
          Availability.daAvailabilityTerminalAccumulatorAssetName(
            r.challenge_asset_name,
          )]: -1n,
        },
        mint(policy, (ctx) => ({
          CloseChallenge: {
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              refs[2]!,
              "close yield",
            ),
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.hubOracleRefInput,
              "hub",
            ),
            record_input_index: requireInputIndex(ctx, p.record, "record"),
            terminal_accumulator_input_index: requireInputIndex(
              ctx,
              p.terminal,
              "terminal",
            ),
            state_queue_input_index: requireInputIndex(ctx, p.queue, "queue"),
            state_queue_output_index: at(ctx, 0, outputs[0]!, "queue"),
            challenger_refund_output_index: at(
              ctx,
              1,
              outputs[1]!,
              "challenger refund",
            ),
          },
        })),
      );
    return complete(
      lucid,
      d,
      p,
      withYield(lucid, d, pay(tx, outputs), "close"),
      {
        action: "close",
        headerHash: r.commitment.header_hash,
        challengeAssetName: r.challenge_asset_name,
        inputs: [p.record, p.terminal, p.queue],
        refs,
        outputs,
      },
    );
  });

/**
 * The pooled DA bond input: the pool NFT exactly once beside lovelace, at a
 * script address whose payment credential is the pool policy (as
 * `get_authentic_pool_input` requires), with a canonical inline pool datum.
 */
export const authenticPool = (d: DaAvailabilityDeployment, u: UTxO) => {
  const policy = d.contracts.daBondPool.policyId;
  const unit = daBondPoolUnit(policy);
  const credential = getAddressDetails(u.address).paymentCredential;
  if (
    u.address !== d.contracts.daBondPool.spendingScriptAddress ||
    credential?.type !== "Script" ||
    credential.hash !== policy ||
    u.scriptRef != null ||
    u.datumHash != null ||
    u.assets[unit] !== 1n ||
    Object.keys(u.assets).some((k) => k !== "lovelace" && k !== unit)
  )
    fail(`Unauthentic DA bond pool input ${refKey(u)}`);
  const value = Data.from(datum(u), DaBondPoolDatum);
  assertCanonicalDaBondPoolDatum(value);
  return value;
};

export type TimeoutLeg = {
  readonly record: UTxO;
  readonly terminal: UTxO;
  readonly pool: UTxO;
  readonly feePart: bigint;
};
