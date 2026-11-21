import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
} from "@lucid-evolution/lucid";

import * as Availability from "./availability-challenge.js";
import {
  at,
  auth,
  type BuiltDaAvailabilityTransaction,
  coordinate,
  type DaAvailabilityDeployment,
  type DaAvailabilityExpectedOutput,
  type DaAvailabilityRemovalParams,
  datum,
  fail,
  keyAddress,
  refKey,
  state,
  terminalDatum,
} from "./availability-challenge-transactions.at.js";
import {
  authenticPool,
  type TimeoutLeg,
} from "./availability-challenge-transactions.build-close-da-availability-challenge-tx-program.js";
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
  planDaAvailabilityTimeout,
  role,
  withYield,
} from "./availability-challenge-transactions.plan-da-availability-timeout.js";
import { scriptRewardAddress } from "./cardano-addresses.js";
import { outputReferenceFromUTxO } from "./common.js";
import {
  CorrectionLockDatum,
  CorrectionLockRedeemer,
  correctionLockUnit,
} from "./correction-lock.js";
import { DaBondPoolSpendRedeemer, daBondPoolUnit } from "./da-bond-pool.js";
import { StateQueueNode } from "./ledger-state.js";
import {
  encodeLinkedListNodeView,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "./linked-list.js";
import { StateQueueRedeemer, StateQueueSpendRedeemer } from "./state-queue.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
} from "./tx-context-redeemer.js";
import { isPlainPositiveAdaOnlyUtxo } from "./tx-output-utils.js";

export const removal = async (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: DaAvailabilityRemovalParams,
  initial?: TimeoutLeg,
): Promise<BuiltDaAvailabilityTransaction> => {
  p = alignResources(lucid, p);
  const q = await state(p.queue, d),
    root = await state(p.confirmedState, d),
    desc = p.descendant ? await state(p.descendant, d) : undefined;
  if (
    q.assetName !== STATE_QUEUE_NODE_ASSET_NAME_PREFIX + p.headerHash ||
    root.datum.key !== "Empty" ||
    root.datum.next === "Empty" ||
    root.datum.next.Key.key !== p.headerHash
  )
    fail("Unavailable removal requires the current queue head");
  const node = Data.castFrom(q.datum.data, StateQueueNode);
  if (
    typeof node.da_attestation !== "object" ||
    !("Challenged" in node.da_attestation) ||
    node.da_attestation.Challenged.challenge_asset_name !== p.challengeAssetName
  )
    fail("Unavailable queue identity mismatch");
  if (desc) {
    if (
      q.datum.next === "Empty" ||
      desc.datum.key === "Empty" ||
      q.datum.next.Key.key !== desc.datum.key.Key.key
    )
      fail("Removal requires the immediate descendant");
  } else if (q.datum.next !== "Empty")
    fail("Remove head only after its descendants are pruned");
  auth(p.correctionLock, d.contracts.correctionLock.spendingScriptAddress, [
    correctionLockUnit(d.hubOraclePolicyId),
  ]);
  const lock = Data.from(datum(p.correctionLock), CorrectionLockDatum),
    locked: CorrectionLockDatum = {
      Locked: {
        target_header_hash: p.headerHash,
        correction_identity: {
          AvailabilityChallenge: { challenge_asset_name: p.challengeAssetName },
        },
      },
    };
  // The timeout (and so the pool's Slash) runs only on an Idle lock; the
  // resume steps run on the Locked lock the timeout left behind.
  if (
    initial
      ? lock !== "Idle"
      : Data.to(lock, CorrectionLockDatum) !==
        Data.to(locked, CorrectionLockDatum)
  )
    fail("Correction lock does not authorize this challenge transition");
  const continued = desc ? q : root,
    removed = desc ?? q;
  const outputs: DaAvailabilityExpectedOutput[] = [
    {
      address: continued.utxo.address,
      assets: continued.utxo.assets,
      datum: encodeLinkedListNodeView({
        ...continued.datum,
        next: removed.datum.next,
      }),
    },
    {
      address: p.correctionLock.address,
      assets: p.correctionLock.assets,
      datum: Data.to(desc ? locked : "Idle", CorrectionLockDatum),
    },
  ];
  const inputs = [
    q.utxo,
    ...(desc ? [desc.utxo] : [root.utxo]),
    p.correctionLock,
  ];
  const refs = [
    hub(d),
    role(d, "state-queue spending", d.contracts.stateQueue.spendingScript),
    role(d, "state-queue minting", d.contracts.stateQueue.mintingScript),
    role(
      d,
      "state-queue unavailable-timeout withdrawal",
      d.contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
    ),
    role(
      d,
      "correction-lock spending",
      d.contracts.correctionLock.spendingScript,
    ),
    ...(desc ? [root.utxo] : []),
  ];
  if (initial && p.fundingQueueTailRefInput) {
    const tail = await state(p.fundingQueueTailRefInput, d);
    if (tail.datum.next !== "Empty")
      fail("Timeout funding witness must be the current queue tail");
    if (![...inputs, ...refs].some((u) => refKey(u) === refKey(tail.utxo)))
      refs.push(tail.utxo);
  }
  const qp = d.contracts.stateQueue.policyId,
    ap = d.contracts.availabilityChallenge.policyId;
  let tx = lucid
    .newTx()
    .collectFrom(
      [q.utxo, ...(desc ? [desc.utxo] : [root.utxo])],
      Data.to("LinkedListMutation", StateQueueSpendRedeemer),
    )
    .collectFrom([p.correctionLock], ((ctx) => {
      requireOwnSpendPurpose(ctx, p.correctionLock, "correction lock");
      return Data.to(
        {
          Correct: {
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.hubOracleRefInput,
              "hub",
            ),
          },
        },
        CorrectionLockRedeemer,
      );
    }) satisfies BuildTxWithRedeemer);
  if (initial) {
    const r = challenged(d, initial.record, initial.terminal),
      terminal = terminalDatum(initial.terminal);
    if (
      r.challenge_asset_name !== p.challengeAssetName ||
      r.commitment.header_hash !== p.headerHash ||
      !terminal.has_timed_out_tranche ||
      terminal.next_tranche_index !==
        BigInt(r.commitment.tranche_descriptors.length) ||
      p.validFrom < r.response_deadline
    )
      fail(
        "Timeout requires all tranches settled and at least one expired active tranche",
      );
    assertChallengedNode(q, r);
    authenticPool(d, initial.pool);
    const plan = planDaAvailabilityTimeout({
      poolLovelace: initial.pool.assets.lovelace ?? 0n,
      remainingChallengerLovelace: terminal.remaining_challenger_lovelace,
      challengerFeeLovelace: p.feeLovelace - initial.feePart,
      parameters: d.parameters,
    });
    if (plan.feePart !== initial.feePart)
      fail("Timeout fee part does not match the pool's slash");
    const challenger = keyAddress(lucid, r.challenger);
    if (p.rentRefundAddress === challenger)
      fail(
        "Queue rent output must be distinct from the one protected challenger output",
      );
    const challengerIndex = outputs.length;
    outputs.push({
      address: challenger,
      assets: { lovelace: plan.challengerOutputLovelace },
    });
    const poolIndex = outputs.length;
    const poolOutput: DaAvailabilityExpectedOutput = {
      address: initial.pool.address,
      assets: {
        lovelace: plan.poolOutputLovelace,
        [daBondPoolUnit(d.contracts.daBondPool.policyId)]: 1n,
      },
      datum: datum(initial.pool),
    };
    outputs.push(poolOutput);
    refs.push(
      ...mintRefs(d, "timeout"),
      role(d, "da-bond-pool spending", d.contracts.daBondPool.spendingScript),
    );
    inputs.push(initial.record, initial.terminal, initial.pool);
    tx = tx
      .collectFrom([initial.record], coordinate(initial.record, ap))
      .collectFrom([initial.terminal], coordinate(initial.terminal, ap))
      .collectFrom([initial.pool], ((ctx) => {
        requireOwnSpendPurpose(ctx, initial.pool, "DA bond pool");
        return Data.to(
          {
            Slash: {
              hub_oracle_ref_input_index: requireReferenceInputIndex(
                ctx,
                d.hubOracleRefInput,
                "hub",
              ),
              state_queue_mint_redeemer_index: requireMintRedeemerIndex(
                ctx,
                qp,
                "queue",
              ),
              correction_lock_input_index: requireInputIndex(
                ctx,
                p.correctionLock,
                "correction lock",
              ),
              output_index: at(ctx, poolIndex, poolOutput, "DA bond pool"),
            },
          },
          DaBondPoolSpendRedeemer,
        );
      }) satisfies BuildTxWithRedeemer)
      .mintAssets(
        {
          [ap + r.challenge_asset_name]: -1n,
          [ap +
          Availability.daAvailabilityTerminalAccumulatorAssetName(
            r.challenge_asset_name,
          )]: -1n,
        },
        mint(ap, (ctx) => ({
          TimeoutChallenge: {
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.referenceScripts["availability-challenge timeout withdrawal"]!,
              "timeout yield",
            ),
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.hubOracleRefInput,
              "hub",
            ),
            record_input_index: requireInputIndex(
              ctx,
              initial.record,
              "record",
            ),
            terminal_accumulator_input_index: requireInputIndex(
              ctx,
              initial.terminal,
              "terminal",
            ),
            state_queue_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              qp,
              "queue",
            ),
            pool_input_index: requireInputIndex(
              ctx,
              initial.pool,
              "DA bond pool",
            ),
            pool_output_index: at(ctx, poolIndex, poolOutput, "DA bond pool"),
            challenger_refund_output_index: at(
              ctx,
              challengerIndex,
              outputs[challengerIndex]!,
              "challenger refund",
            ),
          },
        })),
      );
    tx = withYield(lucid, d, tx, "timeout");
  } else {
    const funding =
      p.feeFunding ??
      fail("Continuation requires an isolated fee funding input");
    if (
      !isPlainPositiveAdaOnlyUtxo(funding) ||
      funding.assets.lovelace < p.feeLovelace ||
      funding.address !== (await lucid.wallet().address())
    )
      fail("Continuation fee input must cover the explicit fee");
    inputs.push(funding);
    tx = tx.collectFrom([funding]);
    if (funding.assets.lovelace > p.feeLovelace)
      outputs.push({
        address: funding.address,
        assets: { lovelace: funding.assets.lovelace - p.feeLovelace },
      });
  }
  const continuedIndex = 0;
  outputs.push({
    address: p.rentRefundAddress,
    assets: { lovelace: removed.utxo.assets.lovelace },
  });
  tx = tx
    .readFrom(refs)
    .mintAssets({ [qp + removed.assetName]: -1n }, ((ctx) => {
      requireOwnMintPurpose(ctx, qp, "unavailable queue removal");
      return Data.to(
        {
          RemoveUnavailableBlockAfterTimeout: {
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.referenceScripts["state-queue unavailable-timeout withdrawal"]!,
              "queue yield",
            ),
            unavailable_header_hash: p.headerHash,
            challenge_asset_name: p.challengeAssetName,
            removal_approach: desc
              ? {
                  PruneTimedOutBlockDescendant: {
                    confirmed_state_ref_input_index: requireReferenceInputIndex(
                      ctx,
                      root.utxo,
                      "root",
                    ),
                    timed_out_node_input_outref: outputReferenceFromUTxO(
                      q.utxo,
                    ),
                    timed_out_node_output_index: at(
                      ctx,
                      continuedIndex,
                      outputs[continuedIndex]!,
                      "continued unavailable head",
                    ),
                  },
                }
              : {
                  RemoveTimedOutHead: {
                    confirmed_state_input_outref: outputReferenceFromUTxO(
                      root.utxo,
                    ),
                    confirmed_state_output_index: at(
                      ctx,
                      continuedIndex,
                      outputs[continuedIndex]!,
                      "continued root",
                    ),
                  },
                },
          },
        },
        StateQueueRedeemer,
      );
    }) satisfies BuildTxWithRedeemer)
    .withdraw(
      scriptRewardAddress(
        lucid.config().network ?? fail("Missing network"),
        d.contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
      ),
      0n,
      Data.void(),
    );
  return complete(lucid, d, p, pay(tx, outputs), {
    action: initial ? "timeout" : desc ? "prune" : "remove",
    headerHash: p.headerHash,
    challengeAssetName: p.challengeAssetName,
    inputs,
    refs,
    outputs,
    ...(initial ? { timeoutFeePart: initial.feePart } : {}),
  });
};
