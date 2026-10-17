import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  Data,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

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

// ---------------------------------------------------------------------------
// Fixture
// ---------------------------------------------------------------------------

export type AvailabilityFixture = Awaited<
  ReturnType<typeof createAvailabilityFixture>
>;

export type OpenAvailability = Awaited<ReturnType<typeof openAvailability>>;

/**
 * The genesis fixture represents an already deployed protocol and committed
 * block. No attestation, challenge, tranche, carrier or terminal asset is
 * seeded: every availability state below is produced by an evaluated ledger
 * transaction. The pooled DA bond is seeded at genesis unless
 * `options.seedPool` is `false`.
 *
 * Genesis output 0 is the hub one-shot out-reference (`"00" * 32 #0`) the
 * contracts are parameterised with; it belongs to `oneShotHolder` and is never
 * spent by any other helper.
 */
export const createAvailabilityFixture = async (
  payloadBytes = 14_021,
  descendantCount = 0,
  /**
   * Places the target header's end time this far after fixture start. Setup
   * advances the emulator by several minutes, which can exceed the selected
   * profile's DA attestation timeout; a lead keeps apply's deadline ahead.
   */
  headerEndTimeLeadMs = 0,
  options: AvailabilityFixtureOptions = {},
): Promise<
  Omit<Awaited<ReturnType<typeof createFixture>>, "target"> & {
    target: NonNullable<Awaited<ReturnType<typeof createFixture>>["target"]>;
  }
> => {
  const { target, ...fixture } = await createFixture(
    payloadBytes,
    descendantCount,
    headerEndTimeLeadMs,
    options,
    false,
  );
  if (target === undefined) throw new Error("Incomplete genesis fixture");
  return { ...fixture, target };
};

export const queueUpdate = (
  ctx: AvailabilityLayout,
  policy: string,
  queue: UTxO,
  outputIndex: bigint,
) =>
  Data.to(
    {
      AvailabilityStatusUpdate: {
        state_queue_input_index: index(ctx, queue),
        state_queue_output_index: outputIndex,
        availability_mint_redeemer_index: mintIndex(ctx, policy),
      },
    },
    SDK.StateQueueSpendRedeemer,
  );

export const yieldTx = (
  f: AvailabilityFixture,
  tx: TxBuilder,
  arm: keyof AvailabilityFixture["contracts"]["availabilityChallenge"]["yields"],
) =>
  tx.withdraw(
    SDK.scriptRewardAddress(
      "Preprod",
      f.contracts.availabilityChallenge.yields[arm].withdrawalScript,
    ),
    0n,
    Data.void(),
  );

/**
 * A hand-built mirror of `OpenChallenge`, so negatives can vary one field.
 * Inputs: the challenger's exact funding coin and the Attested queue node.
 * Outputs: the record (0), the node now `Challenged` (1), the tranche
 * threads (2..), the terminal accumulator (last). The commitment is explicit:
 * it must be the preimage of the node's `Attested{commitment_hash}`.
 */
export const openAvailability = async (
  f: AvailabilityFixture,
  attested: {
    readonly queue: UTxO;
    readonly commitment: SDK.DaAvailabilityCommitment;
  },
  validity: { validFrom?: bigint; validTo?: bigint } = {},
) => {
  const { lucid, contracts } = f;
  const parameters = f.parameters;
  lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
  const fee = parameters.max_open_fee_lovelace;
  const fundingLovelace =
    parameters.challenger_bond_lovelace +
    parameters.challenge_record_lovelace +
    fee;
  const fundingOutputs = await f.submit(
    "prepare isolated challenger funding",
    lucid.newTx().pay.ToAddress(f.challenger.address, {
      lovelace: fundingLovelace,
    }),
    true,
  );
  const funding = fundingOutputs.find(
    (utxo) => utxo.assets.lovelace === fundingLovelace,
  );
  if (!funding) throw new Error("Missing isolated challenger funding");
  const validFrom = validity.validFrom ?? BigInt(f.emulator.now());
  const validTo = validity.validTo ?? validFrom + 60_000n;
  const queue = attested.queue;
  const queueView = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(queue),
  );
  const queueNode = Data.castFrom(queueView.data, SDK.StateQueueNode);
  const planAt = (
    openedAt: bigint,
    commitment: SDK.DaAvailabilityCommitment = attested.commitment,
  ) =>
    SDK.buildDaAvailabilityChallengeDatumPlan({
      commitment,
      challengerFundingOutRef: outRef(funding),
      challenger: f.challengerKey,
      openedAt,
      parameters,
    });
  // The validator anchors the response window at the inclusive upper validity
  // bound; the ledger's upper end is exclusive.
  const plan = planAt(validTo - 1n);
  const policy = contracts.availabilityChallenge.policyId;
  const address = contracts.availabilityChallenge.spendingScriptAddress;
  const terminalUnit =
    policy +
    SDK.daAvailabilityTerminalAccumulatorAssetName(plan.challengeAssetName);
  const build = (
    options: {
      omitSigner?: boolean;
      omitYield?: boolean;
      wrongYield?: boolean;
      /** Datums anchored at this `opened_at` instead of the upper bound. */
      anchorAt?: bigint;
      /**
       * Records (and marks the node Challenged with) this commitment instead
       * of the preimage of the node's `Attested{commitment_hash}`.
       */
      commitment?: SDK.DaAvailabilityCommitment;
    } = {},
  ) => {
    const outputs = planAt(
      options.anchorAt ?? validTo - 1n,
      options.commitment ?? attested.commitment,
    );
    const challengedQueue = SDK.encodeLinkedListNodeView({
      ...queueView,
      data: SDK.castStateQueueNodeToData({
        ...queueNode,
        da_attestation: {
          Challenged: {
            commitment_hash: SDK.daAvailabilityCommitmentHash(
              outputs.record.commitment,
            ),
            challenge_asset_name: plan.challengeAssetName,
          },
        },
      }) as SDK.LinkedListNodeView["data"],
    });
    const yieldReference = f.reference(
      options.wrongYield
        ? "availability-challenge close withdrawal"
        : "availability-challenge open withdrawal",
    );
    const ctx: AvailabilityLayout = {
      inputs: [funding, queue],
      policies: [policy],
      references: [
        f.hubOracleRefInput,
        f.reference("availability-challenge minting"),
        yieldReference,
        f.reference("state-queue spending"),
      ],
    };
    const mint: Assets = {
      [policy + plan.challengeAssetName]: 1n,
      [terminalUnit]: 1n,
    };
    for (let i = 0; i < plan.trancheThreads.length; i += 1)
      mint[
        policy +
          SDK.daAvailabilityTrancheAssetName({
            challengeAssetName: plan.challengeAssetName,
            trancheIndex: i,
          })
      ] = 1n;
    let tx = lucid
      .newTx()
      .setMinFee(fee)
      .validFrom(Number(validFrom))
      .validTo(Number(validTo))
      .collectFrom([funding])
      .collectFrom([queue], queueUpdate(ctx, policy, queue, 1n))
      .readFrom([...ctx.references])
      .mintAssets(
        mint,
        Data.to(
          {
            OpenChallenge: {
              yield_to_ref_input_index: refIndex(ctx, yieldReference),
              hub_oracle_ref_input_index: refIndex(ctx, f.hubOracleRefInput),
              record_output_index: 0n,
              challenger_input_index: index(ctx, funding),
              state_queue_input_index: index(ctx, queue),
              state_queue_output_index: 1n,
              first_tranche_output_index: 2n,
              terminal_accumulator_output_index: BigInt(
                2 + plan.trancheThreads.length,
              ),
              challenger: f.challengerKey,
            },
          },
          SDK.DaAvailabilityMintRedeemer,
        ),
      )
      .pay.ToContract(
        address,
        inline(
          SDK.encodeDaAvailabilityChallengeRecord(outputs.record, parameters),
        ),
        {
          lovelace: outputs.recordLovelace,
          [policy + plan.challengeAssetName]: 1n,
        },
      )
      .pay.ToContract(queue.address, inline(challengedQueue), queue.assets);
    for (let i = 0; i < plan.trancheThreads.length; i += 1)
      tx = tx.pay.ToContract(
        address,
        inline(
          SDK.encodeDaAvailabilityTrancheDatum(outputs.trancheThreads[i]!),
        ),
        {
          lovelace: plan.trancheFunding[i]!.initialLovelace,
          [policy +
          SDK.daAvailabilityTrancheAssetName({
            challengeAssetName: plan.challengeAssetName,
            trancheIndex: i,
          })]: 1n,
        },
      );
    tx = tx.pay.ToContract(
      address,
      inline(
        SDK.encodeDaAvailabilityTerminalAccumulatorDatum(
          outputs.terminalAccumulator,
        ),
      ),
      { lovelace: plan.terminalAccumulatorFundingLovelace, [terminalUnit]: 1n },
    );
    if (!options.omitSigner) tx = tx.addSignerKey(f.challengerKey);
    if (!options.omitYield)
      tx = yieldTx(f, tx, options.wrongYield ? "close" : "open");
    return tx;
  };
  return {
    attested,
    funding,
    plan,
    policy,
    address,
    terminalUnit,
    build,
    async submit() {
      const outputs = await f.submit(
        `open ${plan.trancheThreads.length} tranches`,
        build(),
      );
      return {
        record: outputs[0]!,
        queue: outputs[1]!,
        threads: outputs.slice(2, 2 + plan.trancheThreads.length),
        terminal: outputs[2 + plan.trancheThreads.length]!,
      };
    },
  };
};
