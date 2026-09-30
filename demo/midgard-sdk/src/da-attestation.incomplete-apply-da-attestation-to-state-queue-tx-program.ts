import { assetsEqual } from "@al-ft/midgard-core/assets";
import {
  type Assets,
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  daAvailabilityCommitmentHash,
  type DaAvailabilityParameters,
} from "./availability-challenge.js";
import { type MidgardValidators } from "./common.js";
import {
  DaAttestationBuildError,
  type DaAttestationReferenceScripts,
  type DaAttestationStateQueueTarget,
  type DaAttestationUtxo,
  failBuild,
} from "./da-attestation.apply-da-attestation-signature-witnesses.js";
import {
  DaAttestationMintRedeemer,
  DaAttestationSpendRedeemer,
  daAttestationUnit,
  DaParamsDatum,
} from "./da-attestation.da-attestation-is-stranded.js";
import {
  DA_ATTESTATION_APPLY_REFUND_OUTPUT_INDEX,
  DA_ATTESTATION_APPLY_STATE_QUEUE_OUTPUT_INDEX,
} from "./da-attestation.incomplete-init-da-attestation-tx-program.js";
import { rescueBeneficiaryBech32 } from "./da-attestation.rescue-beneficiary-bech32.js";
import { fetchDaBondPool } from "./da-bond-pool.js";
import { castStateQueueNodeToData } from "./ledger-state.js";
import {
  encodeLinkedListNodeView,
  type LinkedListNodeView,
} from "./linked-list.js";
import { MAX_VALIDITY_RANGE_LENGTH_MS } from "./protocol-parameters.js";
import {
  DA_ATTESTATION_TIMEOUT_MS,
  StateQueueSpendRedeemer,
} from "./state-queue.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
} from "./tx-context-redeemer.js";
import { outputDatumCborMatches } from "./tx-output-utils.js";

/**
 * Builds `ApplyToStateQueue`: burns the threshold-signed DAAT, moves the
 * state-queue node from `Unattested` to `Attested{commitment_hash}`, refunds
 * the attestation's value to its `rescue_beneficiary`, and reads the pooled
 * committee bond as a reference input. Nothing is minted under the
 * availability policy.
 *
 * The pool is fetched here, immediately before the transaction is assembled,
 * and never taken from the caller: any top-up, slash or withdrawal step spends
 * the pool UTxO, so a pool outref captured earlier can already be consumed
 * (hazard H8). A caller that loses the race resubmits by calling this again.
 *
 * Before building, the pool is checked the way the validator checks it:
 * a `Withdrawing` pool refuses with `pool-withdrawing`, and a pool whose
 * backing above its floor is below `da_bond_lovelace` refuses with
 * `pool-under-backed`. A pool that cannot be fetched and authenticated refuses
 * with `pool-unavailable`.
 */
export const incompleteApplyDaAttestationToStateQueueTxProgram = (
  lucid: LucidEvolution,
  contracts: Pick<
    MidgardValidators,
    "daAttestation" | "daBondPool" | "stateQueue"
  >,
  config: {
    readonly daParamsUtxo: UTxO;
    readonly daParamsDatum: DaParamsDatum;
    readonly target: DaAttestationStateQueueTarget;
    readonly attestation: DaAttestationUtxo;
    readonly referenceScripts: DaAttestationReferenceScripts;
    readonly validityRange: {
      readonly validFrom: bigint;
      readonly validTo: bigint;
    };
    /**
     * The deployment's `ParametersV1`, the same value compiled into the DA
     * attestation validator. The pool pre-check reads `da_bond_lovelace` and
     * `da_bond_pool_floor_lovelace` from it.
     */
    readonly availabilityParameters: DaAvailabilityParameters;
    /**
     * TEST-ONLY. Skips the `pool-withdrawing` and `pool-under-backed`
     * pre-build refusals (the pool is still fetched and referenced) so an
     * emulator negative can reach the validator's own refusal. Production
     * callers never set it: the build refusal is the legible form of a
     * transaction the chain would reject.
     */
    readonly skipPoolPrecheck?: true;
  },
): Effect.Effect<TxBuilder, DaAttestationBuildError> =>
  Effect.gen(function* () {
    if (
      config.validityRange.validTo < config.validityRange.validFrom ||
      config.validityRange.validTo - config.validityRange.validFrom >
        MAX_VALIDITY_RANGE_LENGTH_MS
    ) {
      return yield* failBuild(
        "invalid_validity_range",
        "DA attestation apply requires a short closed validity range",
        `valid_from=${config.validityRange.validFrom.toString()},valid_to=${config.validityRange.validTo.toString()},max_length=${MAX_VALIDITY_RANGE_LENGTH_MS.toString()}`,
      );
    }
    const attestationDeadline =
      config.target.stateQueueNode.header.endTime + DA_ATTESTATION_TIMEOUT_MS;
    if (config.validityRange.validTo > attestationDeadline) {
      return yield* failBuild(
        "validity_range_past_deadline",
        "DA attestation apply validity range exceeds the attestation deadline",
        `valid_to=${config.validityRange.validTo.toString()},deadline=${attestationDeadline.toString()}`,
      );
    }
    // Decision row D-DA4: committee rotation is retroactive, so apply now reads
    // the current governed params on-chain and requires the attestation's
    // frozen pair to still equal them. Refusing to build the transaction here
    // is not the enforcement — the validator is — but it turns a rotation into
    // a legible build error instead of a script failure at submission.
    //
    // The two branches are split for the diagnostic, not for the decision: they
    // are together exactly `daAttestationIsStranded`, so anything this refuses
    // to apply the rescue path can pick up. Nothing falls between them.
    if (
      config.attestation.datum.committee_signers_hash !==
      config.daParamsDatum.committee_signers_hash
    ) {
      return yield* failBuild(
        "committee_rotated",
        "DA committee rotated away from the attestation's frozen committee; this attestation can no longer apply and must be rescued",
        `frozen=${config.attestation.datum.committee_signers_hash},governed=${config.daParamsDatum.committee_signers_hash}`,
      );
    }
    if (
      config.attestation.datum.da_threshold !==
      config.daParamsDatum.da_threshold
    ) {
      return yield* failBuild(
        "threshold_changed",
        "DA threshold changed since the attestation froze it; this attestation can no longer apply and must be rescued",
        `frozen=${config.attestation.datum.da_threshold.toString()},governed=${config.daParamsDatum.da_threshold.toString()}`,
      );
    }
    if (config.attestation.datum.header_hash !== config.target.headerHash) {
      return yield* failBuild(
        "attestation_header_mismatch",
        "DA attestation header does not match state-queue target",
        `attestation=${config.attestation.datum.header_hash},target=${config.target.headerHash}`,
      );
    }
    if (
      config.attestation.datum.attestation_count <
      config.attestation.datum.da_threshold
    ) {
      return yield* failBuild(
        "threshold_not_reached",
        "DA attestation has not reached threshold",
        `attestation_count=${config.attestation.datum.attestation_count.toString()},threshold=${config.attestation.datum.da_threshold.toString()}`,
      );
    }
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* failBuild(
        "missing_network",
        "DA attestation apply requires a configured network",
        "lucid.config().network is undefined",
      );
    }
    const refundAddress = yield* rescueBeneficiaryBech32(
      network,
      config.attestation.datum.rescue_beneficiary,
    );
    const attestationUnit = daAttestationUnit(
      contracts.daAttestation,
      config.target.headerHash,
    );
    const refundAssets: Assets = Object.fromEntries(
      Object.entries(config.attestation.utxo.assets).filter(
        ([unit]) => unit !== attestationUnit,
      ),
    );
    const updatedStateQueueDatum = encodeLinkedListNodeView({
      ...config.target.stateQueueUtxo.datum,
      data: castStateQueueNodeToData({
        proven_fraud: config.target.stateQueueNode.proven_fraud,
        header: config.target.stateQueueNode.header,
        da_attestation: {
          Attested: {
            commitment_hash: daAvailabilityCommitmentHash(
              config.attestation.datum.availability_commitment,
            ),
          },
        },
      }) as LinkedListNodeView["data"],
    });

    // Hazard H8: the pool is re-read as late as possible, right before the
    // transaction is assembled.
    const pool = yield* Effect.tryPromise({
      try: () =>
        fetchDaBondPool(lucid, {
          policyId: contracts.daBondPool.policyId,
          address: contracts.daBondPool.spendingScriptAddress,
          parameters: config.availabilityParameters,
        }),
      catch: (cause) =>
        new DaAttestationBuildError({
          reason: "pool-unavailable",
          message:
            "DA attestation apply could not fetch the authentic DA bond pool",
          cause,
        }),
    });
    if (config.skipPoolPrecheck !== true) {
      if (pool.datum !== "Bonded") {
        return yield* failBuild(
          "pool-withdrawing",
          "The DA bond pool is withdrawing and backs no new attestation until the withdrawal is cancelled",
          `unlock_at=${pool.datum.Withdrawing.unlock_at.toString()}`,
        );
      }
      const backing = pool.backing ?? 0n;
      if (backing < config.availabilityParameters.da_bond_lovelace) {
        return yield* failBuild(
          "pool-under-backed",
          "The DA bond pool backs less than one DA bond above its floor; top it up before applying",
          `backing=${backing.toString()},da_bond=${config.availabilityParameters.da_bond_lovelace.toString()}`,
        );
      }
    }

    const daMintRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        contracts.daAttestation.policyId,
        "DA attestation apply mint",
      );
      const stateQueueOutput =
        ctx.outputs[DA_ATTESTATION_APPLY_STATE_QUEUE_OUTPUT_INDEX];
      if (
        stateQueueOutput === undefined ||
        stateQueueOutput.address !==
          contracts.stateQueue.spendingScriptAddress ||
        !outputDatumCborMatches(stateQueueOutput, updatedStateQueueDatum) ||
        !assetsEqual(
          stateQueueOutput.assets,
          config.target.stateQueueUtxo.utxo.assets,
        )
      ) {
        throw new Error(
          `DA attestation apply state queue output is not at position ${DA_ATTESTATION_APPLY_STATE_QUEUE_OUTPUT_INDEX.toString()}`,
        );
      }
      const refundOutput =
        ctx.outputs[DA_ATTESTATION_APPLY_REFUND_OUTPUT_INDEX];
      if (
        refundOutput === undefined ||
        refundOutput.address !== refundAddress ||
        !assetsEqual(refundOutput.assets, refundAssets)
      ) {
        throw new Error(
          `DA attestation apply beneficiary refund is not at position ${DA_ATTESTATION_APPLY_REFUND_OUTPUT_INDEX.toString()}`,
        );
      }
      return Data.to(
        {
          ApplyToStateQueue: {
            da_attestation_input_index: requireInputIndex(
              ctx,
              config.attestation.utxo,
              "DA attestation apply DA attestation",
            ),
            da_params_ref_input_index: requireReferenceInputIndex(
              ctx,
              config.daParamsUtxo,
              "DA attestation apply DA params",
            ),
            state_queue_input_index: requireInputIndex(
              ctx,
              config.target.stateQueueUtxo.utxo,
              "DA attestation apply state queue",
            ),
            state_queue_output_index: BigInt(
              DA_ATTESTATION_APPLY_STATE_QUEUE_OUTPUT_INDEX,
            ),
            state_queue_mint_ref_script_input_index: requireReferenceInputIndex(
              ctx,
              config.referenceScripts.stateQueueMinting,
              "DA attestation apply state_queue mint reference script",
            ),
            // Indexed over the ledger's sorted reference-input set.
            pool_ref_input_index: requireReferenceInputIndex(
              ctx,
              pool.utxo,
              "DA attestation apply DA bond pool",
            ),
            refund_output_index: BigInt(
              DA_ATTESTATION_APPLY_REFUND_OUTPUT_INDEX,
            ),
          },
        } satisfies DaAttestationMintRedeemer as never,
        DaAttestationMintRedeemer as never,
      );
    }) satisfies BuildTxWithRedeemer;
    const daSpendRedeemer = ((ctx) =>
      Data.to(
        {
          BurnForStateQueue: {
            mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.daAttestation.policyId,
              "DA attestation apply DA attestation mint",
            ),
          },
        } satisfies DaAttestationSpendRedeemer as never,
        DaAttestationSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;
    const stateQueueSpendRedeemer = ((ctx) =>
      Data.to(
        {
          AttachDaAttestation: {
            state_queue_input_index: requireInputIndex(
              ctx,
              config.target.stateQueueUtxo.utxo,
              "DA attestation apply state queue",
            ),
            da_attestation_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.daAttestation.policyId,
              "DA attestation apply DA attestation mint",
            ),
          },
        } satisfies StateQueueSpendRedeemer as never,
        StateQueueSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;

    // Output order is load-bearing: see the position constants above.
    return lucid
      .newTx()
      .validFrom(Number(config.validityRange.validFrom))
      .validTo(Number(config.validityRange.validTo))
      .readFrom([
        config.daParamsUtxo,
        pool.utxo,
        config.referenceScripts.daAttestationMinting,
        config.referenceScripts.daAttestationSpending,
        config.referenceScripts.stateQueueMinting,
        config.referenceScripts.stateQueueSpending,
      ])
      .collectFrom([config.attestation.utxo], daSpendRedeemer)
      .collectFrom([config.target.stateQueueUtxo.utxo], stateQueueSpendRedeemer)
      .pay.ToContract(
        contracts.stateQueue.spendingScriptAddress,
        { kind: "inline", value: updatedStateQueueDatum },
        config.target.stateQueueUtxo.utxo.assets,
      )
      .pay.ToAddress(refundAddress, refundAssets)
      .mintAssets({ [attestationUnit]: -1n }, daMintRedeemer);
  });
