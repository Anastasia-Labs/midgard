import { assetsEqual } from "@al-ft/midgard-core/assets";
import {
  type BuildTxWithRedeemer,
  type Credential,
  Data,
  type LucidEvolution,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  assertCanonicalDaAvailabilityCommitment,
  type DaAvailabilityCommitment,
} from "./availability-challenge.js";
import {
  type AddressData,
  type CredentialD,
  type MidgardValidators,
} from "./common.js";
import {
  applyDaAttestationSignatureWitnesses,
  committeeSizeFromParamsDatum,
  DaAttestationBuildError,
  type DaAttestationReferenceScripts,
  type DaAttestationSignatureWitness,
  type DaAttestationStateQueueTarget,
  type DaAttestationUtxo,
  failBuild,
} from "./da-attestation.apply-da-attestation-signature-witnesses.js";
import {
  DaAttestationDatum,
  DaAttestationMintRedeemer,
  DaAttestationSpendRedeemer,
  daAttestationUnit,
  DaParamsDatum,
  EMPTY_ATTESTED_SIGNER_BITMAP,
} from "./da-attestation.da-attestation-is-stranded.js";
import { MAX_VALIDITY_RANGE_LENGTH_MS } from "./protocol-parameters.js";
import { DA_ATTESTATION_TIMEOUT_MS } from "./state-queue.js";
import {
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";
import { outputDatumCborMatches } from "./tx-output-utils.js";

export const incompleteInitDaAttestationTxProgram = (
  lucid: LucidEvolution,
  contracts: Pick<MidgardValidators, "daAttestation">,
  config: {
    readonly daParamsUtxo: UTxO;
    readonly daParamsDatum: DaParamsDatum;
    readonly target: DaAttestationStateQueueTarget;
    readonly referenceScripts: Pick<
      DaAttestationReferenceScripts,
      "daAttestationMinting" | "stateQueueMinting"
    >;
    /**
     * Any amount at or above the output's min-UTxO: the attestation carries no
     * bond, and Apply or Rescue returns its whole value to
     * `rescueBeneficiary`.
     */
    readonly attestationOutputLovelace: bigint;
    readonly rescueBeneficiary: AddressData;
    readonly availabilityCommitment: DaAvailabilityCommitment;
  },
): Effect.Effect<TxBuilder, DaAttestationBuildError> =>
  Effect.gen(function* () {
    assertCanonicalDaAvailabilityCommitment(config.availabilityCommitment);
    if (
      config.availabilityCommitment.header_hash !== config.target.headerHash
    ) {
      return yield* failBuild(
        "availability_commitment_header_mismatch",
        "DA availability commitment header does not match state-queue target",
        `commitment=${config.availabilityCommitment.header_hash},target=${config.target.headerHash}`,
      );
    }
    const attestationUnit = daAttestationUnit(
      contracts.daAttestation,
      config.target.headerHash,
    );
    const attestationDatum: DaAttestationDatum = {
      header_hash: config.target.headerHash,
      availability_commitment: config.availabilityCommitment,
      da_threshold: config.daParamsDatum.da_threshold,
      committee_signers_hash: config.daParamsDatum.committee_signers_hash,
      rescue_beneficiary: config.rescueBeneficiary,
      attested_signers: EMPTY_ATTESTED_SIGNER_BITMAP,
      attestation_count: 0n,
    };
    const encodedAttestationDatum = Data.to(
      attestationDatum as never,
      DaAttestationDatum as never,
    );
    const initRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        contracts.daAttestation.policyId,
        "DA attestation init",
      );
      return Data.to(
        {
          Init: {
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address ===
                  contracts.daAttestation.spendingScriptAddress &&
                outputDatumCborMatches(output, encodedAttestationDatum) &&
                (output.assets[attestationUnit] ?? 0n) === 1n,
              "DA attestation init",
            ),
            da_params_ref_input_index: requireReferenceInputIndex(
              ctx,
              config.daParamsUtxo,
              "DA attestation init DA params",
            ),
            state_queue_ref_input_index: requireReferenceInputIndex(
              ctx,
              config.target.stateQueueUtxo.utxo,
              "DA attestation init state queue",
            ),
            state_queue_mint_ref_script_input_index: requireReferenceInputIndex(
              ctx,
              config.referenceScripts.stateQueueMinting,
              "DA attestation init state_queue mint reference script",
            ),
          },
        } satisfies DaAttestationMintRedeemer as never,
        DaAttestationMintRedeemer as never,
      );
    }) satisfies BuildTxWithRedeemer;

    return lucid
      .newTx()
      .readFrom([
        config.daParamsUtxo,
        config.target.stateQueueUtxo.utxo,
        config.referenceScripts.daAttestationMinting,
        config.referenceScripts.stateQueueMinting,
      ])
      .mintAssets({ [attestationUnit]: 1n }, initRedeemer)
      .pay.ToContract(
        contracts.daAttestation.spendingScriptAddress,
        { kind: "inline", value: encodedAttestationDatum },
        {
          lovelace: config.attestationOutputLovelace,
          [attestationUnit]: 1n,
        },
      );
  });

export const incompleteAddDaAttestationSignaturesTxProgram = (
  lucid: LucidEvolution,
  contracts: Pick<MidgardValidators, "daAttestation">,
  config: {
    readonly daParamsUtxo: UTxO;
    readonly daParamsDatum: DaParamsDatum;
    readonly attestation: DaAttestationUtxo;
    readonly witnesses: readonly DaAttestationSignatureWitness[];
    readonly referenceScripts: Pick<
      DaAttestationReferenceScripts,
      "daAttestationSpending"
    >;
  },
): Effect.Effect<TxBuilder, DaAttestationBuildError> =>
  Effect.gen(function* () {
    if (
      config.attestation.datum.da_threshold !==
      config.daParamsDatum.da_threshold
    ) {
      return yield* failBuild(
        "params_threshold_mismatch",
        "DA attestation datum threshold does not match DA params",
        `attestation=${config.attestation.datum.da_threshold.toString()},params=${config.daParamsDatum.da_threshold.toString()}`,
      );
    }
    if (
      config.attestation.datum.committee_signers_hash !==
      config.daParamsDatum.committee_signers_hash
    ) {
      return yield* failBuild(
        "params_committee_hash_mismatch",
        "DA attestation datum committee hash does not match DA params",
        `attestation=${config.attestation.datum.committee_signers_hash},params=${config.daParamsDatum.committee_signers_hash}`,
      );
    }
    const committeeSize = yield* committeeSizeFromParamsDatum(
      config.daParamsDatum,
    );
    const applied = yield* applyDaAttestationSignatureWitnesses({
      attestedSignersHex: config.attestation.datum.attested_signers,
      witnesses: config.witnesses,
      committeeSize,
    });
    const updatedDatum: DaAttestationDatum = {
      ...config.attestation.datum,
      attested_signers: applied.attestedSigners,
      attestation_count: applied.attestationCount,
    };
    const encodedUpdatedDatum = Data.to(
      updatedDatum as never,
      DaAttestationDatum as never,
    );
    const addSignaturesRedeemer = ((ctx) =>
      Data.to(
        {
          AddSignatures: {
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address ===
                  contracts.daAttestation.spendingScriptAddress &&
                outputDatumCborMatches(output, encodedUpdatedDatum) &&
                assetsEqual(output.assets, config.attestation.utxo.assets),
              "DA attestation add-signatures",
            ),
            da_params_ref_input_index: requireReferenceInputIndex(
              ctx,
              config.daParamsUtxo,
              "DA attestation add-signatures DA params",
            ),
            signatures: applied.packedWitnesses,
          },
        } satisfies DaAttestationSpendRedeemer as never,
        DaAttestationSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;

    return lucid
      .newTx()
      .readFrom([
        config.daParamsUtxo,
        config.referenceScripts.daAttestationSpending,
      ])
      .collectFrom([config.attestation.utxo], addSignaturesRedeemer)
      .pay.ToContract(
        contracts.daAttestation.spendingScriptAddress,
        { kind: "inline", value: encodedUpdatedDatum },
        config.attestation.utxo.assets,
      );
  });

/**
 * How far before the submitter's clock the DA apply validity range opens.
 *
 * The ledger checks the lower bound against the chain's slot (the mempool uses
 * tip slot + 1), and the tip trails the wall clock by seconds on a live
 * network. A lower bound at the submitter's clock is therefore routinely in
 * the chain's future and the transaction is refused until a block lands,
 * burning the attestation deadline. On-chain, apply only bounds the range from
 * above, so opening it early costs nothing.
 */
export const DA_ATTESTATION_APPLY_SLOT_LAG_ALLOWANCE_MS = 60_000n;

/**
 * The validity range for the DA apply transaction: it opens
 * {@link DA_ATTESTATION_APPLY_SLOT_LAG_ALLOWANCE_MS} before `currentTime` and
 * closes at the earlier of the maximum range length and the attestation
 * deadline, so `validTo` never exceeds the deadline.
 *
 * Fails with `validity_range_past_deadline` when no inclusion window remains,
 * i.e. the deadline is at or before `currentTime`. That failure is terminal for
 * the header: no later attempt can produce a range the validator accepts.
 */
export const daAttestationApplyValidityRangeProgram = ({
  currentTime,
  headerEndTime,
}: {
  readonly currentTime: bigint;
  readonly headerEndTime: bigint;
}): Effect.Effect<
  { readonly validFrom: bigint; readonly validTo: bigint },
  DaAttestationBuildError
> =>
  Effect.gen(function* () {
    const deadline = headerEndTime + DA_ATTESTATION_TIMEOUT_MS;
    if (currentTime >= deadline) {
      return yield* failBuild(
        "validity_range_past_deadline",
        "DA attestation apply deadline has already elapsed",
        `current_time=${currentTime.toString()},deadline=${deadline.toString()}`,
      );
    }
    const validFrom = currentTime - DA_ATTESTATION_APPLY_SLOT_LAG_ALLOWANCE_MS;
    const maximumValidTo = validFrom + MAX_VALIDITY_RANGE_LENGTH_MS;
    return {
      validFrom,
      validTo: maximumValidTo < deadline ? maximumValidTo : deadline,
    };
  });

/**
 * Output positions of the apply transaction. Lucid keeps explicit outputs in
 * the order they are paid and appends change after them, so the state-queue
 * continuation is output 0 and the beneficiary refund is output 1. The
 * redeemer names both positionally (a value lookup could collide with a change
 * output at the beneficiary's own address) and each position is checked
 * against the final outputs before the index is written.
 */
export const DA_ATTESTATION_APPLY_STATE_QUEUE_OUTPUT_INDEX = 0;

export const DA_ATTESTATION_APPLY_REFUND_OUTPUT_INDEX = 1;

export const credentialFromData = (credential: CredentialD): Credential =>
  "PublicKeyCredential" in credential
    ? { type: "Key", hash: credential.PublicKeyCredential[0] }
    : { type: "Script", hash: credential.ScriptCredential[0] };
