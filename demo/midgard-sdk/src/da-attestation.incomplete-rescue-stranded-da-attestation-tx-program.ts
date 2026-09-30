import { assetsEqual } from "@al-ft/midgard-core/assets";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  addressDataFromBech32,
  AddressSchema,
  type MidgardValidators,
} from "./common.js";
import {
  DaAttestationBuildError,
  type DaAttestationReferenceScripts,
  type DaAttestationUtxo,
  failBuild,
} from "./da-attestation.apply-da-attestation-signature-witnesses.js";
import {
  daAttestationIsStranded,
  DaAttestationMintRedeemer,
  DaAttestationSpendRedeemer,
  daAttestationUnit,
  DaParamsDatum,
} from "./da-attestation.da-attestation-is-stranded.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";

/**
 * Refunds an attestation that a mid-flight committee rotation stranded
 * (decision row D-DA5 clause c).
 *
 * The transaction burns the DAAT and pays the attestation's entire remaining
 * value to one address. There is no state-queue leg: a stranded attestation is
 * by definition one that can never reach quorum, so there is nothing to attach.
 */
export const incompleteRescueStrandedDaAttestationTxProgram = (
  lucid: LucidEvolution,
  contracts: Pick<MidgardValidators, "daAttestation">,
  config: {
    readonly daParamsUtxo: UTxO;
    readonly daParamsDatum: DaParamsDatum;
    readonly attestation: DaAttestationUtxo;
    readonly refundAddress: string;
    readonly referenceScripts: Pick<
      DaAttestationReferenceScripts,
      "daAttestationMinting" | "daAttestationSpending"
    >;
  },
): Effect.Effect<TxBuilder, DaAttestationBuildError> =>
  Effect.gen(function* () {
    if (
      !daAttestationIsStranded({
        attestationDatum: config.attestation.datum,
        daParamsDatum: config.daParamsDatum,
      })
    ) {
      return yield* failBuild(
        "attestation_not_stranded",
        "DA attestation is not stranded: its committee is still the governed one, so it may still be signed and applied",
        `frozen=${config.attestation.datum.committee_signers_hash},governed=${config.daParamsDatum.committee_signers_hash}`,
      );
    }
    if (
      config.refundAddress === contracts.daAttestation.spendingScriptAddress
    ) {
      return yield* failBuild(
        "rescue_refund_to_attestation_script",
        "DA attestation rescue refund may not return to the attestation script; the burnt DAAT would leave it unspendable",
        config.refundAddress,
      );
    }
    const refundAddressData = yield* addressDataFromBech32(
      config.refundAddress,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new DaAttestationBuildError({
            reason: "rescue_refund_address_undecodable",
            message: "Failed to decode DA attestation rescue refund address",
            cause,
          }),
      ),
    );
    const encodedRefundAddress = Data.to(
      refundAddressData as never,
      AddressSchema as never,
    );
    const encodedBeneficiary = Data.to(
      config.attestation.datum.rescue_beneficiary as never,
      AddressSchema as never,
    );
    if (encodedRefundAddress !== encodedBeneficiary) {
      return yield* failBuild(
        "rescue_refund_beneficiary_mismatch",
        "DA attestation rescue refund address does not match the frozen beneficiary",
        config.refundAddress,
      );
    }

    const attestationUnit = daAttestationUnit(
      contracts.daAttestation,
      config.attestation.datum.header_hash,
    );
    const refundAssets = Object.fromEntries(
      Object.entries(config.attestation.utxo.assets).filter(
        ([unit]) => unit !== attestationUnit,
      ),
    );

    const rescueMintRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        contracts.daAttestation.policyId,
        "DA attestation rescue mint",
      );
      return Data.to(
        {
          RescueStrandedAttestation: {
            da_attestation_input_index: requireInputIndex(
              ctx,
              config.attestation.utxo,
              "DA attestation rescue DA attestation",
            ),
            da_params_ref_input_index: requireReferenceInputIndex(
              ctx,
              config.daParamsUtxo,
              "DA attestation rescue DA params",
            ),
            refund_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === config.refundAddress &&
                assetsEqual(output.assets, refundAssets),
              "DA attestation rescue refund",
            ),
          },
        } satisfies DaAttestationMintRedeemer as never,
        DaAttestationMintRedeemer as never,
      );
    }) satisfies BuildTxWithRedeemer;

    const rescueSpendRedeemer = ((ctx) =>
      Data.to(
        {
          BurnForRescue: {
            mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.daAttestation.policyId,
              "DA attestation rescue DA attestation mint",
            ),
          },
        } satisfies DaAttestationSpendRedeemer as never,
        DaAttestationSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;

    return lucid
      .newTx()
      .readFrom([
        config.daParamsUtxo,
        config.referenceScripts.daAttestationMinting,
        config.referenceScripts.daAttestationSpending,
      ])
      .collectFrom([config.attestation.utxo], rescueSpendRedeemer)
      .pay.ToAddress(config.refundAddress, refundAssets)
      .mintAssets({ [attestationUnit]: -1n }, rescueMintRedeemer);
  });
