import { assetsEqual } from "@al-ft/midgard-core/assets";
import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  Data,
  type LucidEvolution,
  type TxOutput,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";

import type { DaAttestationValidatorSet } from "../l1/deployment.js";
import type { DaAttestationReferenceScripts } from "../l1/reference-scripts.js";
import {
  DaBondPoolApplyBackoffError,
  isDaBondPoolApplyBackoffReason,
} from "./pool-backoff.js";
import { isSignerBitSet, setSignerBit } from "./witnesses.js";

type CompletableTx = {
  readonly complete: (options?: {
    readonly localUPLCEval?: boolean;
  }) => Promise<TxSignBuilder>;
};

type RedeemerContextLike = {
  readonly outputs: readonly TxOutput[];
};

export type DaAttestationTarget = {
  readonly stateQueueUtxo: SDK.StateQueueUTxO;
  readonly stateQueueNode: SDK.StateQueueNode;
  readonly headerHash: string;
};

export type DaAttestationCandidate = {
  readonly utxo: UTxO;
  readonly datum: SDK.DaAttestationDatum;
};

/**
 * The widest `attestation_count` an attestation can reach: the committee is at
 * most 256 signers, and 256 is the first count whose CBOR integer takes three
 * bytes. Add-signatures must carry the attestation's value unchanged, so the
 * init locks enough for the attestation at its widest.
 */
export const DA_ATTESTATION_WIDEST_COUNT = 256n;

/**
 * The lovelace the init locks in the attestation output: the min-UTxO of that
 * output with `attestation_count` at {@link DA_ATTESTATION_WIDEST_COUNT}, so no
 * later add-signatures (which keeps the value and grows only the count) falls
 * below the ledger minimum. The attestation carries no bond; apply and rescue
 * return all of it to the rescue beneficiary.
 */
export const daAttestationInitOutputLovelace = ({
  attestationAddress,
  attestationUnit,
  attestationDatum,
  coinsPerUtxoByte,
}: {
  readonly attestationAddress: string;
  readonly attestationUnit: string;
  readonly attestationDatum: SDK.DaAttestationDatum;
  readonly coinsPerUtxoByte: bigint;
}): bigint =>
  calculateMinLovelaceFromUTxO(coinsPerUtxoByte, {
    txHash: "00".repeat(32),
    outputIndex: 0,
    address: attestationAddress,
    assets: { lovelace: 0n, [attestationUnit]: 1n },
    datum: Data.to(
      {
        ...attestationDatum,
        attestation_count: DA_ATTESTATION_WIDEST_COUNT,
      } as never,
      SDK.DaAttestationDatum as never,
    ),
  });

/**
 * Builds the DA attestation init through the SDK builder. The output locks its
 * own min-UTxO (sized by {@link daAttestationInitOutputLovelace}) and no bond:
 * the committee's bond is the pooled DA bond that apply reads.
 */
export const buildInitDaAttestationTx = async ({
  lucid,
  contracts,
  daParamsUtxo,
  daParamsDatum,
  target,
  referenceScripts,
  rescueBeneficiary,
  availabilityCommitment,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: Pick<DaAttestationValidatorSet, "daAttestation">;
  readonly daParamsUtxo: UTxO;
  readonly daParamsDatum: SDK.DaParamsDatum;
  readonly target: DaAttestationTarget;
  readonly referenceScripts: Pick<
    DaAttestationReferenceScripts,
    "daAttestationMinting" | "stateQueueMinting"
  >;
  readonly rescueBeneficiary: SDK.AddressData;
  readonly availabilityCommitment: SDK.DaAvailabilityCommitment;
}): Promise<TxSignBuilder> => {
  const coinsPerUtxoByte = lucid.config().protocolParameters?.coinsPerUtxoByte;
  if (coinsPerUtxoByte === undefined) {
    throw new Error(
      "DA attestation init requires live protocol parameters for the output's min-UTxO",
    );
  }
  const attestationOutputLovelace = daAttestationInitOutputLovelace({
    attestationAddress: contracts.daAttestation.spendingScriptAddress,
    attestationUnit: SDK.daAttestationUnit(
      contracts.daAttestation,
      target.headerHash,
    ),
    attestationDatum: {
      header_hash: target.headerHash,
      availability_commitment: availabilityCommitment,
      da_threshold: daParamsDatum.da_threshold,
      committee_signers_hash: daParamsDatum.committee_signers_hash,
      rescue_beneficiary: rescueBeneficiary,
      attested_signers: SDK.EMPTY_ATTESTED_SIGNER_BITMAP,
      attestation_count: 0n,
    },
    coinsPerUtxoByte: BigInt(coinsPerUtxoByte),
  });
  return completeWithLocalUplc(
    await runBuild(
      SDK.incompleteInitDaAttestationTxProgram(lucid, contracts, {
        daParamsUtxo,
        daParamsDatum,
        target,
        referenceScripts,
        attestationOutputLovelace,
        rescueBeneficiary,
        availabilityCommitment,
      }),
    ),
    "DA attestation init",
  );
};

export const buildAddSignaturesTx = async ({
  lucid,
  contracts,
  daParamsUtxo,
  attestationUtxo,
  attestationDatum,
  packedWitnessesHex,
  signerIndexes,
  referenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DaAttestationValidatorSet;
  readonly daParamsUtxo: UTxO;
  readonly attestationUtxo: UTxO;
  readonly attestationDatum: SDK.DaAttestationDatum;
  readonly packedWitnessesHex: string;
  readonly signerIndexes: readonly number[];
  readonly referenceScripts: DaAttestationReferenceScripts;
}): Promise<TxSignBuilder> => {
  const updatedDatum = addSignaturesToDaAttestationDatum(
    attestationDatum,
    signerIndexes,
  );
  const encodedUpdatedDatum = Data.to(
    updatedDatum as never,
    SDK.DaAttestationDatum as never,
  );
  const addSignaturesRedeemer = ((ctx: RedeemerContextLike) =>
    Data.to(
      {
        AddSignatures: {
          output_index: SDK.requireUniqueOutputIndex(
            ctx.outputs,
            (output: TxOutput) =>
              output.address ===
                contracts.daAttestation.spendingScriptAddress &&
              outputDatumCborMatches(output, encodedUpdatedDatum) &&
              assetsEqual(output.assets, attestationUtxo.assets),
            "DA attestation add-signatures",
          ),
          da_params_ref_input_index: SDK.requireReferenceInputIndex(
            ctx as never,
            daParamsUtxo,
            "DA attestation add-signatures DA params",
          ),
          signatures: packedWitnessesHex,
        },
      } satisfies SDK.DaAttestationSpendRedeemer as never,
      SDK.DaAttestationSpendRedeemer as never,
    )) as never;

  return completeWithLocalUplc(
    lucid
      .newTx()
      .readFrom([daParamsUtxo, referenceScripts.daAttestationSpending])
      .collectFrom([attestationUtxo], addSignaturesRedeemer)
      .pay.ToContract(
        contracts.daAttestation.spendingScriptAddress,
        { kind: "inline", value: encodedUpdatedDatum },
        attestationUtxo.assets,
      ),
    "DA attestation add-signatures",
  );
};

/**
 * Builds the DA attestation apply through the SDK builder, which fetches the
 * pooled DA bond itself immediately before assembling (a pool outref captured
 * earlier can already be spent by a top-up).
 *
 * A pool the validator would refuse (under-backed, withdrawing) or that cannot
 * be read rejects with {@link DaBondPoolApplyBackoffError}; every other build
 * failure rejects with the SDK's error unchanged.
 */
export const buildApplyAttestationTx = async ({
  lucid,
  contracts,
  target,
  attestationUtxo,
  attestationDatum,
  daParamsUtxo,
  daParamsDatum,
  referenceScripts,
  validityRange,
  availabilityParameters,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: Pick<
    DaAttestationValidatorSet,
    "daAttestation" | "daBondPool" | "stateQueue"
  >;
  readonly target: DaAttestationTarget;
  readonly attestationUtxo: UTxO;
  readonly attestationDatum: SDK.DaAttestationDatum;
  readonly daParamsUtxo: UTxO;
  readonly daParamsDatum: SDK.DaParamsDatum;
  readonly referenceScripts: DaAttestationReferenceScripts;
  /** From {@link daAttestationApplyValidityRange}. */
  readonly validityRange: DaAttestationApplyValidityRange;
  /** The deployment's parameters; the pool check reads the bond and floor. */
  readonly availabilityParameters: SDK.DaAvailabilityParameters;
}): Promise<TxSignBuilder> => {
  const result = await Effect.runPromise(
    SDK.incompleteApplyDaAttestationToStateQueueTxProgram(lucid, contracts, {
      daParamsUtxo,
      daParamsDatum,
      target,
      attestation: {
        utxo: attestationUtxo,
        datum: attestationDatum,
      },
      referenceScripts,
      validityRange,
      availabilityParameters,
    }).pipe(Effect.either),
  );
  if (Either.isLeft(result)) {
    const error = result.left;
    if (isDaBondPoolApplyBackoffReason(error.reason)) {
      throw new DaBondPoolApplyBackoffError(
        error.reason,
        `${error.message}: ${causeText(error.cause)}`,
      );
    }
    throw error;
  }
  return completeWithLocalUplc(result.right, "DA attestation apply");
};

export type DaAttestationApplyValidityRange = {
  readonly validFrom: bigint;
  readonly validTo: bigint;
};

/**
 * The apply validity range for `target` at `currentTime`, derived by the SDK
 * exactly as the node derives it.
 *
 * Rejects with the SDK's `DaAttestationBuildError` itself (reason
 * `validity_range_past_deadline`) once the attestation deadline has passed,
 * so a caller can recognise that no later attempt for this header can land.
 */
export const daAttestationApplyValidityRange = async ({
  target,
  currentTime,
}: {
  readonly target: DaAttestationTarget;
  readonly currentTime: bigint;
}): Promise<DaAttestationApplyValidityRange> => {
  const result = await Effect.runPromise(
    SDK.daAttestationApplyValidityRangeProgram({
      currentTime,
      headerEndTime: target.stateQueueNode.header.endTime,
    }).pipe(Effect.either),
  );
  if (Either.isLeft(result)) {
    throw result.left;
  }
  return result.right;
};

export const addSignaturesToDaAttestationDatum = (
  attestationDatum: SDK.DaAttestationDatum,
  signerIndexes: readonly number[],
): SDK.DaAttestationDatum => {
  if (signerIndexes.length === 0) {
    throw new Error("at least one signer index is required");
  }
  for (const signerIndex of signerIndexes) {
    if (isSignerBitSet(attestationDatum.attested_signers, signerIndex)) {
      throw new Error(
        `DA signer ${signerIndex.toString()} is already attested for this header`,
      );
    }
  }
  const updatedAttestedSigners = signerIndexes.reduce(
    (bitmap, signerIndex) => setSignerBit(bitmap, signerIndex),
    attestationDatum.attested_signers,
  );
  const inputCount = countSetBits(attestationDatum.attested_signers);
  const outputCount = countSetBits(updatedAttestedSigners);
  if (outputCount !== inputCount + BigInt(signerIndexes.length)) {
    throw new Error("DA signer indexes must identify distinct new witnesses");
  }
  return {
    ...attestationDatum,
    attested_signers: updatedAttestedSigners,
    attestation_count: outputCount,
  };
};

const runBuild = async <A, E extends Error>(
  program: Effect.Effect<A, E>,
): Promise<A> => {
  const result = await Effect.runPromise(program.pipe(Effect.either));
  if (Either.isLeft(result)) {
    throw result.left;
  }
  return result.right;
};

const causeText = (cause: unknown): string =>
  cause instanceof Error
    ? cause.cause === undefined
      ? cause.message
      : `${cause.message}: ${causeText(cause.cause)}`
    : String(cause);

const outputDatumCborMatches = (
  output: Pick<TxOutput, "datum">,
  datumCbor: string,
): boolean =>
  output.datum != null &&
  canonicalPlutusDataCbor(output.datum) === canonicalPlutusDataCbor(datumCbor);

const completeWithLocalUplc = async (
  tx: CompletableTx,
  label: string,
): Promise<TxSignBuilder> => {
  try {
    return await tx.complete({ localUPLCEval: true });
  } catch (error) {
    throw new Error(
      `Failed to build ${label} transaction with local UPLC evaluation`,
      { cause: error },
    );
  }
};

const countSetBits = (hex: string): bigint => {
  let count = 0n;
  for (const byte of Buffer.from(hex, "hex")) {
    let value = byte;
    while (value !== 0) {
      count += BigInt(value & 1);
      value >>= 1;
    }
  }
  return count;
};
