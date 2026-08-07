import { assetsEqual } from "@al-ft/midgard-core/assets";
import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type TxOutput,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { DaAttestationValidatorSet } from "../l1/deployment.js";
import type { DaAttestationReferenceScripts } from "../l1/reference-scripts.js";
import { isSignerBitSet, setSignerBit } from "./witnesses.js";

const DA_ATTESTATION_OUTPUT_LOVELACE = 5_000_000n;

type Assets = Record<string, bigint>;

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
  readonly stateQueueNode: SDK.StateQueueNodeV1;
  readonly headerHash: string;
};

export type DaAttestationCandidate = {
  readonly utxo: UTxO;
  readonly datum: SDK.DaAttestationDatum;
};

export const buildInitDaAttestationTx = async ({
  lucid,
  contracts,
  daParamsUtxo,
  daParamsDatum,
  target,
  referenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DaAttestationValidatorSet;
  readonly daParamsUtxo: UTxO;
  readonly daParamsDatum: SDK.DaParamsDatum;
  readonly target: DaAttestationTarget;
  readonly referenceScripts: DaAttestationReferenceScripts;
}): Promise<TxSignBuilder> => {
  const attestationUnit = SDK.daAttestationUnit(
    contracts.daAttestation,
    target.headerHash,
  );
  const attestationAssets: Assets = {
    lovelace: DA_ATTESTATION_OUTPUT_LOVELACE,
    [attestationUnit]: 1n,
  };
  const attestationDatum: SDK.DaAttestationDatum = {
    header_hash: target.headerHash,
    da_threshold: daParamsDatum.da_threshold,
    committee_signers_hash: daParamsDatum.committee_signers_hash,
    attested_signers: SDK.EMPTY_ATTESTED_SIGNER_BITMAP,
    attestation_count: 0n,
  };
  const encodedAttestationDatum = Data.to(
    attestationDatum as never,
    SDK.DaAttestationDatum as never,
  );
  const initRedeemer = ((ctx: RedeemerContextLike) => {
    SDK.requireOwnMintPurpose(
      ctx as never,
      contracts.daAttestation.policyId,
      "DA attestation init",
    );
    return Data.to(
      {
        Init: {
          output_index: SDK.requireUniqueOutputIndex(
            ctx.outputs,
            (output: TxOutput) =>
              output.address ===
                contracts.daAttestation.spendingScriptAddress &&
              outputDatumCborMatches(output, encodedAttestationDatum) &&
              (output.assets[attestationUnit] ?? 0n) === 1n,
            "DA attestation init",
          ),
          da_params_ref_input_index: SDK.requireReferenceInputIndex(
            ctx as never,
            daParamsUtxo,
            "DA attestation init DA params",
          ),
          state_queue_ref_input_index: SDK.requireReferenceInputIndex(
            ctx as never,
            target.stateQueueUtxo.utxo,
            "DA attestation init state queue",
          ),
          state_queue_mint_ref_script_input_index:
            SDK.requireReferenceInputIndex(
              ctx as never,
              referenceScripts.stateQueueMinting,
              "DA attestation init state_queue mint reference script",
            ),
        },
      } satisfies SDK.DaAttestationMintRedeemer as never,
      SDK.DaAttestationMintRedeemer as never,
    );
  }) as never;

  return completeWithLocalUplc(
    lucid
      .newTx()
      .readFrom([
        daParamsUtxo,
        target.stateQueueUtxo.utxo,
        referenceScripts.daAttestationMinting,
        referenceScripts.stateQueueMinting,
      ])
      .mintAssets({ [attestationUnit]: 1n }, initRedeemer)
      .pay.ToContract(
        contracts.daAttestation.spendingScriptAddress,
        { kind: "inline", value: encodedAttestationDatum },
        attestationAssets,
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

export const buildApplyAttestationTx = async ({
  lucid,
  contracts,
  target,
  attestationUtxo,
  daParamsUtxo,
  referenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DaAttestationValidatorSet;
  readonly target: DaAttestationTarget;
  readonly attestationUtxo: UTxO;
  readonly daParamsUtxo: UTxO;
  readonly referenceScripts: DaAttestationReferenceScripts;
}): Promise<TxSignBuilder> => {
  const attestationUnit = SDK.daAttestationUnit(
    contracts.daAttestation,
    target.headerHash,
  );
  const updatedStateQueueDatum = SDK.encodeLinkedListNodeView({
    ...target.stateQueueUtxo.datum,
    data: SDK.castStateQueueNodeV1ToData({
      header: target.stateQueueNode.header,
      da_attestation: contracts.daAttestation.policyId,
    }) as SDK.LinkedListNodeView["data"],
  });
  const daMintRedeemer = ((ctx: RedeemerContextLike) => {
    SDK.requireOwnMintPurpose(
      ctx as never,
      contracts.daAttestation.policyId,
      "DA attestation apply mint",
    );
    return Data.to(
      {
        ApplyToStateQueue: {
          da_attestation_input_index: SDK.requireInputIndex(
            ctx as never,
            attestationUtxo,
            "DA attestation apply DA attestation",
          ),
          state_queue_input_index: SDK.requireInputIndex(
            ctx as never,
            target.stateQueueUtxo.utxo,
            "DA attestation apply state queue",
          ),
          state_queue_output_index: SDK.requireUniqueOutputIndex(
            ctx.outputs,
            (output: TxOutput) =>
              output.address === contracts.stateQueue.spendingScriptAddress &&
              outputDatumCborMatches(output, updatedStateQueueDatum) &&
              assetsEqual(output.assets, target.stateQueueUtxo.utxo.assets),
            "DA attestation apply state queue",
          ),
          da_params_ref_input_index: SDK.requireReferenceInputIndex(
            ctx as never,
            daParamsUtxo,
            "DA attestation apply DA params",
          ),
          state_queue_mint_ref_script_input_index:
            SDK.requireReferenceInputIndex(
              ctx as never,
              referenceScripts.stateQueueMinting,
              "DA attestation apply state_queue mint reference script",
            ),
        },
      } satisfies SDK.DaAttestationMintRedeemer as never,
      SDK.DaAttestationMintRedeemer as never,
    );
  }) as never;
  const daSpendRedeemer = ((ctx: RedeemerContextLike) =>
    Data.to(
      {
        BurnForStateQueue: {
          mint_redeemer_index: SDK.requireMintRedeemerIndex(
            ctx as never,
            contracts.daAttestation.policyId,
            "DA attestation apply DA attestation mint",
          ),
        },
      } satisfies SDK.DaAttestationSpendRedeemer as never,
      SDK.DaAttestationSpendRedeemer as never,
    )) as never;
  const stateQueueSpendRedeemer = ((ctx: RedeemerContextLike) =>
    Data.to(
      {
        AttachDaAttestation: {
          state_queue_input_index: SDK.requireInputIndex(
            ctx as never,
            target.stateQueueUtxo.utxo,
            "DA attestation apply state queue",
          ),
          da_attestation_mint_redeemer_index: SDK.requireMintRedeemerIndex(
            ctx as never,
            contracts.daAttestation.policyId,
            "DA attestation apply DA attestation mint",
          ),
        },
      } satisfies SDK.StateQueueSpendRedeemer as never,
      SDK.StateQueueSpendRedeemer as never,
    )) as never;

  return completeWithLocalUplc(
    lucid
      .newTx()
      .readFrom([
        daParamsUtxo,
        referenceScripts.daAttestationMinting,
        referenceScripts.daAttestationSpending,
        referenceScripts.stateQueueMinting,
        referenceScripts.stateQueueSpending,
      ])
      .collectFrom([attestationUtxo], daSpendRedeemer)
      .collectFrom([target.stateQueueUtxo.utxo], stateQueueSpendRedeemer)
      .pay.ToContract(
        contracts.stateQueue.spendingScriptAddress,
        { kind: "inline", value: updatedStateQueueDatum },
        target.stateQueueUtxo.utxo.assets,
      )
      .mintAssets({ [attestationUnit]: -1n }, daMintRedeemer),
    "DA attestation apply",
  );
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
