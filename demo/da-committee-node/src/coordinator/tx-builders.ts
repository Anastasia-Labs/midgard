import { assetsEqual } from "@al-ft/midgard-core/assets";
import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  type LucidEvolution,
  type TxOutput,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { DaAttestationValidatorSet } from "../l1/deployment.js";
import type { DaAttestationReferenceScripts } from "../l1/reference-scripts.js";
import { setSignerBit } from "./witnesses.js";

const DA_ATTESTATION_OUTPUT_LOVELACE = 5_000_000n;

type Assets = Record<string, bigint>;

type CompletableTx = {
  readonly complete: (options?: {
    readonly localUPLCEval?: boolean;
    readonly evaluator?: TxEvaluator;
  }) => Promise<TxSignBuilder>;
};

type RedeemerContextLike = {
  readonly outputs: readonly TxOutput[];
};

type TxEvaluationRedeemer = {
  readonly redeemer_tag:
    | "spend"
    | "mint"
    | "publish"
    | "withdraw"
    | "vote"
    | "propose";
  readonly redeemer_index: number;
  readonly ex_units: {
    readonly mem: number;
    readonly steps: number;
  };
};

type TxEvaluator = {
  readonly name: string;
  readonly evaluate: (args: {
    readonly tx: string;
    readonly context: {
      readonly protocolParameters: {
        readonly maxTxExMem: bigint | number;
        readonly maxTxExSteps: bigint | number;
      };
    };
  }) => Promise<TxEvaluationRedeemer[]>;
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
    {
      providerEvalOnLocalBudgetFailure: true,
      bootstrapExUnitsOnProviderFailure: true,
    },
  );
};

export const buildApplyAttestationTx = async ({
  lucid,
  contracts,
  target,
  attestationUtxo,
  referenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DaAttestationValidatorSet;
  readonly target: DaAttestationTarget;
  readonly attestationUtxo: UTxO;
  readonly referenceScripts: DaAttestationReferenceScripts;
}): Promise<TxSignBuilder> => {
  const attestationUnit = SDK.daAttestationUnit(
    contracts.daAttestation,
    target.headerHash,
  );
  const updatedStateQueueDatum = SDK.encodeLinkedListNodeView({
    ...target.stateQueueUtxo.datum,
    data: SDK.castStateQueueNodeToData({
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
    {
      providerEvalOnLocalBudgetFailure: true,
      bootstrapExUnitsOnProviderFailure: true,
    },
  );
};

export const addSignaturesToDaAttestationDatum = (
  attestationDatum: SDK.DaAttestationDatum,
  signerIndexes: readonly number[],
): SDK.DaAttestationDatum => {
  if (signerIndexes.length === 0) {
    throw new Error("at least one signer index is required");
  }
  const updatedAttestedSigners = signerIndexes.reduce(
    (bitmap, signerIndex) => setSignerBit(bitmap, signerIndex),
    attestationDatum.attested_signers,
  );
  return {
    ...attestationDatum,
    attested_signers: updatedAttestedSigners,
    attestation_count: countSetBits(updatedAttestedSigners),
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
  options: {
    readonly providerEvalOnLocalBudgetFailure?: boolean;
    readonly bootstrapExUnitsOnProviderFailure?: boolean;
  } = {},
): Promise<TxSignBuilder> => {
  try {
    return await tx.complete({ localUPLCEval: true });
  } catch (error) {
    if (
      options.providerEvalOnLocalBudgetFailure === true &&
      isLocalUplcEvaluationFallbackError(error)
    ) {
      try {
        return await tx.complete({ localUPLCEval: false });
      } catch (providerError) {
        if (
          options.bootstrapExUnitsOnProviderFailure === true &&
          isLocalUplcEvaluationFallbackError(providerError)
        ) {
          return tx.complete({
            localUPLCEval: true,
            evaluator: bootstrapExUnitsEvaluator,
          });
        }
        throw new Error(`Failed to build ${label} transaction`, {
          cause: providerError,
        });
      }
    }
    throw new Error(`Failed to build ${label} transaction`, { cause: error });
  }
};

const isLocalUplcEvaluationFallbackError = (cause: unknown): boolean =>
  /over budget|ex-?units|exunits|couldn'?t decode plutus script|failed to decode payload from base64 or base16/i.test(
    cause instanceof Error ? (cause.stack ?? cause.message) : String(cause),
  );

const bootstrapExUnitsEvaluator: TxEvaluator = {
  name: "midgard-bootstrap-exunits",
  evaluate: async ({ tx, context }) => {
    const transaction = CML.Transaction.from_cbor_hex(tx);
    const redeemers = transaction.witness_set().redeemers();
    if (redeemers == null) {
      return [];
    }
    const keys = redeemerKeys(redeemers as never);
    const count = keys.length;
    if (count === 0) {
      return [];
    }
    return keys.map((key, index) => ({
      redeemer_tag: fromCmlRedeemerTag(key.tag),
      redeemer_index: safeNumber(key.index, "redeemer index"),
      ex_units: {
        mem: splitBudget(context.protocolParameters.maxTxExMem, count, index),
        steps: splitBudget(
          context.protocolParameters.maxTxExSteps,
          count,
          index,
        ),
      },
    }));
  },
};

type RedeemerKey = {
  readonly tag: unknown;
  readonly index: bigint;
};

const redeemerKeys = (redeemers: {
  readonly as_arr_legacy_redeemer: () =>
    | {
        readonly len: () => number;
        readonly get: (index: number) => {
          readonly tag: () => unknown;
          readonly index: () => bigint;
        };
      }
    | undefined;
  readonly as_map_redeemer_key_to_redeemer_val: () =>
    | {
        readonly keys: () => {
          readonly len: () => number;
          readonly get: (index: number) => {
            readonly tag: () => unknown;
            readonly index: () => bigint;
          };
        };
      }
    | undefined;
}): readonly RedeemerKey[] => {
  const keys: RedeemerKey[] = [];
  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    for (let index = 0; index < legacy.len(); index += 1) {
      const redeemer = legacy.get(index);
      keys.push({ tag: redeemer.tag(), index: redeemer.index() });
    }
  }
  const mapped = redeemers.as_map_redeemer_key_to_redeemer_val();
  if (mapped !== undefined) {
    const mapKeys = mapped.keys();
    for (let index = 0; index < mapKeys.len(); index += 1) {
      const key = mapKeys.get(index);
      keys.push({ tag: key.tag(), index: key.index() });
    }
  }
  return keys;
};

const fromCmlRedeemerTag = (
  tag: unknown,
): TxEvaluationRedeemer["redeemer_tag"] => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return "spend";
    case CML.RedeemerTag.Mint:
      return "mint";
    case CML.RedeemerTag.Cert:
      return "publish";
    case CML.RedeemerTag.Reward:
      return "withdraw";
    case CML.RedeemerTag.Voting:
      return "vote";
    case CML.RedeemerTag.Proposing:
      return "propose";
    default:
      throw new Error(`unsupported redeemer tag ${String(tag)}`);
  }
};

const splitBudget = (
  total: bigint | number,
  count: number,
  index: number,
): number => {
  const totalBudget = BigInt(total);
  const countBudget = BigInt(count);
  const indexBudget = BigInt(index);
  return safeNumber(
    totalBudget / countBudget +
      (indexBudget < totalBudget % countBudget ? 1n : 0n),
    "redeemer execution budget",
  );
};

const safeNumber = (value: bigint, label: string): number => {
  if (value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`${label} ${value.toString()} exceeds safe integer range`);
  }
  return Number(value);
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
