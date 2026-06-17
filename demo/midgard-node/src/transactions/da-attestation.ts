import { assetsEqual } from "@al-ft/midgard-core/assets";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  type Assets,
  type BuildTxWithRedeemer,
  type LucidEvolution,
  type Network,
  type TxOutput,
  type TxSignBuilder,
  type UTxO,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import { NodeConfig } from "@/services/config.js";
import { Lucid, MidgardContracts } from "@/services/index.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "@/transactions/reference-scripts.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { outRefLabel } from "@/tx-context.js";

const DA_ATTESTATION_OUTPUT_LOVELACE = 5_000_000n;
const UTXO_VISIBILITY_RETRY_DELAY = "2 seconds";
const UTXO_VISIBILITY_RETRY_COUNT = 12;
const OPERATOR_DA_SIGNER_INDEX = 0;

type CompleteOptions = {
  readonly localUPLCEval: boolean;
  readonly evaluator?: TxEvaluator;
};

type CompletableTx = {
  readonly complete: (options?: CompleteOptions) => Promise<TxSignBuilder>;
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

type DaAttestationReferenceScripts = {
  readonly daAttestationMinting: UTxO;
  readonly daAttestationSpending: UTxO;
  readonly stateQueueMinting: UTxO;
  readonly stateQueueSpending: UTxO;
};

type UnattestedStateQueueHeader = {
  readonly stateQueueUtxo: SDK.StateQueueUTxO;
  readonly stateQueueNode: SDK.StateQueueNode;
  readonly headerHash: string;
};

type DaAttestationCandidate = {
  readonly utxo: UTxO;
  readonly datum: SDK.DaAttestationDatum;
};

type OperatorDaConfig = {
  readonly L1_OPERATOR_SEED_PHRASE: string;
  readonly NETWORK: Network;
};

export type AttestStateQueueOnceOptions = {
  readonly headerHash?: string;
};

export type AttestStateQueueHeaderResult = {
  readonly headerHash: string;
  readonly initTxHash: string | null;
  readonly addSignaturesTxHash: string | null;
  readonly applyTxHash: string;
  readonly appliedAttestationOutRef: string;
  readonly candidateCount: number;
};

const outputDatumCborMatches = (
  output: Pick<TxOutput, "datum">,
  datumCbor: string,
): boolean =>
  output.datum != null &&
  canonicalPlutusDataCbor(output.datum) === canonicalPlutusDataCbor(datumCbor);

const decodeDatum = <T>(
  utxo: UTxO,
  schema: Parameters<typeof Data.from>[1],
  label: string,
): Effect.Effect<T, SDK.StateQueueError> =>
  Effect.try({
    try: () => {
      if (utxo.datum == null) {
        throw new Error(`${label} UTxO has no inline datum`);
      }
      return Data.from(utxo.datum, schema) as T;
    },
    catch: (cause) =>
      new SDK.StateQueueError({
        message: `Failed to decode ${label} datum`,
        cause,
      }),
  });

const isLocalUplcEvaluationFallbackError = (cause: unknown): boolean =>
  /over budget|ex-?units|exunits|couldn'?t decode plutus script/i.test(
    formatUnknownError(cause, { includeCause: true }),
  );

const completeWithLocalUplc = (
  tx: CompletableTx,
  label: string,
  options: {
    readonly providerEvalOnLocalBudgetFailure?: boolean;
    readonly bootstrapExUnitsOnProviderFailure?: boolean;
  } = {},
): Effect.Effect<TxSignBuilder, SDK.LucidError> =>
  Effect.tryPromise({
    try: () => tx.complete({ localUPLCEval: true }),
    catch: (cause) =>
      new SDK.LucidError({
        message: `Failed to build ${label} transaction`,
        cause,
      }),
  }).pipe(
    Effect.catchAll((localError) =>
      Effect.gen(function* () {
        if (
          !options.providerEvalOnLocalBudgetFailure ||
          !isLocalUplcEvaluationFallbackError(localError.cause)
        ) {
          return yield* Effect.fail(localError);
        }
        yield* Effect.logWarning(
          `Local UPLC evaluation failed while building ${label}; retrying completion with provider evaluation.`,
        );
        return yield* Effect.tryPromise({
          try: () => tx.complete({ localUPLCEval: false }),
          catch: (cause) =>
            new SDK.LucidError({
              message: `Failed to build ${label} transaction with provider evaluation after local UPLC evaluation failure`,
              cause,
            }),
        }).pipe(
          Effect.catchAll((providerError) =>
            Effect.gen(function* () {
              if (
                !options.bootstrapExUnitsOnProviderFailure ||
                !isLocalUplcEvaluationFallbackError(providerError.cause)
              ) {
                return yield* Effect.fail(providerError);
              }
              yield* Effect.logWarning(
                `Provider evaluation failed while building ${label}; retrying completion with bootstrap execution units.`,
              );
              return yield* Effect.tryPromise({
                try: () =>
                  tx.complete({
                    localUPLCEval: true,
                    evaluator: bootstrapExUnitsEvaluator,
                  }),
                catch: (cause) =>
                  new SDK.LucidError({
                    message: `Failed to build ${label} transaction with bootstrap execution units after provider evaluation failure`,
                    cause,
                  }),
              });
            }),
          ),
        );
      }),
    ),
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
        mem: splitBudget(
          context.protocolParameters.maxTxExMem,
          count,
          index,
        ),
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

const submitCompletedTx = (
  lucid: LucidEvolution,
  tx: TxSignBuilder,
): Effect.Effect<string, TxConfirmError | TxSignError | TxSubmitError> =>
  handleSignSubmit(lucid, tx);

const fetchDaParamsUtxo = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<
  {
    readonly utxo: UTxO;
    readonly datum: SDK.DaParamsDatum;
  },
  SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const daParamsUnit = SDK.daParamsUnit(contracts.daParamsGovernor);
    const daParamsUtxos = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosAtWithUnit(
          contracts.daParamsGovernor.spendingScriptAddress,
          daParamsUnit,
        ),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to fetch DA params UTxO",
          cause,
        }),
    });
    if (daParamsUtxos.length !== 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Failed to resolve unique DA params UTxO",
          cause: `expected=1,found=${daParamsUtxos.length.toString()},unit=${daParamsUnit}`,
        }),
      );
    }
    const utxo = daParamsUtxos[0]!;
    const datum = yield* decodeDatum<SDK.DaParamsDatum>(
      utxo,
      SDK.DaParamsDatum as never,
      "DA params",
    );
    return { utxo, datum };
  });

const compareOutRefs = (left: UTxO, right: UTxO): number =>
  left.txHash.localeCompare(right.txHash) ||
  left.outputIndex - right.outputIndex;

const compareBigIntDesc = (left: bigint, right: bigint): number =>
  left === right ? 0 : left > right ? -1 : 1;

const daAttestationMatchesParams = (
  candidate: DaAttestationCandidate,
  daParamsDatum: SDK.DaParamsDatum,
): boolean =>
  candidate.datum.da_threshold === daParamsDatum.da_threshold &&
  candidate.datum.committee_signers_hash ===
    daParamsDatum.committee_signers_hash;

const daAttestationReachedThreshold = (
  candidate: DaAttestationCandidate,
): boolean => candidate.datum.attestation_count >= candidate.datum.da_threshold;

const signerIndexIsAttested = (
  attestedSigners: string,
  signerIndex: number,
): boolean => {
  const bytes = Buffer.from(attestedSigners, "hex");
  const byteIndex = Math.floor(signerIndex / 8);
  const byte = bytes[byteIndex];
  if (byte === undefined) {
    return false;
  }
  const bitInByte = signerIndex % 8;
  return (byte & (1 << (7 - bitInByte))) !== 0;
};

const addSignerToAttestedBitmap = (
  attestedSigners: string,
  signerIndex: number,
): string => {
  const bytes = Buffer.from(attestedSigners, "hex");
  const byteIndex = Math.floor(signerIndex / 8);
  if (byteIndex >= bytes.length) {
    throw new Error(
      `DA signer index ${signerIndex.toString()} is out of range`,
    );
  }
  const bitInByte = signerIndex % 8;
  bytes[byteIndex] |= 1 << (7 - bitInByte);
  return bytes.toString("hex");
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

const selectDaAttestationCandidate = (
  candidates: readonly DaAttestationCandidate[],
  daParamsDatum: SDK.DaParamsDatum,
  label: string,
  predicate: (candidate: DaAttestationCandidate) => boolean = () => true,
): Effect.Effect<DaAttestationCandidate, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const matching = candidates
      .filter((candidate) =>
        daAttestationMatchesParams(candidate, daParamsDatum),
      )
      .filter(predicate)
      .sort(
        (left, right) =>
          Number(daAttestationReachedThreshold(right)) -
            Number(daAttestationReachedThreshold(left)) ||
          compareBigIntDesc(
            left.datum.attestation_count,
            right.datum.attestation_count,
          ) ||
          compareOutRefs(left.utxo, right.utxo),
      );
    if (matching.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: `Failed to select ${label} DA attestation UTxO`,
          cause: `candidate_count=${candidates.length.toString()}`,
        }),
      );
    }
    return matching[0]!;
  });

const fetchDaAttestationCandidates = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  headerHash: string,
): Effect.Effect<readonly DaAttestationCandidate[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const unit = SDK.daAttestationUnit(contracts.daAttestation, headerHash);
    const utxos = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosAtWithUnit(
          contracts.daAttestation.spendingScriptAddress,
          unit,
        ),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to fetch DA attestation UTxO",
          cause,
        }),
    });
    const decoded = yield* Effect.forEach(utxos, (utxo) =>
      Effect.gen(function* () {
        const datum = yield* decodeDatum<SDK.DaAttestationDatum>(
          utxo,
          SDK.DaAttestationDatum as never,
          "DA attestation",
        );
        return { utxo, datum };
      }).pipe(Effect.either),
    );
    const matching = decoded
      .filter((entry) => entry._tag === "Right")
      .map((entry) => entry.right)
      .filter((entry) => entry.datum.header_hash === headerHash)
      .sort((left, right) => compareOutRefs(left.utxo, right.utxo));
    return matching;
  });

const fetchVisibleDaAttestationCandidates = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  headerHash: string,
): Effect.Effect<readonly DaAttestationCandidate[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const candidates = yield* fetchDaAttestationCandidates(
      lucid,
      contracts,
      headerHash,
    );
    if (candidates.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Failed to find visible DA attestation UTxO",
          cause: `header=${headerHash}`,
        }),
      );
    }
    return candidates;
  }).pipe(
    Effect.retry(
      Schedule.intersect(
        Schedule.fixed(UTXO_VISIBILITY_RETRY_DELAY),
        Schedule.recurs(UTXO_VISIBILITY_RETRY_COUNT),
      ),
    ),
  );

const fetchDaAttestationReferenceScripts = (
  lucid: LucidEvolution,
  referenceScriptsAddress: string,
  contracts: SDK.MidgardValidators,
): Effect.Effect<DaAttestationReferenceScripts, SDK.StateQueueError> =>
  fetchReferenceScriptUtxosProgram(
    lucid,
    referenceScriptsAddress,
    [
      {
        name: "da-attestation minting",
        script: contracts.daAttestation.mintingScript,
      },
      {
        name: "da-attestation spending",
        script: contracts.daAttestation.spendingScript,
      },
      {
        name: "state-queue minting",
        script: contracts.stateQueue.mintingScript,
      },
      {
        name: "state-queue spending",
        script: contracts.stateQueue.spendingScript,
      },
    ],
    contracts.referenceScriptAuth,
  ).pipe(
    Effect.map((resolved) => ({
      daAttestationMinting: referenceScriptByName(
        resolved,
        "da-attestation minting",
      ),
      daAttestationSpending: referenceScriptByName(
        resolved,
        "da-attestation spending",
      ),
      stateQueueMinting: referenceScriptByName(resolved, "state-queue minting"),
      stateQueueSpending: referenceScriptByName(
        resolved,
        "state-queue spending",
      ),
    })),
  );

const fetchUnattestedHeaders = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  headerHash?: string,
): Effect.Effect<
  readonly UnattestedStateQueueHeader[],
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LinkedListError
  | SDK.LucidError
  | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const stateQueueUtxos = yield* SDK.fetchSortedStateQueueUTxOsProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
    );
    const matches: UnattestedStateQueueHeader[] = [];
    for (const stateQueueUtxo of stateQueueUtxos) {
      if (stateQueueUtxo.datum.key === "Empty") {
        continue;
      }
      const node = yield* SDK.getStateQueueNodeFromStateQueueDatum(
        stateQueueUtxo.datum,
      );
      if (node.da_attestation !== SDK.NO_DA_ATTESTATION) {
        continue;
      }
      const recomputedHeaderHash = yield* SDK.hashBlockHeader(node.header);
      const datumHeaderHash = stateQueueUtxo.datum.key.Key.key;
      if (recomputedHeaderHash !== datumHeaderHash) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to select DA attestation target: state-queue key/hash mismatch",
            cause: `outRef=${outRefLabel(stateQueueUtxo.utxo)},datumKey=${datumHeaderHash},computed=${recomputedHeaderHash}`,
          }),
        );
      }
      if (headerHash !== undefined && recomputedHeaderHash !== headerHash) {
        continue;
      }
      matches.push({
        stateQueueUtxo,
        stateQueueNode: node,
        headerHash: recomputedHeaderHash,
      });
    }
    if (headerHash !== undefined && matches.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Requested state-queue header is not available for DA attestation",
          cause: `header=${headerHash}`,
        }),
      );
    }
    return matches;
  });

const indexedOperatorSignature = (
  headerHash: string,
  operatorSeedPhrase: string,
  network: Network,
): string => {
  const wallet = walletFromSeed(operatorSeedPhrase, { network });
  const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
  return `${OPERATOR_DA_SIGNER_INDEX.toString(16).padStart(2, "0")}${privateKey.sign(SDK.daAttestationMessage(headerHash)).to_hex()}`;
};

const buildInitDaAttestationTx = ({
  lucid,
  contracts,
  daParamsUtxo,
  daParamsDatum,
  target,
  referenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly daParamsUtxo: UTxO;
  readonly daParamsDatum: SDK.DaParamsDatum;
  readonly target: UnattestedStateQueueHeader;
  readonly referenceScripts: DaAttestationReferenceScripts;
}): Effect.Effect<TxSignBuilder, SDK.LucidError> =>
  Effect.gen(function* () {
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
    const initRedeemer = ((ctx) => {
      SDK.requireOwnMintPurpose(
        ctx,
        contracts.daAttestation.policyId,
        "DA attestation init",
      );
      return Data.to(
        {
          Init: {
            output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address ===
                  contracts.daAttestation.spendingScriptAddress &&
                outputDatumCborMatches(output, encodedAttestationDatum) &&
                (output.assets[attestationUnit] ?? 0n) === 1n,
              "DA attestation init",
            ),
            da_params_ref_input_index: SDK.requireReferenceInputIndex(
              ctx,
              daParamsUtxo,
              "DA attestation init DA params",
            ),
            state_queue_ref_input_index: SDK.requireReferenceInputIndex(
              ctx,
              target.stateQueueUtxo.utxo,
              "DA attestation init state queue",
            ),
            state_queue_mint_ref_script_input_index:
              SDK.requireReferenceInputIndex(
                ctx,
                referenceScripts.stateQueueMinting,
                "DA attestation init state_queue mint reference script",
              ),
          },
        } satisfies SDK.DaAttestationMintRedeemer as never,
        SDK.DaAttestationMintRedeemer as never,
      );
    }) satisfies BuildTxWithRedeemer;

    return yield* completeWithLocalUplc(
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
  });

const buildAddSignaturesTx = ({
  lucid,
  contracts,
  daParamsUtxo,
  attestationUtxo,
  attestationDatum,
  signatureWitnesses,
  referenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly daParamsUtxo: UTxO;
  readonly attestationUtxo: UTxO;
  readonly attestationDatum: SDK.DaAttestationDatum;
  readonly signatureWitnesses: string;
  readonly referenceScripts: DaAttestationReferenceScripts;
}): Effect.Effect<TxSignBuilder, SDK.LucidError> =>
  Effect.gen(function* () {
    const updatedAttestedSigners = addSignerToAttestedBitmap(
      attestationDatum.attested_signers,
      OPERATOR_DA_SIGNER_INDEX,
    );
    const updatedDatum: SDK.DaAttestationDatum = {
      ...attestationDatum,
      attested_signers: updatedAttestedSigners,
      attestation_count: countSetBits(updatedAttestedSigners),
    };
    const encodedUpdatedDatum = Data.to(
      updatedDatum as never,
      SDK.DaAttestationDatum as never,
    );
    const addSignaturesRedeemer = ((ctx) =>
      Data.to(
        {
          AddSignatures: {
            output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address ===
                  contracts.daAttestation.spendingScriptAddress &&
                outputDatumCborMatches(output, encodedUpdatedDatum) &&
                assetsEqual(output.assets, attestationUtxo.assets),
              "DA attestation add-signatures",
            ),
            da_params_ref_input_index: SDK.requireReferenceInputIndex(
              ctx,
              daParamsUtxo,
              "DA attestation add-signatures DA params",
            ),
            signatures: signatureWitnesses,
          },
        } satisfies SDK.DaAttestationSpendRedeemer as never,
        SDK.DaAttestationSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;

    const tx = lucid
      .newTx()
      .readFrom([daParamsUtxo, referenceScripts.daAttestationSpending])
      .collectFrom([attestationUtxo], addSignaturesRedeemer)
      .pay.ToContract(
        contracts.daAttestation.spendingScriptAddress,
        { kind: "inline", value: encodedUpdatedDatum },
        attestationUtxo.assets,
      );
    return yield* completeWithLocalUplc(tx, "DA attestation add-signatures", {
      providerEvalOnLocalBudgetFailure: true,
      bootstrapExUnitsOnProviderFailure: true,
    });
  });

const buildApplyAttestationTx = ({
  lucid,
  contracts,
  target,
  attestationUtxo,
  referenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly target: UnattestedStateQueueHeader;
  readonly attestationUtxo: UTxO;
  readonly referenceScripts: DaAttestationReferenceScripts;
}): Effect.Effect<TxSignBuilder, SDK.LucidError> =>
  Effect.gen(function* () {
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
    const daMintRedeemer = ((ctx) => {
      SDK.requireOwnMintPurpose(
        ctx,
        contracts.daAttestation.policyId,
        "DA attestation apply mint",
      );
      return Data.to(
        {
          ApplyToStateQueue: {
            da_attestation_input_index: SDK.requireInputIndex(
              ctx,
              attestationUtxo,
              "DA attestation apply DA attestation",
            ),
            state_queue_input_index: SDK.requireInputIndex(
              ctx,
              target.stateQueueUtxo.utxo,
              "DA attestation apply state queue",
            ),
            state_queue_output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === contracts.stateQueue.spendingScriptAddress &&
                outputDatumCborMatches(output, updatedStateQueueDatum) &&
                assetsEqual(output.assets, target.stateQueueUtxo.utxo.assets),
              "DA attestation apply state queue",
            ),
            state_queue_mint_ref_script_input_index:
              SDK.requireReferenceInputIndex(
                ctx,
                referenceScripts.stateQueueMinting,
                "DA attestation apply state_queue mint reference script",
              ),
          },
        } satisfies SDK.DaAttestationMintRedeemer as never,
        SDK.DaAttestationMintRedeemer as never,
      );
    }) satisfies BuildTxWithRedeemer;
    const daSpendRedeemer = ((ctx) =>
      Data.to(
        {
          BurnForStateQueue: {
            mint_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.daAttestation.policyId,
              "DA attestation apply DA attestation mint",
            ),
          },
        } satisfies SDK.DaAttestationSpendRedeemer as never,
        SDK.DaAttestationSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;
    const stateQueueSpendRedeemer = ((ctx) =>
      Data.to(
        {
          AttachDaAttestation: {
            state_queue_input_index: SDK.requireInputIndex(
              ctx,
              target.stateQueueUtxo.utxo,
              "DA attestation apply state queue",
            ),
            da_attestation_mint_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.daAttestation.policyId,
              "DA attestation apply DA attestation mint",
            ),
          },
        } satisfies SDK.StateQueueSpendRedeemer as never,
        SDK.StateQueueSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;

    return yield* completeWithLocalUplc(
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
  });

const attestHeader = ({
  lucid,
  contracts,
  nodeConfig,
  daParamsUtxo,
  daParamsDatum,
  target,
  referenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly nodeConfig: OperatorDaConfig;
  readonly daParamsUtxo: UTxO;
  readonly daParamsDatum: SDK.DaParamsDatum;
  readonly target: UnattestedStateQueueHeader;
  readonly referenceScripts: DaAttestationReferenceScripts;
}): Effect.Effect<
  AttestStateQueueHeaderResult,
  | SDK.LucidError
  | SDK.StateQueueError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  Effect.gen(function* () {
    let initTxHash: string | null = null;
    let addSignaturesTxHash: string | null = null;
    let candidates = yield* fetchDaAttestationCandidates(
      lucid,
      contracts,
      target.headerHash,
    );
    if (candidates.length === 0) {
      initTxHash = yield* submitCompletedTx(
        lucid,
        yield* buildInitDaAttestationTx({
          lucid,
          contracts,
          daParamsUtxo,
          daParamsDatum,
          target,
          referenceScripts,
        }),
      );
      candidates = yield* fetchVisibleDaAttestationCandidates(
        lucid,
        contracts,
        target.headerHash,
      );
    } else {
      yield* Effect.logInfo(
        `Resuming DA attestation for header ${target.headerHash}: found ${candidates.length.toString()} existing candidate UTxO(s).`,
      );
    }

    const signatureWitnesses = indexedOperatorSignature(
      target.headerHash,
      nodeConfig.L1_OPERATOR_SEED_PHRASE,
      nodeConfig.NETWORK,
    );
    const initializedAttestation = yield* selectDaAttestationCandidate(
      candidates,
      daParamsDatum,
      "initialized",
    );

    if (!daAttestationReachedThreshold(initializedAttestation)) {
      if (
        signerIndexIsAttested(
          initializedAttestation.datum.attested_signers,
          OPERATOR_DA_SIGNER_INDEX,
        )
      ) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Selected DA attestation UTxO already includes the local operator signature but has not reached threshold",
            cause: `outRef=${outRefLabel(initializedAttestation.utxo)},attestation_count=${initializedAttestation.datum.attestation_count.toString()},threshold=${initializedAttestation.datum.da_threshold.toString()}`,
          }),
        );
      }
      addSignaturesTxHash = yield* submitCompletedTx(
        lucid,
        yield* buildAddSignaturesTx({
          lucid,
          contracts,
          daParamsUtxo,
          attestationUtxo: initializedAttestation.utxo,
          attestationDatum: initializedAttestation.datum,
          signatureWitnesses,
          referenceScripts,
        }),
      );
      candidates = yield* fetchVisibleDaAttestationCandidates(
        lucid,
        contracts,
        target.headerHash,
      );
    }

    const signedAttestation = yield* selectDaAttestationCandidate(
      candidates,
      daParamsDatum,
      "threshold-signed",
      daAttestationReachedThreshold,
    );
    const applyTxHash = yield* submitCompletedTx(
      lucid,
      yield* buildApplyAttestationTx({
        lucid,
        contracts,
        target,
        attestationUtxo: signedAttestation.utxo,
        referenceScripts,
      }),
    );
    return {
      headerHash: target.headerHash,
      initTxHash,
      addSignaturesTxHash,
      applyTxHash,
      appliedAttestationOutRef: outRefLabel(signedAttestation.utxo),
      candidateCount: candidates.length,
    };
  });

export const attestStateQueueOnceProgram = (
  options: AttestStateQueueOnceOptions = {},
): Effect.Effect<
  readonly AttestStateQueueHeaderResult[],
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LinkedListError
  | SDK.LucidError
  | SDK.StateQueueError
  | TxConfirmError
  | TxSignError
  | TxSubmitError,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const nodeConfig = yield* NodeConfig;
    yield* lucidService.switchToOperatorsMainWallet;
    const lucid = lucidService.api;
    const daParams = yield* fetchDaParamsUtxo(lucid, contracts);
    const referenceScripts = yield* fetchDaAttestationReferenceScripts(
      lucid,
      lucidService.referenceScriptsAddress,
      contracts,
    );
    const targets = yield* fetchUnattestedHeaders(
      lucid,
      contracts,
      options.headerHash,
    );
    yield* Effect.logInfo(
      `DA attestation targets selected: count=${targets.length.toString()},headers=${targets.map((target) => target.headerHash).join(",")}`,
    );
    const results: AttestStateQueueHeaderResult[] = [];
    for (const target of targets) {
      const result = yield* attestHeader({
        lucid,
        contracts,
        nodeConfig,
        daParamsUtxo: daParams.utxo,
        daParamsDatum: daParams.datum,
        target,
        referenceScripts,
      });
      results.push(result);
    }
    return results;
  });
