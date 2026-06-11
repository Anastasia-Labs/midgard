import { assetsEqual } from "@al-ft/midgard-core/assets";
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

type OperatorDaConfig = {
  readonly L1_OPERATOR_SEED_PHRASE: string;
  readonly NETWORK: Network;
};

export type AttestStateQueueOnceOptions = {
  readonly headerHash?: string;
};

export type AttestStateQueueHeaderResult = {
  readonly headerHash: string;
  readonly initTxHash: string;
  readonly addSignaturesTxHash: string;
  readonly applyTxHash: string;
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

const completeWithLocalUplc = (
  tx: {
    readonly complete: (options: {
      readonly localUPLCEval: true;
    }) => Promise<TxSignBuilder>;
  },
  label: string,
): Effect.Effect<TxSignBuilder, SDK.LucidError> =>
  Effect.tryPromise({
    try: () => tx.complete({ localUPLCEval: true }),
    catch: (cause) =>
      new SDK.LucidError({
        message: `Failed to build ${label} transaction`,
        cause,
      }),
  });

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

const fetchDaAttestationUtxo = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  headerHash: string,
): Effect.Effect<
  {
    readonly utxo: UTxO;
    readonly datum: SDK.DaAttestationDatum;
  },
  SDK.StateQueueError
> =>
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
      .filter((entry) => entry.datum.header_hash === headerHash);
    if (matching.length !== 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Failed to resolve unique DA attestation UTxO",
          cause: `expected=1,found=${matching.length.toString()},unit=${unit}`,
        }),
      );
    }
    return matching[0]!;
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
  return `00${privateKey.sign(SDK.daAttestationMessage(headerHash)).to_hex()}`;
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
    const updatedDatum: SDK.DaAttestationDatum = {
      ...attestationDatum,
      attested_signers: SDK.prefixAttestedSignerBitmap(1),
      attestation_count: 1n,
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

    return yield* completeWithLocalUplc(
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
    const initTxHash = yield* submitCompletedTx(
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
    const initializedAttestation = yield* fetchDaAttestationUtxo(
      lucid,
      contracts,
      target.headerHash,
    );
    const signatureWitnesses = indexedOperatorSignature(
      target.headerHash,
      nodeConfig.L1_OPERATOR_SEED_PHRASE,
      nodeConfig.NETWORK,
    );
    const addSignaturesTxHash = yield* submitCompletedTx(
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
    const signedAttestation = yield* fetchDaAttestationUtxo(
      lucid,
      contracts,
      target.headerHash,
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
