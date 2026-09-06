import {
  MintAuthorizationEvaluateDatum,
  type MintAuthorizationEvaluateSpendRedeemer as EvaluateRedeemer,
  MintAuthorizationEvaluateSpendRedeemer,
  type MintAuthorizationEvaluateState,
  MintAuthorizationStep05Datum,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { witnessSpendingValidatorCarriage } from "../witness-reference-scripts.js";
import { createRawDatumPreimageRequirement } from "../workflow/raw-datum-preimage-prerequisite.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import type { MintAuthorizationContracts } from "./contracts.js";
import { mintAuthorizationEvaluationBatches } from "./evaluate.js";
import {
  requireMintAuthorizationReferenceScript,
  requireMintAuthorizationStepState,
  requireMintAuthorizationThreadUtxo,
} from "./submit-common.js";

export const submitMintAuthorizationEvaluate = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  scriptBytesHex,
  rawPreimageUtxos,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintAuthorizationContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly scriptBytesHex: string;
  readonly rawPreimageUtxos: readonly UTxO[];
  readonly referenceScriptUtxo?: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const { threadUtxo, threadToken } = await requireMintAuthorizationThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 5,
    threadOutRef,
  });
  const state: MintAuthorizationEvaluateState =
    requireMintAuthorizationStepState({
      threadUtxo,
      signer,
      schema: MintAuthorizationEvaluateDatum,
      stepIndex: 5,
    });
  const bytes = Buffer.from(scriptBytesHex, "hex");
  const requirement = createRawDatumPreimageRequirement({ preimage: bytes });
  if (
    BigInt(bytes.length) !== state.script_length ||
    JSON.stringify(requirement.publicationDigests) !==
      JSON.stringify(state.script_chunk_hashes) ||
    rawPreimageUtxos.length !== requirement.publicationDatums.length ||
    rawPreimageUtxos.some(
      (utxo, index) => utxo.datum !== requirement.publicationDatums[index],
    )
  )
    throw new Error(
      "mint native preimage differs from authenticated evaluator state",
    );
  const terminal =
    state.cursor === state.script_length &&
    state.stack_depth === 0n &&
    state.stack_root === "" &&
    state.result === 0n;
  const initial = {
    ...state,
    cursor: 0n,
    node_count: 0n,
    stack_root: "",
    stack_depth: 0n,
    result: -1n,
  };
  const encodedState = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state },
    MintAuthorizationEvaluateDatum,
  );
  let batch:
    | (ReturnType<typeof mintAuthorizationEvaluationBatches> extends Generator<
        infer Batch
      >
        ? Batch
        : never)
    | undefined;
  if (!terminal) {
    for (const candidate of mintAuthorizationEvaluationBatches(
      initial,
      bytes,
    )) {
      if (
        Data.to(
          { fraud_prover: signer.paymentKeyHash, data: candidate.before },
          MintAuthorizationEvaluateDatum,
        ) === encodedState
      ) {
        batch = candidate;
        break;
      }
    }
  }
  if (!terminal && batch === undefined)
    throw new Error(
      "mint evaluator cursor is not reachable from retained native policy",
    );
  const nextStep = terminal ? contracts.steps[4] : contracts.steps[5];
  const datum = terminal
    ? Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: { policy_id: state.policy_id, direction: 1n },
        },
        MintAuthorizationStep05Datum,
      )
    : Data.to(
        { fraud_prover: signer.paymentKeyHash, data: batch!.after },
        MintAuthorizationEvaluateDatum,
      );
  const reference =
    referenceScriptUtxo === undefined
      ? undefined
      : requireMintAuthorizationReferenceScript({
          utxo: referenceScriptUtxo,
          expectedScriptHash: contracts.steps[5].spendingScriptHash,
          stepIndex: 5,
        });
  const carriage = witnessSpendingValidatorCarriage({
    script: contracts.steps[5].spendingScript,
    referenceUtxo: reference,
    label: "mint authorization evaluator",
  });
  const refs = [
    ...(terminal ? [] : rawPreimageUtxos),
    ...carriage.referenceInputs,
  ];
  const matches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "mint evaluator");
    const input_index = requireInputIndex(ctx, threadUtxo, "mint evaluator");
    const output_index = requireUniqueOutputIndex(
      ctx.outputs,
      matches,
      "mint evaluator output",
    );
    outputIndex = output_index;
    const value: EvaluateRedeemer = {
      Continue: [
        terminal
          ? { Finalize: { input_index, output_index } }
          : {
              Advance: {
                input_index,
                output_index,
                chunk_reference_indices: rawPreimageUtxos.map((utxo) =>
                  requireReferenceInputIndex(ctx, utxo, "mint native chunk"),
                ),
                operations: batch!.operations,
              },
            },
      ],
    };
    return Data.to(value, MintAuthorizationEvaluateSpendRedeemer);
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const wallet = (await lucid.wallet().getUtxos()).filter(
    (candidate) =>
      !refs.some(
        (ref) =>
          ref.txHash === candidate.txHash &&
          ref.outputIndex === candidate.outputIndex,
      ) &&
      !(
        candidate.txHash === threadUtxo.txHash &&
        candidate.outputIndex === threadUtxo.outputIndex
      ),
  );
  const builder = lucid
    .newTx()
    .collectFrom([selectFeeInput(wallet)])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(refs)
    .pay.ToContract(
      nextStep.spendingScriptAddress,
      { kind: "inline", value: datum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [threadToken.unit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await carriage
    .attach(builder)
    .complete({ localUPLCEval: true, presetWalletInputs: wallet });
  const signed = await unsigned.sign.withWallet().complete();
  if (outputIndex === undefined)
    throw new Error("mint evaluator did not resolve output index");
  const expected = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof mint-authorization step-06",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[5].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expected)
    throw new Error("mint evaluator submission hash changed");
  if (awaitConfirmation) await lucid.awaitTx(txHash);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex}`,
    terminal,
    nextStepAddress: nextStep.spendingScriptAddress,
  };
};
