import {
  decodeMidgardFieldArrayHeader,
  decodeMidgardFieldPreimage,
  decodeMidgardVersionedScript,
  encodeMidgardDefiniteBytes,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  MintAuthorizationStep04Datum,
  MintAuthorizationWitnessScanDatum,
  MintAuthorizationWitnessScanSpendRedeemer,
  type MintAuthorizationWitnessScanSpendRedeemer as WitnessScanRedeemer,
  type MintAuthorizationWitnessScanState,
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
import {
  requireMintAuthorizationReferenceScript,
  requireMintAuthorizationStepState,
  requireMintAuthorizationThreadUtxo,
} from "./submit-common.js";

export const submitMintAuthorizationWitnessScan = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  fieldBytesHex,
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
  readonly fieldBytesHex: string;
  readonly rawPreimageUtxos: readonly UTxO[];
  readonly referenceScriptUtxo?: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const { threadUtxo, threadToken } = await requireMintAuthorizationThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 6,
    threadOutRef,
  });
  const state: MintAuthorizationWitnessScanState =
    requireMintAuthorizationStepState({
      threadUtxo,
      signer,
      schema: MintAuthorizationWitnessScanDatum,
      stepIndex: 6,
    });
  const bytes = Buffer.from(fieldBytesHex, "hex");
  const requirement = createRawDatumPreimageRequirement({ preimage: bytes });
  if (
    BigInt(bytes.length) !== state.field_length ||
    JSON.stringify(requirement.publicationDigests) !==
      JSON.stringify(state.field_chunk_hashes) ||
    rawPreimageUtxos.length !== requirement.publicationDatums.length ||
    rawPreimageUtxos.some(
      (utxo, index) => utxo.datum !== requirement.publicationDatums[index],
    )
  )
    throw new Error(
      "mint witness preimage differs from authenticated evaluator state",
    );
  const items = decodeMidgardFieldPreimage(bytes);
  if (
    BigInt(items.length) !== state.item_count ||
    state.item_index < 0n ||
    state.item_index > state.item_count
  )
    throw new Error("mint witness count differs from authenticated state");
  const offsetAt = (index: number) =>
    decodeMidgardFieldArrayHeader(bytes).nextOffset +
    items
      .slice(0, index)
      .reduce(
        (total, item) => total + encodeMidgardDefiniteBytes(item).length,
        0,
      );
  if (BigInt(offsetAt(Number(state.item_index))) !== state.cursor)
    throw new Error("mint witness cursor is not a canonical envelope boundary");
  const terminal =
    state.item_index === state.item_count &&
    state.cursor === state.field_length;
  const end = Math.min(items.length, Number(state.item_index) + 16);
  for (const item of items.slice(Number(state.item_index), end))
    if (
      hashMidgardVersionedScript(decodeMidgardVersionedScript(item)) ===
      state.policy_id
    )
      throw new Error("mint policy has a matching inline script witness");
  const nextStep = terminal ? contracts.steps[3] : contracts.steps[6];
  const datum = terminal
    ? Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            policy_id: state.policy_id,
            bad_tx_id: state.bad_tx_id,
            prior_ledger_root: state.prior_ledger_root,
            ref_cursor: 0n,
          },
        },
        MintAuthorizationStep04Datum,
      )
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            ...state,
            cursor: BigInt(offsetAt(end)),
            item_index: BigInt(end),
          },
        },
        MintAuthorizationWitnessScanDatum,
      );
  const reference =
    referenceScriptUtxo === undefined
      ? undefined
      : requireMintAuthorizationReferenceScript({
          utxo: referenceScriptUtxo,
          expectedScriptHash: contracts.steps[6].spendingScriptHash,
          stepIndex: 6,
        });
  const carriage = witnessSpendingValidatorCarriage({
    script: contracts.steps[6].spendingScript,
    referenceUtxo: reference,
    label: "mint authorization witness scan",
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
    requireOwnSpendPurpose(ctx, threadUtxo, "mint witness scan");
    const input_index = requireInputIndex(ctx, threadUtxo, "mint witness scan");
    const output_index = requireUniqueOutputIndex(
      ctx.outputs,
      matches,
      "mint witness scan output",
    );
    outputIndex = output_index;
    const value: WitnessScanRedeemer = {
      Continue: [
        terminal
          ? { Finalize: { input_index, output_index } }
          : {
              Advance: {
                input_index,
                output_index,
                chunk_reference_indices: rawPreimageUtxos.map((utxo) =>
                  requireReferenceInputIndex(ctx, utxo, "mint witness chunk"),
                ),
              },
            },
      ],
    };
    return Data.to(value, MintAuthorizationWitnessScanSpendRedeemer);
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
    throw new Error("mint witness scan did not resolve output index");
  const expected = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof mint-authorization step-07",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[6].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expected)
    throw new Error("mint witness scan submission hash changed");
  if (awaitConfirmation) await lucid.awaitTx(txHash);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex}`,
    terminal,
    nextStepAddress: nextStep.spendingScriptAddress,
  };
};
