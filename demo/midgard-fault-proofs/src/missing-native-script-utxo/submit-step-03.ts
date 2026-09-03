import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import {
  MissingNativeScriptUtxoStep03DatumSchema,
  MissingNativeScriptUtxoStep03SpendRedeemerSchema,
  MissingNativeScriptUtxoStep04DatumSchema,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  linearFaultStepLabel,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import {
  chunkedMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunk,
  requireBuiltChunkReferenceIndices,
  walletInputsExcludingChunks,
} from "../proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  getCompiledScript,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  selectFeeInput,
} from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary.js";
import {
  MISSING_NATIVE_SCRIPT_UTXO_CATEGORY_LABEL as FAMILY,
  type MissingNativeScriptUtxoContracts,
} from "./contracts.js";
import type { PreparedMissingNativeScriptUtxo } from "./prepare.js";

type Step03State = NonNullable<
  Data.Static<typeof MissingNativeScriptUtxoStep03DatumSchema>["data"]
>;
type Step03Datum = Data.Static<typeof MissingNativeScriptUtxoStep03DatumSchema>;
const Step03Datum =
  MissingNativeScriptUtxoStep03DatumSchema as unknown as Step03Datum;
type Step04Datum = Data.Static<typeof MissingNativeScriptUtxoStep04DatumSchema>;
const Step04Datum =
  MissingNativeScriptUtxoStep04DatumSchema as unknown as Step04Datum;
type Step03Redeemer = Data.Static<
  typeof MissingNativeScriptUtxoStep03SpendRedeemerSchema
>;
const Step03Redeemer =
  MissingNativeScriptUtxoStep03SpendRedeemerSchema as unknown as Step03Redeemer;

export const submitMissingNativeScriptUtxoStep03 = async ({
  lucid,
  blueprint,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  prepared,
  referenceScriptUtxo,
  publishedProofChunks = [],
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: MissingNativeScriptUtxoContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly prepared: PreparedMissingNativeScriptUtxo;
  readonly referenceScriptUtxo: UTxO;
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2;
  const label = linearFaultStepLabel(FAMILY, stepIndex);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<Step03State>({
    threadUtxo,
    signer,
    schema: Step03Datum,
    family: FAMILY,
    stepIndex,
  });
  if (
    state.prev_utxos_root !== prepared.prevUtxosRoot ||
    state.input_with_missing_script.tx_id !== prepared.outRef.transactionId ||
    state.input_with_missing_script.output_index !== prepared.outRef.outputIndex
  ) {
    throw new Error(`${label}: prepared member does not match thread state`);
  }
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const carriedByChunks = publishedProofChunks.length > 0;
  const memberKeyCbor = encodeMidgardSpendInputItem({
    txId: Buffer.from(prepared.outRef.transactionId, "hex"),
    outputIndex: Number(prepared.outRef.outputIndex),
  }).toString("hex");
  const membershipScript: Script = carriedByChunks
    ? chunkedVerifyWithdrawalScript(blueprint)
    : {
        type: "PlutusV3",
        script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
      };
  const membershipAddress = phasMembershipRewardAddress(
    network,
    membershipScript,
  );
  const membershipCarriage = witnessWithdrawalValidatorCarriage({
    script: membershipScript,
    referenceUtxo: carriedByChunks
      ? witnessReferenceScripts?.chunkedVerifyWithdraw
      : witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${label} predecessor membership`,
  });
  const referenceInputs = [
    ...publishedProofChunks.map(({ utxo }) => utxo),
    stepReference,
    ...membershipCarriage.referenceInputs,
  ];
  const chunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks: publishedProofChunks,
    label,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        out_ref: prepared.outRef,
        descriptor_cbor: prepared.descriptorCbor,
        bad_tx_id: state.bad_tx_id,
        bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
      },
    },
    Step04Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const inputIndex = requireInputIndex(ctx, threadUtxo, label);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, label);
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks: publishedProofChunks,
      derived: chunkIndices,
      label,
    });
    const membership = carriedByChunks
      ? {
          PublishedChunkMembership: [
            { ordered_chunk_reference_input_indices: chunkIndices },
          ],
        }
      : {
          RedeemerCarriedMembership: {
            membership_proof: prepared.membershipProof,
            membership_proof_script_redeemer_index:
              requireWithdrawalRedeemerIndex(ctx, membershipAddress, label),
          },
        };
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            out_ref: prepared.outRef,
            descriptor_cbor: prepared.descriptorCbor,
            membership,
          },
        ],
      } as never,
      Step03Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks: publishedProofChunks,
    }),
  );
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs);
  const withMembership = carriedByChunks
    ? base.withdraw(
        membershipAddress,
        0n,
        chunkedMembershipClaimRedeemer({
          merkleRoot: prepared.prevUtxosRoot,
          keyBytes: memberKeyCbor,
          valueBytes: prepared.descriptorCbor,
          orderedChunkReferenceInputIndices: chunkIndices,
        }),
      )
    : base.withdraw(
        membershipAddress,
        0n,
        encodeRawPhasMembershipProofRedeemer({
          root: prepared.prevUtxosRoot,
          keyBytes: memberKeyCbor,
          valueBytes: prepared.descriptorCbor,
          membershipProofCbor: prepared.membershipProofCbor,
        }),
      );
  const unsigned = await membershipCarriage
    .attach(
      withMembership.pay
        .ToContract(
          contracts.steps[3].spendingScriptAddress,
          { kind: "inline", value: nextDatum },
          {
            lovelace: threadUtxo.assets.lovelace ?? 0n,
            [threadToken.unit]: 1n,
          },
        )
        .addSignerKey(signer.paymentKeyHash),
    )
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: label,
        utxo: stepReference,
        expectedScript: contracts.steps[stepIndex].spendingScript,
      }),
      workflowReferenceScript({
        role: `${label}-membership`,
        utxo: membershipCarriage.referenceInputs[0],
        expectedScript: membershipScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) throw new Error(`${label}: hash mismatch`);
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    membershipCarriage: carriedByChunks ? "Published" : "Direct",
  };
};
