import {
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  fieldOpeningForField,
  requireInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { witnessWithdrawalValidatorCarriage } from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary.js";
import {
  acceptedAdvanceReferenceWithoutSource,
  acceptedAppendSource,
  type AcceptedReconstructionState,
} from "./accepted-reconstruction-machine.js";
import type { ExecutionNativeScriptInvalidContracts } from "./contracts.js";
import {
  ExecutionNativeScriptInvalidAcceptedDatumSchema,
  ExecutionNativeScriptInvalidAcceptedReferenceRedeemerSchema,
  ExecutionNativeScriptInvalidStep03DatumSchema,
} from "./schemas.js";
import {
  FAMILY,
  requireAcceptedPrelude,
} from "./submit-accepted-reconstruction.submit-execution-native-script-invalid-accepted-init.js";

/** Authenticate one resolved-reference source against the predecessor ledger. */
export const submitExecutionNativeScriptInvalidAcceptedReferenceSource =
  async ({
    lucid,
    network,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    nativeTxCompactCbor,
    referenceInputsPreimageCbor,
    descriptorCbor,
    membershipProof,
    membershipProofCbor,
    membershipReferenceScriptUtxo,
    referenceScriptUtxo,
    preSubmitBoundary,
    awaitConfirmation = true,
  }: {
    lucid: LucidEvolution;
    network: Parameters<typeof phasMembershipRewardAddress>[0];
    contracts: ExecutionNativeScriptInvalidContracts;
    categoryId: string;
    signer: ResolvedProverSigner;
    threadOutRef: string;
    nativeTxCompactCbor: string;
    referenceInputsPreimageCbor: string;
    descriptorCbor: string;
    membershipProof: unknown;
    membershipProofCbor: string;
    membershipReferenceScriptUtxo: UTxO;
    referenceScriptUtxo: UTxO;
    preSubmitBoundary?: FraudProofPreSubmitBoundary;
    awaitConfirmation?: boolean;
  }) => {
    const accepted = requireAcceptedPrelude(contracts);
    const physicalContracts = { ...contracts, steps: accepted };
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      lucid,
      contracts: physicalContracts,
      categoryId,
      family: FAMILY,
      stepIndex: 6,
      threadOutRef,
    });
    const state = requireLinearFaultStepState<AcceptedReconstructionState>({
      threadUtxo,
      signer,
      schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
      family: FAMILY,
      stepIndex: 6,
    });
    if (state.phase !== 5n || state.selected_purpose === null)
      throw new Error(`${FAMILY}: reference scanner lacks selected purpose`);
    const item = decodeMidgardFieldPreimage(
      Buffer.from(referenceInputsPreimageCbor, "hex"),
    )[Number(state.field_cursor)];
    if (item === undefined)
      throw new Error(`${FAMILY}: reference cursor is outside canonical field`);
    const outRef = decodeMidgardSpendInputItem(item);
    const descriptor = decodeMidgardLedgerOutputCommitment(
      Buffer.from(descriptorCbor, "hex"),
    );
    if (descriptor.outputIndex !== outRef.outputIndex)
      throw new Error(`${FAMILY}: reference descriptor output index changed`);
    const hasScript = descriptor.referenceScriptLanguage !== -1;
    const source = hasScript
      ? {
          source_index: state.source_cursor,
          origin_kind: 1n,
          source_key: item.toString("hex"),
          language_tag: BigInt(descriptor.referenceScriptLanguage),
          script_hash: descriptor.referenceScriptHash.toString("hex"),
          total_length: BigInt(descriptor.referenceScriptTotalLength),
          item_commitment:
            descriptor.referenceScriptItemCommitment.toString("hex"),
        }
      : null;
    const selected = source?.script_hash === state.selected_purpose.script_hash;
    const advanced =
      source === null
        ? acceptedAdvanceReferenceWithoutSource({
            state,
            nextScriptHash: accepted[6]!.spendingScriptHash,
          })
        : acceptedAppendSource({
            state,
            source,
            nextScriptHash: selected
              ? contracts.steps[2]!.spendingScriptHash
              : accepted[6]!.spendingScriptHash,
          });
    const nextContract = selected ? contracts.steps[2]! : accepted[6]!;
    const nextData = selected
      ? {
          bound: state.bound,
          prior_ledger_root: state.bound.prior_ledger_root,
          ...source!,
          compact_cbor: state.bound.compact_cbor,
        }
      : advanced;
    const nextDatum = Data.to(
      { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
      (selected
        ? ExecutionNativeScriptInvalidStep03DatumSchema
        : ExecutionNativeScriptInvalidAcceptedDatumSchema) as never,
    );
    const outputMatches = computationThreadOutputPredicate({
      address: nextContract.spendingScriptAddress,
      datum: nextDatum,
      unit: threadToken.unit,
    });
    const stepReference = requireLinearFaultReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash: accepted[6]!.spendingScriptHash,
      family: FAMILY,
      stepIndex: 12,
    });
    if (membershipReferenceScriptUtxo.scriptRef == null)
      throw new Error(`${FAMILY}: prior-ledger membership script absent`);
    const membershipScript = membershipReferenceScriptUtxo.scriptRef;
    const membershipAddress = phasMembershipRewardAddress(
      network,
      membershipScript,
    );
    const membershipCarriage = witnessWithdrawalValidatorCarriage({
      script: membershipScript,
      referenceUtxo: membershipReferenceScriptUtxo,
      label: `${FAMILY} accepted reference membership`,
    });
    const opening = fieldOpeningForField({
      fieldIndex: 2,
      nativeTxCompactCbor,
      carriage: { Inline: { preimage: referenceInputsPreimageCbor } },
    });
    let outputIndex: bigint | undefined;
    const redeemer = ((ctx) => {
      const inputIndex = requireInputIndex(
        ctx,
        threadUtxo,
        `${FAMILY} accepted reference`,
      );
      outputIndex = requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${FAMILY} accepted reference output`,
      );
      return Data.to(
        {
          Continue: [
            {
              input_index: inputIndex,
              output_index: outputIndex,
              reference_inputs_opening: opening,
              descriptor_cbor: descriptorCbor,
              membership: {
                RedeemerCarriedMembership: {
                  membership_proof: membershipProof,
                  membership_proof_script_redeemer_index:
                    requireWithdrawalRedeemerIndex(
                      ctx,
                      membershipAddress,
                      `${FAMILY} accepted reference membership`,
                    ),
                },
              },
            },
          ],
        } as never,
        ExecutionNativeScriptInvalidAcceptedReferenceRedeemerSchema as never,
      );
    }) satisfies BuildTxWithRedeemer;
    signer.selectWallet(lucid);
    const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
    const unsigned = await membershipCarriage
      .attach(
        lucid
          .newTx()
          .collectFrom([feeInput])
          .collectFrom([threadUtxo], redeemer)
          .readFrom([stepReference, ...membershipCarriage.referenceInputs])
          .withdraw(
            membershipAddress,
            0n,
            encodeRawPhasMembershipProofRedeemer({
              root: state.bound.prior_ledger_root,
              keyBytes: item.toString("hex"),
              valueBytes: descriptorCbor,
              membershipProofCbor,
            }),
          )
          .pay.ToContract(
            nextContract.spendingScriptAddress,
            { kind: "inline", value: nextDatum },
            {
              lovelace: threadUtxo.assets.lovelace ?? 0n,
              [threadToken.unit]: 1n,
            },
          )
          .addSignerKey(signer.paymentKeyHash),
      )
      .complete({ localUPLCEval: true });
    if (outputIndex === undefined)
      throw new Error(`${FAMILY}: unresolved layout`);
    const signed = await unsigned.sign.withWallet().complete();
    const expectedHash = await reachFraudProofPreSubmitBoundary({
      signed,
      referenceScripts: [
        workflowReferenceScript({
          role: `${FAMILY}-accepted-reference`,
          utxo: stepReference,
          expectedScript: accepted[6]!.spendingScript,
        }),
        workflowReferenceScript({
          role: `${FAMILY}-accepted-reference-membership`,
          utxo: membershipReferenceScriptUtxo,
          expectedScript: membershipScript,
        }),
      ],
      boundary: preSubmitBoundary,
    });
    const txHash = await signed.submit();
    if (txHash !== expectedHash)
      throw new Error(`${FAMILY}: accepted reference transaction hash changed`);
    if (awaitConfirmation)
      await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    return {
      txHash,
      nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
      selected,
    };
  };
