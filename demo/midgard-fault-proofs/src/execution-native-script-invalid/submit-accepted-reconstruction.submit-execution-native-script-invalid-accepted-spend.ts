import {
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
} from "@al-ft/midgard-core";
import {
  fieldOpeningForField,
  requireInputIndex,
  requireOwnSpendPurpose,
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
  acceptedAdvanceNonScript,
  acceptedAppendPurpose,
  type AcceptedReconstructionState,
} from "./accepted-reconstruction-machine.js";
import type { ExecutionNativeScriptInvalidContracts } from "./contracts.js";
import {
  ExecutionNativeScriptInvalidAcceptedDatumSchema,
  ExecutionNativeScriptInvalidAcceptedSpendRedeemerSchema,
} from "./schemas.js";
import {
  FAMILY,
  requireAcceptedPrelude,
} from "./submit-accepted-reconstruction.submit-execution-native-script-invalid-accepted-init.js";

/** Authenticate and consume exactly one canonical spend-input descriptor. */
export const submitExecutionNativeScriptInvalidAcceptedSpend = async ({
  lucid,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  spendInputsPreimageCbor,
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
  spendInputsPreimageCbor: string;
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
    stepIndex: 1,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<AcceptedReconstructionState>({
    threadUtxo,
    signer,
    schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
    family: FAMILY,
    stepIndex: 1,
  });
  if (state.phase !== 0n)
    throw new Error(`${FAMILY}: spend scanner received another phase`);
  const items = decodeMidgardFieldPreimage(
    Buffer.from(spendInputsPreimageCbor, "hex"),
  );
  const item = items[Number(state.field_cursor)];
  if (item === undefined)
    throw new Error(`${FAMILY}: spend cursor is outside retained field`);
  const descriptor = decodeMidgardLedgerOutputCommitment(
    Buffer.from(descriptorCbor, "hex"),
  );
  const credential = decodeMidgardAddressBytes(
    descriptor.address,
  ).paymentCredential;
  const selects =
    credential.kind === "Script" &&
    state.execution_cursor === state.bound.execution_index;
  const nextState =
    credential.kind === "Script"
      ? acceptedAppendPurpose({
          state,
          purposeKind: 0n,
          purposeIndex: state.field_cursor,
          scriptHash: credential.hash.toString("hex"),
          subject: item.toString("hex"),
          canonicalKey: item.toString("hex"),
          nextScriptHash: selects
            ? accepted[5]!.spendingScriptHash
            : accepted[1]!.spendingScriptHash,
        })
      : acceptedAdvanceNonScript({
          state,
          canonicalKey: item.toString("hex"),
          nextScriptHash: accepted[1]!.spendingScriptHash,
        });
  const nextContract = selects ? accepted[5]! : accepted[1]!;
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: nextContract.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: accepted[1]!.spendingScriptHash,
    family: FAMILY,
    stepIndex: 7,
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
    label: `${FAMILY} accepted spend membership`,
  });
  const referenceInputs = [
    stepReference,
    ...membershipCarriage.referenceInputs,
  ];
  const opening = fieldOpeningForField({
    fieldIndex: 0,
    nativeTxCompactCbor,
    carriage: { Inline: { preimage: spendInputsPreimageCbor } },
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} accepted spend`);
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} accepted spend`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} accepted spend output`,
    );
    return Data.to(
      {
        Continue: [
          {
            ScanSpend: {
              input_index: inputIndex,
              output_index: outputIndex,
              spend_inputs_opening: opening,
              descriptor_cbor: descriptorCbor,
              membership: {
                RedeemerCarriedMembership: {
                  membership_proof: membershipProof,
                  membership_proof_script_redeemer_index:
                    requireWithdrawalRedeemerIndex(
                      ctx,
                      membershipAddress,
                      `${FAMILY} accepted spend membership`,
                    ),
                },
              },
            },
          },
        ],
      } as never,
      ExecutionNativeScriptInvalidAcceptedSpendRedeemerSchema as never,
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
        .readFrom(referenceInputs)
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
        role: `${FAMILY}-accepted-spend`,
        utxo: stepReference,
        expectedScript: accepted[1]!.spendingScript,
      }),
      workflowReferenceScript({
        role: `${FAMILY}-accepted-spend-membership`,
        utxo: membershipReferenceScriptUtxo,
        expectedScript: membershipScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedHash)
    throw new Error(`${FAMILY}: accepted spend transaction hash changed`);
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    selected: selects,
  };
};
