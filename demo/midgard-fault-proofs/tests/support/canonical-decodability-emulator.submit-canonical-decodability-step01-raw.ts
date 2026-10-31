import {
  CanonicalDecodabilityStep01Datum,
  CanonicalDecodabilityStep01SpendRedeemer,
  CanonicalDecodabilityStep02Datum,
  type CanonicalDecodabilityStep02State,
  type CanonicalDecodabilityWalkState,
  type CommittedFieldClaim,
  HUB_ORACLE_ASSET_NAME,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type CanonicalDecodabilityContracts,
  requireCanonicalDecodabilityReferenceScript,
  requireCanonicalDecodabilityThreadUtxo,
} from "../../src/canonical-decodability/index.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
} from "../../src/runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../../src/witness-reference-scripts.js";
import { network } from "./submit-init-emulator-shared.js";

/**
 * The successor a raw step-01 transaction writes: the walk state back at step
 * 01, or the verdict at step 02. `address` overrides the step's own address.
 */
export type CanonicalDecodabilityRawSuccessor =
  | Readonly<{
      step: 1;
      state: CanonicalDecodabilityWalkState;
      address?: string;
    }>
  | Readonly<{
      step: 2;
      state: CanonicalDecodabilityStep02State;
      address?: string;
    }>;

/**
 * Guard-bypassing step-01 builder used only for validator-positive twins and
 * validator-negative tests: it writes whatever successor and claim it is
 * given. The successor keeps the thread's recorded prover, so a stranger
 * `signer` changes only who signs.
 */
export const submitCanonicalDecodabilityStep01Raw = async ({
  lucid,
  blueprint,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  claim,
  successor,
  carriageReferenceInputs = [],
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: CanonicalDecodabilityContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  /**
   * The claim, or a function of the built transaction's reference inputs for
   * a carriage whose indices name positions in them.
   */
  readonly claim:
    | CommittedFieldClaim
    | ((referenceInputs: readonly UTxO[]) => CommittedFieldClaim);
  readonly successor: CanonicalDecodabilityRawSuccessor;
  /** §8 chunk publications and certificate a certified claim reads. */
  readonly carriageReferenceInputs?: readonly UTxO[];
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<{ readonly txHash: string; readonly nextThreadOutRef: string }> => {
  const { threadUtxo, threadToken } =
    await requireCanonicalDecodabilityThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  if (threadUtxo.datum == null) {
    throw new Error("raw canonical step-01 thread has no inline datum");
  }
  const { fraud_prover } = Data.from(
    threadUtxo.datum,
    CanonicalDecodabilityStep01Datum,
  );
  const [stateQueueUtxo, hubOracleUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "raw state queue out-ref"),
      label: "raw canonical-decodability state queue node",
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: "raw canonical-decodability hub oracle",
    }),
  ]);
  const stepReference = requireCanonicalDecodabilityReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(network, phasScript);
  const phasCarriage = witnessWithdrawalValidatorCarriage({
    script: phasScript,
    referenceUtxo: witnessReferenceScripts.phasMembershipWithdraw,
    label: "raw canonical step-01 PHAS membership",
  });
  const successorAddress =
    successor.address ??
    contracts.steps[successor.step - 1]!.spendingScriptAddress;
  const datum =
    successor.step === 1
      ? Data.to(
          { fraud_prover, data: successor.state },
          CanonicalDecodabilityStep01Datum,
        )
      : Data.to(
          { fraud_prover, data: successor.state },
          CanonicalDecodabilityStep02Datum,
        );
  const outputMatches = computationThreadOutputPredicate({
    address: successorAddress,
    datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw canonical step-01");
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "raw canonical step-01",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "raw canonical step-01 output",
    );
    return Data.to(
      {
        Continue: [
          {
            inclusion: {
              RedeemerCarriedInclusion: [
                {
                  input_index: inputIndex,
                  output_index: outputIndex,
                  hub_ref_input_index: requireReferenceInputIndex(
                    ctx,
                    hubOracleUtxo,
                    "raw canonical step-01 hub",
                  ),
                  state_queue_node_ref_input_index: requireReferenceInputIndex(
                    ctx,
                    stateQueueUtxo,
                    "raw canonical step-01 state queue",
                  ),
                  native_tx_id: txInclusion.nativeTxId,
                  l2_transaction_source_cbor:
                    txInclusion.l2TransactionSourceCbor,
                  transactions_phas_root: txInclusion.transactionsPhasRoot,
                  tx_membership_proof: txInclusion.txMembershipProof,
                  inclusion_proof_script_withdraw_redeemer_index:
                    requireWithdrawalRedeemerIndex(
                      ctx,
                      phasRewardAddress,
                      "raw canonical step-01 membership",
                    ),
                },
              ],
            },
            claim:
              typeof claim === "function" ? claim(ctx.referenceInputs) : claim,
          },
        ],
      },
      CanonicalDecodabilityStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      hubOracleUtxo,
      stateQueueUtxo,
      ...carriageReferenceInputs,
      stepReference,
      ...phasCarriage.referenceInputs,
    ])
    .withdraw(
      phasRewardAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.l2TransactionSourceCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      successorAddress,
      { kind: "inline", value: datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await phasCarriage
    .attach(base)
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined)
    throw new Error("Raw step-01 layout unresolved");
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
