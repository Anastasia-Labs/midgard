import {
  type CommittedFieldClaim,
  CommittedFieldShapeStep01Datum,
  CommittedFieldShapeStep01SpendRedeemer,
  CommittedFieldShapeStep02Datum,
  type CommittedFieldShapeStep02State,
  type CommittedFieldShapeWalkState,
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

import { requireCommittedFieldShapeThreadUtxo } from "../../src/committed-field-shape/submit-common.js";
import {
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../../src/runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  selectFeeInput,
} from "../../src/step-support.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import { witnessWithdrawalValidatorCarriage } from "../../src/witness-reference-scripts.js";
import {
  type CommittedFieldShapeEmulatorHarness,
  type CommittedFieldShapeScenario,
} from "./committed-field-shape-emulator.committed-field-shape-scenario-material.js";
import { network } from "./submit-init-emulator-shared.js";

/**
 * Funds the outsider after setup has consumed the parameterizing nonce.
 *
 * Both of its addresses are funded. `selectWallet.fromSeed` derives the seed's
 * base address while `resolveProverSigner` derives its enterprise address, and
 * the raw drivers re-select through the signer, so funding only the base
 * address strands every transaction the outsider builds after that call.
 */
export const fundCommittedFieldShapeOutsider = async (
  harness: CommittedFieldShapeEmulatorHarness,
): Promise<void> => {
  const outsiderAddress = await harness.outsiderLucid.wallet().address();
  const unsigned = await harness.funderLucid
    .newTx()
    .pay.ToAddress(outsiderAddress, { lovelace: 1_000_000_000n })
    .pay.ToAddress(outsiderAddress, { lovelace: 1_000_000_000n })
    .pay.ToAddress(harness.outsiderSigner.address, { lovelace: 1_000_000_000n })
    .pay.ToAddress(harness.outsiderSigner.address, { lovelace: 1_000_000_000n })
    .complete();
  const signed = await unsigned.sign.withWallet().complete();
  await harness.funderLucid.awaitTx(await signed.submit());
};

/**
 * The successor a raw step-01 transaction writes: the walk state back at step
 * 01, or the verdict at step 02. `address` overrides the step's own address.
 */
export type CommittedFieldShapeRawSuccessor =
  | Readonly<{
      step: 1;
      state: CommittedFieldShapeWalkState;
      address?: string;
    }>
  | Readonly<{
      step: 2;
      state: CommittedFieldShapeStep02State;
      address?: string;
    }>;

/**
 * Raw step-01 with no evidence/verdict guard: it writes whatever successor and
 * claim it is given. Used for validator-positive twins and to prove the
 * validator itself refuses fabricated successors and uncommitted preimages.
 * The successor keeps the thread's recorded prover, so a stranger `signer`
 * changes only who signs.
 */
export const submitRawCommittedFieldShapeStep01 = async ({
  harness,
  threadOutRef,
  scenario,
  claim,
  successor,
  referenceScriptUtxo,
  carriageReferenceInputs = [],
  signer = harness.proverSigner,
  lucid = harness.proverLucid,
}: {
  readonly harness: CommittedFieldShapeEmulatorHarness;
  readonly threadOutRef: string;
  readonly scenario: CommittedFieldShapeScenario;
  /**
   * A tier-2/3 claim indexes the complete reference-input set, so it may be
   * spelled from that set once the builder has assembled it.
   */
  readonly claim:
    | CommittedFieldClaim
    | ((referenceInputs: readonly UTxO[]) => CommittedFieldClaim);
  readonly successor: CommittedFieldShapeRawSuccessor;
  readonly referenceScriptUtxo: UTxO;
  /** Published §8 field carriage (chunks and certificate) the claim reads. */
  readonly carriageReferenceInputs?: readonly UTxO[];
  readonly signer?: ResolvedProverSigner;
  readonly lucid?: LucidEvolution;
}): Promise<{ readonly txHash: string; readonly nextThreadOutRef: string }> => {
  const { threadUtxo, threadToken } =
    await requireCommittedFieldShapeThreadUtxo({
      lucid,
      contracts: harness.committedFieldShape,
      categoryId: harness.category.categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  if (threadUtxo.datum == null) {
    throw new Error("raw committed-field-shape step-01 thread has no datum");
  }
  const { fraud_prover } = Data.from(
    threadUtxo.datum,
    CommittedFieldShapeStep01Datum,
  );
  const [stateQueueBlockUtxo, hubOracleUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(
        scenario.setup.fraudulentBlockOutRef,
        "raw committed-field-shape block",
      ),
      label: "raw committed-field-shape block",
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(harness.committedFieldShape.hubOraclePolicyId),
      ),
      unit: toUnit(
        harness.committedFieldShape.hubOraclePolicyId,
        HUB_ORACLE_ASSET_NAME,
      ),
      label: "raw committed-field-shape hub oracle",
    }),
  ]);
  const observedHeader = resolveFraudulentHeaderHash({
    stateQueuePolicyId: harness.committedFieldShape.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (observedHeader !== threadToken.fraudulentHeaderHash) {
    throw new Error("raw step-01 scenario/thread header mismatch");
  }
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(
      harness.realBlueprint,
      PHAS_MEMBERSHIP_WITHDRAW_TITLE,
    ),
  };
  const phasAddress = phasMembershipRewardAddress(network, phasScript);
  const phasCarriage = witnessWithdrawalValidatorCarriage({
    script: phasScript,
    referenceUtxo: harness.witnessReferenceScripts.phasMembershipWithdraw,
    label: "raw committed-field-shape PHAS membership",
  });
  const successorAddress =
    successor.address ??
    harness.committedFieldShape.steps[successor.step - 1]!
      .spendingScriptAddress;
  const successorDatum =
    successor.step === 1
      ? Data.to(
          { fraud_prover, data: successor.state },
          CommittedFieldShapeStep01Datum,
        )
      : Data.to(
          { fraud_prover, data: successor.state },
          CommittedFieldShapeStep02Datum,
        );
  const outputMatches = computationThreadOutputPredicate({
    address: successorAddress,
    datum: successorDatum,
    unit: threadToken.unit,
  });
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    ...carriageReferenceInputs,
    referenceScriptUtxo,
    ...phasCarriage.referenceInputs,
  ];
  const resolvedClaim =
    typeof claim === "function" ? claim(referenceInputs) : claim;
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw committed-field-shape step-01",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "raw committed-field-shape step-01",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "raw committed-field-shape step-01 output",
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
                    "raw committed-field-shape hub oracle",
                  ),
                  state_queue_node_ref_input_index: requireReferenceInputIndex(
                    ctx,
                    stateQueueBlockUtxo,
                    "raw committed-field-shape block",
                  ),
                  native_tx_id: scenario.inclusion.nativeTxId,
                  l2_transaction_source_cbor:
                    scenario.inclusion.l2TransactionSourceCbor,
                  transactions_phas_root:
                    scenario.inclusion.transactionsPhasRoot,
                  tx_membership_proof: scenario.inclusion.txMembershipProof,
                  inclusion_proof_script_withdraw_redeemer_index:
                    requireWithdrawalRedeemerIndex(
                      ctx,
                      phasAddress,
                      "raw committed-field-shape PHAS membership",
                    ),
                },
              ],
            },
            claim: resolvedClaim,
          },
        ],
      },
      CommittedFieldShapeStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs)
    .withdraw(
      phasAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: scenario.inclusion.transactionsPhasRoot,
        keyBytes: scenario.inclusion.nativeTxId,
        valueBytes: scenario.inclusion.l2TransactionSourceCbor,
        membershipProofCbor: scenario.inclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      successorAddress,
      { kind: "inline", value: successorDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await phasCarriage
    .attach(base)
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined) {
    throw new Error("raw step-01 layout did not resolve");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
