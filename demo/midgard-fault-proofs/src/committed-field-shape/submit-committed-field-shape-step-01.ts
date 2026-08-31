import {
  encodeMidgardNativeTxWitnessSetCompactV1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  type CommittedFieldClaimV1,
  committedFieldShapeEvidenceFromCommittedFieldV1,
  type CommittedFieldShapeEvidenceV1,
  CommittedFieldShapeStep01SpendRedeemer,
  CommittedFieldShapeStep02Datum,
  type CommittedFieldShapeStep02State,
  HUB_ORACLE_ASSET_NAME,
  isCommittedFieldShapeViolationV1,
  MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX_V1,
  type NativeTxInclusionCarriage,
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
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  chunkedMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunkV1,
  requireBuiltChunkReferenceIndices,
  walletInputsExcludingChunks,
} from "../proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessWithdrawalValidatorCarriageV1,
} from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import {
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { CommittedFieldShapeContractsV1 } from "./contracts-v1.js";
import type { PreparedCommittedFieldShapeV1 } from "./prepare-committed-field-shape-v1.js";
import {
  committedFieldShapeStepLabelV1,
  committedFieldShapeSubmitError,
  requireCommittedFieldShapeReferenceScriptV1,
  requireCommittedFieldShapeThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = committedFieldShapeStepLabelV1(0);

export type SubmitCommittedFieldShapeStep01Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  readonly evidence: CommittedFieldShapeEvidenceV1;
  readonly step02State: CommittedFieldShapeStep02State;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly proofCarriage: "redeemer" | "published-chunks";
  readonly awaitedConfirmation: boolean;
};

export const committedFieldShapeInlineClaimDetailsV1 = (
  claim: CommittedFieldClaimV1,
): {
  readonly fieldIndex: number;
  readonly preimage: Buffer;
  readonly witnessSetHash?: string;
} => {
  const witnessSetHash = (() => {
    if (!("WitnessFieldClaim" in claim)) {
      return undefined;
    }
    const h32 = (value: string, label: string): Buffer => {
      if (!/^[0-9a-f]{64}$/u.test(value)) {
        throw committedFieldShapeSubmitError(
          `${label} must be canonical lowercase 32-byte hexadecimal.`,
        );
      }
      return Buffer.from(value, "hex");
    };
    return Buffer.from(
      computeHash32(
        encodeMidgardNativeTxWitnessSetCompactV1({
          addrTxWitsHash: h32(
            claim.WitnessFieldClaim.witness_set.addr_tx_wits_hash,
            "witness claim addr_tx_wits_hash",
          ),
          scriptTxWitsHash: h32(
            claim.WitnessFieldClaim.witness_set.script_tx_wits_hash,
            "witness claim script_tx_wits_hash",
          ),
          redeemerTxWitsHash: h32(
            claim.WitnessFieldClaim.witness_set.redeemer_tx_wits_hash,
            "witness claim redeemer_tx_wits_hash",
          ),
        }),
      ),
    ).toString("hex");
  })();
  const selected =
    "BodyFieldClaim" in claim
      ? {
          kind: "body" as const,
          fieldIndex: Number(claim.BodyFieldClaim.field_index),
          carriage: claim.BodyFieldClaim.carriage,
        }
      : {
          kind: "witness" as const,
          fieldIndex: Number(claim.WitnessFieldClaim.field_index),
          carriage: claim.WitnessFieldClaim.carriage,
        };
  if (
    !Number.isSafeInteger(selected.fieldIndex) ||
    selected.fieldIndex < 0 ||
    selected.fieldIndex >= 9
  ) {
    throw committedFieldShapeSubmitError(
      `claim field index ${selected.fieldIndex.toString()} is outside 0..8.`,
    );
  }
  const expectedKind =
    selected.fieldIndex < MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX_V1
      ? "body"
      : "witness";
  if (selected.kind !== expectedKind) {
    throw committedFieldShapeSubmitError(
      `${selected.kind} claim cannot name field ${selected.fieldIndex.toString()} (${expectedKind} slot).`,
    );
  }
  if (!("Inline" in selected.carriage)) {
    throw committedFieldShapeSubmitError(
      "this submitter wave admits only tier-1 Inline claim carriage.",
    );
  }
  const preimageHex = selected.carriage.Inline.preimage;
  if (!/^(?:[0-9a-f]{2})*$/u.test(preimageHex)) {
    throw committedFieldShapeSubmitError(
      "inline preimage must be canonical lowercase whole-byte hexadecimal.",
    );
  }
  const preimage = Buffer.from(preimageHex, "hex");
  if (preimage.length > MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1) {
    throw committedFieldShapeSubmitError(
      `inline preimage is ${preimage.length.toString()} bytes, above the tier-1 frontier ${MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1.toString()}; tier-2/3 TypeScript carriage is deferred.`,
    );
  }
  return {
    fieldIndex: selected.fieldIndex,
    preimage,
    ...(witnessSetHash === undefined ? {} : { witnessSetHash }),
  };
};

/**
 * Authenticates one committed slot and forwards the on-chain-derived verdict.
 * Every prepared member is re-derived here; a stale or edited plan cannot be
 * converted into a transaction contradicting the validator.
 */
export const submitCommittedFieldShapeStep01 = async ({
  lucid,
  blueprint,
  contracts,
  categoryId,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  prepared,
  publishedProofChunks,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: CommittedFieldShapeContractsV1;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly prepared: Pick<
    PreparedCommittedFieldShapeV1,
    "claim" | "evidence" | "step02State"
  >;
  readonly publishedProofChunks?: readonly PublishedProofChunkV1[];
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitCommittedFieldShapeStep01Result> => {
  const { threadUtxo, threadToken } =
    await requireCommittedFieldShapeThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  requireInitialStepDatum({ threadUtxo, signer });
  const [stateQueueBlockUtxo, hubOracleUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: `${STEP_LABEL} state-queue block UTxO`,
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: `${STEP_LABEL} hub oracle`,
    }),
  ]);
  const stateQueueHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (stateQueueHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw committedFieldShapeSubmitError(
      `state-queue block header hash ${stateQueueHeaderHash} does not match computation-thread header hash ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  requireNativeTxMatchesCompactCbor(txInclusion);
  const { fieldIndex, preimage, witnessSetHash } =
    committedFieldShapeInlineClaimDetailsV1(prepared.claim);
  if (
    witnessSetHash !== undefined &&
    witnessSetHash !== txInclusion.nativeTx.witness_set_hash
  ) {
    throw committedFieldShapeSubmitError(
      `witness claim hashes to compact witness set ${witnessSetHash}, not the committed ${txInclusion.nativeTx.witness_set_hash}.`,
    );
  }
  const evidence = committedFieldShapeEvidenceFromCommittedFieldV1({
    badTxId: txInclusion.nativeTxId,
    fieldIndex,
    committedPreimage: preimage,
  });
  if (!evidence.isViolation) {
    throw committedFieldShapeSubmitError(
      `field ${fieldIndex.toString()} earns non-convicting verdict ${evidence.verdictName}; an honest or disjoint fault cannot advance.`,
    );
  }
  const derivedState: CommittedFieldShapeStep02State = {
    bad_tx_id: evidence.badTxId,
    field_index: BigInt(evidence.fieldIndex),
    verdict: BigInt(evidence.verdict),
  };
  if (
    evidence.badTxId !== prepared.evidence.badTxId ||
    evidence.fieldIndex !== prepared.evidence.fieldIndex ||
    evidence.fieldStride !== prepared.evidence.fieldStride ||
    evidence.committedPreimage !== prepared.evidence.committedPreimage ||
    evidence.committedPreimageByteCount !==
      prepared.evidence.committedPreimageByteCount ||
    evidence.verdict !== prepared.evidence.verdict ||
    evidence.verdictName !== prepared.evidence.verdictName ||
    evidence.isViolation !== prepared.evidence.isViolation ||
    derivedState.bad_tx_id !== prepared.step02State.bad_tx_id ||
    derivedState.field_index !== prepared.step02State.field_index ||
    derivedState.verdict !== prepared.step02State.verdict ||
    !isCommittedFieldShapeViolationV1({
      fieldIndex: Number(derivedState.field_index),
      verdict: Number(derivedState.verdict),
    })
  ) {
    throw committedFieldShapeSubmitError(
      "prepared evidence/state does not equal the verdict re-derived from the claimed committed bytes.",
    );
  }

  signer.selectWallet(lucid);
  const chunks = publishedProofChunks ?? [];
  const carriedByChunks = chunks.length > 0;
  const walletUtxos = await lucid.wallet().getUtxos();
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({ walletUtxos, chunks }),
  );
  const validatedStepReference = requireCommittedFieldShapeReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const chunkedVerifyScript = chunkedVerifyWithdrawalScript(blueprint);
  const chunkedVerifyRewardAddress = phasMembershipRewardAddress(
    network,
    chunkedVerifyScript,
  );
  const membershipCarriage = witnessWithdrawalValidatorCarriageV1({
    script: carriedByChunks ? chunkedVerifyScript : phasMembershipScript,
    referenceUtxo: carriedByChunks
      ? witnessReferenceScripts?.chunkedVerifyWithdraw
      : witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${STEP_LABEL} ${carriedByChunks ? "chunked verify" : "PHAS membership"}`,
  });
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    ...chunks.map((chunk) => chunk.utxo),
    validatedStepReference,
    ...membershipCarriage.referenceInputs,
  ];
  const resolvedChunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks,
    label: STEP_LABEL,
  });
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: derivedState },
    CommittedFieldShapeStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    const common = {
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      hub_ref_input_index: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        `${STEP_LABEL} hub oracle`,
      ),
      state_queue_node_ref_input_index: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        `${STEP_LABEL} state-queue node`,
      ),
      native_tx_id: txInclusion.nativeTxId,
      l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
      transactions_phas_root: txInclusion.transactionsPhasRoot,
    };
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks,
      derived: resolvedChunkIndices,
      label: STEP_LABEL,
    });
    const inclusion: NativeTxInclusionCarriage = carriedByChunks
      ? {
          PublishedChunkInclusion: [
            {
              ...common,
              ordered_chunk_reference_input_indices: resolvedChunkIndices,
            },
          ],
        }
      : {
          RedeemerCarriedInclusion: [
            {
              ...common,
              tx_membership_proof: txInclusion.txMembershipProof,
              inclusion_proof_script_withdraw_redeemer_index:
                requireWithdrawalRedeemerIndex(
                  ctx,
                  phasRewardAddress,
                  `${STEP_LABEL} PHAS membership`,
                ),
            },
          ],
        };
    return Data.to(
      { Continue: [{ inclusion, claim: prepared.claim }] },
      CommittedFieldShapeStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs);
  const withCarriage = carriedByChunks
    ? base.withdraw(chunkedVerifyRewardAddress, 0n, ((_ctx) =>
        chunkedMembershipClaimRedeemer({
          merkleRoot: txInclusion.transactionsPhasRoot,
          keyBytes: txInclusion.nativeTxId,
          valueBytes: txInclusion.l2TransactionSourceCbor,
          orderedChunkReferenceInputIndices: resolvedChunkIndices,
        })) satisfies BuildTxWithRedeemer)
    : base.withdraw(
        phasRewardAddress,
        0n,
        encodeRawPhasMembershipProofRedeemer({
          root: txInclusion.transactionsPhasRoot,
          keyBytes: txInclusion.nativeTxId,
          valueBytes: txInclusion.l2TransactionSourceCbor,
          membershipProofCbor: txInclusion.txMembershipProofCbor,
        }),
      );
  const paid = withCarriage.pay
    .ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = membershipCarriage.attach(paid);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw committedFieldShapeSubmitError(
      "BuildTxWithRedeemer did not resolve the step-01 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof committed-field-shape step-01",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[0].spendingScript,
        },
        {
          role: "membership proof withdrawal",
          utxo: witnessReferenceScripts?.phasMembershipWithdraw,
          expectedScript: phasMembershipScript,
        },
        {
          role: "V1 MPF chunked-verify withdrawal",
          utxo: witnessReferenceScripts?.chunkedVerifyWithdraw,
          expectedScript: chunkedVerifyScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw committedFieldShapeSubmitError(
      `step-01 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    evidence,
    step02State: derivedState,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    proofCarriage: carriedByChunks ? "published-chunks" : "redeemer",
    awaitedConfirmation: awaitConfirmation,
  };
};
