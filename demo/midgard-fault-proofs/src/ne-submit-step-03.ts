/**
 * `non-existent-input` step-03 submitter — the initial-ledger absence proof.
 *
 * **Unchanged by #604's re-derivation, and checked to be.** The #575 rebind
 * moved this family's step-02 state and redeemer onto the §2.5 anchor and the
 * §8.8 door; step-03 forwards `missing_input` and the two ledger roots exactly
 * as `midgard/fraud_proofs/no_input/step_03` still declares them, and carries a
 * non-membership proof rather than a field preimage. The banner this header
 * used to carry is gone because the family is re-derived, not because this step
 * was skipped.
 */

import { encodeMidgardSpendInputItemV1 } from "@al-ft/midgard-core/codec";
import {
  type MidgardTxInput,
  NonExistentInputStep03Datum,
  NonExistentInputStep03SpendRedeemer,
  NonExistentInputStep04Datum,
  type NonMembershipCarriage,
  Proof,
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

import { rejectRetiredUnauthenticatedSubmissionRouteV1 } from "./legacy-submission-boundary-v1.js";
import {
  chunkedNonMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunkV1,
  requireBuiltChunkReferenceIndices,
  walletInputsExcludingChunks,
} from "./proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPexcludesProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  phasMembershipRewardAddress,
  readJsonFile,
  type ResolvedProverSigner,
  resolveNonExistentInputDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessSpendingValidatorCarriageV1,
  witnessWithdrawalValidatorCarriageV1,
} from "./witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "./workflow/transaction-boundary-v1.js";

export const PEXCLUDES_EXCLUSION_WITHDRAW_TITLE =
  "pexcludes.exclusion.withdraw";

/**
 * Encodes a `MidgardTxInput` as the node's ledger MPF key: the §5.3 field-0/1
 * item form `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes with a
 * deliberately non-minimal uint16 output index. These are the bytes on-chain
 * `ledger_outref_key` derives via `encode_midgard_tx_input`, not CML's
 * minimal-index `TransactionInput` CBOR, and NOT
 * `cbor.serialise(OutputReference)`.
 */
export const ledgerKeyBytesHex = (input: MidgardTxInput): string =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.from(input.tx_id, "hex"),
    outputIndex: Number(input.output_index),
  }).toString("hex");

export type NeSubmitStep03CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly ledgerNonMembershipProofPath: string;
  readonly awaitConfirmation?: boolean;
};

export type NeSubmitStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  readonly fourthStepAddress: string;
  readonly missingInput: MidgardTxInput;
  readonly missingInputTxId: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly nonMembershipProofScriptRedeemerIndex: number;
  readonly proofCarriage: "redeemer" | "published-chunks";
  readonly publishedChunkOutRefs: readonly string[];
  readonly awaitedConfirmation: boolean;
};

type Step03DatumWithState = NonExistentInputStep03Datum & {
  readonly data: NonNullable<NonExistentInputStep03Datum["data"]>;
};

const requireStep03Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step03DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, NonExistentInputStep03Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error("Step 03 input datum must carry missing-input state data.");
  }
  return datum as Step03DatumWithState;
};

export const neSubmitStep03 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  ledgerNonMembershipProofCbor,
  publishedProofChunks,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly ledgerNonMembershipProofCbor: string;
  /** Authenticated proof chunks, in proof order, for the bounded L1 route. */
  readonly publishedProofChunks?: readonly PublishedProofChunkV1[];
  /** The mandatory published step-03 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<NeSubmitStep03Result> => {
  const { nonExistentInputCategory, contracts } =
    await resolveNonExistentInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const steps = contracts.nonExistentInput.steps;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "non-existent-input step-03 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[2].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at non-existent-input step 03.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: nonExistentInputCategory.categoryId,
    categoryLabel: "non-existent-input",
  });
  const inputDatum = requireStep03Datum({ threadUtxo, signer });
  const missingInput = inputDatum.data.missing_input;
  const keyBytes = ledgerKeyBytesHex(missingInput);
  const proof = Data.from(ledgerNonMembershipProofCbor, Proof);

  signer.selectWallet(lucid);
  const chunks = publishedProofChunks ?? [];
  const carriedByChunks = chunks.length > 0;
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks,
    }),
  );
  const pexcludesScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PEXCLUDES_EXCLUSION_WITHDRAW_TITLE),
  };
  const pexcludesRewardAddress = phasMembershipRewardAddress(
    network,
    pexcludesScript,
  );
  const chunkedVerifyScript = chunkedVerifyWithdrawalScript(blueprint);
  const chunkedVerifyRewardAddress = phasMembershipRewardAddress(
    network,
    chunkedVerifyScript,
  );
  const stepScriptCarriage = witnessSpendingValidatorCarriageV1({
    script: steps[2].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "non-existent-input step 03 validator",
  });
  const nonMembershipCarriage = carriedByChunks
    ? witnessWithdrawalValidatorCarriageV1({
        script: chunkedVerifyScript,
        referenceUtxo: witnessReferenceScripts?.chunkedVerifyWithdraw,
        label: "non-existent-input step 03 chunked verify",
      })
    : witnessWithdrawalValidatorCarriageV1({
        script: pexcludesScript,
        referenceUtxo: witnessReferenceScripts?.pexcludesWithdraw,
        label: "non-existent-input step 03 pexcludes exclusion",
      });
  const referenceInputs = [
    ...chunks.map((chunk) => chunk.utxo),
    ...stepScriptCarriage.referenceInputs,
    ...nonMembershipCarriage.referenceInputs,
  ];
  const resolvedChunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks,
    label: "non-existent-input step 03",
  });
  const step04Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        missing_input_tx_id: missingInput.tx_id,
        blocks_transactions_root: inputDatum.data.blocks_transactions_root,
      },
    },
    NonExistentInputStep04Datum,
  );
  const step04OutputMatches = computationThreadOutputPredicate({
    address: steps[3].spendingScriptAddress,
    datum: step04Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        nonMembershipProofScriptRedeemerIndex: bigint;
      }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "non-existent-input step 03");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "non-existent-input step 03",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step04OutputMatches,
        "non-existent-input step 03 output",
      ),
      nonMembershipProofScriptRedeemerIndex: requireWithdrawalRedeemerIndex(
        ctx,
        carriedByChunks ? chunkedVerifyRewardAddress : pexcludesRewardAddress,
        "non-existent-input step 03 ledger non-membership",
      ),
    };
    resolvedLayout = layout;
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks,
      derived: resolvedChunkIndices,
      label: "non-existent-input step 03",
    });
    const carriage: NonMembershipCarriage = carriedByChunks
      ? {
          PublishedChunkNonMembership: [
            {
              ordered_chunk_reference_input_indices: resolvedChunkIndices,
            },
          ],
        }
      : {
          RedeemerCarriedNonMembership: {
            non_membership_proof: proof,
            non_membership_proof_script_redeemer_index:
              layout.nonMembershipProofScriptRedeemerIndex,
          },
        };
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            non_membership_in_ledger: carriage,
          },
        ],
      },
      NonExistentInputStep03SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const collected = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  // Without published witnesses this step reads nothing, and `readFrom([])`
  // is an error rather than a no-op, so the branch is on whether the
  // carriages produced reference inputs at all.
  const base =
    referenceInputs.length === 0
      ? collected
      : collected.readFrom([...referenceInputs]);
  const withCarriage = carriedByChunks
    ? base.withdraw(chunkedVerifyRewardAddress, 0n, ((_ctx) =>
        chunkedNonMembershipClaimRedeemer({
          merkleRoot: inputDatum.data.blocks_prev_utxos_root,
          keyBytes,
          orderedChunkReferenceInputIndices: resolvedChunkIndices,
        })) satisfies BuildTxWithRedeemer)
    : base.withdraw(
        pexcludesRewardAddress,
        0n,
        encodeRawPexcludesProofRedeemer({
          root: inputDatum.data.blocks_prev_utxos_root,
          keyBytes,
          nonMembershipProofCbor: ledgerNonMembershipProofCbor,
        }),
      );
  const tx = withCarriage.pay
    .ToContract(
      steps[3].spendingScriptAddress,
      { kind: "inline", value: step04Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = nonMembershipCarriage.attach(
    stepScriptCarriage.attach(tx),
  );
  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve non-existent-input step 03 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof non-existent-input step-03",
          utxo: referenceScriptUtxo,
          expectedScript: steps[2].spendingScript,
        },
        {
          role: carriedByChunks
            ? "V1 MPF chunked-verify withdrawal"
            : "V1 MPF pexcludes withdrawal",
          utxo: carriedByChunks
            ? witnessReferenceScripts?.chunkedVerifyWithdraw
            : witnessReferenceScripts?.pexcludesWithdraw,
          expectedScript: carriedByChunks
            ? chunkedVerifyScript
            : pexcludesScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `non-existent-input step-03 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    thirdStepAddress: steps[2].spendingScriptAddress,
    fourthStepAddress: steps[3].spendingScriptAddress,
    missingInput,
    missingInputTxId: missingInput.tx_id,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    nonMembershipProofScriptRedeemerIndex: Number(
      resolvedLayout.nonMembershipProofScriptRedeemerIndex,
    ),
    proofCarriage: carriedByChunks ? "published-chunks" : "redeemer",
    publishedChunkOutRefs: chunks.map((chunk) => outRefLabel(chunk.utxo)),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const neSubmitStep03FromFiles = async (
  config: NeSubmitStep03CliConfig,
): Promise<NeSubmitStep03Result> => {
  rejectRetiredUnauthenticatedSubmissionRouteV1({
    command: "submit-non-existent-input-step-03",
  });
  const [blueprint, deploymentInfo, proofJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.ledgerNonMembershipProofPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await neSubmitStep03({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    ledgerNonMembershipProofCbor: proofJson as string,
    awaitConfirmation: config.awaitConfirmation,
  });
};
