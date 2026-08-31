/**
 * `no-reference-input` step-03 submitter — a non-membership proof, not a field
 * opening.
 *
 * **Unchanged by #604's re-derivation, and checked to be.** The #575 rebind moved
 * this family's step-02 state and redeemer onto the §2.5 anchor and the §8.8
 * door; this step's state and redeemer are exactly what
 * `midgard/fraud_proofs/no_reference_input/step_03` still declares.
 *
 * `no-reference-input` step-03 submitter (Goal task `Q18`, §9.1 output 8).
 *
 * Structural mirror of the `non-existent-input` chain's step 03
 * (`ne-submit-step-03.ts`): proves the challenged input is absent from the
 * block's prev-utxos ledger via the shared `pexcludes.exclusion.withdraw`
 * validator, then forwards its producing transaction id to step 04. Only the
 * threaded field differs — `missing_reference_input` rather than
 * `missing_input`.
 *
 * Nothing in the prepared JSON is trusted for anything the chain re-derives:
 * the exclusion key and the `prev_utxos_root` the proof must open are both read
 * back from the **on-chain** step-03 datum, and the key is recomputed with the
 * `encode_midgard_tx_input` twin (`ledgerKeyBytesHex`) rather than taken from a
 * file. The prepared file supplies only the MPF proof itself.
 *
 * Proof carriage mirrors Q11 exactly: fitting proofs use the direct pexcludes
 * withdrawal and larger proofs use authenticated published chunks through the
 * shared `NonMembershipCarriage` ABI.
 */
import {
  type MidgardTxInput,
  type NonMembershipCarriage,
  NoReferenceInputStep03Datum,
  NoReferenceInputStep03SpendRedeemer,
  NoReferenceInputStep04Datum,
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

import {
  ledgerKeyBytesHex,
  PEXCLUDES_EXCLUSION_WITHDRAW_TITLE,
} from "./ne-submit-step-03.js";
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
  resolveNoReferenceInputDeploymentContracts,
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

export type SubmitNoReferenceInputStep03CliConfig = SubmitProviderConfig & {
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

export type SubmitNoReferenceInputStep03Result = {
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
  readonly missingReferenceInput: MidgardTxInput;
  readonly missingReferenceInputTxId: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly nonMembershipProofScriptRedeemerIndex: number;
  readonly proofCarriage: "redeemer" | "published-chunks";
  readonly publishedChunkOutRefs: readonly string[];
  readonly awaitedConfirmation: boolean;
};

type Step03DatumWithState = NoReferenceInputStep03Datum & {
  readonly data: NonNullable<NoReferenceInputStep03Datum["data"]>;
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
  const datum = Data.from(threadUtxo.datum, NoReferenceInputStep03Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Step 03 input datum must carry missing-reference-input state data.",
    );
  }
  return datum as Step03DatumWithState;
};

export const submitNoReferenceInputStep03 = async ({
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
  /** Authenticated proof chunks, in proof order, selected only after fit refusal. */
  readonly publishedProofChunks?: readonly PublishedProofChunkV1[];
  /** The mandatory published step-03 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNoReferenceInputStep03Result> => {
  const { noReferenceInputCategory, contracts } =
    await resolveNoReferenceInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const steps = contracts.noReferenceInput.steps;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "no-reference-input step-03 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[2].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at no-reference-input step 03.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: noReferenceInputCategory.categoryId,
    categoryLabel: "no-reference-input",
  });
  const inputDatum = requireStep03Datum({ threadUtxo, signer });
  const missingReferenceInput = inputDatum.data.missing_reference_input;
  const keyBytes = ledgerKeyBytesHex(missingReferenceInput);
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
    label: "no-reference-input step 03 validator",
  });
  const nonMembershipCarriage = carriedByChunks
    ? witnessWithdrawalValidatorCarriageV1({
        script: chunkedVerifyScript,
        referenceUtxo: witnessReferenceScripts?.chunkedVerifyWithdraw,
        label: "no-reference-input step 03 chunked verify",
      })
    : witnessWithdrawalValidatorCarriageV1({
        script: pexcludesScript,
        referenceUtxo: witnessReferenceScripts?.pexcludesWithdraw,
        label: "no-reference-input step 03 pexcludes exclusion",
      });
  const referenceInputs = [
    ...chunks.map((chunk) => chunk.utxo),
    ...stepScriptCarriage.referenceInputs,
    ...nonMembershipCarriage.referenceInputs,
  ];
  const resolvedChunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks,
    label: "no-reference-input step 03",
  });
  const step04Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        missing_reference_input_tx_id: missingReferenceInput.tx_id,
        blocks_transactions_root: inputDatum.data.blocks_transactions_root,
      },
    },
    NoReferenceInputStep04Datum,
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
    requireOwnSpendPurpose(ctx, threadUtxo, "no-reference-input step 03");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "no-reference-input step 03",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step04OutputMatches,
        "no-reference-input step 03 output",
      ),
      nonMembershipProofScriptRedeemerIndex: requireWithdrawalRedeemerIndex(
        ctx,
        carriedByChunks ? chunkedVerifyRewardAddress : pexcludesRewardAddress,
        "no-reference-input step 03 ledger non-membership",
      ),
    };
    resolvedLayout = layout;
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks,
      derived: resolvedChunkIndices,
      label: "no-reference-input step 03",
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
      NoReferenceInputStep03SpendRedeemer,
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
      "BuildTxWithRedeemer did not resolve no-reference-input step 03 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof no-reference-input step-03",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.noReferenceInput.steps[2].spendingScript,
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
      `no-reference-input step-03 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    missingReferenceInput,
    missingReferenceInputTxId: missingReferenceInput.tx_id,
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

export const submitNoReferenceInputStep03FromFiles = async (
  config: SubmitNoReferenceInputStep03CliConfig,
): Promise<SubmitNoReferenceInputStep03Result> => {
  const [blueprint, deploymentInfo, proofJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.ledgerNonMembershipProofPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitNoReferenceInputStep03({
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
