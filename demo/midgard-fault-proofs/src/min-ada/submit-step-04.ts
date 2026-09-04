import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  MinAdaStep04DatumSchema,
  MinAdaStep04SpendRedeemerSchema,
  MinAdaStep05DatumSchema,
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

import {
  linearFaultStepLabel,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { PEXCLUDES_EXCLUSION_WITHDRAW_TITLE } from "../ne-submit-step-03.js";
import {
  chunkedNonMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunk,
  requireBuiltChunkReferenceIndices,
  walletInputsExcludingChunks,
} from "../proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPexcludesProofRedeemer,
  getCompiledScript,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import {
  MIN_ADA_CATEGORY_LABEL as FAMILY,
  type MinAdaContracts,
} from "./contracts.js";

type State = NonNullable<Data.Static<typeof MinAdaStep04DatumSchema>["data"]>;
type Step04Datum = Data.Static<typeof MinAdaStep04DatumSchema>;
const Step04Datum = asDataType<Step04Datum>(MinAdaStep04DatumSchema);
type Step05Datum = Data.Static<typeof MinAdaStep05DatumSchema>;
const Step05Datum = asDataType<Step05Datum>(MinAdaStep05DatumSchema);
type Redeemer = Data.Static<typeof MinAdaStep04SpendRedeemerSchema>;
const Redeemer = asDataType<Redeemer>(MinAdaStep04SpendRedeemerSchema);

/** Proves that the underfunded post-root member was introduced by this block. */
export const submitMinAdaUtxoStep04 = async ({
  lucid,
  blueprint,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  predecessorNonMembershipProofCbor,
  publishedProofChunks = [],
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: MinAdaContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly predecessorNonMembershipProofCbor: string;
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 3;
  const label = linearFaultStepLabel(FAMILY, stepIndex);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<State>({
    threadUtxo,
    signer,
    schema: Step04Datum,
    family: FAMILY,
    stepIndex,
  });
  const proof = Data.from(predecessorNonMembershipProofCbor, Proof);
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const carriedByChunks = publishedProofChunks.length > 0;
  const proofScript: Script = carriedByChunks
    ? chunkedVerifyWithdrawalScript(blueprint)
    : {
        type: "PlutusV3",
        script: getCompiledScript(
          blueprint,
          PEXCLUDES_EXCLUSION_WITHDRAW_TITLE,
        ),
      };
  const proofRewardAddress = phasMembershipRewardAddress(network, proofScript);
  const proofCarriage = witnessWithdrawalValidatorCarriage({
    script: proofScript,
    referenceUtxo: carriedByChunks
      ? witnessReferenceScripts.chunkedVerifyWithdraw
      : witnessReferenceScripts.pexcludesWithdraw,
    label: `${label} predecessor exclusion`,
  });
  const referenceInputs = [
    ...publishedProofChunks.map(({ utxo }) => utxo),
    stepReference,
    ...proofCarriage.referenceInputs,
  ];
  const chunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks: publishedProofChunks,
    label,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: "PredicateAndCulpabilityAuthenticated",
    },
    Step05Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[4].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const inputIndex = requireInputIndex(ctx, threadUtxo, label);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, label);
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks: publishedProofChunks,
      derived: chunkIndices,
      label,
    });
    const predecessorNonMembership: NonMembershipCarriage = carriedByChunks
      ? {
          PublishedChunkNonMembership: [
            { ordered_chunk_reference_input_indices: chunkIndices },
          ],
        }
      : {
          RedeemerCarriedNonMembership: {
            non_membership_proof: proof,
            non_membership_proof_script_redeemer_index:
              requireWithdrawalRedeemerIndex(ctx, proofRewardAddress, label),
          },
        };
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            predecessor_non_membership: predecessorNonMembership,
          },
        ],
      },
      Redeemer,
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
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom(referenceInputs);
  const withProof = carriedByChunks
    ? base.withdraw(proofRewardAddress, 0n, ((_ctx) =>
        chunkedNonMembershipClaimRedeemer({
          merkleRoot: state.prev_utxos_root,
          keyBytes: state.out_ref_key,
          orderedChunkReferenceInputIndices: chunkIndices,
        })) satisfies BuildTxWithRedeemer)
    : base.withdraw(
        proofRewardAddress,
        0n,
        encodeRawPexcludesProofRedeemer({
          root: state.prev_utxos_root,
          keyBytes: state.out_ref_key,
          nonMembershipProofCbor: predecessorNonMembershipProofCbor,
        }),
      );
  const unsigned = await withProof.pay
    .ToContract(
      contracts.steps[4].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: label,
          utxo: stepReference,
          expectedScript: contracts.steps[stepIndex].spendingScript,
        },
        {
          role: `${label} predecessor exclusion`,
          utxo: proofCarriage.referenceInputs[0]!,
          expectedScript: proofScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `${label}: provider returned ${txHash}, expected ${expectedTxHash}`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    proofCarriage: carriedByChunks ? "published-chunks" : "redeemer",
  } as const;
};
