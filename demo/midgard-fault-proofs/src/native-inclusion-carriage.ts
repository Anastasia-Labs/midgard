/**
 * Shared transaction-root inclusion carriage for fault-proof step submitters.
 *
 * A caller supplies the reference inputs that are already part of its step
 * transaction. This module appends exactly one published witness script and,
 * on the tier-3 route, the ordered proof-chunk UTxOs. The redeemer constructor
 * and zero-yielding withdrawal are then derived from that same complete set so
 * a caller cannot accidentally disagree about chunk indices or the verifier
 * that authenticated them.
 */
import {
  type NativeTxInclusionCarriage,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  type Network,
  type RedeemerContext,
  type Script,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  chunkedMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunk,
  requireBuiltChunkReferenceIndices,
} from "./proof-chunk-carriage.js";
import {
  encodeRawPhasMembershipProofRedeemer,
  getCompiledScript,
  phasMembershipRewardAddress,
} from "./runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  type SubmitStep01TxInclusion,
} from "./submit-step-01.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "./witness-reference-scripts.js";

type NativeInclusionLayout = {
  readonly input_index: bigint;
  readonly output_index: bigint;
  readonly hub_ref_input_index: bigint;
  readonly state_queue_node_ref_input_index: bigint;
};

export type PreparedNativeTxInclusionCarriage = {
  readonly referenceInputs: UTxO[];
  readonly chunks: readonly PublishedProofChunk[];
  readonly redeemer: (
    ctx: RedeemerContext,
    layout: NativeInclusionLayout,
  ) => NativeTxInclusionCarriage;
  readonly attachWithdrawal: (tx: TxBuilder) => TxBuilder;
  readonly referenceScriptCandidates: readonly {
    readonly role: string;
    readonly utxo: UTxO | undefined;
    readonly expectedScript: Script;
  }[];
};

export const prepareNativeTxInclusionCarriage = ({
  blueprint,
  network,
  txInclusion,
  publishedProofChunks = [],
  baseReferenceInputs,
  witnessReferenceScripts,
  label,
}: {
  readonly blueprint: unknown;
  readonly network: Network;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  readonly baseReferenceInputs: readonly UTxO[];
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly label: string;
}): PreparedNativeTxInclusionCarriage => {
  const carriedByChunks = publishedProofChunks.length > 0;
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const chunkedVerifyScript = chunkedVerifyWithdrawalScript(blueprint);
  const selectedScript = carriedByChunks
    ? chunkedVerifyScript
    : phasMembershipScript;
  const selectedReferenceUtxo = carriedByChunks
    ? witnessReferenceScripts?.chunkedVerifyWithdraw
    : witnessReferenceScripts?.phasMembershipWithdraw;
  const witness = witnessWithdrawalValidatorCarriage({
    script: selectedScript,
    referenceUtxo: selectedReferenceUtxo,
    label: `${label} ${carriedByChunks ? "chunked verify" : "PHAS membership"}`,
  });
  const rewardAddress = phasMembershipRewardAddress(network, selectedScript);
  const referenceInputs = [
    ...baseReferenceInputs,
    ...publishedProofChunks.map((chunk) => chunk.utxo),
    ...witness.referenceInputs,
  ];
  const orderedChunkReferenceInputIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks: publishedProofChunks,
    label,
  });

  return {
    referenceInputs,
    chunks: publishedProofChunks,
    redeemer: (ctx, layout) => {
      requireBuiltChunkReferenceIndices({
        ctx,
        chunks: publishedProofChunks,
        derived: orderedChunkReferenceInputIndices,
        label,
      });
      const common = {
        ...layout,
        native_tx_id: txInclusion.nativeTxId,
        l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
        transactions_phas_root: txInclusion.transactionsPhasRoot,
      };
      return carriedByChunks
        ? {
            PublishedChunkInclusion: [
              {
                ...common,
                ordered_chunk_reference_input_indices:
                  orderedChunkReferenceInputIndices,
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
                    rewardAddress,
                    `${label} PHAS membership`,
                  ),
              },
            ],
          };
    },
    attachWithdrawal: (tx) => {
      const withWithdrawal = carriedByChunks
        ? tx.withdraw(rewardAddress, 0n, ((_ctx) =>
            chunkedMembershipClaimRedeemer({
              merkleRoot: txInclusion.transactionsPhasRoot,
              keyBytes: txInclusion.nativeTxId,
              valueBytes: txInclusion.l2TransactionSourceCbor,
              orderedChunkReferenceInputIndices,
            })) satisfies BuildTxWithRedeemer)
        : tx.withdraw(
            rewardAddress,
            0n,
            encodeRawPhasMembershipProofRedeemer({
              root: txInclusion.transactionsPhasRoot,
              keyBytes: txInclusion.nativeTxId,
              valueBytes: txInclusion.l2TransactionSourceCbor,
              membershipProofCbor: txInclusion.txMembershipProofCbor,
            }),
          );
      return witness.attach(withWithdrawal);
    },
    referenceScriptCandidates: [
      {
        role: carriedByChunks
          ? "V1 MPF chunked-verify withdrawal"
          : "membership proof withdrawal",
        utxo: selectedReferenceUtxo,
        expectedScript: selectedScript,
      },
    ],
  };
};
