/** Build one counted opening using indices from the completed transaction.
 * The caller attaches the corresponding existing direct/published verifier. */
import * as SDK from "@al-ft/midgard-sdk";
import type { RedeemerContext } from "@lucid-evolution/lucid";

import type { PublishedProofChunk } from "../proof-chunk-carriage.js";

export const carriedCountedMembership = <K, V>({
  witness,
  ctx,
  directRewardAddress,
  publishedChunks = [],
  label,
}: {
  readonly witness: SDK.RootMembershipProof<K, V>;
  readonly ctx: RedeemerContext;
  readonly directRewardAddress: string;
  readonly publishedChunks?: readonly PublishedProofChunk[];
  readonly label: string;
}) => {
  const carriage: SDK.MembershipCarriage =
    publishedChunks.length === 0
      ? {
          RedeemerCarriedMembership: {
            membership_proof: witness.proof,
            membership_proof_script_redeemer_index:
              SDK.requireWithdrawalRedeemerIndex(
                ctx,
                directRewardAddress,
                label,
              ),
          },
        }
      : {
          PublishedChunkMembership: [
            {
              ordered_chunk_reference_input_indices: publishedChunks.map(
                (chunk) =>
                  SDK.requireReferenceInputIndex(ctx, chunk.utxo, label),
              ),
            },
          ],
        };
  return {
    domain: witness.domain,
    root: witness.root,
    phas_root: witness.phas_root,
    count: witness.count,
    key: witness.key,
    value: witness.value,
    carriage,
  };
};
