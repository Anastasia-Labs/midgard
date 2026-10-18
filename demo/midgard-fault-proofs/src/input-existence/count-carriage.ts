/** One actual counted claim per physical step. The existing verifier and
 * final transaction context determine every withdrawal/reference index. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  builtChunkReferenceIndices,
  chunkedMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  type PublishedProofChunk,
} from "../proof-chunk-carriage.js";
import {
  encodeRawPhasMembershipProofRedeemer,
  getCompiledScript,
  phasMembershipRewardAddress,
} from "../runtime.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "../step-support.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts.js";
import { carriedCountedMembership } from "./counted-membership-carriage.js";

export const prepareCountedMembershipCarriage = <K, V>({
  witness,
  keyBytes,
  valueBytes,
  blueprint,
  network,
  baseReferenceInputs,
  publishedProofChunks = [],
  witnessReferenceScripts,
  label,
}: {
  readonly witness: SDK.RootMembershipProof<K, V>;
  readonly keyBytes: string;
  readonly valueBytes: string;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly baseReferenceInputs: readonly UTxO[];
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly label: string;
}) => {
  const published = publishedProofChunks.length > 0;
  const script = published
    ? chunkedVerifyWithdrawalScript(blueprint)
    : {
        type: "PlutusV3" as const,
        script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
      };
  const reference = published
    ? witnessReferenceScripts?.chunkedVerifyWithdraw
    : witnessReferenceScripts?.phasMembershipWithdraw;
  const validator = witnessWithdrawalValidatorCarriage({
    script,
    referenceUtxo: reference,
    label,
  });
  const rewardAddress = phasMembershipRewardAddress(network, script);
  const referenceInputs = [
    ...baseReferenceInputs,
    ...publishedProofChunks.map(({ utxo }) => utxo),
    ...validator.referenceInputs,
  ];
  return {
    referenceInputs,
    redeemer: (ctx: Parameters<BuildTxWithRedeemer>[0]) =>
      carriedCountedMembership({
        witness,
        ctx,
        directRewardAddress: rewardAddress,
        publishedChunks: publishedProofChunks,
        label,
      }),
    attachWithdrawal: (tx: Parameters<typeof validator.attach>[0]) =>
      validator.attach(
        published
          ? tx.withdraw(rewardAddress, 0n, ((ctx) =>
              chunkedMembershipClaimRedeemer({
                merkleRoot: witness.phas_root,
                keyBytes,
                valueBytes,
                orderedChunkReferenceInputIndices: builtChunkReferenceIndices({
                  ctx,
                  chunks: publishedProofChunks,
                  label,
                }),
              })) satisfies BuildTxWithRedeemer)
          : tx.withdraw(
              rewardAddress,
              0n,
              encodeRawPhasMembershipProofRedeemer({
                root: witness.phas_root,
                keyBytes,
                valueBytes,
                membershipProofCbor: Data.to(witness.proof, SDK.Proof),
              }),
            ),
      ),
    referenceScriptCandidates: [
      { role: label, utxo: reference, expectedScript: script },
    ],
  };
};
