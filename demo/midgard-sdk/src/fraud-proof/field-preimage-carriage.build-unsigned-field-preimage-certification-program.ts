import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  type BuildTxWithRedeemer,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { requireUniqueOutputIndex } from "../tx-context-redeemer.js";
import { resolveFieldPreimageCertificationReferenceLayout } from "./field-preimage-carriage.assert-midgard-field-carriage-resolves-at-door.js";
import { resolveProtocolParameters } from "./field-preimage-carriage.build-unsigned-field-preimage-publication-program.js";
import {
  certifyFieldPreimageRedeemer,
  deriveFieldPreimageCertification,
  minimumLovelaceForFieldPreimageCertificate,
  resolveChunkReferenceIndices,
} from "./field-preimage-carriage.resolve-certificate-reference-index.js";

/**
 * Certifies a tier-3 plan: one mint of the policy's constant-name token, one
 * certificate output at the validator's own address carrying the manifest as an
 * inline datum, and the plan's chunks as reference inputs.
 *
 * **The token is not content-addressed (#606, owner ruling 2026-08-16).** It
 * used to be — its name was `blake2b_256(field_index ‖ tx_id)` — and the
 * derivation is retired: every certificate of the policy now wears the same
 * fixed name, which carries no content and no security weight. What is
 * content-bound is the *datum*, whose `field_hash` the mint welds to the
 * verified `chunk_digests` and which the §8.8 door then holds against the
 * commitment it anchored itself. So a consumer of a certificate reads the
 * datum; the token only says which policy minted it (see
 * {@link resolveCertificateReferenceIndex}).
 *
 * The chunk UTxOs must already exist. That is not a builder limitation but a
 * ledger one, and it is worth stating because it is the reason last-chunk
 * publication and certification can never share a transaction: reference inputs
 * are resolved against the UTxO set as it stands *before* the transaction, so
 * an output the same transaction creates is not available to it.
 */
export const buildUnsignedFieldPreimageCertificationProgram = (
  lucid: LucidEvolution,
  {
    plan,
    certificatePolicyId,
    certificateAddress,
    certificatePolicyReferenceUtxo,
    chunkUtxos,
    compactCbor,
    sourceKind,
    witnessSetCompactCbor,
  }: {
    readonly plan: MidgardFieldCarriagePlan;
    readonly certificatePolicyId: string;
    readonly certificateAddress: string;
    /** The published UTxO carrying the certificate policy as a reference script. */
    readonly certificatePolicyReferenceUtxo: UTxO;
    readonly chunkUtxos: readonly UTxO[];
    readonly compactCbor: string;
    readonly sourceKind: 0n | 1n;
    readonly witnessSetCompactCbor: string;
  },
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.tryPromise({
    try: async () => {
      const certification = deriveFieldPreimageCertification(plan);
      if (chunkUtxos.length !== certification.chunkCount) {
        throw new Error(
          "certification must reference exactly the plan's chunks",
        );
      }
      const { referenceInputs } =
        resolveFieldPreimageCertificationReferenceLayout({
          plan,
          certificatePolicyId,
          certificatePolicyReferenceUtxo,
          chunkUtxos,
        });
      const protocolParameters = await resolveProtocolParameters(lucid);
      const lovelace = minimumLovelaceForFieldPreimageCertificate({
        certificateAddress,
        certification,
        certificatePolicyId,
        coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
      });
      const unit = `${certificatePolicyId}${certification.assetNameHex}`;
      return await lucid
        .newTx()
        .readFrom([...referenceInputs])
        .mintAssets({ [unit]: 1n }, ((ctx) =>
          certifyFieldPreimageRedeemer({
            compactCbor,
            sourceKind,
            witnessSetCompactCbor,
            chunkRefInputIndices: resolveChunkReferenceIndices({
              plan,
              referenceInputs: ctx.referenceInputs,
            }),
            outputIndex: Number(
              requireUniqueOutputIndex(
                ctx.outputs,
                (output) =>
                  output.address === certificateAddress &&
                  output.assets[unit] === 1n,
                "field-preimage certificate",
              ),
            ),
          })) satisfies BuildTxWithRedeemer)
        .pay.ToAddressWithData(
          certificateAddress,
          { kind: "inline", value: certification.datumCbor },
          { lovelace, [unit]: 1n },
        )
        .complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new Error(
        `Failed to certify field-preimage carriage: ${
          cause instanceof Error ? cause.message : String(cause)
        }`,
      ),
  });
