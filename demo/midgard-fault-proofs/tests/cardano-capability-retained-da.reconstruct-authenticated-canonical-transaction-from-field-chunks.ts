import { readFileSync } from "node:fs";
import { isAbsolute } from "node:path";

import {
  collectMidgardAttachedProgramEnvelopes,
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardCekProgramMaterialDaEntry,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  deriveMidgardTxFieldPreimages,
  reconstructMidgardTransaction,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core";
import {
  computeMidgardForcedTxProofCommitment,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { countedMachineTransactionChunkSteps } from "@al-ft/midgard-validation";

type BoundaryCorpusEntry = {
  readonly label: string;
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly canonicalCborHex: string;
  readonly canonicalMaterialSidecarCborHex?: string;
  readonly sourceRawScriptAuditHash?: string;
  readonly productionAdmission:
    | "required"
    | "diagnostic-synthetic-script-witnesses";
  readonly resolvedReferenceUtxos?: readonly SDK.DaPayloadEntry[];
};

const boundaryCorpusInput = (): string | URL => {
  const override = process.env.MIDGARD_BOUNDARY_CORPUS_JSON;
  if (override === undefined) {
    return new URL(
      "./fixtures/cardano-capability-p2-boundary-corpus-v1.json",
      import.meta.url,
    );
  }
  if (!isAbsolute(override)) {
    throw new Error("MIDGARD_BOUNDARY_CORPUS_JSON must be an absolute path");
  }
  return override;
};

export const corpus = JSON.parse(
  readFileSync(boundaryCorpusInput(), "utf8"),
) as {
  readonly schema: string;
  readonly entries: readonly BoundaryCorpusEntry[];
};

export const expectedLabels = [
  "balanced-nested-datum",
  "balanced-nested-redeemer",
  "maximum-constructor-datum-breadth",
  "maximum-constructor-redeemer-breadth",
  "maximum-inline-datum-blob",
  "maximum-list-datum-breadth",
  "maximum-list-redeemer-breadth",
  "maximum-map-datum-breadth",
  "maximum-map-redeemer-breadth",
  "maximum-mint-and-native-policies",
  "maximum-nested-value",
  "maximum-observers-and-native-scripts",
  "maximum-outputs",
  "maximum-redeemers",
  "maximum-reference-inputs",
  "maximum-signers-and-witnesses",
  "maximum-spend-inputs",
  "mixed-size-balanced",
] as const;

export const recomputeCorpusIdentity = (
  canonicalCbor: Uint8Array,
): {
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
} => {
  const exactCanonicalCbor = Buffer.from(canonicalCbor);
  const transaction =
    decodeMidgardNativeTxFullFromCanonicalCbor(exactCanonicalCbor);
  const source =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(exactCanonicalCbor);
  return {
    transactionIdHex: computeMidgardNativeTxId(transaction).toString("hex"),
    transactionCommitmentHex:
      computeMidgardNativeTxProofCommitment(source).toString("hex"),
  };
};

export const verifyFixtureProgramMaterial = ({
  canonicalCbor,
  payload,
}: {
  readonly canonicalCbor: Uint8Array;
  readonly payload: SDK.DaPayload;
}) => {
  const transaction = decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor);
  const envelopes = collectMidgardAttachedProgramEnvelopes(transaction);
  const material = payload.block_body.cek_program_material.map(
    ([rootHex, valueHex]) =>
      decodeMidgardCekProgramMaterialDaEntry(
        Buffer.from(rootHex, "hex"),
        Buffer.from(valueHex, "hex"),
      ),
  );
  return verifyMidgardCekProgramMaterialBundle(envelopes, material);
};

export const reconstructAuthenticatedCanonicalTransactionFromFieldChunks = (
  canonicalCbor: Uint8Array,
  sourceKind: "normal" | "forced",
): {
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly revealStepCount: number;
  readonly maximumChunkBytes: number;
  readonly reconstructed: Buffer;
} => {
  const exactCanonicalCbor = Buffer.from(canonicalCbor);
  const transaction = (
    sourceKind === "forced"
      ? decodeMidgardForcedTxFullFromCanonicalCbor
      : decodeMidgardNativeTxFullFromCanonicalCbor
  )(exactCanonicalCbor);
  const transactionId = computeMidgardNativeTxId(transaction);
  const source = (
    sourceKind === "forced"
      ? deriveMidgardForcedTxProofSourceFromCanonicalCbor
      : deriveMidgardNativeTxProofSourceFromCanonicalCbor
  )(exactCanonicalCbor);
  const transactionCommitment = (
    sourceKind === "forced"
      ? computeMidgardForcedTxProofCommitment
      : computeMidgardNativeTxProofCommitment
  )(source);
  // §4 authenticates a field once, over its whole preimage, against the hash the
  // compact structure carries — which is what `reconstructMidgardTransaction`
  // does for all nine. The retired counted chain verified per-item chunk openings
  // here instead; §4 leaves nothing for such an opening to be checked against.
  const fields = deriveMidgardTxFieldPreimages(exactCanonicalCbor, sourceKind);
  // The machine's own counted trace is still what a dispute step walks, so its
  // step count and widest chunk stay measured here. They are trace measurements,
  // not publication claims (see `countedMachineFieldChunkSteps`).
  const chunks = countedMachineTransactionChunkSteps(
    exactCanonicalCbor,
    sourceKind,
  );
  return {
    transactionIdHex: transactionId.toString("hex"),
    transactionCommitmentHex: transactionCommitment.toString("hex"),
    revealStepCount: chunks.length,
    maximumChunkBytes: Math.max(
      ...chunks.map(({ chunkProof }) => chunkProof.chunk.length),
    ),
    reconstructed: reconstructMidgardTransaction({
      sourceKind,
      transactionId,
      transactionCommitment,
      source,
      fieldPreimages: fields.map((field) => field.preimageCbor),
    }),
  };
};
