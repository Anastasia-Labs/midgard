/** Durable forced evidence: replay the root and rederive every semantic claim. */
import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeHash28,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  encodeHeaderCbor,
  ForcedInclusionTxV1,
  MissingSignatureForcedStepArgs,
  OutputReference,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { commitCountedRoot } from "../transition-trace/phas.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import {
  detectMissingSignatureForcedTransaction,
  type PreparedMissingSignatureWrongfulRejection,
} from "./wrongful-rejection.js";

export const MISSING_SIGNATURE_FORCED_ARTIFACT =
  "midgard-missing-signature-forced-artifact-v1";
export type MissingSignatureForcedArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof MISSING_SIGNATURE_FORCED_ARTIFACT;
    headerHash: string;
    transactionCbor: string;
    forcedSourceCbor: string;
  }>;
export const missingSignatureForcedArtifact = (
  prepared: PreparedMissingSignatureWrongfulRejection,
  transactionCbor: string,
): MissingSignatureForcedArtifact => ({
  schemaVersion: MISSING_SIGNATURE_FORCED_ARTIFACT,
  headerHash: prepared.headerHash,
  transactionCbor,
  forcedSourceCbor: Data.to(
    { input_index: 0n, output_index: 0n, ...prepared.forcedSource },
    MissingSignatureForcedStepArgs,
  ),
});
export const admitMissingSignatureForcedArtifact = async (
  value: JournalJsonObject,
): Promise<PreparedMissingSignatureWrongfulRejection> => {
  if (
    Object.keys(value).sort().join() !==
      ["schemaVersion", "headerHash", "transactionCbor", "forcedSourceCbor"]
        .sort()
        .join() ||
    value.schemaVersion !== MISSING_SIGNATURE_FORCED_ARTIFACT ||
    typeof value.headerHash !== "string" ||
    typeof value.transactionCbor !== "string" ||
    typeof value.forcedSourceCbor !== "string"
  )
    throw new Error("missingSignature: malformed forced artifact");
  const { headerHash, transactionCbor, forcedSourceCbor } = value;
  if (
    !/^[0-9a-f]{56}$/u.test(headerHash) ||
    !/^(?:[0-9a-f]{2})+$/u.test(transactionCbor)
  )
    throw new Error("missingSignature: malformed forced artifact bytes");
  const source = Data.from(forcedSourceCbor, MissingSignatureForcedStepArgs);
  const { header, membership } = source;
  if (
    Data.to(source, MissingSignatureForcedStepArgs) !== forcedSourceCbor ||
    source.direction !== 1n ||
    source.input_index !== 0n ||
    source.output_index !== 0n ||
    computeHash28(encodeHeaderCbor(header)).toString("hex") !== headerHash ||
    membership.domain !== ROOT_DOMAINS.forcedTransactionsV1 ||
    membership.count !== header.forcedTransactionCount ||
    membership.root !== header.forcedTransactionsRoot ||
    membership.root !==
      (await commitCountedRoot({
        domain: membership.domain,
        phasRoot: membership.phas_root,
        count: membership.count,
      }))
  )
    throw new Error(
      "missingSignature: forced artifact header/root identity differs",
    );
  const steps = membership.proof.map((step) =>
    "Branch" in step
      ? {
          type: "branch" as const,
          skip: Number(step.Branch.skip),
          neighbors: step.Branch.neighbors,
        }
      : "Fork" in step
        ? {
            type: "fork" as const,
            skip: Number(step.Fork.skip),
            neighbor: {
              nibble: Number(step.Fork.neighbor.nibble),
              prefix: step.Fork.neighbor.prefix,
              root: step.Fork.neighbor.root,
            },
          }
        : {
            type: "leaf" as const,
            skip: Number(step.Leaf.skip),
            neighbor: { key: step.Leaf.key, value: step.Leaf.value },
          },
  );
  const root = MpfProof.fromJSON(
    Buffer.from(Data.to(membership.key, OutputReference), "hex"),
    Buffer.from(Data.to(membership.value, ForcedInclusionTxV1), "hex"),
    steps,
  ).verify(true);
  if (root?.toString("hex") !== membership.phas_root)
    throw new Error("missingSignature: forced artifact membership differs");
  const found = detectMissingSignatureForcedTransaction(
    {
      key: membership.key,
      value: membership.value,
      fullTransactionCbor: Buffer.from(transactionCbor, "hex"),
    },
    headerHash,
    0,
  )[0];
  if (found === undefined)
    throw new Error(
      "missingSignature: artifact is not a wrongful RequiredSignerUnsigned rejection",
    );
  const transaction = decodeMidgardNativeTxFullFromCanonicalCbor(
    Buffer.from(transactionCbor, "hex"),
  );
  const witnessSet = deriveMidgardNativeTxWitnessSetCompact(
    transaction.witnessSet,
  );
  return {
    ...found,
    nativeTxCompactCbor: membership.value.source.compact_cbor,
    verifiedWitnessSetHash:
      transaction.compact.transactionWitnessSetHash.toString("hex"),
    witnessSetCompact: {
      addr_tx_wits_hash: witnessSet.addrTxWitsHash.toString("hex"),
      script_tx_wits_hash: witnessSet.scriptTxWitsHash.toString("hex"),
      redeemer_tx_wits_hash: witnessSet.redeemerTxWitsHash.toString("hex"),
    },
    forcedSource: { header, membership, direction: 1n },
  };
};
