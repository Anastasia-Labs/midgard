import { createHash } from "node:crypto";

import { decodeMidgardNativeTxProofFieldLengths } from "@al-ft/midgard-core/codec";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import type { AuthenticatedFieldPreimageLengthEvidence } from "./evidence.js";
import { fieldPreimageLengthCommittedClaim } from "./prepare-accepted.js";
import { prepareFieldPreimageLengthWorkflow } from "./workflow.js";

/** Reopens only the disputed length vector; all other source identities remain exact. */
export const forcedFieldPreimageLengthRawFinding = async ({
  headerHash,
  header,
  body,
}: {
  readonly headerHash: string;
  readonly header: SDK.Header;
  readonly body: SDK.DaPayload["block_body"];
}): Promise<
  | (AuthenticatedFieldPreimageLengthEvidence & { readonly position: bigint })
  | undefined
> => {
  const entries = body.forced_transactions;
  if (BigInt(entries.length) !== header.forcedTransactionCount)
    throw new Error(
      "fieldPreimageLengthMismatch forced cardinality differs from the L1 header",
    );
  const preimages = new Map(body.forced_transaction_preimages);
  if (
    preimages.size !== body.forced_transaction_preimages.length ||
    preimages.size !== entries.length
  )
    throw new Error(
      "fieldPreimageLengthMismatch forced preimages differ in cardinality",
    );
  const trie = await buildTrieView(
    entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const root = await Effect.runPromise(
    SDK.commitCountedRootProgram({
      domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
      phasRoot: trie.root,
      count: header.forcedTransactionCount,
    }),
  );
  if (root !== header.forcedTransactionsRoot)
    throw new Error(
      "fieldPreimageLengthMismatch forced source root differs from the L1 header",
    );
  let finding:
    | (AuthenticatedFieldPreimageLengthEvidence & { readonly position: bigint })
    | undefined;
  for (const [position, [keyCbor, valueCbor]] of entries.entries()) {
    const key = Data.from(keyCbor, SDK.OutputReference) as SDK.OutputReference;
    const value = Data.from(
      valueCbor,
      SDK.ForcedInclusionTxV1,
    ) as SDK.ForcedInclusionTxV1;
    if (
      Data.to(key, SDK.OutputReference) !== keyCbor ||
      Data.to(value, SDK.ForcedInclusionTxV1) !== valueCbor
    )
      throw new Error(
        "fieldPreimageLengthMismatch forced source is not canonical Data",
      );
    const canonicalCbor = preimages.get(keyCbor);
    if (canonicalCbor === undefined)
      throw new Error(
        "fieldPreimageLengthMismatch forced source preimage is absent",
      );
    preimages.delete(keyCbor);
    const material = deriveMidgardForcedTxFaultEvidenceMaterial(
      Buffer.from(canonicalCbor, "hex"),
    );
    if (
      material.transactionId.toString("hex") !== value.tx_id ||
      material.proofSource.compactCbor.toString("hex") !==
        value.submitted_source.compact_cbor ||
      material.proofSource.witnessSetCompactCbor.toString("hex") !==
        value.submitted_source.witness_set_compact_cbor
    )
      throw new Error(
        "fieldPreimageLengthMismatch forced source differs outside its field-length vector",
      );
    const declared = decodeMidgardNativeTxProofFieldLengths(
      Buffer.from(value.submitted_source.field_preimage_lengths_cbor, "hex"),
    );
    const canonical = decodeMidgardNativeTxProofFieldLengths(
      material.proofSource.fieldPreimageLengthsCbor,
    );
    // Raw acceptance is adjudicable. A rejected raw length mismatch is truthful,
    // and whole canonical replay supplies wrongful-rejection findings separately.
    if (value.verdict !== "ForcedTxValid" || finding !== undefined) continue;
    const fieldIndex = canonical.findIndex(
      (length, index) => declared[index] !== length,
    );
    if (fieldIndex < 0) continue;
    const preimage = material.fieldPreimages[fieldIndex]!;
    const base = prepareFieldPreimageLengthWorkflow({
      headerHash,
      transactionId: value.tx_id,
      sourceKind: "forced",
      direction: "wrongfulAcceptance",
      fieldIndex,
      fieldPreimageLengthsCbor: Buffer.from(
        value.submitted_source.field_preimage_lengths_cbor,
        "hex",
      ),
      fieldPreimage: preimage,
    });
    finding = Object.freeze({
      position: BigInt(position),
      prepared: Object.freeze({
        ...base,
        evidenceDigest: createHash("sha256")
          .update(base.evidenceDigest, "hex")
          .update(keyCbor, "hex")
          .update(valueCbor, "hex")
          .digest("hex"),
      }),
      fieldMaterial: Object.freeze({
        nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
        witnessSetCompactCbor:
          material.proofSource.witnessSetCompactCbor.toString("hex"),
      }),
      stageEvidence: Object.freeze({
        forcedDirection: 0n,
        forcedHeader: header,
        forcedMembership: {
          domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
          root,
          phas_root: trie.root,
          count: header.forcedTransactionCount,
          key,
          value,
          proof: Data.from(
            requireProof(
              trie,
              Buffer.from(keyCbor, "hex"),
              "forced length mismatch",
            ),
            SDK.Proof,
          ),
        },
        ...(base.carriage === "Inline"
          ? {
              forcedClaim: fieldPreimageLengthCommittedClaim({
                fieldIndex,
                witnessSetCompactCbor:
                  material.proofSource.witnessSetCompactCbor,
                carriage: { Inline: { preimage: preimage.toString("hex") } },
              }),
            }
          : {}),
      }),
    });
  }
  if (preimages.size !== 0)
    throw new Error(
      "fieldPreimageLengthMismatch forced preimages contain an uncommitted source",
    );
  return finding;
};
