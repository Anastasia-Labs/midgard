import {
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardSpendInputItem,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  midgardFieldCommitment,
  type MidgardRedeemerPurpose,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";

import {
  type AuthenticatedScriptPurpose,
  type MissingRedeemerPurposeKind,
  prepareMissingRedeemerEvidence,
} from "../src/missing-redeemer/family.js";

export const txId = "00".repeat(32);

export const sourceKey = { transactionId: "11".repeat(32), outputIndex: 0n };

const sourceByKind = [
  "resolved-reference",
  "witness",
  "witness",
  "resolved-reference",
] as const;

export const frontier = (): readonly AuthenticatedScriptPurpose[] =>
  ([0, 1, 2, 3] as const).map((purposeKind) => {
    const source = sourceByKind[purposeKind];
    const sourceIndex = purposeKind;
    const sourceOriginKind = source === "witness" ? 0 : 1;
    const sourceKey =
      sourceOriginKind === 0
        ? encodeCbor(BigInt(sourceIndex))
        : encodeMidgardSpendInputItem({
            txId: Buffer.alloc(32, purposeKind + 20),
            outputIndex: purposeKind,
          });
    const scriptHashHex = (purposeKind + 1)
      .toString(16)
      .padStart(2, "0")
      .repeat(28);
    const sourceItemCommitmentHex = (purposeKind + 40)
      .toString(16)
      .padStart(2, "0")
      .repeat(32);
    const sourceLeaf =
      sourceOriginKind === 0
        ? hashMidgardInlineScriptSourceLeaf({
            sourceIndex: BigInt(sourceIndex),
            scriptLanguageTag: 3,
            scriptHash: Buffer.from(scriptHashHex, "hex"),
            scriptTotalLength: 17,
            itemCommitment: Buffer.from(sourceItemCommitmentHex, "hex"),
          })
        : hashMidgardReferenceScriptSourceLeaf({
            sourceKey,
            scriptLanguageTag: 3,
            scriptHash: Buffer.from(scriptHashHex, "hex"),
            scriptTotalLength: 17,
            itemCommitment: Buffer.from(sourceItemCommitmentHex, "hex"),
          });
    return {
      purposeKind,
      purposeIndex: 0,
      scriptHashHex,
      subjectHex: (purposeKind + 5).toString(16).padStart(2, "0").repeat(32),
      source,
      sourceIndex,
      sourceOriginKind,
      sourceKeyHex: sourceKey.toString("hex"),
      sourceLanguageTag: 3,
      sourceTotalLength: 17,
      sourceItemCommitmentHex,
      sourceLeafHashHex: sourceLeaf.toString("hex"),
      traceStateHashHex: "aa".repeat(32),
      workRootHex: "bb".repeat(32),
    };
  });

export const purposeName = ["Spend", "Mint", "Reward", "Receive"] as const;

export const field = (
  entries: readonly (readonly [MissingRedeemerPurposeKind, number])[],
) =>
  encodeMidgardFieldPreimage(
    entries.map(([kind, index]) =>
      encodeMidgardRedeemerWitnessItem({
        purpose: purposeName[kind] satisfies MidgardRedeemerPurpose,
        index: BigInt(index),
        redeemerCbor: Buffer.from("00", "hex"),
        executionUnits: { memory: 1n, steps: 2n },
      }),
    ),
  );

export const evidence = (
  kind: MissingRedeemerPurposeKind,
  entries: readonly (readonly [MissingRedeemerPurposeKind, number])[],
  forced = false,
) => {
  const bytes = field(entries);
  return prepareMissingRedeemerEvidence({
    finding: {
      subject: forced
        ? forcedVerdictSubject({
            transactionId: txId,
            sourceKey,
            rejectionReason: {
              RedeemerMissing: {
                purpose_kind: BigInt(kind),
                purpose_index: 0n,
              },
            },
          })
        : acceptedVerdictSubject(txId),
      purposeKind: kind,
      purposeIndex: 0,
    },
    authenticatedPurpose: frontier()[kind]!,
    redeemerFieldPreimage: bytes,
    committedFieldHashHex: midgardFieldCommitment(bytes).toString("hex"),
  });
};
