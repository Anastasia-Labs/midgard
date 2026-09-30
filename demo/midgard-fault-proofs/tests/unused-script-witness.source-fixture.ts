import {
  buildMidgardBoundedItem,
  buildMidgardValidationMerkleMembership,
  encodeMidgardVersionedScript,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardScriptPurposeLeaf,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";

import {
  inlineSourceKeyHex,
  prepareUnusedScriptWitnessEvidence as prepareAgainstUniverse,
  type UnusedScriptPurposeOpening,
  type UnusedScriptSourceOpening,
  type UnusedScriptWitnessFinding,
} from "../src/unused-script-witness/family.js";

export const txId = "11".repeat(32);

export const scriptA = {
  language: "PlutusV3" as const,
  scriptBytes: Buffer.from("01", "hex"),
};

export const scriptB = {
  language: "PlutusV3" as const,
  scriptBytes: Buffer.from("02", "hex"),
};

const scripts = [scriptA, scriptB];

export const preimage = encodeMidgardVersionedScriptListPreimage(scripts);

export const sourceFixture = (): readonly UnusedScriptSourceOpening[] => {
  const leaves = scripts.map((script, sourceIndex) => {
    const bytes = encodeMidgardVersionedScript(script);
    return hashMidgardInlineScriptSourceLeaf({
      sourceIndex: BigInt(sourceIndex),
      scriptLanguageTag: 3,
      scriptHash: Buffer.from(hashMidgardVersionedScript(script), "hex"),
      scriptTotalLength: bytes.length,
      itemCommitment: buildMidgardBoundedItem({
        fieldIndex: 6,
        itemIndex: sourceIndex,
        bytes,
      }).commitment,
    });
  });
  return leaves.map((_, sourceIndex) => ({
    frontierIndex: sourceIndex,
    originKind: 0 as const,
    sourceIndex,
    sourceKeyHex: inlineSourceKeyHex(sourceIndex),
    languageTag: 3 as const,
    scriptHashHex: hashMidgardVersionedScript(scripts[sourceIndex]!),
    scriptTotalLength: encodeMidgardVersionedScript(scripts[sourceIndex]!)
      .length,
    itemCommitmentHex: buildMidgardBoundedItem({
      fieldIndex: 6,
      itemIndex: sourceIndex,
      bytes: encodeMidgardVersionedScript(scripts[sourceIndex]!),
    }).commitment.toString("hex"),
    membership: buildMidgardValidationMerkleMembership(leaves, sourceIndex),
  }));
};

export const purposeFixture = (
  hashes: readonly string[],
): readonly UnusedScriptPurposeOpening[] => {
  const leaves = hashes.map((scriptHashHex, frontierIndex) =>
    hashMidgardScriptPurposeLeaf({
      purposeKind: (frontierIndex % 4) as 0 | 1 | 2 | 3,
      purposeIndex: 0n,
      scriptHash: Buffer.from(scriptHashHex, "hex"),
      subject: Buffer.from([frontierIndex]),
    }),
  );
  return leaves.map((_, frontierIndex) => ({
    frontierIndex,
    purposeKind: (frontierIndex % 4) as 0 | 1 | 2 | 3,
    purposeIndex: 0,
    scriptHashHex: hashes[frontierIndex]!,
    purposeSubjectHex: Buffer.from([frontierIndex]).toString("hex"),
    membership: buildMidgardValidationMerkleMembership(leaves, frontierIndex),
  }));
};

export const acceptedFinding = {
  subject: acceptedVerdictSubject(txId),
  scriptIndex: 1,
} as const;

export const forcedFinding = {
  subject: forcedVerdictSubject({
    transactionId: txId,
    sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
    rejectionReason: { UnusedScriptWitness: { script_index: 1n } },
  }),
  scriptIndex: 1,
} as const;

export const prepareUnusedScriptWitnessEvidence = ({
  finding,
  fieldPreimage,
  sources,
  purposes,
}: {
  readonly finding: UnusedScriptWitnessFinding;
  readonly fieldPreimage: Uint8Array;
  readonly sources: readonly UnusedScriptSourceOpening[];
  readonly purposes: readonly UnusedScriptPurposeOpening[];
}) =>
  prepareAgainstUniverse({
    finding,
    fieldPreimage,
    universe: {
      schemaVersion: "midgard-committed-script-universe-v1",
      transactionId: finding.subject.transaction_id,
      universeDigest: "99".repeat(32),
      sources,
      purposes,
    },
  });
