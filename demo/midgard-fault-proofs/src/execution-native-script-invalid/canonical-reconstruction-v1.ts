import {
  buildMidgardBoundedItemV1,
  decodeMidgardAddressBytes,
  encodeCbor,
  encodeMidgardSpendInputItemV1,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  decodeMidgardLedgerTxFromCanonicalCbor,
  type MidgardLedgerTx,
} from "@al-ft/midgard-validation";

export type ExecutionNativeScriptPurposeKindV1 =
  | "spend"
  | "mint"
  | "observe"
  | "receive";

export type ExecutionNativeScriptCanonicalSourceV1 = Readonly<{
  originKind: 0 | 1;
  sourceIndex: number;
  sourceKey: string;
  scriptHash: string;
  languageTag: 0 | 3 | 128;
  versionedItemCbor: string;
  itemCommitment: string;
  totalLength: number;
}>;

export type ExecutionNativeScriptCanonicalPurposeV1 = Readonly<{
  executionIndex: number;
  purposeKind: ExecutionNativeScriptPurposeKindV1;
  purposeKindTag: 0 | 1 | 2 | 3;
  purposeIndex: bigint;
  scriptHash: string;
  subject: string;
  source: ExecutionNativeScriptCanonicalSourceV1;
}>;

export type ExecutionNativeScriptCanonicalReconstructionV1 = Readonly<{
  transactionId: string;
  purposes: readonly ExecutionNativeScriptCanonicalPurposeV1[];
  sources: readonly ExecutionNativeScriptCanonicalSourceV1[];
}>;

const outRefBytes = ({
  txId,
  index,
}: MidgardLedgerTx["spendInputs"][number]): Buffer => {
  const outputIndex = Number(index);
  if (
    !Number.isSafeInteger(outputIndex) ||
    outputIndex < 0 ||
    outputIndex > 65_535
  )
    throw new Error(
      "executionNativeScriptInvalid: out-ref index is outside V1",
    );
  return encodeMidgardSpendInputItemV1({ txId, outputIndex });
};

const outRefKey = (input: MidgardLedgerTx["spendInputs"][number]): string =>
  outRefBytes(input).toString("hex");

const languageTag = (script: MidgardVersionedScript): 0 | 3 | 128 =>
  script.language === "NativeCardano"
    ? 0
    : script.language === "PlutusV3"
      ? 3
      : 128;

const canonicalSource = ({
  script,
  originKind,
  sourceIndex,
  sourceKey,
  itemIndex,
}: {
  script: MidgardVersionedScript;
  originKind: 0 | 1;
  sourceIndex: number;
  sourceKey: Buffer;
  itemIndex: number;
}): ExecutionNativeScriptCanonicalSourceV1 => {
  const item = encodeMidgardVersionedScript(script);
  const bounded = buildMidgardBoundedItemV1({
    fieldIndex: originKind === 0 ? 6 : 2,
    itemIndex,
    bytes: item,
  });
  return Object.freeze({
    originKind,
    sourceIndex,
    sourceKey: sourceKey.toString("hex"),
    scriptHash: hashMidgardVersionedScript(script),
    languageTag: languageTag(script),
    versionedItemCbor: item.toString("hex"),
    itemCommitment: bounded.commitment.toString("hex"),
    totalLength: item.length,
  });
};

const uniqueInOrder = (values: readonly string[]): string[] => {
  const seen = new Set<string>();
  return values.filter((value) => {
    if (seen.has(value)) return false;
    seen.add(value);
    return true;
  });
};

/**
 * Reconstructs the consensus execution order without consulting a retained
 * ScriptSources/NativeScripts trace frontier. `resolvedOutputsByOutRef` is the
 * canonical prior-ledger material; the on-chain twin authenticates every such
 * member against the header's `prev_utxos_root` before advancing its prefix
 * fold.
 */
export const reconstructExecutionNativeScriptPurposesV1 = ({
  canonicalTransactionCbor,
  resolvedOutputsByOutRef,
}: {
  canonicalTransactionCbor: Uint8Array;
  resolvedOutputsByOutRef: ReadonlyMap<string, Uint8Array>;
}): ExecutionNativeScriptCanonicalReconstructionV1 => {
  const tx = decodeMidgardLedgerTxFromCanonicalCbor(canonicalTransactionCbor);
  const sources: ExecutionNativeScriptCanonicalSourceV1[] =
    tx.scriptWitnesses.map((witness) =>
      canonicalSource({
        script: witness.script,
        originKind: 0,
        sourceIndex: witness.index,
        sourceKey: encodeCbor(BigInt(witness.index)),
        itemIndex: witness.index,
      }),
    );
  const sortedReferences = [...tx.referenceInputs].sort((left, right) =>
    Buffer.compare(outRefBytes(left), outRefBytes(right)),
  );
  for (const input of sortedReferences) {
    const key = outRefKey(input);
    const output = resolvedOutputsByOutRef.get(key);
    if (output === undefined)
      throw new Error(
        `executionNativeScriptInvalid: unresolved reference input ${key}`,
      );
    const decoded = awaitImportFreeDecodeOutput(output);
    if (decoded.script_ref === undefined) continue;
    sources.push(
      canonicalSource({
        script: decoded.script_ref,
        originKind: 1,
        sourceIndex: sources.length,
        sourceKey: outRefBytes(input),
        itemIndex: Number(input.index),
      }),
    );
  }

  const purposes: ExecutionNativeScriptCanonicalPurposeV1[] = [];
  const add = ({
    purposeKind,
    purposeKindTag,
    purposeIndex,
    scriptHash,
    subject,
  }: Omit<
    ExecutionNativeScriptCanonicalPurposeV1,
    "executionIndex" | "source"
  >) => {
    const source = sources.find(
      (candidate) => candidate.scriptHash === scriptHash,
    );
    if (source === undefined)
      throw new Error(
        `executionNativeScriptInvalid: purpose ${purposeKind}/${purposeIndex.toString()} has no canonical source`,
      );
    purposes.push(
      Object.freeze({
        executionIndex: purposes.length,
        purposeKind,
        purposeKindTag,
        purposeIndex,
        scriptHash,
        subject,
        source,
      }),
    );
  };

  const sortedSpends = [...tx.spendInputs].sort((left, right) =>
    Buffer.compare(outRefBytes(left), outRefBytes(right)),
  );
  sortedSpends.forEach((input, spendIndex) => {
    const key = outRefKey(input);
    const output = resolvedOutputsByOutRef.get(key);
    if (output === undefined)
      throw new Error(
        `executionNativeScriptInvalid: unresolved spend input ${key}`,
      );
    const credential = decodeMidgardAddressBytes(
      awaitImportFreeDecodeOutput(output).address,
    ).paymentCredential;
    if (credential.kind === "Script")
      add({
        purposeKind: "spend",
        purposeKindTag: 0,
        purposeIndex: BigInt(spendIndex),
        scriptHash: credential.hash.toString("hex"),
        subject: key,
      });
  });

  const policies = uniqueInOrder(
    tx.mint.assets.map(({ policyId }) => policyId.toString("hex")),
  );
  policies.forEach((policy, index) =>
    add({
      purposeKind: "mint",
      purposeKindTag: 1,
      purposeIndex: BigInt(index),
      scriptHash: policy,
      subject: policy,
    }),
  );
  [...tx.requiredObserverHashes]
    .map((hash) => hash.toString("hex"))
    .sort()
    .forEach((observer, index) =>
      add({
        purposeKind: "observe",
        purposeKindTag: 2,
        purposeIndex: BigInt(index),
        scriptHash: observer,
        subject: observer,
      }),
    );
  const receives = uniqueInOrder(
    tx.outputs.flatMap((output) => {
      const address = decodeMidgardAddressBytes(output.address);
      return address.protected && address.paymentCredential.kind === "Script"
        ? [address.paymentCredential.hash.toString("hex")]
        : [];
    }),
  ).sort();
  receives.forEach((scriptHash, index) =>
    add({
      purposeKind: "receive",
      purposeKindTag: 3,
      purposeIndex: BigInt(index),
      scriptHash,
      subject: scriptHash,
    }),
  );
  return Object.freeze({
    transactionId: tx.txId.toString("hex"),
    purposes: Object.freeze(purposes),
    sources: Object.freeze(sources),
  });
};

// Kept behind one local alias so the reconstruction algorithm remains easy to
// compare line-for-line with the on-chain output-descriptor decoder.
import { decodeMidgardTxOutput as awaitImportFreeDecodeOutput } from "@al-ft/midgard-core";
