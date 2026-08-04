/**
 * `input-no-idx` (`nonExistentInputNoIndex`) evidence builder (Goal task `Q13`,
 * §9.1 outputs 6-8).
 *
 * **Canonical evidence.** One committed block, authenticated end to end:
 *
 * 1. an authenticated L1 observation of the state-queue header
 *    (`authenticated_cardano_l1`), and
 * 2. the exact `DaPayloadEnvelopeV1` bytes retrieved over the public retained-DA
 *    protocol (`public_or_permissionless_da`),
 *
 * reduced by the Q03 evidence-source API to a `CanonicalBlockEvidenceV1` whose
 * transaction leaves re-commit to the header's counted `transactions_root`
 * under `TransactionsV1RootDomain`. Two committed transactions of that single
 * block are the whole evidence: the **bad** transaction, which spends
 * `(producing_tx_id, output_index)`, and its **producing** transaction, which
 * is committed in the same block and whose canonical outputs list is shorter
 * than or equal to `output_index`.
 *
 * No operator REST/DB/file input can reach the security-grade entry point:
 * {@link prepareInputNoIdxFromCanonicalEvidenceV1} routes provenance through
 * `assertSecurityGradeEvidenceV1` and the native inclusion root through
 * `assertNativeInclusionRootAuthenticatedV1`
 * (`src/evidence/prepare-from-evidence-v1.ts`). The `--midgard-node-url` and
 * `--transactions` entry points below are the operator-diagnostic rehearsal
 * routes and are labelled as such; they can never mint a security-grade claim.
 *
 * **Valid-block negative.** A transaction input that really exists cannot be
 * prepared: {@link InputNoIdxRejectionV1} `input_exists_in_producing_tx` is
 * raised whenever the challenged `output_index` is inside the producer's
 * canonical outputs list, and `no_violating_input` whenever the block contains
 * no such input at all.
 */
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  decodeMidgardNativeByteListPreimage,
  formatUnknownError,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  admitCanonicalEvidenceForProofBuildV1,
  type CanonicalBlockEvidenceV1,
} from "./evidence/index.js";
import { parseHex, stringifyJson } from "./json-file.js";
import {
  buildTrieView,
  type DecodedTransactionMaterial,
  decodeTransactionMaterial,
  type FetchLike,
  fetchNodeBlockTransactions,
  nativeTrieItem,
  type NodeTransactionPayload,
  type PreparedTxInclusionJson,
  readNodeTransactionPayloadsFile,
  requireProof,
} from "./prepare-double-spend.js";

type LucidDataSchema = Parameters<typeof Data.to>[1];

export const INPUT_NO_IDX_EVIDENCE_V1_SCHEMA_VERSION =
  "midgard-input-no-idx-evidence-v1" as const;

export type InputNoIdxRejectionCodeV1 =
  | "block_has_no_transactions"
  | "transactions_root_mismatch"
  | "bad_tx_not_committed"
  | "bad_input_index_out_of_range"
  | "producing_tx_not_committed"
  | "no_violating_input"
  | "input_exists_in_producing_tx"
  | "malformed_native_output";

/** Deterministic, value-free rejection; `detail` carries only public data. */
export class InputNoIdxRejectionV1 extends Error {
  readonly code: InputNoIdxRejectionCodeV1;
  readonly detail: string;

  constructor(code: InputNoIdxRejectionCodeV1, detail: string) {
    super(`${code}: ${detail}`);
    this.name = "InputNoIdxRejectionV1";
    this.code = code;
    this.detail = detail;
  }
}

const reject = (code: InputNoIdxRejectionCodeV1, detail: string): never => {
  throw new InputNoIdxRejectionV1(code, detail);
};

// ## Canonical native output projection
//
// Step 04 carries the producing transaction's complete outputs preimage as
// structured `MidgardTxOutput` PlutusData and re-encodes it on-chain with
// `encode_midgard_tx_output`. These readers invert exactly that encoder
// (`onchain/aiken/lib/midgard/fraud-proofs/native-tx/components.ak:73-181,
// 341-377, 538-585`); anything they cannot invert is refused rather than
// guessed.

type Cursor = { readonly value: number };

const byteAt = (bytes: Buffer, offset: number, label: string): number => {
  const value = bytes[offset];
  if (value === undefined) {
    return reject("malformed_native_output", `${label} exceeds the output`);
  }
  return value;
};

const readUint = (
  bytes: Buffer,
  offset: number,
  label: string,
): readonly [bigint, number] => {
  const tag = byteAt(bytes, offset, label);
  if (tag <= 23) {
    return [BigInt(tag), offset + 1];
  }
  const width =
    tag === 24 ? 1 : tag === 25 ? 2 : tag === 26 ? 4 : tag === 27 ? 8 : 0;
  if (width === 0 || offset + 1 + width > bytes.length) {
    return reject(
      "malformed_native_output",
      `${label} is not a canonical uint`,
    );
  }
  let value = 0n;
  for (let index = 0; index < width; index += 1) {
    value = value * 256n + BigInt(byteAt(bytes, offset + 1 + index, label));
  }
  return [value, offset + 1 + width];
};

const readDefiniteLength = (
  bytes: Buffer,
  offset: number,
  major: 2 | 5,
  label: string,
): readonly [number, number] => {
  const tag = byteAt(bytes, offset, label);
  const base = major << 5;
  if (tag >= base && tag <= base + 23) {
    return [tag - base, offset + 1];
  }
  const width =
    tag === base + 24 ? 1 : tag === base + 25 ? 2 : tag === base + 26 ? 4 : 0;
  if (width === 0 || offset + 1 + width > bytes.length) {
    return reject(
      "malformed_native_output",
      `${label} is not a canonical definite-length header`,
    );
  }
  let length = 0;
  for (let index = 0; index < width; index += 1) {
    length = length * 256 + byteAt(bytes, offset + 1 + index, label);
  }
  return [length, offset + 1 + width];
};

const readBytes = (
  bytes: Buffer,
  offset: number,
  label: string,
): readonly [Buffer, number] => {
  const [length, next] = readDefiniteLength(bytes, offset, 2, label);
  const end = next + length;
  if (end > bytes.length) {
    return reject("malformed_native_output", `${label} exceeds the output`);
  }
  return [Buffer.from(bytes.subarray(next, end)), end];
};

const credentialFrom = (
  isScript: boolean,
  hash: Buffer,
): SDK.MidgardCredential =>
  isScript
    ? { ScriptCredential: [hash.toString("hex")] }
    : { PubKeyCredential: [hash.toString("hex")] };

/** Inverts `encode_midgard_address`. */
const readAddress = (
  bytes: Buffer,
  offset: number,
): readonly [SDK.MidgardAddress, number] => {
  const [payload, next] = readBytes(bytes, offset, "output.address");
  if (payload.length !== 29 && payload.length !== 57) {
    return reject(
      "malformed_native_output",
      `output.address must contain 29 or 57 bytes, got ${payload.length.toString()}`,
    );
  }
  const header = payload[0]!;
  const addressType = Math.floor(header / 16);
  const networkNibble = header - addressType * 16;
  const isProtected = networkNibble >= 8;
  const networkId = isProtected ? networkNibble - 8 : networkNibble;
  if (networkId !== 0 && networkId !== 1) {
    return reject("malformed_native_output", "output.address network id");
  }
  const paymentIsScript =
    addressType === 1 || addressType === 3 || addressType === 7;
  const paymentCredential = credentialFrom(
    paymentIsScript,
    Buffer.from(payload.subarray(1, 29)),
  );
  let stakeCredential: SDK.MidgardCredential | null = null;
  if (payload.length === 57) {
    if (addressType > 3) {
      return reject(
        "malformed_native_output",
        "output.address credential layout",
      );
    }
    stakeCredential = credentialFrom(
      addressType === 2 || addressType === 3,
      Buffer.from(payload.subarray(29, 57)),
    );
  } else if (addressType !== 6 && addressType !== 7) {
    return reject(
      "malformed_native_output",
      "output.address credential layout",
    );
  }
  return [
    {
      protected: isProtected,
      network_id: BigInt(networkId),
      payment_credential: paymentCredential,
      stake_credential: stakeCredential,
    },
    next,
  ];
};

/** Inverts `encode_midgard_value`; units stay `policy_id ++ asset_name`. */
const readValue = (
  bytes: Buffer,
  offset: number,
): readonly [SDK.MidgardValue, number] => {
  if (byteAt(bytes, offset, "output.value") !== 0x82) {
    return reject("malformed_native_output", "output.value must be a pair");
  }
  const [lovelace, afterLovelace] = readUint(
    bytes,
    offset + 1,
    "output.value.lovelace",
  );
  const [policyCount, afterHeader] = readDefiniteLength(
    bytes,
    afterLovelace,
    5,
    "output.value.assets",
  );
  const assets = new Map<string, bigint>();
  let cursor = afterHeader;
  for (let policy = 0; policy < policyCount; policy += 1) {
    const [policyId, afterPolicy] = readBytes(
      bytes,
      cursor,
      "output.value.policy",
    );
    if (policyId.length !== 28) {
      return reject(
        "malformed_native_output",
        "output.value.policy byte length",
      );
    }
    const [assetCount, afterAssetHeader] = readDefiniteLength(
      bytes,
      afterPolicy,
      5,
      "output.value.policy_assets",
    );
    if (assetCount === 0) {
      return reject(
        "malformed_native_output",
        "output.value.policy_assets empty",
      );
    }
    cursor = afterAssetHeader;
    for (let asset = 0; asset < assetCount; asset += 1) {
      const [assetName, afterName] = readBytes(
        bytes,
        cursor,
        "output.value.asset_name",
      );
      const [quantity, afterQuantity] = readUint(
        bytes,
        afterName,
        "output.value.quantity",
      );
      if (quantity <= 0n) {
        return reject("malformed_native_output", "output.value.quantity");
      }
      assets.set(
        Buffer.concat([policyId, assetName]).toString("hex"),
        quantity,
      );
      cursor = afterQuantity;
    }
  }
  return [{ lovelace, assets }, cursor];
};

const SCRIPT_LANGUAGE_BY_TAG: Readonly<
  Record<number, SDK.MidgardScriptLanguage>
> = {
  0: "NativeCardanoScript",
  3: "PlutusV3Script",
  128: "MidgardV1Script",
};

/** Inverts `encode_midgard_versioned_script`. */
const readVersionedScript = (
  bytes: Buffer,
  offset: number,
): readonly [SDK.MidgardVersionedScript, number] => {
  if (byteAt(bytes, offset, "output.script_ref") !== 0x82) {
    return reject(
      "malformed_native_output",
      "output.script_ref must be a pair",
    );
  }
  const first = byteAt(bytes, offset + 1, "output.script_ref.language");
  let tag: number;
  let cursor: number;
  if (first === 0 || first === 3) {
    tag = first;
    cursor = offset + 2;
  } else if (first === 24) {
    tag = byteAt(bytes, offset + 2, "output.script_ref.language");
    cursor = offset + 3;
  } else {
    return reject(
      "malformed_native_output",
      "output.script_ref.language encoding",
    );
  }
  const language = SCRIPT_LANGUAGE_BY_TAG[tag];
  if (language === undefined) {
    return reject("malformed_native_output", "output.script_ref.language tag");
  }
  const [scriptBytes, next] = readBytes(
    bytes,
    cursor,
    "output.script_ref.script_bytes",
  );
  return [{ language, script_bytes: scriptBytes.toString("hex") }, next];
};

/**
 * Projects one canonical native output CBOR item to the exact
 * `MidgardTxOutput` PlutusData the step-04 redeemer carries.
 */
export const midgardTxOutputFromCanonicalCborV1 = (
  bytes: Buffer,
): SDK.MidgardTxOutput => {
  const entryTag = byteAt(bytes, 0, "output");
  if (entryTag < 0xa2 || entryTag > 0xa4) {
    return reject("malformed_native_output", "output map entry count");
  }
  const entryCount = entryTag - 0xa0;
  if (byteAt(bytes, 1, "output.address_key") !== 0) {
    return reject("malformed_native_output", "output.address key must be 0");
  }
  const [address, afterAddress] = readAddress(bytes, 2);
  if (byteAt(bytes, afterAddress, "output.value_key") !== 1) {
    return reject("malformed_native_output", "output.value key must be 1");
  }
  const [value, afterValue] = readValue(bytes, afterAddress + 1);
  const end: Cursor = { value: afterValue };
  let datumCbor: string | null = null;
  let scriptRef: SDK.MidgardVersionedScript | null = null;
  let cursor = end.value;
  if (entryCount > 2) {
    const extraKey = byteAt(bytes, cursor, "output.extra_key");
    cursor += 1;
    if (extraKey === 2) {
      const [datum, afterDatum] = readBytes(bytes, cursor, "output.datum");
      datumCbor = datum.toString("hex");
      cursor = afterDatum;
      if (entryCount === 4) {
        if (byteAt(bytes, cursor, "output.script_key") !== 3) {
          return reject(
            "malformed_native_output",
            "output.script_ref key must be 3",
          );
        }
        const [script, afterScript] = readVersionedScript(bytes, cursor + 1);
        scriptRef = script;
        cursor = afterScript;
      }
    } else if (extraKey === 3 && entryCount === 3) {
      const [script, afterScript] = readVersionedScript(bytes, cursor);
      scriptRef = script;
      cursor = afterScript;
    } else {
      return reject("malformed_native_output", "output optional field layout");
    }
  }
  if (cursor !== bytes.length) {
    return reject("malformed_native_output", "output has trailing bytes");
  }
  return {
    address,
    value,
    datum_cbor: datumCbor,
    script_ref: scriptRef,
  };
};

// ## Builder

/** step-02 material: the complete spend-inputs preimage of the bad tx. */
export type PreparedInputNoIdxInputsPreimageJson = {
  readonly badTxId: string;
  readonly verifiedTxInputsHash: string;
  readonly inputsPreimage: readonly SDK.MidgardTxInput[];
  readonly badInputsIndex: number;
};

/** step-04 material: the complete outputs preimage of the producing tx. */
export type PreparedInputNoIdxOutputsPreimageJson = {
  readonly producingTxId: string;
  readonly producingTxOutputsHash: string;
  readonly outputsPreimage: readonly SDK.MidgardTxOutput[];
  readonly badInputOutputIndex: string;
};

/**
 * Complete-item proof-fit measurement (§3.2/§3.3). Byte counts are of the
 * serialized PlutusData each step redeemer carries directly; no chunked or
 * multi-output representation is produced.
 */
export type PreparedInputNoIdxProofFitV1 = {
  readonly step02InputsPreimageItemCount: number;
  readonly step02InputsPreimageDatumBytes: number;
  readonly step04OutputsPreimageItemCount: number;
  readonly step04OutputsPreimageDatumBytes: number;
  readonly badTxCompactCborBytes: number;
  readonly producingTxCompactCborBytes: number;
  /** Carried complete in one redeemer per step at these measured sizes. */
  readonly completeItemCarriage: true;
};

export type PreparedInputNoIdxOutput = {
  readonly schemaVersion: typeof INPUT_NO_IDX_EVIDENCE_V1_SCHEMA_VERSION;
  readonly violationId: typeof SDK.INPUT_NO_IDX_VIOLATION_ID_V1;
  readonly headerHash: string;
  readonly txCount: number;
  /** Raw MPF root opened by both membership proofs. */
  readonly transactionsPhasRoot: string;
  /** Counted, domain-separated root committed by the block header. */
  readonly committedTransactionsRoot: string;
  readonly expectedTransactionsRoot: {
    readonly value: string;
    readonly matches: boolean;
  };
  readonly evidence: SDK.InputNoIdxEvidenceV1;
  /** step-01 argument: the bad transaction's inclusion in the block. */
  readonly badTxInclusion: PreparedTxInclusionJson;
  /** step-03 argument: the producing transaction's inclusion in the block. */
  readonly producingTxInclusion: PreparedTxInclusionJson;
  readonly step02: PreparedInputNoIdxInputsPreimageJson;
  readonly step02State: SDK.InputNoIdxStep02State;
  readonly step03State: SDK.InputNoIdxStep03State;
  readonly step04: PreparedInputNoIdxOutputsPreimageJson;
  readonly step04State: SDK.InputNoIdxStep04State;
  readonly proofFit: PreparedInputNoIdxProofFitV1;
  readonly files?: {
    readonly badTxInclusionPath: string;
    readonly producingTxInclusionPath: string;
    readonly inputsPreimagePath: string;
    readonly outputsPreimagePath: string;
    readonly planPath: string;
  };
};

const spendInputsOf = (
  tx: DecodedTransactionMaterial,
): readonly SDK.MidgardTxInput[] =>
  tx.inputs.map((input) => ({
    tx_id: input.transactionId.toLowerCase(),
    output_index: input.outputIndex,
  }));

const nativeOutputItems = (
  tx: DecodedTransactionMaterial,
): readonly Buffer[] => {
  try {
    return decodeMidgardNativeByteListPreimage(
      tx.nativeTx.body.outputsPreimageCbor,
      `tx ${tx.nodeTxId} outputs`,
    ).map((bytes) => Buffer.from(bytes));
  } catch (cause) {
    return reject(
      "malformed_native_output",
      `tx ${tx.nodeTxId} outputs preimage is not a canonical native byte list: ${formatUnknownError(cause)}`,
    );
  }
};

type Candidate = {
  readonly badTx: DecodedTransactionMaterial;
  readonly badInputsIndex: number;
  readonly badInput: SDK.MidgardTxInput;
  readonly producingTx: DecodedTransactionMaterial;
  readonly producingOutputs: readonly Buffer[];
};

const findCandidate = ({
  decoded,
  byTxId,
  badTxId,
  badInputsIndex,
  headerHash,
}: {
  readonly decoded: readonly DecodedTransactionMaterial[];
  readonly byTxId: ReadonlyMap<string, DecodedTransactionMaterial>;
  readonly badTxId?: string;
  readonly badInputsIndex?: number;
  readonly headerHash: string;
}): Candidate => {
  const searched =
    badTxId === undefined
      ? decoded
      : (() => {
          const selected = byTxId.get(badTxId);
          if (selected === undefined) {
            return reject(
              "bad_tx_not_committed",
              `bad_tx_id=${badTxId} header_hash=${headerHash}`,
            );
          }
          return [selected];
        })();

  // The block is scanned input by input, so several inputs can fail for
  // different reasons. Report the most informative one: a real input that
  // exists in its producer (the valid-block negative) outranks a pinned index
  // that is out of range, which outranks an input whose producer simply is not
  // in this block (a `non-existent-input` claim, not this family's).
  const failureRank: Readonly<Record<string, number>> = {
    input_exists_in_producing_tx: 3,
    bad_input_index_out_of_range: 2,
    producing_tx_not_committed: 1,
  };
  let pinnedFailure: InputNoIdxRejectionV1 | undefined;
  const pinFailure = (failure: InputNoIdxRejectionV1): void => {
    if (
      pinnedFailure === undefined ||
      (failureRank[failure.code] ?? 0) > (failureRank[pinnedFailure.code] ?? 0)
    ) {
      pinnedFailure = failure;
    }
  };
  for (const badTx of searched) {
    const inputs = spendInputsOf(badTx);
    const indices =
      badInputsIndex === undefined
        ? inputs.map((_, index) => index)
        : [badInputsIndex];
    for (const index of indices) {
      const badInput = inputs[index];
      if (badInput === undefined) {
        pinFailure(
          new InputNoIdxRejectionV1(
            "bad_input_index_out_of_range",
            `bad_tx_id=${badTx.nodeTxId} bad_inputs_index=${index.toString()} input_count=${inputs.length.toString()}`,
          ),
        );
        continue;
      }
      const producingTx = byTxId.get(badInput.tx_id);
      if (producingTx === undefined) {
        pinFailure(
          new InputNoIdxRejectionV1(
            "producing_tx_not_committed",
            `bad_tx_id=${badTx.nodeTxId} producing_tx_id=${badInput.tx_id}; the preimage of the input's transaction id is not in this block, so this is a non-existent-input claim, not input-no-idx`,
          ),
        );
        continue;
      }
      const producingOutputs = nativeOutputItems(producingTx);
      if (
        !SDK.isInputNoIdxViolationV1({
          badInputOutputIndex: badInput.output_index,
          producingTxOutputCount: producingOutputs.length,
        })
      ) {
        pinFailure(
          new InputNoIdxRejectionV1(
            "input_exists_in_producing_tx",
            `bad_tx_id=${badTx.nodeTxId} producing_tx_id=${badInput.tx_id} output_index=${badInput.output_index.toString()} producing_output_count=${producingOutputs.length.toString()}; an existing transaction input cannot be proven non-existent`,
          ),
        );
        continue;
      }
      return {
        badTx,
        badInputsIndex: index,
        badInput,
        producingTx,
        producingOutputs,
      };
    }
  }
  if (pinnedFailure !== undefined) {
    throw pinnedFailure;
  }
  return reject(
    "no_violating_input",
    `header_hash=${headerHash} tx_count=${decoded.length.toString()}`,
  );
};

const txInclusionOf = (
  tx: DecodedTransactionMaterial,
  transactionsPhasRoot: string,
  txMembershipProofCbor: string,
): PreparedTxInclusionJson => ({
  nativeTxId: tx.nodeTxId,
  nativeTx: tx.nativeTxCompact,
  nativeTxCompactCbor: tx.nativeCompactCbor,
  transactionsPhasRoot,
  txMembershipProofCbor,
});

export type PrepareInputNoIdxFromTransactionsOptions = {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly expectedTransactionsRoot: string;
  /** Pin the challenged transaction; otherwise the first violation is used. */
  readonly badTxId?: string;
  /** Pin the challenged spend-input position inside that transaction. */
  readonly badInputsIndex?: string | number;
  readonly outputDir?: string;
};

const parseIndex = (value: string | number | undefined): number | undefined => {
  if (value === undefined) {
    return undefined;
  }
  const parsed = typeof value === "number" ? value : Number(value);
  if (!Number.isInteger(parsed) || parsed < 0) {
    return reject(
      "bad_input_index_out_of_range",
      `--bad-inputs-index must be a non-negative integer, got "${String(value)}"`,
    );
  }
  return parsed;
};

/**
 * Core builder over the already-authenticated transaction material of one
 * committed block. Pure with respect to the network: it never fetches.
 */
export const prepareInputNoIdxFromTransactions = async ({
  headerHash,
  transactions,
  expectedTransactionsRoot,
  badTxId,
  badInputsIndex,
  outputDir,
}: PrepareInputNoIdxFromTransactionsOptions): Promise<PreparedInputNoIdxOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const normalizedExpectedRoot = parseHex(
    expectedTransactionsRoot,
    "--expected-transactions-root",
    32,
  );
  const normalizedBadTxId =
    badTxId === undefined ? undefined : parseHex(badTxId, "--bad-tx-id", 32);
  const normalizedBadInputsIndex = parseIndex(badInputsIndex);

  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  if (decoded.length === 0) {
    reject("block_has_no_transactions", `header_hash=${normalizedHeaderHash}`);
  }
  const byTxId = new Map(decoded.map((tx) => [tx.nodeTxId, tx] as const));

  const candidate = findCandidate({
    decoded,
    byTxId,
    ...(normalizedBadTxId === undefined ? {} : { badTxId: normalizedBadTxId }),
    ...(normalizedBadInputsIndex === undefined
      ? {}
      : { badInputsIndex: normalizedBadInputsIndex }),
    headerHash: normalizedHeaderHash,
  });

  const trie = await buildTrieView(decoded.map(nativeTrieItem));
  const committedTransactionsRoot = await Effect.runPromise(
    SDK.commitCountedRootProgram({
      domain: SDK.ROOT_DOMAINS.transactionsV1,
      phasRoot: trie.root,
      count: BigInt(decoded.length),
    }),
  );
  if (committedTransactionsRoot !== normalizedExpectedRoot) {
    reject(
      "transactions_root_mismatch",
      `header_transactions_root=${normalizedExpectedRoot} derived=${committedTransactionsRoot} derived_count=${decoded.length.toString()}; the prepared proof would not verify against this block`,
    );
  }

  const evidence = SDK.inputNoIdxEvidenceFromCommittedTransactionsV1({
    badTxId: candidate.badTx.nodeTxId,
    badInputsIndex: candidate.badInputsIndex,
    badInput: candidate.badInput,
    producingTxOutputCount: candidate.producingOutputs.length,
  });

  const inputsPreimage = spendInputsOf(candidate.badTx);
  const outputsPreimage = candidate.producingOutputs.map(
    midgardTxOutputFromCanonicalCborV1,
  );
  const step02State = SDK.inputNoIdxStep02StateFromBadTxV1(
    candidate.badTx.nativeTxCompact.body.spend_inputs_hash,
  );
  const step03State = SDK.inputNoIdxStep03StateFromEvidenceV1(evidence);
  const step04State = SDK.inputNoIdxStep04StateFromEvidenceV1({
    evidence,
    producingTxOutputsHash:
      candidate.producingTx.nativeTxCompact.body.outputs_hash,
  });

  const inputsPreimageDatum = Data.to(
    inputsPreimage,
    SDK.MidgardTxInputList as unknown as LucidDataSchema,
  );
  const outputsPreimageDatum = Data.to(
    outputsPreimage,
    SDK.MidgardTxOutputList as unknown as LucidDataSchema,
  );

  const output: PreparedInputNoIdxOutput = {
    schemaVersion: INPUT_NO_IDX_EVIDENCE_V1_SCHEMA_VERSION,
    violationId: SDK.INPUT_NO_IDX_VIOLATION_ID_V1,
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    transactionsPhasRoot: trie.root,
    committedTransactionsRoot,
    expectedTransactionsRoot: {
      value: normalizedExpectedRoot,
      matches: true,
    },
    evidence,
    badTxInclusion: txInclusionOf(
      candidate.badTx,
      trie.root,
      requireProof(trie, nativeTrieItem(candidate.badTx).key, "bad tx"),
    ),
    producingTxInclusion: txInclusionOf(
      candidate.producingTx,
      trie.root,
      requireProof(
        trie,
        nativeTrieItem(candidate.producingTx).key,
        "producing tx",
      ),
    ),
    step02: {
      badTxId: candidate.badTx.nodeTxId,
      verifiedTxInputsHash: step02State.verified_tx_inputs_hash,
      inputsPreimage,
      badInputsIndex: candidate.badInputsIndex,
    },
    step02State,
    step03State,
    step04: {
      producingTxId: candidate.producingTx.nodeTxId,
      producingTxOutputsHash: step04State.producing_tx_outputs_hash,
      outputsPreimage,
      badInputOutputIndex: candidate.badInput.output_index.toString(),
    },
    step04State,
    proofFit: {
      step02InputsPreimageItemCount: inputsPreimage.length,
      step02InputsPreimageDatumBytes: inputsPreimageDatum.length / 2,
      step04OutputsPreimageItemCount: outputsPreimage.length,
      step04OutputsPreimageDatumBytes: outputsPreimageDatum.length / 2,
      badTxCompactCborBytes: candidate.badTx.nativeCompactCbor.length / 2,
      producingTxCompactCborBytes:
        candidate.producingTx.nativeCompactCbor.length / 2,
      completeItemCarriage: true,
    },
  };

  if (outputDir === undefined) {
    return output;
  }
  await mkdir(outputDir, { recursive: true });
  const paths = {
    badTxInclusionPath: join(outputDir, "bad-tx-inclusion.json"),
    producingTxInclusionPath: join(outputDir, "producing-tx-inclusion.json"),
    inputsPreimagePath: join(outputDir, "inputs-preimage.json"),
    outputsPreimagePath: join(outputDir, "outputs-preimage.json"),
    planPath: join(outputDir, "plan.json"),
  };
  await Promise.all([
    writeFile(paths.badTxInclusionPath, stringifyJson(output.badTxInclusion)),
    writeFile(
      paths.producingTxInclusionPath,
      stringifyJson(output.producingTxInclusion),
    ),
    writeFile(paths.inputsPreimagePath, stringifyJson(output.step02)),
    writeFile(paths.outputsPreimagePath, stringifyJson(output.step04)),
    writeFile(
      paths.planPath,
      stringifyJson({
        schemaVersion: output.schemaVersion,
        violationId: output.violationId,
        headerHash: output.headerHash,
        txCount: output.txCount,
        transactionsPhasRoot: output.transactionsPhasRoot,
        committedTransactionsRoot: output.committedTransactionsRoot,
        expectedTransactionsRoot: output.expectedTransactionsRoot,
        evidence: output.evidence,
        step02State: output.step02State,
        step03State: output.step03State,
        step04State: output.step04State,
        proofFit: output.proofFit,
      }),
    ),
  ]);
  return { ...output, files: paths };
};

// ## Layered entry points

export type PrepareInputNoIdxCliConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly expectedTransactionsRoot: string;
  readonly badTxId?: string;
  readonly badInputsIndex?: string | number;
  readonly outputDir?: string;
  readonly fetchImpl?: FetchLike;
};

export type PrepareInputNoIdxFromFileConfig = {
  readonly transactionsPath: string;
  readonly headerHash: string;
  readonly expectedTransactionsRoot: string;
  readonly badTxId?: string;
  readonly badInputsIndex?: string | number;
  readonly outputDir?: string;
};

/**
 * Operator-diagnostic rehearsal route: block material is read from the node's
 * REST surface, an `operator_only_diagnostic_endpoint` under
 * `PROHIBITED_EVIDENCE_TRUST_CLASSES_V1`. Never security grade.
 */
export const prepareInputNoIdxFromNode = async (
  config: PrepareInputNoIdxCliConfig,
): Promise<PreparedInputNoIdxOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
    ...(config.fetchImpl === undefined ? {} : { fetchImpl: config.fetchImpl }),
  });
  return await prepareInputNoIdxFromTransactions({
    headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    ...(config.badTxId === undefined ? {} : { badTxId: config.badTxId }),
    ...(config.badInputsIndex === undefined
      ? {}
      : { badInputsIndex: config.badInputsIndex }),
    ...(config.outputDir === undefined ? {} : { outputDir: config.outputDir }),
  });
};

/** Operator-diagnostic rehearsal route over an `operator_private_file`. */
export const prepareInputNoIdxFromFile = async (
  config: PrepareInputNoIdxFromFileConfig,
): Promise<PreparedInputNoIdxOutput> => {
  const transactions = await readNodeTransactionPayloadsFile(
    config.transactionsPath,
  );
  return await prepareInputNoIdxFromTransactions({
    headerHash: config.headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    ...(config.badTxId === undefined ? {} : { badTxId: config.badTxId }),
    ...(config.badInputsIndex === undefined
      ? {}
      : { badInputsIndex: config.badInputsIndex }),
    ...(config.outputDir === undefined ? {} : { outputDir: config.outputDir }),
  });
};

/**
 * The security-grade entry point (§9.2/§9.1 output 7): an authenticated L1
 * header observation plus public retained-DA block evidence, and nothing else.
 *
 * `admitCanonicalEvidenceForProofBuildV1` re-runs both Q03 gates —
 * `assertSecurityGradeEvidenceV1` over every contributing provenance record
 * and `assertNativeInclusionRootAuthenticatedV1` over the raw transactions MPF
 * root the family's two `NativeTxInclusionArgs` will carry — so a diagnostic,
 * operator-private, or unauthenticated-root record can never reach a
 * submittable `input-no-idx` proof.
 */
export const prepareInputNoIdxFromCanonicalEvidenceV1 = async ({
  evidence,
  badTxId,
  badInputsIndex,
  outputDir,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly badTxId?: string;
  readonly badInputsIndex?: string | number;
  readonly outputDir?: string;
}): Promise<PreparedInputNoIdxOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuildV1(evidence);
  return await prepareInputNoIdxFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(badTxId === undefined ? {} : { badTxId }),
    ...(badInputsIndex === undefined ? {} : { badInputsIndex }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};
