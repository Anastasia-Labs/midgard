import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardBoundedItem,
  buildMidgardLedgerOutputMaterial,
  encodeMidgardNativeScript,
  encodeMidgardTxOutput,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
  type MidgardLedgerOutputCommitmentFacts,
  type MidgardLedgerOutputReferenceScriptLanguage,
} from "@al-ft/midgard-core";
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";

import type { NativeScriptDecodingLedgerTrieHandle } from "../../src/native-script-decoding/evidence.js";
import { nativeScriptDecodingOutpointKey } from "../../src/native-script-decoding/evidence.js";
import type { SubmitStep01TxInclusion } from "../../src/step-support.js";
import { type TransitionTraceReconstruction } from "../../src/transition-trace/reconstruct.js";

// ---------------------------------------------------------------------------
// Reference-script item payloads (§8.2's "handful of nodes")
// ---------------------------------------------------------------------------

/** The single key hash every fixture native script signs under. */
export const DECODING_SIGNER_KEY = Buffer.alloc(28, 0x55);

const SIGNATURE_NODE_HEX = `8200581c${DECODING_SIGNER_KEY.toString("hex")}`;

/** Wrap a payload as the §5.3 versioned tag-0 item (`[0, payload-bytes]`). */
export const decodingItemFromPayload = (payload: Buffer): Buffer => {
  const head =
    payload.length <= 23
      ? Buffer.from([0x40 + payload.length])
      : payload.length < 256
        ? Buffer.from([0x58, payload.length])
        : Buffer.from([
            0x59,
            (payload.length >> 8) & 0xff,
            payload.length & 0xff,
          ]);
  return Buffer.concat([Buffer.from("8200", "hex"), head, payload]);
};

/**
 * `all(sig, <undecodable>)` padded past one chunk boundary: the machine
 * advances three primitive steps (container token, leaf token, frame pop) and
 * refuses the fourth token. Two chunks, so every window the plan carries is
 * the mandatory chunk-plus-next shape.
 */
export const decodingMalformedMultiChunkItem = (): Buffer => {
  const core = Buffer.from(`820182${SIGNATURE_NODE_HEX}820700`, "hex");
  return decodingItemFromPayload(
    Buffer.concat([core, Buffer.alloc(4_100 - core.length, 0)]),
  );
};

/** Maximum field-6 shape: `[item]` is exactly 32,768 bytes. */
export const decodingMalformedMaximumItem = (): Buffer => {
  const core = Buffer.from(`820182${SIGNATURE_NODE_HEX}820700`, "hex");
  return decodingItemFromPayload(
    Buffer.concat([core, Buffer.alloc(32_759 - core.length, 0)]),
  );
};

/** `all(sig)`: canonical, four primitive steps, one chunk. */
export const decodingCanonicalItem = (): Buffer =>
  decodingItemFromPayload(
    encodeMidgardNativeScript({
      type: "all",
      scripts: [{ type: "sig", keyHash: DECODING_SIGNER_KEY }],
    }),
  );

/** A tag-3 (Plutus) item: the direction-B descriptor contradiction. */
export const decodingPlutusItem = (): Buffer =>
  Buffer.from("82034401020304", "hex");

// ---------------------------------------------------------------------------
// Pre-state ledger trie
// ---------------------------------------------------------------------------

export const LEDGER_OUTPUT_ADDRESS = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.alloc(28, 0x99),
]);

/** A plain key-hash output, the descriptor's carrier for every fixture. */
const fixtureOutputCbor = (): Buffer =>
  encodeMidgardTxOutput({
    address: LEDGER_OUTPUT_ADDRESS,
    value: { lovelace: 5_000_000n, assets: new Map() },
  });

export type DecodingLedgerFixture = {
  readonly descriptorCbor: string;
  readonly rootHex: string;
  readonly trie: NativeScriptDecodingLedgerTrieHandle;
  readonly outpointKey: Buffer;
};

/**
 * Files the accused outpoint's descriptor in a fresh MPF, with the
 * reference-script facts named rather than derived: a direction-A fault is
 * precisely a descriptor whose committed reference-script item the canonical
 * builder would refuse to decode.
 */
export const buildDecodingLedgerFixture = async ({
  txIdHex,
  outputIndex,
  referenceScriptItemBytes,
  referenceScriptLanguage,
  siblings = 1,
}: {
  readonly txIdHex: string;
  readonly outputIndex: number;
  readonly referenceScriptItemBytes: Uint8Array;
  readonly referenceScriptLanguage: Exclude<
    MidgardLedgerOutputReferenceScriptLanguage,
    -1
  >;
  readonly siblings?: number;
}): Promise<DecodingLedgerFixture> => {
  const outputCbor = fixtureOutputCbor();
  const base = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex,
    outputCbor,
  });
  const item = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
    itemIndex: outputIndex,
    bytes: referenceScriptItemBytes,
  });
  const {
    version: _version,
    outputIndex: _outputIndex,
    totalLength: _totalLength,
    itemCommitment: _itemCommitment,
    ...baseFacts
  } = base.descriptor;
  const facts: MidgardLedgerOutputCommitmentFacts = {
    ...baseFacts,
    referenceScriptLanguage,
    referenceScriptHash: Buffer.alloc(28, 0x5a),
    referenceScriptTotalLength: referenceScriptItemBytes.length,
    referenceScriptItemCommitment: item.commitment,
  };
  const material = buildMidgardLedgerOutputMaterial({
    outputIndex,
    outputCbor,
    facts,
  });
  const outpointKey = nativeScriptDecodingOutpointKey({
    txIdHex,
    outputIndex,
  });
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(outpointKey, material.descriptorCbor);
  for (let index = 0; index < siblings; index += 1) {
    await trie.insert(
      Buffer.concat([Buffer.alloc(37, 0xee), Buffer.from([index])]),
      Buffer.from([0xd0 + index]),
    );
  }
  const rootHex = (trie.hash as Buffer).toString("hex");
  return {
    descriptorCbor: material.descriptorCbor.toString("hex"),
    rootHex,
    outpointKey,
    trie: {
      rootHex,
      prove: async (target: Buffer) =>
        Buffer.from((await trie.prove(target)).toCBOR()),
    },
  };
};

// ---------------------------------------------------------------------------
// The committed block
// ---------------------------------------------------------------------------

/** A minimal native transaction with the named spend/reference input items. */
export const decodingSubjectTransaction = ({
  spendInputCbors = [],
  referenceInputCbors = [],
  fee = 0n,
}: {
  readonly spendInputCbors?: readonly Buffer[];
  readonly referenceInputCbors?: readonly Buffer[];
  readonly fee?: bigint;
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([...spendInputCbors]),
      referenceInputsPreimageCbor: encodeCbor([...referenceInputCbors]),
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

export const entry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

export const sorted = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

export const bufferEntries = (entries: readonly SDK.DaPayloadEntry[]) =>
  entries.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));

export type DecodingSubjectSource =
  | { readonly kind: "normal"; readonly nativeTx: MidgardNativeTxFull }
  | {
      readonly kind: "forced";
      readonly nativeTx: MidgardNativeTxFull;
      readonly orderKey: SDK.OutputReference;
      readonly verdict: SDK.OperatorVerdict;
    };

export type DecodingBlockFixture = {
  readonly header: SDK.Header;
  readonly headerHash: string;
  readonly payloadEnvelopeCbor: Buffer;
  readonly reconstruction: TransitionTraceReconstruction;
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  /** Direction-A normal-source threads only: the step-01 inclusion evidence. */
  readonly txInclusion: SubmitStep01TxInclusion | null;
  /** Inclusion evidence for every normal transaction committed by the fixture. */
  readonly txInclusions: ReadonlyMap<string, SubmitStep01TxInclusion>;
  readonly forcedOrderKey: SDK.OutputReference | null;
  readonly transactionsPhasRoot: string;
};
