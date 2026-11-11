/**
 * What the trace's producers read of one replayed transaction, derived once,
 * before the walk: its proof source, committed fields, input-resolution
 * schedule with its pre-state proofs, signer set, script sources, outputs
 * and redeemers. Nothing here depends on the walk, and nothing changes it.
 */
import {
  buildMidgardValidationMerkleFrontier,
  buildMidgardValidationMerkleMembership,
  commitMidgardValidationMerkleFrontier,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  deriveMidgardTxFieldPreimages,
  encodeCbor,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardOutputLeaf,
  hashMidgardRedeemerLeaf,
  hashMidgardSignerLeaf,
  midgardBlake2b,
  type MidgardBoundedCollection,
  type MidgardBoundedItem,
  type MidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core/codec";
import {
  readCborArrayHeader,
  readCborBytes,
} from "@al-ft/midgard-core/codec/cbor";
import {
  decodeMidgardForcedTxCompact,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec/forced";
import { Effect } from "effect";

import { decodeMidgardRedeemers } from "../midgard-redeemers.js";
import {
  MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX,
  MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
} from "./canonical-field-item.js";
import { canonicalDecodeItemRefusal } from "./canonical-field-order.js";
import { countedMachineFieldTrace } from "./field-carriage.js";
import {
  emptyMidgardInputResolutionSchedule,
  orderMidgardInputResolutionSchedule,
  prependMidgardInputResolutionSchedule,
} from "./input-resolution.js";
import type { PreStateLedger } from "./pre-state-ledger.js";
import type {
  ResolutionScheduleNode,
  ScriptSourceProofEntry,
} from "./trace-builder-types.js";
import type { ReplayedValidationTransaction } from "./trace-walk.js";
import type { ValidationMachineSignerSetProof } from "./types.js";

/** `T` with every object, array and map in it readonly. A byte buffer keeps
 * its type, so its bytes are read-only only by use. */
export type DeepReadonly<T> = unknown extends T
  ? T
  : T extends
        | Uint8Array
        | bigint
        | boolean
        | number
        | string
        | null
        | undefined
        | ((...args: never[]) => unknown)
    ? T
    : T extends ReadonlyMap<infer K, infer V>
      ? ReadonlyMap<K, DeepReadonly<V>>
      : { readonly [K in keyof T]: DeepReadonly<T[K]> };

type AddressWitnessItem = {
  readonly verificationKey: Buffer;
  readonly signature: Buffer;
  readonly signerHash: Buffer;
};

export type PreparedValidationTransaction = DeepReadonly<
  Omit<ReplayedValidationTransaction, "scriptWitnesses"> & {
    /** A canonical-decode refusal stops the walk before any later semantic item is read. */
    stopsAtCanonicalDecode: boolean;
    proofSource: ReturnType<
      typeof deriveMidgardNativeTxProofSourceFromCanonicalCbor
    >;
    compactProofTransaction:
      | ReturnType<typeof decodeMidgardNativeTxCompact>
      | ReturnType<typeof decodeMidgardForcedTxCompact>;
    compactProofWitnessSet: ReturnType<
      typeof decodeMidgardNativeTxWitnessSetCompact
    >;
    fieldPreimages: ReturnType<typeof deriveMidgardTxFieldPreimages>;
    spendInputsCollection: MidgardBoundedCollection;
    referenceInputsCollection: MidgardBoundedCollection;
    outputsCollection: MidgardBoundedCollection;
    requiredObserversCollection: MidgardBoundedCollection;
    requiredSignersCollection: MidgardBoundedCollection;
    mintCollection: MidgardBoundedCollection;
    scriptWitnessesCollection: MidgardBoundedCollection;
    addressWitnessesCollection: MidgardBoundedCollection;
    redeemerWitnessesCollection: MidgardBoundedCollection;
    /** Input resolution's schedule, each node with its pre-state proof. */
    resolutionScheduleNodes: ResolutionScheduleNode[];
    resolutionScheduleHash: Buffer;
    transactionContextWitnessCbor: Buffer;
    sourceContextWitnessCbor: Buffer;
    /** Field 7 in physical order; none after a canonical-decode refusal. */
    addressWitnessScanItems: {
      item: MidgardBoundedItem;
      decoded: AddressWitnessItem;
    }[];
    /** The witnessed signer hashes, ascending and distinct. */
    canonicalSignerHashes: Buffer[];
    signerLeafHashes: Buffer[];
    signerFrontier: MidgardValidationMerkleFrontier;
    signerFrontierCommitment: Buffer;
    /** The inline script sources, in field order: where the walk's script
     * sources start. */
    inlineScriptSourceEntries: ScriptSourceProofEntry[];
    inlineScriptSourceLeafHashes: Buffer[];
    inlineScriptSourceFrontier: MidgardValidationMerkleFrontier;
    outputCbors: ReturnType<typeof decodeMidgardNativeByteListPreimage>;
    outputLeafHashes: Buffer[];
    outputFrontier: MidgardValidationMerkleFrontier;
    decodedProofRedeemers: ReturnType<typeof decodeMidgardRedeemers>;
    canonicalRedeemerWitnessCbors: Buffer[];
    redeemerLeafHashes: Buffer[];
    redeemerFrontier: MidgardValidationMerkleFrontier;
    emptyValidationFrontier: MidgardValidationMerkleFrontier;
  }
>;

const decodeAddressWitnessItem = (witnessCbor: Buffer): AddressWitnessItem => {
  const header = readCborArrayHeader(witnessCbor, 0, "address_witness");
  if (header.length !== 2) {
    throw new Error("address witness must contain [vkey, signature]");
  }
  const verificationKey = readCborBytes(
    witnessCbor,
    header.nextOffset,
    "address_witness.vkey",
  );
  const signature = readCborBytes(
    witnessCbor,
    verificationKey.nextOffset,
    "address_witness.signature",
  );
  if (
    verificationKey.value.length !== 32 ||
    signature.value.length !== 64 ||
    signature.nextOffset !== witnessCbor.length
  ) {
    throw new Error("address witness has a non-canonical shape");
  }
  return {
    verificationKey: verificationKey.value,
    signature: signature.value,
    signerHash: Buffer.from(
      midgardBlake2b(verificationKey.value, { dkLen: 28 }),
    ),
  };
};

/** Input resolution's schedule over `spend` then `reference` items, in
 * schedule order, each node proven against the pre-state `ledger`. */
const resolutionSchedule = (
  spend: MidgardBoundedCollection,
  reference: MidgardBoundedCollection,
  ledger: PreStateLedger,
) =>
  Effect.gen(function* () {
    const items = orderMidgardInputResolutionSchedule({
      spend: spend.items,
      reference: reference.items,
      keyOf: (item) => item.bytes,
    }).map(({ sourceKind, itemIndex, item }) => ({
      sourceKind,
      itemIndex,
      key: item.bytes,
    }));
    // Hash i commits items i.. ; hash items.length is the empty schedule.
    const hashes = new Array<Buffer>(items.length + 1);
    hashes[items.length] = emptyMidgardInputResolutionSchedule();
    for (let index = items.length - 1; index >= 0; index -= 1) {
      hashes[index] = prependMidgardInputResolutionSchedule({
        ...items[index]!,
        nextHash: hashes[index + 1]!,
      });
    }
    const proofs = yield* Effect.tryPromise({
      try: () => ledger.proveResolution(items.map(({ key }) => key)),
      catch: (cause) =>
        cause instanceof Error
          ? cause
          : new Error("failed to build input-resolution MPF witnesses"),
    });
    if (proofs.length !== items.length) {
      return yield* Effect.fail(
        new Error("the pre-state ledger proved the wrong number of keys"),
      );
    }
    const nodes: ResolutionScheduleNode[] = items.map((item, index) => ({
      ...item,
      nextScheduleHash: hashes[index + 1]!,
      scheduleHash: hashes[index]!,
      proofCbor: proofs[index]!,
    }));
    return { nodes, hash: hashes[0]! };
  });

/** The prepared transaction of `transaction`, its input resolution proven
 * against the pre-state `ledger`. */
export const buildPreparedValidationTransaction = (
  transaction: ReplayedValidationTransaction,
  ledger: PreStateLedger,
): Effect.Effect<PreparedValidationTransaction, Error> =>
  Effect.gen(function* () {
    const { scriptWitnesses, ...replayed } = transaction;
    const { input, contextCbor } = replayed;
    const stopsAtCanonicalDecode = canonicalDecodeItemRefusal(
      input.canonicalTransactionCbor,
      input.sourceKind,
    );
    // Exact submitted bytes bind the source independently of the operator's verdict.
    const proofSource = (
      input.sourceKind === "forced"
        ? deriveMidgardForcedTxProofSourceFromCanonicalCbor
        : deriveMidgardNativeTxProofSourceFromCanonicalCbor
    )(input.canonicalTransactionCbor);
    const compactProofTransaction = (
      input.sourceKind === "forced"
        ? decodeMidgardForcedTxCompact
        : decodeMidgardNativeTxCompact
    )(proofSource.compactCbor);
    const compactProofWitnessSet = decodeMidgardNativeTxWitnessSetCompact(
      proofSource.witnessSetCompactCbor,
    );
    const fieldPreimages = deriveMidgardTxFieldPreimages(
      input.canonicalTransactionCbor,
      input.sourceKind,
    );
    const machineFieldTrace = (fieldIndex: number): MidgardBoundedCollection =>
      countedMachineFieldTrace(
        fieldIndex,
        fieldPreimages[fieldIndex]!.preimageCbor,
      );
    // In field order, so a malformed field fails at the first one.
    const collections = {
      spendInputsCollection: machineFieldTrace(0),
      referenceInputsCollection: machineFieldTrace(1),
      outputsCollection: machineFieldTrace(2),
      requiredObserversCollection: machineFieldTrace(3),
      requiredSignersCollection: machineFieldTrace(4),
      mintCollection: machineFieldTrace(5),
      scriptWitnessesCollection: machineFieldTrace(
        MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
      ),
      addressWitnessesCollection: machineFieldTrace(
        MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX,
      ),
      redeemerWitnessesCollection: machineFieldTrace(8),
    };
    const {
      spendInputsCollection,
      referenceInputsCollection,
      scriptWitnessesCollection,
      addressWitnessesCollection,
    } = collections;
    const resolution = yield* resolutionSchedule(
      spendInputsCollection,
      referenceInputsCollection,
      ledger,
    );
    // Later semantic items remain unread after a canonical terminal; exact source bytes stay committed.
    // Signatures reads field 7 in physical order: CanonicalDecode has refused
    // it unless it is strictly ascending by (signer hash, item bytes).
    const addressWitnessScanItems = (
      stopsAtCanonicalDecode ? [] : addressWitnessesCollection.items
    ).map((item) => ({ item, decoded: decodeAddressWitnessItem(item.bytes) }));
    const canonicalSignerHashes = addressWitnessScanItems
      .map(({ decoded }) => decoded.signerHash)
      .sort(Buffer.compare)
      .filter(
        (hash, index, hashes) =>
          index === 0 || !hash.equals(hashes[index - 1]!),
      );
    const signerLeafHashes = canonicalSignerHashes.map((signerHash) =>
      hashMidgardSignerLeaf(signerHash),
    );
    const signerFrontier =
      buildMidgardValidationMerkleFrontier(signerLeafHashes);
    const inlineScriptSourceEntries = scriptWitnesses.map(
      (witness): ScriptSourceProofEntry => {
        const sourceKey = encodeCbor(BigInt(witness.index));
        const item = scriptWitnessesCollection.items[witness.index]!;
        const scriptLanguageTag =
          witness.script.language === "NativeCardano"
            ? 0
            : witness.script.language === "PlutusV3"
              ? 3
              : 128;
        const scriptHash = Buffer.from(witness.hash);
        return {
          originKind: "inline",
          sourceKey,
          script: witness.script,
          authenticatedVersionedItemBytes: Buffer.from(item.bytes),
          scriptLanguageTag,
          scriptHash,
          scriptTotalLength: item.bytes.length,
          scriptItemCommitment: item.commitment,
          leaf: hashMidgardInlineScriptSourceLeaf({
            sourceIndex: BigInt(witness.index),
            scriptLanguageTag,
            scriptHash,
            scriptTotalLength: item.bytes.length,
            itemCommitment: item.commitment,
          }),
        };
      },
    );
    const inlineScriptSourceLeafHashes = inlineScriptSourceEntries.map(
      (entry) => entry.leaf,
    );
    const outputCbors = decodeMidgardNativeByteListPreimage(
      fieldPreimages[2]!.preimageCbor,
      "v1.outputs",
    );
    const outputLeafHashes = outputCbors.map((outputCbor, outputIndex) =>
      hashMidgardOutputLeaf({ outputIndex, outputCbor }),
    );
    const decodedProofRedeemers = stopsAtCanonicalDecode
      ? []
      : decodeMidgardRedeemers(fieldPreimages[8]!.preimageCbor);
    const canonicalRedeemerWitnessCbors = decodedProofRedeemers.map(
      (redeemer) =>
        encodeCbor([
          BigInt(redeemer.tag),
          redeemer.index,
          Buffer.from(redeemer.dataCborHex, "hex"),
          [redeemer.exUnits.memory, redeemer.exUnits.steps],
        ]),
    );
    const redeemerLeafHashes = canonicalRedeemerWitnessCbors.map(
      (canonicalRedeemerWitnessCbor, redeemerIndex) =>
        hashMidgardRedeemerLeaf({
          redeemerIndex,
          canonicalRedeemerWitnessCbor,
        }),
    );
    return {
      ...replayed,
      stopsAtCanonicalDecode,
      proofSource,
      compactProofTransaction,
      compactProofWitnessSet,
      fieldPreimages,
      ...collections,
      resolutionScheduleNodes: resolution.nodes,
      resolutionScheduleHash: resolution.hash,
      transactionContextWitnessCbor: encodeCbor([
        input.canonicalTransactionCbor,
        contextCbor,
      ]),
      sourceContextWitnessCbor: encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
      ]),
      addressWitnessScanItems,
      canonicalSignerHashes,
      signerLeafHashes,
      signerFrontier,
      signerFrontierCommitment:
        commitMidgardValidationMerkleFrontier(signerFrontier),
      inlineScriptSourceEntries,
      inlineScriptSourceLeafHashes,
      inlineScriptSourceFrontier: buildMidgardValidationMerkleFrontier(
        inlineScriptSourceLeafHashes,
      ),
      outputCbors,
      outputLeafHashes,
      outputFrontier: buildMidgardValidationMerkleFrontier(outputLeafHashes),
      decodedProofRedeemers,
      canonicalRedeemerWitnessCbors,
      redeemerLeafHashes,
      redeemerFrontier:
        buildMidgardValidationMerkleFrontier(redeemerLeafHashes),
      emptyValidationFrontier: buildMidgardValidationMerkleFrontier([]),
    };
  });

/**
 * The §5.1 preimage every field-reading step names — the carriage plan
 * input, not a carriage (#600). One helper rather than thirteen call-site
 * expressions, because "which bytes this step read" has to be answered the
 * same way at every site; the tier those bytes travel under is decided once,
 * later, where a transaction exists.
 */
export const preparedFieldPreimage = (
  prepared: PreparedValidationTransaction,
  fieldIndex: number,
): Buffer => Buffer.from(prepared.fieldPreimages[fieldIndex]!.preimageCbor);

export const preparedSignerMembership = (
  prepared: PreparedValidationTransaction,
  signerIndex: number,
) =>
  buildMidgardValidationMerkleMembership(
    prepared.signerLeafHashes,
    signerIndex,
  );

/** The signer set's proof that `signerHash` is in it, or where it would be. */
export const preparedSignerProof = (
  prepared: PreparedValidationTransaction,
  signerHash: Buffer,
): ValidationMachineSignerSetProof => {
  const { canonicalSignerHashes, signerFrontier: frontier } = prepared;
  const siblings = (index: number) =>
    preparedSignerMembership(prepared, index).siblings;
  const insertionIndex = canonicalSignerHashes.findIndex(
    (candidate) => Buffer.compare(candidate, signerHash) >= 0,
  );
  if (
    insertionIndex >= 0 &&
    canonicalSignerHashes[insertionIndex]!.equals(signerHash)
  ) {
    return {
      kind: "membership",
      frontier,
      signerIndex: insertionIndex,
      siblings: siblings(insertionIndex),
    };
  }
  if (canonicalSignerHashes.length === 0) {
    return { kind: "empty", frontier };
  }
  if (insertionIndex === 0) {
    return {
      kind: "belowFirst",
      frontier,
      firstSignerHash: canonicalSignerHashes[0]!,
      siblings: siblings(0),
    };
  }
  if (insertionIndex === -1) {
    const lastIndex = canonicalSignerHashes.length - 1;
    return {
      kind: "aboveLast",
      frontier,
      lastSignerHash: canonicalSignerHashes[lastIndex]!,
      siblings: siblings(lastIndex),
    };
  }
  return {
    kind: "between",
    frontier,
    lowerIndex: insertionIndex - 1,
    lowerSignerHash: canonicalSignerHashes[insertionIndex - 1]!,
    lowerSiblings: siblings(insertionIndex - 1),
    upperSignerHash: canonicalSignerHashes[insertionIndex]!,
    upperSiblings: siblings(insertionIndex),
  };
};
