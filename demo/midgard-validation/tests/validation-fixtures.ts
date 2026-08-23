import { encodeMidgardCekProgramMaterialSidecarV1 } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  computeScriptIntegrityHashForLanguages,
  decodeMidgardTxOutput,
  deriveMidgardNativeTxBodyCompactV1,
  deriveMidgardNativeTxCompactV1,
  EMPTY_NULL_ROOT,
  encodeMidgardAddressText,
  encodeMidgardFieldPreimageForFieldV1,
  encodeMidgardNativeScript,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardFieldCommitmentV1,
  type MidgardNativeScript,
  type MidgardNativeTxBodyCanonicalV1,
  type MidgardNativeTxFullV1,
  type MidgardNativeTxWitnessSetCanonicalV1,
  midgardRedeemerPurposeFromTagV1,
  type MidgardTxOutput,
  type MidgardTxValidity,
  type MidgardVersionedScript,
  protectMidgardAddress,
  type ScriptLanguageName,
  sortMidgardMintItemsV1,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { CML, Constr, Data } from "@lucid-evolution/lucid";

import { LedgerColumns, type LedgerEntry } from "../src/ledger.js";
import { decodeMidgardSubmittedTxFromCanonicalCbor } from "../src/ledger-tx/codec.js";
import type { PhaseAValidatedTx, QueuedTx } from "../src/types.js";
import { buildPhaseAValidatedTx } from "../src/validation-candidate.js";
import {
  MIDGARD_COINS_PER_UTXO_BYTE_V1,
  minAdaLovelaceV1,
} from "../src/value-accounting.js";

export const EMPTY_CBOR_LIST = Buffer.from([0x80]);
export const EMPTY_CBOR_NULL = Buffer.from([0xf6]);

const TEST_PRIVATE_KEY = CML.PrivateKey.generate_ed25519();
const TEST_PUBLIC_KEY = TEST_PRIVATE_KEY.to_public();
const TEST_PUBLIC_KEY_HASH = TEST_PUBLIC_KEY.hash();

export const TEST_SIGNER_HASH = Buffer.from(
  TEST_PUBLIC_KEY_HASH.to_raw_bytes(),
).toString("hex");

export const TEST_ADDRESS_BYTES = Buffer.from(
  CML.EnterpriseAddress.new(0, CML.Credential.new_pub_key(TEST_PUBLIC_KEY_HASH))
    .to_address()
    .to_raw_bytes(),
);
export const TEST_ADDRESS_TEXT = encodeMidgardAddressText(TEST_ADDRESS_BYTES);

type NativeTxOptions = {
  readonly spendInputs?: readonly Buffer[];
  readonly referenceInputs?: readonly Buffer[];
  readonly outputs?: readonly Buffer[];
  readonly fee?: bigint;
  readonly validity?: MidgardTxValidity;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
  readonly requiredObserverItems?: readonly Uint8Array[];
  readonly requiredSignerItems?: readonly Uint8Array[];
  readonly scriptWitnesses?: readonly MidgardVersionedScript[];
  readonly redeemerTxWitsPreimageCbor?: Buffer;
  readonly mintPreimageCbor?: Buffer;
  readonly scriptLanguages?: readonly ScriptLanguageName[];
  readonly auxiliaryDataHash?: Buffer;
  readonly networkId?: bigint;
  readonly invalidVkeyWitness?: true;
  readonly omitVkeyWitness?: true;
  readonly privateKey?: CML.PrivateKey;
  readonly version?: 1n;
};

export type NativeTxFixture = {
  readonly tx: MidgardNativeTxFullV1;
  readonly txId: Buffer;
  readonly txCbor: Buffer;
};

export const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  encodeCbor(items.map((item) => Buffer.from(item)));

// Out-ref bytes are §5.3's fixed-index field-0/1 item (38 bytes), which is also
// the ledger MPF trie key on-chain `ledger_outref_key` derives. Fixtures have to
// build them with the same encoder the production producers use: CML's
// minimal-index TransactionInput CBOR would make every fixture transaction
// re-encode to a different tx id than the one it was built with.
export const outRefFromByte = (byte: number, index = 0n): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.alloc(32, byte),
    outputIndex: Number(index),
  });

export const outRefFromTxId = (txId: Buffer, index = 0n): Buffer =>
  encodeMidgardSpendInputItemV1({ txId, outputIndex: Number(index) });

/**
 * The lovelace every fixture output that a fixture transaction PRODUCES is
 * funded with.
 *
 * RE-AUTHORED, NOT SUPPRESSED (#618 ruling 1; R8 of decision 0005). These
 * fixtures used to produce 10-lovelace outputs, which was admissible while the
 * minimum-Ada floor was unwired arithmetic. The ValueAndMint output-descriptor
 * scan now convicts an under-funded output with `E_MIN_ADA`, so a 10-lovelace
 * output is no longer a valid transaction at all and a fixture built from one
 * measures the min-Ada rejection instead of whatever it was written to
 * measure.
 *
 * 10 ADA is a round number chosen to clear the floor with room to spare rather
 * than to sit on it: at the pinned rate it funds any canonical output up to
 * 2,160 serialized bytes, which is far past every shape these fixtures build.
 * `phase B validation > funds every produced fixture output above the minimum-Ada floor`
 * measures that headroom rather than asserting it, so a rate or intercept
 * change fails there instead of scattering `E_MIN_ADA` across unrelated
 * fixtures. The adjacent boundary itself is pinned where it belongs -- in
 * min-ada-twin-cross-check-v1.test.ts and in the Aiken wiring vectors -- not
 * here.
 *
 * Pre-state (input and reference) outputs are deliberately NOT re-funded: the
 * wiring gates outputs a transaction produces (MIN-ADA-TX), not outputs it
 * resolves from prior state, whose under-funding is the separate
 * MIN-ADA-UTXO shape Q27 owns.
 */
export const FUNDED_OUTPUT_LOVELACE_V1 = 10_000_000n;

export const makeOutput = (
  lovelace: bigint,
  address = TEST_ADDRESS_BYTES,
  assets: ReadonlyMap<string, ReadonlyMap<string, bigint>> = new Map(),
): Buffer => {
  const output: MidgardTxOutput = {
    address,
    value: {
      lovelace,
      assets,
    },
  };
  return encodeMidgardTxOutput(output);
};

const encodeBoundedDatumChunkV1 = (payload: Buffer): Buffer => {
  if (payload.length < 24) {
    return Buffer.concat([Buffer.from([0x40 + payload.length]), payload]);
  }
  if (payload.length <= 0xff) {
    return Buffer.concat([Buffer.from([0x58, payload.length]), payload]);
  }
  throw new Error("bounded datum chunk must stay below 256 bytes");
};

/**
 * A canonical Plutus-data filler whose ENCODED length is exactly `datumBytes`:
 * one definite byte string when one fits, otherwise an indefinite list of
 * bounded byte strings, which matches Aiken's `cbor.serialise` convention.
 *
 * Sizing by encoded length rather than by payload length is what makes every
 * target reachable. A chunk of `take` data bytes costs `(take < 24 ? 1 : 2) +
 * take` bytes, so a list of 64-byte chunks plus one remainder only reaches
 * `2 + 66k + c` for `c` in `{0} u {2..24} u {26..66}` -- two lengths in every
 * 66 are skipped, and a payload-driven search simply fails on them. Handing
 * one full chunk back and closing the gap with a pair of chunks reaches both
 * skipped residues, so every length from 67 up is constructible.
 */
export const canonicalDatumOfExactLengthV1 = (datumBytes: number): Buffer => {
  const unreachable = (): never => {
    throw new Error(
      `no canonical datum encodes to exactly ${datumBytes.toString()} bytes`,
    );
  };
  const chunkBytes = (take: number): number => (take < 24 ? 1 : 2) + take;
  if (datumBytes <= 66) {
    const take = datumBytes <= 24 ? datumBytes - 1 : datumBytes - 2;
    if (take < 0 || chunkBytes(take) !== datumBytes) {
      return unreachable();
    }
    return encodeBoundedDatumChunkV1(Buffer.alloc(take, 0xa5));
  }
  const takes: number[] = [];
  let remaining = datumBytes - 2;
  while (remaining > 66) {
    takes.push(64);
    remaining -= 66;
  }
  if (remaining === 1 || remaining === 25) {
    if (takes.length === 0) {
      return unreachable();
    }
    takes.pop();
    takes.push(63);
    remaining += 66 - chunkBytes(63);
  }
  if (remaining > 0) {
    takes.push(remaining <= 24 ? remaining - 1 : remaining - 2);
  }
  return Buffer.concat([
    Buffer.from([0x9f]),
    ...takes.map((take) => encodeBoundedDatumChunkV1(Buffer.alloc(take, 0xa5))),
    Buffer.from([0xff]),
  ]);
};

/**
 * Builds a canonical output item whose exact encoded length equals
 * `targetItemBytes`, funded at exactly its own minimum-Ada floor.
 *
 * RE-AUTHORED, NOT SUPPRESSED (#618 ruling 1; R8 of decision 0005). The four
 * carriage suites each carried their own copy of this builder, each producing
 * 10-lovelace items, which the ValueAndMint output-descriptor scan now
 * convicts with `E_MIN_ADA` -- and it would convict every one of them, since
 * the whole point of those suites is items at and beyond the carriage
 * frontiers. The four copies are now this one, so the exact-length algorithm
 * and the funding rule cannot drift apart between them.
 *
 * Funding is at the floor rather than at `FUNDED_OUTPUT_LOVELACE_V1` on
 * purpose: these items are sized in bytes, and a fixed lovelace amount would
 * make the item's own length depend on how wide that amount happens to encode.
 * The floor, `coins_per_utxo_byte * (160 + targetItemBytes)`, is computable up
 * front precisely because the total length is what this builder pins, so
 * funding changes no measured length -- every carriage measurement over these
 * items measures the same number of bytes it did before the wiring.
 */
export const makeMinAdaFundedExactSizeOutputItemV1 = (
  targetItemBytes: number,
): Buffer => {
  const lovelace = minAdaLovelaceV1(
    MIDGARD_COINS_PER_UTXO_BYTE_V1,
    BigInt(targetItemBytes),
  );
  const encodeWithDatum = (datumBytes: number): Buffer =>
    encodeMidgardTxOutput({
      address: TEST_ADDRESS_BYTES,
      value: { lovelace, assets: new Map() },
      datum: {
        kind: "inline",
        cbor: canonicalDatumOfExactLengthV1(datumBytes),
      },
    });
  // The output framing around the datum is a fixed prefix plus one CBOR byte
  // string header, so the item length is the datum length plus an overhead
  // that moves only when that header changes width. Measure the overhead at a
  // reference size, solve for the datum length, and re-solve if the solution
  // crossed a header boundary -- which converges in one further round.
  const referenceDatumBytes = 1_024;
  let datumBytes =
    targetItemBytes -
    (encodeWithDatum(referenceDatumBytes).length - referenceDatumBytes);
  for (let round = 0; round < 4; round += 1) {
    const candidate = encodeWithDatum(datumBytes);
    if (candidate.length === targetItemBytes) {
      return candidate;
    }
    datumBytes += targetItemBytes - candidate.length;
  }
  throw new Error(
    `could not size an exact ${targetItemBytes.toString()}-byte output item`,
  );
};

/**
 * The lovelace a resolved input must carry to fund `outputs` exactly, so a
 * fixture transaction that produces min-Ada-funded outputs still settles to
 * zero at stage five instead of being convicted with `E_VALUE_NOT_PRESERVED`.
 * Fixture transactions here pay no fee, so the sum is exact.
 */
export const fundingLovelaceForOutputsV1 = (
  outputs: readonly Buffer[],
): bigint =>
  outputs.reduce(
    (total, output) => total + decodeMidgardTxOutput(output).value.lovelace,
    0n,
  );

export const makeProtectedScriptOutput = (
  scriptHash: string,
  lovelace: bigint,
): Buffer => {
  const scriptAddress = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
  ).to_address();
  return makeOutput(
    lovelace,
    protectMidgardAddress(Buffer.from(scriptAddress.to_raw_bytes())),
  );
};

export const nativeScriptWitness = (
  nativeScript: MidgardNativeScript,
): MidgardVersionedScript => ({
  language: "NativeCardano",
  scriptBytes: encodeMidgardNativeScript(nativeScript),
  nativeScript,
});

export const plutusV3ScriptWitness = (
  scriptBytes: Buffer,
): MidgardVersionedScript => ({
  language: "PlutusV3",
  scriptBytes,
});

export const hashScriptWitness = hashMidgardVersionedScript;

export const makeRedeemersCbor = (
  items: readonly {
    readonly tag: number;
    readonly index: bigint;
    readonly data?: Uint8Array;
    readonly exUnits?: readonly [bigint, bigint];
  }[],
): Buffer => {
  const emptyData = Buffer.from(Data.to(new Constr(0, [])), "hex");
  // §5.1/§5.3: field 8 is the enveloped list of `enc_8` items. The retired counted
  // scheme spelled this as a bare CBOR array of four-element arrays.
  return encodeMidgardFieldPreimageForFieldV1({
    fieldIndex: 8,
    items: items.map((item) => ({
      purpose: midgardRedeemerPurposeFromTagV1(item.tag),
      index: item.index,
      redeemerCbor: Buffer.from(item.data ?? emptyData),
      executionUnits: {
        memory: item.exUnits?.[0] ?? 1_000_000_000n,
        steps: item.exUnits?.[1] ?? 1_000_000_000n,
      },
    })),
  });
};

/**
 * §5.6: a field-5 preimage from a policy → asset-name → quantity map, sorted into
 * canonical key order at both levels. The retired scheme committed the raw map
 * itself, which is why so many fixtures spelled `encodeCbor(new Map(...))` here.
 */
export const makeMintPreimageCbor = (
  policies: ReadonlyMap<Uint8Array, ReadonlyMap<Uint8Array, bigint>>,
): Buffer =>
  encodeMidgardFieldPreimageForFieldV1({
    fieldIndex: 5,
    items: sortMidgardMintItemsV1(
      [...policies.entries()].map(([policyId, assets]) => ({
        policyId,
        assets: [...assets.entries()].map(([assetName, quantity]) => ({
          assetName,
          quantity,
        })),
      })),
    ),
  });

export const makeNativeTx = (opts: NativeTxOptions = {}): NativeTxFixture => {
  const spendInputs = opts.spendInputs ?? [outRefFromByte(0x11)];
  const referenceInputs = opts.referenceInputs ?? [];
  const outputs = opts.outputs ?? [makeOutput(10n)];
  const requiredSignerItems = opts.requiredSignerItems ?? [];
  const scriptTxWitsPreimageCbor =
    opts.scriptWitnesses === undefined
      ? EMPTY_CBOR_LIST
      : encodeMidgardVersionedScriptListPreimage(opts.scriptWitnesses);
  const redeemerTxWitsPreimageCbor =
    opts.redeemerTxWitsPreimageCbor ?? EMPTY_CBOR_LIST;
  const scriptIntegrityHash =
    opts.scriptLanguages === undefined
      ? EMPTY_NULL_ROOT
      : computeScriptIntegrityHashForLanguages(
          midgardFieldCommitmentV1(redeemerTxWitsPreimageCbor),
          opts.scriptLanguages,
        );

  const body: MidgardNativeTxBodyCanonicalV1 = {
    spendInputsPreimageCbor: encodeByteList(spendInputs),
    referenceInputsPreimageCbor: encodeByteList(referenceInputs),
    outputsPreimageCbor: encodeByteList(outputs),
    fee: opts.fee ?? 0n,
    validityIntervalStart:
      opts.validityIntervalStart ?? MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: opts.validityIntervalEnd ?? MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: encodeByteList(
      opts.requiredObserverItems ?? [],
    ),
    requiredSignersPreimageCbor: encodeByteList(requiredSignerItems),
    mintPreimageCbor: opts.mintPreimageCbor ?? EMPTY_CBOR_LIST,
    scriptIntegrityHash,
    auxiliaryDataHash: opts.auxiliaryDataHash ?? EMPTY_NULL_ROOT,
    networkId: opts.networkId ?? MIDGARD_NATIVE_NETWORK_ID_NONE,
  };

  const version = opts.version ?? MIDGARD_NATIVE_TX_V1_VERSION;
  const bodyCompact = deriveMidgardNativeTxBodyCompactV1(body);
  const bodyHash = computeMidgardNativeTxIdV1({
    version,
    transactionBody: bodyCompact,
    transactionWitnessSetHash: Buffer.alloc(32),
    validity: opts.validity ?? "TxIsValid",
  });
  const signedBodyHash =
    opts.invalidVkeyWitness === true ? Buffer.alloc(32, 0x7f) : bodyHash;
  const addrTxWitsPreimageCbor =
    opts.omitVkeyWitness === true
      ? EMPTY_CBOR_LIST
      : encodeByteList([
          Buffer.from(
            CML.make_vkey_witness(
              CML.TransactionHash.from_raw_bytes(signedBodyHash),
              opts.privateKey ?? TEST_PRIVATE_KEY,
            ).to_cbor_bytes(),
          ),
        ]);

  const witnessSet: MidgardNativeTxWitnessSetCanonicalV1 = {
    addrTxWitsPreimageCbor,
    scriptTxWitsPreimageCbor,
    redeemerTxWitsPreimageCbor,
  };
  const validity = opts.validity ?? "TxIsValid";
  const tx: MidgardNativeTxFullV1 = {
    version,
    validity,
    compact: deriveMidgardNativeTxCompactV1(
      body,
      witnessSet,
      validity,
      version,
    ),
    body,
    witnessSet,
  };
  const txId = computeMidgardNativeTxIdV1(tx);
  const txCbor = encodeMidgardNativeTxCanonicalV1(tx);

  return {
    tx,
    txId,
    txCbor,
  };
};

export const encodeRecomputedNativeTx = (
  tx: MidgardNativeTxFullV1,
): NativeTxFixture => {
  const updated: MidgardNativeTxFullV1 = {
    ...tx,
    compact: deriveMidgardNativeTxCompactV1(
      tx.body,
      tx.witnessSet,
      tx.validity,
    ),
  };
  const txId = computeMidgardNativeTxIdV1(updated);
  const txCbor = encodeMidgardNativeTxCanonicalV1(updated);
  return {
    tx: updated,
    txId,
    txCbor,
  };
};

export const makeQueued = (
  txId: Buffer,
  txCbor: Buffer,
  arrivalSeq = 0n,
): QueuedTx => ({
  txId,
  txCbor,
  arrivalSeq,
  createdAt: new Date(0),
  programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([]),
});

export const ledgerEntry = (outRef: Buffer, output: Buffer): LedgerEntry => ({
  [LedgerColumns.TX_ID]: Buffer.alloc(32, 0),
  [LedgerColumns.OUTREF]: outRef,
  [LedgerColumns.OUTPUT]: output,
  [LedgerColumns.ADDRESS]: TEST_ADDRESS_TEXT,
});

type PhaseBCandidateOptions = Omit<
  NativeTxOptions,
  "spendInputs" | "referenceInputs" | "outputs"
> & {
  readonly arrivalSeq?: bigint;
  readonly spent?: readonly Buffer[];
  readonly referenceInputs?: readonly Buffer[];
  readonly outputLovelace?: bigint;
  readonly outputs?: readonly Buffer[];
  readonly programMaterialSidecarCbor?: Buffer | null;
};

export const makePhaseBCandidate = (
  opts: PhaseBCandidateOptions = {},
): PhaseAValidatedTx => {
  const spent = opts.spent ?? [outRefFromByte(0x11)];
  const referenceInputs = opts.referenceInputs ?? [];
  const outputLovelace = opts.outputLovelace ?? 10n;
  const outputs = opts.outputs ?? [makeOutput(outputLovelace)];
  const fixture = makeNativeTx({
    ...opts,
    spendInputs: spent,
    referenceInputs,
    outputs,
  });
  const submittedTx = decodeMidgardSubmittedTxFromCanonicalCbor(fixture.txCbor);
  return buildPhaseAValidatedTx({
    ledgerTx: submittedTx.ledgerTx,
    txCbor: submittedTx.txCbor,
    programMaterialSidecarCbor: opts.programMaterialSidecarCbor ?? null,
    arrivalSeq: opts.arrivalSeq ?? 0n,
    createdAt: new Date(0),
    redeemerWitnessHash: submittedTx.commitments.redeemerWitnessHash,
  });
};
