import { encodeMidgardCekProgramMaterialSidecarV1 } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  computeScriptIntegrityHashForLanguages,
  deriveMidgardNativeFieldCollectionV1,
  deriveMidgardNativeTxBodyCompactV1,
  deriveMidgardNativeTxCompactV1,
  EMPTY_NULL_ROOT,
  encodeMidgardAddressText,
  encodeMidgardNativeScript,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeScript,
  type MidgardNativeTxBodyCanonicalV1,
  type MidgardNativeTxFullV1,
  type MidgardNativeTxWitnessSetCanonicalV1,
  type MidgardTxOutput,
  type MidgardTxValidity,
  type MidgardVersionedScript,
  protectMidgardAddress,
  type ScriptLanguageName,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { CML, Constr, Data } from "@lucid-evolution/lucid";

import { LedgerColumns, type LedgerEntry } from "../src/ledger.js";
import { decodeMidgardSubmittedTxFromCanonicalCbor } from "../src/ledger-tx/codec.js";
import type { PhaseAValidatedTx, QueuedTx } from "../src/types.js";
import { buildPhaseAValidatedTx } from "../src/validation-candidate.js";

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

export const outRefFromByte = (byte: number, index = 0n): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(Buffer.alloc(32, byte).toString("hex")),
      index,
    ).to_cbor_bytes(),
  );

export const outRefFromTxId = (txId: Buffer, index = 0n): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(txId),
      index,
    ).to_cbor_bytes(),
  );

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
  return encodeCbor(
    items.map((item) => [
      item.tag,
      item.index,
      Buffer.from(item.data ?? emptyData),
      [
        item.exUnits?.[0] ?? 1_000_000_000n,
        item.exUnits?.[1] ?? 1_000_000_000n,
      ],
    ]),
  );
};

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
          deriveMidgardNativeFieldCollectionV1({
            fieldIndex: 8,
            preimageCbor: redeemerTxWitsPreimageCbor,
          }).commitment,
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
    compact: deriveMidgardNativeTxCompactV1(tx.body, tx.witnessSet, tx.validity),
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
  programMaterialSidecarCbor:
    encodeMidgardCekProgramMaterialSidecarV1([]),
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
    programMaterialSidecarCbor:
      opts.programMaterialSidecarCbor ?? null,
    arrivalSeq: opts.arrivalSeq ?? 0n,
    createdAt: new Date(0),
    redeemerWitnessHash: submittedTx.commitments.redeemerWitnessHash,
  });
};
