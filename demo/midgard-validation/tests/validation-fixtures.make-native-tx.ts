import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  computeScriptIntegrityHashForLanguages,
  deriveMidgardNativeTxBodyCompact,
  deriveMidgardNativeTxCompact,
  EMPTY_NULL_ROOT,
  encodeMidgardFieldPreimageForField,
  encodeMidgardNativeTxCanonical,
  encodeMidgardVersionedScriptListPreimage,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardFieldCommitment,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetCanonical,
  sortMidgardMintItems,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";

import { LedgerColumns, type LedgerEntry } from "../src/ledger.js";
import { decodeMidgardSubmittedTxFromCanonicalCbor } from "../src/ledger-tx/codec.js";
import type { PhaseAValidatedTx, QueuedTx } from "../src/types.js";
import { buildPhaseAValidatedTx } from "../src/validation-candidate.js";
import {
  EMPTY_CBOR_LIST,
  encodeByteList,
  FUNDED_OUTPUT_LOVELACE,
  makeOutput,
  type NativeTxFixture,
  type NativeTxOptions,
  outRefFromByte,
  TEST_ADDRESS_TEXT,
  TEST_PRIVATE_KEY,
} from "./validation-fixtures.make-min-ada-funded-exact-size-output-item.js";

/**
 * §5.6: a field-5 preimage from a policy → asset-name → quantity map, sorted into
 * canonical key order at both levels. The retired scheme committed the raw map
 * itself, which is why so many fixtures spelled `encodeCbor(new Map(...))` here.
 */
export const makeMintPreimageCbor = (
  policies: ReadonlyMap<Uint8Array, ReadonlyMap<Uint8Array, bigint>>,
): Buffer =>
  encodeMidgardFieldPreimageForField({
    fieldIndex: 5,
    items: sortMidgardMintItems(
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
          midgardFieldCommitment(redeemerTxWitsPreimageCbor),
          opts.scriptLanguages,
        );

  const body: MidgardNativeTxBodyCanonical = {
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

  const version = opts.version ?? MIDGARD_NATIVE_TX_VERSION;
  const bodyCompact = deriveMidgardNativeTxBodyCompact(body);
  const bodyHash = computeMidgardNativeTxId({
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

  const witnessSet: MidgardNativeTxWitnessSetCanonical = {
    addrTxWitsPreimageCbor,
    scriptTxWitsPreimageCbor,
    redeemerTxWitsPreimageCbor,
  };
  const validity = opts.validity ?? "TxIsValid";
  const tx: MidgardNativeTxFull = {
    version,
    validity,
    compact: deriveMidgardNativeTxCompact(body, witnessSet, validity, version),
    body,
    witnessSet,
  };
  const txId = computeMidgardNativeTxId(tx);
  const txCbor = encodeMidgardNativeTxCanonical(tx);

  return {
    tx,
    txId,
    txCbor,
  };
};

export const encodeRecomputedNativeTx = (
  tx: MidgardNativeTxFull,
): NativeTxFixture => {
  const updated: MidgardNativeTxFull = {
    ...tx,
    compact: deriveMidgardNativeTxCompact(tx.body, tx.witnessSet, tx.validity),
  };
  const txId = computeMidgardNativeTxId(updated);
  const txCbor = encodeMidgardNativeTxCanonical(updated);
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
  programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
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
  const outputLovelace = opts.outputLovelace ?? FUNDED_OUTPUT_LOVELACE;
  const outputs = opts.outputs ?? [makeOutput(outputLovelace)];
  const fixture = makeNativeTx({
    ...opts,
    spendInputs: spent,
    referenceInputs,
    outputs,
  });
  const submittedTx = decodeMidgardSubmittedTxFromCanonicalCbor(fixture.txCbor);
  return buildPhaseAValidatedTx({
    sourceKind: "normal",
    ledgerTx: submittedTx.ledgerTx,
    expectedNetworkId: 0n,
    txCbor: submittedTx.txCbor,
    programMaterialSidecarCbor: opts.programMaterialSidecarCbor ?? null,
    arrivalSeq: opts.arrivalSeq ?? 0n,
    createdAt: new Date(0),
    redeemerWitnessHash: submittedTx.commitments.redeemerWitnessHash,
  });
};
