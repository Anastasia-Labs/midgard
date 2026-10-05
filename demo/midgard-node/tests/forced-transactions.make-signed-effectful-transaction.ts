import { createHash } from "node:crypto";

import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxBodyCompact,
  deriveMidgardNativeTxCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardForcedTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxCanonical,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetCanonical,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { ForcedTransactionsDB } from "../src/database/index.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";

export const canonicalTransaction = (
  version: bigint = MIDGARD_NATIVE_TX_VERSION,
): MidgardNativeTxCanonical => ({
  version,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: EMPTY_CBOR_LIST,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  },
});

export const encodedTransaction = (version?: bigint): Buffer =>
  encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical(canonicalTransaction(version)),
  );

const TEST_PRIVATE_KEY = CML.PrivateKey.generate_ed25519();

export const TEST_ADDRESS = Buffer.from(
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(TEST_PRIVATE_KEY.to_public().hash()),
  )
    .to_address()
    .to_raw_bytes(),
);

const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  encodeCbor(items.map((item) => Buffer.from(item)));

export const outputReferenceFromHash = (
  transactionId: Buffer,
  outputIndex = 0n,
): Buffer => makeOutRefCbor(transactionId, outputIndex);

export const makeOutput = (
  lovelace: bigint,
  assets: ReadonlyMap<string, ReadonlyMap<string, bigint>> = new Map(),
): Buffer =>
  encodeMidgardTxOutput({
    address: TEST_ADDRESS,
    value: { lovelace, assets },
  });

export const makeInlineDatumOutput = (
  lovelace: bigint,
  datumHex: string,
): Buffer =>
  encodeMidgardTxOutput({
    address: TEST_ADDRESS,
    value: { lovelace, assets: new Map() },
    datum: {
      kind: "inline",
      cbor: Buffer.from(
        aikenSerialisedPlutusDataCbor(Data.to(datumHex)),
        "hex",
      ),
    },
  });

export const makeSignedEffectfulTransaction = (
  spendInput: Buffer,
  output: Buffer,
  {
    referenceInputs = [],
    additionalOutputs = [],
    networkId = MIDGARD_NATIVE_NETWORK_ID_NONE,
  }: {
    readonly referenceInputs?: readonly Buffer[];
    readonly additionalOutputs?: readonly Buffer[];
    readonly networkId?: bigint;
  } = {},
): {
  readonly transaction: MidgardNativeTxFull;
  readonly transactionId: Buffer;
  readonly canonicalCbor: Buffer;
} => {
  const body: MidgardNativeTxBodyCanonical = {
    spendInputsPreimageCbor: encodeByteList([spendInput]),
    referenceInputsPreimageCbor:
      referenceInputs.length === 0
        ? EMPTY_CBOR_LIST
        : encodeByteList(referenceInputs),
    outputsPreimageCbor: encodeByteList([output, ...additionalOutputs]),
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId,
  };
  const bodyHash = computeMidgardNativeTxId({
    version: MIDGARD_NATIVE_TX_VERSION,
    transactionBody: deriveMidgardNativeTxBodyCompact(body),
    transactionWitnessSetHash: Buffer.alloc(32),
    validity: "TxIsValid",
  });
  const witnessSet: MidgardNativeTxWitnessSetCanonical = {
    addrTxWitsPreimageCbor: encodeByteList([
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(bodyHash),
          TEST_PRIVATE_KEY,
        ).to_cbor_bytes(),
      ),
    ]),
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  };
  const transaction: MidgardNativeTxFull = {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    compact: deriveMidgardNativeTxCompact(
      body,
      witnessSet,
      "TxIsValid",
      MIDGARD_NATIVE_TX_VERSION,
    ),
    body,
    witnessSet,
  };
  return {
    transaction,
    transactionId: computeMidgardNativeTxId(transaction),
    canonicalCbor: encodeMidgardForcedTxCanonical(
      materializeMidgardForcedTxFromCanonical(transaction),
    ),
  };
};

export const forcedEntry = async ({
  label,
  transaction,
}: {
  readonly label: number;
  readonly transaction: ReturnType<typeof makeSignedEffectfulTransaction>;
}): Promise<ForcedTransactionsDB.Entry> => {
  // Mirrors ingest: the row is written with the provisional `ForcedTxValid`
  // verdict, so its identity columns carry the SUBMITTED bytes. Adjudication
  // happens at classification, which changes only the verdict in the leaf.
  const encoded = await Effect.runPromise(
    ForcedTransactionsDB.encodeForcedInclusionValueV1({
      nativeTxCbor: transaction.canonicalCbor,
      verdict: "ForcedTxValid",
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    }),
  );
  const txOrderId: SDK.OutputReference = {
    transactionId: Buffer.alloc(32, label).toString("hex"),
    outputIndex: 0n,
  };
  const sidecarCbor = encodeMidgardCekProgramMaterialSidecar([]);
  return {
    [ForcedTransactionsDB.Columns.TX_ORDER_ID]: Buffer.from(
      Data.to(txOrderId, SDK.OutputReference),
      "hex",
    ),
    [ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH]: Buffer.alloc(32, label),
    [ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX]: 0,
    [ForcedTransactionsDB.Columns.ASSET_NAME]: Buffer.from([label]),
    [ForcedTransactionsDB.Columns.RAW_DATUM]: Buffer.from([label]),
    [ForcedTransactionsDB.Columns.TX_ID]: encoded.txId,
    [ForcedTransactionsDB.Columns.TX_COMPACT]: encoded.txCompact,
    [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoded.value,

    [ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID]:
      MIDGARD_CONSENSUS_PROFILE.profileId,
    [ForcedTransactionsDB.Columns.NATIVE_TX_CBOR]: transaction.canonicalCbor,
    [ForcedTransactionsDB.Columns.TRANSACTION_COMMITMENT]:
      encoded.transactionCommitment,
    [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]:
      sidecarCbor,
    [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]:
      createHash("sha256").update(sidecarCbor).digest(),
    [ForcedTransactionsDB.Columns.INCLUSION_TIME]: new Date(
      `2026-07-23T12:00:${label.toString().padStart(2, "0")}.000Z`,
    ),
    [ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH]: null,
    [ForcedTransactionsDB.Columns.STATUS]: ForcedTransactionsDB.Status.Awaiting,
  };
};
