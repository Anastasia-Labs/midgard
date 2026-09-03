import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";

import { deriveL2TransactionSourceCbor } from "../../../src/prepare-double-spend.js";
import { h32 } from "./header-fixtures.js";

export const makeNativeTx = ({
  spendInputCbors,
  fee,
  referenceByte,
  outputByte,
  outputCbor,
  outputCbors,
  witnessByte,
  addrTxWitsPreimageCbor,
  requiredSignerHashes = [],
  scriptTxWitsPreimageCbor = EMPTY_CBOR_LIST,
  redeemerTxWitsPreimageCbor = EMPTY_CBOR_LIST,
  validityIntervalStart = MIDGARD_POSIX_TIME_NONE,
  validityIntervalEnd = MIDGARD_POSIX_TIME_NONE,
}: {
  readonly spendInputCbors: readonly Buffer[];
  readonly fee: bigint;
  readonly referenceByte?: string;
  readonly outputByte?: string;
  readonly outputCbor?: Buffer;
  readonly outputCbors?: readonly Buffer[];
  readonly witnessByte?: string;
  readonly addrTxWitsPreimageCbor?: Buffer;
  readonly requiredSignerHashes?: readonly string[];
  readonly scriptTxWitsPreimageCbor?: Buffer;
  readonly redeemerTxWitsPreimageCbor?: Buffer;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(spendInputCbors),
      // §5.3 fields 0 and 1 share one item form: `82 ‖ 58 20 tx_id(32) ‖ 19
      // index_be16`, a fixed 38 bytes. A bare 32-byte filler is not a
      // reference-input item at all, so fixtures built from one committed to a
      // field the decoders cannot read back.
      referenceInputsPreimageCbor:
        referenceByte === undefined
          ? EMPTY_CBOR_LIST
          : encodeCbor([
              encodeMidgardSpendInputItem({
                txId: Buffer.from(h32(referenceByte), "hex"),
                outputIndex: 0,
              }),
            ]),
      outputsPreimageCbor:
        outputCbors !== undefined
          ? encodeCbor([...outputCbors])
          : outputCbor !== undefined
            ? encodeCbor([outputCbor])
            : outputByte === undefined
              ? EMPTY_CBOR_LIST
              : encodeCbor([Buffer.from(h32(outputByte), "hex")]),
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: encodeCbor(
        requiredSignerHashes.map((hash) => {
          if (!/^[0-9a-f]{56}$/u.test(hash)) {
            throw new Error(
              "required signer hashes must be 28 bytes of lowercase hex",
            );
          }
          return Buffer.from(hash, "hex");
        }),
      ),
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart,
      validityIntervalEnd,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor:
        addrTxWitsPreimageCbor ??
        (witnessByte === undefined
          ? EMPTY_CBOR_LIST
          : encodeCbor([Buffer.from(h32(witnessByte), "hex")])),
      scriptTxWitsPreimageCbor,
      redeemerTxWitsPreimageCbor,
    },
  });

/** Exact transactions-root leaf value for a canonical full native transaction. */
export const l2TransactionSourceCbor = (
  transaction: MidgardNativeTxFull,
): string =>
  deriveL2TransactionSourceCbor(encodeMidgardNativeTxCanonical(transaction));
