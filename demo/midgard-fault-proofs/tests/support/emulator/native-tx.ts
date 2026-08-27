import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";

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
  scriptTxWitsPreimageCbor,
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
  readonly scriptTxWitsPreimageCbor?: Buffer;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
}): MidgardNativeTxFullV1 =>
  materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(spendInputCbors),
      referenceInputsPreimageCbor:
        referenceByte === undefined
          ? EMPTY_CBOR_LIST
          : encodeCbor([Buffer.from(h32(referenceByte), "hex")]),
      outputsPreimageCbor:
        outputCbors !== undefined
          ? encodeCbor([...outputCbors])
          : outputCbor !== undefined
            ? encodeCbor([outputCbor])
            : outputByte === undefined
              ? EMPTY_CBOR_LIST
              : encodeCbor([Buffer.from(h32(outputByte), "hex")]),
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
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
      scriptTxWitsPreimageCbor: scriptTxWitsPreimageCbor ?? EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
