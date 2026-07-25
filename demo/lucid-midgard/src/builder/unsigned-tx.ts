import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core/codec";
import { hexToBytes } from "@al-ft/midgard-core/hex";

import type { Assets } from "../core/assets.js";
import { compareOutRefs } from "../core/out-ref.js";
import {
  type AuthoredOutput,
  encodeMidgardTxOutput,
  utxoOutRefCbor,
} from "../core/output.js";
import type { MidgardUtxo } from "../core/types.js";
import { type BuilderState, stateNetworkId } from "./context.js";

export type ScriptMaterialization = {
  readonly requiredObserversPreimageCbor: Buffer;
  readonly mintPreimageCbor: Buffer;
  readonly scriptTxWitsPreimageCbor: Buffer;
  readonly redeemerTxWitsPreimageCbor: Buffer;
  readonly scriptIntegrityHash: Buffer;
  readonly mintDelta: Assets;
};

export const encodeByteListPreimage = (items: readonly Uint8Array[]): Buffer =>
  encodeCbor(items.map((item) => Buffer.from(item)));

const sortedInputCbors = (inputs: readonly MidgardUtxo[]): Buffer[] =>
  [...inputs].sort(compareOutRefs).map((input) => utxoOutRefCbor(input));

const sortedRequiredSignerCbors = (signers: readonly string[]): Buffer[] =>
  signers
    .map((signer) => hexToBytes(signer, { fieldName: "requiredSigner" }))
    .sort(Buffer.compare);

const outputCbors = (outputs: readonly AuthoredOutput[]): Buffer[] =>
  outputs.map((output) =>
    encodeMidgardTxOutput(output.address, output.assets, {
      kind: output.kind,
      datum: output.datum,
      scriptRef: output.scriptRef,
    }),
  );

export const buildCanonicalUnsignedTx = (
  state: BuilderState,
  fee: bigint,
  scriptMaterialization: ScriptMaterialization,
  nativeTxVersion: bigint = MIDGARD_NATIVE_TX_V1_VERSION,
): MidgardNativeTxCanonicalV1 => ({
  version: nativeTxVersion,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: encodeByteListPreimage(
      sortedInputCbors(state.spendInputs),
    ),
    referenceInputsPreimageCbor: encodeByteListPreimage(
      sortedInputCbors(state.referenceInputs),
    ),
    outputsPreimageCbor: encodeByteListPreimage(outputCbors(state.outputs)),
    fee,
    validityIntervalStart:
      state.validityIntervalStart ?? MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: state.validityIntervalEnd ?? MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor:
      scriptMaterialization.requiredObserversPreimageCbor,
    requiredSignersPreimageCbor: encodeByteListPreimage(
      sortedRequiredSignerCbors(state.requiredSigners),
    ),
    mintPreimageCbor: scriptMaterialization.mintPreimageCbor,
    scriptIntegrityHash: scriptMaterialization.scriptIntegrityHash,
    auxiliaryDataHash: Buffer.from(EMPTY_NULL_ROOT),
    networkId: stateNetworkId(state),
  },
  witnessSet: {
    addrTxWitsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    scriptTxWitsPreimageCbor: scriptMaterialization.scriptTxWitsPreimageCbor,
    redeemerTxWitsPreimageCbor:
      scriptMaterialization.redeemerTxWitsPreimageCbor,
  },
});
