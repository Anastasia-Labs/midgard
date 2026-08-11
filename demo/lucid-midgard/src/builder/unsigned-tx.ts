import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardFieldPreimageV1,
  encodeMidgardHash28ItemV1,
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

/**
 * The §5.1 preimage of a field whose `enc_i` bytes the caller already has:
 * `definite_array_header(N)` followed by one definite byte-string-wrapped item
 * each. Fields 0/1/2/3/4/7 reach §5.1 through here; fields 5/6/8 have per-item
 * interiors and go through their own §5.3 encoders.
 *
 * Routed through `midgard-core`'s one §5.1 encoder rather than a local
 * `encodeCbor` of a Buffer array: both spell the same bytes for well-formed
 * input, but only the §5.1 encoder shares its width rules with the decoder and
 * the field-access door, so the producer and the reader cannot drift.
 */
export const encodeByteListPreimage = (items: readonly Uint8Array[]): Buffer =>
  encodeMidgardFieldPreimageV1(items);

const sortedInputCbors = (inputs: readonly MidgardUtxo[]): Buffer[] =>
  [...inputs].sort(compareOutRefs).map((input) => utxoOutRefCbor(input));

/**
 * §5.3 field 4 items: the raw 28-byte signer hash, no interior CBOR.
 * `encodeMidgardHash28ItemV1` is the §5.3 encoder that says so and asserts the
 * width — the stride-30 arithmetic on the on-chain side depends on it, and
 * `hexToBytes` alone accepts any length.
 */
const sortedRequiredSignerCbors = (signers: readonly string[]): Buffer[] =>
  signers
    .map((signer) =>
      encodeMidgardHash28ItemV1(
        hexToBytes(signer, { fieldName: "requiredSigner" }),
      ),
    )
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
