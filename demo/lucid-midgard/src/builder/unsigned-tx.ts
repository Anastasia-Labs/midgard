import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  encodeCbor,
  type MidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec";

import { compareOutRefs } from "../core/out-ref.js";
import {
  encodeMidgardTxOutput,
  utxoOutRefCbor,
  type AuthoredOutput,
} from "../core/output.js";
import type { Assets } from "../core/assets.js";
import type { MidgardUtxo } from "../core/types.js";
import { stateNetworkId, type BuilderState } from "./context.js";

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
  [...signers]
    .sort((left, right) =>
      Buffer.from(left, "hex").compare(Buffer.from(right, "hex")),
    )
    .map((signer) => Buffer.from(signer, "hex"));

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
): MidgardNativeTxCanonical => ({
  version: MIDGARD_NATIVE_TX_VERSION,
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
