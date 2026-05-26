import {
  EMPTY_PREIMAGE_LIST,
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  encodeMidgardBytesListPreimage,
  encodeMidgardHash28ListPreimage,
  encodeMidgardOutputReferenceListPreimage,
  type MidgardNativeTxCanonical,
  type MidgardOutputReference,
} from "@al-ft/midgard-core/codec";

import { compareOutRefs } from "../core/out-ref.js";
import {
  encodeMidgardTxOutput,
  type AuthoredOutput,
} from "../core/output.js";
import type { Assets } from "../core/assets.js";
import type { MidgardUtxo } from "../core/types.js";
import { stateNetworkId, type BuilderState } from "./context.js";

export type ScriptMaterialization = {
  readonly requiredObserversPreimage: Buffer;
  readonly mintPreimage: Buffer;
  readonly scriptTxWitsPreimage: Buffer;
  readonly redeemerTxWitsPreimage: Buffer;
  readonly scriptIntegrityHash: Buffer;
  readonly mintDelta: Assets;
};

/** Re-export for callers that build outputs/scripts preimages from CBOR bytes. */
export const encodeByteListPreimage = encodeMidgardBytesListPreimage;

const sortedInputOutRefs = (
  inputs: readonly MidgardUtxo[],
): MidgardOutputReference[] =>
  [...inputs].sort(compareOutRefs).map((input) => ({
    txId: Buffer.from(input.txHash, "hex"),
    index: input.outputIndex,
  }));

const sortedRequiredSignerHashes = (signers: readonly string[]): Buffer[] =>
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
    spendInputsPreimage: encodeMidgardOutputReferenceListPreimage(
      sortedInputOutRefs(state.spendInputs),
    ),
    referenceInputsPreimage: encodeMidgardOutputReferenceListPreimage(
      sortedInputOutRefs(state.referenceInputs),
    ),
    outputsPreimage: encodeMidgardBytesListPreimage(outputCbors(state.outputs)),
    fee,
    validityIntervalStart:
      state.validityIntervalStart ?? MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: state.validityIntervalEnd ?? MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimage:
      scriptMaterialization.requiredObserversPreimage,
    requiredSignersPreimage: encodeMidgardHash28ListPreimage(
      sortedRequiredSignerHashes(state.requiredSigners),
      "required_signers",
    ),
    mintPreimage: scriptMaterialization.mintPreimage,
    scriptIntegrityHash: scriptMaterialization.scriptIntegrityHash,
    auxiliaryDataHash: Buffer.from(EMPTY_NULL_ROOT),
    networkId: stateNetworkId(state),
  },
  witnessSet: {
    addrTxWitsPreimage: Buffer.from(EMPTY_PREIMAGE_LIST),
    scriptTxWitsPreimage: scriptMaterialization.scriptTxWitsPreimage,
    redeemerTxWitsPreimage:
      scriptMaterialization.redeemerTxWitsPreimage,
  },
});
