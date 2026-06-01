import {
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardMint,
  type MidgardNativeTxCanonical,
  type MidgardVersionedScript,
  type OutputReference,
} from "@al-ft/midgard-core/codec";
import { hexToBytes } from "@al-ft/midgard-core/hex";

import { compareOutRefs } from "../core/out-ref.js";
import {
  type AuthoredOutput,
  authoredOutputToCore,
} from "../core/output.js";
import type { MidgardUtxo } from "../core/types.js";
import { type BuilderState, stateNetworkId } from "./context.js";

export type ScriptMaterialization = {
  readonly requiredObservers: readonly Buffer[];
  readonly mint: MidgardMint;
  readonly scriptTxWits: readonly MidgardVersionedScript[];
  /** Opaque CBOR redeemer-set blob (Plutus payload). */
  readonly redeemerTxWits: Buffer;
  readonly scriptIntegrityHash: Buffer;
  readonly mintDelta: import("../core/assets.js").Assets;
};

const utxoToOutputReference = (utxo: MidgardUtxo): OutputReference => ({
  txId: Buffer.from(utxo.txHash, "hex"),
  index: utxo.outputIndex,
});

const sortedSpendInputs = (
  inputs: readonly MidgardUtxo[],
): OutputReference[] =>
  [...inputs].sort(compareOutRefs).map(utxoToOutputReference);

const sortedRequiredSignerHashes = (signers: readonly string[]): Buffer[] =>
  signers
    .map((signer) => hexToBytes(signer, { fieldName: "requiredSigner" }))
    .sort(Buffer.compare);

export const buildCanonicalUnsignedTx = (
  state: BuilderState,
  fee: bigint,
  scriptMaterialization: ScriptMaterialization,
): MidgardNativeTxCanonical => ({
  version: MIDGARD_NATIVE_TX_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputs: sortedSpendInputs(state.spendInputs),
    referenceInputs: sortedSpendInputs(state.referenceInputs),
    outputs: state.outputs.map((output: AuthoredOutput) =>
      authoredOutputToCore(output),
    ),
    fee,
    validityIntervalStart:
      state.validityIntervalStart ?? MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: state.validityIntervalEnd ?? MIDGARD_POSIX_TIME_NONE,
    requiredObservers: scriptMaterialization.requiredObservers,
    requiredSigners: sortedRequiredSignerHashes(state.requiredSigners),
    mint: scriptMaterialization.mint,
    scriptIntegrityHash: scriptMaterialization.scriptIntegrityHash,
    auxiliaryDataHash: Buffer.from(EMPTY_NULL_ROOT),
    networkId: stateNetworkId(state),
  },
  witnessSet: {
    addrTxWits: [],
    scriptTxWits: scriptMaterialization.scriptTxWits,
    redeemerTxWits: scriptMaterialization.redeemerTxWits,
  },
});
