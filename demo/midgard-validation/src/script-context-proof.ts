import type { MidgardTxOutput } from "@al-ft/midgard-core/codec";
import { dataFromCbor } from "@harmoniclabs/plutus-data";
import { Data, fromHex } from "@lucid-evolution/lucid";

import {
  commitMidgardCekDataTree,
  type MidgardCekDataTreeCommitment,
} from "./cek-data-tree.js";
import {
  type ScriptContextAddressEncoding,
  scriptContextTxInInfoData,
  scriptContextTxOutData,
} from "./script-context.js";

export {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "@al-ft/midgard-core";

/**
 * Commits the exact `TxOut` Data subtree used by PlutusV3/MidgardV1 context
 * construction. The source output remains the independently bounded,
 * canonically decoded ledger preimage; this commitment is the semantic bridge
 * that later context-building steps can append without re-revealing the whole
 * transaction.
 */
export const commitMidgardScriptContextTxOut = (
  output: MidgardTxOutput,
  addressEncoding: ScriptContextAddressEncoding,
): MidgardCekDataTreeCommitment => {
  const contextOutput = scriptContextTxOutData(output, addressEncoding);
  const cbor = fromHex(Data.to(contextOutput as never));
  return commitMidgardCekDataTree(dataFromCbor(cbor));
};

export const commitMidgardScriptContextTxInInfo = (
  outRefHex: string,
  output: MidgardTxOutput,
  addressEncoding: ScriptContextAddressEncoding,
): MidgardCekDataTreeCommitment => {
  const contextInput = scriptContextTxInInfoData(
    { outRefHex, output },
    addressEncoding,
  );
  const cbor = fromHex(Data.to(contextInput as never));
  return commitMidgardCekDataTree(dataFromCbor(cbor));
};
