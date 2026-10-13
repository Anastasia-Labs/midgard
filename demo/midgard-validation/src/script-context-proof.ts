import type { MidgardTxOutput } from "@al-ft/midgard-core/codec";

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
 * transaction. The context and the ledger output maps both order a Value's
 * policies and asset names as plain bytes.
 */
export const commitMidgardScriptContextTxOut = (
  output: MidgardTxOutput,
  addressEncoding: ScriptContextAddressEncoding,
): MidgardCekDataTreeCommitment =>
  commitMidgardCekDataTree(scriptContextTxOutData(output, addressEncoding));

export const commitMidgardScriptContextTxInInfo = (
  outRefHex: string,
  output: MidgardTxOutput,
  addressEncoding: ScriptContextAddressEncoding,
): MidgardCekDataTreeCommitment =>
  commitMidgardCekDataTree(
    scriptContextTxInInfoData({ outRefHex, output }, addressEncoding),
  );
