import type { MidgardTxOutput } from "@al-ft/midgard-core/codec";
import { dataFromCbor } from "@harmoniclabs/plutus-data";
import { Data, fromHex } from "@lucid-evolution/lucid";

import {
  commitMidgardCekDataTreeV1,
  type MidgardCekDataTreeCommitmentV1,
} from "./cek-data-tree.js";
import {
  type ScriptContextAddressEncoding,
  scriptContextTxInInfoData,
  scriptContextTxOutData,
} from "./script-context.js";

export {
  emptyMidgardCekDataListSummaryV1,
  emptyMidgardCekDataPairSummaryV1,
  type MidgardCekDataSequenceSummaryV1,
  type MidgardCekDataSummaryV1,
  prependMidgardCekDataListSummaryV1,
  prependMidgardCekDataPairSummaryV1,
  summarizeMidgardCekListDataV1,
  summarizeMidgardCekMapDataV1,
  summarizeMidgardCekSmallConstrDataV1,
} from "@al-ft/midgard-core";

/**
 * Commits the exact `TxOut` Data subtree used by PlutusV3/MidgardV1 context
 * construction. The source output remains the independently bounded,
 * canonically decoded ledger preimage; this commitment is the semantic bridge
 * that later context-building steps can append without re-revealing the whole
 * transaction.
 */
export const commitMidgardScriptContextTxOutV1 = (
  output: MidgardTxOutput,
  addressEncoding: ScriptContextAddressEncoding,
): MidgardCekDataTreeCommitmentV1 => {
  const contextOutput = scriptContextTxOutData(output, addressEncoding);
  const cbor = fromHex(Data.to(contextOutput as never));
  return commitMidgardCekDataTreeV1(dataFromCbor(cbor));
};

export const commitMidgardScriptContextTxInInfoV1 = (
  outRefHex: string,
  output: MidgardTxOutput,
  addressEncoding: ScriptContextAddressEncoding,
): MidgardCekDataTreeCommitmentV1 => {
  const contextInput = scriptContextTxInInfoData(
    { outRefHex, output },
    addressEncoding,
  );
  const cbor = fromHex(Data.to(contextInput as never));
  return commitMidgardCekDataTreeV1(dataFromCbor(cbor));
};
