import type { MidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  DataB,
  DataConstr,
  dataFromCbor,
  DataI,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
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
const ledgerOutputContextData = (
  output: MidgardTxOutput,
  addressEncoding: ScriptContextAddressEncoding,
): DataConstr => {
  const context = scriptContextTxOutData(output, addressEncoding);
  // Lucid's PlutusMap builder sorts keys lexicographically. Ledger output
  // maps have already been authenticated in canonical byte-string order;
  // changing that order changes their semantic Data root.
  const entries = [...output.value.assets].map(
    ([policy, assets]) =>
      new DataPair(
        new DataB(fromHex(policy)),
        new DataMap(
          [...assets].map(
            ([name, quantity]) =>
              new DataPair(new DataB(fromHex(name)), new DataI(quantity)),
          ),
        ),
      ),
  );
  if (output.value.lovelace !== 0n)
    entries.unshift(
      new DataPair(
        new DataB(new Uint8Array()),
        new DataMap([
          new DataPair(
            new DataB(new Uint8Array()),
            new DataI(output.value.lovelace),
          ),
        ]),
      ),
    );
  return new DataConstr(0n, [
    dataFromCbor(fromHex(Data.to(context.fields[0] as never))),
    new DataMap(entries),
    output.datum === undefined
      ? new DataConstr(0n, [])
      : new DataConstr(2n, [dataFromCbor(output.datum.cbor)]),
    dataFromCbor(fromHex(Data.to(context.fields[3] as never))),
  ]);
};

export const commitMidgardScriptContextTxOut = (
  output: MidgardTxOutput,
  addressEncoding: ScriptContextAddressEncoding,
): MidgardCekDataTreeCommitment =>
  commitMidgardCekDataTree(ledgerOutputContextData(output, addressEncoding));

export const commitMidgardScriptContextTxInInfo = (
  outRefHex: string,
  output: MidgardTxOutput,
  addressEncoding: ScriptContextAddressEncoding,
): MidgardCekDataTreeCommitment => {
  const contextInput = scriptContextTxInInfoData(
    { outRefHex, output },
    addressEncoding,
  );
  return commitMidgardCekDataTree(
    new DataConstr(0n, [
      dataFromCbor(fromHex(Data.to(contextInput.fields[0] as never))),
      ledgerOutputContextData(output, addressEncoding),
    ]),
  );
};
