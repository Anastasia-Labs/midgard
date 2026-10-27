/** A populated predecessor ledger for the `distinctAssetAccumulationLimit`
 * max-shape subject: the heavy spends' out-refs share a five-digit trie path
 * prefix, and padding entries fill every branch along it, so each heavy
 * spend's membership proof steps through full branches as in a production
 * ledger. The recorded nonces find the same out-refs without searching. */
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";

import { clusterPadding } from "./deep-ledger-trie.js";

/** The trie path prefix the heavy spends share in the populated ledger. */
export const DEEP_CLUSTER = "c3a5e";

/** Each heavy spend's first nonce (the last eight bytes of its transaction
 * id, the rest `42`) whose out-ref path starts with `DEEP_CLUSTER`. */
export const HEAVY_SPEND_NONCES = [
  233_930n,
  1_280_419n,
  1_341_705n,
  1_139_765n,
  679_602n,
  293_458n,
  752_752n,
  17_655n,
  633_848n,
  1_792_666n,
  1_838_608n,
];

/** The first nonce of each padding entry leaving `DEEP_CLUSTER`, by digit. */
const PADDING_NONCES: readonly (readonly bigint[])[] = [
  [37n, 58n, 14n, 13n, 1n, 1n, 16n, 14n, 24n, 20n, 2n, 84n, 9n, 1n, 46n],
  [
    52n,
    17n,
    117n,
    557n,
    181n,
    193n,
    114n,
    22n,
    253n,
    160n,
    157n,
    276n,
    25n,
    106n,
    8n,
  ],
  [
    1_465n,
    3_714n,
    3_370n,
    1_192n,
    575n,
    5_996n,
    6_566n,
    7_728n,
    1_346n,
    6_679n,
    3_495n,
    3_006n,
    6_941n,
    12_893n,
    14_674n,
  ],
  [
    76_251n,
    11_380n,
    48_432n,
    83_883n,
    39_254n,
    68_007n,
    54_013n,
    62_115n,
    26_697n,
    47_360n,
    130_030n,
    41_406n,
    139_822n,
    17_608n,
    57_362n,
  ],
  [
    1_801_825n,
    2_000_388n,
    2_375_989n,
    266_155n,
    187_888n,
    1_701_855n,
    873_896n,
    2_109_861n,
    508_771n,
    96_310n,
    201_997n,
    1_118_234n,
    1_852_050n,
    253_915n,
    898_047n,
  ],
];

/** The heavy spends' out-refs in `DEEP_CLUSTER`, ascending, their ids
 * sorting above every light spend and reference input, and the padding that
 * fills each branch on their shared path. */
export const deepLedger = (firstOutputIndex: number) => ({
  spentOutRefs: HEAVY_SPEND_NONCES.map((nonce, index) => {
    const outRef = encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x42),
      outputIndex: firstOutputIndex + index,
    });
    outRef.writeBigUInt64BE(nonce, 27);
    return outRef;
  }).sort(Buffer.compare),
  padding: clusterPadding(DEEP_CLUSTER, PADDING_NONCES),
});
