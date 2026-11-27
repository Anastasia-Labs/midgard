import { UTxO } from "@lucid-evolution/lucid";

import { OutputReference } from "../common.js";

export type TxOrderBuildMetadata = {
  readonly txOrderAddress: string;
  readonly txOrderId: OutputReference;
  readonly authNonceCbor: string;
  readonly txOrderAuthUnit: string;
  readonly nonceInput: Pick<UTxO, "txHash" | "outputIndex">;
  readonly validTo: number;
  readonly inclusionTime: number;
};

export const DEFAULT_TX_ORDER_LOVELACE = 3_000_000n;
