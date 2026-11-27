import { readFile } from "node:fs/promises";

import { parse } from "dotenv";

/**
 * The L2 fee schedule a Preprod stack's node commits into every header it
 * builds, and so the schedule a drill's retained predecessor carries. A
 * stack's node `.env` keeps the operator settings of the node's
 * `.env.example` (docs/PREPROD_STACK.md), `MIN_FEE_A` and `MIN_FEE_B` among
 * them.
 */
export const stackNodeFeeSchedule = async (): Promise<{
  minFeeA: bigint;
  minFeeB: bigint;
}> => {
  const env = parse(
    await readFile(
      new URL("../../../midgard-node/.env.example", import.meta.url),
    ),
  );
  if (env.MIN_FEE_A === undefined || env.MIN_FEE_B === undefined)
    throw new Error("The node environment example sets no L2 fee schedule");
  return { minFeeA: BigInt(env.MIN_FEE_A), minFeeB: BigInt(env.MIN_FEE_B) };
};
