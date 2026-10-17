import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core";

import { keyValuePhasRootWithCount } from "../../src/transition-trace/phas.js";
import {
  buildFixtureTransaction,
  outRefCbor,
} from "../helpers/canonical-block-evidence-fixture.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.build-decoding-block-fixture.js";

/**
 * One accepted forced transaction and one L2 transaction, with header counts
 * forced=1 and l2=1. The honest block steps the forced transaction first;
 * `forcedLast` steps it after the L2 transaction while event_to_step still
 * agrees with the trace, so only the header phase bands are broken.
 */
export const buildForcedAndL2Block = async ({
  forcedLast,
  operatorVkey = "b1".repeat(28),
  startTime = 10n,
}: {
  readonly forcedLast: boolean;
  readonly operatorVkey?: string;
  readonly startTime?: bigint;
}) => {
  const empty = await keyValuePhasRootWithCount([]);
  const [forced, l2] = [0x51, 0x41].map((byte, index) =>
    decodeMidgardNativeTxFullFromCanonicalCbor(
      buildFixtureTransaction({
        spendInputs: [outRefCbor(byte, 0n)],
        fee: BigInt(index + 1),
      }).canonicalCbor,
    ),
  );
  return await buildDecodingBlockFixture({
    operatorVkey,
    startTime,
    priorLedgerRoot: empty.root,
    subject: {
      kind: "forced",
      nativeTx: forced!,
      orderKey: { transactionId: "31".repeat(32), outputIndex: 0n },
      verdict: "ForcedTxValid",
    },
    additionalTransactions: [l2!],
    ...(forcedLast
      ? { orderEvents: <T>(events: readonly T[]) => [...events].reverse() }
      : {}),
  });
};
