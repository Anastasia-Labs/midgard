/** The validation-trace dispute runner at script-sources redeemer
 * normalization steps over failing transactions whose redeemers are the
 * default constructor, an integer and a byte string, each step driven
 * through the redeemer item chain to the award by the executor its item
 * step selects
 * (support/validation-trace-dispute-one-step-script-sources-redeemer-drive.ts). */
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, it } from "vitest";

import type { ValidationTraceScriptSourcesRedeemerHolder } from "../src/validation-trace-dispute/one-step-script-sources-redeemer.js";
import { scriptSourcesRedeemerDrive } from "./support/validation-trace-dispute-one-step-script-sources-redeemer-drive.js";

/** A step, the executor its item step selects, and the failing
 * transaction's redeemer when not the default. */
type Case = Readonly<{
  from: number;
  executor: ValidationTraceScriptSourcesRedeemerHolder;
  redeemer?: Data;
}>;

const cbor = (data: Data) => Buffer.from(Data.to(data), "hex");

describe.each([
  { from: 50, executor: "redeemerItemOpenTailExecutor" },
  { from: 51, executor: "redeemerItemHeadSequenceExecutor" },
  { from: 52, executor: "redeemerItemFinalizeFrameExecutor" },
  { from: 53, executor: "redeemerItemFinishDataExecutor" },
  { from: 51, executor: "redeemerItemHeadScalarExecutor", redeemer: 42n },
  { from: 52, executor: "redeemerItemAdvanceIntegerExecutor", redeemer: 42n },
  { from: 69, executor: "redeemerItemAttachIntegerExecutor", redeemer: 42n },
  { from: 52, executor: "redeemerItemAdvanceBytesExecutor", redeemer: "abcd" },
  { from: 69, executor: "redeemerItemAttachBytesExecutor", redeemer: "abcd" },
] as const satisfies readonly Case[])(
  "validation-trace dispute runner: driving a thread at the script-sources redeemer normalization step after state $from, verified by $executor",
  ({ from, executor, redeemer }: Case) => {
    const drive = scriptSourcesRedeemerDrive({
      from,
      executor,
      ...(redeemer === undefined
        ? {}
        : { failingRedeemerData: cbor(redeemer) }),
    });

    beforeAll(drive.setUp, 1_800_000);

    it(
      "routes the step through the phase resolver to the redeemer normalization semantic resolver",
      drive.routes,
      300_000,
    );

    it(
      "moves the thread through the redeemer item chain to the award, and closes it",
      drive.awards,
      1_200_000,
    );
  },
);
