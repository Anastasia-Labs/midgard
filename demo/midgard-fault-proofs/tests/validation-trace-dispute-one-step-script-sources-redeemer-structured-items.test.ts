/** The validation-trace dispute runner at script-sources redeemer
 * normalization steps over failing transactions whose redeemers are a list,
 * a map and a large-tag constructor, each step driven through the redeemer
 * item chain to the award by the executor its item step selects; and one
 * step driven over a deployment that also publishes the CEK context
 * resolution, whose redeemer item chain shares its holders
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
  redeemer?: string;
  contexts?: boolean;
}>;

const LIST = Data.to([1n, "ab"]);
/** `{1: 2}`. */
const MAP = "a10102";
/** Constructor 1000 of no fields, under the large-tag encoding. */
const LARGE_CONSTRUCTOR = "d866821903e880";

describe.each([
  { from: 90, executor: "redeemerItemCloseExecutor", redeemer: LIST },
  { from: 91, executor: "redeemerItemFoldListExecutor", redeemer: LIST },
  { from: 51, executor: "redeemerItemHeadMapExecutor", redeemer: MAP },
  { from: 90, executor: "redeemerItemFoldMapExecutor", redeemer: MAP },
  {
    from: 51,
    executor: "redeemerItemHeadLargeConstructorExecutor",
    redeemer: LARGE_CONSTRUCTOR,
  },
  {
    from: 52,
    executor: "redeemerItemAdvanceLargeConstructorExecutor",
    redeemer: LARGE_CONSTRUCTOR,
  },
  {
    from: 70,
    executor: "redeemerItemAdvanceLargeFieldsExecutor",
    redeemer: LARGE_CONSTRUCTOR,
  },
  { from: 53, executor: "redeemerItemFinishDataExecutor", contexts: true },
] as const satisfies readonly Case[])(
  "validation-trace dispute runner: driving a thread at the script-sources redeemer normalization step after state $from, verified by $executor",
  ({ from, executor, redeemer, contexts }: Case) => {
    const drive = scriptSourcesRedeemerDrive({
      from,
      executor,
      ...(redeemer === undefined
        ? {}
        : { failingRedeemerData: Buffer.from(redeemer, "hex") }),
      ...(contexts === undefined ? {} : { contexts }),
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
