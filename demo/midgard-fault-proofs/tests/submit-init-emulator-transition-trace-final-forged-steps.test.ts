import { describe, it } from "vitest";

import { transitionTraceFinalCaseRunner } from "./support/transition-trace-final-cases.js";

// The streamed accepted-transaction replay, honest and with one step forged
// per withdraw-zero script. Each forgery is visible to that script alone: the
// step is refused by its withdrawal redeemer, and the honest continuation from
// the same checkpoint proves the fault.
const run = transitionTraceFinalCaseRunner();
const TIMEOUT = 1_800_000;
const shape = { assetCount: 0, outputCount: 1, spentCount: 1 } as const;

describe("streamed accepted-transaction steps", () => {
  it(
    "convicts a wrong transition root with every streamed step honest",
    () => run(shape),
    TIMEOUT,
  );
  it(
    "refuses an input delete step claiming a post-root its witness does not give",
    () => run({ ...shape, forgeStep: "delete" }),
    TIMEOUT,
  );
  it(
    "refuses an outputs certification step claiming one output more than the field holds",
    () => run({ ...shape, forgeStep: "outputs" }),
    TIMEOUT,
  );
  it(
    "refuses an output step claiming a post-root its insert does not give",
    () => run({ ...shape, forgeStep: "outputStep" }),
    TIMEOUT,
  );
  it(
    "refuses an output step inserting a descriptor its output does not derive",
    () => run({ ...shape, forgeStep: "descriptor" }),
    TIMEOUT,
  );
  it(
    "refuses an output step whose summaries and descriptor are forged together",
    () => run({ ...shape, forgeStep: "summary" }),
    TIMEOUT,
  );
});
