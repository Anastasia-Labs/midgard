import { describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { transitionTraceFinalCaseRunner } from "./support/transition-trace-final-cases.js";

// The streamed accepted-transaction replay, honest and with one step forged
// per withdraw-zero script. Each forgery is visible to that script alone: the
// step is refused by its withdrawal redeemer, and the honest continuation from
// the same checkpoint proves the fault.
const run = transitionTraceFinalCaseRunner();
const TIMEOUT = 1_800_000;
const shape = { assetCount: 0, outputCount: 1, spentCount: 1 } as const;
// The longest output a phase-11 step replays whole, and a staged output of
// the same length whose one asset is what stages it.
const longestWhole = `a200583900${"ab".repeat(56)}01821bffffffffffffffffa0`;
const stagedAtBound = `a200581d60${"aa".repeat(28)}01821818a1581c${"44".repeat(28)}a1410118a0`;

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
    "refuses an input delete step deleting another spend input than the walked one",
    () =>
      run({
        ...shape,
        spentCount: 2,
        forgeStep: "deleteKey",
        forgedStepRefusal: (submit) =>
          expectOnchainRefusal(submit, {
            refusedBy:
              "fraud_proofs/transition_trace/accepted_transaction_yields",
            check: /decode_midgard_tx_input_cbor\(item\)/u,
          }),
      }),
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
    "refuses an output step inserting its output under another output's reference",
    () =>
      run({
        ...shape,
        forgeStep: "insertKey",
        forgedStepRefusal: (submit) =>
          expectOnchainRefusal(submit, {
            refusedBy:
              "fraud_proofs/transition_trace/accepted_transaction_yields",
            check: /MidgardTxInput \{ tx_id: state\.tx_id/u,
          }),
      }),
    TIMEOUT,
  );
  it(
    "refuses an output step inserting another value than the verified descriptor",
    () =>
      run({
        ...shape,
        forgeStep: "insertValue",
        forgedStepRefusal: (submit) =>
          expectOnchainRefusal(submit, {
            refusedBy:
              "fraud_proofs/transition_trace/accepted_transaction_yields",
            check: /witness\.value == descriptor/u,
          }),
      }),
    TIMEOUT,
  );
  it(
    "refuses an output step resuming the outputs walk at the next output",
    () =>
      run({
        ...shape,
        outputCount: 2,
        forgeStep: "walk",
        forgedStepRefusal: (submit) =>
          expectOnchainRefusal(submit, {
            refusedBy:
              "fraud_proofs/transition_trace/accepted_transaction_yields",
            check: /field_walk_checkpoint_hash\(checkpoint\) == committed/u,
          }),
      }),
    TIMEOUT,
  );
  it(
    "refuses an output step staging the longest output it replays whole",
    () =>
      run({
        ...shape,
        outputCbor: longestWhole,
        forgeStep: "wholeAsStaged",
        forgedStepRefusal: (submit) =>
          expectOnchainRefusal(submit, {
            refusedBy:
              "fraud_proofs/transition_trace/accepted_transaction_yields",
            check: /pairs\.has_key\(tx\.withdrawals, Script\(verifier\)\)/u,
          }),
      }),
    TIMEOUT,
  );
  it(
    "refuses an output step replaying whole an output of the same length its asset stages",
    () =>
      run({
        ...shape,
        outputCbor: stagedAtBound,
        forgeStep: "stagedAsWhole",
        forgedStepRefusal: (submit) =>
          expectOnchainRefusal(submit, {
            refusedBy:
              "fraud_proofs/transition_trace/accepted_transaction_yields",
            check: /Validator returned false/u,
          }),
      }),
    TIMEOUT,
  );
  it(
    "refuses a staged output's insert under another output's reference",
    () =>
      run({
        ...shape,
        assetCount: 1,
        forgeStep: "stagedInsert",
        forgedStepRefusal: (submit) =>
          expectOnchainRefusal(submit, {
            refusedBy:
              "fraud_proofs/transition_trace/accepted_transaction_yields",
            check: /MidgardTxInput \{ tx_id: state\.tx_id/u,
          }),
      }),
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
