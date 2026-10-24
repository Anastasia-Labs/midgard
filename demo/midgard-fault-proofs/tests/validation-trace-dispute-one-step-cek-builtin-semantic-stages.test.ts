/** The validation-trace dispute runner at a CEK builtin step of core group
 * 5, a semantic builtin, for each semantic stage the semantic budget routes
 * a builtin to other than the data scalar stage (which
 * validation-trace-dispute-one-step-cek-builtin-semantic.test.ts drives):
 * planned through the binder to the group's arm chain ending at that stage,
 * and driven through it to the award after a restart mid-chain
 * (support/validation-trace-dispute-one-step-cek-builtin-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import type { ValidationTraceArmHolder } from "../src/validation-trace-dispute/one-step-core-chains.js";
import {
  cekBuiltinDrive,
  flatScript,
} from "./support/validation-trace-dispute-one-step-cek-builtin-drive.js";

/**
 * Each stage, and the failing script whose trace ends in a semantic builtin
 * step routed to it, `(program 1.1.0 (lam ctx [(lam _ (error)) STEP]))` with
 * STEP the builtin's application.
 */
const STAGES: readonly (readonly [ValidationTraceArmHolder, string, string])[] =
  [
    ["semanticPair", "fstPair (1, 2)", "010100232635573a97bda1001021"],
    [
      "semanticListConstruct",
      "mkCons (I 1) []",
      "010100232633574098010101004bd701",
    ],
    ["semanticListSelect", "headList [1, 2]", "01010023263574297ac1028201"],
    [
      "semanticChoose",
      "chooseData (I 3) 1 2 3 4 5",
      "010100232633333357489801010300480092004480192008480281",
    ],
    [
      "semanticDataConstruct",
      "unConstrData (Constr 0 [I 7])",
      "01010023263754980105d8799f07ff0001",
    ],
    [
      "semanticDataMisc",
      "equalsData (I 1) (I 1)",
      "01010023263375e9810101004c0101010001",
    ],
  ];

describe.each(STAGES)(
  "validation-trace dispute runner: driving a thread at a CEK builtin step of core group 5 routed to %s, %s",
  (stage, _step, script) => {
    const drive = cekBuiltinDrive({
      group: 5,
      arm: 31,
      arms: ["semanticRoots", "semanticResult", "semanticBudget", stage],
      script: flatScript(script),
    });

    beforeAll(drive.setUp, 1_800_000);

    it(
      "plans the step through the binder to the group's arm chain, and leaves it unresolved where the chain is not installed",
      drive.plans,
      300_000,
    );

    it("resumes after a restart mid-chain, moving the thread through the rest of the chain to the award, and closes it", async () => {
      drive.restart();
      await drive.awards();
    }, 1_200_000);
  },
);
