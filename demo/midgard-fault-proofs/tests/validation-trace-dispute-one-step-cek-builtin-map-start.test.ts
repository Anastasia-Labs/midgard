/** The validation-trace dispute runner at a CEK builtin step of core group 6,
 * a map conversion's start (unMapData), driven through the binder and the
 * group's arm chain to the award
 * (support/validation-trace-dispute-one-step-cek-builtin-drive.ts): over an
 * empty map, where a hop whose successor's bound is not the one the chain
 * writes is refused, and over a map with entries whose keys repeat. */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  cekBuiltinDrive,
  flatScript,
} from "./support/validation-trace-dispute-one-step-cek-builtin-drive.js";

const ARMS = ["mapStartRoots", "mapStartBudget", "mapStartNodes"] as const;

const drive = cekBuiltinDrive({ group: 6, arm: 32, arms: ARMS });

describe("validation-trace dispute runner: driving a thread at a CEK builtin step of core group 6", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "plans the step through the binder to the group's arm chain, and leaves it unresolved where the chain is not installed",
    drive.plans,
    300_000,
  );

  it("refuses a hop whose successor keeps the hop's own progress, at the map start budget", async () => {
    const refused = await drive.unadvanced();
    await expectOnchainRefusal(refused.build, {
      refusedBy: "fraud_proofs/validation_trace/cek_core_map_start_budget",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it("refuses a hop whose successor carries the facts of the hop before it, at the map start budget", async () => {
    const refused = await drive.otherFacts();
    await expectOnchainRefusal(refused.build, {
      refusedBy: "fraud_proofs/validation_trace/cek_core_map_start_budget",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it("refuses a hop that pays its successor to the settle rather than the chain's next holder, at the map start budget", async () => {
    const refused = await drive.otherNext();
    await expectOnchainRefusal(refused.build, {
      refusedBy: "fraud_proofs/validation_trace/cek_core_map_start_budget",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it("resumes after a restart mid-chain, moving the thread through the rest of the chain to the award, and closes it", async () => {
    drive.restart();
    await drive.awards();
  }, 1_200_000);

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});

/**
 * `(program 1.1.0 (lam ctx [(lam _ (error)) [(builtin unMapData) (Map
 * [(I 1, I 2), (I 1, B #), (B #07..07, List [])])]]))`, 40 bytes of 07: a
 * map whose first two entries share a key.
 */
const REPEATED_KEYS = flatScript(
  "01010023263756980131bf0102014058280707070707070707070707070707070707070707070707070707070707070707070707070707070780ff0001",
);

const repeated = cekBuiltinDrive({
  group: 6,
  arm: 32,
  arms: ARMS,
  script: REPEATED_KEYS,
});

describe("validation-trace dispute runner: driving a thread at a CEK builtin step of core group 6 over a map whose keys repeat", () => {
  beforeAll(repeated.setUp, 1_800_000);

  it(
    "plans the step through the binder to the group's arm chain, and leaves it unresolved where the chain is not installed",
    repeated.plans,
    300_000,
  );

  it("resumes after a restart mid-chain, moving the thread through the rest of the chain to the award, and closes it", async () => {
    repeated.restart();
    await repeated.awards();
  }, 1_200_000);
});
