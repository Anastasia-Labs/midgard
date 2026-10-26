/**
 * `unusedRedeemer` custody refuses a forged successor at every role: from a
 * genuine Init, step-01, each planned transition and the terminal are first
 * built paying another state than the honest one, and the spending role's
 * validator must refuse each before the honest transaction advances the
 * thread. The scenarios together visit every role of the family: an
 * accepted purposeless redeemer beside spent and referenced inputs walks the
 * whole source prefix and the observe claim, and pointers at a key-locked
 * spend, a native mint and a native receive reach the spend, mint and
 * receive claim domains.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import type { DirectPurposeRole } from "../src/direct-purpose-family/index.js";
import { UNUSED_REDEEMER_FAMILY } from "../src/unused-redeemer/contracts.js";
import { prepareUnusedRedeemerProof } from "../src/unused-redeemer/prover.js";
import { forgeEveryDirectPurposeRole } from "./support/direct-purpose-forged-successor.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  honestForcedObserverShape,
  missingRedeemerLedgerEntries,
  nativeScript,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  makeUnusedRedeemerHarness,
  publishUnusedRedeemerRoles,
  unusedRedeemerVerdict,
} from "./support/unused-redeemer-emulator.js";

type Scenario = Parameters<typeof setupMissingRedeemerScenario>[0];

/** Every role a scenario of this file refused a forgery at. */
const covered = new Set<DirectPurposeRole>();

/** Forges then advances every role of the scenario's thread and convicts;
 * returns the roles that refused a forgery. */
const forgeEveryRole = async (
  shape: Scenario["shape"],
  source: Scenario["source"],
): Promise<ReadonlySet<DirectPurposeRole>> => {
  const harness = await makeUnusedRedeemerHarness();
  const references = await publishUnusedRedeemerRoles(harness);
  const scenario = await setupMissingRedeemerScenario({
    harness,
    shape,
    source,
  });
  const refused = await forgeEveryDirectPurposeRole({
    harness,
    family: UNUSED_REDEEMER_FAMILY,
    scenario,
    references,
    prepare: prepareUnusedRedeemerProof,
  });
  for (const role of refused) covered.add(role);
  return new Set(refused);
};

const DATA = Buffer.from(Data.to(0n), "hex");

/** The binding and source prefix of a subject with inputs and witnesses. */
const SOURCE = [
  "bindEvent",
  "bindPreState",
  "sourceStart",
  "sourceResolved",
  "sourceAuthenticate",
  "sourceMap",
  "sourceInputFinish",
  "sourceGrammar",
  "sourceWitnesses",
  "sourceFinish",
] as const satisfies readonly DirectPurposeRole[];

/** The prefix of a subject with no input: no descriptor to authenticate. */
const INPUTLESS_SOURCE = SOURCE.filter((role) => role !== "sourceAuthenticate");

/** Step-01, the walk to the convicting pointer, and the terminal. */
const WALK = [
  "step01",
  "familyEntry",
  "redeemerGrammar",
  "redeemerScan",
  "terminal",
] as const satisfies readonly DirectPurposeRole[];

/** The claim's entry and award. */
const CLAIM = [
  "claimStart",
  "claimAdjudicate",
] as const satisfies readonly DirectPurposeRole[];

const roles = (...groups: readonly (readonly DirectPurposeRole[])[]) =>
  new Set<DirectPurposeRole>(groups.flat());

describe("unused-redeemer forged successor refusal", () => {
  afterAll(() => {
    expect(covered).toStrictEqual(new Set(SDK.UNUSED_REDEEMER_ROLE_ORDER));
  });

  it("refuses a forged successor at every role of an accepted purposeless redeemer beside spent and referenced inputs", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [{ script: plutusScript(1), redeemer: true }],
        paddingRedeemers: [DATA],
        spend: missingRedeemerLedgerEntries(0x41, [null]),
        reference: missingRedeemerLedgerEntries(0x51, [null]),
      },
      { kind: "normal" },
    );
    expect(refused).toStrictEqual(roles(SOURCE, WALK, CLAIM));
  }, 2_400_000);

  it("refuses a forged successor at every role of a Spend pointer at a key-locked input", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [],
        spend: missingRedeemerLedgerEntries(0x42, [null]),
        spendRedeemers: [0n],
      },
      { kind: "normal" },
    );
    expect(refused).toStrictEqual(
      roles(SOURCE, WALK, CLAIM, ["claimSpendStart", "claimSpendScan"]),
    );
  }, 2_400_000);

  it("refuses a forged successor at every role of a Mint pointer at a native policy", async () => {
    const refused = await forgeEveryRole(
      { observers: [], mints: [{ script: nativeScript(3n), redeemer: true }] },
      { kind: "normal" },
    );
    expect(refused).toStrictEqual(
      roles(INPUTLESS_SOURCE, WALK, CLAIM, [
        "claimMintGrammar",
        "claimMintScan",
      ]),
    );
  }, 2_400_000);

  it("refuses a forged successor at every role of a Receive pointer at a native receiver", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [],
        receives: [{ script: nativeScript(4n), redeemer: true }],
      },
      { kind: "normal" },
    );
    expect(refused).toStrictEqual(
      roles(INPUTLESS_SOURCE, WALK, CLAIM, [
        "claimReceiveGrammar",
        "claimReceiveOutputs",
        "claimReceiveFinish",
        "claimReceiveDeclaration",
      ]),
    );
  }, 2_400_000);

  it("refuses a forged successor at every role of an accepted repeated pointer", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [],
        spend: missingRedeemerLedgerEntries(
          0x43,
          [null],
          scriptAddress(plutusScript(9)),
        ),
        witnesses: [plutusScript(9)],
        spendRedeemers: [0n, 0n],
      },
      { kind: "normal" },
    );
    expect(refused).toStrictEqual(roles(SOURCE, WALK));
  }, 2_400_000);

  it("refuses a forged successor at every role of a forced out-of-range pointer", async () => {
    const refused = await forgeEveryRole(
      {
        ...honestForcedObserverShape(5),
        observers: [
          { script: plutusScript(5), witness: false, redeemer: false },
        ],
        reference: [],
      },
      { kind: "forced", verdict: unusedRedeemerVerdict(0n) },
    );
    expect(refused).toStrictEqual(roles(SOURCE, WALK));
  }, 2_400_000);
});
