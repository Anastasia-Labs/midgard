/**
 * `receivePurposeLanguage` custody refuses a forged successor at every role:
 * from a genuine Init, step-01, each planned transition and the terminal are
 * first built paying another state than the honest one, and the spending
 * role's validator must refuse each before the honest transaction advances
 * the thread. The scenarios together visit every role of the family: an
 * accepted PlutusV3 receive execution beside spent and referenced inputs
 * walks the whole source prefix, the receive route, the family lookup and the
 * claimed redeemer scan; forced transactions the admission screen accepts
 * reach the family entry's contradiction and the receive route's lookup.
 */
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { afterAll, describe, expect, it } from "vitest";

import type { DirectPurposeRole } from "../src/direct-purpose-family/index.js";
import {
  prepareReceivePurposeLanguageProof,
  RECEIVE_PURPOSE_LANGUAGE_FAMILY,
} from "../src/receive-purpose-language/index.js";
import { forgeEveryDirectPurposeRole } from "./support/direct-purpose-forged-successor.js";
import {
  buildMissingRedeemerSubject,
  honestForcedObserverShape,
  missingRedeemerLedgerEntries,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  forcedReceiving,
  makeReceivePurposeLanguageHarness,
  publishReceivePurposeLanguageRoles,
  receivePurposeLanguageVerdict,
} from "./support/receive-purpose-language-emulator.js";

type Scenario = Parameters<typeof setupMissingRedeemerScenario>[0];

/** Every role a scenario of this file refused a forgery at. */
const covered = new Set<DirectPurposeRole>();

/** Forges then advances every role of the scenario's thread and convicts;
 * returns the roles that refused a forgery. */
const forgeEveryRole = async (
  shape: Scenario["shape"],
  source: Scenario["source"],
): Promise<ReadonlySet<DirectPurposeRole>> => {
  if (source.kind === "forced")
    expect(
      screenMidgardForcedTxAdmission(
        buildMissingRedeemerSubject(shape).nativeTx,
      ),
    ).toBeNull();
  const harness = await makeReceivePurposeLanguageHarness();
  const references = await publishReceivePurposeLanguageRoles(harness);
  const scenario = await setupMissingRedeemerScenario({
    harness,
    shape,
    source,
  });
  const refused = await forgeEveryDirectPurposeRole({
    harness,
    family: RECEIVE_PURPOSE_LANGUAGE_FAMILY,
    scenario,
    references,
    prepare: prepareReceivePurposeLanguageProof,
  });
  for (const role of refused) covered.add(role);
  return new Set(refused);
};

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

/** Step-01, the family's entry and the terminal. */
const ENDS = [
  "step01",
  "familyEntry",
  "terminal",
] as const satisfies readonly DirectPurposeRole[];

/** The shared receive route to one ordinal and the family lookup. */
const RECEIVE = [
  "receiveStart",
  "receiveGrammar",
  "receiveOutputs",
  "receiveFinish",
  "receiveDeclaration",
  "sourceLookup",
] as const satisfies readonly DirectPurposeRole[];

const roles = (...groups: readonly (readonly DirectPurposeRole[])[]) =>
  new Set<DirectPurposeRole>(groups.flat());

describe("receive-purpose-language forged successor refusal", () => {
  afterAll(() => {
    expect(covered).toStrictEqual(
      new Set(SDK.RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER),
    );
  });

  it("refuses a forged successor at every role of an accepted PlutusV3 receive execution beside spent and referenced inputs", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [],
        receives: [{ script: plutusScript(3), redeemer: true }],
        spend: missingRedeemerLedgerEntries(0x41, [null]),
        reference: missingRedeemerLedgerEntries(0x51, [null]),
      },
      { kind: "normal" },
    );
    expect(refused).toStrictEqual(
      roles(SOURCE, ENDS, RECEIVE, [
        "claimedRedeemerGrammar",
        "claimedRedeemerScan",
      ]),
    );
  }, 2_400_000);

  it("refuses a forged successor at every role of the forced contradiction route", async () => {
    const refused = await forgeEveryRole(honestForcedObserverShape(5), {
      kind: "forced",
      verdict: receivePurposeLanguageVerdict(2n, 0n),
    });
    expect(refused).toStrictEqual(roles(SOURCE, ENDS));
  }, 2_400_000);

  it.each([
    [
      "an ordinal past the last receive purpose",
      forcedReceiving(plutusScript(4)),
      1n,
    ],
    [
      "a receive purpose with no source",
      forcedReceiving(plutusScript(4), { sourced: false }),
      0n,
    ],
  ] as const)(
    "refuses a forged successor at every role of the forced route to %s",
    async (_label, shape, ordinal) => {
      const refused = await forgeEveryRole(shape, {
        kind: "forced",
        verdict: receivePurposeLanguageVerdict(3n, ordinal),
      });
      expect(refused).toStrictEqual(roles(SOURCE, ENDS, RECEIVE));
    },
    2_400_000,
  );
});
