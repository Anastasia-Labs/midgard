/**
 * `missingScriptSource` custody refuses a forged successor at every role:
 * from a genuine Init, step-01, each planned transition and the terminal are
 * first built paying another state than the honest one, and the spending
 * role's validator must refuse each before the honest transaction advances
 * the thread. The scenarios together visit every role of the family: an
 * accepted sourceless observer beside spent and referenced inputs walks the
 * whole source prefix; a sourceless spend, mint and receive walk each
 * observed purpose domain; rejections naming a sourced spend, mint and
 * receive reach each claim domain; and a RedeemerMissing behind a missing
 * source reaches the claimed-redeemer scan.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { afterAll, describe, expect, it } from "vitest";

import type { DirectPurposeRole } from "../src/direct-purpose-family/index.js";
import { MISSING_SCRIPT_SOURCE_FAMILY } from "../src/missing-script-source/contracts.js";
import { prepareMissingScriptSourceProof } from "../src/missing-script-source/prover.js";
import { forgeEveryDirectPurposeRole } from "./support/direct-purpose-forged-successor.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  honestForcedObserverShape,
  missingRedeemerLedgerEntries,
  plutusScript,
  redeemerMissingVerdict,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  makeMissingScriptSourceHarness,
  publishMissingScriptSourceRoles,
  scriptSourceMissingVerdict,
} from "./support/missing-script-source-emulator.js";

type Scenario = Parameters<typeof setupMissingRedeemerScenario>[0];

/** Every role a scenario of this file refused a forgery at. */
const covered = new Set<DirectPurposeRole>();

/** Forges then advances every role of the scenario's thread and convicts;
 * returns the roles that refused a forgery. */
const forgeEveryRole = async (
  shape: Scenario["shape"],
  source: Scenario["source"],
): Promise<ReadonlySet<DirectPurposeRole>> => {
  const harness = await makeMissingScriptSourceHarness();
  const references = await publishMissingScriptSourceRoles(harness);
  const scenario = await setupMissingRedeemerScenario({
    harness,
    shape,
    source,
  });
  const refused = await forgeEveryDirectPurposeRole({
    harness,
    family: MISSING_SCRIPT_SOURCE_FAMILY,
    scenario,
    references,
    prepare: prepareMissingScriptSourceProof,
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

/** The prefix of a subject with no input and no witness: nothing to
 * authenticate or map. */
const BARE_SOURCE = SOURCE.filter(
  (role) => role !== "sourceAuthenticate" && role !== "sourceMap",
);

/** Step-01, the family's entry, the claim's entry and award, and the
 * terminal. */
const ENDS = [
  "step01",
  "familyEntry",
  "claimStart",
  "claimAdjudicate",
  "terminal",
] as const satisfies readonly DirectPurposeRole[];

/** The observed purpose's selection, source lookup and decision. */
const OBSERVED = [
  "purposeSelect",
  "sourceLookup",
  "observedDecision",
] as const satisfies readonly DirectPurposeRole[];

const roles = (...groups: readonly (readonly DirectPurposeRole[])[]) =>
  new Set<DirectPurposeRole>(groups.flat());

const signed = honestForcedObserverShape(5);

/** A forced subject spending its signed-for input, beside `purposes`. */
const forcedWith = (
  purposes: Partial<Scenario["shape"]>,
): Scenario["shape"] => ({ ...signed, observers: [], ...purposes });

describe("missing-script-source forged successor refusal", () => {
  afterAll(() => {
    expect(covered).toStrictEqual(
      new Set(SDK.MISSING_SCRIPT_SOURCE_ROLE_ORDER),
    );
  });

  it("refuses a forged successor at every role of an accepted sourceless observer beside spent and referenced inputs", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [
          { script: plutusScript(1), witness: false, redeemer: true },
          { script: plutusScript(2), redeemer: true },
        ],
        spend: missingRedeemerLedgerEntries(0x41, [null]),
        reference: missingRedeemerLedgerEntries(0x51, [null]),
      },
      { kind: "normal" },
    );
    expect(refused).toStrictEqual(roles(SOURCE, ENDS, OBSERVED));
  }, 2_400_000);

  it("refuses a forged successor at every role of an admitted sourceless spend", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [],
        spend: missingRedeemerLedgerEntries(
          0x42,
          [null],
          scriptAddress(plutusScript(9)),
        ),
      },
      { kind: "forced", verdict: "ForcedTxValid" },
    );
    expect(refused).toStrictEqual(
      roles(SOURCE, ENDS, OBSERVED, ["spendSelect", "spendScan"]),
    );
  }, 2_400_000);

  it("refuses a forged successor at every role of an accepted sourceless mint", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [],
        mints: [{ script: plutusScript(3), witness: false, redeemer: true }],
      },
      { kind: "normal" },
    );
    expect(refused).toStrictEqual(
      roles(BARE_SOURCE, ENDS, OBSERVED, ["mintGrammar", "mintScan"]),
    );
  }, 2_400_000);

  it("refuses a forged successor at every role of an accepted sourceless receive", async () => {
    const refused = await forgeEveryRole(
      {
        observers: [],
        receives: [{ script: plutusScript(4), witness: false, redeemer: true }],
      },
      { kind: "normal" },
    );
    expect(refused).toStrictEqual(
      roles(BARE_SOURCE, ENDS, OBSERVED, [
        "receiveStart",
        "receiveGrammar",
        "receiveOutputs",
        "receiveFinish",
        "receiveDeclaration",
      ]),
    );
  }, 2_400_000);

  it.each([
    [
      "spend",
      forcedWith({
        spend: [
          ...(signed.spend ?? []),
          ...missingRedeemerLedgerEntries(
            0x43,
            [null],
            scriptAddress(plutusScript(9)),
          ),
        ],
        witnesses: [plutusScript(9)],
      }),
      scriptSourceMissingVerdict(0n, 0n),
      ["claimSpendStart", "claimSpendScan"],
    ],
    [
      "mint",
      forcedWith({
        mints: [{ script: plutusScript(3), redeemer: false }],
      }),
      scriptSourceMissingVerdict(1n, 0n),
      ["claimMintGrammar", "claimMintScan"],
    ],
    [
      "receive",
      forcedWith({
        receives: [{ script: plutusScript(4), redeemer: false }],
      }),
      scriptSourceMissingVerdict(3n, 0n),
      [
        "claimReceiveGrammar",
        "claimReceiveOutputs",
        "claimReceiveFinish",
        "claimReceiveDeclaration",
      ],
    ],
  ] as const)(
    "refuses a forged successor at every role of a ScriptSourceMissing naming a sourced %s",
    async (_label, shape, verdict, domain) => {
      const refused = await forgeEveryRole(shape, {
        kind: "forced",
        verdict,
      });
      expect(refused).toStrictEqual(roles(SOURCE, ENDS, domain));
    },
    2_400_000,
  );

  it("refuses a forged successor at every role of a RedeemerMissing behind a missing source", async () => {
    // Observer 0 has no source; observer 1 is sourced by a reference input
    // and carries no redeemer.
    const refused = await forgeEveryRole(
      {
        ...signed,
        observers: [11, 12].map((seed) => ({
          script: plutusScript(seed),
          witness: false,
          redeemer: false,
        })),
        reference: missingRedeemerLedgerEntries(0x31, [plutusScript(12)]),
      },
      { kind: "forced", verdict: redeemerMissingVerdict(2n, 1n) },
    );
    expect(refused).toStrictEqual(
      roles(SOURCE, ENDS, OBSERVED, [
        "claimedRedeemerGrammar",
        "claimedRedeemerScan",
      ]),
    );
  }, 2_400_000);
});
