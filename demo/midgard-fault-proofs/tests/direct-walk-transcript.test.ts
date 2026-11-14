/**
 * Pins the bytes of every direct-walk fault-proof family: each capture case
 * of the nine installed lifecycles is driven again with seeded wallets and a
 * fixed clock, and the full and step digests of every transaction its cursor
 * submits must equal the committed snapshot. Folding the families onto one
 * kit must leave this transcript byte-identical; a drift is a behaviour
 * change, never a reason to rewrite the snapshot.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createDirectWalkTransactionPort,
  DIRECT_PURPOSE_FIELDS,
  type DirectPurposePlannedArtifact,
  type DirectPurposeReplay,
  type DirectPurposeRole,
  directPurposeStep01Datum,
  type DirectWalkFamily,
  type DirectWalkPortConfig,
  type DirectWalkProof,
  planDirectPurposeArtifact,
} from "../src/direct-purpose-family/index.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_DIRECT_WALK } from "../src/distinct-asset-accumulation-limit/index.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_WALK } from "../src/execution-native-script-invalid/index.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_DIRECT_WALK } from "../src/execution-source-script-decoding/index.js";
import type { LedgerEntry } from "../src/ledger-at.js";
import { MISSING_REDEEMER_DIRECT_WALK } from "../src/missing-redeemer/index.js";
import { MISSING_SCRIPT_SOURCE_DIRECT_WALK } from "../src/missing-script-source/index.js";
import { RECEIVE_PURPOSE_LANGUAGE_DIRECT_WALK } from "../src/receive-purpose-language/index.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_DIRECT_WALK } from "../src/script-integrity-hash-mismatch/index.js";
import { UNUSED_REDEEMER_DIRECT_WALK } from "../src/unused-redeemer/index.js";
import { UNUSED_SCRIPT_WITNESS_DIRECT_WALK } from "../src/unused-script-witness/index.js";
import { deepSourceInputs } from "./support/direct-purpose-deep-source.js";
import {
  type DirectWalkCategory,
  type DirectWalkHarness,
  makeDirectWalkHarness,
  publishDirectWalkRoles,
} from "./support/direct-walk-emulator.js";
import {
  type DirectWalkScene,
  type DirectWalkSceneFamily,
  driveDirectWalkLifecycle,
  recordDirectWalkTranscript,
} from "./support/direct-walk-transcript.js";
import {
  referenceEntries,
  rejectedFor as distinctAssetRejectedFor,
  setupDistinctAssetScenario,
  spentEntries,
} from "./support/distinct-asset-accumulation-limit-emulator.subject.js";
import {
  admissibleNativeObservers,
  native,
  nativeFalseAt,
  PAYER,
  setupNativeScenario,
  sig,
} from "./support/execution-native-script-invalid-emulator.subject.js";
import {
  honestForcedObserverShape,
  type MissingRedeemerSource,
  type MissingRedeemerSubjectShape,
  nativeScript,
  plutusScript,
  redeemerMissingVerdict,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import { scriptSourceMissingVerdict } from "./support/missing-script-source-emulator.js";
import {
  forcedReceiving,
  receivePurposeLanguageVerdict,
} from "./support/receive-purpose-language-emulator.js";
import {
  expectedHash,
  plutusScript as integrityPlutusScript,
  rejectedFor as integrityRejectedFor,
  setupIntegrityScenario,
  WRONG_HASH,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import { unusedRedeemerVerdict } from "./support/unused-redeemer-emulator.js";
import {
  admissibleForcedWith,
  everyPurposeWith,
  holdingNativeScript,
} from "./support/unused-script-witness-emulator.subject.js";

type Source = MissingRedeemerSource;

const NORMAL: Source = { kind: "normal" };

const forced = (verdict: SDK.OperatorVerdict): Source => ({
  kind: "forced",
  verdict,
});

/** The subject's spent and referenced entries. */
const inputsOf = (shape: {
  readonly spend?: readonly LedgerEntry[];
  readonly reference?: readonly LedgerEntry[];
}) => [...(shape.spend ?? []), ...(shape.reference ?? [])];

type Case = Readonly<{
  name: string;
  drive: () => ReturnType<typeof driveDirectWalkLifecycle>;
}>;

/**
 * The fields a page opens past its page-start role's: following the plan
 * from step 01's datum, every page the port fits at a page start must open
 * only fields its role's transitions open (`fieldsOf`), since the carriage
 * prerequisite publishes a field ahead of an action by that role alone while
 * the port resolves the carriage of the whole page.
 */
const uncarriedPageFields = async (
  roleOrder: readonly DirectPurposeRole[],
  planned: Readonly<{
    prover: string;
    plan: () => Promise<
      DirectPurposePlannedArtifact<DirectPurposeReplay, DirectWalkProof>
    >;
  }>,
) => {
  const plan = await planned.plan();
  const { paging } = plan;
  const uncarried: string[] = [];
  let pages = 0;
  let role = roleOrder[1]!;
  let datum = directPurposeStep01Datum(plan.admitted.material, planned.prover);
  while (role !== "terminal") {
    if (paging.isPageStart(role, datum)) {
      pages += 1;
      const carried = plan.fieldsOf(role);
      for (const transition of paging.page(role, datum).transitions)
        for (const field of transition.fields)
          if (!carried.includes(field))
            uncarried.push(`${role} page: ${transition.role} opens ${field}`);
    }
    const next = paging.transitionAt(role, datum);
    role = next.next;
    datum = next.datum;
  }
  return { pages, uncarried };
};

/** A family's cases, each driving the port its walk declares and checking
 * the carriage of every page of the plan the port drove. */
const familyCases = <
  Category extends DirectWalkCategory,
  StepCount extends number,
  Prepared extends DirectPurposeReplay,
  Proof extends DirectWalkProof,
>(
  walk: DirectWalkFamily<Category, StepCount, Prepared, Proof>,
  scenes: Readonly<Record<string, () => Promise<DirectWalkScene>>>,
): readonly Case[] => {
  let planned: Parameters<typeof uncarriedPageFields>[1] | null = null;
  const family: DirectWalkSceneFamily<Category> = {
    label: walk.family.label,
    category: walk.spec.category,
    spec: walk.spec,
    replay: walk.replay,
    roleOrder: walk.family.roleOrder,
    createPort: (config: DirectWalkPortConfig<Category, StepCount>) => {
      const port = createDirectWalkTransactionPort(walk, config);
      return {
        ...port,
        prepare: async (args) => {
          const artifact = await port.prepare(args);
          planned = {
            prover: config.signer.paymentKeyHash,
            // Planned after the lifecycle, the port's own plan, cached.
            plan: () =>
              planDirectPurposeArtifact(walk.artifact, {
                artifact,
                contracts: config.contracts,
                prover: config.signer.paymentKeyHash,
              }),
          };
          return artifact;
        },
      };
    },
  };
  return Object.entries(scenes).map(([name, scene]) => ({
    name: `${family.label}: ${name}`,
    drive: async () => {
      planned = null;
      const actions = await driveDirectWalkLifecycle(family, await scene());
      const { pages, uncarried } = await uncarriedPageFields(
        walk.family.roleOrder,
        planned!,
      );
      expect(uncarried).toStrictEqual([]);
      // A walk that opens fields fits at least one page.
      expect(pages > 0).toBe((walk.fields ?? DIRECT_PURPOSE_FIELDS).length > 0);
      return actions;
    },
  }));
};

/** A family's scene builder: its walk's harness, block setup and role
 * publication, and the predecessor its lifecycle replays over, where it
 * replays over one. */
const scenes =
  <Shape>(family: {
    readonly walk: Parameters<typeof makeDirectWalkHarness>[0] &
      Parameters<typeof publishDirectWalkRoles>[1];
    readonly setup: (args: {
      readonly harness: DirectWalkHarness;
      readonly shape: Shape;
      readonly source: Source;
    }) => Promise<DirectWalkScene["scenario"]>;
    readonly predecessor?: (shape: Shape) => readonly LedgerEntry[];
  }) =>
  (shape: Shape, source: Source) =>
  async (): Promise<DirectWalkScene> => {
    const h = await makeDirectWalkHarness(family.walk);
    return {
      harness: h,
      contracts: h.direct,
      scenario: await family.setup({ harness: h, shape, source }),
      ...(family.predecessor === undefined
        ? {}
        : { predecessor: family.predecessor(shape) }),
      publishRoles: () => publishDirectWalkRoles(h, family.walk),
    };
  };

const missingRedeemer = scenes({
  walk: MISSING_REDEEMER_DIRECT_WALK,
  setup: setupMissingRedeemerScenario,
});
const missingScriptSource = scenes({
  walk: MISSING_SCRIPT_SOURCE_DIRECT_WALK,
  setup: setupMissingRedeemerScenario,
  predecessor: inputsOf,
});
const receivePurposeLanguage = scenes({
  walk: RECEIVE_PURPOSE_LANGUAGE_DIRECT_WALK,
  setup: setupMissingRedeemerScenario,
  predecessor: (shape) => [...inputsOf(shape), ...(shape.padding ?? [])],
});
const unusedRedeemer = scenes({
  walk: UNUSED_REDEEMER_DIRECT_WALK,
  setup: setupMissingRedeemerScenario,
  predecessor: inputsOf,
});
const executionSourceScriptDecoding = scenes({
  walk: EXECUTION_SOURCE_SCRIPT_DECODING_DIRECT_WALK,
  setup: setupMissingRedeemerScenario,
  predecessor: inputsOf,
});
const distinctAsset = scenes({
  walk: DISTINCT_ASSET_ACCUMULATION_LIMIT_DIRECT_WALK,
  setup: setupDistinctAssetScenario,
  predecessor: (shape) => [...spentEntries(shape), ...referenceEntries(shape)],
});
const nativeInvalid = scenes({
  walk: EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_WALK,
  setup: setupNativeScenario,
  predecessor: inputsOf,
});
const integrityMismatch = scenes({
  walk: SCRIPT_INTEGRITY_HASH_MISMATCH_DIRECT_WALK,
  setup: setupIntegrityScenario,
});
const unusedWitness = scenes({
  walk: UNUSED_SCRIPT_WITNESS_DIRECT_WALK,
  setup: setupIntegrityScenario,
  predecessor: inputsOf,
});

const SPENT = Array.from({ length: 10 }, () => 1_536);
const ESSD_OBSERVER = { purpose_kind: 2n, purpose_index: 0n };
const FORCED_SOURCELESS: MissingRedeemerSubjectShape = {
  ...honestForcedObserverShape(5),
  observers: [{ script: plutusScript(5), witness: false, redeemer: false }],
  reference: [],
};
const DEEP_RECEIVE = {
  observers: [],
  receives: [{ script: plutusScript(1), witness: false, redeemer: true }],
  ...deepSourceInputs(plutusScript(1)),
};

/** Every capture case of the nine installed lifecycles, in their order. */
const CASES: readonly Case[] = [
  ...familyCases(MISSING_REDEEMER_DIRECT_WALK, {
    "accepted missing observer redeemer": missingRedeemer(
      {
        observers: [
          { script: plutusScript(1), redeemer: false },
          { script: plutusScript(2), redeemer: true },
        ],
      },
      NORMAL,
    ),
    "forced RedeemerMissing on a native observer": missingRedeemer(
      { observers: [{ script: nativeScript(1n), redeemer: false }] },
      forced(redeemerMissingVerdict(2n, 0n)),
    ),
  }),
  ...familyCases(MISSING_SCRIPT_SOURCE_DIRECT_WALK, {
    "accepted observer with no source": missingScriptSource(
      {
        observers: [
          { script: plutusScript(1), witness: false, redeemer: true },
          { script: plutusScript(2), redeemer: true },
        ],
      },
      NORMAL,
    ),
    "forced ScriptSourceMissing on a reference-sourced observer":
      missingScriptSource(
        honestForcedObserverShape(5),
        forced(scriptSourceMissingVerdict(2n, 0n)),
      ),
  }),
  ...familyCases(RECEIVE_PURPOSE_LANGUAGE_DIRECT_WALK, {
    "accepted PlutusV3 receive execution": receivePurposeLanguage(
      {
        observers: [],
        receives: [{ script: plutusScript(3), redeemer: true }],
      },
      NORMAL,
    ),
    "accepted PlutusV3 receive execution over a deep ledger":
      receivePurposeLanguage(DEEP_RECEIVE, NORMAL),
    "forced arm at a PlutusV3 receive no redeemer reaches":
      receivePurposeLanguage(
        forcedReceiving(plutusScript(4)),
        forced(receivePurposeLanguageVerdict(3n, 0n)),
      ),
  }),
  ...familyCases(UNUSED_REDEEMER_DIRECT_WALK, {
    "accepted redeemer with no purpose": unusedRedeemer(
      {
        observers: [{ script: plutusScript(1), redeemer: true }],
        paddingRedeemers: [Buffer.from(Data.to(0n), "hex")],
      },
      NORMAL,
    ),
    "forced UnusedRedeemer past an empty redeemer field": unusedRedeemer(
      FORCED_SOURCELESS,
      forced(unusedRedeemerVerdict(0n)),
    ),
  }),
  ...familyCases(EXECUTION_SOURCE_SCRIPT_DECODING_DIRECT_WALK, {
    "forced ExecutionNativeScriptMalformed rejection":
      executionSourceScriptDecoding(
        honestForcedObserverShape(5),
        forced({
          ForcedTxInvalid: {
            reason: { ExecutionNativeScriptMalformed: ESSD_OBSERVER },
          },
        }),
      ),
    "forced ExecutionNativeScriptDepthLimit rejection":
      executionSourceScriptDecoding(
        honestForcedObserverShape(5),
        forced({
          ForcedTxInvalid: {
            reason: { ExecutionNativeScriptDepthLimit: ESSD_OBSERVER },
          },
        }),
      ),
  }),
  ...familyCases(DISTINCT_ASSET_ACCUMULATION_LIMIT_DIRECT_WALK, {
    "accepted mint crossing": distinctAsset(
      { inputs: SPENT, outputs: [[256], [256]], mint: [256, 256, 10] },
      NORMAL,
    ),
    "forced input crossing recorded at its field ordinal": distinctAsset(
      { inputs: [...SPENT, 1_536], references: 2, outputs: [[1]], mint: [] },
      distinctAssetRejectedFor({
        InputAssetAccumulationLimit: {
          input_index: 10n,
          asset_index: 1_024n,
        },
      }),
    ),
  }),
  ...familyCases(EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_WALK, {
    "accepted observer whose native script is not yet valid": nativeInvalid(
      admissibleNativeObservers(
        [
          native({
            type: "all",
            scripts: [sig(PAYER), { type: "after", slot: 100n }],
          }),
        ],
        { validityStart: 0n },
      ),
      NORMAL,
    ),
    "forced ExecutionNativeScriptFalse rejection of a script that holds":
      nativeInvalid(
        admissibleNativeObservers([native(sig(PAYER))]),
        forced({ ForcedTxInvalid: { reason: nativeFalseAt(2n, 0n) } }),
      ),
  }),
  ...familyCases(SCRIPT_INTEGRITY_HASH_MISMATCH_DIRECT_WALK, {
    "accepted wrong hash": integrityMismatch(
      {
        observers: [{ script: integrityPlutusScript(1), redeemer: true }],
        integrity: WRONG_HASH,
      },
      NORMAL,
    ),
    "forced false ScriptIntegrityHashMismatch": integrityMismatch(
      {
        observers: [{ script: integrityPlutusScript(2), redeemer: true }],
        integrity: expectedHash("PlutusV3"),
      },
      integrityRejectedFor("ScriptIntegrityHashMismatch"),
    ),
  }),
  ...familyCases(UNUSED_SCRIPT_WITNESS_DIRECT_WALK, {
    "accepted unused Plutus witness": unusedWitness(
      everyPurposeWith([integrityPlutusScript(16)]),
      NORMAL,
    ),
    "admitted forced acceptance with an unused native witness": unusedWitness(
      admissibleForcedWith([holdingNativeScript(3)]),
      { kind: "forced", verdict: "ForcedTxValid" },
    ),
  }),
];

describe("direct-walk transcript", () => {
  for (const { name, drive } of CASES)
    it(`keeps the bytes of ${name}`, { timeout: 600_000 }, async () => {
      expect(await recordDirectWalkTranscript(name, drive)).toMatchSnapshot();
    });
});
