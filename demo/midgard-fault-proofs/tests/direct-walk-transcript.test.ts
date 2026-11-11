/**
 * Pins the bytes of every direct-walk fault-proof family: each capture case
 * of the nine installed lifecycles is driven again with seeded wallets and a
 * fixed clock, and the hash and signed-CBOR digest of every transaction its
 * cursor submits must equal the committed snapshot. Folding the families onto
 * one kit must leave this transcript byte-identical; a drift is a behaviour
 * change, never a reason to rewrite the snapshot.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { createDistinctAssetAccumulationLimitTransactionPort } from "../src/distinct-asset-accumulation-limit/index.js";
import { createExecutionNativeScriptInvalidTransactionPort } from "../src/execution-native-script-invalid/index.js";
import { createExecutionSourceScriptDecodingTransactionPort } from "../src/execution-source-script-decoding/index.js";
import type { LedgerEntry } from "../src/ledger-at.js";
import { createMissingRedeemerTransactionPort } from "../src/missing-redeemer/index.js";
import { createMissingScriptSourceTransactionPort } from "../src/missing-script-source/index.js";
import { createReceivePurposeLanguageTransactionPort } from "../src/receive-purpose-language/index.js";
import { createScriptIntegrityHashMismatchTransactionPort } from "../src/script-integrity-hash-mismatch/index.js";
import { createUnusedRedeemerTransactionPort } from "../src/unused-redeemer/index.js";
import { createUnusedScriptWitnessTransactionPort } from "../src/unused-script-witness/index.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
  RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
  SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
  UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { CURSOR_FAMILY_SPECS } from "../src/workflow/cursor-family-spec.js";
import { deepSourceInputs } from "./support/direct-purpose-deep-source.js";
import {
  type DirectWalkCategory,
  type DirectWalkFamily,
  type DirectWalkScene,
  driveDirectWalkLifecycle,
  recordDirectWalkTranscript,
} from "./support/direct-walk-transcript.js";
import {
  makeDistinctAssetAccumulationLimitHarness,
  publishDistinctAssetAccumulationLimitRoles,
} from "./support/distinct-asset-accumulation-limit-emulator.js";
import {
  referenceEntries,
  rejectedFor as distinctAssetRejectedFor,
  setupDistinctAssetScenario,
  spentEntries,
} from "./support/distinct-asset-accumulation-limit-emulator.subject.js";
import {
  makeExecutionNativeScriptInvalidHarness,
  publishExecutionNativeScriptInvalidRoles,
} from "./support/execution-native-script-invalid-emulator.js";
import {
  admissibleNativeObservers,
  native,
  nativeFalseAt,
  PAYER,
  setupNativeScenario,
  sig,
} from "./support/execution-native-script-invalid-emulator.subject.js";
import {
  makeExecutionSourceScriptDecodingHarness,
  publishExecutionSourceScriptDecodingRoles,
} from "./support/execution-source-script-decoding-emulator.js";
import {
  makeMissingRedeemerHarness,
  publishMissingRedeemerRoles,
} from "./support/missing-redeemer-emulator.js";
import {
  honestForcedObserverShape,
  type MissingRedeemerSource,
  type MissingRedeemerSubjectShape,
  nativeScript,
  plutusScript,
  redeemerMissingVerdict,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  makeMissingScriptSourceHarness,
  publishMissingScriptSourceRoles,
  scriptSourceMissingVerdict,
} from "./support/missing-script-source-emulator.js";
import {
  forcedReceiving,
  makeReceivePurposeLanguageHarness,
  publishReceivePurposeLanguageRoles,
  receivePurposeLanguageVerdict,
} from "./support/receive-purpose-language-emulator.js";
import {
  makeScriptIntegrityHashMismatchHarness,
  publishScriptIntegrityHashMismatchRoles,
} from "./support/script-integrity-hash-mismatch-emulator.js";
import {
  expectedHash,
  plutusScript as integrityPlutusScript,
  rejectedFor as integrityRejectedFor,
  setupIntegrityScenario,
  WRONG_HASH,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import {
  makeUnusedRedeemerHarness,
  publishUnusedRedeemerRoles,
  unusedRedeemerVerdict,
} from "./support/unused-redeemer-emulator.js";
import {
  makeUnusedScriptWitnessHarness,
  publishUnusedScriptWitnessRoles,
} from "./support/unused-script-witness-emulator.js";
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

const familyCases = <Category extends DirectWalkCategory>(
  family: Omit<DirectWalkFamily<Category>, "spec">,
  scenes: Readonly<Record<string, () => Promise<DirectWalkScene>>>,
): readonly Case[] =>
  Object.entries(scenes).map(([name, scene]) => ({
    name: `${family.label}: ${name}`,
    drive: async () =>
      await driveDirectWalkLifecycle(
        { ...family, spec: CURSOR_FAMILY_SPECS[family.category] as never },
        await scene(),
      ),
  }));

/** A family's scene builder: its harness, block setup and role publication,
 * and the predecessor its lifecycle replays over, where it replays over one. */
const scenes =
  <Harness extends DirectWalkScene["harness"], Shape>(family: {
    readonly harness: () => Promise<Harness>;
    readonly setup: (args: {
      readonly harness: Harness;
      readonly shape: Shape;
      readonly source: Source;
    }) => Promise<DirectWalkScene["scenario"]>;
    readonly contracts: (harness: Harness) => DirectWalkScene["contracts"];
    readonly publishRoles: (
      harness: Harness,
    ) => ReturnType<DirectWalkScene["publishRoles"]>;
    readonly predecessor?: (shape: Shape) => readonly LedgerEntry[];
  }) =>
  (shape: Shape, source: Source) =>
  async (): Promise<DirectWalkScene> => {
    const h = await family.harness();
    return {
      harness: h,
      contracts: family.contracts(h),
      scenario: await family.setup({ harness: h, shape, source }),
      ...(family.predecessor === undefined
        ? {}
        : { predecessor: family.predecessor(shape) }),
      publishRoles: () => family.publishRoles(h),
    };
  };

const missingRedeemer = scenes({
  harness: makeMissingRedeemerHarness,
  setup: setupMissingRedeemerScenario,
  contracts: (h) => h.missingRedeemer,
  publishRoles: publishMissingRedeemerRoles,
});
const missingScriptSource = scenes({
  harness: makeMissingScriptSourceHarness,
  setup: setupMissingRedeemerScenario,
  contracts: (h) => h.missingScriptSource,
  publishRoles: publishMissingScriptSourceRoles,
  predecessor: inputsOf,
});
const receivePurposeLanguage = scenes({
  harness: makeReceivePurposeLanguageHarness,
  setup: setupMissingRedeemerScenario,
  contracts: (h) => h.receivePurposeLanguage,
  publishRoles: publishReceivePurposeLanguageRoles,
  predecessor: (shape) => [...inputsOf(shape), ...(shape.padding ?? [])],
});
const unusedRedeemer = scenes({
  harness: makeUnusedRedeemerHarness,
  setup: setupMissingRedeemerScenario,
  contracts: (h) => h.unusedRedeemer,
  publishRoles: publishUnusedRedeemerRoles,
  predecessor: inputsOf,
});
const executionSourceScriptDecoding = scenes({
  harness: makeExecutionSourceScriptDecodingHarness,
  setup: setupMissingRedeemerScenario,
  contracts: (h) => h.essd,
  publishRoles: publishExecutionSourceScriptDecodingRoles,
  predecessor: inputsOf,
});
const distinctAsset = scenes({
  harness: makeDistinctAssetAccumulationLimitHarness,
  setup: setupDistinctAssetScenario,
  contracts: (h) => h.distinctAssetAccumulationLimit,
  publishRoles: publishDistinctAssetAccumulationLimitRoles,
  predecessor: (shape) => [...spentEntries(shape), ...referenceEntries(shape)],
});
const nativeInvalid = scenes({
  harness: makeExecutionNativeScriptInvalidHarness,
  setup: setupNativeScenario,
  contracts: (h) => h.executionNativeScriptInvalid,
  publishRoles: publishExecutionNativeScriptInvalidRoles,
  predecessor: inputsOf,
});
const integrityMismatch = scenes({
  harness: makeScriptIntegrityHashMismatchHarness,
  setup: setupIntegrityScenario,
  contracts: (h) => h.scriptIntegrityHashMismatch,
  publishRoles: publishScriptIntegrityHashMismatchRoles,
});
const unusedWitness = scenes({
  harness: makeUnusedScriptWitnessHarness,
  setup: setupIntegrityScenario,
  contracts: (h) => h.unusedScriptWitness,
  publishRoles: publishUnusedScriptWitnessRoles,
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
  ...familyCases(
    {
      label: "missing-redeemer",
      category: "missingRedeemer",
      replay: MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
      roleOrder: SDK.MISSING_REDEEMER_ROLE_ORDER,
      createPort: createMissingRedeemerTransactionPort,
    },
    {
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
    },
  ),
  ...familyCases(
    {
      label: "missing-script-source",
      category: "missingScriptSource",
      replay: MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY,
      roleOrder: SDK.MISSING_SCRIPT_SOURCE_ROLE_ORDER,
      createPort: createMissingScriptSourceTransactionPort,
    },
    {
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
    },
  ),
  ...familyCases(
    {
      label: "receive-purpose-language",
      category: "receivePurposeLanguage",
      replay: RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY,
      roleOrder: SDK.RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER,
      createPort: createReceivePurposeLanguageTransactionPort,
    },
    {
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
    },
  ),
  ...familyCases(
    {
      label: "unused-redeemer",
      category: "unusedRedeemer",
      replay: UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
      roleOrder: SDK.UNUSED_REDEEMER_ROLE_ORDER,
      createPort: createUnusedRedeemerTransactionPort,
    },
    {
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
    },
  ),
  ...familyCases(
    {
      label: "execution-source-script-decoding",
      category: "executionSourceScriptDecoding",
      replay: EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      roleOrder: SDK.EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER,
      createPort: createExecutionSourceScriptDecodingTransactionPort,
    },
    {
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
    },
  ),
  ...familyCases(
    {
      label: "distinct-asset-accumulation-limit",
      category: "distinctAssetAccumulationLimit",
      replay: DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
      roleOrder: SDK.DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER,
      createPort: createDistinctAssetAccumulationLimitTransactionPort,
    },
    {
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
    },
  ),
  ...familyCases(
    {
      label: "execution-native-script-invalid",
      category: "executionNativeScriptInvalid",
      replay: EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
      roleOrder: SDK.EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER,
      createPort: createExecutionNativeScriptInvalidTransactionPort,
    },
    {
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
    },
  ),
  ...familyCases(
    {
      label: "script-integrity-hash-mismatch",
      category: "scriptIntegrityHashMismatch",
      replay: SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
      roleOrder: SDK.SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER,
      createPort: createScriptIntegrityHashMismatchTransactionPort,
    },
    {
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
    },
  ),
  ...familyCases(
    {
      label: "unused-script-witness",
      category: "unusedScriptWitness",
      replay: UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
      roleOrder: SDK.UNUSED_SCRIPT_WITNESS_ROLE_ORDER,
      createPort: createUnusedScriptWitnessTransactionPort,
    },
    {
      "accepted unused Plutus witness": unusedWitness(
        everyPurposeWith([integrityPlutusScript(16)]),
        NORMAL,
      ),
      "admitted forced acceptance with an unused native witness": unusedWitness(
        admissibleForcedWith([holdingNativeScript(3)]),
        { kind: "forced", verdict: "ForcedTxValid" },
      ),
    },
  ),
];

describe("direct-walk transcript", () => {
  for (const { name, drive } of CASES)
    it(`keeps the bytes of ${name}`, { timeout: 600_000 }, async () => {
      expect(await recordDirectWalkTranscript(name, drive)).toMatchSnapshot();
    });
});
