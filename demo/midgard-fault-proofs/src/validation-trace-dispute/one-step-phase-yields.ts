/** The yields a phase's semantic resolver invokes beside its step, and what
 * its hop reads for them: either the one item yield its item's kind
 * selects, or a list of yields whose reference-input indices the redeemer
 * carries in order (one-step-ledger-output-proof.ts). */
import { parseMidgardVersionedScriptHeader } from "@al-ft/midgard-core";
import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import type { Data, Script, UTxO } from "@lucid-evolution/lucid";

import type { LedgerOutputProofYield } from "./one-step-ledger-output-proof.js";
import type { ValidationTracePhaseStep } from "./one-step-phase.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

/** The yields an item semantic resolver invokes beside the item it reads,
 * by the item's kind, `yield_kind`: the native yield verifies a native
 * script's item, or the rejection of an item whose versioned-script header
 * is malformed; the foreign yield verifies any other script's. */
export const PHASE_A_NATIVE_ITEM_YIELDS = Object.freeze([
  "phaseANativeItemNative",
  "phaseANativeItemForeign",
] as const);

/** The yields a script-sources semantic resolver invokes beside its step
 * (one-step-script-sources-yields.ts): the middle yield of each role
 * (`middle_role`), and the redeemer item-step yield. */
export const SCRIPT_SOURCES_YIELDS = Object.freeze([
  "scriptSourcesStageTwoAdvance",
  "scriptSourcesStageThreeReplay",
  "scriptSourcesStageThreeFinish",
  "scriptSourcesStageFourBegin",
  "scriptSourcesStageFourFinish",
  "scriptSourcesStageSixBeginPolicy",
  "scriptSourcesStageSixFoldAsset",
  "scriptSourcesStageSixFinish",
  "scriptSourcesRedeemerDescriptor",
] as const);

export type ValidationTracePhaseYield =
  | (typeof PHASE_A_NATIVE_ITEM_YIELDS)[number]
  | (typeof SCRIPT_SOURCES_YIELDS)[number]
  | LedgerOutputProofYield;

/** The yield a semantic resolver invokes beside the item it reads: its
 * `yield_kind`, if the resolver takes one, and which yield that is. */
export type ValidationTracePhaseItemYield = Readonly<{
  kind?: number;
  name: ValidationTracePhaseYield;
}>;

/** The yield among `yields` that verifies the item `itemIndex` of the field
 * `preimage`, by its versioned-script header (`versioned_script_header_v1`):
 * the first, native, for a native script or a malformed header, the second,
 * foreign, for any other script. */
export const itemYieldOf = (
  yields: readonly ValidationTracePhaseYield[],
  preimage: string,
  itemIndex: Data | undefined,
): ValidationTracePhaseItemYield => {
  const item =
    typeof itemIndex === "bigint"
      ? decodeMidgardFieldPreimage(Buffer.from(preimage, "hex"))[
          Number(itemIndex)
        ]
      : undefined;
  if (item === undefined)
    throw new Error("validation-trace item is not in its field");
  const header = parseMidgardVersionedScriptHeader(item, item.length);
  const kind = header === null || header.languageTag === 0 ? 0 : 1;
  const name = yields[kind];
  if (name === undefined)
    throw new Error(`validation-trace item has no yield of kind ${kind}`);
  return Object.freeze({ kind, name });
};

/** The item yield a semantic resolver's hop invokes, as the hop reads it:
 * its kind, if any, its reference under its role's authentication token,
 * and its withdrawal script. */
export type ValidationTracePhaseYieldReads = Readonly<{
  kind?: number;
  reference: UTxO;
  script: Script;
}>;

/** The yield list a semantic resolver's hop invokes, as the hop reads it:
 * each yield's reference and withdrawal script, in the list's order. */
export type ValidationTracePhaseYieldListReads = Readonly<{
  references: readonly UTxO[];
  scripts: readonly Script[];
}>;

const yieldRead = (
  deployment: ValidationTraceDisputeDeployment,
  name: ValidationTracePhaseYield,
): Readonly<{ reference: UTxO; script: Script }> => {
  const reference = deployment.references.oneStep?.phaseYields?.[name];
  if (reference === undefined)
    throw new Error(
      `validation-trace deployment does not publish the ${name} yield`,
    );
  return { reference, script: deployment.chain.yields[name].withdrawalScript };
};

/** What the hop of the step `step` reads of `deployment` for the item
 * yield its semantic resolver invokes; undefined when it invokes none. */
export const phaseYieldReads = (
  deployment: ValidationTraceDisputeDeployment,
  step: ValidationTracePhaseStep,
): ValidationTracePhaseYieldReads | undefined => {
  if (step.itemYield === undefined) return undefined;
  const { kind, name } = step.itemYield;
  return Object.freeze({
    ...(kind === undefined ? {} : { kind }),
    ...yieldRead(deployment, name),
  });
};

/** What the hop of the step `step` reads of `deployment` for the yield
 * list its semantic resolver invokes; undefined when it takes none. */
export const phaseYieldListReads = (
  deployment: ValidationTraceDisputeDeployment,
  step: ValidationTracePhaseStep,
): ValidationTracePhaseYieldListReads | undefined => {
  if (step.yieldList === undefined) return undefined;
  const reads = step.yieldList.map((name) => yieldRead(deployment, name));
  return Object.freeze({
    references: Object.freeze(reads.map(({ reference }) => reference)),
    scripts: Object.freeze(reads.map(({ script }) => script)),
  });
};
