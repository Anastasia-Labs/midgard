/** Committed `unusedScriptWitness` subjects: the script-integrity subjects'
 * transaction builder, with chosen field-6 witnesses beside the purposes'
 * own. Field 6 lists the witnessed observers (by hash), then the mint and
 * receive scripts in shape order, then `witnesses`. */
import {
  type MidgardVersionedScript,
  screenMidgardForcedTxAdmission,
} from "@al-ft/midgard-core";
import { expect } from "vitest";

import {
  buildIntegritySubject,
  expectedHash,
  honestForcedShape,
  type IntegritySubjectShape,
  nativeScript,
  plutusScript,
  scriptLockedEntries,
} from "./script-integrity-hash-mismatch-emulator.subject.js";

type MidgardNativeScript = Extract<
  MidgardVersionedScript,
  { readonly language: "NativeCardano" }
>["nativeScript"];

/** A native script that holds under any context, distinct per `depth`: an
 * `all` of nothing, nested `depth` times. */
export const holdingNativeScript = (depth: number): MidgardVersionedScript => {
  let script: MidgardNativeScript = { type: "all", scripts: [] };
  for (let level = 0; level < depth; level += 1)
    script = { type: "all", scripts: [script] };
  return {
    language: "NativeCardano",
    scriptBytes: Buffer.alloc(0),
    nativeScript: script,
  };
};

/** Every purpose kind, each script used: a script-locked spend, a mint, a
 * native and a MidgardV1 observer, and a protected receive output; then
 * `extra` witnesses, the first at field-6 index 5. */
export const everyPurposeWith = (
  extra: readonly MidgardVersionedScript[] = [],
): IntegritySubjectShape => {
  const spent = plutusScript(11);
  return {
    spend: scriptLockedEntries(0x41, [spent]),
    witnesses: [spent, ...extra],
    spendRedeemers: [0n],
    mints: [{ script: plutusScript(12), redeemer: true }],
    observers: [
      { script: nativeScript(1n), redeemer: false },
      { script: plutusScript(13, "MidgardV1"), redeemer: true },
    ],
    receives: [{ script: plutusScript(14), redeemer: true }],
    integrity: expectedHash("PlutusV3", "MidgardV1"),
  };
};

/** The field-6 index of the first `extra` witness of `everyPurposeWith`. */
export const EVERY_PURPOSE_FIRST_EXTRA = 5n;

const admitted = (shape: IntegritySubjectShape): IntegritySubjectShape => {
  expect(screenMidgardForcedTxAdmission(buildIntegritySubject(shape))).toBe(
    null,
  );
  return shape;
};

/** A forced subject L1 admission accepts (a native-only key spend) carrying
 * `witnesses`, none of which any purpose executes. */
export const admissibleForcedWith = (
  witnesses: readonly MidgardVersionedScript[],
): IntegritySubjectShape =>
  admitted({ ...honestForcedShape(expectedHash()), witnesses });

/** A forced subject L1 admission accepts whose one witness is used: it
 * spends an output locked by that native script. */
export const nativeLockedForced = (): IntegritySubjectShape => {
  const locking = holdingNativeScript(4);
  return admitted({
    spend: scriptLockedEntries(0x31, [locking]),
    witnesses: [locking],
    integrity: expectedHash(),
  });
};
