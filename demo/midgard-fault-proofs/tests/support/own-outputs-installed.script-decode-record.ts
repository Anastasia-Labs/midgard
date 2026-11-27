/** The script-decode record rows of the installed own-outputs test
 * (`witness-script-decoding-installed-own-outputs.test.ts`): the witness it
 * records, its carriage roles, and what a run's own transactions were. */
import { expect } from "vitest";

import { scriptDecodeRunTokenName } from "../../src/script-decode/data.js";
import {
  adoptedAndRejected,
  kindOf,
  ledgerTimeline,
  type LedgerWatch,
  scriptDecodeRoles,
  stagedChunkOf,
  strictShape,
  type TakenPlanted,
  takenPlanted,
} from "./own-outputs-installed.js";
import { scriptDecodeDeploymentOf } from "./script-decode-emulator.js";
import { type InstalledTurnContext } from "./script-decode-installed-run.js";
import {
  delayChainScript,
  plutusItem,
} from "./script-decode-plutus-max-shape.js";

/** An undecodable witness whose record takes one staged chunk, a Bind, a few Walks, a Finish. */
export const ITEM = plutusItem(delayChainScript(600));

export type Harness = InstalledTurnContext["harness"];

export const rolesOf = (harness: Harness) =>
  scriptDecodeRoles({
    emulator: harness.emulator,
    materialsAddress: harness.proverSigner.proofMaterialsAddress,
    item: ITEM,
  });

/** What a run's own transactions were and what they cost. */
export const costOf = (watch: LedgerWatch, harness: Harness) => {
  const { staging, run } = rolesOf(harness);
  return watch.ours().map((transaction) => ({
    kind: kindOf(transaction, staging, run),
    fee: transaction.fee,
  }));
};

/**
 * Every output of another party the application's own transactions took is
 * an exact copy of carriage, or a run of the script at the record's address
 * holding the record policy's own run token; returns those it took.
 */
export const expectOwnOutputsOnly = (
  watch: LedgerWatch,
  harness: Harness,
): readonly TakenPlanted[] => {
  expect(watch.plantedOutRefs().size).toBeGreaterThan(0);
  const { script, run } = rolesOf(harness);
  const runUnit = `${scriptDecodeDeploymentOf(harness.emulator).validator.policyId}${scriptDecodeRunTokenName(script.scriptHashHex)}`;
  const { adopted, rejected } = adoptedAndRejected(
    takenPlanted(watch),
    (shape) =>
      strictShape(
        shape,
        (unit, quantity) =>
          shape.address === run.address && unit === runUnit && quantity === "1",
      ),
  );
  expect(
    rejected,
    JSON.stringify({ rejected, timeline: ledgerTimeline(watch) }),
  ).toEqual([]);
  return adopted;
};

/** The baseline's transactions without the first of `kind`. */
export const withoutFirst = (
  baseline: readonly Readonly<{ kind: string; fee: bigint }>[],
  kind: string,
) => {
  const index = baseline.findIndex((transaction) => transaction.kind === kind);
  expect(index).toBeGreaterThanOrEqual(0);
  return baseline.filter((_, at) => at !== index);
};

/** The application's stage transactions, and the chunk each one staged. */
export const stagesOf = (watch: LedgerWatch, harness: Harness) => {
  const { staging } = rolesOf(harness);
  return watch.ours().flatMap((transaction) => {
    const chunk = stagedChunkOf(transaction, staging);
    return chunk === undefined ? [] : [{ transaction, chunk }];
  });
};

/** The transactions that read the run's staged bytes: the Bind, then each Walk. */
export const readersOf = (watch: LedgerWatch, harness: Harness) => {
  const { staging, run } = rolesOf(harness);
  const kinds = watch.ours().map((transaction) => ({
    transaction,
    kind: kindOf(transaction, staging, run),
  }));
  const bind = kinds.find(({ kind }) => kind === "bind-or-finish");
  return [
    ...(bind === undefined ? [] : [bind.transaction]),
    ...kinds
      .filter(({ kind }) => kind === "walk")
      .map(({ transaction }) => transaction),
  ];
};
