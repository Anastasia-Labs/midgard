/** The failing transactions whose traces reach each native-script step of
 * phase A and script sources: over the native script that reads every kind
 * of token, minting under it, with one extra signer; over a native script
 * nobody signs for, minting under it; and over a native script the
 * transaction requires as an observer, or pays its one output to. */
import { hashScriptWitness } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Constr, type Data } from "@lucid-evolution/lucid";

import {
  everyTokenNativeScript,
  ownerOrAnyoneNativeScript,
} from "../helpers/native-script-shapes.js";
import { anyoneObserverScript } from "../helpers/plutus-execution-block-fixture.js";

export const EVERY_TOKEN = Object.freeze({
  failingNative: { script: everyTokenNativeScript() },
  failingNativeMint: true,
  failingSigners: { extraSigners: 1 },
});

export const UNSIGNED = Object.freeze({
  failingNative: { script: ownerOrAnyoneNativeScript() },
  failingNativeMint: true,
  failingSigners: { unsigned: true },
});

export const OBSERVED = Object.freeze({
  failingNative: { script: ownerOrAnyoneNativeScript(), observer: true },
});

export const RECEIVING = Object.freeze({
  failingNative: { script: ownerOrAnyoneNativeScript(), receive: true },
});

/** `OBSERVED` with `observers` observers in all: the first `OBSERVED`'s,
 * each other one any transaction satisfies. */
export const observedBy = (observers: number) =>
  Object.freeze({
    failingNative: {
      ...OBSERVED.failingNative,
      extraObservers: observers - 1,
    },
  });

/** The observer at `index` of those `observedBy(observers)` requires, in
 * the order the transaction lists them. */
export const observerHashAt = (observers: number, index: number): string =>
  [
    ownerOrAnyoneNativeScript(),
    ...Array.from({ length: observers - 1 }, (_, index) =>
      anyoneObserverScript(index),
    ),
  ]
    .map(hashScriptWitness)
    .sort()[index]!;

/** The least of the observers `observedBy(observers)` requires, the first
 * the transaction lists. */
export const firstObserverHash = (observers: number): string =>
  observerHashAt(observers, 0);

/** The one observer `OBSERVED` requires, as Data. */
export const OBSERVER_HASH: Data = hashScriptWitness(
  ownerOrAnyoneNativeScript(),
);

/** The signer-set proof the native-script token semantic resolvers but the
 * signature payloads fix: `NoSignerSetProof`. */
export const NO_SIGNER_SET_PROOF: Data = new Constr(0, []);
