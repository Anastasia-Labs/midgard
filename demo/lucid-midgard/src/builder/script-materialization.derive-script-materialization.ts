import {
  computeScriptIntegrityHashForLanguages,
  EMPTY_NULL_ROOT,
  encodeMidgardFieldPreimageForField,
  encodeMidgardVersionedScriptListPreimage,
  midgardFieldCommitment,
  midgardRedeemerPurposeFromTag,
  type ScriptLanguageName,
} from "@al-ft/midgard-core/codec";

import { BuilderInvariantError } from "../core/errors.js";
import { compareOutRefs, outRefLabel } from "../core/out-ref.js";
import {
  outputAddressPaymentScriptHash,
  outputAddressProtected,
} from "../core/output.js";
import type { Redeemer } from "../core/scripts.js";
import { mintDeltaAssets } from "./balancing.js";
import type { BuilderState } from "./context.js";
import {
  assertAllRedeemerIntentsConsumed,
  collectKnownScriptSources,
  effectiveMints,
  findMintRedeemer,
  findObserverRedeemer,
  findReceiveRedeemer,
  findSpendRedeemer,
  mintPreimageCbor,
  paymentScriptHashFromUtxo,
  pointerKey,
  recordConsumedRedeemer,
  redeemerIntentKey,
  requiredObserversPreimageCbor,
  resolveKnownScript,
} from "./script-materialization.collect-known-script-sources.js";
import {
  type DerivedRedeemer,
  type KnownScriptSource,
  normalizeExUnits,
  redeemerDataBytes,
  type RedeemerPointer,
  RedeemerTags,
} from "./script-materialization.known-script-source.js";
import { type ScriptMaterialization } from "./unsigned-tx.js";

const addRequiredExecution = ({
  scriptHash,
  purpose,
  pointer,
  redeemer,
  sources,
  redeemers,
  usedSources,
  languages,
}: {
  readonly scriptHash: string;
  readonly purpose: string;
  readonly pointer: RedeemerPointer;
  readonly redeemer: Redeemer | undefined;
  readonly sources: readonly KnownScriptSource[];
  readonly redeemers: DerivedRedeemer[];
  readonly usedSources: Set<string>;
  readonly languages: Set<ScriptLanguageName>;
}): void => {
  const resolved = resolveKnownScript(scriptHash, sources);
  if (resolved === undefined) {
    throw new BuilderInvariantError(
      `Missing script source for ${purpose}`,
      scriptHash,
    );
  }
  usedSources.add(resolved.source.sourceId);
  if (resolved.language === "NativeCardano") {
    if (redeemer !== undefined) {
      throw new BuilderInvariantError(
        `Native script ${purpose} cannot have a redeemer`,
        scriptHash,
      );
    }
    return;
  }
  if (purpose === "receive" && resolved.language === "PlutusV3") {
    throw new BuilderInvariantError(
      "PlutusV3 receive scripts are not supported",
    );
  }
  if (redeemer === undefined) {
    throw new BuilderInvariantError(
      `Missing redeemer for ${purpose}`,
      scriptHash,
    );
  }
  languages.add(resolved.language);
  redeemers.push({ pointer, redeemer });
};

/**
 * §5.1/§5.3: field 8 is the enveloped list of `enc_8` items
 * (`84 ‖ uint(purpose_tag) ‖ uint(index) ‖ bytes(redeemer_cbor) ‖ 82 ‖ uint(ex_memory) ‖ uint(ex_steps)`).
 * The retired counted scheme concatenated the raw item arrays with no per-item
 * envelope; §5.1 prohibits that form for all nine fields.
 *
 * Pointer ordering and duplicate rejection stay here — they are a builder
 * invariant about which redeemers may coexist, not a property of the byte
 * grammar, and the error the caller wants is `BuilderInvariantError`.
 */
const encodeRedeemers = (redeemers: readonly DerivedRedeemer[]): Buffer => {
  const seen = new Set<string>();
  const entries = [...redeemers].sort((left, right) => {
    if (left.pointer.tag !== right.pointer.tag) {
      return left.pointer.tag - right.pointer.tag;
    }
    return left.pointer.index < right.pointer.index
      ? -1
      : left.pointer.index > right.pointer.index
        ? 1
        : 0;
  });
  return encodeMidgardFieldPreimageForField({
    fieldIndex: 8,
    items: entries.map((entry) => {
      const key = pointerKey(entry.pointer);
      if (seen.has(key)) {
        throw new BuilderInvariantError("Duplicate redeemer pointer", key);
      }
      seen.add(key);
      const exUnits = normalizeExUnits(entry.redeemer);
      return {
        purpose: midgardRedeemerPurposeFromTag(entry.pointer.tag),
        index: entry.pointer.index,
        redeemerCbor: redeemerDataBytes(entry.redeemer),
        executionUnits: { memory: exUnits.mem, steps: exUnits.steps },
      };
    }),
  });
};

export const deriveScriptMaterialization = (
  state: BuilderState,
): ScriptMaterialization => {
  if (state.scripts.datumWitnesses.length > 0) {
    throw new BuilderInvariantError(
      "Datum witnesses are not supported by Midgard native transactions; use inline datums",
    );
  }
  const sources = collectKnownScriptSources(state);
  const usedSources = new Set<string>();
  const languages = new Set<ScriptLanguageName>();
  const redeemers: DerivedRedeemer[] = [];
  const consumedRedeemers = new Set<string>();
  const effective = effectiveMints(state.scripts.mints);

  const spent = [...state.spendInputs].sort(compareOutRefs);
  for (let index = 0; index < spent.length; index += 1) {
    const input = spent[index]!;
    const scriptHash = paymentScriptHashFromUtxo(input);
    if (scriptHash === undefined) {
      continue;
    }
    const redeemer = findSpendRedeemer(state, input);
    addRequiredExecution({
      scriptHash,
      purpose: "spend",
      pointer: { tag: RedeemerTags.Spend, index: BigInt(index) },
      redeemer,
      sources,
      redeemers,
      usedSources,
      languages,
    });
    if (redeemer !== undefined) {
      recordConsumedRedeemer(
        consumedRedeemers,
        redeemerIntentKey("spend", outRefLabel(input)),
      );
    }
  }

  const policyIds = effective.map((mint) => mint.policyId);
  for (let index = 0; index < policyIds.length; index += 1) {
    const policyId = policyIds[index]!;
    const redeemer = findMintRedeemer(effective, policyId);
    addRequiredExecution({
      scriptHash: policyId,
      purpose: "mint",
      pointer: { tag: RedeemerTags.Mint, index: BigInt(index) },
      redeemer,
      sources,
      redeemers,
      usedSources,
      languages,
    });
    if (redeemer !== undefined) {
      recordConsumedRedeemer(
        consumedRedeemers,
        redeemerIntentKey("mint", policyId),
      );
    }
  }

  const observers = [
    ...new Set(state.scripts.observers.map(({ scriptHash }) => scriptHash)),
  ].sort();
  for (let index = 0; index < observers.length; index += 1) {
    const observer = observers[index]!;
    const redeemer = findObserverRedeemer(state, observer);
    addRequiredExecution({
      scriptHash: observer,
      purpose: "observe",
      pointer: { tag: RedeemerTags.Reward, index: BigInt(index) },
      redeemer,
      sources,
      redeemers,
      usedSources,
      languages,
    });
    if (redeemer !== undefined) {
      recordConsumedRedeemer(
        consumedRedeemers,
        redeemerIntentKey("observe", observer),
      );
    }
  }

  const receivingHashes = [
    ...new Set(
      state.outputs.flatMap((output) => {
        if (!outputAddressProtected(output.address)) {
          return [];
        }
        const scriptHash = outputAddressPaymentScriptHash(output.address);
        return scriptHash === undefined ? [] : [scriptHash];
      }),
    ),
  ].sort();
  for (let index = 0; index < receivingHashes.length; index += 1) {
    const scriptHash = receivingHashes[index]!;
    const redeemer = findReceiveRedeemer(state, scriptHash);
    addRequiredExecution({
      scriptHash,
      purpose: "receive",
      pointer: { tag: RedeemerTags.Receive, index: BigInt(index) },
      redeemer,
      sources,
      redeemers,
      usedSources,
      languages,
    });
    if (redeemer !== undefined) {
      recordConsumedRedeemer(
        consumedRedeemers,
        redeemerIntentKey("receive", scriptHash),
      );
    }
  }

  assertAllRedeemerIntentsConsumed(state, effective, consumedRedeemers);

  for (const source of sources) {
    if (source.inline && !usedSources.has(source.sourceId)) {
      throw new BuilderInvariantError(
        "Extraneous script witness",
        source.sourceId,
      );
    }
  }

  const redeemerTxWitsPreimageCbor = encodeRedeemers(redeemers);
  const redeemerTxWitsHash = midgardFieldCommitment(redeemerTxWitsPreimageCbor);
  const requiredLanguages = [...languages].sort();
  return {
    requiredObserversPreimageCbor: requiredObserversPreimageCbor(
      state.scripts.observers,
    ),
    mintPreimageCbor: mintPreimageCbor(effective),
    scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage(
      sources
        .filter((source) => source.inline)
        .map((known) => {
          if (known.witnessScript === undefined) {
            throw new BuilderInvariantError(
              "Inline script source missing witness bytes",
            );
          }
          return known.witnessScript;
        }),
    ),
    redeemerTxWitsPreimageCbor,
    scriptIntegrityHash:
      requiredLanguages.length === 0
        ? Buffer.from(EMPTY_NULL_ROOT)
        : computeScriptIntegrityHashForLanguages(
            redeemerTxWitsHash,
            requiredLanguages,
          ),
    mintDelta: mintDeltaAssets(effective),
  };
};
