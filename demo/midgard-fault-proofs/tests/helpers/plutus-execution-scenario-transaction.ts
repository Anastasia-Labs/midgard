/** The transaction a Plutus-execution scenario commits over its seed
 * output, and the options that shape it. */
import {
  computeHash28,
  MIDGARD_CARDANO_MAX_VALUE_CBOR_BYTES,
} from "@al-ft/midgard-core";
import type { MidgardVersionedScript } from "@al-ft/midgard-core/codec";
import {
  MidgardRedeemerTag,
  outputCborMinAdaLovelace,
} from "@al-ft/midgard-validation";
import {
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML } from "@lucid-evolution/lucid";

import type { LedgerEntry } from "../../src/ledger-at.js";
import {
  FAILING_SCRIPT,
  PASSING_SCRIPT,
  type ScriptSpend,
} from "./plutus-execution-scripts.js";
import {
  mintPolicies,
  NATIVE_SPEND_SCRIPT,
  nativeMintAssets,
  nativeMintPreimageCbor,
  ownerKey,
  ownerKeyHash,
  protectedScriptAddress,
  type ScenarioMint,
  SEED_LOVELACE,
  type SeedCredential,
} from "./plutus-execution-seeds.js";

export type PlutusExecutionScenario =
  | "failing"
  | "passing"
  | "forced_failing"
  | "no_redeemers"
  /** The failing spend, stamped `TxIsInvalid`. */
  | "failing_stamped_invalid"
  /** The passing spend, also requiring a signer that does not sign. */
  | "unsigned_required_signer";

/** The script `scenario`'s spend runs: `spend`'s when given, else the
 * passing script for a passing spend and the failing script otherwise. */
const scenarioScript = (
  scenario: PlutusExecutionScenario,
  spend?: ScriptSpend,
): MidgardVersionedScript =>
  spend?.script ??
  (scenario === "passing" || scenario === "unsigned_required_signer"
    ? PASSING_SCRIPT
    : FAILING_SCRIPT);

export const scriptCredential = (
  scenario: PlutusExecutionScenario,
  spend?: ScriptSpend,
): SeedCredential => ({
  ScriptCredential: [hashScriptWitness(scenarioScript(scenario, spend))],
});

/** Who signs a script spend beyond its owner: `extraSigners` other keys,
 * each adding an address witness, and how many times the spend lists the
 * owner's key among its required signers (`requiredOwner`); or, with
 * `unsigned`, nobody, not even its owner. */
export type ScenarioSigners = Readonly<{
  extraSigners?: number;
  requiredOwner?: number;
  unsigned?: boolean;
}>;

/** The native script a transaction witnesses first, when it spends an
 * output a native script holds or mints, and what else of the transaction
 * names it: `script` in place of the native spend's (whose own output it
 * then cannot spend), required as an observer (`observer`), and holding
 * the transaction's outputs at its protected address (`receive`), which
 * also has the transaction witness it. With `observer`, the transaction
 * also requires and witnesses `extraObservers` more native scripts, each
 * one that any transaction satisfies. */
export type ScenarioNative = Readonly<{
  script?: MidgardVersionedScript;
  observer?: boolean;
  extraObservers?: number;
  receive?: boolean;
}>;

/** The `index`th native script any transaction satisfies, each distinct. */
export const anyoneObserverScript = (index: number) =>
  nativeScriptWitness({
    type: "atLeast",
    required: 0n,
    scripts: [{ type: "after", slot: BigInt(index) }],
  });

/** How `scenarioTransaction` builds the script spend; every field defaults
 * to the plain scenario. */
export type ScenarioTransactionOptions = Readonly<{
  redeemerData?: Uint8Array;
  referenceInputs?: readonly Buffer[];
  signers?: ScenarioSigners;
  spend?: ScriptSpend;
  nativeSpend?: LedgerEntry;
  nativeMint?: ScenarioMint;
  native?: ScenarioNative;
  passingSpends?: readonly LedgerEntry[];
}>;

/** A lovelace amount whose encoding is the longest a coin has: an output
 * that holds it is at least as long as one holding its minimum. */
const LONGEST_COIN = 2n ** 32n;

/** The outputs that hold what `nativeMint` mints under `native`'s script,
 * at its protected address when `native` says so: one, when it fits under
 * the ledger's value bound, else one per run of whole policies that does,
 * each holding its minimum lovelace; and the lovelace they hold beyond the
 * one output's. */
const mintOutputs = (
  nativeMint: ScenarioMint,
  { script = NATIVE_SPEND_SCRIPT, receive = false }: ScenarioNative,
) => {
  const address = receive ? protectedScriptAddress(script) : undefined;
  const output = (
    lovelace: bigint,
    policies: readonly (readonly [string, ReadonlyMap<string, bigint>])[],
  ) => makeOutput(lovelace, address, new Map(policies));
  const policies = [...nativeMintAssets(nativeMint, script)];
  if (
    output(LONGEST_COIN, policies).length <=
    MIDGARD_CARDANO_MAX_VALUE_CBOR_BYTES
  )
    return { address, held: [policies], minimums: [0n] };
  const held = [[] as (typeof policies)[number][]];
  for (const policy of policies) {
    const run = held.at(-1)!;
    if (
      run.length > 0 &&
      output(LONGEST_COIN, [...run, policy]).length >
        MIDGARD_CARDANO_MAX_VALUE_CBOR_BYTES
    )
      held.push([policy]);
    else run.push(policy);
  }
  return {
    address,
    held,
    minimums: held.map((run) =>
      outputCborMinAdaLovelace(output(LONGEST_COIN, run)),
    ),
  };
};

/** The lovelace the seed of a transaction minting `nativeMint` under
 * `native`'s script holds: the seed's own, and the minimum of each output
 * the mint needs past the first. */
export const scenarioSeedLovelace = (
  nativeMint: ScenarioMint,
  native: ScenarioNative = {},
): bigint =>
  mintOutputs(nativeMint, native).minimums.reduce(
    (sum, minimum) => sum + minimum,
    SEED_LOVELACE,
  );

/** The outputs of a transaction over `spent` outputs, its seed's lovelace
 * `scenarioSeedLovelace`, minting `nativeMint` under `native`'s script:
 * the first holds what it spends beside its outputs' minimums. */
export const scenarioOutputs = (
  spent: bigint,
  nativeMint: ScenarioMint,
  native: ScenarioNative = {},
): readonly Buffer[] => {
  const { address, held, minimums } = mintOutputs(nativeMint, native);
  return held.map((run, at) =>
    makeOutput(
      (at === 0 ? spent * SEED_LOVELACE : 0n) + minimums[at]!,
      address,
      new Map(run),
    ),
  );
};

/** The `index`th key beyond the owner's. */
const extraSignerKey = (index: number) => {
  const bytes = Buffer.alloc(32, 0x73);
  bytes.writeUInt16BE(index);
  return CML.PrivateKey.from_normal_bytes(bytes);
};

/** The hash of the `index`th key beyond the owner's, which signs a spend
 * with at least `index + 1` extra signers. */
export const extraSignerKeyHash = (index: number) =>
  computeHash28(
    Buffer.from(extraSignerKey(index).to_public().to_raw_bytes()),
  ).toString("hex");

/** The transaction the scenario commits, over its seed output; a script
 * spend's redeemer is `redeemerData` (Plutus Data CBOR) when given, it
 * reads `referenceInputs` (out-refs) as reference inputs, `signers` sign it
 * beside its owner, and it runs `spend`'s script (under `spend`'s units when
 * they are given) in place of the scenario's. With `nativeSpend`, the
 * transaction also spends that output, which sorts after the seed and which
 * the native script, witnessed first, holds; its first output carries both.
 * With `nativeMint`, it also mints under the native script's policy,
 * witnessed first, and under any further policy `nativeMint` names,
 * witnessed last, into its outputs (`scenarioOutputs`); `native` shapes that native
 * script. With `passingSpends`, it first spends those passing-script
 * outputs, each under its own redeemer. */
export const scenarioTransaction = (
  scenario: PlutusExecutionScenario,
  seed: LedgerEntry,
  {
    redeemerData,
    referenceInputs = [],
    signers: { extraSigners = 0, requiredOwner = 0, unsigned = false } = {},
    spend,
    nativeSpend,
    nativeMint = false,
    native = {},
    passingSpends = [],
  }: ScenarioTransactionOptions = {},
) => {
  const scriptIndex = BigInt(passingSpends.length);
  const outputs = [
    ...scenarioOutputs(
      scriptIndex + (nativeSpend === undefined ? 1n : 2n),
      nativeMint,
      native,
    ),
  ];
  const nativeScript = native.script ?? NATIVE_SPEND_SCRIPT;
  const extraObservers = Array.from(
    { length: native.observer === true ? (native.extraObservers ?? 0) : 0 },
    (_, index) => anyoneObserverScript(index),
  );
  const witnessesNative =
    nativeSpend !== undefined ||
    nativeMint !== false ||
    native.observer === true ||
    native.receive === true;
  const key = ownerKey();
  const extraKeys = Array.from({ length: extraSigners }, (_, index) =>
    extraSignerKey(index),
  );
  try {
    if (scenario === "no_redeemers")
      return makeNativeTx({
        spendInputs: [seed.outRef],
        outputs,
        privateKey: key,
      });
    return makeNativeTx({
      spendInputs: [
        ...passingSpends.map(({ outRef }) => outRef),
        seed.outRef,
        ...(nativeSpend === undefined ? [] : [nativeSpend.outRef]),
      ],
      referenceInputs,
      outputs,
      privateKey: key,
      scriptWitnesses: [
        ...(witnessesNative ? [nativeScript] : []),
        scenarioScript(scenario, spend),
        ...(passingSpends.length === 0 ? [] : [PASSING_SCRIPT]),
        ...extraObservers,
        ...mintPolicies(nativeMint, nativeScript).slice(1),
      ],
      ...(nativeMint === false
        ? {}
        : {
            mintPreimageCbor: nativeMintPreimageCbor(nativeMint, nativeScript),
          }),
      // A transaction that requires observers names its network.
      ...(native.observer === true
        ? {
            requiredObserverItems: [nativeScript, ...extraObservers]
              .map((script) => Buffer.from(hashScriptWitness(script), "hex"))
              .sort(Buffer.compare),
            networkId: 0n,
          }
        : {}),
      ...(unsigned ? { omitVkeyWitness: true as const } : {}),
      ...(scenario === "failing_stamped_invalid"
        ? { validity: "TxIsInvalid" as const }
        : {}),
      ...(scenario === "unsigned_required_signer"
        ? { requiredSignerItems: [Buffer.alloc(28, 0x71)] }
        : requiredOwner > 0
          ? {
              requiredSignerItems: Array.from({ length: requiredOwner }, () =>
                Buffer.from(ownerKeyHash(), "hex"),
              ),
            }
          : {}),
      ...(extraKeys.length > 0 ? { extraVkeyWitnessKeys: extraKeys } : {}),
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        ...passingSpends.map((_, index) => ({
          tag: MidgardRedeemerTag.Spend,
          index: BigInt(index),
        })),
        {
          tag: MidgardRedeemerTag.Spend,
          index: scriptIndex,
          ...(redeemerData === undefined ? {} : { data: redeemerData }),
          ...(spend?.exUnits === undefined ? {} : { exUnits: spend.exUnits }),
        },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
  } finally {
    key.free();
    for (const extra of extraKeys) extra.free();
  }
};
